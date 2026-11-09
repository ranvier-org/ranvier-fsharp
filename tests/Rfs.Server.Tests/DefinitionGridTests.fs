module Rfs.Server.Tests.DefinitionGridTests

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Server
open Rfs.Server.Tests.TokenGrid

/// How one token compares with FCS
type private Outcome =
    | Same
    /// Answers nothing where FCS answers, as ruled for this kind of symbol
    | Limit of string
    | Different of string

let private show (r: range option) =
    match r with
    | None -> "none"
    | Some r -> sprintf "%s %d,%d-%d,%d" (Path.GetFileName r.FileName) r.StartLine r.StartColumn r.EndLine r.EndColumn

/// Why answering nothing is allowed here, if it is:
/// - a parameter: the symbol's own location is stale in a signature or from another file, and a parameter has no key;
/// - a namespace: every file that opens a fragment of it declares it, and which FCS names depends on the check order;
/// - a tie: in a file that doesn't compile, a key that two definitions share, which nothing current can tell apart.
let private limitOf (check: FSharpCheckFileResults) (ties: HashSet<string>) (token: Token) =
    match check.GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names) with
    | None -> None
    | Some u ->
        match u.Symbol, DeclarationChecker.SymbolKey u.Symbol with
        | :? FSharpEntity as e, _ when e.IsNamespace -> Some "namespace"
        | _, Some key when ties.Contains key -> Some "tie"
        | :? FSharpParameter, None -> Some "parameter"
        | :? FSharpMemberOrFunctionOrValue, None -> Some "parameter"
        | _ -> None

/// The keys that two definitions of a file with errors share
let private tiesIn (check: FSharpCheckFileResults) (ties: HashSet<string>) =
    if check.Diagnostics |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) then
        for _, group in
            check.GetAllUsesOfAllSymbolsInFile()
            |> Seq.filter _.IsFromDefinition
            |> Seq.choose (fun u -> DeclarationChecker.SymbolKey u.Symbol |> Option.map (fun k -> k, u.Symbol.SignatureLocation))
            |> Seq.distinct
            |> Seq.groupBy fst do
            if Seq.length group > 1 then
                ties.Add(fst (Seq.head group)) |> ignore

/// Every token of every implementation file of one state
let private compareState (state: State) : Task<Outcome list> =
    task {
        let outcomes = ResizeArray<Outcome>()
        let ties = HashSet<string>()

        for file in state.Implementations do
            let! check = state.Reference file
            tiesIn check ties

        for file in state.Implementations do
            let! check = state.Reference file

            for token in tokensOf (state.TextOf file) do
                let expected =
                    match check.GetDeclarationLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names, preferFlag = true) with
                    | FindDeclResult.DeclFound r when not (state.OwnersOf r.FileName).IsEmpty -> Some r
                    | _ -> None

                let! actual =
                    Definition.at state.OwnersOf state.Graph file token.Line token.Character CancellationToken.None
                    |> fun t -> t.WaitAsync(TimeSpan.FromSeconds 110.)

                if show actual = show expected then
                    outcomes.Add Same
                else
                    let where = sprintf "%s / %s: %s %d,%d %A [%s]" state.Scenario state.Step (Path.GetFileName file) (token.Line + 1) token.EndColumn token.Names (token.LineText.Trim())

                    match actual, limitOf check ties token with
                    | None, Some limit -> outcomes.Add(Limit limit)
                    | _ -> outcomes.Add(Different $"%s{where}: ours %s{show actual}, FCS %s{show expected}")

        return List.ofSeq outcomes
    }

let private expectGrid (promote: bool) (scenario: Scenario) =
    task {
        let! outcomes = run promote scenario compareState
        Expect.isGreaterThan outcomes.Length 0 "tokens"
        let different = outcomes |> List.choose (function Different d -> Some d | _ -> None)
        let limits = outcomes |> List.choose (function Limit l -> Some l | _ -> None) |> List.countBy id
        let shown = different |> List.truncate 25 |> String.concat "\n  "
        printfn "grid %s (%s): %d tokens, %d differ, limits %A" scenario.Name (if promote then "by declaration" else "file level") outcomes.Length different.Length limits
        Expect.isTrue different.IsEmpty $"%d{different.Length} of %d{outcomes.Length} tokens differ from FCS:\n  %s{shown}"
    }

/// A signature file's types reach other files' entities, whose rows are being checked again, and its read takes no lock
let private stressFiles =
    let types (gap: string) (body: int) =
        $"module Proj.T\n\ntype Ab =%s{gap} int\n\ntype L<'a> = 'a list\n\ntype C() =\n    member _.A = %d{body}\n\ntype C with\n    member _.B = 2\n"

    let signature (blank: string) =
        $"module Proj.S\n%s{blank}\nopen Proj.T\n\ntype O =\n    new: unit -> O\n    member M: x: Ab -> Ab\n    member M: x: L<C> -> L<C>\n    member M: x: C -> C\n\nval v: C\n"

    let implementation =
        "module Proj.S\n\nopen Proj.T\n\ntype O() =\n    member _.M(x: Ab) = x\n    member _.M(x: L<C>) = x\n    member _.M(x: C) = x\n\nlet v = C()\n"

    let later = "module Proj.U\n\nlet a = Proj.S.O().M 1\n\nlet b = Proj.S.O().M [ Proj.S.v ]\n\nlet c = Proj.S.O().M Proj.S.v\n"
    types, signature, implementation, later

[<Tests>]
let tests =
    testList
        "DefinitionGrid"
        [ testTask "requests into a signature file end while the files its types reach are edited" {
              let types, signature, implementation, later = stressFiles

              let scenario =
                  { Name = "stress"
                    Files = [ "T.fs", types "" 1; "S.fsi", signature ""; "S.fs", implementation; "U.fs", later ]
                    Steps = [] }

              let! (requests: int list) =
                  run true scenario (fun state ->
                      task {
                          let file (name: string) = state.Files |> List.find (fun f -> Path.GetFileName f = name)
                          let u = file "U.fs"
                          let tokens = tokensOf later |> List.filter (fun t -> List.last t.Names = "M" || List.last t.Names = "v")
                          use stop = new CancellationTokenSource()
                          let mutable count = 0

                          let worker () =
                              Task.Run<unit>(fun () ->
                                  task {
                                      while not stop.IsCancellationRequested do
                                          for t in tokens do
                                              let! _ = Definition.at state.OwnersOf state.Graph u t.Line t.Character CancellationToken.None
                                              Interlocked.Increment &count |> ignore
                                  })

                          let workers = [ for _ in 1..4 -> worker () ]

                          for i in 1..60 do
                              state.Graph.SetText(file "T.fs", types (String(' ', i % 3 + 1)) i) |> ignore
                              state.Graph.SetText(file "S.fsi", signature (String('\n', i % 2))) |> ignore
                              let! _ = state.Graph.AllDiagnostics().WaitAsync(TimeSpan.FromSeconds 110.)
                              ()

                          stop.Cancel()
                          let! _ = (Task.WhenAll workers).WaitAsync(TimeSpan.FromSeconds 110.)
                          return [ count ]
                      })

              Expect.isGreaterThan requests.Head 0 "requests ran, and none faulted"
          }

          testList
              "by declaration"
              [ for s in DefinitionGridTexts.all -> testTask s.Name { do! expectGrid true s } ]
          testList
              "file level"
              [ for s in DefinitionGridTexts.batteries -> testTask s.Name { do! expectGrid false s } ] ]
