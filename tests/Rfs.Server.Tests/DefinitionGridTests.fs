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

/// Why answering nothing is allowed here, if it is. A parameter or local in the asking file, outside a signature, is not
/// excused, nor a namespace token of the file's own header.
/// - "signed parameter": FCS lands in a signature, whose parameter has no key to look it up by;
/// - "named argument": its parameter is another row's or another file's, with no key either;
/// - "namespace": elsewhere than its own header: every file that opens a fragment of it declares it;
/// - "tie": in a file that doesn't compile, a key that two definitions share, which nothing current can tell apart;
/// - "generic overload": an implementation more generic than its signature, with another overload of as many arguments
///   there: nothing says which it conformed to.
let private limitOf (check: FSharpCheckFileResults) (file: string) (ties: HashSet<string>) (token: Token) (expected: range option) =
    let isNamespace (s: FSharpSymbol) = match s with :? FSharpEntity as e -> e.IsNamespace | _ -> false

    let isHeader () =
        check.GetAllUsesOfAllSymbolsInFile()
        |> Seq.exists (fun d -> d.IsFromDefinition && d.Range.EndLine = token.Line + 1 && d.Range.EndColumn = token.EndColumn && isNamespace d.Symbol)

    let inSignature = expected |> Option.exists (fun r -> r.FileName.EndsWith ".fsi")
    let elsewhere = expected |> Option.exists (fun r -> not (String.Equals(r.FileName, file, StringComparison.OrdinalIgnoreCase)))

    match check.GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names) with
    | None -> None
    | Some u ->
        match u.Symbol, DeclarationChecker.SymbolKey u.Symbol with
        | s, _ when isNamespace s -> if isHeader () then None else Some "namespace"
        | _, Some key when ties.Contains key -> Some "tie"
        | :? FSharpParameter, None -> Some "named argument"
        | :? FSharpMemberOrFunctionOrValue, None when inSignature || elsewhere -> Some "signed parameter"
        | :? FSharpMemberOrFunctionOrValue as v, Some _ when inSignature && v.IsMember && v.GenericParameters.Count > 0 -> Some "generic overload"
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

                    match actual, limitOf check file ties token expected with
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

/// App on Lib in memory, at every token of App. FCS can't check App on Lib's unbuilt text, so a token whose key is Lib's
/// must answer where FCS on Lib says that key's definition is; a token in App itself only has to stay in App.
let private expectAcrossProjects (lib: Scenario) (appText: string) =
    task {
        let mutable inLib = 0

        let! outcomes =
            runReferencing lib appText (fun state app appFile ->
                task {
                    let definitions = Dictionary<string, Set<string>>()

                    for file in state.Implementations do
                        let! check = state.Reference file

                        let add (symbol: FSharpSymbol) =
                            match DeclarationChecker.SymbolKey symbol with
                            | Some key ->
                                let here = show symbol.SignatureLocation
                                definitions[key] <- (match definitions.TryGetValue key with | true, s -> s.Add here | _ -> Set.singleton here)
                            | None -> ()

                        for u in check.GetAllUsesOfAllSymbolsInFile() do
                            if u.IsFromDefinition then
                                add u.Symbol

                                // An unnamed field has no definition use; FCS answers its location, as the batteries show
                                match u.Symbol with
                                | :? FSharpUnionCase as c -> c.Fields |> Seq.iter add
                                | :? FSharpEntity as e when e.IsFSharpExceptionDeclaration -> e.FSharpFields |> Seq.iter add
                                | _ -> ()

                    let found = ResizeArray<Outcome>()

                    for token in tokensOf appText do
                        let! key =
                            (app: Rfs.Checker.ProjectGraph).ReadFile(
                                appFile,
                                (fun view ->
                                    view.Result.CheckResults.GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names)
                                    |> Option.bind (fun u ->
                                        match u.Symbol with
                                        | :? FSharpEntity as e when e.IsNamespace -> None
                                        | :? FSharpParameter -> None
                                        | s -> DeclarationChecker.SymbolKey s)),
                                CancellationToken.None
                            )

                        let! actual = Definition.at state.OwnersOf app appFile token.Line token.Character CancellationToken.None

                        match key with
                        | Some k when k.StartsWith "Lib|" ->
                            let want = (match definitions.TryGetValue k with | true, s -> s | _ -> Set.empty) + (match definitions.TryGetValue(k + "|setter") with | true, s -> s | _ -> Set.empty)

                            if want.Contains(show actual) then
                                found.Add Same

                                if actual.IsSome then
                                    inLib <- inLib + 1
                            else
                                found.Add(Different $"%s{state.Step}: %d{token.Line + 1},%d{token.EndColumn} %A{token.Names} [%s{token.LineText.Trim()}]: ours %s{show actual}, FCS on Lib %A{Set.toList want}")
                        | _ ->
                            match actual with
                            | Some r when not (String.Equals(r.FileName, appFile, StringComparison.OrdinalIgnoreCase)) ->
                                found.Add(Different $"%s{state.Step}: %d{token.Line + 1},%d{token.EndColumn} %A{token.Names}: no key of Lib's, and ours %s{show actual}")
                            | _ -> found.Add Same

                    return List.ofSeq found
                })

        let different = outcomes |> List.choose (function Different d -> Some d | _ -> None)
        printfn "grid %s, App on Lib: %d tokens, %d differ, %d answer in Lib" lib.Name outcomes.Length different.Length inLib
        // App resolving nothing of Lib's would pass every token
        Expect.isGreaterThan inLib (100 * (lib.Steps.Length + 1)) "App's tokens answer in Lib, in every state"
        let shown = different |> List.truncate 25 |> String.concat "\n  "
        Expect.isTrue different.IsEmpty $"%d{different.Length} of %d{outcomes.Length} tokens differ:\n  %s{shown}"
    }

/// The index's lookups (FileUses.DefinitionOf, SignatureDefinitionOf, which copy the fork's SymbolKeys rules) answer what
/// the fork's DeclarationChecker.DefinitionLocation and SignatureLocation answer, for every key used in the project's
/// implementation files, in every file. The one allowed difference: the index also finds an unnamed field of a case or
/// exception in an implementation, which has no definition use there. Returns how many lookups were compared.
let private wait (t: Task<'a>) = t.WaitAsync(TimeSpan.FromSeconds 110.)

let private expectLookupsAsFork (graph: Rfs.Checker.ProjectGraph) : Task<int> =
    task {
        let files = graph.Spec.SourceFiles
        let isSignature (f: string) = f.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)
        let keys = HashSet<string>()

        for f in files |> List.filter (isSignature >> not) do
            let! (found: string[]) =
                graph.ReadFile(
                    f,
                    (fun v ->
                        v.Result.CheckResults.GetAllUsesOfAllSymbolsInFile()
                        |> Seq.choose (fun u -> DeclarationChecker.SymbolKey u.Symbol)
                        |> Seq.toArray),
                    CancellationToken.None
                )
                |> wait

            for k in found do
                keys.Add k |> ignore

        let scope () : Rfs.Checker.TurnScope<unit> =
            { Loading = false
              Projects = [| graph, files |]
              Name = None
              Extra = () }

        let! read = graph.ReadInOneTurn(scope, CancellationToken.None) |> wait
        let keys = Array.ofSeq keys
        let different = ResizeArray<string>()
        let mutable compared = 0

        match read with
        | Rfs.Checker.TurnRead.Gone -> failtest "the project went away"
        | Rfs.Checker.TurnRead.Read(_, projects) ->
            for _, turnFiles in projects do
                for tf in turnFiles do
                    match tf with
                    | Rfs.Checker.TurnFile.Indexed(path, _, uses) ->
                        let! (fork: (range option)[]) =
                            graph.ReadFile(
                                path,
                                (fun v -> keys |> Array.map (fun k -> DeclarationChecker.DefinitionLocation(v.Result.CheckResults, k, CancellationToken.None))),
                                CancellationToken.None
                            )
                            |> wait

                        for k, expected in Array.zip keys fork do
                            compared <- compared + 1
                            let ours = uses.DefinitionOf k
                            let unnamedField = expected.IsNone && ours.IsSome && k.Split('|')[1] = "field"

                            if ours <> expected && not unnamedField then
                                different.Add $"%s{Path.GetFileName path} %s{k}: ours %s{show ours}, fork %s{show expected}"
                    | Rfs.Checker.TurnFile.Signature(path, _, uses) ->
                        let signature = uses.Source.Target :?> FileSignature

                        for k in keys do
                            for conformed in [ false; true ] do
                                compared <- compared + 1
                                let ours = uses.SignatureDefinitionOf(k, conformed)
                                let expected = DeclarationChecker.SignatureLocation(graph.Context, signature, k, conformed, CancellationToken.None)

                                if ours <> expected then
                                    different.Add $"%s{Path.GetFileName path} %s{k} conformed %b{conformed}: ours %s{show ours}, fork %s{show expected}"
                    | Rfs.Checker.TurnFile.Skipped _ -> ()

        let shown = String.concat "\n  " (Seq.truncate 30 different)
        Expect.isTrue (different.Count = 0) $"%d{different.Count} of %d{compared} lookups differ:\n  %s{shown}"
        return compared
    }

[<Tests>]
let tests =
    testList
        "DefinitionGrid"
        [ testTask "the index's lookups answer as the fork's, for every key of the Deps fixture" {
              let! (s: Rfs.Server.Tests.PublishTests.Session) = Rfs.Server.Tests.PublishTests.startSession () |> wait
              use _ = Rfs.Server.Tests.PublishTests.disposing s
              let graph = (s.Server.Workspace.ProjectOf Rfs.Server.Tests.PublishTests.a).Value
              let! _ = graph.AllDiagnostics() |> wait
              let! compared = expectLookupsAsFork graph
              Expect.isGreaterThan compared 0 "lookups compared"
          }

          testTask "the index's lookups answer as the fork's, for every key of the signed battery" {
              let! (compared: int list) = run true { DefinitionGridTexts.signed with Steps = [] } (fun state -> task { let! n = expectLookupsAsFork state.Graph in return [ n ] })
              Expect.isGreaterThan compared.Head 0 "lookups compared"
          }

          testTask "App on Lib, unsigned" { do! expectAcrossProjects DefinitionGridTexts.libUnsigned DefinitionGridTexts.uses }
          testTask "App on Lib, signed" { do! expectAcrossProjects DefinitionGridTexts.libSigned DefinitionGridTexts.uses }

          testTask "requests into a signature file end while the files its types reach are edited" {
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
