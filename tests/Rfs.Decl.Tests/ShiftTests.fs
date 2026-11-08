module Rfs.Decl.Tests.ShiftTests

open System
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.SourceCodeServices
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private depsDir = Path.Combine(Path.GetDirectoryName tinyDir, "Deps")
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")
let private a = Path.Combine(depsDir, "A.fs")
let private e = Path.Combine(depsDir, "E.fs")

let private oracleClient = lazy (OracleClient.start ())

type private Project =
    { Spec: ProjectSpec
      Context: ProjectContext
      Texts: Map<string, string> }

let private load (overrides: (string * string) list) =
    async {
        let! spec = ProjectSpec.load depsProject |> Async.AwaitTask
        let spec = ProjectSpec.sequential spec
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)

        return
            { Spec = spec
              Context = ctx
              Texts = sources spec (Map.ofList overrides) }
    }

let private checkAll (start: FileStart) (env0: DeclEnv) (decls: ParsedDeclaration[]) =
    decls
    |> Array.mapFold
        (fun env decl ->
            let result = DeclarationChecker.CheckDeclaration(start, env, decl, CancellationToken.None)
            result, DeclarationChecker.ExtendEnv(env, result))
        env0
    |> fst

/// Three declarations: a value with an unused local (FS1182, from the finish's tail), a union type with a member, and a
/// function with a lambda, a match and locals
let private threeDecls (third: string) =
    "module Deps.A\n\nlet baseValue =\n    let unused = 3\n    10\n\ntype Shape =\n    | Circle of radius: int\n    | Square of side: int\n    member s.Area = match s with Circle r -> 3 * r * r | Square a -> a * a\n\n"
    + third

let private area =
    "let area (s: Shape) : int =\n    let half = fun (x: int) -> x / 2\n    match s with\n    | Circle r -> half (3 * r * r)\n    | Square a -> a * a + baseValue\n"

/// A literal, a union, a record, a class with an interface, an active pattern, a nested module with an object
/// expression, a match, a value with inference variables a later declaration solves (DependsOnLaterDeclarations), and
/// an unused local
let private richFile =
    """module Deps.A

[<Literal>]
let baseValue = 10

let scale (x: int) : int = x * 2

type Shape =
    | Circle of radius: int
    | Square of side: int

type Point = { X: int; Y: int }

type Counter(start: int) =
    let mutable count = start
    member _.Next() =
        count <- count + 1
        count
    interface System.IDisposable with
        member _.Dispose() = ()

let (|Even|Odd|) (n: int) = if n % 2 = 0 then Even else Odd

module Inner =
    let describe (n: int) =
        match n with
        | Even -> "even"
        | Odd -> "odd"

    let disposable = { new System.IDisposable with member _.Dispose() = () }

let area (s: Shape) : int =
    match s with
    | Circle r -> 3 * r * r
    | Square a -> a * a

let r = ref []

let fill () = r.Value <- [ baseValue ]

let unusedOuter () =
    let notUsed = 1
    { X = 1; Y = scale 2 }
"""

/// Each step writes a blank line above the anchor, or takes the one above it out, keeping every declaration's text
let private blankLineSteps =
    [ "let scale", true
      "module Inner", true
      "    let disposable", true
      "let fill", true
      "let scale", false
      "module Inner", false
      "let r = ref", true
      "    let disposable", false ]

/// A blank line above the type, the second declaration
let private blankAboveSecond (text: string) = text.Replace("type Shape", "\ntype Shape")

/// The file checked by declaration and finished; then its edited text parsed, each declaration that moved shifted and
/// the others kept, and the file finished again on the same FileStart. Returns the first results, the declarations
/// and results given to the second finish, which declarations were shifted, and the second finish.
let private shiftAndFinish (ctx: ProjectContext) (before: FileSignature list) (file: string) (text: string) (edited: string) =
    let parse = DeclarationChecker.Parse(ctx, file, text)
    let start, env0 = DeclarationChecker.BeginFile(ctx, before, parse)
    let results = checkAll start env0 (DeclarationChecker.Split parse)
    let first = DeclarationChecker.FinishFile(start, results, parse)
    // Its lazy results apply the signature's conformance again, which a shift must undo first
    first.CheckResults |> ignore
    let parse2 = DeclarationChecker.Parse(ctx, file, edited)
    let decls2 = DeclarationChecker.Split parse2
    Expect.equal (decls2 |> Array.map _.Key) (results |> Array.map _.Key) "the edit keeps every declaration"

    let shifted =
        Array.map2
            (fun (decl: ParsedDeclaration) (result: DeclarationResult) ->
                match DeclarationChecker.Shift(result, decl) with
                | ValueSome s -> s, true
                | ValueNone -> result, false)
            decls2
            results

    let second = DeclarationChecker.FinishFile(start, Array.map fst shifted, parse2)
    results, shifted, second

/// A.fs with this text, checked from scratch by declaration
let private fresh (p: Project) (text: string) =
    let parse = DeclarationChecker.Parse(p.Context, a, text)
    let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
    DeclarationChecker.FinishFile(start, checkAll start env0 (DeclarationChecker.Split parse), parse)

let private printsOf (r: FileResult) =
    r.Fingerprints |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.sort |> List.ofSeq

let private interactive (p: Project) (file: string) (text: string) =
    async {
        let texts = p.Texts.Add(file, text)
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions p.Spec)
        let sourceReader f = let t = texts[f] in hash t, lazy t
        let! _, checkResults, _ = checker.ParseAndCheckFileInProject(p.Spec.ProjectFile, Array.ofList p.Spec.SourceFiles, sourceReader, file)
        return checkResults
    }

/// Every symbol use with where its symbol is declared, in the signature and in the implementation (go to declaration)
let private locationsOf (results: FSharpCheckFileResults) =
    results.GetAllUsesOfAllSymbolsInFile()
    |> Seq.map (fun u ->
        $"%A{u.Range} %s{u.Symbol.FullName}: declared %A{u.Symbol.DeclarationLocation}, signature %A{u.Symbol.SignatureLocation}, implementation %A{u.Symbol.ImplementationLocation}")
    |> Seq.sort
    |> List.ofSeq

let private expectLocations (actual: FSharpCheckFileResults) (expected: FSharpCheckFileResults) (what: string) =
    let actual, expected = locationsOf actual, locationsOf expected
    let only (these: string list) (those: string list) = these |> List.filter (fun l -> not (List.contains l those))
    let missing, extra = only expected actual, only actual expected
    Expect.isTrue (missing.IsEmpty && extra.IsEmpty) $"%s{what}:\nexpected only:\n%s{String.concat Environment.NewLine missing}\nactual only:\n%s{String.concat Environment.NewLine extra}"

let private expectDiagnostics(p: Project) (file: string) (text: string) (actual: FileResult) (what: string) =
    task {
        let! oracle = oracleClient.Value
        let! all = oracle.Check p.Spec.ProjectFile (p.Texts.Add(file, text))
        let path = LspClient.NormalizePath file
        let expected = all |> List.filter (fun d -> String.Equals(LspClient.NormalizePath d.File, path, StringComparison.OrdinalIgnoreCase))
        let diff = Diagnostics.compare expected (toLsp actual.Diagnostics)
        Expect.isEmpty diff $"%s{what}: the diagnostics, ranges included, equal the oracle's:\n%s{String.concat Environment.NewLine diff}"
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Shift"
        [ testTask "shifted result equals re-check" {
              let text = threeDecls area
              let edited = blankAboveSecond text
              let! p = load [ a, text ]
              let results, shifted, second = shiftAndFinish p.Context [] a text edited
              Expect.equal (shifted |> Array.map snd) [| false; true; true |] "rows 2 and 3 are shifted, row 1 is kept"

              for old, (s, _) in Array.zip results shifted do
                  Expect.equal s.Fingerprint old.Fingerprint $"%s{old.Key.Name}: the fingerprint is kept"
                  Expect.isTrue (s.IsSameDelta old) $"%s{old.Key.Name}: the same values and entities"

              let expected = fresh p edited
              Expect.equal (printsOf second) (printsOf expected) "the final fingerprints are a fresh check's"
              Expect.equal second.Signature.Fingerprint expected.Signature.Fingerprint "the file's fingerprint is a fresh check's"
              Expect.isNonEmpty second.Diagnostics "the unused local is reported"
              let diff = Diagnostics.compare (toLsp expected.Diagnostics) (toLsp second.Diagnostics)
              Expect.isEmpty diff $"the diagnostics are a fresh check's:\n%s{String.concat Environment.NewLine diff}"
              do! expectDiagnostics p a edited second "after the shift"
              Expect.equal second.Reused 3 "the finish reuses what it computed for every result, the shifted ones included"
              let! reference = interactive p a edited
              expectLikeReference 5 edited reference second.CheckResults "A.fs after the shift"
              expectLocations (second.CheckResults) reference "every symbol's declaration, as go to declaration finds it"
          }

          testTask "shift moves an error's range" {
              let text = threeDecls "let bad : string = baseValue\n"
              let edited = blankAboveSecond text
              let! p = load [ a, text ]
              let results, shifted, second = shiftAndFinish p.Context [] a text edited
              Expect.equal (shifted |> Array.map snd) [| false; true; true |] "rows 2 and 3 are shifted"
              let error (r: FileResult) = r.Diagnostics |> Array.find (fun d -> d.ErrorNumber = 1)
              let before = results[2].Diagnostics |> Array.find (fun d -> d.ErrorNumber = 1)
              Expect.equal (error second).StartLine (before.StartLine + 1) "the error is one line lower"
              Expect.equal (fst shifted[2]).Diagnostics[0].StartLine (before.StartLine + 1) "so is the result's own"
              do! expectDiagnostics p a edited second "after the shift"
              Expect.equal second.Reused 2 "the error's row renders its diagnostics again"
          }

          testTask "blank lines in and out keep every result a fresh check's" {
              let! p = load [ a, richFile ]
              let parse = DeclarationChecker.Parse(p.Context, a, richFile)
              let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
              let mutable decls = DeclarationChecker.Split parse
              let mutable results = checkAll start env0 decls
              DeclarationChecker.FinishFile(start, results, parse) |> ignore
              let mutable text = richFile
              let refusals = Collections.Concurrent.ConcurrentQueue<string>()
              DeclarationChecker.ShiftRefusedHook <- refusals.Enqueue

              try
                  for step, (anchor, insert) in List.indexed blankLineSteps do
                      let edited =
                          if insert then text.Replace(anchor, "\n" + anchor)
                          else text.Replace("\n" + anchor, anchor)

                      Expect.notEqual edited text $"step %d{step}: the edit applies"
                      let parse2 = DeclarationChecker.Parse(p.Context, a, edited)
                      let decls2 = DeclarationChecker.Split parse2

                      let next =
                          Array.map2
                              (fun (d: ParsedDeclaration) (r: DeclarationResult) ->
                                  if decls |> Array.exists (fun o -> o.Key = d.Key && o.Range = d.Range) then
                                      r, false
                                  else
                                      match DeclarationChecker.Shift(r, d) with
                                      | ValueSome s -> s, true
                                      | ValueNone -> failtest $"step %d{step}: %s{d.Key.Name} was not shifted: %A{List.ofSeq refusals}")
                              decls2
                              results

                      let finished = DeclarationChecker.FinishFile(start, Array.map fst next, parse2)
                      let expected = fresh p edited
                      Expect.equal (printsOf finished) (printsOf expected) $"step %d{step}: the final fingerprints are a fresh check's"
                      let diff = Diagnostics.compare (toLsp expected.Diagnostics) (toLsp finished.Diagnostics)
                      Expect.isEmpty diff $"step %d{step}: the diagnostics are a fresh check's:\n%s{String.concat Environment.NewLine diff}"
                      do! expectDiagnostics p a edited finished $"step %d{step}"
                      let! reference = interactive p a edited
                      expectLikeReference 5 edited reference finished.CheckResults $"step %d{step}"
                      expectLocations (finished.CheckResults) reference $"step %d{step}: every symbol's declaration"
                      text <- edited
                      decls <- decls2
                      results <- Array.map fst next
              finally
                  DeclarationChecker.ShiftRefusedHook <- ignore
          }

          // Fuzz Declarations seed 1515: a lambda left to the file-end defaults, whose message shows the default
          testTask "a body left to the defaults is shifted" {
              let text =
                  "module Deps.A\n\nlet after = 42\n\nlet scaleDefaults (x: int) : int = x * 2\n\nlet areaDefaults (r: int) : int = 3 * scaleDefaults r\n\nlet gDefaults () =\n    (fun x -> ())\n    1\n"

              let edited = text.Replace("42\n\nlet", "42\nlet")
              let! p = load [ a, text ]
              let _, shifted, second = shiftAndFinish p.Context [] a text edited
              Expect.equal (shifted |> Array.map snd) [| false; true; true; true |] "the rows below the blank line are shifted"
              Expect.isFalse second.Signature.Failed $"the finish succeeds: %A{second.Diagnostics |> Array.map string}"
              do! expectDiagnostics p a edited second "after the shift"
          }

          // huge-il-lineinsert's file is checked with nullness on (its deferred checks are not reproduced here)
          testTask "a file checked with nullness on is shifted" {
              let project = Path.Combine(Path.GetDirectoryName depsDir, "Nullness", "Nullness.fsproj")
              let file = Path.Combine(Path.GetDirectoryName project, "A.fs")
              let! spec = ProjectSpec.load project |> Async.AwaitTask
              let spec = ProjectSpec.sequential spec
              let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
              let p = { Spec = spec; Context = ctx; Texts = sources spec Map.empty }
              let text = p.Texts[file]
              let edited = text.Replace("type Keyed", "\ntype Keyed")
              let _, shifted, second = shiftAndFinish ctx [] file text edited
              Expect.contains spec.OtherOptions "--checknulls+" "nullness is checked"
              let above = set [ "first"; "lazyMap"; "System.Collections.Generic" ]
              let wrong = shifted |> Array.filter (fun (s, moved) -> moved = above.Contains s.Key.Name) |> Array.map (fun (s, _) -> s.Key.Name)
              Expect.isEmpty wrong $"exactly the rows below the line are shifted: %A{wrong}"
              Expect.isNonEmpty second.Diagnostics "the nullness warning is reported"
              do! expectDiagnostics p file edited second "after the shift"
              let! reference = interactive p file edited
              expectLocations second.CheckResults reference "every symbol's declaration"
          }

          // A superseded graph flight shifts the result it read again, after another flight shifted it
          testTask "a result shifted twice moves its items once" {
              let text = threeDecls area
              let edited = blankAboveSecond text
              let! p = load [ a, text ]
              let parse = DeclarationChecker.Parse(p.Context, a, text)
              let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
              let results = checkAll start env0 (DeclarationChecker.Split parse)
              DeclarationChecker.FinishFile(start, results, parse) |> ignore
              let parse2 = DeclarationChecker.Parse(p.Context, a, edited)
              let decls2 = DeclarationChecker.Split parse2
              let shift i = DeclarationChecker.Shift(results[i], decls2[i])
              let first = [| for i in 1..2 -> (shift i).Value |]
              let again = [| for i in 1..2 -> (shift i).Value |]

              for f, g in Array.zip first again do
                  Expect.isTrue (obj.ReferenceEquals(f, g)) $"%s{f.Key.Name}: the second shift gives the first's result"

              let finished = DeclarationChecker.FinishFile(start, Array.append [| results[0] |] again, parse2)
              do! expectDiagnostics p a edited finished "after shifting twice"
              let! reference = interactive p a edited
              expectLocations finished.CheckResults reference "every symbol's declaration after shifting twice"

              // Back to the first text, from the results first read: shifted from where they were last shifted to
              let decls1 = DeclarationChecker.Split(DeclarationChecker.Parse(p.Context, a, text))
              let back = [| for i in 1..2 -> (DeclarationChecker.Shift(results[i], decls1[i])).Value |]
              let parse3 = DeclarationChecker.Parse(p.Context, a, text)
              let finished = DeclarationChecker.FinishFile(start, Array.append [| results[0] |] back, parse3)
              do! expectDiagnostics p a text finished "after shifting back"
              let! reference = interactive p a text
              expectLocations finished.CheckResults reference "every symbol's declaration after shifting back"
          }

          testTask "shift refuses a column change" {
              let text = "module Deps.A\n\nmodule Inner =\n    let one = 1\n\n    let two = one + 1\n"
              let edited = "module Deps.A\n\nmodule Inner =\n      let one = 1\n\n\n      let two = one + 1\n"
              let! p = load [ a, text ]
              let parse = DeclarationChecker.Parse(p.Context, a, text)
              let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
              let decls = DeclarationChecker.Split parse
              let results = checkAll start env0 decls
              let decls2 = DeclarationChecker.Split(DeclarationChecker.Parse(p.Context, a, edited))
              let two = decls2 |> Array.findIndex (fun d -> d.Key.Name = "two")
              Expect.equal decls2[two].Text decls[two].Text "the same text"
              Expect.notEqual decls2[two].Range.StartLine decls[two].Range.StartLine "on another line"
              Expect.notEqual decls2[two].Range.StartColumn decls[two].Range.StartColumn "at another column"
              Expect.isTrue (DeclarationChecker.Shift(results[two], decls2[two])).IsNone "not shifted"
          }

          testTask "shift of a signed file's row keeps go to declaration" {
              let text = File.ReadAllText e
              let edited = text.Replace("let compute", "\nlet compute")
              let! p = load []
              let files = p.Spec.SourceFiles
              let before = files |> List.takeWhile (fun f -> f <> e)

              let signatures =
                  (([]: FileSignature list), before)
                  ||> List.fold (fun sigs file ->
                      let parse = DeclarationChecker.Parse(p.Context, file, p.Texts[file])

                      if file.EndsWith ".fsi" then
                          sigs @ [ DeclarationChecker.CheckSignatureFile(p.Context, sigs, parse) ]
                      else
                          let start, env0 = DeclarationChecker.BeginFile(p.Context, sigs, parse)
                          let finished = DeclarationChecker.FinishFile(start, checkAll start env0 (DeclarationChecker.Split parse), parse)
                          sigs @ [ finished.Signature ])

              let refusals = Collections.Concurrent.ConcurrentQueue<string>()
              DeclarationChecker.ShiftRefusedHook <- refusals.Enqueue
              let _, shifted, second = try shiftAndFinish p.Context signatures e text edited finally DeclarationChecker.ShiftRefusedHook <- ignore

              Expect.equal
                  (shifted |> Array.map (fun (s, moved) -> s.Key.Name, moved))
                  [| "hidden", false; "user", false; "helper", false; "compute", true; "other", true; "unrelated", true |]
                  $"exactly the rows below the blank line are shifted: %A{List.ofSeq refusals}"
              do! expectDiagnostics p e edited second "E.fs after the shift"
              let! reference = interactive p e edited
              expectLikeReference 5 edited reference second.CheckResults "E.fs after the shift"
              expectLocations (second.CheckResults) reference "every symbol's declaration, in E.fsi and in E.fs"
          } ]
