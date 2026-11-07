module Rfs.Decl.Tests.FinishCacheTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Symbols
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private depsDir = Path.Combine(Path.GetDirectoryName tinyDir, "Deps")
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")
let private a = Path.Combine(depsDir, "A.fs")

/// One oracle process for the whole run; it answers one request at a time.
let private oracleClient = lazy (OracleClient.start ())

type private Project =
    { Spec: ProjectSpec
      Context: ProjectContext
      Texts: Map<string, string> }

/// A context of its own for each call: the #nowarn scopes a parse records are the project's.
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

/// Each declaration checked in the environment ExtendEnv folds from the results before it, unless `keep` gives the
/// result to use for it.
let private checkAll (start: FileStart) (env0: DeclEnv) (decls: ParsedDeclaration[]) (keep: int -> DeclarationResult option) =
    let results = ResizeArray<DeclarationResult>()
    let mutable env = env0

    for i in 0 .. decls.Length - 1 do
        let result =
            match keep i with
            | Some result -> result
            | None -> DeclarationChecker.CheckDeclaration(start, env, decls[i], CancellationToken.None)

        results.Add result
        env <- DeclarationChecker.ExtendEnv(env, result)

    results.ToArray()

/// A.fs (which depends on no file) with this text, checked from scratch by declaration on the context.
let private fresh (p: Project) (text: string) =
    let parse = DeclarationChecker.Parse(p.Context, a, text)
    let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
    DeclarationChecker.FinishFile(start, checkAll start env0 (DeclarationChecker.Split parse) (fun _ -> None), parse)

let private printsOf (r: FileResult) =
    r.Fingerprints |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.filter (fun (_, fp) -> not (fp.StartsWith "?")) |> Seq.sort |> List.ofSeq

/// The finish equals a check of the same text from scratch, and its diagnostics the oracle's.
let private expectFresh (p: Project) (text: string) (actual: FileResult) (what: string) =
    task {
        let expected = fresh p text
        Expect.equal (printsOf actual) (printsOf expected) $"%s{what}: the final fingerprints are a fresh check's"
        // A fingerprint that is not describable (`?`) is unequal to every other, a fresh check's included
        if not (actual.Signature.Fingerprint.StartsWith "?" && expected.Signature.Fingerprint.StartsWith "?") then
            Expect.equal actual.Signature.Fingerprint expected.Signature.Fingerprint $"%s{what}: the file's fingerprint is a fresh check's"
        let diff = Diagnostics.compare (toLsp expected.Diagnostics) (toLsp actual.Diagnostics)
        Expect.isEmpty diff $"%s{what}: the diagnostics are a fresh check's:\n%s{String.concat Environment.NewLine diff}"
        let! oracle = oracleClient.Value
        let! all = oracle.Check p.Spec.ProjectFile (p.Texts.Add(a, text))
        let file = LspClient.NormalizePath a
        let expected = all |> List.filter (fun d -> String.Equals(LspClient.NormalizePath d.File, file, StringComparison.OrdinalIgnoreCase))
        let diff = Diagnostics.compare expected (toLsp actual.Diagnostics)
        Expect.isEmpty diff $"%s{what}: the diagnostics equal the oracle's:\n%s{String.concat Environment.NewLine diff}"
    }

/// A.fs with a warning and an error, so reused results carry rendered diagnostics.
let private withDiagnostics =
    File.ReadAllText a + "\nlet unusedWarn () =\n    let unused = 5\n    1\n\nlet bad : string = 1\n"

/// A.fs checked by declaration and finished, then the text edited and finished again on the same FileStart: with the
/// old result of every declaration whose syntax and place are unchanged and that `recheck` does not pick, and a new
/// check of the others. Returns both finishes, how many old results were kept, and the second finish's results.
let private finishTwice (p: Project) (text: string) (edited: string) (recheck: ParsedDeclaration[] -> int -> bool) =
    let parse = DeclarationChecker.Parse(p.Context, a, text)
    let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
    let decls = DeclarationChecker.Split parse
    let results = checkAll start env0 decls (fun _ -> None)
    let first = DeclarationChecker.FinishFile(start, results, parse)
    let parse2 = DeclarationChecker.Parse(p.Context, a, edited)
    let decls2 = DeclarationChecker.Split parse2
    let byKey = Array.zip decls results |> Array.map (fun (d, r) -> d.Key, (d, r)) |> dict

    let keep i =
        match byKey.TryGetValue decls2[i].Key with
        | true, (d, r) when d.IsUnchangedFrom decls2[i] && d.Range = decls2[i].Range && not (recheck decls2 i) -> Some r
        | _ -> None

    let results2 = checkAll start env0 decls2 keep
    let kept = Array.init decls2.Length (fun i -> (keep i).IsSome) |> Array.filter id |> Array.length
    first, DeclarationChecker.FinishFile(start, results2, parse2), kept, results2

/// The interactive checker's results for A.fs with this text.
let private interactive (p: Project) (text: string) =
    async {
        let texts = p.Texts.Add(a, text)
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions p.Spec)
        let sourceReader f = let t = texts[f] in hash t, lazy t
        let! _, checkResults, _ = checker.ParseAndCheckFileInProject(p.Spec.ProjectFile, Array.ofList p.Spec.SourceFiles, sourceReader, a)
        return checkResults
    }

/// C1 of the Task 5 review: the file-end defaults stop at the first default that fails, so whether a later body's
/// inference variable gets its default, and how its messages show it, depends on another declaration.
let private failingDefault (body: string) =
    $"module Deps.A\n\nlet scale (x: int) : int = x * 2\n\nlet area (r: int) : int = 3 * scale * r\n\nlet g () =\n%s{body}    1\n"

/// Each compiled name the implementation file's values have, in order.
let private compiledNames (results: FSharpCheckFileResults) =
    let rec walk (decl: FSharpImplementationFileDeclaration) =
        [ match decl with
          | FSharpImplementationFileDeclaration.Entity(_, subDecls) -> yield! List.collect walk subDecls
          | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, _, _) -> yield v.CompiledName
          | FSharpImplementationFileDeclaration.InitAction _ -> () ]

    results.ImplementationFile.Value.Declarations |> List.collect walk

[<Tests>]
let tests =
    testList
        "FinishCache"
        [ testTask "second finish equals a fresh finish" {
              let edited = withDiagnostics.Replace("x * 2", "x * 3")
              let! p = load [ a, withDiagnostics ]
              let first, second, kept, results = finishTwice p withDiagnostics edited (fun _ _ -> false)
              Expect.equal first.Reused 0 "a first finish reuses nothing"
              Expect.equal kept (results.Length - 1) "only scale is checked again"
              Expect.isFalse (results |> Array.exists _.DependsOnLaterDeclarations) "no result depends on later declarations"
              Expect.equal second.Reused (results.Length - 1) "every result given to the first finish is reused"
              Expect.isNonEmpty second.Diagnostics "the reused results carry diagnostics"
              do! expectFresh p edited second "the second finish"
          }

          testTask "default-dependent row re-renders" {
              let text = "module Deps.A\n\nlet first = 1\n\nlet h x = x + x\n\nlet other = 2\n"
              let withUse = text + "\nlet useH = h 1.0\n"
              let! p = load [ a, text ]
              // A use added after h solves h's inference variables in place: h's result is the same object, but its
              // types are no longer the ones the first finish's defaults gave
              let first, second, kept, results = finishTwice p text withUse (fun _ _ -> false)
              let h = results |> Array.find (fun r -> r.Key.Name = "h")
              Expect.isTrue h.DependsOnLaterDeclarations "h depends on later declarations"
              Expect.equal kept 3 "first, h and other are given to the second finish again"
              // other's environment holds h's inference variables too
              Expect.equal
                  (results |> Array.filter (fun r -> not r.DependsOnLaterDeclarations) |> Array.map _.Key.Name)
                  [| "first"; "useH" |]
                  "only first, and the use that solves h, are independent of later declarations"
              Expect.equal second.Reused 1 "h and other are not reused"
              let printOfH (r: FileResult) = r.Fingerprints |> Seq.find (fun kv -> kv.Key.Name = "h") |> _.Value
              Expect.notEqual (printOfH second) (printOfH first) "h's final types follow the use, not the first finish's defaults"
              do! expectFresh p withUse second "h used with a float"

              // The use changed to an int: h and what follows are checked again (R12)
              let changed = withUse.Replace("h 1.0", "h 1")
              let hAt (decls: ParsedDeclaration[]) = decls |> Array.findIndex (fun d -> d.Key.Name = "h")
              let _, third, kept, _ = finishTwice p withUse changed (fun decls i -> i >= hAt decls)
              Expect.equal kept 1 "only first is kept"
              Expect.equal third.Reused 1 "first is reused"
              Expect.notEqual (printOfH third) (printOfH second) "h's types follow the int use"
              do! expectFresh p changed third "h used with an int"
          }

          testTask "signature object reused when fingerprints are equal" {
              let edited = withDiagnostics.Replace("x * 2", "x * 3")
              let! p = load [ a, withDiagnostics ]
              let first, second, _, _ = finishTwice p withDiagnostics edited (fun _ _ -> false)
              Expect.isTrue (obj.ReferenceEquals(first.Signature, second.Signature)) "the second finish gives the first's signature"
              do! expectFresh p edited second "after a body edit"
          }

          testTask "signature replaced when a fingerprint changes" {
              let edited = withDiagnostics.Replace("let scale (x: int) : int = x * 2", "let scale (x: int) : int64 = int64 x * 2L")
              let! p = load [ a, withDiagnostics ]
              let scaleAt (decls: ParsedDeclaration[]) = decls |> Array.findIndex (fun d -> d.Key.Name = "scale")
              let first, second, kept, _ = finishTwice p withDiagnostics edited (fun decls i -> i >= scaleAt decls)
              Expect.isFalse (obj.ReferenceEquals(first.Signature, second.Signature)) "a new signature"
              Expect.notEqual second.Signature.Fingerprint first.Signature.Fingerprint "with a new fingerprint"
              Expect.equal second.Reused kept "the declarations before scale are reused"
              do! expectFresh p edited second "after a signature edit"
          }

          testTask "lazy check results stay correct" {
              let edited = withDiagnostics.Replace("x * 2", "x * 3")
              let! p = load [ a, withDiagnostics ]
              let _, second, _, _ = finishTwice p withDiagnostics edited (fun _ _ -> false)
              let! expected = interactive p edited
              expectLikeReference 5 edited expected second.CheckResults "A.fs after its second finish"
              Expect.equal second.CheckResults.Diagnostics second.Diagnostics "the results carry the finish's diagnostics"
          }

          yield!
              [ for name, body in
                    [ "x + x", "    (fun x -> x + x)\n"
                      "x + 1.0", "    (fun x -> x + 1.0)\n"
                      // C1-R: a variable only a lambda parameter's type holds, and one only a local value's does
                      "a lambda parameter", "    (fun x -> ())\n"
                      "a local value", "    let f = fun x -> ()\n    ignore f\n" ] do
                    for broken in [ true; false ] do
                        let what = if broken then "fixed" else "broken"

                        testTask $"a body whose default another declaration stops is rendered again (%s{name}, area %s{what})" {
                            let broken' = failingDefault body
                            let fixedText = broken'.Replace("3 * scale * r", "3 * scale r")
                            let text, edited = if broken then broken', fixedText else fixedText, broken'
                            let! p = load [ a, text ]
                            let _, second, kept, results = finishTwice p text edited (fun decls i -> decls[i].Key.Name = "area")
                            Expect.equal kept 2 "scale and g are given to the second finish again"
                            Expect.isFalse (results |> Array.exists _.DependsOnLaterDeclarations) "no result depends on later declarations"
                            do! expectFresh p edited second $"area %s{what}"
                            Expect.equal second.Reused 1 "only scale is reused: g's lambda is left to the defaults"
                        } ]

          // M2: a finish's lazy check results are built from the state that finish gave, never during a later undo of it
          testTask "a superseded finish's check results are built from its own finish" {
              let! p = load [ a, withDiagnostics ]
              let parse = DeclarationChecker.Parse(p.Context, a, withDiagnostics)
              let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
              let decls = DeclarationChecker.Split parse
              let results = checkAll start env0 decls (fun _ -> None)
              let first = DeclarationChecker.FinishFile(start, results, parse)
              let second = DeclarationChecker.FinishFile(start, results, parse)
              let! expected = interactive p withDiagnostics
              let uses (r: FSharpCheckFileResults) = r.GetAllUsesOfAllSymbolsInFile() |> Seq.map (fun u -> u.Symbol.FullName, string u.Range) |> Seq.sort |> List.ofSeq
              // The first finish's, read after the second undid it, while declaration checks of the FileStart undo the
              // second: the builds take the FileStart's lock, so each runs between two of them
              let rechecks =
                  Task.Run(fun () ->
                      for _ in 1..20 do
                          DeclarationChecker.CheckDeclaration(start, env0, decls[0], CancellationToken.None) |> ignore)

              let firstResults = first.CheckResults
              let secondResults = second.CheckResults
              do! rechecks
              Expect.equal (uses firstResults) (uses expected) "the first finish's symbol uses are the full check's"
              Expect.equal (uses secondResults) (uses expected) "and the second's"
              Expect.equal (compiledNames firstResults) (compiledNames expected) "as are its compiled names"
              Expect.equal firstResults.Diagnostics first.Diagnostics "the results carry their own finish's diagnostics"
          }
          // M3: the compiled names the implementation file hands out are given under the file's lock, at the build
          testTask "compiled names in the implementation file are given at the build" {
              let text = "module Deps.A\n\nlet first, second = 1, 2\n\nlet third, fourth = 3, 4\n\nlet adder = fun (x: int) -> x + 1\n"
              let! p = load [ a, text ]
              let reference = compiledNames (fresh p text).CheckResults
              Expect.exists reference (fun n -> n.Contains "@") "a generated compiled name"

              for round in 1..5 do
                  let finished = fresh p text
                  let results = finished.CheckResults
                  // Checks of the file reset its counters; a name first read after them would be numbered from there
                  for _ in 1..3 do
                      DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text)) |> ignore

                  let reads = [ for _ in 1..4 -> Task.Run(fun () -> compiledNames results) ]
                  let check = Task.Run(fun () -> DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text)) |> ignore)
                  let! names = Task.WhenAll reads
                  do! check

                  for n in names do
                      Expect.equal n reference $"round %d{round}: the compiled names a sequential build gives"
          } ]