module Rfs.Decl.Tests.FileLevelTests

open System
open System.IO
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private depsDir = Path.Combine(Path.GetDirectoryName tinyDir, "Deps")
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")

/// One oracle process for the whole run; it answers one request at a time.
let private oracleClient = lazy (OracleClient.start ())

type private Project =
    { Spec: ProjectSpec
      Context: ProjectContext
      Texts: Map<string, string> }

    member p.FileNamed(name: string) =
        p.Spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = name)

/// A context of its own for each call: the #nowarn scopes a parse records are the project's.
let private load (project: string) (overrides: (string * string) list) =
    async {
        let! spec = ProjectSpec.load project |> Async.AwaitTask
        let spec = ProjectSpec.sequential spec
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)

        return
            { Spec = spec
              Context = ctx
              Texts = sources spec (Map.ofList overrides) }
    }

let private parseAll (p: Project) =
    [| for f in p.Spec.SourceFiles -> DeclarationChecker.Parse(p.Context, f, p.Texts[f]) |]

type private WholeFile =
    { Parse: FSharpParseFileResults
      /// None for a signature file
      Result: FileResult option
      Signature: FileSignature }

/// Every file checked whole, each after every earlier file.
let private chain (p: Project) (parses: FSharpParseFileResults[]) =
    (([], []), parses)
    ||> Array.fold (fun (files, before) parse ->
        if parse.FileName.EndsWith ".fsi" then
            let signature = DeclarationChecker.CheckSignatureFile(p.Context, before, parse)
            files @ [ { Parse = parse; Result = None; Signature = signature } ], before @ [ signature ]
        else
            let result = DeclarationChecker.CheckFile(p.Context, before, parse)

            files @ [ { Parse = parse; Result = Some result; Signature = result.Signature } ],
            before @ [ result.Signature ])
    |> fst
    |> Array.ofList

let private fixture (name: string) =
    Path.Combine(Path.GetDirectoryName tinyDir, name, name + ".fsproj")

/// Every file checked after only the files it depends on, from the signatures this pipeline gave them, as a graph
/// that sets files up from their dependencies does.
let private scopedPipeline (p: Project) (parses: FSharpParseFileResults[]) (dependencies: int[][]) =
    let signatures = Array.zeroCreate<FileSignature> parses.Length

    [| for idx, parse in Array.indexed parses do
           let before = [ for dep in dependencies[idx] -> signatures[dep] ]

           if parse.FileName.EndsWith ".fsi" then
               signatures[idx] <- DeclarationChecker.CheckSignatureFile(p.Context, before, parse)
               yield { Parse = parse; Result = None; Signature = signatures[idx] }
           else
               let result = DeclarationChecker.CheckFile(p.Context, before, parse)
               signatures[idx] <- result.Signature
               yield { Parse = parse; Result = Some result; Signature = result.Signature } |]

/// The oracle's diagnostics for one file of the project with these texts.
let private oracleFor (p: Project) (file: string) =
    task {
        let! oracle = oracleClient.Value
        let! all = oracle.Check p.Spec.ProjectFile p.Texts
        let file = LspClient.NormalizePath file
        return all |> List.filter (fun d -> String.Equals(LspClient.NormalizePath d.File, file, StringComparison.OrdinalIgnoreCase))
    }

let private expectOracle (p: Project) (file: string) (actual: FSharpDiagnostic[]) (what: string) =
    task {
        let! expected = oracleFor p file
        let diff = Diagnostics.compare expected (toLsp actual)
        Expect.isEmpty diff $"%s{Path.GetFileName file} (%s{what}) equals the oracle:\n%s{String.concat Environment.NewLine diff}"
        return expected
    }

/// The message of the ArgumentException the call raises.
let private refusal (check: unit -> unit) =
    try
        check ()
        failtest "the call was accepted"
    with :? ArgumentException as e ->
        e.Message

let private sameDiagnostics (expected: FSharpDiagnostic[]) (actual: FSharpDiagnostic[]) =
    Diagnostics.compare (toLsp expected) (toLsp actual)

/// The project checked through the scoped pipeline: every implementation file's diagnostics equal the chain's and
/// the oracle's, and every file's signature fingerprint equals the chain's.
let private expectScopedEqualsChainWith (project: string) (overrides: (string * string) list) =
    task {
        let! p = load project overrides
        let parses = parseAll p
        let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
        let whole = chain p parses
        let scoped = scopedPipeline p parses dependencies

        for idx in 0 .. parses.Length - 1 do
            let name = Path.GetFileName parses[idx].FileName
            let chainedPrint, scopedPrint = whole[idx].Signature.Fingerprint, scoped[idx].Signature.Fingerprint

            if not (chainedPrint.StartsWith "?" && scopedPrint.StartsWith "?") then
                Expect.equal scopedPrint chainedPrint $"%s{name}: the scoped signature equals the chain's"

            match whole[idx].Result, scoped[idx].Result with
            | Some chained, Some scopedResult ->
                let diff = sameDiagnostics chained.Diagnostics scopedResult.Diagnostics

                Expect.isEmpty
                    diff
                    $"%s{name}: the setup from %A{dependencies[idx]} gives the chain's diagnostics:\n%s{String.concat Environment.NewLine diff}"

                do! expectOracle p parses[idx].FileName scopedResult.Diagnostics "scoped CheckFile" :> Task
            | _ -> ()

        return p, dependencies
    }

let private expectScopedEqualsChain (project: string) = expectScopedEqualsChainWith project []

/// BeginFile, a fold of CheckDeclaration and FinishFile over these parses, after `before`.
let private byDeclarations (p: Project) (before: FileSignature list) (parse: FSharpParseFileResults) =
    let start, env0 = DeclarationChecker.BeginFile(p.Context, before, parse)

    let results =
        DeclarationChecker.Split parse
        |> Array.mapFold
            (fun env decl ->
                let result = DeclarationChecker.CheckDeclaration(start, env, decl, Threading.CancellationToken.None)
                result, result.EnvAfter)
            env0
        |> fst

    DeclarationChecker.FinishFile(start, results, parse)

let private lineDirective = "module Tiny.C\n\n# 1 \"elsewhere.fsy\"\nlet pair = [ 1, 2 ]\n"
let private noLineDirective = "module Tiny.C\n\nlet pair = [ 1, 2 ]\n"

/// Tiny's C.fs with this text, checked whole and one declaration at a time, each compared with the oracle.
let private checkTinyC (text: string) =
    task {
        let! p = load tinyProject [ "C.fs", text ]
        let file = p.FileNamed "C.fs"
        let whole = chain p (parseAll p) |> Array.find (fun f -> f.Parse.FileName = file)
        let wholeDiagnostics = whole.Result.Value.Diagnostics
        let byDecl = declByDecl p.Context p.Spec p.Texts |> List.find (fun f -> f.File = file)
        let byDeclDiagnostics = byDecl.FileResult.Diagnostics
        let! expected = expectOracle p file wholeDiagnostics "CheckFile"
        let! _ = expectOracle p file byDeclDiagnostics "FinishFile"
        return expected, wholeDiagnostics, byDeclDiagnostics
    }


/// A.fs checked by declaration: its FileStart, each declaration's environment before it and result, and the FileResult
/// of finishing those results.
let private finishByDeclaration (p: Project) (parse: FSharpParseFileResults) =
    let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
    let decls = DeclarationChecker.Split parse

    let checks, _ =
        (env0, decls)
        ||> Array.mapFold (fun env decl ->
            let result = DeclarationChecker.CheckDeclaration(start, env, decl, Threading.CancellationToken.None)
            (env, result), DeclarationChecker.ExtendEnv(env, result))

    start, decls, checks, DeclarationChecker.FinishFile(start, Array.map snd checks, parse)

/// B.fs checked whole and by declaration after the given signature of A.fs, the only file it depends on.
let private checkBAfter (p: Project) (parseB: FSharpParseFileResults) (a: FileSignature) =
    let whole = DeclarationChecker.CheckFile(p.Context, [ a ], parseB)
    let start, env0 = DeclarationChecker.BeginFile(p.Context, [ a ], parseB)

    let results =
        DeclarationChecker.Split parseB
        |> Array.mapFold
            (fun env decl ->
                let r = DeclarationChecker.CheckDeclaration(start, env, decl, Threading.CancellationToken.None)
                r, DeclarationChecker.ExtendEnv(env, r))
            env0
        |> fst

    whole, DeclarationChecker.FinishFile(start, results, parseB)

/// The fork's InteractiveChecker over the project's texts: the per-file check results a server builds today.
let private interactive (p: Project) =
    let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions p.Spec)
    let sourceReader f = let t = p.Texts[f] in hash t, lazy t

    fun (file: string) ->
        async {
            let! _, checkResults, _ = checker.ParseAndCheckFileInProject(p.Spec.ProjectFile, Array.ofList p.Spec.SourceFiles, sourceReader, file)
            return checkResults
        }

/// The message of the InvalidOperationException reading the results raises, if any.
let private refusedResults (result: FileResult) =
    try
        result.CheckResults |> ignore
        None
    with :? InvalidOperationException as e ->
        Some e.Message

[<Tests>]
let fileLevelTests =
    testList
        "FileLevel"
        [ testTask "file check equals oracle" {
              for project in [ tinyProject; depsProject ] do
                  let! p = load project []

                  for f in chain p (parseAll p) do
                      match f.Result with
                      | Some result -> do! expectOracle p f.Parse.FileName result.Diagnostics "CheckFile" :> Task
                      | None -> ()
          }

          testTask "dependency-scoped setup equals chain setup" {
              let! _, (dependencies: int[][]) = expectScopedEqualsChain depsProject

              Expect.exists
                  (Array.indexed dependencies)
                  (fun (idx, deps: int[]) -> deps.Length < idx)
                  "some file depends on fewer than all earlier files"
          }

          testTask "scoped BeginFile, CheckDeclaration and FinishFile equal the chain setup" {
              let! p = load depsProject []
              let parses = parseAll p
              let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
              let whole = chain p parses

              for idx, f in Array.indexed whole do
                  match f.Result with
                  | Some chained ->
                      let name = Path.GetFileName f.Parse.FileName
                      let scoped = byDeclarations p [ for dep in dependencies[idx] -> whole[dep].Signature ] parses[idx]
                      let diff = sameDiagnostics chained.Diagnostics scoped.Diagnostics
                      Expect.isEmpty diff $"%s{name}: gives the chain's diagnostics:\n%s{String.concat Environment.NewLine diff}"
                      Expect.equal scoped.Signature.Fingerprint chained.Signature.Fingerprint $"%s{name}: the same fingerprint"
                      do! expectOracle p f.Parse.FileName scoped.Diagnostics "scoped FinishFile" :> Task
                  | None -> ()
          }

          testTask "scoped setup deduplicates module names as the whole project does" {
              let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChain (fixture "SameName")
              let bUtil = p.Spec.SourceFiles |> List.findIndex (fun f -> f.EndsWith(Path.Combine("b", "Util.fs")))
              Expect.isEmpty dependencies[bUtil] "b/Util.fs depends on nothing, so a/Util.fs's name is not in its setup"
          }

          testTask "duplicate entities depend on each other" {
              let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChain (fixture "Dups")
              let indexOf name = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = name)
              Expect.contains dependencies[indexOf "Dup2.fs"] (indexOf "Dup1.fs") "a second module Dup depends on the first"
              Expect.contains dependencies[indexOf "T2.fs"] (indexOf "T1.fs") "a second type N.T depends on the first"
              let! dup2 = oracleFor p (p.FileNamed "Dup2.fs")
              Expect.exists dup2 (fun d -> d.Code = "FS0239") "the oracle reports the duplicate module"
              // The check reports nothing for the second N.T (the clash surfaces only when compiling); the edge stays
          }

          testTask "a module named like another's suffixed name depends on it" {
              // X.fs's module X sits next to type X, so it compiles as S.XModule, the name Other.fs declares
              let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChain (fixture "Suffix")
              let indexOf name = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = name)
              Expect.contains dependencies[indexOf "Other.fs"] (indexOf "X.fs") "module XModule depends on the file whose module X is XModule"
          }

          testTask "scoped setup covers the dependency graph's shapes" {
              // A ghost namespace, a namespace split across files, a type extension in another file, fully qualified
              // uses without open, an AutoOpen module and a .fsi before its dependant
              let! _ = expectScopedEqualsChain (fixture "Graph")
              ()
          }

          testTask "stale dependency list is refused" {
              let! p = load depsProject []
              let parses = parseAll p
              let indexOf name = parses |> Array.findIndex (fun parse -> Path.GetFileName parse.FileName = name)
              let e, d = indexOf "E.fs", indexOf "D.fs"
              let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
              Expect.isFalse (Array.contains d dependencies[e]) "E.fs does not depend on D.fs"
              let whole = chain p parses
              let scopedBefore (deps: int[]) (whole: WholeFile[]) = [ for dep in deps -> whole[dep].Signature ]
              DeclarationChecker.CheckFile(p.Context, scopedBefore dependencies[e] whole, parses[e]) |> ignore

              // D.fs now puts a module in namespace Deps, which E.fs is in
              let newD = DeclarationChecker.Parse(p.Context, parses[d].FileName, "namespace Deps\n\n[<AutoOpen>]\nmodule DHelpers =\n    let extra = 1\n")

              let stale =
                  refusal (fun () -> DeclarationChecker.CheckFile(p.Context, scopedBefore dependencies[e] whole, parses[e]) |> ignore)

              Expect.stringContains stale "FileDependencies" "the list from before D.fs changed is refused"

              let parses = Array.updateAt d newD parses
              let current = DeclarationChecker.FileDependencies(p.Context, parses)
              Expect.contains current[e] d "E.fs now depends on D.fs"
              let whole = chain p parses

              let missing =
                  refusal (fun () -> DeclarationChecker.CheckFile(p.Context, scopedBefore dependencies[e] whole, parses[e]) |> ignore)

              Expect.stringContains missing "D.fs" "the old setup misses D.fs"
              DeclarationChecker.CheckFile(p.Context, scopedBefore current[e] whole, parses[e]) |> ignore
          }

          testTask "dependencies need every source file in order" {
              let! p = load depsProject []
              let parses = parseAll p

              refusal (fun () -> DeclarationChecker.FileDependencies(p.Context, parses[1..]) |> ignore)
              |> ignore

              refusal (fun () -> DeclarationChecker.FileDependencies(p.Context, Array.rev parses) |> ignore)
              |> ignore
          }

          testTask "dependencies are transitive and earlier" {
              let! p = load depsProject []
              let parses = parseAll p
              let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
              let indexOf name = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = name)

              for idx, deps in Array.indexed dependencies do
                  Expect.equal deps (Array.sort (Array.distinct deps)) $"file %d{idx}: ascending, no duplicates"
                  Expect.all deps (fun dep -> dep < idx) $"file %d{idx}: only earlier files"

                  for dep in deps do
                      Expect.containsAll deps dependencies[dep] $"file %d{idx}: holds what file %d{dep} depends on"

              let last = dependencies[dependencies.Length - 1]
              let middle = indexOf "C.fs"
              Expect.contains last middle "E.fs depends on C.fs (namespace Deps holds its AutoOpen module)"
              Expect.containsAll last [ indexOf "A.fs"; indexOf "B.fs" ] "E.fs depends on what C.fs depends on"
              Expect.equal dependencies[middle] [| indexOf "A.fs"; indexOf "B.fs" |] "C.fs depends on A.fs and B.fs"
          }

          testTask "a setup missing a dependency is refused" {
              let! p = load depsProject []
              let parses = parseAll p
              DeclarationChecker.FileDependencies(p.Context, parses) |> ignore
              let c = parses |> Array.findIndex (fun parse -> Path.GetFileName parse.FileName = "C.fs")

              let checkFile = refusal (fun () -> DeclarationChecker.CheckFile(p.Context, [], parses[c]) |> ignore)
              let beginFile = refusal (fun () -> DeclarationChecker.BeginFile(p.Context, [], parses[c]) |> ignore)
              Expect.stringContains checkFile "A.fs" "CheckFile names the missing file"
              Expect.stringContains beginFile "A.fs" "BeginFile names the missing file"
          }

          testTask "promotion keeps the signature fingerprint" {
              let! p = load depsProject []
              let whole = chain p (parseAll p)
              let byDecl = declByDecl p.Context p.Spec p.Texts

              for f in byDecl do
                  let checkedWhole = whole |> Array.find (fun w -> w.Parse.FileName = f.File)
                  let name = Path.GetFileName f.File
                  Expect.isFalse (checkedWhole.Signature.Fingerprint.StartsWith "?") $"%s{name}: describable"
                  Expect.equal f.FileResult.Signature.Fingerprint checkedWhole.Signature.Fingerprint $"%s{name}: CheckFile and FinishFile agree"

              let fsi = whole |> Array.find (fun w -> w.Parse.FileName.EndsWith "E.fsi")
              let fs = whole |> Array.find (fun w -> w.Parse.FileName.EndsWith "E.fs")
              Expect.equal fs.Signature.Fingerprint fsi.Signature.Fingerprint "E.fs has E.fsi's fingerprint"
          }

          testTask "body edit keeps the file fingerprint; return-type edit changes it" {
              let aText = File.ReadAllText(Path.Combine(depsDir, "A.fs"))
              let scale = "let scale (x: int) : int = x * 2"

              let fingerprintOf (text: string) =
                  task {
                      let! p = load depsProject [ "A.fs", text ]
                      let a = p.FileNamed "A.fs"
                      let result = DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text))
                      return result.Signature.Fingerprint
                  }

              Expect.stringContains aText scale "A.fs has scale"
              let! original = fingerprintOf aText
              let! body = fingerprintOf (aText.Replace(scale, "let scale (x: int) : int = x * 3"))
              let! returnType = fingerprintOf (aText.Replace(scale, "let scale (x: int) : int64 = int64 x * 2L"))
              Expect.equal body original "a body edit keeps it"
              Expect.notEqual returnType original "a return-type edit changes it"
          }

          testTask "parallel file checks equal sequential" {
              let! p = load depsProject []
              let parses = parseAll p
              let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
              let whole = chain p parses

              // Each implementation file twice: after every earlier file, and after only its dependencies
              let jobs =
                  [| for idx, f in Array.indexed whole do
                         match f.Result with
                         | Some result ->
                             yield idx, [ for w in whole[.. idx - 1] -> w.Signature ], result.Diagnostics
                             yield idx, [ for dep in dependencies[idx] -> whole[dep].Signature ], result.Diagnostics
                         | None -> () |]

              for round in 1..20 do
                  let! results =
                      jobs
                      |> Array.map (fun (idx, before, _) -> Task.Run(fun () -> DeclarationChecker.CheckFile(p.Context, before, parses[idx])))
                      |> Task.WhenAll

                  for (idx, _, expected), (actual: FileResult) in Array.zip jobs results do
                      let diff = sameDiagnostics expected actual.Diagnostics
                      Expect.isEmpty diff $"round %d{round}, %s{Path.GetFileName parses[idx].FileName}:\n%s{String.concat Environment.NewLine diff}"
          }

          // Sequenced: rendering applies the #line directives of the latest parse of the file in the process (FCS keeps
          // them in a global store by file), and the other tests parse other texts of Tiny's C.fs.
          testSequenced
          <| testTask "line-directive diagnostics outside the project are dropped" {
              let! expected, whole, byDecl = checkTinyC lineDirective
              let elsewhere (d: FSharpDiagnostic) = d.FileName.EndsWith "elsewhere.fsy"
              Expect.isFalse (expected |> List.exists (fun (d: Diagnostic) -> d.File.EndsWith "elsewhere.fsy")) "the oracle drops it"
              Expect.isFalse (Array.exists elsewhere whole) "CheckFile drops it"
              Expect.isFalse (Array.exists elsewhere byDecl) "FinishFile drops it"

              // The control: the same warning without the directive is reported by both
              let! expected, whole, byDecl = checkTinyC noLineDirective
              let fs3886 (d: FSharpDiagnostic) = d.ErrorNumber = 3886
              Expect.isTrue (expected |> List.exists (fun (d: Diagnostic) -> d.Code = "FS3886")) "the oracle reports it"
              Expect.isTrue (Array.exists fs3886 whole) "CheckFile reports it"
              Expect.isTrue (Array.exists fs3886 byDecl) "FinishFile reports it"
          }
          // I1 (Task 3 fix round 1): does a FileSignature FinishFile returned stay valid after a later CheckDeclaration
          // on the same FileStart runs UndoLastFinish, and after the file is finished again?
          testTask "a finished file's signature stays valid after its file is re-checked and finished again" {
              // A.fs gains a declaration that B.fs uses, as in the Declarations-mode fuzz failure (seed 4242, step 25)
              let a = Path.Combine(depsDir, "A.fs")
              let b = Path.Combine(depsDir, "B.fs")
              let aText = File.ReadAllText(a) + "\nlet start = \"text\"\n"
              let bText = File.ReadAllText(b) + "\nlet usesStart = start + \"!\"\n"
              let! p = load depsProject [ a, aText; b, bText ]
              let parses = parseAll p
              let parseA = parses |> Array.find (fun x -> x.FileName = a)
              let parseB = parses |> Array.find (fun x -> x.FileName = b)
              let fresh = chain p parses |> Array.find (fun f -> f.Parse.FileName = b)
              let freshB = fresh.Result.Value
              let! _ = expectOracle p b freshB.Diagnostics "B.fs from a fresh chain"

              let start, decls, checks, first = finishByDeclaration p parseA
              let s1 = first.Signature

              let expectB (what: string) (a: FileSignature) =
                  let whole, byDecl = checkBAfter p parseB a
                  let d1 = sameDiagnostics freshB.Diagnostics whole.Diagnostics
                  Expect.isEmpty d1 $"%s{what}: CheckFile of B.fs equals the fresh chain's:\n%s{String.concat Environment.NewLine d1}"
                  let d2 = sameDiagnostics freshB.Diagnostics byDecl.Diagnostics
                  Expect.isEmpty d2 $"%s{what}: B.fs by declaration equals the fresh chain's:\n%s{String.concat Environment.NewLine d2}"
                  Expect.equal whole.Signature.Fingerprint freshB.Signature.Fingerprint $"%s{what}: B.fs's signature fingerprint"

              expectB "S1, just finished" s1

              for k in 0 .. decls.Length - 1 do
                  let env, _ = checks[k]
                  // Runs UndoLastFinish on A.fs's FileStart
                  let again = DeclarationChecker.CheckDeclaration(start, env, decls[k], Threading.CancellationToken.None)
                  expectB $"S1 after re-checking %s{decls[k].Key.Name}, before finishing again" s1
                  let results = checks |> Array.mapi (fun i (_, r) -> if i = k then again else r)
                  let finished = DeclarationChecker.FinishFile(start, results, parseA)
                  Expect.equal finished.Signature.Fingerprint s1.Fingerprint $"re-finishing after %s{decls[k].Key.Name} gives S1's fingerprint"
                  expectB $"S1 after re-checking %s{decls[k].Key.Name} and finishing again" s1
                  expectB $"the new signature after %s{decls[k].Key.Name}" finished.Signature
          }

          testTask "cold check builds no check results until asked" {
              let! p = load depsProject []
              let parses = parseAll p
              let scoped = scopedPipeline p parses (DeclarationChecker.FileDependencies(p.Context, parses))
              Expect.equal p.Context.CheckResultsBuilt 0 "CheckFile builds no sink and no check results"
              let results = scoped |> Array.choose (fun f -> f.Result)
              let first = results[0]
              let! (built: FSharpCheckFileResults[]) =
                  Array.init 8 (fun _ -> Task.Run<FSharpCheckFileResults>(fun () -> first.CheckResults)) |> Task.WhenAll

              Expect.equal p.Context.CheckResultsBuilt 1 "the first file's results are built once, though asked from 8 threads at once"
              Expect.all built (fun r -> obj.ReferenceEquals(r, built[0])) "every thread gets the same results"
              first.CheckResults |> ignore
              Expect.equal p.Context.CheckResultsBuilt 1 "a later read reuses them"
              Expect.equal first.CheckResults.Diagnostics first.Diagnostics "the results carry the check's diagnostics"
          }

          testTask "lazy check results equal eager ones" {
              let! p = load depsProject []
              let parses = parseAll p
              let scoped = scopedPipeline p parses (DeclarationChecker.FileDependencies(p.Context, parses))
              let reference = interactive p

              for f in scoped do
                  match f.Result with
                  | Some result ->
                      let file = f.Parse.FileName
                      let! expected = reference file
                      expectLikeReference 5 p.Texts[file] expected result.CheckResults (Path.GetFileName file)
                  | None -> ()
          }

          // Sequenced: the file's line directives are the latest parse's in the process, as for the R18 test
          testSequenced
          <| testTask "lazy results after a newer parse are correct or refused" {
              let! p = load tinyProject [ "C.fs", lineDirective ]
              let parses = parseAll p
              let file = p.FileNamed "C.fs"
              let idx = parses |> Array.findIndex (fun parse -> parse.FileName = file)
              let before = chain p parses |> Array.take idx |> Array.map (fun f -> f.Signature) |> List.ofArray
              let first = DeclarationChecker.CheckFile(p.Context, before, parses[idx])
              let second = DeclarationChecker.CheckFile(p.Context, before, parses[idx])

              // A newer text without the directive replaces the file's line directives
              DeclarationChecker.Parse(p.Context, file, noLineDirective) |> ignore
              let refused = refusedResults first
              Expect.isSome refused "built after a newer parse with other line directives, the results are refused"
              Expect.stringContains refused.Value "line directives" "the refusal says why"
              Expect.equal (refusedResults first) refused "and stay refused"
              Expect.equal p.Context.CheckResultsBuilt 0 "a refused build checks nothing"

              // The same text again gives the file the directives it had: the other result builds, and is right
              DeclarationChecker.Parse(p.Context, file, lineDirective) |> ignore
              let! expected = interactive p file
              expectLikeReference 1 lineDirective expected second.CheckResults "C.fs with a line directive"
              Expect.equal p.Context.CheckResultsBuilt 1 "it is built"

              // A text without directives builds after a newer parse that has none either
              let! p = load tinyProject [ "C.fs", noLineDirective ]
              let parses = parseAll p
              let whole = chain p parses
              let result = whole[idx].Result.Value
              DeclarationChecker.Parse(p.Context, file, "module Tiny.C\n\nlet other = Tiny.B.two\n") |> ignore
              let! expected = interactive p file
              expectLikeReference 1 noLineDirective expected result.CheckResults "C.fs without a line directive"
          }

          testTask "sinkless diagnostics equal the oracle" {
              let chains = fixture "Chains"
              let chainsDir = Path.GetDirectoryName chains
              // FS0039 with a suggestion, and FS1182, which the fixtures turn on
              let suggestion = File.ReadAllText(Path.Combine(chainsDir, "B.fs")) + "\nlet shift = 99\n\nlet added15 = shifted\n"
              let unused = File.ReadAllText(Path.Combine(chainsDir, "D.fs")) + "\nlet useless () =\n    let unused = 5\n    1\n"

              for project, overrides in [ tinyProject, []; depsProject, []; chains, []; chains, [ "B.fs", suggestion; "D.fs", unused ] ] do
                  let! p = load project overrides
                  let parses = parseAll p
                  let whole = chain p parses
                  let scoped = scopedPipeline p parses (DeclarationChecker.FileDependencies(p.Context, parses))

                  for f in Array.append whole scoped do
                      match f.Result with
                      | Some result -> do! expectOracle p f.Parse.FileName result.Diagnostics "sinkless CheckFile" :> Task
                      | None -> ()

                  Expect.equal p.Context.CheckResultsBuilt 0 $"%s{Path.GetFileName project}: no file was checked with a sink"

                  if not overrides.IsEmpty then
                      let! b = oracleFor p (p.FileNamed "B.fs")
                      let! d = oracleFor p (p.FileNamed "D.fs")

                      Expect.exists
                          b
                          (fun d -> d.Code = "FS0039" && d.Message.Contains "Maybe you want one of the following")
                          "B.fs has an FS0039 with suggestions"

                      Expect.exists d (fun d -> d.Code = "FS1182") "D.fs has an FS1182"
          }

          // The compiler-generated-name counters are the project's, by file: a lazy build of one text's results must
          // not move the names a check of a newer text of the same file gives at the same time
          testTask "a lazy build beside a check of the same file leaves its fingerprint alone" {
              let holders =
                  [ for i in 1..150 ->
                        $"type Holder%d{i}() as this =\n    let value = %d{i}\n    member _.V = value\n    member _.Self = this\n\n" ]
                  |> String.concat ""

              let older = "module Deps.A\n\n" + holders
              let newer = older + "let extra = 1\n"
              let! p = load depsProject [ "A.fs", older ]
              let a = p.FileNamed "A.fs"
              let fresh = DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, newer)).Signature.Fingerprint
              let clock = Diagnostics.Stopwatch.StartNew()
              DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, newer)) |> ignore
              let checkMs = int clock.ElapsedMilliseconds
              Expect.isFalse (fresh.StartsWith "?") "the signature is describable"
              Expect.stringContains fresh "@" "the signature holds compiler-generated names"
              let mutable moved = []

              // Lazy builds of older results start while the newer text is checked, staggered over the time a check
              // takes, so that some reset the counters while the check is still giving names
              for round in 1..10 do
                  let olds =
                      [ for _ in 1..8 -> DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, older)) ]

                  let parse = DeclarationChecker.Parse(p.Context, a, newer)
                  // Threads of their own, so the thread pool's growth does not delay them
                  let onThread (work: unit -> 'T) = Task.Factory.StartNew<'T>(work, TaskCreationOptions.LongRunning)
                  let check = onThread (fun () -> DeclarationChecker.CheckFile(p.Context, [], parse).Signature.Fingerprint)

                  let builds =
                      olds
                      |> List.mapi (fun k old ->
                          onThread (fun () ->
                              Threading.Thread.Sleep(k * checkMs / 6)
                              old.CheckResults |> ignore))

                  let! fingerprint = check
                  let! _ = Task.WhenAll builds

                  if fingerprint <> fresh then
                      moved <- round :: moved

              Expect.isEmpty moved "the newer text's fingerprint equals a sequential check's in every round"
          }

          // Task 4: a promotion builds a file by declaration while edits still check it whole. The two share the file's
          // compiler-generated-name counters, which BeginFile and CheckFile reset, so each check of the file holds its lock
          testTask "a declaration build beside a whole check of the same file leaves both fingerprints alone" {
              let holders =
                  [ for i in 1..150 ->
                        $"type Holder%d{i}() as this =\n    let value = %d{i}\n    member _.V = value\n    member _.Self = this\n\n" ]
                  |> String.concat ""

              let text = "module Deps.A\n\n" + holders
              let! p = load depsProject [ "A.fs", text ]
              let a = p.FileNamed "A.fs"
              let fresh = DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text)).Signature.Fingerprint
              Expect.stringContains fresh "@" "the signature holds compiler-generated names"
              // Each is compared with its own sequential check: a declaration's names are numbered from the declaration
              let freshByDeclarations = (byDeclarations p [] (DeclarationChecker.Parse(p.Context, a, text))).Signature.Fingerprint
              Expect.stringContains freshByDeclarations "@" "the declarations' signature holds compiler-generated names"
              let clock = Diagnostics.Stopwatch.StartNew()
              byDeclarations p [] (DeclarationChecker.Parse(p.Context, a, text)) |> ignore
              let buildMs = int clock.ElapsedMilliseconds
              let onThread (work: unit -> 'T) = Task.Factory.StartNew<'T>(work, TaskCreationOptions.LongRunning)
              let mutable moved = []

              for round in 1..10 do
                  let parse = DeclarationChecker.Parse(p.Context, a, text)
                  let build = onThread (fun () -> (byDeclarations p [] parse).Signature.Fingerprint)

                  // Whole checks start over the time the build takes, so that some reset the counters mid-build
                  let wholes =
                      [ for k in 0..5 ->
                            onThread (fun () ->
                                Threading.Thread.Sleep(k * buildMs / 5)
                                DeclarationChecker.CheckFile(p.Context, [], parse).Signature.Fingerprint) ]

                  let! built = build
                  let! whole = Task.WhenAll wholes

                  if built <> freshByDeclarations || whole |> Array.exists ((<>) fresh) then
                      moved <- round :: moved

              Expect.isEmpty moved "the build's and every whole check's fingerprint equal a sequential check's in every round"
          }

          testTask "the fingerprint writes access paths as sprintf does" {
              let corpus = Path.GetFullPath(Path.Combine(tinyDir, "..", "..", "..", "..", "ranvier-fsharp-corpus"))

              let projects =
                  [ for f in [ "Tiny"; "Deps"; "Chains"; "Graph"; "SameName"; "Dups"; "Suffix" ] do
                        yield fixture f
                    yield Path.Combine(corpus, "small", "src", "FSharp.Data.Adaptive", "FSharp.Data.Adaptive.fsproj")
                    yield Path.Combine(corpus, "mid", "src", "FsAutoComplete.Core", "FsAutoComplete.Core.fsproj")
                    yield Path.Combine(corpus, "large", "src", "Fantomas.Core", "Fantomas.Core.fsproj") ]

              let texts = Collections.Generic.HashSet<string * string>()

              for project in projects do
                  let! p = load project []

                  for f in chain p (parseAll p) do
                      for pair in DeclarationChecker.AccessPathTexts f.Signature do
                          texts.Add pair |> ignore

              let mismatches = [ for written, printed in texts do if written <> printed then yield $"%s{written} <> %s{printed}" ]
              Expect.isEmpty mismatches "every access path is written as sprintf \"%A\" gives it"
              Expect.isGreaterThan texts.Count 100 "the corpus has many access paths"
              Expect.exists texts (fun (written, _) -> written.Contains "Namespace") "some path is in a namespace"
              Expect.exists texts (fun (written, _) -> written.Length > 60) "some path is long enough to go to sprintf"
          }

          // Graph mode's ArtificialImplFile: a file set up from a signature file sees it in the implementation
          // environment, so it need not wait for the implementation file's check
          testTask "a file depending on a signature file does not depend on its implementation" {
              for project, user, signature, implementation in
                  [ tinyProject, "C.fs", "B.fsi", "B.fs"; fixture "Graph", "Use.fs", "Lib.fsi", "Lib.fs" ] do
                  let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChain project
                  let indexOf name = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = name)
                  let name = Path.GetFileName project
                  Expect.contains dependencies[indexOf user] (indexOf signature) $"%s{name}: %s{user} depends on %s{signature}"

                  Expect.isFalse
                      (Array.contains (indexOf implementation) dependencies[indexOf user])
                      $"%s{name}: %s{user} does not depend on %s{implementation}"
          }

          // Sequenced: it parses Deps' E.fs with a line directive, which the process keeps for the file
          testSequenced
          <| testTask "refusals have their own exception types" {
              let! p = load depsProject []
              let parses = parseAll p
              DeclarationChecker.FileDependencies(p.Context, parses) |> ignore
              let e = parses |> Array.findIndex (fun parse -> Path.GetFileName parse.FileName = "E.fs")
              let d = parses |> Array.findIndex (fun parse -> Path.GetFileName parse.FileName = "D.fs")
              let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
              let whole = chain p parses
              DeclarationChecker.Parse(p.Context, parses[d].FileName, p.Texts[parses[d].FileName] + "\nlet more = 1\n") |> ignore

              Expect.throwsT<StaleDependenciesException>
                  (fun () -> DeclarationChecker.CheckFile(p.Context, [ for dep in dependencies[e] -> whole[dep].Signature ], parses[e]) |> ignore)
                  "a stale snapshot"

              DeclarationChecker.Parse(p.Context, parses[e].FileName, "# 1 \"elsewhere.fsy\"\n" + p.Texts[parses[e].FileName]) |> ignore

              Expect.throwsT<SupersededLineDirectivesException>
                  (fun () -> DeclarationChecker.CheckFile(p.Context, [ for w in whole[.. e - 1] -> w.Signature ], parses[e]) |> ignore)
                  "another parse's line directives"
          }

          // C1: a signature file's contents reach the implementation environment at its implementation file's place,
          // so a later file's AutoOpen module or extension member still shadows them
          yield!
              [ for name in [ "SigShadow"; "SigShadowNs"; "SigShadowExt" ] ->
                    testTask $"a signature-only setup keeps the file order of what it opens (%s{name})" {
                        let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChain (fixture name)
                        let indexOf file = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = file)

                        Expect.isFalse
                            (Array.contains (indexOf "A.fs") dependencies[indexOf "C.fs"])
                            $"%s{name}: C.fs is set up from A.fsi, without A.fs"

                        let! c = oracleFor p (p.FileNamed "C.fs")
                        Expect.isEmpty c $"%s{name}: the oracle reports nothing for C.fs (B's member shadows A's)"
                    } ]

          // C2: a signature file stands for its implementation file only while their qualified names match
          testTask "a signature-only setup needs the implementation's qualified name to match" {
              let project = fixture "SigQualName"
              let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChain project
              let indexOf file = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = file)
              Expect.isFalse (Array.contains (indexOf "X.fs") dependencies[indexOf "Y.fs"]) "matching names: Y.fs needs only X.fsi"

              for header, text in [ "module Ab", "module Ab\n\nlet x = 1\n"; "no header", "let x = 1\n" ] do
                  let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChainWith project [ "X.fs", text ]
                  Expect.contains dependencies[indexOf "Y.fs"] (indexOf "X.fs") $"%s{header}: Y.fs depends on X.fs"
                  let! y = oracleFor p (p.FileNamed "Y.fs")
                  Expect.exists y (fun d -> d.Code = "FS0039") $"%s{header}: the oracle reports A as not defined in Y.fs"
          }

          // M7 of the Task 4 review: a one-declaration file is recognised without splitting it
          testTask "counting declarations gives Split's count" {
              let corpus = Path.GetFullPath(Path.Combine(tinyDir, "..", "..", "..", "..", "ranvier-fsharp-corpus"))

              let projects =
                  [ for f in [ "Tiny"; "Deps"; "Chains"; "Graph"; "SameName"; "Dups"; "Suffix"; "Rec"; "SigUnmatched" ] do
                        yield fixture f
                    yield Path.Combine(corpus, "large", "src", "Fantomas.Core", "Fantomas.Core.fsproj") ]

              let mutable single = 0

              for project in projects do
                  let! p = load project []

                  for parse in parseAll p do
                      if parse.FileName.EndsWith ".fs" then
                          let count = DeclarationChecker.CountDeclarations parse
                          Expect.equal count (DeclarationChecker.Split parse).Length $"%s{Path.GetFileName parse.FileName}"

                          if count = 1 then
                              single <- single + 1

              Expect.isGreaterThan single 0 "some file is one declaration (module rec)"
          }

          // M2 of the Task 4 review: does re-checking one declaration move the compiler-generated names its finish gives?
          testTask "re-checking a declaration keeps the finish's compiler-generated names" {
              let holders =
                  [ for i in 1..20 ->
                        $"type Holder%d{i}() as this =\n    let value = %d{i}\n    member _.V = value\n    member _.Self = this\n\n" ]
                  |> String.concat ""

              let text = "module Deps.A\n\n" + holders
              let edited = text.Replace("let value = 7\n", "let value = 70\n")
              let! p = load depsProject [ "A.fs", text ]
              let a = p.FileNamed "A.fs"
              let parse = DeclarationChecker.Parse(p.Context, a, text)
              let start, env0 = DeclarationChecker.BeginFile(p.Context, [], parse)
              let decls = DeclarationChecker.Split parse

              let checks, _ =
                  (env0, decls)
                  ||> Array.mapFold (fun env decl ->
                      let result = DeclarationChecker.CheckDeclaration(start, env, decl, Threading.CancellationToken.None)
                      (env, result), DeclarationChecker.ExtendEnv(env, result))

              let before = (DeclarationChecker.FinishFile(start, Array.map snd checks, parse)).Signature.Fingerprint
              Expect.stringContains before "@" "the signature holds compiler-generated names"
              // Holder7 is edited and checked again in the environment it had, on the same FileStart, as a promoted file
              let editedParse = DeclarationChecker.Parse(p.Context, a, edited)
              let editedDecls = DeclarationChecker.Split editedParse
              let k = editedDecls |> Array.findIndex (fun d -> d.Key.Name = "Holder7")
              let env, _ = checks[k]
              let recheck = DeclarationChecker.CheckDeclaration(start, env, editedDecls[k], Threading.CancellationToken.None)
              let results = checks |> Array.mapi (fun i (_, r) -> if i = k then recheck else r)
              let after = (DeclarationChecker.FinishFile(start, results, editedParse)).Signature.Fingerprint
              let fresh = (byDeclarations p [] (DeclarationChecker.Parse(p.Context, a, edited))).Signature.Fingerprint
              Expect.equal after fresh "the finish after the re-check gives a fresh build's fingerprint"
          }

          // T4-C3: an implementation file whose signature file has another qualified name is read by the files after it
          // as a file without a signature, so its root AutoOpen contents shadow what earlier files give
          testTask "an implementation file unmatched by its signature file is visible to the files after it" {
              let project = fixture "SigUnmatched"
              let unmatched = "[<AutoOpen>]\nmodule Ab\n\nlet f = \"s\"\n"

              for header, text, matched in
                  [ "on disk", unmatched, false
                    "module A", "module A\n\nlet x = 1\nlet f = \"s\"\n", true
                    "back to AutoOpen Ab", unmatched, false ] do
                  let! (p: Project), (dependencies: int[][]) = expectScopedEqualsChainWith project [ "X.fs", text ]
                  let indexOf file = p.Spec.SourceFiles |> List.findIndex (fun f -> Path.GetFileName f = file)
                  let! z = oracleFor p (p.FileNamed "Z.fs")

                  if matched then
                      Expect.isFalse (Array.contains (indexOf "X.fs") dependencies[indexOf "Z.fs"]) $"%s{header}: Z.fs does not read X.fs"
                      Expect.exists z (fun d -> d.Code = "FS0001") $"%s{header}: the oracle reports W's f as an int in Z.fs"
                  else
                      Expect.contains dependencies[indexOf "Z.fs"] (indexOf "X.fs") $"%s{header}: Z.fs depends on X.fs"
                      Expect.contains dependencies[indexOf "X.fs"] (indexOf "X.fsi") $"%s{header}: X.fs still depends on X.fsi"
                      Expect.isEmpty z $"%s{header}: the oracle reports nothing for Z.fs (Ab's f shadows W's)"
          }

          // M2 (Task 5): a check numbers its compiler-generated names over what it checks, the whole file or one
          // declaration, so the fingerprint leaves the count out
          testTask "a file checked whole and by declaration gives one fingerprint for its compiler-generated names" {
              let holder i =
                  $"type Holder%d{i}() as this =\n    let value = %d{i}\n    member _.V = value\n    member _.Self = this\n\n"

              let text =
                  "module Deps.A\n\n"
                  + holder 1
                  + holder 2
                  + "let first, second = 1, 2\n\nlet third, fourth = 3, 4\n\nlet adder = fun (x: int) -> x + 1\n"

              let! p = load depsProject [ "A.fs", text ]
              let a = p.FileNamed "A.fs"
              let whole = DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text))
              let byDecl = byDeclarations p [] (DeclarationChecker.Parse(p.Context, a, text))
              Expect.stringContains whole.Signature.Fingerprint "init@" "the signature holds compiler-generated names"
              Expect.equal byDecl.Signature.Fingerprint whole.Signature.Fingerprint "FinishFile's fingerprint is CheckFile's"
          }

          // The lazy FinishFile results and the reads of compiled names in them hold the file's lock, as CheckFile's do
          testTask "a lazy finish build beside checks of the same file leaves their fingerprints alone" {
              let holders =
                  [ for i in 1..150 ->
                        $"type Holder%d{i}() as this =\n    let value = %d{i}\n    member _.V = value\n    member _.Self = this\n    member _.Twice = (fun () -> value * 2) ()\n\n" ]
                  |> String.concat ""

              let text = "module Deps.A\n\n" + holders
              let! p = load depsProject [ "A.fs", text ]
              let a = p.FileNamed "A.fs"
              let fresh = DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text)).Signature.Fingerprint
              let freshByDeclarations = (byDeclarations p [] (DeclarationChecker.Parse(p.Context, a, text))).Signature.Fingerprint
              Expect.stringContains fresh "@" "the signature holds compiler-generated names"
              let clock = Diagnostics.Stopwatch.StartNew()
              DeclarationChecker.CheckFile(p.Context, [], DeclarationChecker.Parse(p.Context, a, text)) |> ignore
              let checkMs = int clock.ElapsedMilliseconds
              let onThread (work: unit -> 'T) = Task.Factory.StartNew<'T>(work, TaskCreationOptions.LongRunning)
              let mutable moved = []

              for round in 1..8 do
                  let parse = DeclarationChecker.Parse(p.Context, a, text)
                  let finished = byDeclarations p [] parse

                  // The build and the queries read the compiled names of the file's values
                  let build =
                      onThread (fun () ->
                          let results = finished.CheckResults

                          for u in results.GetAllUsesOfAllSymbolsInFile() do
                              match u.Symbol with
                              | :? FSharpMemberOrFunctionOrValue as v -> v.CompiledName |> ignore
                              | _ -> ())

                  let checks =
                      [ for k in 0..3 ->
                            onThread (fun () ->
                                Threading.Thread.Sleep(k * checkMs / 4)
                                DeclarationChecker.CheckFile(p.Context, [], parse).Signature.Fingerprint) ]

                  let byDecl = onThread (fun () -> (byDeclarations p [] parse).Signature.Fingerprint)
                  let! whole = Task.WhenAll checks
                  let! declared = byDecl
                  do! build

                  if declared <> freshByDeclarations || whole |> Array.exists ((<>) fresh) then
                      moved <- round :: moved

              Expect.isEmpty moved "every check's fingerprint equals a sequential check's in every round"
          }

          // Go to implementation on a use of a signature file's value reads the range the implementation file's
          // conformance wrote on it: only a check of the implementation file's latest parse writes it
          testTask "go to implementation through a signature gives the current implementation's range" {
              let project = fixture "Graph"
              let! p = load project []
              let lib = p.FileNamed "Lib.fs"
              let user = p.FileNamed "Use.fs"
              let moved = "module Lib\n\n\n\n// moved down\nlet libValue = 41\n\nlet private hidden = 0\n"
              let parses = parseAll p
              let libAt = parses |> Array.findIndex (fun parse -> parse.FileName = lib)
              let userAt = parses |> Array.findIndex (fun parse -> parse.FileName = user)
              let whole = chain p parses
              let before = [ for w in whole[.. libAt - 1] -> w.Signature ]
              let older = parses[libAt]
              let newer = DeclarationChecker.Parse(p.Context, lib, moved)
              DeclarationChecker.CheckFile(p.Context, before, newer) |> ignore
              // A check of the text before it, as a superseded flight or a lazy build of an older result runs
              DeclarationChecker.CheckFile(p.Context, before, older) |> ignore
              let userResult = whole[userAt].Result.Value
              let userText = p.Texts[user]
              let lineText = userText.Split('\n') |> Array.find (fun line -> line.Contains "Lib.libValue")
              let line = (userText.Split('\n') |> Array.findIndex (fun l -> l.Contains "Lib.libValue")) + 1
              let column = lineText.IndexOf "Lib.libValue" + "Lib.libValue".Length
              let goTo (results: FSharpCheckFileResults) =
                  match results.GetDeclarationLocation(line, column, lineText, [ "Lib"; "libValue" ], false) with
                  | FindDeclResult.DeclFound r -> $"%s{Path.GetFileName r.FileName} %O{r}"
                  | other -> sprintf "%A" other
              let! reference = interactive { p with Texts = p.Texts.Add(lib, moved) } user
              let expected = goTo reference
              Expect.stringContains expected "Lib.fs" "the reference goes to Lib.fs"
              Expect.equal (goTo userResult.CheckResults) expected "after a check of the older text"
              let onThread (work: unit -> 'T) = Task.Factory.StartNew<'T>(work, TaskCreationOptions.LongRunning)
              let mutable differed = []

              for round in 1..5 do
                  let others =
                      [ onThread (fun () -> DeclarationChecker.CheckFile(p.Context, before, older) |> ignore)
                        onThread (fun () -> DeclarationChecker.CheckFile(p.Context, [ for w in whole[.. userAt - 1] -> w.Signature ], parses[userAt]) |> ignore) ]

                  for _ in 1..20 do
                      let actual = goTo userResult.CheckResults

                      if actual <> expected then
                          differed <- (round, actual) :: differed

                  do! Task.WhenAll others

              Expect.isEmpty differed "go to implementation gives the current text's range each time"
          } ]
