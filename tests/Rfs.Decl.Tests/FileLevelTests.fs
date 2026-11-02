module Rfs.Decl.Tests.FileLevelTests

open System
open System.IO
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
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
let private expectScopedEqualsChain (project: string) =
    task {
        let! p = load project []
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
          } ]
