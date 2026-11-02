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

let private sameDiagnostics (expected: FSharpDiagnostic[]) (actual: FSharpDiagnostic[]) =
    Diagnostics.compare (toLsp expected) (toLsp actual)

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
              let! p = load depsProject []
              let parses = parseAll p
              let dependencies = DeclarationChecker.FileDependencies(p.Context, parses)
              let whole = chain p parses

              Expect.exists
                  (Array.indexed dependencies)
                  (fun (idx, deps) -> deps.Length < idx)
                  "some file depends on fewer than all earlier files"

              for idx, f in Array.indexed whole do
                  match f.Result with
                  | Some chained ->
                      let before = [ for dep in dependencies[idx] -> whole[dep].Signature ]
                      let scoped = DeclarationChecker.CheckFile(p.Context, before, parses[idx])
                      let diff = sameDiagnostics chained.Diagnostics scoped.Diagnostics
                      let name = Path.GetFileName f.Parse.FileName
                      Expect.isEmpty diff $"%s{name}: the setup from %A{dependencies[idx]} gives the chain's diagnostics:\n%s{String.concat Environment.NewLine diff}"
                  | None -> ()
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

              let refusal (check: unit -> unit) =
                  try
                      check ()
                      failtest "the setup was accepted"
                  with :? ArgumentException as e ->
                      e.Message

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
              let whole = chain p parses

              let jobs =
                  [| for idx, f in Array.indexed whole do
                         match f.Result with
                         | Some result -> yield idx, [ for w in whole[.. idx - 1] -> w.Signature ], result.Diagnostics
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
