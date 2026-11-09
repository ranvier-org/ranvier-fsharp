module Rfs.Checker.Tests.SchemeTests

open System
open System.IO
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Tokenization
open Rfs.Checker
open Rfs.Checker.Tests.Deps
open Rfs.Lsp
open Rfs.Projects

/// The compiler options of a project of no project's making: the framework and FSharp.Core Deps builds with
let private baseOptions =
    lazy
        (task {
            let! deps = ProjectSpec.load depsProject

            return
                [ "--noframework"; "--targetprofile:netcore"; "--warn:3"; "--target:library"; "--parallelcompilation-" ]
                @ (deps.OtherOptions
                   |> List.filter (fun o ->
                       o.StartsWith "-r:" && not (o.Contains(Path.DirectorySeparatorChar.ToString() + "tests" + Path.DirectorySeparatorChar.ToString()))))
         })

let private plainSpec (project: string) (files: string list) (options: string list) : ProjectSpec =
    { ProjectFile = project
      SourceFiles = files
      OtherOptions = options
      ReferencedProjects = []
      ReferencedFrameworks = []
      TargetFramework = "x"
      GeneratedFiles = [] }

/// Lib: A.fs holds the shape, B.fs uses it. App, another project, uses it too. The oracle's App reads a compile of Lib.
type private Pair =
    { Dir: string
      Host: GraphHost
      LibSpec: ProjectSpec
      AppSpec: ProjectSpec
      Lib: ProjectGraph
      App: ProjectGraph
      A: string
      B: string
      Use: string
      Texts: Collections.Generic.Dictionary<string, string> }

    interface IAsyncDisposable with
        member p.DisposeAsync() =
            p.Host.Dispose()

            try
                Directory.Delete(p.Dir, true)
            with _ ->
                ()

            ValueTask()

let private startPair (mode: FileMode) (lib: string) (usage: string) : Task<Pair> =
    task {
        let! options = baseOptions.Value
        let dir = Path.Combine(Path.GetTempPath(), "rfs-schemes-" + Guid.NewGuid().ToString("N").Substring(0, 8))
        let libDir, appDir = Path.Combine(dir, "Lib"), Path.Combine(dir, "App")
        Directory.CreateDirectory libDir |> ignore
        Directory.CreateDirectory appDir |> ignore
        let a, b, u = Path.Combine(libDir, "A.fs"), Path.Combine(libDir, "B.fs"), Path.Combine(appDir, "Use.fs")
        let texts = Collections.Generic.Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        texts[a] <- "module Lib.M\n\n" + lib + "\n\nlet other = 1\n"
        texts[b] <- "module Lib.B\n\nopen Lib.M\n\n" + usage + "\n"
        texts[u] <- "module App.U\n\nopen Lib.M\n\n" + usage + "\n"

        for KeyValue(f, t) in texts do
            File.WriteAllText(f, t)

        let libDll = Path.Combine(libDir, "out", "Lib.dll")
        Directory.CreateDirectory(Path.GetDirectoryName libDll) |> ignore
        let compile = [| "fsc.exe"; "-o:" + libDll; yield! options; a; b |]
        let! compileDiagnostics, failure = FSharpChecker.Create().Compile compile |> Async.StartAsTask

        if failure.IsSome || not (File.Exists libDll) then
            failtest $"Lib is correct code, and compiles: %A{compileDiagnostics}"

        let libSpec = plainSpec (Path.Combine(libDir, "Lib.fsproj")) [ a; b ] (("-o:" + libDll) :: options)

        let appSpec =
            { plainSpec (Path.Combine(appDir, "App.fsproj")) [ u ] (("-o:" + Path.Combine(appDir, "out", "App.dll")) :: ("-r:" + libDll) :: options) with
                ReferencedProjects = [ libSpec.ProjectFile ]
                ReferencedFrameworks = [ "x" ] }

        let host = GraphHost.Start "schemes"
        let! (libGraph: ProjectGraph) = ProjectGraph.CreateWithMode(host, libSpec, mode)
        let! _ = libGraph.AllDiagnostics() |> settle
        do! whenPromoted libGraph libSpec
        let! (appGraph: ProjectGraph) = ProjectGraph.CreateWithMode(host, appSpec, mode, [ libGraph ])
        let! _ = appGraph.AllDiagnostics() |> settle
        do! whenPromoted appGraph appSpec

        return
            { Dir = dir
              Host = host
              LibSpec = libSpec
              AppSpec = appSpec
              Lib = libGraph
              App = appGraph
              A = a
              B = b
              Use = u
              Texts = texts }
    }

/// A check from scratch of `file`, as the project's current texts stand
let private oracleOf (p: Pair) (spec: ProjectSpec) (file: string) =
    task {
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
        let reader (f: string) = let t = p.Texts[f] in hash t, lazy t
        let! _, check, _ = checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, reader, file) |> Async.StartAsTask
        return check
    }

let private errors (ds: FSharpDiagnostic[]) =
    ds |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

let private signaturesInScope (what: string) (graph: ProjectGraph) =
    task {
        let! results = graph.AllFileResults() |> settle

        for KeyValue(file, result) in results do
            Expect.isEmpty (result.Signature.OutOfScope()) $"%s{what}: %s{Path.GetFileName file}'s signature holds its own links"
    }

/// B.fs and App equal the oracle by full message on correct code, and every signature holds its own links.
let private expectOracle (what: string) (p: Pair) =
    task {
        let! lib = p.Lib.AllDiagnostics() |> settle
        let! app = p.App.AllDiagnostics() |> settle
        let! libCheck = oracleOf p p.LibSpec p.B
        let! appCheck = oracleOf p p.AppSpec p.Use
        Expect.isEmpty (errors libCheck.Diagnostics) $"%s{what}: correct code in B.fs, for the oracle"
        Expect.isEmpty (errors appCheck.Diagnostics) $"%s{what}: correct code in App, for the oracle"
        let compare (expected: FSharpDiagnostic[]) (actual: FSharpDiagnostic[]) =
            Diagnostics.compare (expected |> Seq.map FcsDiagnostics.ofFcs |> List.ofSeq) (actual |> Seq.map FcsDiagnostics.ofFcs |> List.ofSeq)

        Expect.isEmpty (compare libCheck.Diagnostics lib[p.B]) $"%s{what}: the later file equals the oracle"
        Expect.isEmpty (compare appCheck.Diagnostics app[p.Use]) $"%s{what}: App equals the oracle"
        do! signaturesInScope what p.Lib
        do! signaturesInScope what p.App
    }

let private ownClass = "type C() = class end\n\n"
let private ownInterface = "type I = interface end\n\ntype C() =\n    interface I\n\n"

/// Every shape a constraint's undo broke, then the ones it left alone, with a use of it
let private shapes =
    [ "let, #seq<'a>", "let f (x: #seq<'a>) = x", "let u = f [ 1 ]"
      "let, 'a :> seq<'b>", "let f<'a, 'b when 'a :> seq<'b>> (x: 'a) = x", "let u = f [ 1 ]"
      "let inline", "let inline add (x: ^T) y = x + y", "let u = add 1 2"
      "SRTP member", "let inline g (x: ^a) = (^a: (member Foo: int) x)", "type X() =\n    member _.Foo = 1\n\nlet u = g (X())"
      "a constraint on itself", "let f<'a when 'a :> System.IComparable<'a>> (x: 'a) = x", "let u = f 1"
      "nested module value", "module N =\n    let f (x: #seq<'a>) = x", "let u = N.f [ 1 ]"
      "extension member", "type System.String with\n    member _.G(x: #seq<'a>) = x", "let u = \"s\".G [ 1 ]"
      "static member", "type C =\n    static member N(x: #seq<'a>) = x", "let u = C.N [ 1 ]"
      "instance member over a class typar", "type C<'k>() =\n    member _.N<'t when 't :> seq<'k>>(x: 't) = x", "let u = C<int>().N [ 1 ]"
      "abstract slot", "type I =\n    abstract M<'t, 'u when 't :> seq<'u>> : 't -> 't", "let u (i: I) = i.M [ 1 ]\n\nlet w = { new I with member _.M x = x }"
      "type definition", "type C<'a, 'b when 'a :> seq<'b>>(x: 'a) =\n    member _.X = x", "let u = C<int list, int>([ 1 ]).X"
      "union", "type U<'a, 'b when 'a :> seq<'b>> =\n    | A of 'a\n    | B of 'b", "let u: U<int list, int> = A [ 1 ]"
      "abbreviation", "type P<'a, 'b when 'a :> seq<'b>> = 'a * 'b", "let u: P<int list, int> = [ 1 ], 1"
      "generated augmentation", "type R<'a when 'a :> System.IComparable<'a>> = { V: 'a }", "let u = compare { V = 1 } { V = 2 }\n\nlet w = { V = 1 } = { V = 2 }"
      "enum", "let f<'a, 'u when 'a: enum<'u>> (x: 'a) = x", "let u = f System.DayOfWeek.Monday"
      "delegate", "let f<'d, 'a, 'b when 'd: delegate<'a, 'b>> (x: 'd) = x", "let u = f (System.EventHandler(fun _ _ -> ()))"
      "a measure in a constraint", "[<Measure>]\ntype m\n\nlet f<'a, [<Measure>] 'u when 'a :> seq<float<'u>>> (x: 'a) = x", "let u = f [ 1.0<m> ]"
      "own type in a let's constraint", ownClass + "let f<'a when 'a :> C> (x: 'a) = x", "let u = f (C())"
      "own type in a type's constraint", ownClass + "type G<'a when 'a :> C>(x: 'a) =\n    member _.X = x", "let u = G<C>(C()).X"
      "own interface in a constraint", ownInterface + "let f<'a when 'a :> I> (x: 'a) = x", "let u = f (C())"
      "an inline over an inline", "let inline add (x: ^T) y = x + y\n\nlet inline twice x = add x x", "let u = twice 1"
      "control: equality only", "let f<'a when 'a: equality> (x: 'a) = x", "let u = f 1"
      "control: another assembly's type", "let f<'a when 'a :> System.IDisposable> (x: 'a) = x", "let u = f (new System.IO.MemoryStream())"
      "control: a measure and no constraint", "let f (x: float<'u>) = x", "let u = f 1.0"
      "control: a nested function", "let outer () =\n    let inner (x: #seq<'a>) = x\n    inner [ 1 ]", "let u = outer ()" ]

let private flex = "let f (x: #seq<'a>) = x"
let private flexUse = "let u = f [ 1 ]"

let private tooltip (check: FSharpCheckFileResults) (line: int) (column: int) (lineText: string) =
    let (ToolTipText elements) = check.GetToolTip(line, column, lineText, [ "f" ], FSharpTokenTag.Identifier)

    [ for element in elements do
          match element with
          | ToolTipElement.Group items ->
              for item in items do
                  yield item.MainDescription.Text
                  yield! item.TypeMapping |> List.map _.Text
          | _ -> () ]

let private expectHover (what: string) (expected: FSharpCheckFileResults) (actual: FSharpCheckFileResults) (file: string) (text: string) =
    let lines = text.Replace("\r\n", "\n").Split '\n'
    let index = lines |> Array.findIndex (fun l -> l.Contains "f [ 1 ]")
    let column = lines[index].IndexOf "f [ 1 ]" + 1
    let at (check: FSharpCheckFileResults) = tooltip check (index + 1) column lines[index]
    Expect.isNonEmpty (at expected) $"%s{what}: the oracle has a tooltip in %s{file}"
    Expect.sequenceEqual (at actual) (at expected) $"%s{what}: the tooltip"
    let shown = String.Join("\n", at actual)
    Expect.stringContains shown "'a0 seq" $"%s{what}: the constraint's type is named"
    Expect.isFalse (shown.Contains "' seq") $"%s{what}: not an empty name: %s{shown}"

[<Tests>]
let tests =
    testList
        "Schemes"
        [ for mode in [ FileLevel; Declarations ] do
              for name, lib, usage in shapes do
                  yield
                      testTask $"%A{mode}: %s{name} reads as the oracle's in a later file and in App" {
                          let! (p: Pair) = startPair mode lib usage
                          use _ = (p :> IAsyncDisposable)
                          let! fileMode = p.Lib.Mode p.A
                          Expect.equal fileMode mode "the file is checked as this test says"
                          do! expectOracle name p
                      }

              yield
                  testTask $"%A{mode}: repeated body edits keep the later file and App on the oracle" {
                      let! (p: Pair) = startPair mode (flex + "\n\nlet inline add (x: ^T) y = x + y\n\n" + ownClass + "let g<'a when 'a :> C> (x: 'a) = x") (flexUse + "\n\nlet v = add 1 2\n\nlet w = g (C())")
                      use _ = (p :> IAsyncDisposable)
                      do! expectOracle "at the start" p

                      for i in 2..7 do
                          let text = p.Texts[p.A].Replace($"let other = {i - 1}", $"let other = {i}")
                          Expect.notEqual text p.Texts[p.A] "the edit changes the body"
                          p.Texts[p.A] <- text
                          p.Lib.SetText(p.A, text) |> ignore
                          let! _ = p.Lib.AllDiagnostics() |> settle
                          do! whenPromoted p.Lib p.LibSpec
                          do! expectOracle $"after body edit %d{i}" p
                  }

              yield
                  testTask $"%A{mode}: a later file's and App's hover show the constraint's type" {
                      let! (p: Pair) = startPair mode flex flexUse
                      use _ = (p :> IAsyncDisposable)
                      let! (libResults: Collections.Generic.IReadOnlyDictionary<string, FileResult>) = p.Lib.AllFileResults() |> settle
                      let! (appResults: Collections.Generic.IReadOnlyDictionary<string, FileResult>) = p.App.AllFileResults() |> settle
                      let! libCheck = oracleOf p p.LibSpec p.B
                      let! appCheck = oracleOf p p.AppSpec p.Use
                      expectHover "the later file" libCheck libResults[p.B].CheckResults p.B (p.Texts[p.B])
                      expectHover "App" appCheck appResults[p.Use].CheckResults p.Use (p.Texts[p.Use])
                  }

          yield
              testTask "promoting a file checks nothing after it again" {
                  let! (p: Pair) = startPair FileLevel flex flexUse
                  use _ = (p :> IAsyncDisposable)
                  let lib, app = p.Lib.FileLevelChecks, p.App.FileLevelChecks
                  p.Lib.Promote p.A
                  let! mode = p.Lib.WhenPromoted(p.A, Threading.CancellationToken.None) |> settle
                  Expect.equal mode Declarations "by declaration now"
                  do! expectOracle "after the promotion" p
                  Expect.equal (p.Lib.FileLevelChecks - lib) 0L "the later file is not checked again"
                  Expect.equal (p.App.FileLevelChecks - app) 0L "nor App"
              } ]
