module Rfs.Server.Tests.CrossProjectTests

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Harness
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests
open Rfs.Server.Tests.PublishTests

let private multiDir = Path.Combine(Path.GetDirectoryName depsDir, "Multi")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private libProject = Path.Combine(multiDir, "Lib", "Lib.fsproj")
let private useFs = Path.Combine(multiDir, "App", "Use.fs")
let private libFs = Path.Combine(multiDir, "Lib", "Lib.fs")

/// A copy of Multi's Lib and App without obj and bin, to break or rename
let private copyMulti () =
    let root = Path.Combine(Path.GetTempPath(), "rfs-multi-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory root |> ignore
    File.Copy(Path.Combine(Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName multiDir)), "global.json"), Path.Combine(root, "global.json"))

    for f in [ "Directory.Build.props"; "Directory.Packages.props" ] do
        File.Copy(Path.Combine(multiDir, f), Path.Combine(root, f))

    for name in [ "Lib"; "App" ] do
        Directory.CreateDirectory(Path.Combine(root, name)) |> ignore

        for f in Directory.GetFiles(Path.Combine(multiDir, name)) do
            File.Copy(f, Path.Combine(root, name, Path.GetFileName f))

    root

let private buildAgain (project: string) =
    let psi = ProcessStartInfo("dotnet", $"build \"{project}\" -v q --nologo")
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    use p = Process.Start psi
    let out = p.StandardOutput.ReadToEndAsync()
    let err = p.StandardError.ReadToEndAsync()
    p.WaitForExit()
    Expect.equal p.ExitCode 0 $"{out.Result}{err.Result}"

/// Every `window/showMessage` from now on: the server says a project failed to load with one
let private watchMessages (s: Session) =
    let messages = System.Collections.Concurrent.ConcurrentQueue<string>()

    s.Lsp.Notifications.Subscribe
        { new IObserver<Received> with
            member _.OnNext r =
                if r.Message["method"].GetValue<string>() = "window/showMessage" then
                    messages.Enqueue(r.Message.ToJsonString())

            member _.OnError _ = ()
            member _.OnCompleted() = () }
    |> ignore

    messages

let private errorsOf (s: Session) (path: string) =
    match publishesOf s path |> Array.tryLast with
    | Some(_, _, list) -> list |> List.filter (fun (d: Diagnostic) -> d.Severity = 1)
    | None -> []

let private toString = "let f (x: int) : string = string x"
let private toInt = "let f (x: int) : int = x + 1"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "CrossProject"
        [ testTask "server publishes App's open file after a Lib edit" {
              Fixtures.build appProject
              let! (s: Session) = startSessionWith multiDir [ appProject ] true ignore
              use _ = (s :> IAsyncDisposable)
              do! openAll s [ useFs; libFs ]
              do! eventually "App's first publish" (fun () -> (publishesOf s useFs).Length > 0)
              Expect.isEmpty (errorsOf s useFs) "App is clean"
              do! s.Lsp.DidChange(libFs, 1, replace s libFs toInt toString)
              do! eventually "App's errors, with no edit of App" (fun () -> not (errorsOf s useFs).IsEmpty)
              do! expectEqualOracle s [ useFs; libFs ] libFs 1
              let! expected = oracleFor s [ useFs; libFs ] useFs
              let _, _, actual = publishesOf s useFs |> Array.last
              Expect.isEmpty (Diagnostics.compare expected actual) "App's publish equals the oracle"
              do! s.Lsp.DidChange(libFs, 2, replace s libFs toString toInt)
              do! eventually "App clean again" (fun () -> (errorsOf s useFs).IsEmpty)
          }

          testTask "a reload of Lib leaves App on the new graph" {
              Fixtures.build appProject
              let! (s: Session) = startSessionWith multiDir [ appProject ] true ignore
              use _ = (s :> IAsyncDisposable)
              do! openAll s [ useFs; libFs ]
              do! eventually "App's first publish" (fun () -> (publishesOf s useFs).Length > 0)
              let workspace = s.Server.Workspace
              let oldLib = (workspace.ProjectOf libFs).Value
              let oldApp = (workspace.ProjectOf useFs).Value
              do! s.Lsp.DidChange(libFs, 1, replace s libFs toInt toString)
              do! eventually "App's errors" (fun () -> not (errorsOf s useFs).IsEmpty)
              // Waits on the old graph end with it
              let waiting = oldApp.AllDiagnostics()
              do! workspace.Reload libProject
              // It ends one way or the other: a graph disposed under it does not leave it waiting
              try
                  let! _ = waiting |> within
                  ()
              with :? OperationCanceledException ->
                  ()

              let lib = (workspace.ProjectOf libFs).Value
              let app = (workspace.ProjectOf useFs).Value
              Expect.isFalse (obj.ReferenceEquals(lib, oldLib)) "a new Lib"
              Expect.isFalse (obj.ReferenceEquals(app, oldApp)) "a new App, as the old one holds the old Lib's assembly"
              // The open text of Lib carried over, and App reads it from the new graph
              Expect.isNonEmpty (workspace.ReferencingProjects lib) "App references the new Lib"
              do! eventually "App's errors after the reload" (fun () -> not (errorsOf s useFs).IsEmpty)
              // An edit on the new graph reaches App: on a disposed graph's assembly it would not
              do! s.Lsp.DidChange(libFs, 2, replace s libFs toString toInt)
              do! eventually "App clean after an edit of the new Lib" (fun () -> (errorsOf s useFs).IsEmpty)
              let! diagnostics = app.Diagnostics useFs |> within
              Expect.isEmpty diagnostics "the new App settled on the new Lib"
          }

          testTask "a C# project reference stays the file on disk" {
              let app = Path.Combine(multiDir, "Cs", "App", "CsApp.fsproj")
              let csUse = Path.Combine(multiDir, "Cs", "App", "Use.fs")
              Fixtures.build app
              let! (s: Session) = startSessionWith multiDir [ app ] false ignore
              use _ = (s :> IAsyncDisposable)
              let messages = watchMessages s
              do! s.Load()
              Expect.equal (List.ofSeq messages) [] "no project failed to load"
              Expect.isNone (s.Server.Workspace.ProjectOf(Path.Combine(multiDir, "Cs", "Lib", "CsLib.csproj"))) "not a graph"
              let graph = (s.Server.Workspace.ProjectOf csUse).Value
              let! diagnostics = graph.Diagnostics csUse |> within
              Expect.isEmpty diagnostics "CsType comes from the DLL"
          }

          testTask "App falls back to the file on disk when Lib stops loading" {
              let root = copyMulti ()
              let lib, app = Path.Combine(root, "Lib", "Lib.fsproj"), Path.Combine(root, "App", "App.fsproj")
              let use' = Path.Combine(root, "App", "Use.fs")
              Fixtures.build app
              let! (s: Session) = startSessionWith root [ app ] true ignore
              use _ = (s :> IAsyncDisposable)
              let workspace = s.Server.Workspace
              Expect.isSome (workspace.ProjectOf(Path.Combine(root, "Lib", "Lib.fs"))) "Lib is in memory first"
              File.WriteAllText(lib, "<Project><not xml")
              do! workspace.Reload lib
              Expect.isNone (workspace.ProjectOf(Path.Combine(root, "Lib", "Lib.fs"))) "Lib no longer loads"
              let graph = (workspace.ProjectOf use').Value
              let! clean = graph.Diagnostics use' |> within
              Expect.isEmpty clean "App reads the DLL"
              let bad = File.ReadAllText(use').Replace("let y = f 1", "let y: string = f 1")
              graph.SetText(use', bad) |> ignore
              let! typed = graph.Diagnostics use' |> within
              Expect.isNonEmpty typed "and checks against what it says"
          }

          testTask "a Lib loaded for another framework than App resolved says App stays on the DLL" {
              let lib = Path.Combine(multiDir, "Frameworks", "Lib", "FwLib.fsproj")
              let app = Path.Combine(multiDir, "Frameworks", "App", "FwApp.fsproj")
              Fixtures.build app
              let! (s: Session) = startSessionWith multiDir [ app ] true ignore
              use _ = (s :> IAsyncDisposable)
              let messages = watchMessages s
              do! s.Server.Workspace.Reload app
              Expect.equal (List.ofSeq messages) [] "App loaded Lib for its framework: its assembly is in memory, and nothing to say"
              // A reload evaluates Lib for its first framework, not the one App resolved
              do! s.Server.Workspace.Reload lib
              let shown = String.Join("\n", messages)
              Expect.stringContains shown "FwApp" "the message names the project on its DLL"
              Expect.stringContains shown "FwLib" "and the project it can't read in memory"
              Expect.stringContains shown "net10.0" "and the framework it needs"
          }

          testTask "an edit of Lib's AssemblyName reaches App's spec on a reload" {
              let root = copyMulti ()
              let lib, app = Path.Combine(root, "Lib", "Lib.fsproj"), Path.Combine(root, "App", "App.fsproj")
              let libSource, use' = Path.Combine(root, "Lib", "Lib.fs"), Path.Combine(root, "App", "Use.fs")
              Fixtures.build app
              let! (s: Session) = startSessionWith root [ app ] true ignore
              use _ = (s :> IAsyncDisposable)
              let messages = watchMessages s
              let renamed = File.ReadAllText(lib).Replace("<TargetFramework>net10.0</TargetFramework>", "<TargetFramework>net10.0</TargetFramework>\n    <AssemblyName>Renamed</AssemblyName>")
              File.WriteAllText(lib, renamed)
              // Lib's alone, as an editor's build of it: App's own files don't change, so its cached spec looks fresh
              buildAgain lib
              do! s.Server.Workspace.Reload lib
              Expect.equal (List.ofSeq messages) [] "Lib and App load again"
              let graph = (s.Server.Workspace.ProjectOf use').Value
              let libGraph = (s.Server.Workspace.ProjectOf libSource).Value
              let! clean = graph.Diagnostics use' |> within
              Expect.isEmpty clean "built as Renamed"
              // In memory, not on the old DLL's path
              libGraph.SetText(libSource, File.ReadAllText(libSource).Replace("let f (x: int) : int = x + 1", "let f (x: int) : string = string x")) |> ignore
              let! broken = graph.Diagnostics use' |> within
              Expect.isNonEmpty broken "App reads Lib in memory"
          } ]

[<Tests>]
let burstTests =
    testSequenced
    <| testList
        "Workspace graph bursts"
        [ testTask "a branch switch's burst of watched-file changes reloads each project once" {
              Fixtures.build appProject
              let created = System.Collections.Concurrent.ConcurrentQueue<string>()
              let! (s: Session) = startSessionWith multiDir [ appProject ] true ignore
              use _ = (s :> IAsyncDisposable)
              do! openAll s [ useFs ]
              do! eventually "App's first publish" (fun () -> (publishesOf s useFs).Length > 0)
              s.Server.Workspace.OnGraphCreated(fun g -> created.Enqueue(Path.GetFileName g.Spec.ProjectFile))
              let props = Path.Combine(multiDir, "Directory.Build.props")

              let notify (path: string) =
                  s.Lsp.Notify("workspace/didChangeWatchedFiles", JsonNode.Parse($$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri path).ToJsonString()}},"type":2}]}"""))

              // What a checkout reports, one notification at a time
              for path in [ libProject; useFs; appProject; props; libFs ] do
                  do! notify path

              do! eventually "both reloaded" (fun () -> created |> Seq.contains "Lib.fsproj" && created |> Seq.contains "App.fsproj")
              do! eventually "App's publish after the reload" (fun () -> (s.Server.Workspace.ProjectOf useFs).IsSome)
              let! _ = (s.Server.Workspace.ProjectOf useFs).Value.Diagnostics useFs |> within
              do! Task.Delay 3000
              let counts = created |> Seq.countBy id |> Map.ofSeq
              Expect.equal counts (Map [ "App.fsproj", 1; "Lib.fsproj", 1 ]) "each project reloads once"
          } ]
