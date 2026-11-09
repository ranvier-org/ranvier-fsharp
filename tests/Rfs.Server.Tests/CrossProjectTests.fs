module Rfs.Server.Tests.CrossProjectTests

open System
open System.Diagnostics
open System.IO
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

let private built =
    lazy
        (let psi = ProcessStartInfo("dotnet", $"build \"{appProject}\" -v q --nologo")
         psi.RedirectStandardOutput <- true
         psi.RedirectStandardError <- true
         use p = Process.Start psi
         let out = p.StandardOutput.ReadToEndAsync()
         let err = p.StandardError.ReadToEndAsync()
         p.WaitForExit()

         if p.ExitCode <> 0 then
             failwith $"building the Multi fixture failed: {out.Result}{err.Result}")

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
              built.Force()
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
              built.Force()
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
              let! ended =
                  task {
                      try
                          let! _ = waiting
                          return true
                      with :? OperationCanceledException ->
                          return true
                  }

              Expect.isTrue ended "the wait on the old App ended"
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
          } ]
