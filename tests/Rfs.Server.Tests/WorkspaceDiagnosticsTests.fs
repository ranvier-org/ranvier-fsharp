module Rfs.Server.Tests.WorkspaceDiagnosticsTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Rfs.Harness
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests
open Rfs.Server.Tests.PublishTests
open Rfs.Server.Tests.PublishFixTests

let private multiDir = Path.Combine(Path.GetDirectoryName depsDir, "Multi")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private useFs = Path.Combine(multiDir, "App", "Use.fs")
let private libFs = Path.Combine(multiDir, "Lib", "Lib.fs")

let private waitFast (what: string) (check: unit -> bool) : Task =
    task {
        let deadline = DateTime.UtcNow.AddSeconds 8.

        while not (check ()) do
            if DateTime.UtcNow > deadline then
                failtest $"timed out waiting for %s{what}"

            do! Task.Delay 25
    }

let private startSessionInit (root: string) (projects: string list) (initOptionsJson: string) : Task<Session> =
    task {
        let! lsp, owner, server = TestServer.startCapturing ()
        let published = ConcurrentQueue()
        let uris = ConcurrentQueue()
        let counts = ConcurrentQueue()

        lsp.Diagnostics.Subscribe
            { new IObserver<_> with
                member _.OnNext((_, path, version, list)) = published.Enqueue((path, version, list))
                member _.OnError _ = ()
                member _.OnCompleted() = () }
        |> ignore

        let! _ = lsp.Initialize(Paths.toUri root, JsonNode.Parse initOptionsJson) |> within
        let! running = server

        let load () : Task =
            let documents = JsonArray()

            for p in projects do
                let d = JsonObject()
                d["Uri"] <- JsonValue.Create(Paths.toUri p)
                documents.Add d

            let parameters = JsonObject()
            parameters["TextDocuments"] <- documents

            task {
                let! _ = lsp.Request("fsharp/workspaceLoad", parameters) |> within
                ()
            }

        do! load ()
        let! oracle = OracleClient.start ()

        return
            { Lsp = lsp
              Server = running
              Oracle = oracle
              Published = published
              Uris = uris
              Counts = counts
              Owner = owner
              Project = projects.Head
              Load = load }
    }

let private lastOf (s: Session) (path: string) =
    let _, _, d = Array.last (publishesOf s path)
    d

let private didSave (s: Session) (path: string) : Task =
    s.Lsp.Notify("textDocument/didSave", textDocument [ "uri", JsonValue.Create(Paths.toUri path) ])

let private didClose (s: Session) (path: string) : Task =
    s.Lsp.Notify("textDocument/didClose", textDocument [ "uri", JsonValue.Create(Paths.toUri path) ])

[<Tests>]
let tests =
    testSequenced
    <| testList
        "WorkspaceDiagnostics"
        [ testTask "initialize advertises textDocumentSync.save" {
              let! started = TestServer.start ()
              let (lsp: LspClient), (owner: IDisposable) = started
              use _ = { new IAsyncDisposable with member _.DisposeAsync() = (owner.Dispose(); ValueTask()) }
              let! (reply: JsonNode), _ = lsp.Request("initialize", JsonNode.Parse """{"processId":null,"rootUri":null,"capabilities":{}}""") |> within
              let save = reply["capabilities"].["textDocumentSync"].["save"]
              Expect.isNotNull save "textDocumentSync.save is advertised so clients send didSave"
          }

          testTask "closing a project file keeps its on-disk errors in published diagnostics" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let cFile = Path.Combine(dir, "C.fs")
              File.AppendAllText(cFile, "\nlet brokenOnDisk : int = \"not-an-int\"\n")
              let! s = startSessionWith dir [ project ] true ignore |> within
              use _ = disposing s
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (publishesOf s cFile) "default Option A does not publish untouched closed files on cold load"

              do! openAll s [ cFile ]
              do! waitFast "open error in C" (fun () -> hasVersion s cFile 0 && not (lastOf s cFile).IsEmpty)
              let! expected = oracleFor s [] cFile
              Expect.isNonEmpty expected "C on disk has a compiler error"

              do! didClose s cFile
              do! waitFast "unversioned close publish for C" (fun () -> publishesOf s cFile |> Array.exists (fun (_, v, _) -> v = None))
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (Diagnostics.compare expected (lastOf s cFile)) "closing C keeps its on-disk error in the Problems panel"
          }

          testTask "saving an open file publishes downstream breaks in closed files of the same project and clears when fixed" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! waitFast "open publish of A" (fun () -> hasVersion s a 0)

              do! s.Lsp.DidChange(a, 1, replace s a "(s: Shape) : int =" "(s: Shape) : unit =")
              do! waitFast "A version 1" (fun () -> hasVersion s a 1)
              do! Task.Delay 250
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (publishesOf s c) "mid-edit typing in A does not publish downstream breaks in closed C"

              let! expectedBroken = oracleFor s [ a ] c
              Expect.isNonEmpty expectedBroken "A's signature change breaks C"

              do! didSave s a
              do! waitFast "closed C downstream break after saving A" (fun () -> not (Array.isEmpty (publishesOf s c)) && not (lastOf s c).IsEmpty)
              Expect.isEmpty (Diagnostics.compare expectedBroken (lastOf s c)) "saved A publishes oracle diagnostics for closed C"

              do! s.Lsp.DidChange(a, 2, replace s a "(s: Shape) : unit =" "(s: Shape) : int =")
              do! waitFast "A version 2" (fun () -> hasVersion s a 2)
              do! Task.Delay 250
              do! s.Server.Publisher.Flushed()
              Expect.isNonEmpty (lastOf s c) "closed C keeps its last published diagnostics until A is saved again"

              do! didSave s a
              do! waitFast "closed C cleared after saving fixed A" (fun () -> (lastOf s c).IsEmpty)
          }

          testTask "saving an open file publishes downstream breaks in closed files of referencing projects" {
              let! s = startSessionWith multiDir [ appProject ] true ignore |> within
              use _ = disposing s
              do! openAll s [ libFs ]
              do! waitFast "open publish of Lib.fs" (fun () -> hasVersion s libFs 0)

              do! s.Lsp.DidChange(libFs, 1, replace s libFs "let f (x: int) : int = x + 1" "let f (x: int) : unit = ()")
              do! waitFast "Lib.fs version 1" (fun () -> hasVersion s libFs 1)
              do! Task.Delay 250
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (publishesOf s useFs) "mid-edit typing in Lib.fs does not publish breaks in closed App/Use.fs"

              do! didSave s libFs
              do! waitFast "closed App/Use.fs break after saving Lib.fs" (fun () ->
                  publishesOf s useFs |> Array.exists (fun (_, v, ds) -> v = None && ds |> List.exists (fun d -> d.Severity = 1)))

              do! s.Lsp.DidChange(libFs, 2, replace s libFs "let f (x: int) : unit = ()" "let f (x: int) : int = x + 1")
              do! waitFast "Lib.fs version 2" (fun () -> hasVersion s libFs 2)
              do! didSave s libFs
              do! waitFast "closed App/Use.fs cleared after saving fixed Lib.fs" (fun () -> (lastOf s useFs).IsEmpty)
          }

          testTask "opt-in workspaceDiagnostics publishes closed-file errors on cold load" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let aFile = Path.Combine(dir, "A.fs")
              let bFile = Path.Combine(dir, "B.fs")
              let cFile = Path.Combine(dir, "C.fs")
              File.AppendAllText(cFile, "\nlet brokenCold : int = \"cold-error\"\n")

              let! s = startSessionInit dir [ project ] """{"workspaceDiagnostics":true}""" |> within
              use _ = disposing s
              let! expected = oracleFor s [] cFile
              Expect.isNonEmpty expected "C on disk has a compiler error"

              do! waitFast "cold workspace diagnostic for unopened C" (fun () -> not (Array.isEmpty (publishesOf s cFile)))
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (Diagnostics.compare expected (lastOf s cFile)) "unopened C publishes oracle error on cold load"
              Expect.isEmpty (publishesOf s aFile) "clean unopened A does not send noisy empty diagnostics"
              Expect.isEmpty (publishesOf s bFile) "clean unopened B does not send noisy empty diagnostics"
          }

          testTask "toggling workspaceDiagnostics via didChangeConfiguration publishes and clears closed-file diagnostics while keeping mid-edit quiet" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let aFile = Path.Combine(dir, "A.fs")
              let bFile = Path.Combine(dir, "B.fs")
              let cFile = Path.Combine(dir, "C.fs")
              File.AppendAllText(cFile, "\nlet brokenToggle : int = \"toggle-error\"\n")

              let! s = startSessionWith dir [ project ] true ignore |> within
              use _ = disposing s
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (publishesOf s cFile) "initially disabled: unopened C is not published"

              do!
                  s.Lsp.Notify(
                      "workspace/didChangeConfiguration",
                      JsonNode.Parse """{"settings":{"FSharp":{"workspaceDiagnostics":true}}}"""
                  )

              do! waitFast "C published when workspaceDiagnostics enabled" (fun () -> not (Array.isEmpty (publishesOf s cFile)) && not (lastOf s cFile).IsEmpty)

              do! openAll s [ aFile ]
              do! waitFast "A open publish" (fun () -> hasVersion s aFile 0)
              // Break Shape so B.fs (closed, depends on Shape) breaks in-memory, without saving A.fs
              do! s.Lsp.DidChange(aFile, 1, replace s aFile "| Circle of radius: int" "| Circle of radius: string")
              do! waitFast "A version 1" (fun () -> hasVersion s aFile 1)
              do! Task.Delay 250
              do! s.Server.Publisher.Flushed()
              Expect.isEmpty (publishesOf s bFile) "even with workspaceDiagnostics enabled, mid-edit typing in A does not publish closed B before save"

              do!
                  s.Lsp.Notify(
                      "workspace/didChangeConfiguration",
                      JsonNode.Parse """{"settings":{"FSharp":{"workspaceDiagnostics":false}}}"""
                  )

              do! waitFast "closed C cleared when workspaceDiagnostics disabled" (fun () -> (lastOf s cFile).IsEmpty)
              Expect.isTrue (hasVersion s aFile 1) "open A keeps its diagnostics"
          }

          testTask "watched disk change on a closed file publishes downstream breaks and clears when fixed" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let aFile = Path.Combine(dir, "A.fs")
              let cFile = Path.Combine(dir, "C.fs")
              let originalA = File.ReadAllText aFile

              let! s =
                  startSessionWith dir [ project ] true (fun srv -> srv.WatchDelayForTests <- TimeSpan.FromMilliseconds 20.)
                  |> within

              use _ = disposing s

              File.WriteAllText(aFile, originalA.Replace("(s: Shape) : int =", "(s: Shape) : unit ="))
              let change (path: string) = $$"""{"uri":{{JsonValue.Create(Paths.toUri path).ToJsonString()}},"type":2}"""
              do! s.Lsp.Notify("workspace/didChangeWatchedFiles", JsonNode.Parse($$"""{"changes":[{{change aFile}}]}"""))

              let! expectedBroken = s.Oracle.Check project Map.empty
              let expectedForC =
                  expectedBroken
                  |> List.filter (fun d -> String.Equals(d.File, LspClient.NormalizePath cFile, StringComparison.OrdinalIgnoreCase))

              Expect.isNonEmpty expectedForC "disk edit to A breaks closed C"
              do! waitFast "closed C break after watched disk change to A" (fun () -> not (Array.isEmpty (publishesOf s cFile)) && not (lastOf s cFile).IsEmpty)
              Expect.isEmpty (Diagnostics.compare expectedForC (lastOf s cFile)) "closed C matches oracle after disk change"

              File.WriteAllText(aFile, originalA)
              do! s.Lsp.Notify("workspace/didChangeWatchedFiles", JsonNode.Parse($$"""{"changes":[{{change aFile}}]}"""))
              do! waitFast "closed C cleared after A fixed on disk" (fun () -> (lastOf s cFile).IsEmpty)
          } ]
