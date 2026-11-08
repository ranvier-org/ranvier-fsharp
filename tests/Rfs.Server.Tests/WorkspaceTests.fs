module Rfs.Server.Tests.WorkspaceTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 120.)
let private withinT (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 120.)

let private disposing (d: IDisposable) =
    { new IAsyncDisposable with
        member _.DisposeAsync() =
            d.Dispose()
            ValueTask() }

let private fixturesDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures")
        if Directory.Exists candidate then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private fixture (name: string) = Path.Combine(fixturesDir, name)
let private depsDir = fixture "Deps"
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")
let private depsA = Path.Combine(depsDir, "A.fs")

/// A copy of a fixture without its obj and bin, so it restores and loads on its own
let private copyFixture (name: string) : string =
    let root = Path.Combine(Path.GetTempPath(), "rfs-ws-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let target = Path.Combine(root, name)
    Directory.CreateDirectory target |> ignore
    // Outside the repository the SDK would be the newest installed, not the one the loader registers
    File.Copy(Path.Combine(Path.GetDirectoryName(Path.GetDirectoryName fixturesDir), "global.json"), Path.Combine(root, "global.json"))

    for f in Directory.GetFiles(fixture name) do
        File.Copy(f, Path.Combine(target, Path.GetFileName f))

    target

/// Everything a Workspace sent to its client
type private Sent() =
    let notifications = ConcurrentQueue<string * obj>()
    let requests = ConcurrentQueue<string * obj>()
    member _.Notifications = notifications.ToArray()
    member _.Requests = requests.ToArray()

    member _.Client =
        let notify (method: string) (payload: obj) =
            notifications.Enqueue((method, payload))
            async.Return(Ok())

        let request =
            { new Ionide.LanguageServerProtocol.Server.ClientRequestSender with
                member _.Send<'a> (method: string) (payload: obj) : Ionide.LanguageServerProtocol.JsonRpc.AsyncLspResult<'a> =
                    requests.Enqueue((method, payload))
                    async.Return(Ok Unchecked.defaultof<'a>) }

        new RanvierClient(notify, request)

    /// The `fsharp/notifyWorkspace` contents, parsed
    member _.WorkspaceKinds: string list =
        notifications.ToArray()
        |> Array.filter (fun (m, _) -> m = "fsharp/notifyWorkspace")
        |> Array.map (fun (_, p) ->
            let parsed = JsonNode.Parse((p :?> PlainNotification).Content)
            parsed["Kind"].GetValue<string>())
        |> List.ofArray

let private withWorkspace (loadTimeout: TimeSpan option) (body: Workspace -> Sent -> Task) =
    task {
        let sent = Sent()
        use host = GraphHost.Start "workspace-test"

        let workspace =
            match loadTimeout with
            | Some t -> new Workspace(host, sent.Client, t)
            | None -> new Workspace(host, sent.Client)

        try
            do! body workspace sent
        finally
            workspace.Dispose()
    }

let private initOptions (json: string) = JsonNode.Parse json

let private content (reply: JsonNode) : JsonNode =
    let text =
        match reply["content"] with
        | null -> reply["Content"].GetValue<string>()
        | c -> c.GetValue<string>()

    JsonNode.Parse text

/// Every `fsharp/notifyWorkspace` content the client receives, parsed
let private watchWorkspace (lsp: LspClient) =
    let seen = ConcurrentQueue<JsonNode>()

    let subscription =
        lsp.Notifications.Subscribe
            { new IObserver<Received> with
                member _.OnNext r =
                    if r.Message["method"].GetValue<string>() = "fsharp/notifyWorkspace" then
                        seen.Enqueue(content r.Message["params"])

                member _.OnError _ = ()
                member _.OnCompleted() = () }

    let waitFor (what: string) (predicate: JsonNode -> bool) : Task =
        task {
            let deadline = DateTime.UtcNow.AddSeconds 120.

            while not (seen.ToArray() |> Array.exists predicate) do
                if DateTime.UtcNow > deadline then
                    let saw = String.Join(", ", seen.ToArray() |> Array.map (fun n -> n.ToJsonString()))
                    failtest $"timed out waiting for %s{what}; saw %s{saw}"

                do! Task.Delay 50
        }

    seen, waitFor, subscription

let private kindIs (kind: string) (n: JsonNode) = n["Kind"].GetValue<string>() = kind

let private startServer (rootUri: string) (options: string) =
    task {
        let! lsp, owner = TestServer.start ()
        let! _ = lsp.Initialize(rootUri, initOptions options) |> within
        return lsp, disposing owner
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Workspace"
        [ test "paths round trip through URIs" {
              let path = Path.GetFullPath(Path.Combine(Path.GetTempPath(), "a b", "c#d.fs"))
              let uri = Paths.toUri path
              Expect.isTrue (uri.StartsWith "file:///") "a file URI"
              Expect.equal (Paths.ofUri uri) path "the path comes back"

              if OperatingSystem.IsWindows() then
                  let lower = "file:///c%3A/Users/x/a%20b.fs"
                  Expect.equal (Paths.ofUri lower) @"C:\Users\x\a b.fs" "percent escapes decoded, drive letter uppercased"
          }

          testTask "peek finds Deps" {
              let! (lsp: LspClient), (owner: IAsyncDisposable) = startServer (Paths.toUri depsDir) "{}"
              use _ = owner

              let parameters =
                  JsonNode.Parse(
                      $$"""{"Directory":{{JsonValue.Create(depsDir).ToJsonString()}},"Deep":2,"ExcludedDirs":[]}"""
                  )

              let! (reply: JsonNode), _ = lsp.Request("fsharp/workspacePeek", parameters) |> within
              let body = content reply
              Expect.equal (body["Kind"].GetValue<string>()) "workspacePeek" "the response kind"
              let found = body["Data"].["Found"].AsArray()
              Expect.equal found.Count 1 "one interesting directory"
              Expect.equal (found[0].["Type"].GetValue<string>()) "directory" "a directory"

              let fsprojs = found[0].["Data"].["Fsprojs"].AsArray() |> Seq.map (fun (n: JsonNode) -> n.GetValue<string>()) |> List.ofSeq
              Expect.equal fsprojs [ depsProject ] "Deps.fsproj"
          }

          testTask "peek lists the projects of a solution" {
              let dir = copyFixture "Deps"
              let sln = Path.Combine(dir, "Deps.sln")

              File.WriteAllText(
                  sln,
                  "Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                  + "Project(\"{F2A71F9B-5D33-465A-A702-920D77279786}\") = \"Deps\", \"Deps.fsproj\", \"{11111111-1111-1111-1111-111111111111}\"\r\n"
                  + "EndProject\r\nGlobal\r\nEndGlobal\r\n"
              )

              let found = WorkspacePeek.peek dir 2 []
              let solutions = found |> List.choose (function WorkspacePeek.Interesting.Solution(p, _) -> Some p | _ -> None)
              Expect.equal solutions [ sln ] "the solution is found"
              Expect.equal (WorkspacePeek.projectsOf found) [ Path.Combine(dir, "Deps.fsproj") ] "its project"
          }

          testTask "workspaceLoad loads Deps and its cold check settles" {
              let! (lsp: LspClient), (owner: IAsyncDisposable) = startServer (Paths.toUri depsDir) "{}"
              use _ = owner
              let seen, _, _ = watchWorkspace lsp

              let parameters =
                  JsonNode.Parse($$"""{"TextDocuments":[{"Uri":{{JsonValue.Create(Paths.toUri depsProject).ToJsonString()}}}]}""")

              let! reply, _ = lsp.Request("fsharp/workspaceLoad", parameters) |> within
              let body = content reply
              Expect.equal (body["Kind"].GetValue<string>()) "workspaceLoad" "the response kind"
              Expect.equal (body["Data"].["Status"].GetValue<string>()) "finished" "finished"

              let kinds = seen.ToArray() |> Array.map (fun n -> n["Kind"].GetValue<string>()) |> List.ofArray
              Expect.equal kinds [ "workspaceLoad"; "projectLoading"; "project"; "workspaceLoad" ] "FSAC's notification sequence"
              Expect.equal ((Array.item 0 (seen.ToArray())).["Data"].["Status"].GetValue<string>()) "started" "started first"
              Expect.equal ((Array.item 3 (seen.ToArray())).["Data"].["Status"].GetValue<string>()) "finished" "finished last"

              let files = (Array.item 2 (seen.ToArray())).["Data"].["Files"].AsArray() |> Seq.map (fun (n: JsonNode) -> n.GetValue<string>()) |> List.ofSeq
              Expect.contains files depsA "the project's files"
          }

          testTask "Load settles the cold check before returning" {
              do!
                  withWorkspace None (fun workspace _ ->
                      task {
                          let! results = workspace.Load [ depsProject ] |> within
                          Expect.equal results [ Loaded depsProject ] "loaded"
                          let graph = (workspace.ProjectOf depsA).Value
                          Expect.equal workspace.Projects.Length 1 "one project"
                          Expect.isGreaterThan graph.FileLevelChecks 0L "the cold check ran"
                          let! _ = graph.AllDiagnostics().WaitAsync(TimeSpan.FromMilliseconds 200.)
                          ()
                      })
          }

          testTask "automatic init loads the root project" {
              let! (lsp: LspClient), (owner: IAsyncDisposable) = startServer (Paths.toUri depsDir) """{"AutomaticWorkspaceInit":true}"""
              use _ = owner
              let seen, waitFor, _ = watchWorkspace lsp
              do! waitFor "workspaceLoad finished" (fun n -> kindIs "workspaceLoad" n && n["Data"].["Status"].GetValue<string>() = "finished") |> withinT

              let projects = seen.ToArray() |> Array.filter (kindIs "project")
              Expect.equal projects.Length 1 "the root project"
              Expect.equal (projects[0].["Data"].["Project"].GetValue<string>()) depsProject "Deps"
          }

          testTask "unloadable project fails alone" {
              let broken = copyFixture "Tiny"
              let brokenProject = Path.Combine(broken, "Tiny.fsproj")
              let text = File.ReadAllText brokenProject
              File.WriteAllText(brokenProject, text.Replace("""<Compile Include="C.fs" />""", """<Compile Include="Missing.fs" />"""))

              do!
                  withWorkspace None (fun workspace sent ->
                      task {
                          let! results = workspace.Load [ brokenProject; depsProject ] |> within

                          match results with
                          | [ Failed(p, reason); Loaded d ] ->
                              Expect.equal p brokenProject "the broken project"
                              Expect.isNotEmpty reason "with a reason"
                              Expect.equal d depsProject "Deps loaded"
                          | other -> failtest $"unexpected results %A{other}"

                          let graph = (workspace.ProjectOf depsA).Value
                          let! diagnostics = graph.AllDiagnostics() |> within
                          Expect.equal diagnostics.Count 6 "Deps still answers"
                          Expect.isNone (workspace.ProjectOf(Path.Combine(broken, "A.fs"))) "the broken project is not served"

                          Expect.isTrue
                              (sent.Notifications |> Array.exists (fun (m, _) -> m = "window/showMessage"))
                              "the user is told"

                          Expect.contains sent.WorkspaceKinds "error" "and the workspace notification says why"
                      })
          }

          testTask "settle timeout fails the project and the server keeps serving" {
              do!
                  withWorkspace (Some(TimeSpan.FromMilliseconds 1.)) (fun workspace sent ->
                      task {
                          let! results = workspace.Load [ depsProject ] |> within

                          match results with
                          | [ Failed(p, reason) ] ->
                              Expect.equal p depsProject "Deps"
                              Expect.stringContains reason "timed out" "the reason names the timeout"
                          | other -> failtest $"unexpected results %A{other}"

                          Expect.isEmpty workspace.Projects "the stalled graph was disposed and dropped"
                          Expect.isTrue (sent.Notifications |> Array.exists (fun (m, _) -> m = "window/showMessage")) "the user is told"
                      })
          }

          test "the load timeout comes from RFS_LOAD_TIMEOUT_MS" {
              let previous = Environment.GetEnvironmentVariable "RFS_LOAD_TIMEOUT_MS"

              try
                  Environment.SetEnvironmentVariable("RFS_LOAD_TIMEOUT_MS", null)
                  Expect.equal (Workspace.DefaultLoadTimeout()) (TimeSpan.FromMinutes 10.) "ten minutes by default"
                  Environment.SetEnvironmentVariable("RFS_LOAD_TIMEOUT_MS", "250")
                  Expect.equal (Workspace.DefaultLoadTimeout()) (TimeSpan.FromMilliseconds 250.) "read from the variable"
              finally
                  Environment.SetEnvironmentVariable("RFS_LOAD_TIMEOUT_MS", previous)
          }

          testTask "open texts are applied again when a project is reloaded" {
              let dir = copyFixture "Deps"
              let a = Path.Combine(dir, "A.fs")

              do!
                  withWorkspace None (fun workspace _ ->
                      task {
                          let! _ = workspace.Load [ Path.Combine(dir, "Deps.fsproj") ] |> within
                          workspace.Open(a, File.ReadAllText a + "\nlet broken : int = \"text\"\n")
                          do! workspace.Reload(Path.Combine(dir, "Deps.fsproj")) |> within
                          let graph = (workspace.ProjectOf a).Value
                          let! diagnostics = graph.Diagnostics a |> within
                          Expect.isTrue (diagnostics |> Array.exists (fun d -> d.Message.Contains "string")) "the open text is checked"
                      })
          }

          testTask "fsproj change reloads" {
              let dir = copyFixture "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let extra = Path.Combine(dir, "F.fs")
              File.WriteAllText(extra, "module Deps.F\n\nlet f = 1\n")
              let! (lsp: LspClient), (owner: IAsyncDisposable) = startServer (Paths.toUri dir) "{}"
              use _ = owner
              let seen, waitFor, _ = watchWorkspace lsp

              let load =
                  JsonNode.Parse($$"""{"TextDocuments":[{"Uri":{{JsonValue.Create(Paths.toUri project).ToJsonString()}}}]}""")

              let! _ = lsp.Request("fsharp/workspaceLoad", load) |> within
              let filesOf (n: JsonNode) = n["Data"].["Files"].AsArray() |> Seq.map (fun (n: JsonNode) -> n.GetValue<string>()) |> List.ofSeq
              let first = seen.ToArray() |> Array.find (kindIs "project")
              Expect.isFalse (filesOf first |> List.contains extra) "F.fs is not part of the project yet"

              File.WriteAllText(project, File.ReadAllText(project).Replace("""<Compile Include="E.fs" />""", """<Compile Include="E.fs" /><Compile Include="F.fs" />"""))

              let changes =
                  JsonNode.Parse($$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri project).ToJsonString()}},"type":2}]}""")

              do! lsp.Notify("workspace/didChangeWatchedFiles", changes)

              do! waitFor "a project notification with F.fs" (fun n -> kindIs "project" n && filesOf n |> List.contains extra) |> withinT
          }

          testTask "dispose stops checks" {
              let host = GraphHost.Start "dispose-test"
              use _ = disposing host
              let! (spec: ProjectSpec) = ProjectSpec.load depsProject
              let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec) |> within
              let! _ = graph.AllDiagnostics() |> within
              let before = graph.FileLevelChecks
              (graph :> IDisposable).Dispose()
              // the disposal is posted: once this read returns, it has run
              do! host.Read(fun () -> ()) |> within
              graph.SetText(depsA, File.ReadAllText depsA + "\nlet afterDispose = 1\n") |> ignore
              do! Task.Delay 1500
              Expect.equal graph.FileLevelChecks before "no CheckFile after Dispose"
          }

          testTask "a check held when its graph is disposed starts no more checks" {
              let host = GraphHost.Start "dispose-held"
              use _ = disposing host
              let! (spec: ProjectSpec) = ProjectSpec.load depsProject
              let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec) |> within
              let! _ = graph.AllDiagnostics() |> within
              let gate = new ManualResetEventSlim(false)
              let reached = TaskCompletionSource()

              // The first file's check is held; the files after it would be checked once it returns
              graph.OnScopedCheck(fun _ ->
                  reached.TrySetResult() |> ignore
                  gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)

              try
                  graph.SetText(depsA, File.ReadAllText depsA + "\nlet inFlight = 1\n") |> ignore
                  do! reached.Task.WaitAsync(TimeSpan.FromSeconds 60.)
                  let checksBefore = graph.FileLevelChecks
                  (graph :> IDisposable).Dispose()
                  do! host.Read(fun () -> ()) |> within
                  gate.Set()
                  do! Task.Delay 3000
                  Expect.isLessThanOrEqual (graph.FileLevelChecks - checksBefore) 1L "at most the check already running finishes"
              finally
                  gate.Set()
          }

          testTask "an Open during a load's cold check reaches the graph" {
              let dir = copyFixture "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let a = Path.Combine(dir, "A.fs")
              let e = Path.Combine(dir, "E.fs")
              let gate = new ManualResetEventSlim(false)
              let reached = TaskCompletionSource()

              do!
                  withWorkspace None (fun workspace _ ->
                      task {
                          let hold (graph: ProjectGraph) =
                              graph.OnScopedCheck(fun path ->
                                  if String.Equals(path, e, StringComparison.OrdinalIgnoreCase) then
                                      reached.TrySetResult() |> ignore
                                      gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)

                          workspace.OnGraphCreated hold

                          try
                              let load = workspace.Load [ project ]
                              do! reached.Task.WaitAsync(TimeSpan.FromSeconds 120.)
                              Expect.isFalse load.IsCompleted "the cold check is held"
                              workspace.Open(a, File.ReadAllText a + "\nlet broken : int = \"text\"\n")
                              gate.Set()
                              let! _ = load |> within
                              let graph = (workspace.ProjectOf a).Value
                              let! diagnostics = graph.Diagnostics a |> within
                              Expect.isTrue (diagnostics |> Array.exists (fun d -> d.Message.Contains "string")) "the text opened during the load is checked"
                          finally
                              gate.Set()
                      })
          } ]
