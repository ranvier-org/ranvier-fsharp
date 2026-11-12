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

/// `path`'s graph, settled, held weakly. Not a task: a task's state machine would hold the graph as a local.
[<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
let private settledWeakly (workspace: Workspace) (path: string) : WeakReference =
    let graph = (workspace.ProjectOf path).Value
    graph.AllDiagnostics().Wait(TimeSpan.FromSeconds 120.) |> ignore
    WeakReference graph

let private collected (weak: WeakReference) =
    for _ in 1..3 do
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

    not weak.IsAlive

/// Drops the graph's event log, which a Debug build of Ranvier (RANVIER_TRACE) keeps with every value its nodes
/// published (as ReferencesIndexTests does). A no-op for an untraced Ranvier.
let private dropTrace (host: GraphHost) : Task =
    host.Read(fun () ->
        let graph = host.Graph

        let traceModule =
            [ "Ranvier.TraceModule"; "Ranvier.Trace" ]
            |> List.tryPick (fun name ->
                match typeof<Ranvier.Graph>.Assembly.GetType name with
                | null -> None
                | t -> Option.ofObj (t.GetMethod "lastSeq") |> Option.map (fun m -> t, m))

        match traceModule with
        | Some(t, lastSeq) ->
            let last = lastSeq.Invoke(null, [| box graph |])
            t.GetMethod("checkpoint").Invoke(null, [| box graph; last; box "dropped" |]) |> ignore
        | None -> ())

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
                  withWorkspace (Some(TimeSpan.FromMilliseconds 200.)) (fun workspace sent ->
                      task {
                          // The cold check cannot finish before the timeout: it is held at a file's first check
                          let gate = new ManualResetEventSlim(false)

                          workspace.OnGraphCreated(fun graph ->
                              graph.OnScopedCheck(fun _ -> gate.Wait(TimeSpan.FromSeconds 60.) |> ignore))

                          try
                              let! results = workspace.Load [ depsProject ] |> within

                              match results with
                              | [ Failed(p, reason) ] ->
                                  Expect.equal p depsProject "Deps"
                                  Expect.stringContains reason "timed out" "the reason names the timeout"
                              | other -> failtest $"unexpected results %A{other}"

                              Expect.isEmpty workspace.Projects "the stalled graph was disposed and dropped"
                              Expect.isTrue (sent.Notifications |> Array.exists (fun (m, _) -> m = "window/showMessage")) "the user is told"
                          finally
                              gate.Set()
                      })
          }

          test "the load timeout comes from RFS_LOAD_TIMEOUT_MS" {
              Expect.equal (Workspace.ParseLoadTimeout null) (TimeSpan.FromMinutes 10.) "ten minutes when unset"
              Expect.equal (Workspace.ParseLoadTimeout "") (TimeSpan.FromMinutes 10.) "ten minutes when empty"
              Expect.equal (Workspace.ParseLoadTimeout "250") (TimeSpan.FromMilliseconds 250.) "read from positive milliseconds"
              Expect.equal (Workspace.ParseLoadTimeout "0") (TimeSpan.FromMinutes 10.) "non-positive falls back to default"
              Expect.equal (Workspace.ParseLoadTimeout "-5") (TimeSpan.FromMinutes 10.) "negative falls back to default"
              Expect.equal (Workspace.ParseLoadTimeout "invalid") (TimeSpan.FromMinutes 10.) "unparseable falls back to default"

              Expect.equal
                  (Workspace.DefaultLoadTimeout())
                  (Workspace.ParseLoadTimeout(Environment.GetEnvironmentVariable "RFS_LOAD_TIMEOUT_MS"))
                  "DefaultLoadTimeout delegates to RFS_LOAD_TIMEOUT_MS"
          }

          testTask "open texts are applied again when a project is reloaded" {
              let dir = copyFixture "Deps"
              let a = Path.Combine(dir, "A.fs")

              do!
                  withWorkspace None (fun workspace _ ->
                      task {
                          let! _ = workspace.Load [ Path.Combine(dir, "Deps.fsproj") ] |> within
                          workspace.Open(a, File.ReadAllText a + "\nlet broken : int = \"text\"\n") |> ignore
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

          testTask "a client that can register file watchers is asked to, and one that cannot is not" {
              let registrations (lsp: LspClient) =
                  lsp.ServerRequests |> Array.filter (fun r -> r["method"].GetValue<string>() = "client/registerCapability")

              let! (lsp: LspClient), (owner: IDisposable) = TestServer.start ()
              use _ = disposing owner
              let! _ = lsp.Initialize(Paths.toUri depsDir, JsonNode.Parse "{}", watchers = true) |> within
              let deadline = DateTime.UtcNow.AddSeconds 30.

              while (registrations lsp).Length = 0 && DateTime.UtcNow < deadline do
                  do! Task.Delay 20

              let sent = registrations lsp |> Array.exactlyOne
              let registration = sent["params"].["registrations"].[0]
              Expect.equal (registration["method"].GetValue<string>()) "workspace/didChangeWatchedFiles" "the method"

              let globs =
                  registration["registerOptions"].["watchers"].AsArray()
                  |> Seq.map (fun w -> w["globPattern"].GetValue<string>())
                  |> Set.ofSeq

              Expect.equal
                  globs
                  (set
                      [ "**/*.{fs,fsi}"
                        "**/*.fsproj"
                        "**/*.props"
                        "**/*.targets"
                        "**/project.assets.json"
                        "**/*.{sln,slnx}"
                        "**/*" ])
                  "the globs"

              let! (plain: LspClient), (plainOwner: IDisposable) = TestServer.start ()
              use _ = disposing plainOwner
              let! _ = plain.Initialize(Paths.toUri depsDir, JsonNode.Parse "{}") |> within
              let! _ = plain.Request("fsharp/workspacePeek", JsonNode.Parse($$"""{"Directory":{{JsonValue.Create(depsDir).ToJsonString()}},"Deep":1,"ExcludedDirs":[]}""")) |> within
              Expect.isEmpty (registrations plain) "no registration"
          }

          testTask "a change on disk to a closed file and to a project file updates diagnostics" {
              let dir = copyFixture "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let a = Path.Combine(dir, "A.fs")
              let b = Path.Combine(dir, "B.fs")
              let! (lsp: LspClient), (owner: IDisposable) = TestServer.start ()
              use _ = disposing owner
              let latest = ConcurrentDictionary<string, Rfs.Lsp.Diagnostic list>(StringComparer.OrdinalIgnoreCase)

              lsp.Diagnostics.Subscribe
                  { new IObserver<_> with
                      member _.OnNext((_, path, _, list)) = latest[Path.GetFullPath path] <- list
                      member _.OnError _ = ()
                      member _.OnCompleted() = () }
              |> ignore

              let! _ = lsp.Initialize(Paths.toUri dir, JsonNode.Parse "{}", watchers = true) |> within
              let load = JsonNode.Parse($$"""{"TextDocuments":[{"Uri":{{JsonValue.Create(Paths.toUri project).ToJsonString()}}}]}""")
              let! _ = lsp.Request("fsharp/workspaceLoad", load) |> within
              do! lsp.DidOpen(b, File.ReadAllText b)

              let waitFor (what: string) (predicate: Rfs.Lsp.Diagnostic list -> bool) : Task =
                  task {
                      let deadline = DateTime.UtcNow.AddSeconds 60.

                      while not (match latest.TryGetValue b with
                                 | true, list -> predicate list
                                 | _ -> false) do
                          if DateTime.UtcNow > deadline then
                              failtest $"timed out waiting for %s{what}: %A{latest.ToArray()}"

                          do! Task.Delay 50
                  }

              let changed (path: string) (kind: int) =
                  lsp.Notify("workspace/didChangeWatchedFiles", JsonNode.Parse($$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri path).ToJsonString()}},"type":{{kind}}}]}"""))

              do! waitFor "B.fs clean" List.isEmpty |> withinT

              // A closed file B.fs depends on loses what B.fs reads
              let original = File.ReadAllText a
              File.WriteAllText(a, original.Replace("baseValue", "renamedValue"))
              do! changed a 2
              do! waitFor "the error from A.fs's change" (List.exists (fun d -> d.Message.Contains "baseValue")) |> withinT

              // Back, then the project drops the file
              File.WriteAllText(a, original)
              do! changed a 2
              do! waitFor "B.fs clean again" List.isEmpty |> withinT
              File.WriteAllText(project, File.ReadAllText(project).Replace("""<Compile Include="A.fs" />""", ""))
              do! changed project 2
              do! waitFor "the error from the project's change" (fun list -> not list.IsEmpty) |> withinT
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
                              workspace.Open(a, File.ReadAllText a + "\nlet broken : int = \"text\"\n") |> ignore
                              gate.Set()
                              let! _ = load |> within
                              let graph = (workspace.ProjectOf a).Value
                              let! diagnostics = graph.Diagnostics a |> within
                              Expect.isTrue (diagnostics |> Array.exists (fun d -> d.Message.Contains "string")) "the text opened during the load is checked"
                          finally
                              gate.Set()
                      })
          }

          // In a Debug build, Ranvier's event log holds the old graph through the entries collection's published Entry
          // (Graph -> Owner -> TraceLog -> TraceEvent[] -> Entry -> ProjectGraph): dropped, nothing else may hold it
          testTask "a reloaded project's old graph is collected once the reload settles" {
              let dir = copyFixture "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let a = Path.Combine(dir, "A.fs")
              let sent = Sent()
              let host = GraphHost.Start "workspace-reload-gc"
              use _ = disposing host
              let workspace = new Workspace(host, sent.Client)
              use _ = disposing workspace
              let! _ = workspace.Load [ project ] |> within
              let old = settledWeakly workspace a
              do! workspace.Reload project |> within
              let fresh = settledWeakly workspace a
              Expect.isTrue fresh.IsAlive "the new graph is loaded"
              do! dropTrace host
              Expect.isTrue (collected old) "the old graph is collected"
          } ]

/// Holds every check of the graph that a dependency computation leads to, until released
let private holdChecks (graph: ProjectGraph) =
    let gate = new ManualResetEventSlim(false)

    graph.OnFlight(fun kind _ ct ->
        if kind = "dependencies" then
            gate.Wait ct)

    gate

/// Hovers B's `counted` settled, then with A edited and the checks held: answered from the last state, marked, until a
/// reload is under way, when it waits. Gives the graph weakly, B and the position.
let private hoverBeforeReload (workspace: Workspace) (answers: Hover.LastAnswers) (dir: string) =
    task {
        let a = Path.Combine(dir, "A.fs")
        let b = Path.Combine(dir, "B.fs")
        let graph = (workspace.ProjectOf b).Value
        let text = File.ReadAllText b
        let index = text.IndexOf("counted", StringComparison.Ordinal)
        let before = text.Substring(0, index)
        let line, character = before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1)
        let hover ct = Hover.answer answers workspace.LoadingNow workspace.OwnersOf graph b line character ct
        let! settled = hover CancellationToken.None |> within
        Expect.isSome settled "the settled hover answers"

        let gate = holdChecks graph
        graph.SetText(a, File.ReadAllText a + "\nlet added = 1\n") |> ignore

        try
            let! marked = hover CancellationToken.None |> fun t -> t.WaitAsync(TimeSpan.FromSeconds 20.)
            Expect.equal marked (settled |> Option.map (fun t -> Hover.StaleMark + "\n\n" + t)) "pending, B answers from the last state"

            // A reload under way, read in the turn of the read: it refuses, and the hover waits for the check
            use _ = workspace.HoldLoading()
            let! last = Hover.fromLastState answers workspace.LoadingNow graph b line character CancellationToken.None |> within
            Expect.equal last (Hover.LastAnswer.Refused Refusal.Reloading) "a reload under way refuses"
            let waiting = hover CancellationToken.None
            let! _ = Task.WhenAny(waiting, Task.Delay 1500)
            Expect.isFalse waiting.IsCompleted "a hover during a reload waits for the check"
            gate.Set()
            let! after = waiting |> within
            Expect.equal after settled "once checked, the hover is the settled one, unmarked"
        finally
            gate.Set()

        return WeakReference graph, b, line, character
    }

[<Tests>]
let lastStateReload =
    testSequenced
    <| testList
        "LastStateReload"
        [ testTask "a reload refuses answers from the last state, and its new graph has none of the old graph's" {
              let dir = copyFixture "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let answers = Hover.LastAnswers()

              do!
                  withWorkspace None (fun workspace _ ->
                      task {
                          let! _ = workspace.Load [ project ] |> within
                          let! old, b, line, character = hoverBeforeReload workspace answers dir
                          do! workspace.Reload project |> within
                          let graph = (workspace.ProjectOf b).Value
                          Expect.isFalse (obj.ReferenceEquals(graph, old.Target)) "a reload makes a new graph"
                          let! _ = graph.AllDiagnostics() |> within
                          let a = Path.Combine(dir, "A.fs")
                          let gate = holdChecks graph
                          graph.SetText(a, File.ReadAllText a + "\nlet again = 2\n") |> ignore

                          try
                              let! last = Hover.fromLastState answers workspace.LoadingNow graph b line character CancellationToken.None |> within

                              match last with
                              | Hover.LastAnswer.Marked(m, _) -> failtest $"the new graph answered from the old graph's answers: %s{m}"
                              | Hover.LastAnswer.Settled -> failtest "settled while held"
                              | Hover.LastAnswer.Refused _ -> ()
                          finally
                              gate.Set()

                          // Whether the old graph is collected says nothing of the cache here: a reload with no hover keeps
                          // it alive too. LastStateGrid's "nothing of the cache keeps a graph alive" checks the cache.
                          ()
                      })
          } ]
