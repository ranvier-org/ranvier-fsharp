module Rfs.Server.Tests.ProgressTests

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests
open Rfs.Server.Tests.PublishTests
open Rfs.Server.Tests.PublishFixTests

let private multiDir = Path.Combine(Path.GetDirectoryName depsDir, "Multi")

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

/// A server initialized by hand, with or without `window.workDoneProgress`, and what it sent about progress
type private Run =
    { Lsp: LspClient
      Server: RanvierServer
      Owner: IDisposable
      /// "create", "begin <message>", "report <message>" or "end", in arrival order; a create is noted before the first
      /// progress of its token
      Events: ConcurrentQueue<string>
      /// Each begin's title
      Titles: ConcurrentQueue<string>
      /// Each window/showMessage, as "<type> <message>"
      Messages: ConcurrentQueue<string>
      /// When each file's list arrived, Stopwatch ticks
      Publishes: ConcurrentQueue<string * int64> }

    member r.Seen = List.ofSeq r.Events

    member r.Creates =
        r.Lsp.ServerRequests |> Array.filter (fun m -> m["method"].GetValue<string>() = "window/workDoneProgress/create")

    interface IAsyncDisposable with
        member r.DisposeAsync() =
            r.Owner.Dispose()
            ValueTask()

let private startInWith (root: string) (progress: bool) (prepare: LspClient -> unit) (configure: RanvierServer -> unit) : Task<Run> =
    task {
        let! lsp, owner, server = TestServer.startCapturing ()
        prepare lsp
        let! made = server |> within
        configure made
        let events = ConcurrentQueue<string>()
        let titles = ConcurrentQueue<string>()
        let messages = ConcurrentQueue<string>()
        let publishes = ConcurrentQueue<string * int64>()
        let noted = ConcurrentDictionary<string, bool>()

        lsp.Notifications.Subscribe
            { new IObserver<Received> with
                member _.OnNext r =
                    match r.Message["method"].GetValue<string>() with
                    | "$/progress" ->
                        let p = r.Message["params"]
                        let token = p["token"].ToJsonString()

                        let created =
                            lsp.ServerRequests
                            |> Array.exists (fun m ->
                                m["method"].GetValue<string>() = "window/workDoneProgress/create"
                                && m["params"].["token"].ToJsonString() = token)

                        if created && noted.TryAdd(token, true) then
                            events.Enqueue "create"

                        let value = p["value"]

                        let message () =
                            match value["message"] with
                            | null -> ""
                            | m -> m.GetValue<string>()

                        match value["kind"].GetValue<string>() with
                        | "begin" ->
                            titles.Enqueue(value["title"].GetValue<string>())
                            events.Enqueue $"begin %s{message ()}"
                        | "report" -> events.Enqueue $"report %s{message ()}"
                        | kind -> events.Enqueue kind
                    | "window/showMessage" ->
                        let p = r.Message["params"]
                        messages.Enqueue $"""%d{p["type"].GetValue<int>()} %s{p["message"].GetValue<string>()}"""
                    | "textDocument/publishDiagnostics" ->
                        publishes.Enqueue(LspClient.NormalizePath(Paths.ofUri (r.Message["params"].["uri"].GetValue<string>())), r.ReceivedAt)
                    | _ -> ()

                member _.OnError _ = ()
                member _.OnCompleted() = () }
        |> ignore

        let capabilities = if progress then """{"window":{"workDoneProgress":true}}""" else "{}"

        let parameters =
            JsonNode.Parse $"""{{"processId":null,"rootUri":"%s{Paths.toUri root}","capabilities":%s{capabilities}}}"""

        let! _ = lsp.Request("initialize", parameters) |> within
        do! lsp.Notify("initialized", JsonObject())
        let! running = server |> within
        // Initialized runs after the notification is read: the next request is answered after it
        let peek = JsonObject()
        peek["Directory"] <- JsonValue.Create root
        peek["Deep"] <- JsonValue.Create 0
        peek["ExcludedDirs"] <- JsonArray()
        let! _ = lsp.Request("fsharp/workspacePeek", peek) |> within

        return
            { Lsp = lsp
              Server = running
              Owner = owner
              Events = events
              Titles = titles
              Messages = messages
              Publishes = publishes }
    }

let private startIn (root: string) (progress: bool) (prepare: LspClient -> unit) = startInWith root progress prepare ignore

let private start (progress: bool) = startIn depsDir progress ignore

let private loadProject (r: Run) (project: string) : Task =
    task {
        let parameters =
            JsonNode.Parse $"""{{"TextDocuments":[{{"Uri":"%s{Paths.toUri project}"}}]}}"""

        let! _ = r.Lsp.Request("fsharp/workspaceLoad", parameters) |> within
        ()
    }

let private load (r: Run) = loadProject r depsProject

let private until (what: string) (r: Run) (check: unit -> Task<bool>) : Task =
    task {
        let deadline = DateTime.UtcNow.AddSeconds 60.
        let mutable ok = false

        while not ok do
            let! now = check ()
            ok <- now

            if not ok then
                if DateTime.UtcNow > deadline then
                    let! status = r.Server.ProgressStatus()
                    failtest $"timed out waiting for %s{what}: seen %A{r.Seen}, status %A{status}"

                do! Task.Delay 10
    }

/// Waits for the events to end with an end, then for a quiet that would let any further one through
let private settledEvents (r: Run) =
    task {
        do! until "the progress to end" r (fun () -> Task.FromResult(r.Seen <> [] && List.last r.Seen = "end"))
        do! Task.Delay 500
        return r.Seen
    }

/// Waits until nothing is pending and no progress is shown: a load may have been too short to show one
let private quiet (r: Run) =
    task {
        do!
            until "quiet" r (fun () ->
                task {
                    let! status = r.Server.ProgressStatus()
                    return status = Some ProgressStatus.Settled && (r.Seen = [] || List.last r.Seen = "end")
                })

        do! Task.Delay 500
    }

let private settledStatus (r: Run) =
    task {
        let! status = r.Server.ProgressStatus() |> within
        Expect.equal status (Some ProgressStatus.Settled) "nothing is pending after the end"
    }

/// How many times the status the progress follows changed
let private changes (r: Run) = r.Server.ProgressChanges()

/// Holds loading until the progress has seen it, and returns the hold
let private holdSeen (r: Run) : Task<IDisposable> =
    task {
        let! before = changes r
        let held = r.Server.Workspace.HoldLoading()
        do! until "the hold seen" r (fun () -> task { let! n = changes r in return n > before })
        return held
    }

/// Ends `held` and waits until the progress has seen it
let private releaseSeen (r: Run) (held: IDisposable) : Task =
    task {
        let! before = changes r
        held.Dispose()
        do! until "the release seen" r (fun () -> task { let! n = changes r in return n > before })
    }

/// Holds the parse of `path` in `graph`'s next flight for it, until the returned gate is set
let private holdParse (graph: Rfs.Checker.ProjectGraph) (path: string) =
    let hold = new ManualResetEventSlim false
    let armed = ref true

    graph.OnFlight(fun kind file _ ->
        if armed.Value && kind = "parse" && String.Equals(file, path, StringComparison.OrdinalIgnoreCase) then
            armed.Value <- false
            hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)

    hold

let private closeDocument (r: Run) (path: string) =
    r.Lsp.Notify("textDocument/didClose", JsonNode.Parse $"""{{"textDocument":{{"uri":"%s{Paths.toUri path}"}}}}""")

/// Opens `path` and waits until its open has settled and shows nothing
let private openQuiet (r: Run) (path: string) =
    task {
        do! r.Lsp.DidOpen(path, File.ReadAllText path)
        do! r.Server.Publisher.Flushed()
        do! quiet r
    }

let private editAt (line: int) (from: int) (until': int) (text: string) : Edit =
    { Range = { Start = { Line = line; Character = from }; End = { Line = line; Character = until' } }
      Text = text }


/// Holds every check of the graph that a dependency computation leads to, until the gate is set
let private holdChecks (graph: Rfs.Checker.ProjectGraph) =
    let gate = new ManualResetEventSlim false
    let armed = ref true

    graph.OnFlight(fun kind _ ct ->
        if armed.Value && kind = "dependencies" then
            gate.Wait ct)

    gate, armed

/// The references request at a 0-based position, its locations as text
let private referencesAt (r: Run) (path: string) (line: int) (character: int) : Task<string list> =
    let parameters =
        JsonNode.Parse
            $"""{{"textDocument":{{"uri":"%s{Paths.toUri path}"}},"position":{{"line":%d{line},"character":%d{character}}},"context":{{"includeDeclaration":true}}}}"""

    task {
        let! result, _ = r.Lsp.Request("textDocument/references", parameters)

        return
            [ for l in result.AsArray() do
                  let range = l["range"]
                  yield $"""%s{Path.GetFileName(Paths.ofUri (l["uri"].GetValue<string>()))} %s{range.ToJsonString()}""" ]
            |> List.sort
    }

/// A references request during a held check, after the same one settled before an edit of another row
let private referencesDuringHeldCheck (progress: bool) =
    task {
        use! r = start progress
        do! load r

        // Without the capability there is no status to wait on: the settled request below waits instead
        if progress then
            do! quiet r
            do! openQuiet r a
        else
            do! r.Lsp.DidOpen(a, File.ReadAllText a)
            do! r.Server.Publisher.Flushed()

        // `scale`, whose uses are in A's rows 4 and 15 and in B: the edit is of row 17
        let! settled = referencesAt r a 4 4 |> within
        Expect.isGreaterThan settled.Length 2 "scale is used"
        let graph = (r.Server.Workspace.ProjectOf a).Value
        let gate, armed = holdChecks graph

        try
            do! r.Lsp.DidChange(a, 1, editAt 17 33 34 "3")
            let asked = referencesAt r a 4 4
            let! _ = Task.WhenAny(asked, Task.Delay(if progress then 20000 else 1500))

            if progress then
                Expect.isTrue asked.IsCompleted "answered from the last state while the check is held"
                Expect.equal asked.Result settled "the answer before the edit"
                Expect.equal r.Server.LastStateAnswers 1L "one answer from the last state"
            else
                Expect.isFalse asked.IsCompleted "without the capability it waits for the check"
                Expect.equal r.Server.LastStateAnswers 0L "nothing from the last state"
        finally
            armed.Value <- false
            gate.Set()

        if progress then
            do! quiet r

        let! after = referencesAt r a 4 4 |> within
        Expect.equal after settled "settled, the answer is unchanged"
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Lifecycle progress"
        [ testTask "a pending period gives create, begin and end, in that order" {
              use! r = start true
              let held = r.Server.Workspace.HoldLoading()
              do! Task.Delay 400
              held.Dispose()
              let! seen = settledEvents r
              Expect.equal seen [ "create"; "begin Loading projects"; "end" ] "one period, its token created first"
              Expect.equal r.Creates.Length 1 "one token"
              Expect.equal (List.ofSeq r.Titles) [ "F#" ] "the title is fixed; the message says what is pending"
              do! settledStatus r
          }

          testTask "a check under way shows Checking, and ends when it settles" {
              use! r = start true
              do! load r
              do! quiet r
              do! openQuiet r a
              let before = r.Seen.Length
              let hold = holdParse (r.Server.Workspace.ProjectOf a).Value a
              do! r.Lsp.DidChange(a, 1, editAt 2 16 18 "11")
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen |> List.skip before |> List.exists (fun e -> e.StartsWith "begin")))
              hold.Set()
              let! seen = settledEvents r
              Expect.equal (seen |> List.skip before) [ "create"; "begin Checking"; "end" ] "the edit's period"
              do! settledStatus r
          }

          testTask "a change of what is pending is a report in the same period" {
              use! r = start true
              do! load r
              do! quiet r
              do! openQuiet r a
              let before = r.Seen.Length
              let creates = r.Creates.Length
              let held = r.Server.Workspace.HoldLoading()
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen.Length > before + 1))
              let hold = holdParse (r.Server.Workspace.ProjectOf a).Value a
              do! r.Lsp.DidChange(a, 1, editAt 2 16 18 "11")
              do! Task.Delay 300
              held.Dispose()
              do! until "a report" r (fun () -> Task.FromResult(r.Seen |> List.exists (fun e -> e.StartsWith "report")))
              hold.Set()
              let! seen = settledEvents r

              Expect.equal
                  (seen |> List.skip before)
                  [ "create"; "begin Loading projects"; "report Checking"; "end" ]
                  "one period, its message following what is pending"

              Expect.equal (r.Creates.Length - creates) 1 "one token"
              do! settledStatus r
          }

          testTask "without the capability nothing is sent" {
              use! r = start false
              let held = r.Server.Workspace.HoldLoading()
              do! Task.Delay 400
              held.Dispose()
              do! load r
              do! Task.Delay 500
              Expect.isEmpty r.Seen "no progress"
              Expect.isEmpty r.Creates "no token created"
          }

          testTask "a flip shorter than the hold sends nothing" {
              use! r = start true
              let! first = changes r

              for _ in 1..5 do
                  let! (held: IDisposable) = holdSeen r
                  held.Dispose()
                  do! Task.Delay 20

              do! Task.Delay 500
              let! last = changes r
              Expect.isGreaterThanOrEqual (last - first) 10 "the status was Busy, and settled, five times"
              Expect.isEmpty r.Seen "no progress"
              Expect.isEmpty r.Creates "no token created"
          }

          testTask "settling for less than the hold keeps one period" {
              use! r = start true
              let held = r.Server.Workspace.HoldLoading()
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen.Length >= 2))
              do! releaseSeen r held
              let again = r.Server.Workspace.HoldLoading()
              do! Task.Delay 300
              Expect.equal r.Seen [ "create"; "begin Loading projects" ] "still the one period"
              again.Dispose()
              let! seen = settledEvents r
              Expect.equal seen [ "create"; "begin Loading projects"; "end" ] "one period"
              Expect.equal r.Creates.Length 1 "one token"
          }

          testTask "a slow create holds back no diagnostics" {
              let answered = ref 0L

              let slow (_: JsonNode) =
                  task {
                      do! Task.Delay 3000
                      Volatile.Write(&answered.contents, Stopwatch.GetTimestamp())
                      return Ok(null: JsonNode)
                  }

              use! r = start true
              do! load r
              do! quiet r
              r.Lsp.AnswerWith("window/workDoneProgress/create", slow)
              let creates = r.Creates.Length
              let before = r.Seen.Length
              let held = r.Server.Workspace.HoldLoading()
              do! until "the create" r (fun () -> Task.FromResult(r.Creates.Length > creates))
              let flushedAt = Stopwatch.StartNew()
              do! r.Lsp.DidOpen(a, File.ReadAllText a)
              do! r.Server.Publisher.Flushed()
              let flushed = flushedAt.Elapsed
              let own = LspClient.NormalizePath a

              do!
                  until "A's list" r (fun () ->
                      Task.FromResult(r.Publishes |> Seq.exists (fun (p, _) -> String.Equals(p, own, StringComparison.OrdinalIgnoreCase))))

              let _, arrived = r.Publishes |> Seq.find (fun (p, _) -> String.Equals(p, own, StringComparison.OrdinalIgnoreCase))
              Expect.equal (Volatile.Read &answered.contents) 0L "A's list arrived before the create was answered"
              Expect.isLessThan flushed (TimeSpan.FromSeconds 2.) $"the outbox did not wait for the answer (%O{flushed})"
              do! until "the begin" r (fun () -> Task.FromResult(r.Seen |> List.skip before |> List.exists (fun e -> e.StartsWith "begin")))
              Expect.isGreaterThan (Volatile.Read &answered.contents) arrived "the begin followed the answer"
              held.Dispose()
              let! seen = settledEvents r
              Expect.equal (seen |> List.skip before) [ "create"; "begin Loading projects"; "end" ] "the period, begun once answered"
          }

          testTask "a create answered with an error sends nothing for its period" {
              let refuse = ref true

              let answer (_: JsonNode) =
                  if refuse.Value then
                      Task.FromResult(Error(JsonNode.Parse """{"code":-32603,"message":"refused"}"""))
                  else
                      Task.FromResult(Ok(null: JsonNode))

              use! r = startIn depsDir true (fun lsp -> lsp.AnswerWith("window/workDoneProgress/create", answer))
              let held = r.Server.Workspace.HoldLoading()
              do! until "the create" r (fun () -> Task.FromResult(r.Creates.Length = 1))
              do! Task.Delay 400
              held.Dispose()
              do! Task.Delay 500
              Expect.isEmpty r.Seen "no progress for the refused period"
              refuse.Value <- false
              let again = r.Server.Workspace.HoldLoading()
              do! Task.Delay 400
              again.Dispose()
              let! seen = settledEvents r
              Expect.equal seen [ "create"; "begin Loading projects"; "end" ] "the next period is shown"
              Expect.equal r.Creates.Length 2 "a token per period"
          }

          // The library ends the run on `shutdown` without calling the server's Shutdown, and closes the connection
          // before the server is disposed, so these dispose the server while the connection is open
          testTask "disposing the server ends a period begun, on the connection still open" {
              use! r = start true
              let _held = r.Server.Workspace.HoldLoading()
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen.Length >= 2))
              (r.Server :> IDisposable).Dispose()
              do! until "the end" r (fun () -> Task.FromResult(List.last r.Seen = "end"))
              Expect.equal r.Seen [ "create"; "begin Loading projects"; "end" ] "ended on dispose"
          }

          testTask "a create answered while the server is disposed begins nothing" {
              let slow (_: JsonNode) =
                  task {
                      do! Task.Delay 1000
                      return Ok(null: JsonNode)
                  }

              use! r = startIn depsDir true (fun lsp -> lsp.AnswerWith("window/workDoneProgress/create", slow))
              let _held = r.Server.Workspace.HoldLoading()
              do! until "the create" r (fun () -> Task.FromResult(r.Creates.Length = 1))
              (r.Server :> IDisposable).Dispose()
              do! Task.Delay 2000
              Expect.isEmpty r.Seen "no begin, so no end owed"
          }

          testTask "a failed restart is shown, naming the project, until a reload succeeds" {
              let dir = copyFixtureDir "Deps"
              let proj = Path.Combine(dir, "Deps.fsproj")
              use! r = startIn dir true ignore
              do! loadProject r proj
              do! quiet r
              let before = r.Seen.Length
              let graph = r.Server.Workspace.Projects.Head
              graph.FailRestart(InvalidOperationException "an injected restart fault")

              do!
                  until "the failure shown" r (fun () ->
                      Task.FromResult(r.Seen |> List.skip before |> List.exists (fun e -> e.StartsWith "begin" && e.Contains "Deps.fsproj")))

              do! Task.Delay 500
              Expect.equal (List.last r.Seen) "begin Restart failed: Deps.fsproj" "shown while the project is dead"
              do! r.Server.Workspace.Reload proj |> within
              let! seen = settledEvents r
              Expect.equal (List.last seen) "end" "cleared by the reload"
              do! settledStatus r
          }

          testTask "without the capability a failed restart is one warning naming the project" {
              use! r = start false
              do! load r
              let graph = r.Server.Workspace.Projects.Head
              graph.FailRestart(InvalidOperationException "an injected restart fault")
              do! until "the warning" r (fun () -> Task.FromResult(r.Messages |> Seq.exists (fun m -> m.Contains "Deps.fsproj")))
              graph.FailRestart(InvalidOperationException "a second injected restart fault")
              do! r.Server.Publisher.Flushed()
              do! Task.Delay 500
              let warnings = r.Messages |> Seq.filter (fun m -> m.Contains "Deps.fsproj") |> List.ofSeq
              Expect.equal warnings.Length 1 "once per graph"
              Expect.stringStarts warnings.Head "2 " "a Warning"
              Expect.isEmpty r.Seen "no progress"
              Expect.isEmpty r.Creates "no token created"
          }

          testTask "a close during a check ends the progress" {
              use! r = start true
              do! load r
              do! quiet r
              do! openQuiet r a
              let before = r.Seen.Length
              let hold = holdParse (r.Server.Workspace.ProjectOf a).Value a
              do! r.Lsp.DidChange(a, 1, editAt 2 16 18 "11")
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen.Length > before + 1))
              do! closeDocument r a
              do! r.Server.Publisher.Barrier()
              hold.Set()
              let! seen = settledEvents r
              Expect.equal (seen |> List.skip before) [ "create"; "begin Checking"; "end" ] "the period ends"
              do! settledStatus r
          }

          testTask "a file leaving its project during a check ends the progress" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let d = Path.Combine(dir, "D.fs")
              use! r = startIn dir true ignore
              do! loadProject r project
              do! quiet r
              do! openQuiet r d
              let before = r.Seen.Length
              let hold = holdParse (r.Server.Workspace.ProjectOf d).Value d
              do! r.Lsp.DidChange(d, 1, editAt 4 24 25 "2")
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen.Length > before + 1))
              File.WriteAllText(project, (File.ReadAllText project).Replace("""    <Compile Include="D.fs" />""", ""))
              let reloading = r.Server.Workspace.Reload project
              hold.Set()
              do! reloading |> within
              let! seen = settledEvents r
              Expect.equal (List.last seen) "end" "the period ends"
              Expect.isEmpty (r.Server.Workspace.OwnersOf d) "D left the project"
              do! settledStatus r
          }

          testTask "references during a held check answer at once from the last state, and the same once settled" {
              do! referencesDuringHeldCheck true
          }

          testTask "without the capability references during a held check wait" {
              do! referencesDuringHeldCheck false
          }

          testTask "a check that settles inside the progress's begin hold gives no answer from the last state" {
              // The hold is this server's: no other test sees it
              use! r = startInWith depsDir true ignore (fun server -> server.ProgressBeginHold <- TimeSpan.FromSeconds 30.)
              do! load r
              do! r.Lsp.DidOpen(a, File.ReadAllText a)
              do! r.Server.Publisher.Flushed()
              let! settled = referencesAt r a 4 4 |> within
              let graph = (r.Server.Workspace.ProjectOf a).Value
              let gate, armed = holdChecks graph

              try
                  do! r.Lsp.DidChange(a, 1, editAt 17 33 34 "3")
                  let asked = referencesAt r a 4 4
                  let! _ = Task.WhenAny(asked, Task.Delay 2000)
                  Expect.isFalse asked.IsCompleted "pending, but the progress hasn't begun: it waits"
                  Expect.equal r.Server.LastStateAnswers 0L "nothing from the last state before the begin"
              finally
                  armed.Value <- false
                  gate.Set()

              let! after = referencesAt r a 4 4 |> within
              Expect.equal after settled "settled inside the hold: the settled answer"
              Expect.equal r.Server.LastStateAnswers 0L "nothing from the last state"
              Expect.isFalse (r.Seen |> List.exists (fun e -> e.StartsWith "begin")) "no begin was sent"
          }

          testTask "a fault of a referenced project on the graph thread ends Checking progress" {
              let root = copyMulti ()
              let app = Path.Combine(root, "App", "App.fsproj")
              let useFile = Path.Combine(root, "App", "Use.fs")
              let libFile = Path.Combine(root, "Lib", "Lib.fs")
              Rfs.Harness.Fixtures.build app
              use! r = startIn root true ignore
              do! loadProject r app
              do! quiet r
              do! openQuiet r useFile
              let before = r.Seen.Length
              let hold = holdParse (r.Server.Workspace.ProjectOf useFile).Value useFile
              do! r.Lsp.DidChange(useFile, 1, editAt 2 4 4 " ")
              do! until "a begin" r (fun () -> Task.FromResult(r.Seen.Length > before + 1))
              let libGraph = (r.Server.Workspace.ProjectOf libFile).Value
              let libHold = holdParse libGraph libFile
              libGraph.SetText(libFile, "// pending\n" + File.ReadAllText libFile) |> ignore
              libGraph.FailPost(InvalidOperationException "Lib faulted on graph thread")
              do! r.Server.Publisher.Barrier()
              hold.Set()
              let checkingEnds = until "Checking ends" r (fun () -> r.Server.Host.Read(fun () -> not (r.Server.Publisher.CheckingNow())))
              let! finished = Task.WhenAny(checkingEnds, Task.Delay 3000)
              Expect.isTrue (obj.ReferenceEquals(finished, checkingEnds)) "CheckingNow should become false after upstream project fault"
              libHold.Set()
              let! seen = settledEvents r
              Expect.equal (List.last seen) "end" "the period ends"
              do! settledStatus r
          } ]
