module Rfs.Server.Tests.ProgressTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests
open Rfs.Server.Tests.PublishTests

/// A server initialized by hand, with or without `window.workDoneProgress`, and what it sent about progress
type private Run =
    { Lsp: LspClient
      Server: RanvierServer
      Owner: IDisposable
      /// "create", "begin <title>", "report" or "end", in arrival order; a create is noted before the first progress of its
      /// token
      Events: ConcurrentQueue<string> }

    member r.Seen = List.ofSeq r.Events

    member r.Creates =
        r.Lsp.ServerRequests |> Array.filter (fun m -> m["method"].GetValue<string>() = "window/workDoneProgress/create")

    interface IAsyncDisposable with
        member r.DisposeAsync() =
            r.Owner.Dispose()
            ValueTask()

let private start (progress: bool) : Task<Run> =
    task {
        let! lsp, owner, server = TestServer.startCapturing ()
        let events = ConcurrentQueue<string>()
        let noted = ConcurrentDictionary<string, bool>()

        lsp.Notifications.Subscribe
            { new IObserver<Received> with
                member _.OnNext r =
                    if r.Message["method"].GetValue<string>() = "$/progress" then
                        let p = r.Message["params"]
                        let token = p["token"].ToJsonString()

                        let created =
                            lsp.ServerRequests
                            |> Array.exists (fun m ->
                                m["method"].GetValue<string>() = "window/workDoneProgress/create"
                                && m["params"].["token"].ToJsonString() = token)

                        if created && noted.TryAdd(token, true) then
                            events.Enqueue "create"

                        match p["value"].["kind"].GetValue<string>() with
                        | "begin" -> events.Enqueue $"""begin %s{p["value"].["title"].GetValue<string>()}"""
                        | kind -> events.Enqueue kind

                member _.OnError _ = ()
                member _.OnCompleted() = () }
        |> ignore

        let capabilities = if progress then """{"window":{"workDoneProgress":true}}""" else "{}"

        let parameters =
            JsonNode.Parse $"""{{"processId":null,"rootUri":"%s{Paths.toUri depsDir}","capabilities":%s{capabilities}}}"""

        let! _ = lsp.Request("initialize", parameters) |> within
        do! lsp.Notify("initialized", JsonObject())
        let! running = server |> within
        // Initialized runs after the notification is read: the next request is answered after it
        let! _ = lsp.Request("fsharp/workspacePeek", JsonNode.Parse $"""{{"Directory":"%s{depsDir.Replace("\\", "\\\\")}","Deep":0,"ExcludedDirs":[]}}""") |> within

        return
            { Lsp = lsp
              Server = running
              Owner = owner
              Events = events }
    }

let private load (r: Run) : Task =
    task {
        let parameters =
            JsonNode.Parse $"""{{"TextDocuments":[{{"Uri":"%s{Paths.toUri depsProject}"}}]}}"""

        let! _ = r.Lsp.Request("fsharp/workspaceLoad", parameters) |> within
        ()
    }

/// Waits for the events to end with an end, then for a quiet that would let any further one through
let private settledEvents (r: Run) =
    task {
        do! eventually "the progress to end" (fun () -> r.Seen <> [] && List.last r.Seen = "end")
        do! Task.Delay 500
        return r.Seen
    }

let private settledStatus (r: Run) =
    task {
        let! status = r.Server.ProgressStatus() |> within
        Expect.equal status (Some ProgressStatus.Settled) "nothing is pending after the end"
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
              do! settledStatus r
          }

          testTask "a check under way shows Checking, and ends when it settles" {
              use! r = start true
              do! load r
              let! _ = settledEvents r
              do! r.Lsp.DidOpen(a, File.ReadAllText a)
              do! r.Server.Publisher.Flushed()
              do! Task.Delay 500
              let graph = (r.Server.Workspace.ProjectOf a).Value
              let before = r.Seen.Length
              let hold = new ManualResetEventSlim false
              let armed = ref true

              graph.OnFlight(fun kind path _ ->
                  if armed.Value && kind = "parse" && String.Equals(path, a, StringComparison.OrdinalIgnoreCase) then
                      armed.Value <- false
                      hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)

              let edit: Edit =
                  { Range = { Start = { Line = 2; Character = 16 }; End = { Line = 2; Character = 18 } }
                    Text = "11" }

              do! r.Lsp.DidChange(a, 1, edit)
              do! eventually "a begin" (fun () -> r.Seen |> List.skip before |> List.exists (fun e -> e.StartsWith "begin"))
              hold.Set()
              let! seen = settledEvents r
              Expect.equal (seen |> List.skip before) [ "create"; "begin Checking"; "end" ] "the edit's period"
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

              for _ in 1..5 do
                  let held = r.Server.Workspace.HoldLoading()
                  do! Task.Delay 10
                  held.Dispose()
                  do! Task.Delay 20

              do! Task.Delay 500
              Expect.isEmpty r.Seen "no progress"
              Expect.isEmpty r.Creates "no token created"
          }

          testTask "a failed restart is shown, naming the project, until a reload succeeds" {
              use! r = start true
              do! load r
              let! _ = settledEvents r
              let before = r.Seen.Length
              let graph = r.Server.Workspace.Projects.Head
              graph.FailRestart(InvalidOperationException "an injected restart fault")

              do!
                  eventually "the failure shown" (fun () ->
                      r.Seen |> List.skip before |> List.exists (fun e -> e.StartsWith "begin" && e.Contains "Deps.fsproj"))

              do! Task.Delay 500
              Expect.equal (List.last r.Seen) "begin Restart failed: Deps.fsproj" "shown while the project is dead"
              do! r.Server.Workspace.Reload depsProject |> within
              let! seen = settledEvents r
              Expect.equal (List.last seen) "end" "cleared by the reload"
              do! settledStatus r
          } ]
