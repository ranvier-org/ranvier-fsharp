namespace Rfs.Harness

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Rfs.Lsp

type StepResult =
    { Index: int
      Step: string
      LatencyMs: float
      Publishes: int
      DeclsChecked: int option
      OracleDiff: string list option
      /// Why the step did not finish (timeout, server error); the run carries on with the next step.
      Error: string option }

type RunResult =
    { Server: string
      Scenario: string
      /// HEAD of the corpus checkout, or "unknown" when it is not a git repository.
      Commit: string
      StartedAt: DateTime
      ColdMs: float
      Steps: StepResult list
      PeakWorkingSetMb: float
      AllocatedMb: float
      Gen2: int }

type RunnerOptions =
    {
        /// How long a step may wait for its publishDiagnostics or response before it is marked failed.
        StepTimeout: TimeSpan
        /// How long no message may arrive before an edit or open step is over.
        Quiet: TimeSpan
    }

module Runner =
    let defaultOptions =
        { StepTimeout = TimeSpan.FromSeconds 120.
          Quiet = TimeSpan.FromMilliseconds 300. }

    /// Task 6 replaces this with `Diagnostics.compare`. Source is ignored: it names the server.
    let diffDiagnostics (expected: Diagnostic list) (actual: Diagnostic list) : string list =
        let key (d: Diagnostic) =
            $"%s{d.File} %d{d.Range.Start.Line}:%d{d.Range.Start.Character}-%d{d.Range.End.Line}:%d{d.Range.End.Character} sev=%d{d.Severity} %s{d.Code} %s{d.Message}"

        let counts items =
            items |> List.countBy key |> Map.ofList

        let e, a = counts expected, counts actual

        let surplus (label: string) (left: Map<string, int>) (right: Map<string, int>) =
            [ for KeyValue(k, n) in left do
                  let extra = n - (right |> Map.tryFind k |> Option.defaultValue 0)

                  for _ in 1..extra -> $"%s{label}: %s{k}" ]

        surplus "missing" e a @ surplus "unexpected" a e

    let private subscribe (source: IObservable<'T>) (f: 'T -> unit) =
        source.Subscribe
            { new IObserver<'T> with
                member _.OnNext value = f value
                member _.OnError _ = ()
                member _.OnCompleted() = () }

    /// Collects what the server sends while one step runs. Messages received before `Start` belong to an earlier step.
    type private Watch(client: LspClient, awaited: string list, version: int option, quiet: TimeSpan, timeout: TimeSpan) =
        let gate = obj ()
        let pending = HashSet<string>(awaited)
        let mutable sentAt = Stopwatch.GetTimestamp()
        let mutable completedAt = 0L
        let mutable lastArrival = 0L
        let mutable publishes = 0
        let mutable decls: int option = None

        let subscriptions =
            [ subscribe client.Diagnostics (fun (at, file, v, _) ->
                  lock gate (fun () ->
                      if at >= sentAt then
                          publishes <- publishes + 1
                          lastArrival <- max lastArrival at

                          let matches =
                              match version, v with
                              | Some wanted, Some got -> wanted = got
                              | _ -> true

                          if matches && pending.Remove file && pending.Count = 0 then
                              completedAt <- at))
              subscribe client.Notifications (fun received ->
                  lock gate (fun () ->
                      if received.ReceivedAt >= sentAt then
                          lastArrival <- max lastArrival received.ReceivedAt

                          let m = received.Message["method"]

                          if not (isNull m) && m.GetValue<string>() = "rfs/declarationsChecked" then
                              let parameters = received.Message["params"]
                              let count = parameters["count"].GetValue<int>()
                              decls <- Some((defaultArg decls 0) + count))) ]

        /// Call right before sending.
        member _.Start() = lock gate (fun () -> sentAt <- Stopwatch.GetTimestamp())

        member _.Publishes = lock gate (fun () -> publishes)
        member _.Decls = lock gate (fun () -> decls)

        /// Waits until every awaited file has its publish and the quiet window has passed.
        /// Returns the latency in ms, or the reason the step failed.
        member _.Finish() : Task<Result<float, float * string>> =
            task {
                let mutable outcome = None

                while outcome.IsNone do
                    let now = Stopwatch.GetTimestamp()

                    let state =
                        lock gate (fun () ->
                            let quietSince = Stopwatch.GetElapsedTime(max sentAt lastArrival, now)

                            // chatter that never stops must not turn a matched step into a failure
                            let settled =
                                quietSince >= quiet || Stopwatch.GetElapsedTime(sentAt, now) >= timeout

                            if pending.Count = 0 && settled then
                                // a step with nothing awaited (save) measures to its last message
                                let finish = if completedAt <> 0L then completedAt else lastArrival
                                let finish = max finish sentAt
                                Some(Ok(Stopwatch.GetElapsedTime(sentAt, finish).TotalMilliseconds))
                            else
                                None)

                    match state with
                    | Some r -> outcome <- Some r
                    | None ->
                        let elapsed = Stopwatch.GetElapsedTime(sentAt, now)

                        if elapsed >= timeout then
                            let waiting = lock gate (fun () -> String.Join(", ", pending))

                            outcome <-
                                Some(
                                    Error(
                                        elapsed.TotalMilliseconds,
                                        $"no matching publishDiagnostics within %.0f{timeout.TotalMilliseconds} ms (waiting for %s{waiting})"
                                    )
                                )
                        else
                            do! Task.Delay 5

                return outcome.Value
            }

        interface IDisposable with
            member _.Dispose() =
                for s in subscriptions do
                    s.Dispose()

    let private lastOffset (start: Position) (inserted: string) : Position =
        match inserted.LastIndexOf '\n' with
        | -1 ->
            { start with
                Character = start.Character + inserted.Length }
        | last ->
            { Line = start.Line + (inserted |> Seq.filter ((=) '\n') |> Seq.length)
              Character = inserted.Length - last - 1 }

    /// The edit that undoes `forward` when applied to the text `forward` produced.
    let private inverse (text: string) (forward: Edit) : Edit =
        let first = Document.offsetOf text forward.Range.Start
        let last = max first (Document.offsetOf text forward.Range.End)

        { Range =
            { Start = forward.Range.Start
              End = lastOffset forward.Range.Start forward.Text }
          Text = text.Substring(first, last - first) }

    let private describe (p: Position) = $"%d{p.Line}:%d{p.Character}"

    let private commitOf (repo: string) =
        try
            let info = ProcessStartInfo("git")
            info.ArgumentList.Add "-C"
            info.ArgumentList.Add repo
            info.ArgumentList.Add "rev-parse"
            info.ArgumentList.Add "HEAD"
            info.RedirectStandardOutput <- true
            info.RedirectStandardError <- true
            info.UseShellExecute <- false
            use proc = Process.Start info
            let output = proc.StandardOutput.ReadToEnd().Trim()
            proc.WaitForExit()
            if proc.ExitCode = 0 && output <> "" then output else "unknown"
        with _ ->
            "unknown"

    let private firstFile step =
        match step with
        | Open f
        | Save f
        | Edit(f, _, _, _)
        | Hover(f, _)
        | Completion(f, _)
        | Definition(f, _)
        | References(f, _) -> Some f
        | OpenMany fs -> List.tryHead fs

    /// Replays `scenario` over an already-connected client. `pid` is the server process to take GC and memory
    /// metrics from; with None (an in-process fake) those stay zero.
    let runConnected
        (options: RunnerOptions)
        (client: LspClient)
        (pid: int option)
        (serverName: string)
        (initOptions: JsonNode)
        (scenario: Scenario)
        (corpusRoot: string)
        (oracle: (Map<string, string> -> Task<Diagnostic list>) option)
        : Task<RunResult> =
        task {
            let startedAt = DateTime.UtcNow
            let repo = Scenario.repoRoot corpusRoot scenario
            let fullPath relative = LspClient.NormalizePath(Scenario.resolve corpusRoot scenario relative)
            let collector = pid |> Option.map MetricsCollector.Start
            let versions = Dictionary<string, int>()
            let opened = HashSet<string>()
            let latest = ConcurrentDictionary<string, Diagnostic list>()
            use latestSubscription = subscribe client.Diagnostics (fun (_, file, _, list) -> latest[file] <- list)
            let results = List<StepResult>()

            let record name latency (watch: Watch option) outcome =
                let error =
                    match outcome with
                    | Ok _ -> None
                    | Error(_: float, why: string) -> Some why

                let result =
                    { Index = results.Count
                      Step = name
                      LatencyMs = latency
                      Publishes = watch |> Option.map _.Publishes |> Option.defaultValue 0
                      DeclsChecked = watch |> Option.bind _.Decls
                      OracleDiff = None
                      Error = error }

                results.Add result
                result

            let awaitPublish name (files: string list) (version: int option) (send: unit -> Task) =
                task {
                    use watch = new Watch(client, files, version, options.Quiet, options.StepTimeout)

                    try
                        watch.Start()
                        do! send ()
                        let! outcome = watch.Finish()

                        match outcome with
                        | Ok latency -> return record name latency (Some watch) outcome
                        | Error(latency, _) -> return record name latency (Some watch) outcome
                    with
                    | e ->
                        return record name 0. (Some watch) (Error(0., e.Message))
                }

            let checkOracle (result: StepResult) =
                task {
                    match oracle with
                    | None -> return result
                    | Some ask ->
                        try
                            let texts = [ for p in opened -> p, client.Text p ] |> Map.ofList
                            let! expected = ask texts

                            let actual =
                                [ for KeyValue(file, list) in latest do
                                      if opened.Contains file then
                                          yield! list ]

                            let updated =
                                { result with
                                    OracleDiff = Some(diffDiagnostics expected actual) }

                            results[results.Count - 1] <- updated
                            return updated
                        with e ->
                            let updated =
                                { result with
                                    Error = Some(defaultArg result.Error $"oracle failed: %s{e.Message}") }

                            results[results.Count - 1] <- updated
                            return updated
                }

            let openFiles name (relatives: string list) =
                let paths = relatives |> List.map fullPath

                awaitPublish name paths None (fun () ->
                    task {
                        for path in paths do
                            do! client.DidOpen(path, File.ReadAllText path)
                            versions[path] <- 0
                            opened.Add path |> ignore
                    })

            let request name method (file: string) (position: Position) (extra: (string * JsonNode) list) =
                task {
                    let path = fullPath file

                    let parameters =
                        JsonObject(
                            [ KeyValuePair("textDocument", JsonObject([ KeyValuePair("uri", JsonValue.Create(Uri(path).AbsoluteUri) :> JsonNode) ]) :> JsonNode)
                              KeyValuePair(
                                  "position",
                                  JsonObject(
                                      [ KeyValuePair("line", JsonValue.Create position.Line :> JsonNode)
                                        KeyValuePair("character", JsonValue.Create position.Character :> JsonNode) ]
                                  )
                                  :> JsonNode
                              ) ]
                            @ [ for k, v in extra -> KeyValuePair(k, v) ]
                        )

                    use watch = new Watch(client, [], None, options.Quiet, options.StepTimeout)
                    watch.Start()

                    try
                        let! _, elapsed = client.Request(method, parameters, options.StepTimeout)
                        return record name elapsed.TotalMilliseconds (Some watch) (Ok elapsed.TotalMilliseconds)
                    with e ->
                        return record name 0. (Some watch) (Error(0., $"%s{method}: %s{e.Message}"))
                }

            let initialized =
                task {
                    let rootUri = Uri(LspClient.NormalizePath repo + string Path.DirectorySeparatorChar).AbsoluteUri
                    let! init = client.Initialize(rootUri, initOptions)
                    return init.TotalMilliseconds
                }

            let! initMs = initialized
            let mutable coldMs = initMs

            // the cold number is initialize plus the first open, whichever step opens it
            let first = scenario.Steps |> List.tryHead

            match first with
            | Some(Open _)
            | Some(OpenMany _) -> ()
            | _ ->
                match first |> Option.bind firstFile with
                | Some file ->
                    let! cold = openFiles "cold open" [ file ]
                    coldMs <- coldMs + cold.LatencyMs
                    results.Clear()
                | None -> ()

            for step in scenario.Steps do
                match step with
                | Open file ->
                    let isFirst = results.Count = 0
                    let! r = openFiles $"open %s{file}" [ file ]

                    if isFirst then
                        coldMs <- coldMs + r.LatencyMs
                | OpenMany files ->
                    let isFirst = results.Count = 0
                    let! r = openFiles $"openMany %d{files.Length}" files

                    if isFirst then
                        coldMs <- coldMs + r.LatencyMs
                | Save file ->
                    let path = fullPath file
                    let! _ = awaitPublish $"save %s{file}" [] None (fun () -> client.DidSave path)
                    ()
                | Edit(file, edit, repeat, mode) ->
                    let path = fullPath file
                    let mutable undo = None

                    for i in 0 .. repeat - 1 do
                        let forward =
                            match mode, undo with
                            | Toggle, Some(undoEdit: Edit) ->
                                undo <- None
                                undoEdit
                            | Toggle, None ->
                                undo <- Some(inverse (client.Text path) edit)
                                edit
                            | Apply, _ -> edit

                        let version = versions[path] + 1
                        versions[path] <- version

                        let name =
                            $"edit %s{file} %s{describe edit.Range.Start}-%s{describe edit.Range.End} #%d{i + 1}"

                        let! r = awaitPublish name [ path ] (Some version) (fun () -> client.DidChange(path, version, forward))
                        let! _ = checkOracle r
                        ()
                | Hover(file, p) -> let! _ = request $"hover %s{file} %s{describe p}" "textDocument/hover" file p [] in ()
                | Completion(file, p) ->
                    let! _ = request $"completion %s{file} %s{describe p}" "textDocument/completion" file p []
                    ()
                | Definition(file, p) ->
                    let! _ = request $"definition %s{file} %s{describe p}" "textDocument/definition" file p []
                    ()
                | References(file, p) ->
                    let context = JsonObject([ KeyValuePair("includeDeclaration", JsonValue.Create true :> JsonNode) ])

                    let! _ =
                        request $"references %s{file} %s{describe p}" "textDocument/references" file p [ "context", context :> JsonNode ]

                    ()

            let snapshot = collector |> Option.map _.Stop()

            return
                { Server = serverName
                  Scenario = scenario.Name
                  Commit = commitOf repo
                  StartedAt = startedAt
                  ColdMs = coldMs
                  Steps = List.ofSeq results
                  PeakWorkingSetMb = snapshot |> Option.map _.PeakWorkingSetMb |> Option.defaultValue 0.
                  AllocatedMb = snapshot |> Option.map _.AllocatedMb |> Option.defaultValue 0.
                  Gen2 = snapshot |> Option.map _.Gen2 |> Option.defaultValue 0 }
        }

    /// Starts the server from its config in the corpus checkout and replays the scenario against it.
    let runWith
        (options: RunnerOptions)
        (server: ServerConfig)
        (scenario: Scenario)
        (corpusRoot: string)
        (oracle: (Map<string, string> -> Task<Diagnostic list>) option)
        : Task<RunResult> =
        task {
            let repo = Scenario.repoRoot corpusRoot scenario
            let! client = LspClient.Start(server.Command, Array.ofList server.Args, server.Env, repo)
            use _ = client :> IDisposable
            let! result = runConnected options client (Some client.ProcessId) server.Name server.InitOptions scenario corpusRoot oracle

            try
                do! client.Shutdown()
            with _ ->
                ()

            return result
        }

    let run server scenario corpusRoot oracle =
        runWith defaultOptions server scenario corpusRoot oracle
