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
      /// What a request answered: a count and a hash, to compare two runs
      Answer: string option
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

    let private hashOf (text: string) =
        (Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(System.Text.Encoding.UTF8.GetBytes text))).Substring(0, 12)

    /// An answer's count and a hash of it, to compare two runs: for a list of locations, how many and their hash whatever
    /// order they come in; for a completion list, its items in order; for anything else (a hover, one location), 1.
    let private answerOf (answer: System.Text.Json.Nodes.JsonNode) =
        match answer with
        | :? System.Text.Json.Nodes.JsonArray as locations ->
            let sorted = [ for l in locations -> l.ToJsonString() ] |> List.sort |> String.concat "|"
            $"%d{locations.Count} %s{hashOf sorted}"
        | :? System.Text.Json.Nodes.JsonObject as o when (o["items"] :? System.Text.Json.Nodes.JsonArray) ->
            let items = o["items"].AsArray()
            $"%d{items.Count} %s{hashOf (items.ToJsonString())}"
        | null -> "null"
        | a -> $"1 %s{hashOf (a.ToJsonString())}"

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
    let inverse (text: string) (forward: Edit) : Edit =
        let first = Document.offsetOf text forward.Range.Start
        let last = max first (Document.offsetOf text forward.Range.End)

        { Range =
            { Start = forward.Range.Start
              End = lastOffset forward.Range.Start forward.Text }
          Text = text.Substring(first, last - first) }

    let private describe (p: Position) = $"%d{p.Line}:%d{p.Character}"

    /// HEAD of `repo`, or "unknown" when it is not a git checkout.
    let commitOf (repo: string) =
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
        (server: ServerConfig)
        (scenario: Scenario)
        (corpusRoot: string)
        (oracle: (Map<string, string> -> Task<Diagnostic list>) option)
        : Task<RunResult> =
        task {
            let startedAt = DateTime.UtcNow
            let mode = server.Oracle
            let repo = Scenario.repoRoot corpusRoot scenario
            let fullPath relative = LspClient.NormalizePath(Scenario.resolve corpusRoot scenario relative)
            let collector = pid |> Option.bind MetricsCollector.TryStart
            let versions = Dictionary<string, int>()
            let opened = HashSet<string>()
            // every publish per file in arrival order, so a step can pick the one for the version it sent
            let history = ConcurrentDictionary<string, List<int option * Diagnostic list>>()

            use historySubscription =
                subscribe client.Diagnostics (fun (_, file, version, list) ->
                    let entries = history.GetOrAdd(file, fun _ -> List())
                    lock entries (fun () -> entries.Add((version, list))))

            /// The newest publish for a file: a late publish of an older version never wins over a newer one.
            let current (file: string) =
                match history.TryGetValue file with
                | true, entries ->
                    lock entries (fun () ->
                        let highest = entries |> Seq.choose fst |> Seq.fold max (-1)

                        entries
                        |> Seq.tryFindBack (fun (v, _) -> v.IsNone || v = Some highest)
                        |> Option.map snd)
                    |> Option.defaultValue []
                | _ -> []

            /// The publish answering `version` of `file`; servers that omit versions are answered by their newest.
            let answering (file: string) (version: int) =
                match history.TryGetValue file with
                | true, entries ->
                    lock entries (fun () ->
                        match entries |> Seq.tryFindBack (fun (v, _) -> v = Some version) with
                        | Some(_, list) -> Some list
                        | None -> entries |> Seq.tryFindBack (fun (v, _) -> v.IsNone) |> Option.map snd)
                | _ -> None
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
                      Answer = None
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

            let checkOracle (result: StepResult) (edited: string) (version: int) =
                task {
                    match oracle with
                    | None -> return result
                    | _ when result.Error.IsSome -> return result
                    | Some ask ->
                        try
                            let texts = [ for p in opened -> p, client.Text p ] |> Map.ofList
                            let! whole = ask texts
                            // servers publish only for open documents, the oracle checks the whole project
                            let expected = whole |> List.filter (fun d -> opened.Contains d.File)
                            let answer = answering edited version

                            let actual =
                                [ for file in opened do
                                      if file = edited then
                                          yield! defaultArg answer []
                                      else
                                          yield! current file ]

                            let missing =
                                if answer.IsNone then
                                    [ $"no publish for version %d{version} of %s{edited}" ]
                                else
                                    []

                            let diff = missing @ Rfs.Lsp.Diagnostics.compare expected actual

                            let updated =
                                { result with
                                    OracleDiff = Some diff
                                    Error =
                                        match mode with
                                        | Strict when not diff.IsEmpty -> Some $"oracle mismatch: %d{diff.Length} diffs"
                                        | _ -> None }

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
                task {
                    let paths = relatives |> List.map fullPath

                    // read before the clock starts: the measurement is the server's, not the harness's disk
                    let texts =
                        try
                            Ok [ for path in paths -> path, File.ReadAllText path ]
                        with e ->
                            Error e.Message

                    match texts with
                    | Error why -> return record name 0. None (Error(0., why))
                    | Ok texts ->
                        return!
                            awaitPublish name paths None (fun () ->
                                task {
                                    for path, text in texts do
                                        do! client.DidOpen(path, text)
                                        versions[path] <- 0
                                        opened.Add path |> ignore
                                })
                }

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
                        let! answer, elapsed = client.Request(method, parameters, options.StepTimeout)
                        let r = record name elapsed.TotalMilliseconds (Some watch) (Ok elapsed.TotalMilliseconds)

                        let answered = { r with Answer = Some(answerOf answer) }
                        results[r.Index] <- answered
                        return answered
                    with e ->
                        return record name 0. (Some watch) (Error(0., $"%s{method}: %s{e.Message}"))
                }

            let initialized =
                task {
                    let rootUri = Uri(LspClient.NormalizePath repo + string Path.DirectorySeparatorChar).AbsoluteUri
                    // a node can have one parent: the config is reused by every run of this server
                    let! init = client.Initialize(rootUri, server.InitOptions.DeepClone())
                    // FSAC reads later configuration from didChangeConfiguration, other servers ask for it
                    do! client.Configure(JsonObject([ KeyValuePair("FSharp", server.InitOptions.DeepClone()) ]))
                    return init.TotalMilliseconds
                }

            let! initMs = initialized
            let mutable coldMs = initMs

            match server.ProjectLoad with
            | None -> ()
            | Some method ->
                let project = fullPath scenario.Project

                let parameters =
                    JsonObject(
                        [ KeyValuePair(
                              "textDocuments",
                              JsonArray(JsonObject([ KeyValuePair("uri", JsonValue.Create(Uri(project).AbsoluteUri) :> JsonNode) ])) :> JsonNode
                          ) ]
                    )

                let name = $"load %s{Path.GetFileName scenario.Project}"

                try
                    let! _, elapsed = client.Request(method, parameters, options.StepTimeout)
                    coldMs <- coldMs + elapsed.TotalMilliseconds
                with e ->
                    record name 0. None (Error(0., $"%s{method}: %s{e.Message}")) |> ignore

            // the cold number is initialize plus the first open, whichever step opens it
            let first = scenario.Steps |> List.tryHead
            // a failed project load is recorded before any step
            let beforeSteps = results.Count

            match first with
            | Some(Open _)
            | Some(OpenMany _) -> ()
            | _ ->
                match first |> Option.bind firstFile with
                | Some file ->
                    let! cold = openFiles "cold open" [ file ]
                    coldMs <- coldMs + cold.LatencyMs
                    results.Remove cold |> ignore
                | None -> ()

            for step in scenario.Steps do
                match step with
                | Open file ->
                    let isFirst = results.Count = beforeSteps
                    let! r = openFiles $"open %s{file}" [ file ]

                    if isFirst then
                        coldMs <- coldMs + r.LatencyMs
                | OpenMany files ->
                    let isFirst = results.Count = beforeSteps
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

                    if not (opened.Contains path) then
                        record $"edit %s{file}" 0. None (Error(0., $"%s{file} was never opened")) |> ignore
                    else
                        for i in 0 .. repeat - 1 do
                            let forward, direction =
                                match mode, undo with
                                | Toggle, Some(undoEdit: Edit) ->
                                    undo <- None
                                    undoEdit, " inv"
                                | Toggle, None ->
                                    undo <- Some(inverse (client.Text path) edit)
                                    edit, " fwd"
                                | Apply, _ -> edit, ""

                            let version = versions[path] + 1
                            versions[path] <- version

                            let name =
                                $"edit %s{file} %s{describe edit.Range.Start}-%s{describe edit.Range.End} #%d{i + 1}%s{direction}"

                            let! r = awaitPublish name [ path ] (Some version) (fun () -> client.DidChange(path, version, forward))
                            let! _ = checkOracle r path version
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

            let snapshot =
                try
                    collector |> Option.map _.Stop()
                with e ->
                    eprintfn "warning: metrics lost: %s" e.Message
                    None

            return
                { Server = server.Name
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
            let! client = LspClient.Start(server.Command, Array.ofList server.Args, server.Env, defaultArg server.Cwd repo)
            use _ = client :> IDisposable
            let! result = runConnected options client (Some client.ProcessId) server scenario corpusRoot oracle

            try
                do! client.Shutdown()
            with _ ->
                ()

            return result
        }

    let run server scenario corpusRoot oracle =
        runWith defaultOptions server scenario corpusRoot oracle
