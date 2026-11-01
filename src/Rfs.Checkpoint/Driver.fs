namespace Rfs.Checkpoint

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects

/// One edit as both engines replay it: the text it leaves in its file, and every edited file's text for the oracle.
type PlannedEdit =
    { Name: string
      File: string
      Text: string
      Overrides: Map<string, string> }

type PlannedStep =
    | Planned of PlannedEdit
    | Unrunnable of name: string * reason: string

/// Peak working set by polling, allocations and gen-2 collections from the runtime's counters: both engines run in
/// this process, so an EventPipe session on it (MetricsCollector) would count its own event processing.
[<Sealed>]
type private InProcessMetrics() =
    let mb (bytes: float) = bytes / 1048576.
    let proc = Process.GetCurrentProcess()
    let stop = new CancellationTokenSource()
    let allocatedAtStart = GC.GetTotalAllocatedBytes true
    let gen2AtStart = GC.CollectionCount 2
    let mutable peak = 0L

    let sample () =
        proc.Refresh()
        peak <- max peak proc.WorkingSet64

    let poll =
        Task.Run(fun () ->
            task {
                use timer = new PeriodicTimer(TimeSpan.FromMilliseconds 50.)
                sample ()

                try
                    while! timer.WaitForNextTickAsync stop.Token do
                        sample ()
                with :? OperationCanceledException ->
                    ()
            }
            :> Task)

    member _.Stop() : MetricsSnapshot =
        stop.Cancel()
        poll.Wait()
        sample ()

        { PeakWorkingSetMb = mb (float peak)
          AllocatedMb = mb (float (GC.GetTotalAllocatedBytes true - allocatedAtStart))
          Gen2 = GC.CollectionCount 2 - gen2AtStart }

    interface IDisposable with
        member _.Dispose() =
            stop.Cancel()
            poll.Wait()
            proc.Dispose()
            stop.Dispose()

module Driver =
    let private describe (p: Position) = $"%d{p.Line}:%d{p.Character}"

    /// The edits of `scenario` in order, computed once from the files on disk so both engines get the same texts.
    /// Only open (which makes a file editable) and edit steps take part; anything else is an Error.
    let plan (corpusRoot: string) (scenario: Scenario) : Result<PlannedStep list, string> =
        let fullPath relative = Path.GetFullPath(Scenario.resolve corpusRoot scenario relative)
        let texts = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        let opened = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let mutable overrides = Map.empty

        let textOf (path: string) =
            match texts.TryGetValue path with
            | true, text -> text
            | _ ->
                let text = File.ReadAllText path
                texts[path] <- text
                text

        let unsupported =
            scenario.Steps
            |> List.tryPick (fun step ->
                match step with
                | Open _
                | Edit _ -> None
                | other -> Some(sprintf "%A" other))

        match unsupported with
        | Some step -> Error $"%s{scenario.Name}: the checkpoint replays only open and edit steps, not %s{step}"
        | None ->
            Ok
                [ for step in scenario.Steps do
                      match step with
                      | Open file -> opened.Add(fullPath file) |> ignore
                      | Edit(file, edit, repeat, mode) ->
                          let path = fullPath file

                          if not (opened.Contains path) then
                              Unrunnable($"edit %s{file}", $"%s{file} was never opened")
                          else
                              let mutable undo = None

                              for i in 0 .. repeat - 1 do
                                  let text = textOf path

                                  let forward, direction =
                                      match mode, undo with
                                      | Toggle, Some(undoEdit: Edit) ->
                                          undo <- None
                                          undoEdit, " inv"
                                      | Toggle, None ->
                                          undo <- Some(Runner.inverse text edit)
                                          edit, " fwd"
                                      | Apply, _ -> edit, ""

                                  let next = Document.apply text forward
                                  texts[path] <- next
                                  overrides <- Map.add path next overrides

                                  Planned
                                      { Name =
                                          $"edit %s{file} %s{describe edit.Range.Start}-%s{describe edit.Range.End} #%d{i + 1}%s{direction}"
                                        File = path
                                        Text = next
                                        Overrides = overrides }
                      | _ -> () ]

    let private fullGc () =
        GC.Collect(2, GCCollectionMode.Forced, true, true)
        GC.WaitForPendingFinalizers()
        GC.Collect(2, GCCollectionMode.Forced, true, true)

    let private within (timeout: TimeSpan) (work: Task<'T>) : Task<'T> =
        task {
            let! winner = Task.WhenAny(work, Task.Delay timeout)

            if not (obj.ReferenceEquals(winner, work)) then
                raise (TimeoutException $"no result within %.0f{timeout.TotalSeconds} s")

            return! work
        }

    /// Creates the engine, runs the first full check (ColdMs, excluded from the edit latencies), then replays `steps`,
    /// comparing the whole project's diagnostics with the oracle after every edit. A mismatch is a step Error.
    let runEngine
        (timeout: TimeSpan)
        (factory: EngineFactory)
        (spec: ProjectSpec)
        (scenario: Scenario)
        (corpusRoot: string)
        (steps: PlannedStep list)
        (oracle: Map<string, string> -> Task<Diagnostic list>)
        : Task<RunResult> =
        task {
            fullGc ()
            use metrics = new InProcessMetrics()
            let startedAt = DateTime.UtcNow
            let results = List<StepResult>()

            let record name latency decls diff error =
                results.Add
                    { Index = results.Count
                      Step = name
                      LatencyMs = latency
                      Publishes = 0
                      DeclsChecked = decls
                      OracleDiff = diff
                      Error = error }

            let coldClock = Stopwatch.StartNew()
            let! engine = factory.Create spec |> within timeout
            use _ = engine
            let! _ = engine.Check() |> within timeout
            let coldMs = coldClock.Elapsed.TotalMilliseconds

            for step in steps do
                match step with
                | Unrunnable(name, reason) -> record name 0. None None (Some reason)
                | Planned edit ->
                    let clock = Stopwatch.StartNew()

                    let! outcome =
                        task {
                            try
                                let! r = engine.SetText(edit.File, edit.Text) |> within timeout
                                return Ok r
                            with e ->
                                return Error e.Message
                        }

                    let latency = clock.Elapsed.TotalMilliseconds

                    match outcome with
                    | Error why -> record edit.Name latency None None (Some why)
                    | Ok r ->
                        try
                            let! expected = oracle edit.Overrides
                            let diff = Diagnostics.compare expected r.Diagnostics

                            let error =
                                if diff.IsEmpty then None else Some $"oracle mismatch: %d{diff.Length} diffs"

                            record edit.Name latency r.DeclsChecked (Some diff) error
                        with e ->
                            record edit.Name latency r.DeclsChecked None (Some $"oracle failed: %s{e.Message}")

            let snapshot = metrics.Stop()

            return
                { Server = factory.Name
                  Scenario = scenario.Name
                  Commit = Runner.commitOf (Scenario.repoRoot corpusRoot scenario)
                  StartedAt = startedAt
                  ColdMs = coldMs
                  Steps = List.ofSeq results
                  PeakWorkingSetMb = snapshot.PeakWorkingSetMb
                  AllocatedMb = snapshot.AllocatedMb
                  Gen2 = snapshot.Gen2 }
        }
