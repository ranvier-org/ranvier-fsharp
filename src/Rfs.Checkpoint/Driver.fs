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
      /// Where the edit starts, in the text before it.
      Start: Position
      Text: string
      Overrides: Map<string, string> }

type PlannedStep =
    | Planned of PlannedEdit
    | Unrunnable of name: string * reason: string

/// What one edit measured beyond its StepResult; the counter fields are None for an engine without counters.
type StepExtra =
    { Index: int
      Name: string
      LatencyMs: float
      /// CheckDeclaration runs during the edit, superseded flights included.
      Checked: int64 option
      /// CheckDeclaration results the edit published.
      Adopted: int64 option
      /// Time inside FinishFile during the edit.
      FinishMs: float option
      /// Rows inside R11/R12 segments after the edit.
      SegmentRows: int option
      /// Time inside each part of FinishFile during the edit (FileResult.PhaseTicks).
      FinishPhaseMs: Map<string, float> option
      /// CheckFile runs during the edit, superseded flights included.
      FileLevelChecks: int64 option
      /// Setups from dependencies refused during the edit (a stale snapshot), checked from every earlier file instead.
      ChainFallbacks: int64 option
      /// The fork's graph-check counts, when logged.
      GraphCheck: GraphCheckCounts option }

    /// The part of the edit's latency spent finishing files.
    member e.FinishShare =
        e.FinishMs |> Option.filter (fun _ -> e.LatencyMs > 0.) |> Option.map (fun f -> f / e.LatencyMs)

/// What one engine's run measured, beyond its RunResult.
type EngineRun =
    { Result: RunResult
      /// Allocated by creating the engine and its first check.
      ColdAllocatedMb: float
      /// Allocated by the engine's edits, without the oracle comparisons between them.
      EditAllocatedMb: float
      /// The engine's counters after its first check.
      ColdCounters: EngineCounters option
      /// The time from the end of the first check to every edited file's promotion having settled; None for an engine
      /// that does not promote, and when no edited file was checked by declaration.
      PromoteMs: float option
      /// The edited files the engine could not promote (checked whole: one declaration, a failed build, or an
      /// unresolved name in the setup from their dependencies); None for an engine that does not promote.
      NotPromoted: string list option
      /// D1 premise breaks published by the first check.
      PremiseBreaksAtCold: int64 option
      /// Rows inside R11/R12 segments after the first check.
      SegmentRowsAtCold: int option
      /// The largest declaration of the edited files, in lines, as their texts on disk split.
      LargestRowLines: int option
      /// The innermost declaration the first edit falls in, in lines; None also when it falls between declarations.
      EditedRowLines: int option
      /// One per edit that returned a result.
      StepExtras: StepExtra list
      /// Why the run stopped before its last step.
      Stopped: string option }

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
                                        Start = forward.Range.Start
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

    let private mb (bytes: int64) = float bytes / 1048576.

    /// The p50 of the edits' finish shares; None for an engine without counters.
    let finishShareP50 (run: EngineRun) =
        run.StepExtras |> List.choose _.FinishShare |> Report.percentile 50.

    /// Warms the oracle up, creates the engine and runs its first full check (ColdMs, excluded from the edit
    /// latencies), then replays `steps`, comparing the whole project's diagnostics with the oracle after every edit. A
    /// mismatch is a step Error. A timeout ends the run at that step: the engine (or the oracle) is still busy, so
    /// later numbers would be meaningless. Allocations are counted around the engine's own work only; the peak working
    /// set is the process's, so it is the engine's alone when the engine has the process to itself.
    let runEngine
        (timeout: TimeSpan)
        (factory: EngineFactory)
        (spec: ProjectSpec)
        (scenario: Scenario)
        (corpusRoot: string)
        (steps: PlannedStep list)
        (oracle: Map<string, string> -> Task<Diagnostic list>)
        : Task<EngineRun> =
        task {
            // the oracle's own start-up must not overlap the cold check
            let! _ = oracle Map.empty |> within timeout
            fullGc ()
            let gen2AtStart = GC.CollectionCount 2
            let startedAt = DateTime.UtcNow
            let results = List<StepResult>()
            let extras = List<StepExtra>()
            let mutable editAllocated = 0L
            let mutable stopped = None

            let record name latency decls diff error =
                results.Add
                    { Index = results.Count
                      Step = name
                      LatencyMs = latency
                      Publishes = 0
                      DeclsChecked = decls
                      OracleDiff = diff
                      Error = error }

            let coldAllocatedBefore = GC.GetTotalAllocatedBytes true
            let coldClock = Stopwatch.StartNew()
            let! engine = factory.Create spec |> within timeout
            use _ = engine
            let! _ = engine.Check() |> within timeout
            let coldMs = coldClock.Elapsed.TotalMilliseconds
            let coldAllocated = GC.GetTotalAllocatedBytes true - coldAllocatedBefore
            let coldCounters = engine.Counters()
            let toMs (ticks: int64) = float ticks * 1000. / float Stopwatch.Frequency
            let edits = steps |> List.choose (function Planned e -> Some e | Unrunnable _ -> None)
            let linesOf (lines: int * int) = snd lines - fst lines + 1

            let largestRowLines =
                edits
                |> List.map _.File
                |> List.distinct
                |> List.choose (fun file -> engine.DeclarationLines(file, File.ReadAllText file))
                |> List.collect (Array.map linesOf >> List.ofArray)
                |> List.fold (fun largest lines -> Some(max lines (defaultArg largest 0))) None

            let editedRowLines =
                edits
                |> List.tryHead
                |> Option.bind (fun e ->
                    let line = e.Start.Line + 1

                    engine.DeclarationLines(e.File, File.ReadAllText e.File)
                    |> Option.bind (fun rows ->
                        rows
                        |> Array.filter (fun (first, last) -> first <= line && line <= last)
                        |> Array.map linesOf
                        |> Array.sort
                        |> Array.tryHead))

            // As an editor opens the files it edits: outside the timed edits, and after the cold counters
            let promoteClock = Stopwatch.StartNew()
            let! promotion = engine.Promote(edits |> List.map _.File |> List.distinct) |> within timeout
            let promotedMs = promoteClock.Elapsed.TotalMilliseconds

            let promoteMs =
                promotion |> Option.filter (fun p -> not p.Promoted.IsEmpty) |> Option.map (fun _ -> promotedMs)

            for step in steps do
                match step, stopped with
                | _, Some _ -> ()
                | Unrunnable(name, reason), None -> record name 0. None None (Some reason)
                | Planned edit, None ->
                    let countersBefore = engine.Counters()
                    let allocatedBefore = GC.GetTotalAllocatedBytes true
                    let clock = Stopwatch.StartNew()

                    let! outcome =
                        task {
                            try
                                let! r = engine.SetText(edit.File, edit.Text) |> within timeout
                                return Ok r
                            with e ->
                                return Error e
                        }

                    let latency = clock.Elapsed.TotalMilliseconds
                    editAllocated <- editAllocated + (GC.GetTotalAllocatedBytes true - allocatedBefore)

                    match outcome with
                    | Error(:? TimeoutException as e) ->
                        record edit.Name latency None None (Some $"engine timed out: %s{e.Message}; run stopped")
                        stopped <- Some $"%s{edit.Name}: engine timed out"
                    | Error e -> record edit.Name latency None None (Some e.Message)
                    | Ok r ->
                        let countersAfter = engine.Counters()

                        let delta (field: EngineCounters -> int64) =
                            match countersBefore, countersAfter with
                            | Some b, Some a -> Some(field a - field b)
                            | _ -> None

                        extras.Add(
                            { Index = results.Count
                              Name = edit.Name
                              LatencyMs = latency
                              Checked = delta _.Checked
                              Adopted = delta _.Adopted
                              FinishMs = delta _.FinishTicks |> Option.map toMs
                              SegmentRows = countersAfter |> Option.map _.SegmentRows
                              FinishPhaseMs =
                                match countersBefore, countersAfter with
                                | Some b, Some a ->
                                    a.FinishPhaseTicks
                                    |> Map.map (fun phase ticks ->
                                        toMs (ticks - (b.FinishPhaseTicks |> Map.tryFind phase |> Option.defaultValue 0L)))
                                    |> Some
                                | _ -> None
                              FileLevelChecks = delta _.FileLevelChecks
                              ChainFallbacks = delta _.ChainFallbacks
                              GraphCheck = r.GraphCheck }
                            : StepExtra
                        )

                        try
                            let! expected = oracle edit.Overrides |> within timeout
                            let diff = Diagnostics.compare expected r.Diagnostics

                            let error =
                                if diff.IsEmpty then None else Some $"oracle mismatch: %d{diff.Length} diffs"

                            record edit.Name latency r.DeclsChecked (Some diff) error
                        with
                        | :? TimeoutException as e ->
                            record edit.Name latency r.DeclsChecked None (Some $"oracle timed out: %s{e.Message}; run stopped")
                            stopped <- Some $"%s{edit.Name}: oracle timed out"
                        | e -> record edit.Name latency r.DeclsChecked None (Some $"oracle failed: %s{e.Message}")

            use proc = Process.GetCurrentProcess()

            let result =
                { Server = factory.Name
                  Scenario = scenario.Name
                  Commit = Runner.commitOf (Scenario.repoRoot corpusRoot scenario)
                  StartedAt = startedAt
                  ColdMs = coldMs
                  Steps = List.ofSeq results
                  PeakWorkingSetMb = mb proc.PeakWorkingSet64
                  AllocatedMb = mb (coldAllocated + editAllocated)
                  Gen2 = GC.CollectionCount 2 - gen2AtStart }

            return
                { Result = result
                  ColdAllocatedMb = mb coldAllocated
                  EditAllocatedMb = mb editAllocated
                  ColdCounters = coldCounters
                  PromoteMs = promoteMs
                  NotPromoted = promotion |> Option.map _.NotPromoted
                  PremiseBreaksAtCold = coldCounters |> Option.map _.PremiseBreaks
                  SegmentRowsAtCold = coldCounters |> Option.map _.SegmentRows
                  LargestRowLines = largestRowLines
                  EditedRowLines = editedRowLines
                  StepExtras = List.ofSeq extras
                  Stopped = stopped }
        }
