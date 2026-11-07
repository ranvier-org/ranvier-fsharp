namespace Rfs.Checkpoint

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Rfs.Harness
open Rfs.Projects

module Checkpoint =
    let private usage =
        """usage:
  rfs-checkpoint --scenario <glob> --out <dir> [--corpus <dir>] [--step-timeout-s <n>] [--child-timeout-min <n>]
                 [--log-cutoff] [--in-process]
  rfs-checkpoint --engine <name> --scenario <glob> --out <dir> [--corpus <dir>] [--step-timeout-s <n>] [--log-cutoff]

Run it from the ranvier-fsharp repo root (R9). By default each engine runs in a child process of its own (--engine), so
cold time, peak working set and allocations are its alone; a child that runs past --child-timeout-min (default 60) is
killed. --in-process runs every engine in this process instead. --engine skips the corpus check, which the parent has
already run. --log-cutoff sets FICKL_FCS_GRAPH_LOG for file-slim and writes its reused/kept/replaced counts per edit
to <server>.<scenario>.cutoff.tsv; the logging runs inside the timed region, so it is off by default.

Beside each <server>.<scenario>.json RunResult, extra/<server>.<scenario>.json holds what RunResult has no field for:
cold and edit allocations, the time from the end of the cold check to every edited file being promoted (promoteMs),
decl's counters after the cold check (with its D1 premise breaks and segment rows), the
largest declaration of the edited files and the one the first edit falls in (in lines), and per edit the
CheckDeclaration runs (superseded flights included), adopted checks, FinishFile time, its share of the latency and its
time per part, rows inside R11/R12 segments, CheckFile runs (fileLevelChecks), setups from dependencies refused for a
stale snapshot and checked from every earlier file instead (chainFallbacks), and file-slim's graph-check counts when
logged. It sits in a subdirectory so `rfs-harness compare` reads only results."""

    let private header (logCutoff: bool) =
        $"""rfs-checkpoint: the same edits and texts on each engine, one engine after the other.
  decl: ProjectGraph.SetText, then AllDiagnostics; checks each file whole (file-level), set up from the files it
    depends on, after promoting the files the scenario edits (as didOpen would) and waiting until each is checked by
    declaration, outside the timed edits (promoteMs).
  file-slim: the fork's InteractiveChecker.ParseAndCheckProject in the project's own type-checking mode (graph unless
    the project sets --parallelcompilation-). The fork's signature cutoff exists only in graph mode, so this is its best
    file-level configuration: the baseline decl must beat.
  oracle: from-scratch check, sequential (R17), Strict: any mismatch fails the step.
  cutoff log (FICKL_FCS_GRAPH_LOG, inside file-slim's timed region): %s{if logCutoff then "ON" else "off"}
  decl decls/edit counts adopted declaration checks (none for an edit that leaves only file-level files to check);
    extra/<server>.<scenario>.json also has its file checks, chain fallbacks, every run, finish time and share, and
    segment rows.
Layer caveat: both engines check the whole project on every edit, in their own process. FSAC's baseline also pays for
LSP and publishing and re-checks only open files, so these latencies are not directly comparable with it."""

    exception private Usage of string

    let private usageError message = raise (Usage message)

    let private switches = set [ "--in-process"; "--log-cutoff" ]

    let private valued =
        set [ "--scenario"; "--out"; "--corpus"; "--step-timeout-s"; "--child-timeout-min"; "--engine" ]

    /// Every flag but a switch takes exactly one value.
    let private parse (argv: string list) : Map<string, string> =
        let add flag value flags =
            if Map.containsKey flag flags then
                usageError $"%s{flag} given twice"

            Map.add flag value flags

        let rec go flags rest =
            match rest with
            | [] -> flags
            | (flag: string) :: tail when switches.Contains flag -> go (add flag "" flags) tail
            | (flag: string) :: value :: tail when valued.Contains flag && not (value.StartsWith "--") ->
                go (add flag value flags) tail
            | (flag: string) :: _ when valued.Contains flag -> usageError $"%s{flag} takes a value"
            | token :: _ -> usageError $"unexpected '%s{token}'"

        go Map.empty argv

    let private fmt (value: float option) =
        match value with
        | Some v -> v.ToString("F1", CultureInfo.InvariantCulture)
        | None -> "-"

    let private graphLogVariable = "FICKL_FCS_GRAPH_LOG"

    type private Settings =
        { Corpus: string
          Out: string
          Timeout: TimeSpan
          ChildTimeout: TimeSpan
          LogCutoff: bool
          /// Each scenario with the file it came from and its planned edits.
          Scenarios: (string * Scenario * PlannedStep list) list }

    let private projectOf (corpus: string) (s: Scenario) =
        Path.GetFullPath(Scenario.resolve corpus s s.Project)

    let private withOracle (f: OracleClient -> Task<'T>) : Task<'T> =
        task {
            let! oracle = OracleClient.start ()

            try
                return! f oracle
            finally
                (oracle :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()
        }

    let private writeCutoffLog (out: string) (run: EngineRun) =
        let path = Path.Combine(out, Path.ChangeExtension(Report.fileName run.Result, ".cutoff.tsv"))

        let lines =
            [ yield "step\tname\treused\tkept\treplaced"
              for e in run.StepExtras do
                  match e.GraphCheck with
                  | Some c -> yield $"%d{e.Index}\t%s{e.Name}\t%d{c.Reused}\t%d{c.Kept}\t%d{c.Replaced}"
                  | None -> () ]

        File.WriteAllLines(path, lines)

    /// Writes extra/<server>.<scenario>.json (see the usage text).
    let private writeExtra (out: string) (run: EngineRun) =
        let opt (value: 'T option) : JsonNode =
            match value with
            | Some v -> JsonValue.Create v
            | None -> null

        let phasesMs (phases: Map<string, float>) : JsonNode =
            let o = JsonObject()

            for KeyValue(phase, ms) in phases do
                o[phase] <- JsonValue.Create ms

            o

        let counters (c: EngineCounters option) : JsonNode =
            match c with
            | None -> null
            | Some c ->
                let o = JsonObject()
                o["checked"] <- JsonValue.Create c.Checked
                o["adopted"] <- JsonValue.Create c.Adopted
                o["finishMs"] <- JsonValue.Create(float c.FinishTicks * 1000. / float Stopwatch.Frequency)
                o["segmentRows"] <- JsonValue.Create c.SegmentRows
                o["premiseBreaks"] <- JsonValue.Create c.PremiseBreaks
                o["finishPhaseMs"] <- phasesMs (c.FinishPhaseTicks |> Map.map (fun _ t -> float t * 1000. / float Stopwatch.Frequency))
                o["phaseAllocatedMb"] <- phasesMs (c.PhaseAllocated |> Map.map (fun _ b -> float b / 1048576.))
                o["fileLevelChecks"] <- JsonValue.Create c.FileLevelChecks
                o["chainFallbacks"] <- JsonValue.Create c.ChainFallbacks
                o

        let step (e: StepExtra) : JsonNode =
            let o = JsonObject()
            o["index"] <- JsonValue.Create e.Index
            o["step"] <- JsonValue.Create e.Name
            o["latencyMs"] <- JsonValue.Create e.LatencyMs
            o["checked"] <- opt e.Checked
            o["adopted"] <- opt e.Adopted
            o["finishMs"] <- opt e.FinishMs
            o["finishShare"] <- opt e.FinishShare
            o["segmentRows"] <- opt e.SegmentRows
            o["finishPhaseMs"] <- e.FinishPhaseMs |> Option.map phasesMs |> Option.toObj
            o["fileLevelChecks"] <- opt e.FileLevelChecks
            o["chainFallbacks"] <- opt e.ChainFallbacks

            o["graphCheck"] <-
                match e.GraphCheck with
                | None -> null
                | Some c ->
                    let g = JsonObject()
                    g["reused"] <- JsonValue.Create c.Reused
                    g["kept"] <- JsonValue.Create c.Kept
                    g["replaced"] <- JsonValue.Create c.Replaced
                    g

            o

        let o = JsonObject()
        o["server"] <- JsonValue.Create run.Result.Server
        o["scenario"] <- JsonValue.Create run.Result.Scenario
        o["coldAllocatedMb"] <- JsonValue.Create run.ColdAllocatedMb
        o["editAllocatedMb"] <- JsonValue.Create run.EditAllocatedMb
        o["promoteMs"] <- opt run.PromoteMs
        o["coldCounters"] <- counters run.ColdCounters
        o["premiseBreaksAtCold"] <- opt run.PremiseBreaksAtCold
        o["segmentRowsAtCold"] <- opt run.SegmentRowsAtCold
        o["largestRowLines"] <- opt run.LargestRowLines
        o["editedRowLines"] <- opt run.EditedRowLines
        o["finishShareP50"] <- opt (Driver.finishShareP50 run)
        o["stopped"] <- opt run.Stopped
        o["steps"] <- JsonArray([| for e in run.StepExtras -> step e |])
        let dir = Path.Combine(out, "extra")
        Directory.CreateDirectory dir |> ignore
        File.WriteAllText(Path.Combine(dir, Report.fileName run.Result), o.ToJsonString(JsonSerializerOptions(WriteIndented = true)))

    /// Runs `engines` here, one after the other per scenario; true when every step of every run succeeded.
    let private runHere (settings: Settings) (engines: EngineFactory list) : Task<bool> =
        withOracle (fun oracle ->
            task {
                Directory.CreateDirectory settings.Out |> ignore
                let mutable ok = true

                for _, scenario, steps in settings.Scenarios do
                    let project = projectOf settings.Corpus scenario
                    let! spec = ProjectSpec.load project

                    for engine in engines do
                        printfn "running %s on %s (process %d)" engine.Name scenario.Name Environment.ProcessId
                        let logs = settings.LogCutoff && engine.Name = Engines.fileSlim.Name

                        if logs then
                            Environment.SetEnvironmentVariable(graphLogVariable, "1")

                        try
                            try
                                let! run =
                                    Driver.runEngine settings.Timeout engine spec scenario settings.Corpus steps (fun overrides ->
                                        oracle.Check project overrides)

                                let result = run.Result
                                File.WriteAllText(Path.Combine(settings.Out, Report.fileName result), Report.resultToJson result)

                                writeExtra settings.Out run

                                if logs then
                                    writeCutoffLog settings.Out run

                                let row = Report.summarise [ result ] |> List.exactlyOne

                                printfn
                                    "  %s: cold %s ms, edit p50 %s ms, p95 %s ms, decls/edit %s, allocated %s MB (cold %s, edits %s), peak %s MB"
                                    engine.Name
                                    (fmt (Some row.ColdMs))
                                    (fmt row.EditP50)
                                    (fmt row.EditP95)
                                    (fmt row.DeclsPerEdit)
                                    (fmt (Some row.AllocatedMb))
                                    (fmt (Some run.ColdAllocatedMb))
                                    (fmt (Some run.EditAllocatedMb))
                                    (fmt (Some row.PeakMb))

                                match run.ColdCounters with
                                | None -> ()
                                | Some cold ->
                                    let mean (f: StepExtra -> int64 option) =
                                        match run.StepExtras |> List.choose f with
                                        | [] -> None
                                        | xs -> Some(xs |> List.averageBy float)

                                    let lastRows =
                                        run.StepExtras |> List.tryLast |> Option.bind _.SegmentRows |> Option.defaultValue cold.SegmentRows

                                    printfn
                                        "  %s: adopted/edit %s, runs/edit %s, finish share p50 %s%%, segment rows %d cold, %d after the last edit"
                                        engine.Name
                                        (fmt (mean _.Adopted))
                                        (fmt (mean _.Checked))
                                        (fmt (Driver.finishShareP50 run |> Option.map ((*) 100.)))
                                        cold.SegmentRows
                                        lastRows

                                    printfn
                                        "  %s: file checks %d cold, %s/edit; chain fallbacks %d cold, %s/edit"
                                        engine.Name
                                        cold.FileLevelChecks
                                        (fmt (mean _.FileLevelChecks))
                                        cold.ChainFallbacks
                                        (fmt (mean _.ChainFallbacks))

                                    printfn "  %s: promoted the edited files in %s ms" engine.Name (fmt run.PromoteMs)

                                let errors = result.Steps |> List.filter (fun s -> s.Error.IsSome)

                                for e in errors do
                                    eprintfn "  step %d (%s): %s" e.Index e.Step e.Error.Value

                                    for d in defaultArg e.OracleDiff [] |> List.truncate 10 do
                                        eprintfn "    %s" d

                                match run.Stopped with
                                | Some why -> eprintfn "  run stopped early: %s" why
                                | None -> ()

                                if not errors.IsEmpty then
                                    ok <- false
                            with e ->
                                eprintfn "  run failed: %s" e.Message
                                ok <- false
                        finally
                            if logs then
                                Environment.SetEnvironmentVariable(graphLogVariable, null)

                return ok
            })

    /// This program's apphost, which sits next to its assembly also when another program (a test) loaded it.
    let private apphost () =
        let dir = Path.GetDirectoryName(typeof<Settings>.Assembly.Location)
        let exe = Path.Combine(dir, if OperatingSystem.IsWindows() then "rfs-checkpoint.exe" else "rfs-checkpoint")

        if File.Exists exe then
            exe
        else
            failwith $"rfs-checkpoint apphost not found in %s{dir}"

    /// Runs one engine on one scenario in a child process in `repo` (R9), with output going to this console. Returns
    /// the child's exit code; a child past the timeout is killed with its tree and counts as exit 1.
    let private runChild (settings: Settings) (repo: string) (scenarioFile: string) (engine: EngineFactory) : Task<int> =
        task {
            let psi = ProcessStartInfo(apphost ())
            psi.UseShellExecute <- false
            psi.WorkingDirectory <- repo

            if settings.LogCutoff && engine.Name = Engines.fileSlim.Name then
                psi.Environment[graphLogVariable] <- "1"

            for arg in
                [ "--engine"
                  engine.Name
                  "--scenario"
                  Path.GetFullPath scenarioFile
                  "--corpus"
                  settings.Corpus
                  "--out"
                  Path.GetFullPath settings.Out
                  "--step-timeout-s"
                  settings.Timeout.TotalSeconds.ToString(CultureInfo.InvariantCulture) ] do
                psi.ArgumentList.Add arg

            if settings.LogCutoff then
                psi.ArgumentList.Add "--log-cutoff"

            use child = Process.Start psi
            use cts = new CancellationTokenSource(settings.ChildTimeout)

            try
                do! child.WaitForExitAsync cts.Token
                return child.ExitCode
            with :? OperationCanceledException ->
                try
                    child.Kill true
                with _ ->
                    ()

                child.WaitForExit()
                eprintfn "%s child timed out after %.0f min and was killed" engine.Name settings.ChildTimeout.TotalMinutes
                return 1
        }

    let private modeOf (spec: ProjectSpec) =
        match spec.OtherOptions |> List.filter (fun o -> o.StartsWith "--parallelcompilation") |> List.tryLast with
        | Some "--parallelcompilation-" -> "sequential"
        | _ -> "graph"

    let private positive (args: Map<string, string>) (flag: string) =
        Map.tryFind flag args
        |> Option.map (fun text ->
            match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
            | true, v when v > 0. -> v
            | _ -> usageError $"%s{flag} must be a positive number, got '%s{text}'")

    /// Exit codes: 0 success, 1 a step, run or child failed (or timed out), 2 bad usage or not run from the repo root,
    /// 3 a scenario's project does not check clean.
    let execute (engines: EngineFactory list) (argv: string[]) : Task<int> =
        task {
            try
                // R9: the corpus clones carry global.json files that break project loading from anywhere else.
                let repo = Environment.CurrentDirectory

                if not (File.Exists(Path.Combine(repo, "RanvierFSharp.slnx"))) then
                    usageError $"run rfs-checkpoint from the ranvier-fsharp repo root (no RanvierFSharp.slnx in %s{repo})"

                let args = parse (List.ofArray argv)

                let required flag =
                    match Map.tryFind flag args with
                    | Some v -> v
                    | None -> usageError $"missing %s{flag}"

                let pattern = required "--scenario"
                let corpus = Map.tryFind "--corpus" args |> Option.defaultValue "../ranvier-fsharp-corpus" |> Path.GetFullPath
                let files = Cli.expandGlob pattern

                if files.IsEmpty then
                    usageError $"no scenario matches '%s{pattern}'"

                let settings =
                    { Corpus = corpus
                      Out = required "--out"
                      Timeout = positive args "--step-timeout-s" |> Option.map TimeSpan.FromSeconds |> Option.defaultValue (TimeSpan.FromMinutes 5.)
                      ChildTimeout =
                        positive args "--child-timeout-min"
                        |> Option.map TimeSpan.FromMinutes
                        |> Option.defaultValue (TimeSpan.FromMinutes 60.)
                      LogCutoff = args.ContainsKey "--log-cutoff"
                      Scenarios =
                        [ for file in files ->
                              match Scenario.load file corpus with
                              | Error e -> usageError e
                              | Ok s ->
                                  match Driver.plan corpus s with
                                  | Error e -> usageError e
                                  | Ok steps -> file, s, steps ] }

                match Map.tryFind "--engine" args with
                | Some name ->
                    if args.ContainsKey "--in-process" || args.ContainsKey "--child-timeout-min" then
                        usageError "--engine is the child's mode: drop --in-process and --child-timeout-min"

                    let engine =
                        match engines |> List.tryFind (fun e -> e.Name = name) with
                        | Some e -> e
                        | None ->
                            let known = String.Join(", ", engines |> List.map _.Name)
                            usageError $"unknown engine '%s{name}' (known: %s{known})"

                    let! ok = runHere settings [ engine ]
                    return if ok then 0 else 1
                | None ->
                    let inProcess = args.ContainsKey "--in-process"
                    printfn "%s" (header settings.LogCutoff)

                    printfn
                        "engine order: %s, %s\n"
                        (String.Join(", ", engines |> List.map _.Name))
                        (if inProcess then "all in this process" else "one child process each")

                    let! clean =
                        withOracle (fun oracle ->
                            task {
                                let mutable clean = true

                                for _, s, _ in settings.Scenarios |> List.distinctBy (fun (_, s, _) -> projectOf corpus s) do
                                    let! ok = CorpusCheck.verify oracle s.Corpus (projectOf corpus s)
                                    clean <- clean && ok

                                return clean
                            })

                    if not clean then
                        eprintfn "refusing to measure: a scenario's project has errors unedited (see above; rfs-harness verify-corpus runs the same check)"
                        return 3
                    elif inProcess then
                        let! ok = runHere settings engines
                        return if ok then 0 else 1
                    else
                        let mutable ok = true

                        for file, scenario, _ in settings.Scenarios do
                            let! spec = ProjectSpec.load (projectOf corpus scenario)
                            printfn "%s: file-slim checks in %s mode" scenario.Name (modeOf spec)

                            for engine in engines do
                                let! code = runChild settings repo file engine

                                if code <> 0 then
                                    eprintfn "%s on %s: child exited with %d" engine.Name scenario.Name code
                                    ok <- false

                        return if ok then 0 else 1
            with Usage message ->
                eprintfn "%s\n%s" message usage
                return 2
        }

module Program =
    [<EntryPoint>]
    let main argv =
        (Checkpoint.execute Engines.all argv).GetAwaiter().GetResult()
