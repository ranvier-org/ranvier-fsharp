namespace Rfs.Checkpoint

open System
open System.Diagnostics
open System.Globalization
open System.IO
open System.Threading.Tasks
open Rfs.Harness
open Rfs.Projects

module Checkpoint =
    let private usage =
        """usage:
  rfs-checkpoint --scenario <glob> --out <dir> [--corpus <dir>] [--step-timeout-s <n>] [--in-process]
  rfs-checkpoint --engine <name> --scenario <glob> --out <dir> [--corpus <dir>] [--step-timeout-s <n>]

By default each engine runs in a child process of its own (--engine), so cold time, peak working set and allocations
are its alone. --in-process runs every engine in this process instead. --engine skips the corpus check, which the
parent has already run."""

    let private header =
        """rfs-checkpoint: the same edits and texts on each engine, one engine after the other.
  decl: ProjectGraph.SetText, then AllDiagnostics; checks per declaration, each file seeing every earlier file.
  file-slim: the fork's InteractiveChecker.ParseAndCheckProject in the project's own type-checking mode (graph unless
    the project sets --parallelcompilation-). The fork's signature cutoff exists only in graph mode, so this is its best
    file-level configuration: the baseline decl must beat.
  oracle: from-scratch check, sequential (R17), Strict: any mismatch fails the step.
Layer caveat: both engines check the whole project on every edit, in their own process. FSAC's baseline also pays for
LSP and publishing and re-checks only open files, so these latencies are not directly comparable with it.
TODO(Task 15): finish-file share of decl latency not recorded: ProjectGraph exposes no finish-time counter yet.
TODO(Task 15): rows in R11/R12 segments not recorded: ProjectGraph exposes no segment row count yet."""

    exception private Usage of string

    let private usageError message = raise (Usage message)

    let private switches = set [ "--in-process" ]

    let private valued =
        set [ "--scenario"; "--out"; "--corpus"; "--step-timeout-s"; "--engine" ]

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

    type private Settings =
        { Corpus: string
          Out: string
          Timeout: TimeSpan
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

                        try
                            let! result =
                                Driver.runEngine settings.Timeout engine spec scenario settings.Corpus steps (fun overrides ->
                                    oracle.Check project overrides)

                            File.WriteAllText(Path.Combine(settings.Out, Report.fileName result), Report.resultToJson result)
                            let row = Report.summarise [ result ] |> List.exactlyOne

                            printfn
                                "  %s: cold %s ms, edit p50 %s ms, p95 %s ms, decls/edit %s, allocated %s MB, peak %s MB"
                                engine.Name
                                (fmt (Some row.ColdMs))
                                (fmt row.EditP50)
                                (fmt row.EditP95)
                                (fmt row.DeclsPerEdit)
                                (fmt (Some row.AllocatedMb))
                                (fmt (Some row.PeakMb))

                            let errors = result.Steps |> List.filter (fun s -> s.Error.IsSome)

                            for e in errors do
                                eprintfn "  step %d (%s): %s" e.Index e.Step e.Error.Value

                                for d in defaultArg e.OracleDiff [] |> List.truncate 10 do
                                    eprintfn "    %s" d

                            if not errors.IsEmpty then
                                ok <- false
                        with e ->
                            eprintfn "  run failed: %s" e.Message
                            ok <- false

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

    /// Runs one engine on one scenario in a child process, in this process's working directory (the repo, R9), with
    /// output going to this console. Returns the child's exit code.
    let private runChild (settings: Settings) (scenarioFile: string) (engine: EngineFactory) : Task<int> =
        task {
            let psi = ProcessStartInfo(apphost ())
            psi.UseShellExecute <- false
            psi.WorkingDirectory <- Environment.CurrentDirectory

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

            use child = Process.Start psi
            do! child.WaitForExitAsync()
            return child.ExitCode
        }

    let private modeOf (spec: ProjectSpec) =
        match spec.OtherOptions |> List.filter (fun o -> o.StartsWith "--parallelcompilation") |> List.tryLast with
        | Some "--parallelcompilation-" -> "sequential"
        | _ -> "graph"

    /// Exit codes: 0 success, 1 a step, run or child failed, 2 bad usage, 3 a scenario's project does not check clean.
    let execute (engines: EngineFactory list) (argv: string[]) : Task<int> =
        task {
            try
                let args = parse (List.ofArray argv)

                let required flag =
                    match Map.tryFind flag args with
                    | Some v -> v
                    | None -> usageError $"missing %s{flag}"

                let pattern = required "--scenario"
                let corpus = Map.tryFind "--corpus" args |> Option.defaultValue "../ranvier-fsharp-corpus" |> Path.GetFullPath

                let timeout =
                    match Map.tryFind "--step-timeout-s" args with
                    | None -> TimeSpan.FromMinutes 5.
                    | Some text ->
                        match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
                        | true, v when v > 0. -> TimeSpan.FromSeconds v
                        | _ -> usageError $"--step-timeout-s must be a positive number, got '%s{text}'"

                let files = Cli.expandGlob pattern

                if files.IsEmpty then
                    usageError $"no scenario matches '%s{pattern}'"

                let settings =
                    { Corpus = corpus
                      Out = required "--out"
                      Timeout = timeout
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
                    if args.ContainsKey "--in-process" then
                        usageError "--engine runs in-process already; drop --in-process"

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
                    printfn "%s" header

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
                                let! code = runChild settings file engine

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
