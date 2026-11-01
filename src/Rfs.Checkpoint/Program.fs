namespace Rfs.Checkpoint

open System
open System.Globalization
open System.IO
open System.Threading.Tasks
open Rfs.Harness
open Rfs.Projects

module Checkpoint =
    let private usage =
        """usage:
  rfs-checkpoint --scenario <glob> --out <dir> [--corpus <dir>] [--step-timeout-s <n>]"""

    let private header =
        """rfs-checkpoint: decl (ProjectGraph.SetText, then AllDiagnostics) against file-slim (the fork's
InteractiveChecker.ParseAndCheckProject with its signature cutoff), the same edits and texts, one engine after the other.
Layer caveat: both engines run in-process and check the whole project on every edit. FSAC's baseline also pays for
LSP and publishing and re-checks only open files, so these latencies are not directly comparable with it.
TODO(Task 15): finish-file share of decl latency not recorded: ProjectGraph exposes no finish-time counter yet.
TODO(Task 15): rows in R11/R12 segments not recorded: ProjectGraph exposes no segment row count yet."""

    exception private Usage of string

    let private usageError message = raise (Usage message)

    /// Every flag takes exactly one value.
    let private parse (argv: string list) : Map<string, string> =
        let rec go flags rest =
            match rest with
            | [] -> flags
            | (flag: string) :: value :: tail when flag.StartsWith "--" && not (value.StartsWith "--") ->
                if Map.containsKey flag flags then
                    usageError $"%s{flag} given twice"

                go (Map.add flag value flags) tail
            | token :: _ -> usageError $"unexpected '%s{token}'"

        go Map.empty argv

    let private fmt (value: float option) =
        match value with
        | Some v -> v.ToString("F1", CultureInfo.InvariantCulture)
        | None -> "-"

    /// Exit codes: 0 success, 1 a step or run failed, 2 bad usage, 3 a scenario's project does not check clean.
    let execute (engines: EngineFactory list) (argv: string[]) : Task<int> =
        task {
            try
                let args = parse (List.ofArray argv)

                for flag in Map.keys args do
                    if not (List.contains flag [ "--scenario"; "--out"; "--corpus"; "--step-timeout-s" ]) then
                        usageError $"unknown flag %s{flag}"

                let required flag =
                    match Map.tryFind flag args with
                    | Some v -> v
                    | None -> usageError $"missing %s{flag}"

                let pattern = required "--scenario"
                let out = required "--out"
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

                let scenarios =
                    [ for file in files ->
                          match Scenario.load file corpus with
                          | Error e -> usageError e
                          | Ok s ->
                              match Driver.plan corpus s with
                              | Error e -> usageError e
                              | Ok steps -> s, steps ]

                printfn "%s\n" header
                let projectOf (s: Scenario) = Path.GetFullPath(Scenario.resolve corpus s s.Project)
                let oracle = OracleClient.start().GetAwaiter().GetResult()

                try
                    let mutable clean = true

                    for s in scenarios |> List.map fst |> List.distinctBy projectOf do
                        let! ok = CorpusCheck.verify oracle s.Corpus (projectOf s)
                        clean <- clean && ok

                    if not clean then
                        eprintfn "refusing to measure: a scenario's project has errors unedited (see above; rfs-harness verify-corpus runs the same check)"
                        return 3
                    else
                        Directory.CreateDirectory out |> ignore
                        let mutable failed = false

                        for scenario, steps in scenarios do
                            let project = projectOf scenario
                            let! spec = ProjectSpec.load project

                            for engine in engines do
                                printfn "running %s on %s" engine.Name scenario.Name

                                try
                                    let! result =
                                        Driver.runEngine timeout engine spec scenario corpus steps (fun overrides -> oracle.Check project overrides)

                                    File.WriteAllText(Path.Combine(out, Report.fileName result), Report.resultToJson result)
                                    let row = Report.summarise [ result ] |> List.exactlyOne

                                    printfn
                                        "  cold %s ms, edit p50 %s ms, p95 %s ms, decls/edit %s, allocated %s MB, peak %s MB"
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
                                        failed <- true
                                with e ->
                                    eprintfn "  run failed: %s" e.Message
                                    failed <- true

                        return if failed then 1 else 0
                finally
                    (oracle :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()
            with Usage message ->
                eprintfn "%s\n%s" message usage
                return 2
        }

module Program =
    [<EntryPoint>]
    let main argv =
        (Checkpoint.execute Engines.all argv).GetAwaiter().GetResult()
