namespace Rfs.Harness

open System
open System.IO
open System.Threading.Tasks

module Cli =
    let private usage =
        """usage:
  rfs-harness run --scenario <glob> --out <dir> [--servers servers.json] [--server <name>...]
                  [--corpus <dir>] [--quiet-ms <n>] [--step-timeout-s <n>]
  rfs-harness compare <dirA> <dirB> [--out compare.md]
  rfs-harness verify-corpus [--manifest corpus/corpus.json] [--corpus <dir>]"""

    type private Args =
        { Positional: string list
          Flags: Map<string, string list> }

    /// `--flag v1 v2 ...` takes every value up to the next `--flag`; tokens before the first flag are positional.
    let private parse (argv: string list) : Args =
        let rec go positional flags current rest =
            match rest with
            | [] -> { Positional = List.rev positional; Flags = flags }
            | (token: string) :: tail when token.StartsWith "--" ->
                go positional (Map.add token [] flags) (Some token) tail
            | token :: tail ->
                match current with
                | Some flag -> go positional (Map.add flag (Map.find flag flags @ [ token ]) flags) current tail
                | None -> go (token :: positional) flags current tail

        go [] Map.empty None argv

    exception private Usage of string

    let private usageError message = raise (Usage message)

    let private single (args: Args) (flag: string) =
        match Map.tryFind flag args.Flags with
        | None -> None
        | Some [ value ] -> Some value
        | Some _ -> usageError $"%s{flag} takes exactly one value"

    let private required (args: Args) (flag: string) =
        match single args flag with
        | Some value -> value
        | None -> usageError $"missing %s{flag}"

    let private number (args: Args) (flag: string) =
        single args flag
        |> Option.map (fun text ->
            match Double.TryParse(text, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v when v > 0. -> v
            | _ -> usageError $"%s{flag} must be a positive number, got '%s{text}'")

    /// `*` and `?` in the file name only; the directory part is literal.
    let expandGlob (pattern: string) =
        let dir =
            match Path.GetDirectoryName pattern with
            | null
            | "" -> "."
            | d -> d

        if not (Directory.Exists dir) then
            []
        else
            Directory.GetFiles(dir, Path.GetFileName pattern) |> Array.sort |> List.ofArray

    let private runCommand (args: Args) : Task<int> =
        task {
            let serversPath = single args "--servers" |> Option.defaultValue "servers.json"
            let corpus = single args "--corpus" |> Option.defaultValue "../ranvier-fsharp-corpus" |> Path.GetFullPath
            let pattern = required args "--scenario"
            let out = required args "--out"

            let options =
                { Runner.defaultOptions with
                    Quiet =
                        number args "--quiet-ms"
                        |> Option.map TimeSpan.FromMilliseconds
                        |> Option.defaultValue Runner.defaultOptions.Quiet
                    StepTimeout =
                        number args "--step-timeout-s"
                        |> Option.map TimeSpan.FromSeconds
                        |> Option.defaultValue Runner.defaultOptions.StepTimeout }

            let servers =
                match Servers.load serversPath with
                | Error e -> usageError e
                | Ok all ->
                    match Map.tryFind "--server" args.Flags with
                    | None
                    | Some [] -> all
                    | Some names ->
                        [ for name in names ->
                              match all |> List.tryFind (fun s -> s.Name = name) with
                              | Some s -> s
                              | None ->
                                  let known = String.Join(", ", all |> List.map _.Name)
                                  usageError $"unknown server '%s{name}' (known: %s{known})" ]

            let files = expandGlob pattern

            if files.IsEmpty then
                usageError $"no scenario matches '%s{pattern}'"

            let scenarios =
                [ for file in files ->
                      match Scenario.load file corpus with
                      | Ok s -> s
                      | Error e -> usageError e ]

            Directory.CreateDirectory out |> ignore
            let mutable failed = false
            let oracle = OracleClient.start().GetAwaiter().GetResult()

            try
                for scenario in scenarios do
                    for server in servers do
                        printfn "running %s on %s" server.Name scenario.Name
                        let project = Path.GetFullPath(Scenario.resolve corpus scenario scenario.Project)

                        try
                            let! result = Runner.runWith options server scenario corpus (Some(fun overrides -> oracle.Check project overrides))
                            let path = Path.Combine(out, Report.fileName result)
                            File.WriteAllText(path, Report.resultToJson result)
                            let errors = result.Steps |> List.filter (fun s -> s.Error.IsSome)

                            for e in errors do
                                eprintfn "  step %d (%s): %s" e.Index e.Step e.Error.Value

                            if not errors.IsEmpty then
                                failed <- true
                        with e ->
                            eprintfn "  run failed: %s" e.Message
                            failed <- true
            finally
                (oracle :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

            return if failed then 1 else 0
        }

    let private readDir (dir: string) : RunResult list =
        if not (Directory.Exists dir) then
            usageError $"not a directory: %s{dir}"

        let results =
            [ for file in Directory.GetFiles(dir, "*.json") |> Array.sort ->
                  try
                      Report.resultOfJson (File.ReadAllText file)
                  with e ->
                      usageError $"%s{file}: not a result file (%s{e.Message})" ]

        if results.IsEmpty then
            usageError $"no result files in %s{dir}"

        results

    let private compareCommand (args: Args) : Task<int> =
        task {
            match args.Positional with
            | [ dirA; dirB ] ->
                let label (dir: string) = Path.GetFileName(Path.TrimEndingDirectorySeparator dir)
                let a = label dirA, Report.summarise (readDir dirA)
                let b = label dirB, Report.summarise (readDir dirB)
                let markdown = Report.renderComparison a b

                match single args "--out" with
                | Some path -> File.WriteAllText(path, markdown)
                | None -> printf "%s" markdown

                return 0
            | _ -> return usageError "compare takes exactly two result directories"
        }

    /// Checks each corpus project unedited with the oracle (CorpusCheck.verify).
    let private verifyCorpusCommand (args: Args) : Task<int> =
        task {
            let manifest = single args "--manifest" |> Option.defaultValue "corpus/corpus.json"
            let corpus = single args "--corpus" |> Option.defaultValue "../ranvier-fsharp-corpus" |> Path.GetFullPath

            let repos =
                try
                    (System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText manifest)).["repos"].AsArray()
                with e ->
                    usageError $"cannot read %s{manifest} (%s{e.Message})"

            let oracle = OracleClient.start().GetAwaiter().GetResult()
            let mutable failed = false

            try
                for repo in repos do
                    let name = repo["name"].GetValue<string>()

                    let project =
                        Path.Combine(corpus, name, repo["project"].GetValue<string>().Replace('/', Path.DirectorySeparatorChar))

                    let! clean = CorpusCheck.verify oracle name project

                    if not clean then
                        failed <- true
            finally
                (oracle :> IAsyncDisposable).DisposeAsync().AsTask().GetAwaiter().GetResult()

            return if failed then 1 else 0
        }

    /// Exit codes: 0 success, 1 a run had failing steps, 2 bad usage.
    let execute (argv: string[]) : Task<int> =
        task {
            try
                match List.ofArray argv with
                | "run" :: rest -> return! runCommand (parse rest)
                | "compare" :: rest -> return! compareCommand (parse rest)
                | "verify-corpus" :: rest -> return! verifyCorpusCommand (parse rest)
                | _ -> return usageError "expected 'run', 'compare' or 'verify-corpus'"
            with Usage message ->
                eprintfn "%s\n%s" message usage
                return 2
        }

module Program =
    [<EntryPoint>]
    let main argv = (Cli.execute argv).GetAwaiter().GetResult()
