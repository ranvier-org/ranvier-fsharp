namespace Rfs.Harness

open System
open System.Globalization
open System.Text
open System.Text.Json.Nodes

/// One (server, scenario) line of the report. Latencies leave out failed steps: a timed-out edit would read as 120000 ms.
type SummaryRow =
    { Server: string
      Scenario: string
      /// Both directions of a toggled edit count.
      EditP50: float option
      EditP95: float option
      /// LSP method to p50 latency.
      RequestP50: Map<string, float>
      ColdMs: float
      PublishesPerEdit: float option
      /// Over the edits whose server reported a count; None when none did.
      DeclsPerEdit: float option
      PeakMb: float
      AllocatedMb: float
      OracleDiffs: int
      FailedSteps: int }

type Summary = SummaryRow list

module Report =
    /// The p-th percentile by nearest rank: the ceil(p/100 * n)-th smallest value.
    let percentile (p: float) (values: float list) : float option =
        match List.sort values with
        | [] -> None
        | sorted ->
            let rank = int (Math.Ceiling(p / 100. * float sorted.Length))
            Some sorted[max 1 (min sorted.Length rank) - 1]

    let private requestMethods =
        Map.ofList
            [ "hover", "textDocument/hover"
              "completion", "textDocument/completion"
              "definition", "textDocument/definition"
              "references", "textDocument/references" ]

    let private kindOf (step: StepResult) =
        step.Step.Split(' ', 2)[0]

    let private mean (values: float list) =
        match values with
        | [] -> None
        | _ -> Some(List.average values)

    let private summariseRun (run: RunResult) : SummaryRow =
        let succeeded = run.Steps |> List.filter (fun s -> s.Error.IsNone)
        let edits = succeeded |> List.filter (fun s -> kindOf s = "edit")

        let requests =
            succeeded
            |> List.choose (fun s -> requestMethods.TryFind(kindOf s) |> Option.map (fun m -> m, s.LatencyMs))
            |> List.groupBy fst
            |> List.choose (fun (m, xs) -> percentile 50. (List.map snd xs) |> Option.map (fun v -> m, v))
            |> Map.ofList

        let editLatencies = edits |> List.map _.LatencyMs

        { Server = run.Server
          Scenario = run.Scenario
          EditP50 = percentile 50. editLatencies
          EditP95 = percentile 95. editLatencies
          RequestP50 = requests
          ColdMs = run.ColdMs
          PublishesPerEdit = edits |> List.map (fun s -> float s.Publishes) |> mean
          DeclsPerEdit = edits |> List.choose _.DeclsChecked |> List.map float |> mean
          PeakMb = run.PeakWorkingSetMb
          AllocatedMb = run.AllocatedMb
          OracleDiffs = run.Steps |> List.sumBy (fun s -> s.OracleDiff |> Option.map List.length |> Option.defaultValue 0)
          FailedSteps = run.Steps |> List.filter (fun s -> s.Error.IsSome) |> List.length }

    /// One row per (server, scenario), ordered by scenario then server; of several runs the latest wins.
    let summarise (results: RunResult list) : Summary =
        results
        |> List.groupBy (fun r -> r.Scenario, r.Server)
        |> List.sortBy fst
        |> List.map (fun (_, runs) -> runs |> List.maxBy _.StartedAt |> summariseRun)

    // ---- result files ----

    let fileName (run: RunResult) =
        let safe (s: string) =
            String(s |> Seq.map (fun c -> if Char.IsLetterOrDigit c || c = '-' || c = '_' then c else '_') |> Seq.toArray)

        $"%s{safe run.Server}.%s{safe run.Scenario}.json"

    let private stepToJson (s: StepResult) : JsonNode =
        let o = JsonObject()
        o["index"] <- JsonValue.Create s.Index
        o["step"] <- JsonValue.Create s.Step
        o["latencyMs"] <- JsonValue.Create s.LatencyMs
        s.Answer |> Option.iter (fun a -> o["answer"] <- JsonValue.Create a)
        o["publishes"] <- JsonValue.Create s.Publishes

        o["declsChecked"] <-
            match s.DeclsChecked with
            | Some n -> JsonValue.Create n
            | None -> null

        o["oracleDiff"] <-
            match s.OracleDiff with
            | Some diffs -> JsonArray([| for d in diffs -> JsonValue.Create d :> JsonNode |])
            | None -> null

        o["error"] <-
            match s.Error with
            | Some e -> JsonValue.Create e
            | None -> null

        o

    let resultToJson (run: RunResult) : string =
        let o = JsonObject()
        o["server"] <- JsonValue.Create run.Server
        o["scenario"] <- JsonValue.Create run.Scenario
        o["commit"] <- JsonValue.Create run.Commit
        o["startedAt"] <- JsonValue.Create(run.StartedAt.ToString("o", CultureInfo.InvariantCulture))
        o["coldMs"] <- JsonValue.Create run.ColdMs
        o["steps"] <- JsonArray([| for s in run.Steps -> stepToJson s |])
        o["peakWorkingSetMb"] <- JsonValue.Create run.PeakWorkingSetMb
        o["allocatedMb"] <- JsonValue.Create run.AllocatedMb
        o["gen2"] <- JsonValue.Create run.Gen2
        o.ToJsonString(Text.Json.JsonSerializerOptions(WriteIndented = true))

    let private stepOfJson (n: JsonNode) : StepResult =
        let opt (name: string) (read: JsonNode -> 'a) =
            match n[name] with
            | null -> None
            | v -> Some(read v)

        { Index = n["index"].GetValue<int>()
          Step = n["step"].GetValue<string>()
          LatencyMs = n["latencyMs"].GetValue<float>()
          Publishes = n["publishes"].GetValue<int>()
          DeclsChecked = opt "declsChecked" (fun v -> v.GetValue<int>())
          OracleDiff = opt "oracleDiff" (fun v -> [ for d in v.AsArray() -> d.GetValue<string>() ])
          Answer = opt "answer" (fun v -> v.GetValue<string>())
          Error = opt "error" (fun v -> v.GetValue<string>()) }

    let resultOfJson (json: string) : RunResult =
        let n = JsonNode.Parse json

        { Server = n["server"].GetValue<string>()
          Scenario = n["scenario"].GetValue<string>()
          Commit = n["commit"].GetValue<string>()
          StartedAt =
            DateTime.Parse(n["startedAt"].GetValue<string>(), CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind)
          ColdMs = n["coldMs"].GetValue<float>()
          Steps = [ for s in n["steps"].AsArray() -> stepOfJson s ]
          PeakWorkingSetMb = n["peakWorkingSetMb"].GetValue<float>()
          AllocatedMb = n["allocatedMb"].GetValue<float>()
          Gen2 = n["gen2"].GetValue<int>() }

    // ---- markdown ----

    let private fmt (value: float) = value.ToString("F1", CultureInfo.InvariantCulture)

    let private cell (value: float option) =
        match value with
        | Some v -> fmt v
        | None -> "-"

    let private cell2 (value: float option) =
        match value with
        | Some v -> v.ToString("F2", CultureInfo.InvariantCulture)
        | None -> "-"

    let private table (header: string list) (rows: string list list) =
        let line (cells: string list) = "| " + String.Join(" | ", cells) + " |"
        [ line header
          line [ for _ in header -> "---" ]
          yield! rows |> List.map line ]

    /// Two result sets side by side: per scenario, each server's row from `a` followed by its row from `b`.
    let renderComparison (labelA: string, a: Summary) (labelB: string, b: Summary) : string =
        let tagged =
            [ for r in a -> labelA, r ]
            @ [ for r in b -> labelB, r ]

        let scenarios = tagged |> List.map (fun (_, r) -> r.Scenario) |> List.distinct |> List.sort
        let sb = StringBuilder()
        let add (line: string) = sb.Append(line).Append('\n') |> ignore

        add "# Benchmark comparison"
        add ""
        add $"Runs: `%s{labelA}` and `%s{labelB}`. Latencies are nearest-rank percentiles over steps that did not fail."

        for scenario in scenarios do
            let rows =
                tagged
                |> List.filter (fun (_, r) -> r.Scenario = scenario)
                |> List.sortBy (fun (label, r) -> r.Server, (if label = labelA then 0 else 1))

            add ""
            add $"## %s{scenario}"
            add ""

            let main =
                rows
                |> List.map (fun (label, r) ->
                    [ label
                      r.Server
                      cell r.EditP50
                      cell r.EditP95
                      fmt r.ColdMs
                      cell2 r.PublishesPerEdit
                      cell2 r.DeclsPerEdit
                      fmt r.PeakMb
                      fmt r.AllocatedMb
                      string r.OracleDiffs
                      string r.FailedSteps ])

            for line in
                table
                    [ "Run"
                      "Server"
                      "Edit p50 ms"
                      "Edit p95 ms"
                      "Cold ms"
                      "Publishes/edit"
                      "Decls/edit"
                      "Peak MB"
                      "Allocated MB"
                      "Oracle diffs"
                      "Failed steps" ]
                    main do
                add line

            let methods = rows |> List.collect (fun (_, r) -> r.RequestP50 |> Map.keys |> List.ofSeq) |> List.distinct |> List.sort

            if not methods.IsEmpty then
                add ""
                add "Request p50 ms"
                add ""

                let requestRows =
                    rows
                    |> List.map (fun (label, r) -> label :: r.Server :: [ for m in methods -> cell (r.RequestP50.TryFind m) ])

                for line in table ("Run" :: "Server" :: methods) requestRows do
                    add line

        sb.ToString()
