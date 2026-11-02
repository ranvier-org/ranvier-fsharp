module Rfs.Checkpoint.Tests.CheckpointTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Rfs.Checkpoint
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects

let private fixturesDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures")
        if Directory.Exists(Path.Combine(candidate, "Deps")) then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private freshDir (prefix: string) =
    let dir = Path.Combine(AppContext.BaseDirectory, prefix + "-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    dir

/// Where `needle` starts in `text`, as an LSP position; positions come from the fixture's current text so the test
/// survives edits to it elsewhere.
let private positionOf (text: string) (needle: string) : Position =
    let lines = text.Split('\n')

    match lines |> Array.tryFindIndex (fun l -> l.Contains needle) with
    | Some line ->
        { Line = line
          Character = lines[line].IndexOf needle }
    | None -> failtest $"fixture text has no '%s{needle}'"

let private rangeJson (start: Position) (length: int) =
    let pos (p: Position) =
        JsonObject([ Collections.Generic.KeyValuePair("line", JsonValue.Create p.Line :> JsonNode)
                     Collections.Generic.KeyValuePair("character", JsonValue.Create p.Character :> JsonNode) ])

    let o = JsonObject()
    o["start"] <- pos start
    o["end"] <- pos { start with Character = start.Character + length }
    o

let private toggle (file: string) (start: Position) (length: int) (text: string) (repeat: int) : JsonNode =
    let edit = JsonObject()
    edit["file"] <- JsonValue.Create file
    edit["range"] <- rangeJson start length
    edit["text"] <- JsonValue.Create text
    edit["repeat"] <- JsonValue.Create repeat
    edit["mode"] <- JsonValue.Create "toggle"
    let step = JsonObject()
    step["edit"] <- edit
    step

/// Five edits of Deps/A.fs: a literal changed and back, then a return type made wrong, right and wrong again.
let private writeDepsScenario (corpus: string) (dir: string) =
    let a = File.ReadAllText(Path.Combine(fixturesDir, "Deps", "A.fs"))
    let literal = positionOf a "= 10"
    let returnType = positionOf a ": int = x * 2"
    let o = JsonObject()
    o["name"] <- JsonValue.Create "deps-five"
    o["corpus"] <- JsonValue.Create corpus
    o["project"] <- JsonValue.Create "Deps.fsproj"
    o["kind"] <- JsonValue.Create "body"
    let opener = JsonObject()
    opener["open"] <- JsonValue.Create "A.fs"

    o["steps"] <-
        JsonArray(
            [| opener :> JsonNode
               toggle "A.fs" { literal with Character = literal.Character + 2 } 2 "11" 2
               toggle "A.fs" { returnType with Character = returnType.Character + 2 } 3 "int64" 3 |]
        )

    let path = Path.Combine(dir, "deps-five.json")
    File.WriteAllText(path, o.ToJsonString())
    path

let private readResults (dir: string) =
    [ for f in Directory.GetFiles(dir, "*.json") -> Report.resultOfJson (File.ReadAllText f) ]

/// Answers every check with the same made-up error, which no full check of Deps reports.
let private wrongEngine: EngineFactory =
    { Name = "wrong"
      Create =
        fun spec ->
            let bogus: Diagnostic =
                { File = List.head spec.SourceFiles
                  Range =
                    { Start = { Line = 0; Character = 0 }
                      End = { Line = 0; Character = 1 } }
                  Severity = 1
                  Code = "FS0001"
                  Message = "not from the compiler"
                  Source = "F# Compiler" }

            let result =
                Task.FromResult
                    { Diagnostics = [ bogus ]
                      DeclsChecked = None
                      GraphCheck = None }

            Task.FromResult
                { new IEngine with
                    member _.Counters() = None
                    member _.DeclarationLines(_, _) = None
                    member _.Check() = result
                    member _.SetText(_, _) = result
                  interface IDisposable with
                      member _.Dispose() = () } }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Checkpoint"
        [ testTask "checkpoint on Deps" {
              let work = freshDir "deps"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code = Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out |]

              let results = readResults out
              let failures =
                  [ for r in results do
                        for s in r.Steps do
                            match s.Error, s.OracleDiff with
                            | None, Some [] -> ()
                            | e, d -> $"%s{r.Server} %s{s.Step}: %A{e} %A{d}" ]

              Expect.isEmpty failures "every step matches the oracle"
              Expect.equal code 0 "exit code"
              Expect.equal (results |> List.map _.Server |> List.sort) [ "decl"; "file-slim" ] "one result per engine"

              for r in results do
                  Expect.equal r.Scenario "deps-five" "scenario name"
                  Expect.equal r.Steps.Length 5 $"%s{r.Server}: one step per edit"
                  Expect.isGreaterThan r.ColdMs 0. $"%s{r.Server}: cold check timed"
                  Expect.isGreaterThan r.AllocatedMb 0. $"%s{r.Server}: allocations recorded"
                  Expect.isGreaterThan r.PeakWorkingSetMb 0. $"%s{r.Server}: peak working set recorded"
                  Expect.isTrue (File.Exists(Path.Combine(out, Report.fileName r))) "named like the harness's files"

              let decl = results |> List.find (fun r -> r.Server = "decl")
              let slim = results |> List.find (fun r -> r.Server = "file-slim")
              Expect.isTrue (decl.Steps |> List.forall (fun s -> s.DeclsChecked |> Option.exists (fun n -> n > 0))) "decl counts its checks"
              Expect.isTrue (slim.Steps |> List.forall (fun s -> s.DeclsChecked.IsNone)) "file-slim has no declaration count"
              Expect.isLessThan slim.StartedAt decl.StartedAt "the baseline runs first"

              let extraOf (r: RunResult) =
                  JsonNode.Parse(File.ReadAllText(Path.Combine(out, "extra", Report.fileName r)))

              let declExtra = extraOf decl
              Expect.isNotNull (declExtra["coldCounters"]) "decl's counters after the cold check"
              let coldAdopted = declExtra["coldCounters"].["adopted"].GetValue<int64>()
              Expect.isGreaterThan coldAdopted 0L "the cold check adopts checks"
              Expect.isNotNull (declExtra["finishShareP50"]) "finish share p50"
              let declSteps = declExtra["steps"].AsArray() |> List.ofSeq
              Expect.equal declSteps.Length 5 "one extra per edit"

              for s, r in List.zip declSteps decl.Steps do
                  let adopted = s["adopted"].GetValue<int64>()
                  Expect.equal (Some(int adopted)) r.DeclsChecked $"%s{r.Step}: decls/edit is the adopted count"
                  Expect.isGreaterThanOrEqual (s["checked"].GetValue<int64>()) adopted $"%s{r.Step}: runs include adopted ones"
                  Expect.isGreaterThan (s["finishMs"].GetValue<float>()) 0. $"%s{r.Step}: finish time"
                  let share = s["finishShare"].GetValue<float>()
                  Expect.isTrue (share > 0. && share <= 1.) $"%s{r.Step}: finish share within the latency ({share})"
                  Expect.isGreaterThanOrEqual (s["segmentRows"].GetValue<int>()) 0 $"%s{r.Step}: segment rows"

              let slimExtra = extraOf slim
              Expect.isNull (slimExtra["coldCounters"]) "file-slim keeps no counters"
              Expect.equal (slimExtra["steps"].AsArray().Count) 5 "one extra per edit"
          }

          testTask "the oracle sees each edit's text" {
              // An empty diff proves nothing if engine and oracle both saw stale text: the int64 steps must give errors.
              let work = freshDir "seen"
              let scenarioFile = writeDepsScenario "Deps" work
              let scenario = Scenario.load scenarioFile fixturesDir |> Result.defaultWith failtest
              let steps = Driver.plan fixturesDir scenario |> Result.defaultWith failtest
              let! spec = ProjectSpec.load (Path.Combine(fixturesDir, "Deps", "Deps.fsproj"))
              let! (oracle: OracleClient) = OracleClient.start ()

              try
                  for engine in Engines.all do
                      let seen = Collections.Generic.List<Map<string, string> * Diagnostic list>()

                      let ask (overrides: Map<string, string>) =
                          task {
                              let! d = oracle.Check spec.ProjectFile overrides
                              seen.Add((overrides, d))
                              return d
                          }

                      let! run = Driver.runEngine (TimeSpan.FromMinutes 2.) engine spec scenario fixturesDir steps ask

                      Expect.isNone run.Stopped $"%s{engine.Name}: ran to the end"
                      Expect.isTrue (run.Result.Steps |> List.forall (fun s -> s.Error.IsNone)) $"%s{engine.Name}: no step failed"

                      let wrongType =
                          seen
                          |> Seq.filter (fun (overrides, _) -> overrides |> Map.exists (fun _ t -> t.Contains ": int64 = x * 2"))
                          |> List.ofSeq

                      Expect.equal wrongType.Length 2 $"%s{engine.Name}: two int64 steps reached the oracle"

                      for _, diagnostics in wrongType do
                          Expect.isTrue (diagnostics |> List.exists (fun d -> d.Severity = 1)) $"%s{engine.Name}: the oracle reports errors for int64"
              finally
                  (oracle :> IAsyncDisposable).DisposeAsync().AsTask().Wait()
          }

          testTask "checkpoint records cold premise breaks and row sizes" {
              let work = freshDir "rows"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work
              let phases = [ "refilter"; "leftovers"; "assemble"; "replay"; "tail"; "typedUses"; "signature"; "render"; "fingerprints"; "sinks" ]

              let! code = Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out; "--in-process" |]
              Expect.equal code 0 "exit code"
              let extraOf (server: string) = JsonNode.Parse(File.ReadAllText(Path.Combine(out, "extra", $"%s{server}.deps-five.json")))
              let optInt (node: JsonNode) = if isNull node then None else Some(node.GetValue<int>())

              let decl = extraOf "decl"
              Expect.equal (decl["premiseBreaksAtCold"].GetValue<int64>()) 0L "the cold check breaks no D1 premise"
              Expect.isNotNull decl["segmentRowsAtCold"] "segment rows after the cold check"
              let largest, edited = optInt decl["largestRowLines"], optInt decl["editedRowLines"]
              Expect.isSome edited "the first edit falls in a declaration"
              Expect.isGreaterThanOrEqual edited.Value 1 "the edited declaration has a line"
              Expect.isGreaterThanOrEqual largest.Value edited.Value "no declaration of the edited file is larger than the largest"

              for s in decl["steps"].AsArray() do
                  let ms = s["finishPhaseMs"].AsObject()
                  Expect.equal (ms |> Seq.map _.Key |> Seq.sort |> List.ofSeq) (List.sort phases) $"""{s["step"]}: the ten parts of the finish"""

              let slim = extraOf "file-slim"

              for field in [ "premiseBreaksAtCold"; "segmentRowsAtCold"; "largestRowLines"; "editedRowLines" ] do
                  Expect.isNull slim[field] $"file-slim has no %s{field}"
          }

          testTask "--log-cutoff records file-slim's graph-check counts" {
              let work = freshDir "cutoff"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code =
                  Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out; "--log-cutoff" |]

              Expect.equal code 0 "exit code"
              let log = Path.Combine(out, "file-slim.deps-five.cutoff.tsv")
              Expect.isTrue (File.Exists log) "cutoff log written"
              Expect.isFalse (File.Exists(Path.Combine(out, "decl.deps-five.cutoff.tsv"))) "only for file-slim"
              let rows = File.ReadAllLines log |> Array.tail
              let! spec = ProjectSpec.load (Path.Combine(fixturesDir, "Deps", "Deps.fsproj"))
              // The fork logs graph checks only; whether Deps checks in graph mode depends on its project file.
              let graph = spec.OtherOptions |> List.filter (fun o -> o.StartsWith "--parallelcompilation") |> List.tryLast <> Some "--parallelcompilation-"

              if graph then
                  Expect.equal rows.Length 5 "one row per edit"

                  for row in rows do
                      let cells = row.Split('\t')
                      Expect.equal cells.Length 5 "step, name, reused, kept, replaced"
                      Expect.isGreaterThan (int cells[2] + int cells[3] + int cells[4]) 0 "counts every file"
              else
                  Expect.isEmpty rows "no graph checks in sequential mode"
          }

          testTask "a step timeout stops the run at that step" {
              let work = freshDir "timeout"
              let scenarioFile = writeDepsScenario "Deps" work
              let scenario = Scenario.load scenarioFile fixturesDir |> Result.defaultWith failtest
              let steps = Driver.plan fixturesDir scenario |> Result.defaultWith failtest
              let! spec = ProjectSpec.load (Path.Combine(fixturesDir, "Deps", "Deps.fsproj"))
              let checks = ref 0

              let hanging: EngineFactory =
                  { Name = "hanging"
                    Create =
                      fun _ ->
                          let ok = Task.FromResult { Diagnostics = []; DeclsChecked = None; GraphCheck = None }

                          Task.FromResult
                              { new IEngine with
                                  member _.Counters() = None
                                  member _.DeclarationLines(_, _) = None
                                  member _.Check() = ok

                                  member _.SetText(_, _) =
                                      checks.Value <- checks.Value + 1
                                      if checks.Value = 2 then Task.Delay(-1).ContinueWith(fun _ -> ok.Result) else ok
                                interface IDisposable with
                                    member _.Dispose() = () } }

              let! run = Driver.runEngine (TimeSpan.FromSeconds 1.) hanging spec scenario fixturesDir steps (fun _ -> Task.FromResult [])

              Expect.equal checks.Value 2 "no edit after the one that timed out"
              Expect.equal run.Result.Steps.Length 2 "the run ends at the timed-out step"
              Expect.stringContains (defaultArg run.Result.Steps[1].Error "") "timed out" "the step says why"
              Expect.isSome run.Stopped "the run says it stopped"
          }

          testTask "refuses to run outside the repo root" {
              let work = freshDir "cwd"
              let scenario = writeDepsScenario "Deps" work
              let previous = Environment.CurrentDirectory

              let! code =
                  task {
                      try
                          Environment.CurrentDirectory <- work
                          return! Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; Path.Combine(work, "out") |]
                      finally
                          Environment.CurrentDirectory <- previous
                  }

              Expect.equal code 2 "exit code"
          }

          testTask "a failing child process fails the run" {
              let work = freshDir "child"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              // The child is the rfs-checkpoint apphost, which knows only the real engines, so it rejects this one.
              let! code = Checkpoint.execute [ wrongEngine ] [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out |]

              Expect.notEqual code 0 "exit code"
              Expect.isFalse (Directory.Exists out && Directory.GetFiles(out).Length > 0) "nothing written"
          }

          testTask "an oracle mismatch is a step error and fails the run" {
              let work = freshDir "wrong"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code =
                  Checkpoint.execute [ wrongEngine ] [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out; "--in-process" |]

              Expect.notEqual code 0 "exit code"
              let result = readResults out |> List.exactlyOne
              Expect.equal result.Server "wrong" "server name"
              Expect.equal result.Steps.Length 5 "every edit recorded"

              for s in result.Steps do
                  Expect.isSome s.Error $"%s{s.Step}: error"
                  Expect.stringContains s.Error.Value "oracle mismatch" $"%s{s.Step}: why"
                  Expect.isNonEmpty (defaultArg s.OracleDiff []) $"%s{s.Step}: diff recorded"
          }

          testTask "refuses to measure a corpus that does not check clean" {
              let work = freshDir "broken"
              let corpus = Path.Combine(work, "corpus")
              let deps = Path.Combine(corpus, "Deps")
              Directory.CreateDirectory deps |> ignore

              for f in Directory.GetFiles(Path.Combine(fixturesDir, "Deps")) do
                  File.Copy(f, Path.Combine(deps, Path.GetFileName f))

              let a = Path.Combine(deps, "A.fs")
              File.WriteAllText(a, File.ReadAllText(a).Replace("let baseValue = 10", "let baseValue : string = 10"))
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code = Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; corpus; "--out"; out |]

              Expect.equal code 3 "exit code for a broken corpus"
              Expect.isFalse (Directory.Exists out && Directory.GetFiles(out).Length > 0) "nothing measured"
          } ]
