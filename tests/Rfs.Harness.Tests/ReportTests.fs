module Rfs.Harness.Tests.ReportTests

open System
open System.IO
open Expecto
open Rfs.Harness

let private step index name latency publishes decls (error: string option) : StepResult =
    { Index = index
      Step = name
      LatencyMs = latency
      Publishes = publishes
      DeclsChecked = decls
      OracleDiff = None
      Answer = None
      Error = error }

let private started = DateTime(2026, 11, 1, 12, 0, 0, DateTimeKind.Utc)

/// Four good edits (10, 20, 30, 40 ms) and one timed-out edit, three hovers, one completion.
let private resultA : RunResult =
    { Server = "ranvier"
      Scenario = "body"
      Commit = "abc1234"
      StartedAt = started
      ColdMs = 1500.
      Steps =
        [ step 0 "open A.fs" 900. 1 None None
          step 1 "edit A.fs 1:0-1:0 #1 fwd" 10. 1 (Some 2) None
          step 2 "edit A.fs 1:0-1:0 #1 inv" 20. 3 (Some 4) None
          step 3 "edit A.fs 1:0-1:0 #2 fwd" 30. 1 (Some 6) None
          step 4 "edit A.fs 1:0-1:0 #2 inv" 40. 2 None None
          step 5 "edit A.fs 1:0-1:0 #3 fwd" 120000. 0 None (Some "no matching publishDiagnostics within 120000 ms")
          step 6 "hover A.fs 1:4" 5. 0 None None
          step 7 "hover A.fs 2:4" 7. 0 None None
          step 8 "hover A.fs 3:4" 9. 0 None None
          step 9 "completion A.fs 1:8" 50. 0 None None ]
      PeakWorkingSetMb = 512.5
      AllocatedMb = 2048.5
      Gen2 = 3 }

/// A run with an oracle mismatch on its single edit, and no request steps.
let private resultB : RunResult =
    { Server = "fsac"
      Scenario = "body"
      Commit = "abc1234"
      StartedAt = started
      ColdMs = 4200.
      Steps =
        [ step 0 "open A.fs" 3000. 2 None None
          { step 1 "edit A.fs 1:0-1:0 #1" 100. 4 None None with
              OracleDiff = Some [ "A.fs: extra FS0001 at 1:0" ]
              Error = Some "oracle mismatch: 1 diffs" } ]
      PeakWorkingSetMb = 900.
      AllocatedMb = 4096.
      Gen2 = 9 }

let private withTempDir f =
    let dir = Path.Combine(Path.GetTempPath(), "rfs-report-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore

    try
        f dir
    finally
        Directory.Delete(dir, true)

let private normalise (s: string) = s.ReplaceLineEndings "\n"

[<Tests>]
let tests =
    testList
        "Report"
        [ test "percentiles are nearest-rank" {
              Expect.equal (Report.percentile 50. [ 4.; 1.; 3.; 2. ]) (Some 2.) "p50 of four"
              Expect.equal (Report.percentile 95. [ 1.; 2.; 3.; 4. ]) (Some 4.) "p95 of four"
              Expect.equal (Report.percentile 50. [ 7. ]) (Some 7.) "single"
              Expect.equal (Report.percentile 95. [ for i in 1..20 -> float i ]) (Some 19.) "p95 of twenty"
              Expect.isNone (Report.percentile 50. []) "empty"
          }

          test "summarise leaves failed steps out of latencies and counts them" {
              let row = Report.summarise [ resultA ] |> List.exactlyOne
              Expect.equal row.EditP50 (Some 20.) "edit p50 over the four good edits"
              Expect.equal row.EditP95 (Some 40.) "edit p95"
              Expect.equal row.RequestP50 (Map.ofList [ "textDocument/completion", 50.; "textDocument/hover", 7. ]) "per method"
              Expect.equal row.FailedSteps 1 "the timed-out edit"
              Expect.equal row.PublishesPerEdit (Some 1.75) "mean publishes of the four good edits"
              Expect.equal row.DeclsPerEdit (Some 4.) "mean over the edits that reported"
              Expect.equal row.ColdMs 1500. "cold"
          }

          test "summarise counts oracle diffs" {
              let row = Report.summarise [ resultB ] |> List.exactlyOne
              Expect.equal row.OracleDiffs 1 "one diff"
              Expect.equal row.FailedSteps 1 "strict mismatch is a failed step"
              Expect.isNone row.EditP50 "its only edit failed"
          }

          test "results round-trip through json" {
              let withSave =
                  { resultB with
                      Steps = resultB.Steps @ [ step 2 "save A.fs" 0.1 0 None None ] }

              for r in [ resultA; withSave ] do
                  Expect.equal (Report.resultOfJson (Report.resultToJson r)) r "same result"
          }

          test "compare matches golden" {
              let a = "a", Report.summarise [ resultA ]
              let b = "b", Report.summarise [ resultB ]
              let actual = Report.renderComparison a b
              let golden = File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "golden", "compare.md"))
              Expect.equal (normalise actual) (normalise golden) "markdown"
          }

          test "cli compare reads two result directories" {
              withTempDir (fun root ->
                  let dirA = Path.Combine(root, "a")
                  let dirB = Path.Combine(root, "b")
                  Directory.CreateDirectory dirA |> ignore
                  Directory.CreateDirectory dirB |> ignore
                  File.WriteAllText(Path.Combine(dirA, Report.fileName resultA), Report.resultToJson resultA)
                  File.WriteAllText(Path.Combine(dirB, Report.fileName resultB), Report.resultToJson resultB)
                  let out = Path.Combine(root, "compare.md")

                  let code = (Cli.execute [| "compare"; dirA; dirB; "--out"; out |]).GetAwaiter().GetResult()
                  Expect.equal code 0 "exit code"

                  let expected = Report.renderComparison ("a", Report.summarise [ resultA ]) ("b", Report.summarise [ resultB ])
                  Expect.equal (normalise (File.ReadAllText out)) (normalise expected) "same document")
          }

          test "cli rejects unknown commands and missing arguments" {
              let run args = (Cli.execute args).GetAwaiter().GetResult()
              Expect.equal (run [||]) 2 "no command"
              Expect.equal (run [| "bogus" |]) 2 "unknown command"
              Expect.equal (run [| "compare"; "only-one" |]) 2 "one directory"
              Expect.equal (run [| "run"; "--server"; "x" |]) 2 "no scenario or out"
          }

          test "cli run rejects an unknown server before running anything" {
              withTempDir (fun root ->
                  let servers = Path.Combine(root, "servers.json")
                  File.WriteAllText(servers, """[{"name":"fake","command":"nonexistent-binary"}]""")
                  let out = Path.Combine(root, "out")

                  let code =
                      (Cli.execute
                          [| "run"; "--servers"; servers; "--server"; "nope"; "--scenario"; Path.Combine(root, "*.json"); "--corpus"; root; "--out"; out |])
                          .GetAwaiter()
                          .GetResult()

                  Expect.equal code 2 "usage error"
                  Expect.isFalse (Directory.Exists out) "nothing written")
          } ]
