module Rfs.Harness.Tests.RunnerTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Rfs.Lsp
open Rfs.Lsp.Tests
open Rfs.Harness

let private source = "module A\nlet x = 1\nlet y = 2\n"

let private options =
    { Runner.defaultOptions with
        StepTimeout = TimeSpan.FromMilliseconds 500.
        Quiet = TimeSpan.FromMilliseconds 100. }

let private insert text : Edit =
    { Range =
        { Start = { Line = 1; Character = 9 }
          End = { Line = 1; Character = 9 } }
      Text = text }

let private scenario steps : Scenario =
    { Name = "s"
      Corpus = "small"
      Project = "P.fsproj"
      Kind = "body"
      Steps = steps }

let private fileA (root: string) =
    LspClient.NormalizePath(Path.Combine(root, "small", "A.fs"))

let private withRun
    (opts: RunnerOptions)
    (steps: string -> Step list)
    (script: FakeServer -> string -> Task)
    (oracle: (Map<string, string> -> Task<Diagnostic list>) option)
    =
    let root = Path.Combine(Path.GetTempPath(), "rfs-runner-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory(Path.Combine(root, "small")) |> ignore
    File.WriteAllText(Path.Combine(root, "small", "A.fs"), source)
    File.WriteAllText(Path.Combine(root, "small", "P.fsproj"), "<Project />")
    use server = new FakeServer()
    use client = LspClient.Connect(server.ClientInput, server.ClientOutput)
    let path = fileA root
    let scripted = script server path

    let result =
        (Runner.runConnected opts client None "fake" (JsonObject()) (scenario (steps path)) root oracle)
            .GetAwaiter()
            .GetResult()

    scripted.GetAwaiter().GetResult()
    Directory.Delete(root, true)
    result, client, path

let private versionOf (m: JsonObject) =
    let document = m["params"]["textDocument"]
    document["version"].GetValue<int>()

let private waitChange (server: FakeServer) (version: int) =
    server.WaitFor(fun m ->
        not (isNull m["method"])
        && m["method"].GetValue<string>() = "textDocument/didChange"
        && versionOf m = version)

let private publish (server: FakeServer) path version =
    server.PublishDiagnostics(path, version, [])

let private afterOpen (server: FakeServer) path =
    task {
        let! _ = server.WaitForMethod "textDocument/didOpen"
        do! publish server path (Some 0)
    }

[<Tests>]
let tests =
    testList
        "Runner"
        [ test "latency waits for matching version" {
              let result, _, _ =
                  withRun
                      options
                      (fun path -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! publish server path (Some 0)
                              do! Task.Delay 150
                              do! publish server path (Some 1)
                          })
                      None

              let step = result.Steps[1]
              Expect.isNone step.Error "not failed"
              Expect.isGreaterThan step.LatencyMs 140. "measured to the version 1 publish"
              Expect.equal step.Publishes 2 "both publishes counted"
          }

          test "latency without versions ends at the first publish" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! Task.Delay 100
                              do! publish server path None
                          })
                      None

              Expect.isGreaterThan result.Steps[1].LatencyMs 90. "after the send"
              Expect.isLessThan result.Steps[1].LatencyMs 400. "first publish"
          }

          test "quiescence extends step" {
              let result, _, _ =
                  withRun
                      { options with Quiet = TimeSpan.FromMilliseconds 400.; StepTimeout = TimeSpan.FromSeconds 5. }
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! publish server path (Some 1)
                              do! Task.Delay 150
                              do! publish server path (Some 1)
                          })
                      None

              let step = result.Steps[1]
              Expect.equal step.Publishes 2 "late publish inside the window is counted"
              Expect.isLessThan step.LatencyMs 100. "latency stays at the first matching publish"
          }

          test "declsChecked summed" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! server.Notify("rfs/declarationsChecked", JsonNode.Parse """{"count":3}""")
                              do! server.Notify("rfs/declarationsChecked", JsonNode.Parse """{"count":4}""")
                              do! publish server path (Some 1)
                          })
                      None

              Expect.equal result.Steps[0].DeclsChecked None "none arrived during open"
              Expect.equal result.Steps[1].DeclsChecked (Some 7) "summed"
          }

          test "timeout fails step and the run continues" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ ->
                          [ Open "A.fs"
                            Edit("A.fs", insert "0", 1, Apply)
                            Hover("A.fs", { Line = 1; Character = 4 }) ])
                      (fun server path -> afterOpen server path)
                      None

              Expect.isSome result.Steps[1].Error "edit step failed"
              Expect.isGreaterThan result.Steps[1].LatencyMs 450. "waited for the timeout"
              Expect.isNone result.Steps[2].Error "later step ran"
              Expect.equal result.Steps.Length 3 "all steps reported"
          }

          test "request latency is send to response" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Hover("A.fs", { Line = 1; Character = 4 }) ])
                      (fun server path ->
                          task {
                              server.OnRequest(
                                  "textDocument/hover",
                                  fun _ ->
                                      Threading.Thread.Sleep 120
                                      null
                              )

                              do! afterOpen server path
                          })
                      None

              Expect.isGreaterThan result.Steps[1].LatencyMs 100. "includes the server's work"
              Expect.isLessThan result.Steps[1].LatencyMs 450. "not the timeout"
              Expect.isNone result.Steps[1].Error "ok"
          }

          test "toggle alternates the edit with its inverse and restores the text" {
              let result, client, path =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "999", 4, Toggle) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path

                              for v in 1..4 do
                                  let! _ = waitChange server v
                                  do! publish server path (Some v)
                          })
                      None

              Expect.equal result.Steps.Length 5 "open plus four repeats"
              Expect.all result.Steps (fun s -> s.Error.IsNone) "none failed"
              Expect.equal (client.Text path) source "every second state is the original text"

              Expect.equal
                  (result.Steps |> List.skip 1 |> List.map (fun s -> s.Step.Substring(s.Step.Length - 3)))
                  [ "fwd"; "inv"; "fwd"; "inv" ]
                  "forward and inverse steps are named apart"
          }

          test "cold time includes initialize and the first open" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs" ])
                      (fun server path ->
                          task {
                              server.OnRequest(
                                  "initialize",
                                  fun _ ->
                                      Threading.Thread.Sleep 100
                                      JsonNode.Parse """{"capabilities":{}}"""
                              )

                              do! afterOpen server path
                          })
                      None

              Expect.isGreaterThan result.ColdMs 100. "initialize counted"
              Expect.isGreaterThan result.ColdMs result.Steps[0].LatencyMs "more than the open alone"
          }

          test "oracle: a server publish equal to the oracle's gives an empty diff" {
              let boom: Diagnostic =
                  { File = ""
                    Range =
                      { Start = { Line = 0; Character = 0 }
                        End = { Line = 0; Character = 1 } }
                    Severity = 1
                    Code = "FS1"
                    Message = "boom"
                    Source = "oracle" }

              let oracle (texts: Map<string, string>) =
                  Task.FromResult [ { boom with File = texts |> Map.toList |> List.head |> fst } ]

              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1

                              do!
                                  server.PublishDiagnostics(
                                      path,
                                      Some 1,
                                      [ { DiagLine = 0
                                          DiagStartChar = 0
                                          DiagEndChar = 1
                                          DiagSeverity = 1
                                          DiagCode = "FS1"
                                          DiagMessage = "boom" } ]
                                  )
                          })
                      (Some oracle)

              Expect.equal result.Steps[1].OracleDiff (Some []) "same diagnostics"
          }

          test "oracle ignores a late publish of an older version" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! server.PublishDiagnostics(path, Some 1, [])

                              do!
                                  server.PublishDiagnostics(
                                      path,
                                      Some 0,
                                      [ { DiagLine = 0
                                          DiagStartChar = 0
                                          DiagEndChar = 1
                                          DiagSeverity = 1
                                          DiagCode = "FS1"
                                          DiagMessage = "stale" } ]
                                  )
                          })
                      (Some(fun _ -> Task.FromResult []))

              Expect.equal result.Steps[1].OracleDiff (Some []) "the version 1 publish is compared, not the stale one"
          }

          test "edit of a file that was never opened fails the step, not the run" {
              let result, _, _ =
                  withRun
                      options
                      (fun _ ->
                          [ Open "A.fs"
                            Edit("B.fs", insert "0", 1, Apply)
                            Hover("A.fs", { Line = 1; Character = 4 }) ])
                      (fun server path -> afterOpen server path)
                      None

              Expect.isSome result.Steps[1].Error "edit failed"
              Expect.isNone result.Steps[2].Error "later step ran"
          }

          test "oracle diffs are stored per edit step" {
              let expected: Diagnostic =
                  { File = ""
                    Range =
                      { Start = { Line = 0; Character = 0 }
                        End = { Line = 0; Character = 1 } }
                    Severity = 1
                    Code = "FS1"
                    Message = "boom"
                    Source = "oracle" }

              let oracle (texts: Map<string, string>) =
                  Task.FromResult [ { expected with File = texts |> Map.toList |> List.head |> fst } ]

              let result, _, _ =
                  withRun
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! publish server path (Some 1)
                          })
                      (Some oracle)

              Expect.isNone result.Steps[0].OracleDiff "only edits are checked"
              Expect.equal (result.Steps[1].OracleDiff |> Option.map List.length) (Some 1) "oracle reported a diagnostic the server did not"
          }

          test "diffDiagnostics reports both directions and ignores source" {
              let d line : Diagnostic =
                  { File = "F"
                    Range =
                      { Start = { Line = line; Character = 0 }
                        End = { Line = line; Character = 1 } }
                    Severity = 1
                    Code = "FS1"
                    Message = "m"
                    Source = "a" }

              Expect.isEmpty (Runner.diffDiagnostics [ d 1 ] [ { d 1 with Source = "b" } ]) "same"
              Expect.equal (Runner.diffDiagnostics [ d 1 ] [ d 2 ]).Length 2 "one missing, one unexpected"
          } ]
