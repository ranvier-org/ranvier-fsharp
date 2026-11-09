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

let private fakeConfig: ServerConfig =
    { Name = "fake"
      Command = "fake"
      Args = []
      Env = Map.empty
      InitOptions = JsonObject()
      Oracle = Report
      ProjectLoad = None
      Cwd = None }

let private withRunConfig
    (config: ServerConfig)
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
        (Runner.runConnected opts client None config (scenario (steps path)) root oracle)
            .GetAwaiter()
            .GetResult()

    scripted.GetAwaiter().GetResult()
    Directory.Delete(root, true)
    result, client, path

let private withRunMode (mode: OracleMode) = withRunConfig { fakeConfig with Oracle = mode }

let private withRun = withRunMode Report

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

// These tests assert elapsed milliseconds against delays the in-process server also waits on, so both sit on
// the same thread pool. Run beside the other lists (about 20 pool threads blocked or queued, 10-20 work items
// pending) a 150 ms server delay took 430 ms and a 400 ms quiet window closed before the second publish arrived.
// Synchronous sequencing runs them with nothing else on the pool.
[<Tests>]
let tests =
    testSequenced
    <| testList
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

          test "init options are also sent as the FSharp settings after initialized" {
              let mutable methods = []
              let mutable settings: JsonNode = null
              let mutable answered: JsonNode = null

              let _ =
                  withRunConfig
                      { fakeConfig with InitOptions = JsonNode.Parse """{"Linter":false}""" }
                      options
                      (fun _ -> [ Open "A.fs" ])
                      (fun server path ->
                          task {
                              let! changed = server.WaitForMethod "workspace/didChangeConfiguration"
                              settings <- (changed["params"]["settings"]).DeepClone()

                              let! config =
                                  server.Request("workspace/configuration", JsonNode.Parse """{"items":[{"section":"FSharp"}]}""")

                              answered <- config
                              methods <-
                                  server.Received
                                  |> List.choose (fun m ->
                                      match m["method"] with
                                      | null -> None
                                      | name -> Some(name.GetValue<string>()))
                              do! afterOpen server path
                          })
                      None

              Expect.equal (settings.ToJsonString()) """{"FSharp":{"Linter":false}}""" "didChangeConfiguration settings"
              Expect.equal (answered.ToJsonString()) """[{"Linter":false}]""" "workspace/configuration answer"
              let index name = methods |> List.findIndex ((=) name)
              Expect.isLessThan (index "initialized") (index "workspace/didChangeConfiguration") "after initialized"
          }

          test "the project load request carries the project and counts towards cold" {
              let mutable loadParams: JsonNode = null

              let result, _, _ =
                  withRunConfig
                      { fakeConfig with ProjectLoad = Some "fsharp/workspaceLoad" }
                      options
                      (fun _ -> [ Open "A.fs" ])
                      (fun server path ->
                          task {
                              server.OnRequest(
                                  "fsharp/workspaceLoad",
                                  fun p ->
                                      loadParams <- p.DeepClone()
                                      Threading.Thread.Sleep 150
                                      JsonNode.Parse """{"content":"ok"}"""
                              )

                              do! afterOpen server path
                          })
                      None

              Expect.isNotNull loadParams "load request sent"
              let documents = loadParams["textDocuments"]
              let document = documents[0]
              let uri = document["uri"].GetValue<string>()
              Expect.stringEnds uri "/small/P.fsproj" "the scenario's project"
              Expect.isGreaterThan result.ColdMs 150. "load time counted as cold"
              Expect.equal (result.Steps |> List.map _.Step) [ "open A.fs" ] "no extra step when the load succeeds"
          }

          test "a project load that never answers fails a step and the run goes on" {
              let result, _, _ =
                  withRunConfig
                      { fakeConfig with ProjectLoad = Some "fsharp/workspaceLoad" }
                      options
                      (fun _ -> [ Open "A.fs" ])
                      (fun server path ->
                          task {
                              server.NeverAnswer "fsharp/workspaceLoad"
                              do! afterOpen server path
                          })
                      None

              match result.Steps with
              | [ load; opened ] ->
                  Expect.stringStarts load.Step "load P.fsproj" "load step named after the project"
                  Expect.isSome load.Error "load failed"
                  Expect.isNone opened.Error "open still ran"
              | other -> failtestf "unexpected steps %A" other
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

          // The oracle reports one error the server does not publish.
          let mismatch (mode: OracleMode) =
              let oracle (texts: Map<string, string>) =
                  let file = texts |> Map.toList |> List.head |> fst

                  Task.FromResult
                      [ { File = file
                          Range =
                            { Start = { Line = 0; Character = 0 }
                              End = { Line = 0; Character = 1 } }
                          Severity = 1
                          Code = "FS0001"
                          Message = "boom"
                          Source = "F# Compiler" } ]

              let result, _, _ =
                  withRunMode
                      mode
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! publish server path (Some 1)
                          })
                      (Some oracle)

              result

          test "oracle: a Strict server with a diff gets an error on the step, the run goes on" {
              let result = mismatch Strict
              Expect.equal result.Steps[1].Error (Some "oracle mismatch: 1 diffs") "step failed"
              Expect.isNone result.Steps[0].Error "open is not checked"
          }

          test "oracle: a Report server with a diff only records it" {
              let result = mismatch Report
              Expect.isNone result.Steps[1].Error "no error"
              Expect.equal (result.Steps[1].OracleDiff |> Option.map List.length) (Some 1) "recorded"
          }

          test "oracle: a Strict server without a diff has no error" {
              let result, _, _ =
                  withRunMode
                      Strict
                      options
                      (fun _ -> [ Open "A.fs"; Edit("A.fs", insert "0", 1, Apply) ])
                      (fun server path ->
                          task {
                              do! afterOpen server path
                              let! _ = waitChange server 1
                              do! publish server path (Some 1)
                          })
                      (Some(fun _ -> Task.FromResult []))

              Expect.isNone result.Steps[1].Error "no error"
              Expect.equal result.Steps[1].OracleDiff (Some []) "equal"
          }

          test "oracle: diagnostics in files the client has not opened are not compared" {
              let at file message : Diagnostic =
                  { File = file
                    Range =
                      { Start = { Line = 0; Character = 0 }
                        End = { Line = 0; Character = 1 } }
                    Severity = 1
                    Code = "FS0001"
                    Message = message
                    Source = "F# Compiler" }

              let oracle (texts: Map<string, string>) =
                  let opened = texts |> Map.toList |> List.head |> fst
                  let unopened = Path.Combine(Path.GetDirectoryName opened, "Unopened.fs")
                  Task.FromResult [ at opened "boom"; at unopened "elsewhere" ]

              let result, _, _ =
                  withRunMode
                      Strict
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
                                          DiagCode = "FS0001"
                                          DiagMessage = "boom" } ]
                                  )
                          })
                      (Some oracle)

              Expect.equal result.Steps[1].OracleDiff (Some []) "only the opened file's diagnostic is compared"
          } ]
