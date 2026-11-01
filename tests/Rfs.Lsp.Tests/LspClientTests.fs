module Rfs.Lsp.Tests.LspClientTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Lsp

type private Collector<'T>() =
    let items = ConcurrentQueue<'T>()
    let signal = new SemaphoreSlim(0)

    /// Waits for the next unseen item.
    member _.Next() : Task<'T> =
        task {
            let! ok = signal.WaitAsync(TimeSpan.FromSeconds 10.)
            Expect.isTrue ok "an item arrived in time"
            let mutable item = Unchecked.defaultof<'T>
            items.TryDequeue(&item) |> ignore
            return item
        }

    interface IObserver<'T> with
        member _.OnNext v =
            items.Enqueue v
            signal.Release() |> ignore

        member _.OnError _ = ()
        member _.OnCompleted() = ()

let private at (keys: string list) (node: JsonNode) =
    keys |> List.fold (fun (n: JsonNode) k -> n[k]) node

let private str (node: JsonNode) = node.GetValue<string>()
let private num (node: JsonNode) = node.GetValue<int>()
let private bln (node: JsonNode) = node.GetValue<bool>()

let private withClient(body: FakeServer -> LspClient -> Task) : Task =
    task {
        use server = new FakeServer()
        let client = LspClient.Connect(server.ClientInput, server.ClientOutput)
        do! body server client
    }

let private filePath = Path.Combine(Path.GetTempPath(), "rfs-lsp-tests", "A.fs")

let private oneDiagnostic =
    [ { DiagLine = 2
        DiagStartChar = 1
        DiagEndChar = 4
        DiagSeverity = 1
        DiagCode = "FS0001"
        DiagMessage = "boom" } ]

[<Tests>]
let tests =
    testList
        "LspClient"
        [ testTask "client answers server requests" {
              do!
                  withClient (fun server _ ->
                      task {
                          let! config =
                              server.Request(
                                  "workspace/configuration",
                                  JsonNode.Parse """{"items":[{"section":"FSharp"},{"section":"editor"}]}"""
                              )

                          let array = config.AsArray()
                          Expect.equal array.Count 2 "one entry per requested item"
                          Expect.isTrue (array |> Seq.forall isNull) "entries are null"

                          let! registered =
                              server.Request("client/registerCapability", JsonNode.Parse """{"registrations":[]}""")

                          Expect.isNull registered "registerCapability answered with null"

                          let! other = server.Request("window/workDoneProgress/create", JsonNode.Parse """{"token":"t"}""")
                          Expect.isNull other "unknown requests answered with null"
                      })
          }

          testTask "diagnostics carry version" {
              do!
                  withClient (fun server client ->
                      task {
                          let seen = Collector<int64 * string * int option * Diagnostic list>()
                          use _ = client.Diagnostics.Subscribe seen

                          do! server.PublishDiagnostics(filePath, Some 3, oneDiagnostic)
                          let! (stamp, file, version, list) = seen.Next()
                          Expect.isGreaterThan stamp 0L "receive timestamp"
                          Expect.equal file filePath "URI converted back to a path"
                          Expect.equal version (Some 3) "version"

                          Expect.equal
                              list
                              [ { File = filePath
                                  Range =
                                    { Start = { Line = 2; Character = 1 }
                                      End = { Line = 2; Character = 4 } }
                                  Severity = 1
                                  Code = "FS0001"
                                  Message = "boom"
                                  Source = "fake" } ]
                              "parsed diagnostic"

                          do! server.PublishDiagnostics(filePath, None, [])
                          let! (_, _, version, list) = seen.Next()
                          Expect.equal version None "no version"
                          Expect.isEmpty list "cleared"
                      })
          }

          testTask "request returns the result and a latency" {
              do!
                  withClient (fun server client ->
                      task {
                          server.OnRequest("custom/echo", fun p -> p.DeepClone())
                          let! (result, elapsed) = client.Request("custom/echo", JsonNode.Parse """{"a":1}""")
                          Expect.equal (result |> at [ "a" ] |> num) 1 "echoed"
                          Expect.isGreaterThan elapsed TimeSpan.Zero "elapsed"
                      })
          }

          testTask "initialize sends rootUri, options and initialized" {
              do!
                  withClient (fun server client ->
                      task {
                          let! elapsed = client.Initialize(Uri(Path.GetTempPath()).AbsoluteUri, JsonNode.Parse """{"x":true}""")
                          Expect.isGreaterThan elapsed TimeSpan.Zero "elapsed"
                          let! init = server.WaitForMethod "initialize"
                          Expect.equal (init |> at [ "params"; "initializationOptions"; "x" ] |> bln) true "options passed"
                          let! _ = server.WaitForMethod "initialized"
                          ()
                      })
          }

          testTask "didChange is incremental and the client tracks the text" {
              do!
                  withClient (fun server client ->
                      task {
                          do! client.DidOpen(filePath, "let x = 1\r\n")

                          let! opened = server.WaitForMethod "textDocument/didOpen"
                          Expect.equal (opened |> at [ "params"; "textDocument"; "text" ] |> str) "let x = 1\r\n" "text"
                          Expect.equal (opened |> at [ "params"; "textDocument"; "languageId" ] |> str) "fsharp" "language"

                          let e: Edit =
                              { Range =
                                  { Start = { Line = 0; Character = 8 }
                                    End = { Line = 0; Character = 9 } }
                                Text = "2" }

                          do! client.DidChange(filePath, 1, e)
                          let! changed = server.WaitForMethod "textDocument/didChange"
                          let change = (changed |> at [ "params"; "contentChanges" ]).AsArray()[0]
                          Expect.equal (change |> at [ "text" ] |> str) "2" "edit text"
                          Expect.equal (change |> at [ "range"; "start"; "character" ] |> num) 8 "range start"
                          Expect.equal (changed |> at [ "params"; "textDocument"; "version" ] |> num) 1 "version"
                          Expect.equal (client.Text filePath) "let x = 2\r\n" "local text"

                          do! client.DidSave filePath
                          let! _ = server.WaitForMethod "textDocument/didSave"
                          ()
                      })
          }

          testTask "notifications are observable" {
              do!
                  withClient (fun server client ->
                      task {
                          let seen = Collector<Received>()
                          use _ = client.Notifications.Subscribe seen
                          do! server.Notify("$/progress", JsonNode.Parse """{"token":1}""")
                          let! r = seen.Next()
                          Expect.equal (r.Message |> at [ "method" ] |> str) "$/progress" "method"
                      })
          }

          testTask "a malformed message is skipped and reported, later requests still complete" {
              do!
                  withClient (fun server client ->
                      task {
                          server.OnRequest("custom/echo", fun p -> p.DeepClone())
                          let seen = Collector<Received>()
                          use _ = client.Notifications.Subscribe seen

                          // no uri, and a diagnostic without a range
                          do! server.Notify("textDocument/publishDiagnostics", JsonNode.Parse """{"diagnostics":[{"severity":"x"}]}""")
                          do! server.Notify("textDocument/publishDiagnostics", JsonNode.Parse """{"uri":"file:///x.fs","diagnostics":[{"severity":"x"}]}""")

                          let! (result, _) = client.Request("custom/echo", JsonNode.Parse """{"a":1}""", TimeSpan.FromSeconds 10.)
                          Expect.equal (result |> at [ "a" ] |> num) 1 "request answered after bad frames"

                          let! r = seen.Next()
                          Expect.equal (r.Message |> at [ "method" ] |> str) "$/rfs/malformedMessage" "first bad frame surfaced"
                          let! r = seen.Next()
                          Expect.equal (r.Message |> at [ "method" ] |> str) "$/rfs/malformedMessage" "second bad frame surfaced"
                      })
          }

          testTask "request after the connection ends faults instead of hanging" {
              let server = new FakeServer()
              let client = LspClient.Connect(server.ClientInput, server.ClientOutput)
              let ended = TaskCompletionSource()

              let _subscription =
                  client.Notifications.Subscribe
                      { new IObserver<Received> with
                          member _.OnNext _ = ()
                          member _.OnError _ = ()
                          member _.OnCompleted() = ended.SetResult() }

              (server :> IDisposable).Dispose()
              do! ended.Task.WaitAsync(TimeSpan.FromSeconds 10.)

              let! outcome =
                  task {
                      try
                          let! _ = client.Request("custom/echo", null, TimeSpan.FromSeconds 5.)
                          return (None: exn option)
                      with e ->
                          return Some e
                  }

              match outcome with
              | Some(e: exn) when (e :? IOException) -> ()
              | other -> failtestf "expected IOException, got %A" other
          }

          testTask "request times out when the server never answers" {
              do!
                  withClient (fun server client ->
                      task {
                          server.NeverAnswer "custom/never"

                          let! outcome =
                              task {
                                  try
                                      let! _ = client.Request("custom/never", null, TimeSpan.FromMilliseconds 200.)
                                      return "answered"
                                  with :? TimeoutException ->
                                      return "timeout"
                              }

                          Expect.equal outcome "timeout" "bounded wait"
                      })
          }

          testTask "file URIs from servers map onto the opened path" {
              if OperatingSystem.IsWindows() then
                  do!
                      withClient (fun server client ->
                          task {
                              let seen = Collector<int64 * string * int option * Diagnostic list>()
                              use _ = client.Diagnostics.Subscribe seen
                              do! client.DidOpen(@"C:\rfs-uri-test\y.fs", "let x = 1")

                              do!
                                  server.Notify(
                                      "textDocument/publishDiagnostics",
                                      JsonNode.Parse """{"uri":"file:///c%3A/rfs-uri-test/y.fs","diagnostics":[]}"""
                                  )

                              let! (_, file, _, _) = seen.Next()
                              Expect.equal file @"C:\rfs-uri-test\y.fs" "drive letter and separators normalised"
                              Expect.equal (client.Text @"c:\rfs-uri-test\y.fs") "let x = 1" "same document under either spelling"
                              let! opened = server.WaitForMethod "textDocument/didOpen"
                              Expect.equal (opened |> at [ "params"; "textDocument"; "uri" ] |> str) "file:///C:/rfs-uri-test/y.fs" "sent URI"
                          })
          } ]
