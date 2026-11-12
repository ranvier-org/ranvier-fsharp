module Rfs.Server.Tests.LifecycleTests

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Text.RegularExpressions
open System.Threading.Tasks
open Expecto
open Rfs.Lsp
open Rfs.Server.Tests

let private initializeParams () =
    JsonNode.Parse """{"processId":null,"rootUri":null,"capabilities":{}}"""

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 30.)
let private request (lsp: LspClient) (method: string) (parameters: JsonNode) : Task<JsonNode * TimeSpan> =
    lsp.Request(method, parameters).WaitAsync(TimeSpan.FromSeconds 30.)

let private at (name: string) (node: JsonNode) : JsonNode = node[name]

let private withinNoResult (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 30.)

/// The exe is copied next to the tests by the project reference
let private serverDll = Path.Combine(AppContext.BaseDirectory, "ranvierautocomplete.dll")

[<Tests>]
let tests =
    testList
        "Lifecycle"
        [ testTask "initialize returns incremental sync" {
              let! started = TestServer.start ()
              let lsp, (owner: IDisposable) = started
              use _ = { new IAsyncDisposable with member _.DisposeAsync() = (owner.Dispose(); ValueTask()) }
              let! reply = request lsp "initialize" (initializeParams ())
              let sync = fst reply |> at "capabilities" |> at "textDocumentSync"
              Expect.equal ((at "openClose" sync).GetValue<bool>()) true "open and close are synchronized"
              Expect.equal ((at "change" sync).GetValue<int>()) 2 "change is Incremental"
          }

          testTask "shutdown then exit ends run with 0" {
              let! lsp, exitCode = TestServer.startRun ()
              use _ = { new IAsyncDisposable with member _.DisposeAsync() = ((lsp :> IDisposable).Dispose(); ValueTask()) }
              let! _ = request lsp "initialize" (initializeParams ())
              do! lsp.Shutdown() |> withinNoResult
              let! code = exitCode |> within
              Expect.equal code 0 "a clean exit"
          }

          testTask "closing the client's end ends the run" {
              let c = TestServer.connect ()
              let! _ = request c.Client "initialize" (initializeParams ())
              c.Owner.Dispose()
              let! _ = c.ExitCode |> within
              ()
          }

          testTask "unimplemented request errors and the server stays up" {
              let! started = TestServer.start ()
              let lsp, (owner: IDisposable) = started
              use _ = { new IAsyncDisposable with member _.DisposeAsync() = (owner.Dispose(); ValueTask()) }
              let! _ = request lsp "initialize" (initializeParams ())

              let onTypeFormatting =
                  JsonNode.Parse
                      """{"textDocument":{"uri":"file:///a.fs"},"position":{"line":0,"character":0},"ch":";","options":{"tabSize":4,"insertSpaces":true}}"""

              let! failure =
                  task {
                      try
                          let! _ = request lsp "textDocument/onTypeFormatting" onTypeFormatting
                          return None
                      with e ->
                          return Some e
                  }

              Expect.isSome failure "onTypeFormatting is answered with an error"
              let! second = request lsp "initialize" (initializeParams ())
              Expect.isNotNull (fst second |> at "capabilities") "the server still answers"
          }

          testTask "stdout carries only protocol" {
              Expect.isTrue (File.Exists serverDll) $"%s{serverDll} was copied next to the tests"
              let info = ProcessStartInfo("dotnet")
              info.ArgumentList.Add serverDll
              info.RedirectStandardInput <- true
              info.RedirectStandardOutput <- true
              info.RedirectStandardError <- true
              info.UseShellExecute <- false
              let proc = Process.Start info
              let stderr = proc.StandardError.ReadToEndAsync()

              let send (body: string) =
                  let bytes = Encoding.UTF8.GetBytes body
                  let header = Encoding.ASCII.GetBytes $"Content-Length: %d{bytes.Length}\r\n\r\n"
                  let input = proc.StandardInput.BaseStream
                  input.Write(header, 0, header.Length)
                  input.Write(bytes, 0, bytes.Length)
                  input.Flush()

              let out = new MemoryStream()
              let mutable exitCode = -1
              let stdout = proc.StandardOutput.BaseStream

              // a client sends exit after the shutdown response, not before
              let readResponse () =
                  task {
                      let buffer = Array.zeroCreate<byte> 1
                      let header = StringBuilder()

                      while not (header.ToString().EndsWith "\r\n\r\n") do
                          let! n = stdout.ReadAsync(buffer, 0, 1)
                          Expect.equal n 1 "stdout ended inside a header"
                          out.WriteByte buffer[0]
                          header.Append(char buffer[0]) |> ignore

                      let length = int (Regex.Match(header.ToString(), @"Content-Length: (\d+)").Groups[1].Value)
                      let body = Array.zeroCreate<byte> length
                      do! stdout.ReadExactlyAsync(body, 0, length)
                      out.Write(body, 0, length)
                  }

              try
                  send """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":null,"capabilities":{}}}"""
                  do! readResponse () |> withinNoResult
                  send """{"jsonrpc":"2.0","id":2,"method":"shutdown"}"""
                  do! readResponse () |> withinNoResult
                  send """{"jsonrpc":"2.0","method":"exit"}"""
                  do! stdout.CopyToAsync out |> withinNoResult
                  do! proc.WaitForExitAsync() |> withinNoResult
                  let! _ = stderr |> within
                  exitCode <- proc.ExitCode
                  ()
              finally
                  if not proc.HasExited then
                      proc.Kill true

                  proc.Dispose()

              let bytes = out.ToArray()

              // every byte belongs to a Content-Length frame: a stray line would break the walk
              let mutable pos = 0
              let mutable frames = 0

              while pos < bytes.Length do
                  let text = Encoding.ASCII.GetString(bytes, pos, min 64 (bytes.Length - pos))
                  let m = Regex.Match(text, @"^Content-Length: (\d+)\r\n\r\n")
                  Expect.isTrue m.Success $"a frame starts at byte %d{pos}: %s{text.Replace('\n', ' ')}"
                  pos <- pos + m.Length + int m.Groups[1].Value
                  frames <- frames + 1

              Expect.equal pos bytes.Length "the frames end where the output ends"
              Expect.equal frames 2 "the initialize and shutdown responses"
              Expect.equal exitCode 0 "a clean exit"
          }

          testCase "parseArgs maps -v to verbose server startup and --version/--help to immediate exit"
          <| fun () ->
              Expect.equal (Rfs.Server.Program.parseArgs [| "--version" |]) Rfs.Server.Program.CliAction.PrintVersion "--version prints version"
              Expect.equal (Rfs.Server.Program.parseArgs [| "-h" |]) Rfs.Server.Program.CliAction.PrintHelp "-h prints help"
              Expect.equal (Rfs.Server.Program.parseArgs [| "--help" |]) Rfs.Server.Program.CliAction.PrintHelp "--help prints help"

              match
                  Rfs.Server.Program.parseArgs
                      [| "-v"
                         "--adaptive-lsp-server-enabled"
                         "--use-fcs-transparent-compiler"
                         "--project-graph-enabled"
                         "--state-directory"
                         "/tmp/rfs-state"
                         "--logfile"
                         "/tmp/rfs.log"
                         "--filter"
                         "Compiler"
                         "LanguageServer" |]
              with
              | Rfs.Server.Program.CliAction.RunServer(opts, unrecognized) ->
                  Expect.isTrue opts.Verbose "-v enables verbose mode rather than exiting"
                  Expect.equal opts.LogFile (Some "/tmp/rfs.log") "--logfile captures path"
                  Expect.isEmpty unrecognized "FSAC compatibility flags are recognized"
              | other -> failtest $"expected RunServer, got %A{other}"

          testTask "--version and --help exit 0 without blocking on stdin" {
              for flag, expectedPrefix in [ "--version", "ranvierautocomplete "; "--help", "ranvierautocomplete " ] do
                  let info = ProcessStartInfo("dotnet")
                  info.ArgumentList.Add serverDll
                  info.ArgumentList.Add flag
                  info.RedirectStandardInput <- true
                  info.RedirectStandardOutput <- true
                  info.RedirectStandardError <- true
                  info.UseShellExecute <- false
                  let proc = Process.Start info

                  try
                      let stdoutTask = proc.StandardOutput.ReadToEndAsync()
                      let stderrTask = proc.StandardError.ReadToEndAsync()
                      do! proc.WaitForExitAsync() |> withinNoResult
                      let! (stdout: string) = stdoutTask |> within
                      let! _ = stderrTask |> within
                      Expect.equal proc.ExitCode 0 $"{flag} exits 0"
                      Expect.stringStarts (stdout.Trim()) expectedPrefix $"{flag} writes banner to stdout"
                  finally
                      if not proc.HasExited then
                          proc.Kill true

                      proc.Dispose()
          }

          testTask "Ionide FSAC CLI flags start stdio server cleanly without polluting stdout" {
              let tmpDir = Path.Combine(Path.GetTempPath(), $"rfs-cli-test-{Guid.NewGuid():N}")
              Directory.CreateDirectory tmpDir |> ignore
              let logPath = Path.Combine(tmpDir, "ionide.log")

              try
                  let info = ProcessStartInfo("dotnet")
                  info.ArgumentList.Add serverDll
                  info.ArgumentList.Add "-v"
                  info.ArgumentList.Add "--adaptive-lsp-server-enabled"
                  info.ArgumentList.Add "--use-fcs-transparent-compiler"
                  info.ArgumentList.Add "--project-graph-enabled"
                  info.ArgumentList.Add "--state-directory"
                  info.ArgumentList.Add tmpDir
                  info.ArgumentList.Add "--logfile"
                  info.ArgumentList.Add logPath
                  info.RedirectStandardInput <- true
                  info.RedirectStandardOutput <- true
                  info.RedirectStandardError <- true
                  info.UseShellExecute <- false
                  let proc = Process.Start info

                  try
                      let stderrTask = proc.StandardError.ReadToEndAsync()

                      let send (body: string) =
                          let bytes = Encoding.UTF8.GetBytes body
                          let header = Encoding.ASCII.GetBytes $"Content-Length: %d{bytes.Length}\r\n\r\n"
                          let input = proc.StandardInput.BaseStream
                          input.Write(header, 0, header.Length)
                          input.Write(bytes, 0, bytes.Length)
                          input.Flush()

                      let readFrame (stream: Stream) : Task<JsonNode> =
                          task {
                              let buffer = Array.zeroCreate<byte> 1
                              let header = StringBuilder()

                              while not (header.ToString().EndsWith "\r\n\r\n") do
                                  let! n = stream.ReadAsync(buffer, 0, 1)
                                  Expect.equal n 1 "frame header byte"
                                  header.Append(char buffer[0]) |> ignore

                              let length = int (Regex.Match(header.ToString(), @"Content-Length: (\d+)").Groups[1].Value)
                              let body = Array.zeroCreate<byte> length
                              do! stream.ReadExactlyAsync(body, 0, length)
                              return JsonNode.Parse(Encoding.UTF8.GetString body)
                          }

                      send """{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":null,"rootUri":null,"capabilities":{}}}"""
                      let! (initReply: JsonNode) = readFrame proc.StandardOutput.BaseStream |> within
                      let serverInfo = initReply |> at "result" |> at "serverInfo"
                      Expect.equal ((at "name" serverInfo).GetValue<string>()) "ranvierautocomplete" "serverInfo.name"
                      Expect.isNotEmpty ((at "version" serverInfo).GetValue<string>()) "serverInfo.version is populated"

                      send """{"jsonrpc":"2.0","id":2,"method":"shutdown"}"""
                      let! _ = readFrame proc.StandardOutput.BaseStream |> within
                      send """{"jsonrpc":"2.0","method":"exit"}"""
                      do! proc.WaitForExitAsync() |> withinNoResult
                      let! _ = stderrTask |> within
                      Expect.equal proc.ExitCode 0 "clean exit under Ionide CLI flags"
                      Expect.isTrue (File.Exists logPath) "--logfile created diagnostic log"
                  finally
                      if not proc.HasExited then
                          proc.Kill true

                      proc.Dispose()
              finally
                  try
                      Directory.Delete(tmpDir, true)
                  with _ ->
                      ()
          } ]
