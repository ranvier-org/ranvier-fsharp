namespace Rfs.Lsp.Tests

open System
open System.Collections.Concurrent
open System.IO
open System.IO.Pipelines
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Rfs.Lsp

type FakeDiagnostic =
    { DiagLine: int
      DiagStartChar: int
      DiagEndChar: int
      DiagSeverity: int
      DiagCode: string
      DiagMessage: string }

/// An in-process LSP server for tests. Hand ClientInput/ClientOutput to LspClient.Connect:
/// the client reads what the server sends from ClientInput and writes to ClientOutput.
/// Requests without a registered handler are answered with a null result.
type FakeServer() =
    let toClient = Pipe()
    let toServer = Pipe()
    let clientInput = toClient.Reader.AsStream()
    let clientOutput = toServer.Writer.AsStream()
    let connection = JsonRpcConnection(toServer.Reader.AsStream(), toClient.Writer.AsStream())
    let cts = new CancellationTokenSource()
    let received = ConcurrentQueue<JsonObject>()
    let handlers = ConcurrentDictionary<string, JsonNode -> JsonNode>()
    let silent = ConcurrentDictionary<string, unit>()
    let pending = ConcurrentDictionary<string, TaskCompletionSource<JsonNode>>()
    let arrivals = Collections.Generic.List<TaskCompletionSource>()
    let mutable nextId = 0

    let envelope () =
        let message = JsonObject()
        message["jsonrpc"] <- JsonValue.Create "2.0"
        message

    let reply (id: JsonNode) (result: JsonNode) =
        let message = envelope ()
        message["id"] <- id.DeepClone()
        message["result"] <- result
        connection.Send message

    let notifyArrival () =
        lock arrivals (fun () ->
            for a in arrivals do
                a.TrySetResult() |> ignore

            arrivals.Clear())

    let handle (message: JsonObject) : Task =
        task {
            match message["method"], message["id"] with
            | null, null -> ()
            | null, id ->
                // a reply to a server -> client request
                match pending.TryRemove(id.ToJsonString()) with
                | true, tcs -> tcs.SetResult(message["result"])
                | _ -> ()
            | _, null -> ()
            | method, id ->
                let name = method.GetValue<string>()

                if not (silent.ContainsKey name) then
                    let result =
                        match handlers.TryGetValue name with
                        | true, h -> h message["params"]
                        | _ when name = "initialize" -> JsonNode.Parse """{"capabilities":{"textDocumentSync":2}}"""
                        | _ -> null

                    do! reply id result
        }

    let loop =
        Task.Run(fun () ->
            task {
                try
                    let e = connection.Messages.GetAsyncEnumerator cts.Token

                    while! e.MoveNextAsync() do
                        let message = e.Current.Message
                        received.Enqueue message
                        notifyArrival ()
                        do! handle message
                with
                | :? OperationCanceledException -> ()
                | :? ObjectDisposedException -> ()
                | :? InvalidOperationException -> ()
            }
            :> Task)

    /// The stream the client should read from.
    member _.ClientInput: Stream = clientInput
    /// The stream the client should write to.
    member _.ClientOutput: Stream = clientOutput

    /// Every message received from the client so far, in order.
    member _.Received: JsonObject list = received.ToArray() |> List.ofArray

    /// Answers requests for `method` with the handler's result (the handler gets the request params).
    member _.OnRequest(method: string, handler: JsonNode -> JsonNode) = handlers[method] <- handler

    /// Requests for `method` get no reply at all.
    member _.NeverAnswer(method: string) = silent[method] <- ()

    /// Waits for a client message satisfying `predicate`, including ones that already arrived.
    member _.WaitFor(predicate: JsonObject -> bool) : Task<JsonObject> =
        task {
            use timeout = new CancellationTokenSource(TimeSpan.FromSeconds 10.)
            let mutable found = None

            while found.IsNone do
                let waiter = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
                lock arrivals (fun () -> arrivals.Add waiter)
                found <- received.ToArray() |> Array.tryFind predicate

                if found.IsNone then
                    do! waiter.Task.WaitAsync timeout.Token

            return found.Value
        }

    member this.WaitForMethod(method: string) : Task<JsonObject> =
        this.WaitFor(fun m -> not (isNull m["method"]) && m["method"].GetValue<string>() = method)

    member _.Notify(method: string, parameters: JsonNode) : Task =
        let message = envelope ()
        message["method"] <- JsonValue.Create method
        message["params"] <- parameters
        connection.Send message

    /// Sends a server -> client request and returns the client's result once it replies.
    member _.Request(method: string, parameters: JsonNode) : Task<JsonNode> =
        task {
            let id = Interlocked.Increment &nextId
            let tcs = TaskCompletionSource<JsonNode>(TaskCreationOptions.RunContinuationsAsynchronously)
            pending[string id] <- tcs
            let message = envelope ()
            message["id"] <- JsonValue.Create id
            message["method"] <- JsonValue.Create method
            message["params"] <- parameters
            do! connection.Send message
            return! tcs.Task.WaitAsync(TimeSpan.FromSeconds 10.)
        }

    /// Publishes diagnostics for `path`; `version` is omitted from the notification when None.
    member this.PublishDiagnostics(path: string, version: int option, diagnostics: FakeDiagnostic list) : Task =
        let items = JsonArray()

        for d in diagnostics do
            let position character =
                let p = JsonObject()
                p["line"] <- JsonValue.Create d.DiagLine
                p["character"] <- JsonValue.Create(character: int)
                p

            let range = JsonObject()
            range["start"] <- position d.DiagStartChar
            range["end"] <- position d.DiagEndChar
            let item = JsonObject()
            item["range"] <- range
            item["severity"] <- JsonValue.Create d.DiagSeverity
            item["code"] <- JsonValue.Create d.DiagCode
            item["source"] <- JsonValue.Create "fake"
            item["message"] <- JsonValue.Create d.DiagMessage
            items.Add item

        let parameters = JsonObject()
        parameters["uri"] <- JsonValue.Create(Uri(path).AbsoluteUri)

        match version with
        | Some v -> parameters["version"] <- JsonValue.Create v
        | None -> ()

        parameters["diagnostics"] <- items
        this.Notify("textDocument/publishDiagnostics", parameters)

    interface IDisposable with
        member _.Dispose() =
            cts.Cancel()
            toClient.Writer.Complete()
            toServer.Writer.Complete()
            toServer.Reader.Complete()
            toClient.Reader.Complete()
