namespace Rfs.Lsp

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

type Diagnostic =
    { File: string
      Range: Rfs.Lsp.Range
      Severity: int
      Code: string
      Message: string
      Source: string }

/// Hot observable: observers see only what is published after they subscribe, on the client's read-loop thread.
type internal Broadcast<'T>() =
    let gate = obj ()
    let mutable observers: IObserver<'T> list = []

    member _.Publish(value: 'T) =
        for o in lock gate (fun () -> observers) do
            // a throwing observer must not stop the read loop
            try
                o.OnNext value
            with _ ->
                ()

    member _.Complete() =
        for o in lock gate (fun () -> observers) do
            try
                o.OnCompleted()
            with _ ->
                ()

    interface IObservable<'T> with
        member _.Subscribe observer =
            lock gate (fun () -> observers <- observer :: observers)

            { new IDisposable with
                member _.Dispose() =
                    lock gate (fun () -> observers <- observers |> List.filter (fun o -> not (obj.ReferenceEquals(o, observer)))) }

module internal Protocol =
    let private text (node: JsonNode) =
        match node with
        | null -> ""
        | :? JsonValue as v ->
            match v.TryGetValue<string>() with
            | true, s -> s
            | _ -> v.ToJsonString()
        | other -> other.ToJsonString()

    let private position (node: JsonNode) : Position =
        { Line = node["line"].GetValue<int>()
          Character = node["character"].GetValue<int>() }

    let range (node: JsonNode) : Rfs.Lsp.Range =
        { Start = position node["start"]
          End = position node["end"] }

    let toJson (p: Position) =
        let o = JsonObject()
        o["line"] <- JsonValue.Create p.Line
        o["character"] <- JsonValue.Create p.Character
        o

    let rangeToJson (r: Rfs.Lsp.Range) =
        let o = JsonObject()
        o["start"] <- toJson r.Start
        o["end"] <- toJson r.End
        o

    /// One spelling per file: servers emit `file:///c%3A/...` while callers pass `C:\...`, and paths are compared ordinally.
    let normalizePath (path: string) =
        let full = Path.GetFullPath path

        if full.Length >= 2 && full[1] = ':' then
            string (Char.ToUpperInvariant full[0]) + full.Substring 1
        else
            full

    let pathOfUri (uri: string) =
        match Uri.TryCreate(uri, UriKind.Absolute) with
        | true, u when u.IsFile ->
            // LocalPath keeps the leading slash when the colon is percent-encoded (`/c%3A/...`)
            let path = Uri.UnescapeDataString u.AbsolutePath

            if path.Length >= 3 && path[0] = '/' && Char.IsAsciiLetter path[1] && path[2] = ':' then
                normalizePath (path.Substring 1)
            else
                normalizePath u.LocalPath
        | _ -> uri

    let uriOfPath (path: string) = Uri(normalizePath path).AbsoluteUri

    let diagnostic (file: string) (node: JsonNode) : Diagnostic =
        { File = file
          Range = range node["range"]
          Severity =
            match node["severity"] with
            | null -> 0
            | s -> s.GetValue<int>()
          Code = text node["code"]
          Message = text node["message"]
          Source = text node["source"] }

    let obj' (pairs: (string * JsonNode) list) =
        let o = JsonObject()

        for k, v in pairs do
            o[k] <- v

        o

    let str (s: string) : JsonNode = JsonValue.Create s

/// A language-server client. Timestamps are Stopwatch ticks taken when the frame was fully read, so latencies
/// measured from them do not include the client's own JSON parsing.
type LspClient private (connection: JsonRpcConnection, server: Process option) =
    let pending = ConcurrentDictionary<int, TaskCompletionSource<Received>>()
    let documents = ConcurrentDictionary<string, string>()
    let diagnostics = Broadcast<int64 * string * int option * Diagnostic list>()
    let notifications = Broadcast<Received>()
    let mutable nextId = 0
    [<VolatileField>]
    let mutable ended = false
    [<VolatileField>]
    let mutable endedBecause: exn = null

    let closedError () =
        IOException("The server connection is closed", endedBecause)

    let message () =
        let m = JsonObject()
        m["jsonrpc"] <- JsonValue.Create "2.0"
        m

    let notify (method: string) (parameters: JsonNode) =
        let m = message ()
        m["method"] <- JsonValue.Create method

        if not (isNull parameters) then
            m["params"] <- parameters

        connection.Send m

    let send (method: string) (parameters: JsonNode) (timeout: TimeSpan option) : Task<Received * int64> =
        task {
            if ended then
                raise (closedError ())

            let id = Interlocked.Increment &nextId
            let tcs = TaskCompletionSource<Received>(TaskCreationOptions.RunContinuationsAsynchronously)
            pending[id] <- tcs

            try
                // the read loop sets `ended` before failing pending requests, so one of the two sees this entry
                if ended then
                    tcs.TrySetException(closedError ()) |> ignore

                let m = message ()
                m["id"] <- JsonValue.Create id
                m["method"] <- JsonValue.Create method

                if not (isNull parameters) then
                    m["params"] <- parameters

                let sentAt = Stopwatch.GetTimestamp()
                do! connection.Send m

                let! response =
                    match timeout with
                    | Some t -> tcs.Task.WaitAsync t
                    | None -> tcs.Task

                return response, sentAt
            finally
                pending.TryRemove id |> ignore
        }

    /// Server -> client requests are answered with empty results so the server never blocks on us.
    let answer (request: JsonObject) =
        let result: JsonNode =
            match request["method"].GetValue<string>() with
            | "workspace/configuration" ->
                let items =
                    match request["params"] with
                    | :? JsonObject as p ->
                        match p["items"] with
                        | :? JsonArray as a -> a.Count
                        | _ -> 0
                    | _ -> 0

                let result = JsonArray()

                for _ in 1..items do
                    result.Add null

                result
            | _ -> null

        let m = message ()
        m["id"] <- request["id"].DeepClone()
        m["result"] <- result
        connection.Send m

    let handlePublish (received: Received) =
        let parameters = received.Message["params"]
        let file = Protocol.pathOfUri (parameters["uri"].GetValue<string>())

        let version =
            match parameters["version"] with
            | null -> None
            | v -> Some(v.GetValue<int>())

        let list =
            match parameters["diagnostics"] with
            | :? JsonArray as a -> a |> Seq.map (Protocol.diagnostic file) |> List.ofSeq
            | _ -> []

        diagnostics.Publish((received.ReceivedAt, file, version, list))

    /// A frame we could not handle is skipped and reported as a `$/rfs/malformedMessage` notification.
    let surface (received: Received) (error: exn) =
        let m = message ()
        m["method"] <- JsonValue.Create "$/rfs/malformedMessage"

        m["params"] <-
            Protocol.obj' [ "error", Protocol.str error.Message
                            "message", Protocol.str (received.Message.ToJsonString()) ]

        notifications.Publish { Message = m; ReceivedAt = received.ReceivedAt }

    let dispatch (received: Received) : Task =
        task {
            try
                let m = received.Message
                let id = m["id"]
                let method = m["method"]

                if not (isNull method) && not (isNull id) then
                    do! answer m
                elif not (isNull method) then
                    if method.GetValue<string>() = "textDocument/publishDiagnostics" then
                        handlePublish received

                    notifications.Publish received
                elif not (isNull id) then
                    match pending.TryRemove(id.GetValue<int>()) with
                    | true, tcs -> tcs.SetResult received
                    | _ -> ()
            with error ->
                surface received error
        }

    let readLoop =
        Task.Run(fun () ->
            task {
                try
                    try
                        let e = connection.Messages.GetAsyncEnumerator()

                        while! e.MoveNextAsync() do
                            do! dispatch e.Current
                    with error ->
                        endedBecause <- error
                finally
                    ended <- true

                    for tcs in pending.Values do
                        tcs.TrySetException(closedError ()) |> ignore

                    diagnostics.Complete()
                    notifications.Complete()
            }
            :> Task)

    /// Wraps an already-connected pair of streams: `input` is what the server writes, `output` is what it reads.
    /// Used by Start, and directly by tests with in-memory streams.
    static member Connect(input: Stream, output: Stream) : LspClient =
        new LspClient(JsonRpcConnection(input, output), None)

    static member Start(exe: string, args: string[], env: Map<string, string>, cwd: string) : Task<LspClient> =
        task {
            let info = ProcessStartInfo(exe)

            for a in args do
                info.ArgumentList.Add a

            for KeyValue(k, v) in env do
                info.Environment[k] <- v

            info.WorkingDirectory <- cwd
            info.UseShellExecute <- false
            info.CreateNoWindow <- true
            info.RedirectStandardInput <- true
            info.RedirectStandardOutput <- true
            info.RedirectStandardError <- true
            let proc = Process.Start info
            // an undrained stderr pipe would eventually block the server
            proc.ErrorDataReceived.Add ignore
            proc.BeginErrorReadLine()

            return
                new LspClient(JsonRpcConnection(proc.StandardOutput.BaseStream, proc.StandardInput.BaseStream), Some proc)
        }

    /// The server's process id, or -1 for a client made with Connect.
    member _.ProcessId: int =
        match server with
        | Some p -> p.Id
        | None -> -1

    /// Completes with the time from sending `initialize` to its response, then sends `initialized`.
    member _.Initialize(rootUri: string, initOptions: JsonNode) : Task<TimeSpan> =
        task {
            let capabilities =
                JsonNode.Parse
                    """{"textDocument":{"synchronization":{"didSave":true},"publishDiagnostics":{"versionSupport":true}},
                        "workspace":{"configuration":true,"workspaceFolders":true,"didChangeConfiguration":{"dynamicRegistration":true}},
                        "window":{"workDoneProgress":true}}"""

            let parameters =
                Protocol.obj' [ "processId", JsonValue.Create Environment.ProcessId
                                "rootUri", Protocol.str rootUri
                                "capabilities", capabilities
                                "initializationOptions", initOptions
                                "workspaceFolders",
                                JsonArray(Protocol.obj' [ "uri", Protocol.str rootUri; "name", Protocol.str "root" ]) ]

            let! response, sentAt = send "initialize" parameters None

            match response.Message["error"] with
            | null -> ()
            | error -> failwith $"initialize failed: {error.ToJsonString()}"

            do! notify "initialized" (JsonObject())
            return Stopwatch.GetElapsedTime(sentAt, response.ReceivedAt)
        }

    /// The client's current text of a file opened with DidOpen and updated with DidChange.
    member _.Text(path: string) : string = documents[Protocol.normalizePath path]

    /// Opens the file at version 0, so the first DidChange should use version 1.
    member _.DidOpen(path: string, text: string) : Task =
        documents[Protocol.normalizePath path] <- text

        let languageId =
            match Path.GetExtension(path).ToLowerInvariant() with
            | ".fs"
            | ".fsx"
            | ".fsi" -> "fsharp"
            | _ -> "plaintext"

        let document =
            Protocol.obj' [ "uri", Protocol.str (Protocol.uriOfPath path)
                            "languageId", Protocol.str languageId
                            "version", JsonValue.Create 0
                            "text", Protocol.str text ]

        notify "textDocument/didOpen" (Protocol.obj' [ "textDocument", document ])

    /// Sends one incremental change and applies it to the client's copy of the text.
    member _.DidChange(path: string, version: int, edit: Edit) : Task =
        let key = Protocol.normalizePath path
        documents[key] <- Document.apply documents[key] edit

        let change =
            Protocol.obj' [ "range", Protocol.rangeToJson edit.Range; "text", Protocol.str edit.Text ]

        let document =
            Protocol.obj' [ "uri", Protocol.str (Protocol.uriOfPath path); "version", JsonValue.Create version ]

        notify
            "textDocument/didChange"
            (Protocol.obj' [ "textDocument", document; "contentChanges", JsonArray(change) ])

    member _.DidSave(path: string) : Task =
        notify
            "textDocument/didSave"
            (Protocol.obj' [ "textDocument", Protocol.obj' [ "uri", Protocol.str (Protocol.uriOfPath path) ] ])

    /// Returns the response's result (null when the server answered null) and the time from sending the request
    /// to the response's receive timestamp. A JSON-RPC error response raises, as does a closed connection; with
    /// `timeout` the wait is bounded and raises TimeoutException.
    member _.Request(method: string, parameters: JsonNode, ?timeout: TimeSpan) : Task<JsonNode * TimeSpan> =
        task {
            let! response, sentAt = send method parameters timeout
            let m = response.Message

            match m["error"] with
            | null -> ()
            | error -> failwith $"{method} failed: {error.ToJsonString()}"

            let result = m["result"]
            m.Remove "result" |> ignore
            return result, Stopwatch.GetElapsedTime(sentAt, response.ReceivedAt)
        }

    /// Receive timestamp (Stopwatch ticks), file path, document version if the server sent one, diagnostics.
    member _.Diagnostics: IObservable<int64 * string * int option * Diagnostic list> = diagnostics

    /// Every server notification, publishDiagnostics included.
    member _.Notifications: IObservable<Received> = notifications

    /// Sends shutdown and exit, then waits briefly for a spawned process to leave before killing it.
    member _.Shutdown() : Task =
        task {
            let bound = TimeSpan.FromSeconds 5.

            // a server that is already gone or unresponsive must not hang the shutdown
            try
                let! _ = send "shutdown" null (Some bound)
                do! (notify "exit" null).WaitAsync bound
            with
            | :? IOException
            | :? ObjectDisposedException
            | :? TimeoutException -> ()

            match server with
            | Some p ->
                try
                    do! p.WaitForExitAsync().WaitAsync bound
                with :? TimeoutException ->
                    p.Kill true
            | None -> ()

            try
                do! readLoop.WaitAsync bound
            with :? TimeoutException ->
                ()
        }

    interface IDisposable with
        member _.Dispose() =
            match server with
            | Some p when not p.HasExited -> p.Kill true
            | _ -> ()
