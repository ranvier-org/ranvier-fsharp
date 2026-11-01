namespace Rfs.Lsp

open System
open System.Buffers
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Pipelines
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

/// A decoded frame; ReceivedAt is a Stopwatch tick count taken once the body was complete, before parsing.
type Received =
    { Message: JsonObject
      ReceivedAt: int64 }

module internal Framing =
    let headerEnd = "\r\n\r\n"B

    let private writerOptions =
        JsonWriterOptions(Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

    let encode (message: JsonObject) : byte[] =
        use buffer = new MemoryStream()

        do
            use writer = new Utf8JsonWriter(buffer, writerOptions)
            message.WriteTo writer

        buffer.ToArray()

    let private contentLength (headers: string) =
        headers.Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.tryPick (fun line ->
            match line.IndexOf ':' with
            | -1 -> None
            | colon when line.AsSpan(0, colon).Trim().Equals("Content-Length", StringComparison.OrdinalIgnoreCase) ->
                match Int32.TryParse(line.AsSpan(colon + 1).Trim()) with
                | true, n when n >= 0 -> Some n
                | _ -> raise (InvalidDataException $"Invalid Content-Length header: {line}")
            | _ -> None)
        |> Option.defaultWith (fun () -> raise (InvalidDataException "Frame has no Content-Length header"))

    /// Tries to decode one frame from the front of the buffer. Returns the frame and the position after it.
    let tryDecode (buffer: ReadOnlySequence<byte>) : struct (Received * SequencePosition) voption =
        let mutable reader = SequenceReader<byte>(buffer)
        let mutable headers = ReadOnlySequence<byte>.Empty

        if not (reader.TryReadTo(&headers, ReadOnlySpan<byte> headerEnd, true)) then
            ValueNone
        else
            let length = contentLength (Encoding.ASCII.GetString(headers.ToArray()))

            if reader.Remaining < int64 length then
                ValueNone
            else
                let body = buffer.Slice(reader.Position, int64 length)
                let receivedAt = Stopwatch.GetTimestamp()

                match JsonNode.Parse(body.ToArray()) with
                | :? JsonObject as message ->
                    ValueSome(struct ({ Message = message; ReceivedAt = receivedAt }, body.End))
                | _ -> raise (InvalidDataException "JSON-RPC frame body is not a JSON object")

type private FrameEnumerator(reader: PipeReader, ct: CancellationToken) =
    let mutable current = Unchecked.defaultof<Received>
    let mutable finished = false

    let rec next () : Task<bool> =
        task {
            if finished then
                return false
            else
                let! result = reader.ReadAsync ct
                let buffer = result.Buffer

                match Framing.tryDecode buffer with
                | ValueSome(struct (frame, after)) ->
                    reader.AdvanceTo after
                    current <- frame
                    return true
                | ValueNone when result.IsCompleted ->
                    reader.AdvanceTo(buffer.Start, buffer.End)
                    finished <- true

                    if buffer.Length > 0L then
                        raise (EndOfStreamException "Stream ended inside a frame")

                    return false
                | ValueNone ->
                    reader.AdvanceTo(buffer.Start, buffer.End)
                    return! next ()
        }

    interface IAsyncEnumerator<Received> with
        member _.Current = current
        member _.MoveNextAsync() = ValueTask<bool>(next ())
        member _.DisposeAsync() = ValueTask()

/// LSP base-protocol framing (Content-Length headers) over a pair of streams. Messages can be enumerated once.
type JsonRpcConnection(input: Stream, output: Stream) =
    let reader = PipeReader.Create(input, StreamPipeReaderOptions(leaveOpen = true))
    let writer = PipeWriter.Create(output, StreamPipeWriterOptions(leaveOpen = true))
    let sendLock = new SemaphoreSlim(1, 1)

    member _.Send(message: JsonObject) : Task =
        task {
            let body = Framing.encode message
            let header = Encoding.ASCII.GetBytes $"Content-Length: {body.Length}\r\n\r\n"
            let frame = Array.append header body
            do! sendLock.WaitAsync()

            try
                let! _ = writer.WriteAsync(ReadOnlyMemory frame)
                ()
            finally
                sendLock.Release() |> ignore
        }

    member _.Messages: IAsyncEnumerable<Received> =
        { new IAsyncEnumerable<Received> with
            member _.GetAsyncEnumerator ct = FrameEnumerator(reader, ct) :> IAsyncEnumerator<Received> }
