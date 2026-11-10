namespace Rfs.Server

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks

/// What the Outbox sends
type internal Outgoing =
    /// A file's whole list. `hooked` is the LSP version of a file's own list, which the test hook runs on.
    | Publish of path: string * hooked: int option * send: (unit -> Task)
    | Notify of send: (unit -> Task)
    | Marker of TaskCompletionSource

/// <summary>
/// The server's sends to the client, sent one at a time by one consumer in the order they were queued. Effects on the
/// graph thread queue them, as an effect can't await a send.
/// </summary>
/// <remarks>
/// A list queued for a file is dropped, unsent, when a later list for the same file is queued before it is sent: each
/// list is the file's whole list, so the client would keep only the later one. Lists for a file therefore reach the
/// client in the order they were queued, and so do all sends across files.
/// </remarks>
type internal Outbox() =
    let channel = Channel.CreateUnbounded<struct (int64 * Outgoing)>(UnboundedChannelOptions(SingleReader = true))
    let latest = ConcurrentDictionary<string, int64>(StringComparer.OrdinalIgnoreCase)
    let mutable sequence = 0L
    let mutable beforePublish: int -> Task = fun _ -> Task.CompletedTask

    let log (text: string) = eprintfn "[ranvierautocomplete] %s" text

    let superseded (path: string) (n: int64) =
        match latest.TryGetValue path with
        | true, last -> last > n
        | _ -> false

    let consumer =
        Task.Run(fun () ->
            task {
                try
                    while! channel.Reader.WaitToReadAsync() do
                        let mutable more = true

                        while more do
                            match channel.Reader.TryRead() with
                            | true, struct (n, item) ->
                                try
                                    match item with
                                    | Publish(path, hooked, send) ->
                                        if not (superseded path n) then
                                            match hooked with
                                            | Some lsp -> do! beforePublish lsp
                                            | None -> ()

                                            if not (superseded path n) then
                                                do! send ()
                                    | Notify send -> do! send ()
                                    | Marker done' -> done'.TrySetResult() |> ignore
                                with ex ->
                                    log $"publishing failed: %O{ex}"
                            | _ -> more <- false
                with ex ->
                    log $"publishing stopped: %O{ex}"
            }
            :> Task)

    /// Queues `item` behind everything queued before it. Dropped once the outbox is disposed.
    member _.Enqueue(item: Outgoing) : unit =
        let n = Interlocked.Increment &sequence

        match item with
        | Publish(path, _, _) -> latest[path] <- n
        | _ -> ()

        channel.Writer.TryWrite(struct (n, item)) |> ignore

    /// Completes once everything queued before it is sent or dropped
    member this.Flushed() : Task =
        let done' = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)

        if channel.Writer.TryWrite(struct (Interlocked.Increment &sequence, Marker done')) then
            done'.Task
        else
            Task.FromException(ObjectDisposedException(nameof Outbox))

    /// Runs `hook` with the LSP version of a file's own list before it is sent, holding the sends behind it. For tests.
    member _.BeforePublish(hook: int -> Task) : unit = beforePublish <- hook

    member _.Dispose() = channel.Writer.TryComplete() |> ignore

    interface IDisposable with
        member this.Dispose() = this.Dispose()
