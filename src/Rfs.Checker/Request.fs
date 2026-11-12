namespace Rfs.Checker

open System
open System.Threading
open System.Threading.Tasks
open Ranvier

/// <summary>
/// A request as a node on a host's graph: a transient root holding an async memo, and an effect that completes the
/// request's task with the memo's first value.
/// </summary>
/// <remarks>
/// <para>
/// The memo's body runs on the graph thread, and reads every node it depends on there, tracked, before its first await.
/// It waits by being pending: a pending read raises NotReadyException, and a body that finds a guard failing returns
/// <c>Request.waitForNextTurn</c>. Either way a change to what it read runs it again, and under CancelPrevious that
/// cancels the flight in progress, so a body never retries by hand.
/// </para>
/// <para>
/// A failed memo (a node it read faulted, or its flight threw) fails the task with that exception and nothing else: a
/// request reads only its own nodes, so a fault in one project fails no request on another. Cancelling the request's
/// token disposes its root, which cancels its flight and ends its wait, and nothing more: what it read is shared, and
/// goes on. The root is also disposed once the task has completed.
/// </para>
/// </remarks>
module Request =
    /// A flight that never completes: the body found a guard failing, and waits for a change of what it read to run it
    /// again, which cancels this flight.
    let waitForNextTurn<'T> (flight: CancellationToken) : Task<'T> =
        task {
            do! Task.Delay(Timeout.Infinite, flight)
            return Unchecked.defaultof<'T>
        }

    /// <summary>
    /// Runs <c>read</c> holding a lock that a check takes to change what it reads (a FileStart's), once
    /// <c>inEffect</c> says, under the lock, that what the body read is still in effect. When it isn't, the lock is
    /// released and the flight waits for the next turn: what changed is what the body read, which runs it again.
    /// </summary>
    let live (acquire: CancellationToken -> Task<IDisposable>) (inEffect: unit -> Task<bool>) (read: unit -> 'T) (flight: CancellationToken) : Task<'T> =
        task {
            let! held = acquire flight
            let mutable answer = ValueNone

            try
                let! still = inEffect ()

                if still then
                    answer <- ValueSome(read ())
            finally
                held.Dispose()

            match answer with
            | ValueSome v -> return v
            | ValueNone -> return! waitForNextTurn flight
        }

    /// <summary>
    /// Runs a request on <c>host</c>: a root of its own with an async memo (CancelPrevious) whose body is <c>body</c>,
    /// and an effect that completes the task with the memo's value, or fails it with the memo's error. Cancelling
    /// <c>ct</c> cancels the task and disposes the root. Never call it from the graph thread.
    /// </summary>
    let run (host: GraphHost) (body: CancellationToken -> Task<'T>) (ct: CancellationToken) : Task<'T> =
        let answer = TaskCompletionSource<'T>(TaskCreationOptions.RunContinuationsAsynchronously)
        // Graph thread only
        let root = ref ValueNone

        let mutable closePosted = 0

        let close () =
            match root.Value with
            | ValueSome(r: Owner) ->
                root.Value <- ValueNone
                r.Dispose()
            | ValueNone -> ()

        let postClose () =
            if Interlocked.Exchange(&closePosted, 1) = 0 then
                try
                    host.Post close
                with :? ObjectDisposedException ->
                    ()

        if ct.IsCancellationRequested then
            answer.TrySetCanceled ct |> ignore
        else
            try
                host.Post(fun () ->
                    if not answer.Task.IsCompleted then
                        root.Value <-
                            createRoot (fun owner ->
                                let flight = createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ token -> body token)

                                createEffect (fun () ->
                                    match flight.TryValue with
                                    | Ready v ->
                                        postClose ()
                                        answer.TrySetResult v |> ignore
                                    | Failed e ->
                                        postClose ()
                                        answer.TrySetException e |> ignore
                                    | Pending -> ())

                                ValueSome owner)

                        // The effect may have answered at once
                        if answer.Task.IsCompleted then
                            close ())

                let registration =
                    ct.Register(fun () ->
                        postClose ()
                        answer.TrySetCanceled ct |> ignore)

                answer.Task.ContinueWith(
                    (fun (_: Task<'T>) ->
                        registration.Dispose()
                        postClose ()),
                    TaskContinuationOptions.ExecuteSynchronously
                )
                |> ignore
            with :? ObjectDisposedException as e ->
                answer.TrySetException e |> ignore

        answer.Task
