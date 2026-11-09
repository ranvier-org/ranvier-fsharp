namespace Rfs.Checker

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Ranvier

/// A parked WhenSettled call. `Step` runs on the host thread and returns true once the call has completed.
[<ReferenceEquality>]
type private Waiter =
    { Step: unit -> bool
      Abort: exn -> unit }

/// <summary>
/// A Ranvier graph with a dedicated owner thread that pumps its inbox.
/// </summary>
/// <remarks>
/// <para>
/// The graph is constructed and activated on the host thread, so that thread is the graph's owner and
/// <c>Graph.Current</c> resolves inside <c>Post</c> and <c>Read</c>. Everything else reaches the graph through
/// <c>Post</c>, <c>Read</c> and <c>WhenSettled</c>. The graph's dispatcher only signals the loop (every off-thread
/// <c>Dispatch</c> and every async memo settling goes through it), and the loop then calls <c>Graph.Pump</c>.
/// </para>
/// <para>
/// An exception thrown by <c>Post</c>ed work is caught, traced, and kept (the newest 256) for
/// <c>TakePostErrors</c>. It is never rethrown, so one failing item cannot fault an unrelated <c>Read</c>. A <c>Read</c>
/// faults its own task. An exception escaping <c>Graph.Pump</c> is kept apart, for <c>TakePumpErrors</c>. Errors from
/// effects and flushes go to Ranvier's <c>Root.Errors</c> and are not surfaced by the host.
/// </para>
/// <para>
/// Once <c>Dispose</c> begins, work still in the inbox does not run: queued <c>Read</c>s fault with
/// <c>ObjectDisposedException</c>, queued <c>Post</c>s are dropped, and parked <c>WhenSettled</c> calls fault.
/// <c>Dispose</c> may be called from the host thread, in which case it does not wait for the loop to finish.
/// </para>
/// </remarks>
[<Sealed; AllowNullLiteral>]
type GraphHost private (name: string) =
    let wake = new ManualResetEventSlim(false)
    let gate = obj ()
    let waiters = List<Waiter>()
    let postErrors = ConcurrentQueue<exn>()
    let pumpErrors = ConcurrentQueue<exn>()
    let mutable closed = false
    let mutable stopping = false
    let mutable graph: Graph = Unchecked.defaultof<Graph>
    let mutable thread: Thread = null
    let mutable startupError: exn = null

    let maxKeptErrors = 256

    let disposedError () = ObjectDisposedException name

    let isStopping () = Volatile.Read &stopping
    let isClosed () = Volatile.Read &closed

    let keep (queue: ConcurrentQueue<exn>) (kind: string) (ex: exn) =
        System.Diagnostics.Trace.WriteLine $"Rfs.Checker.GraphHost %s{name} %s{kind}: %O{ex}"
        queue.Enqueue ex
        let mutable dropped = Unchecked.defaultof<exn>

        while queue.Count > maxKeptErrors && queue.TryDequeue &dropped do
            ()

    let drain (queue: ConcurrentQueue<exn>) =
        let taken = List<exn>()
        let mutable e = Unchecked.defaultof<exn>

        while queue.TryDequeue &e do
            taken.Add e

        taken.ToArray()

    let runWaiters () =
        let snapshot = lock gate (fun () -> waiters.ToArray())

        if snapshot.Length > 0 then
            let finished = snapshot |> Array.filter (fun w -> w.Step())

            if finished.Length > 0 then
                lock gate (fun () ->
                    for w in finished do
                        waiters.Remove w |> ignore)

    let loop (ready: ManualResetEventSlim) () =
        let dispatcher =
            { new IGraphDispatcher with
                member _.Post _ = wake.Set() }

        let mutable activation = Unchecked.defaultof<IDisposable>

        try
            try
                graph <- new Graph({ GraphOptions.Default with Dispatcher = Some dispatcher })
                activation <- graph.Activate()
            with ex ->
                startupError <- ex

            ready.Set()

            if isNull startupError then
                while not (isStopping ()) do
                    wake.Wait()
                    wake.Reset()

                    if not (isStopping ()) then
                        try
                            graph.Pump() |> ignore
                        with ex ->
                            keep pumpErrors "pump" ex

                        // Work run by this pump may have disposed the host; parked reads must then fault, not settle.
                        if not (isStopping ()) then
                            runWaiters ()
        finally
            let parked =
                lock gate (fun () ->
                    Volatile.Write(&closed, true)
                    let all = waiters.ToArray()
                    waiters.Clear()
                    all)

            for w in parked do
                w.Abort(disposedError ())

            if not (isNull (box activation)) then
                activation.Dispose()

            if not (isNull (box graph)) then
                graph.Dispose()

    /// The reserved size of the graph thread's stack: about 500000 rows in a chain, at most.
    static member StackSize: int = 256 * 1024 * 1024

    /// Starts a host whose loop thread is named <c>name</c>.
    static member Start(name: string) : GraphHost =
        let host = new GraphHost(name)
        host.Launch()
        host

    member private this.Launch() =
        use ready = new ManualResetEventSlim(false)
        // Ranvier's notification walks recurse once per dependent, and the rows of a file chain: about 512 bytes a row, so
        // a 4000-row file overflows the default stack and kills the process. FCS guards its own recursion by moving to a
        // new thread (StackGuard), which nothing here does. Only reserved: pages are committed as they are touched.
        thread <- Thread(loop ready, GraphHost.StackSize, IsBackground = true, Name = name)
        thread.Start()
        ready.Wait()

        if not (isNull startupError) then
            thread.Join()
            raise startupError

    /// The graph. Touch it only on the host thread, from inside <c>Post</c>, <c>Read</c> or a <c>WhenSettled</c> read.
    member _.Graph = graph

    /// Runs <c>work</c> on the host thread, after earlier posts from the same caller. Throws if the host is disposed.
    /// A failure of <c>work</c> is kept for <c>TakePostErrors</c>.
    member _.Post(work: unit -> unit) : unit =
        if isClosed () || isStopping () then
            raise (disposedError ())

        graph.Dispatch(
            Action(fun () ->
                if not (isStopping ()) then
                    try
                        work ()
                    with ex ->
                        keep postErrors "post" ex)
        )

    /// Runs <c>f</c> on the host thread. The task faults with <c>f</c>'s exception, or with
    /// <c>ObjectDisposedException</c> when the host is disposed before <c>f</c> runs.
    member _.Read(f: unit -> 'T) : Task<'T> =
        if isClosed () || isStopping () then
            Task.FromException<'T>(disposedError ())
        else
            graph.DispatchAsync(
                Func<'T>(fun () ->
                    if isStopping () then
                        raise (disposedError ())

                    f ())
            )

    /// Re-runs <c>read</c> on the host thread after each pump until it returns <c>ValueSome</c>. A <c>read</c> that
    /// keeps returning <c>ValueNone</c> is not run again until something wakes the loop. A <c>read</c> that throws
    /// <c>NotReadyException</c> (a tracked read of a pending node) counts as <c>ValueNone</c>. The task otherwise faults
    /// with <c>read</c>'s exception, is canceled by <c>cancellationToken</c>, and faults with
    /// <c>ObjectDisposedException</c> if the host is disposed first.
    member this.WhenSettled(read: unit -> 'T voption, cancellationToken: CancellationToken) : Task<'T> =
        let completion =
            TaskCompletionSource<'T>(TaskCreationOptions.RunContinuationsAsynchronously)

        let step () =
            if completion.Task.IsCompleted then
                true
            else
                try
                    match read () with
                    | ValueSome v ->
                        completion.TrySetResult v |> ignore
                        true
                    | ValueNone -> false
                with
                | NotReadyException _ -> false
                | ex ->
                    completion.TrySetException ex |> ignore
                    true

        let waiter =
            { Step = step
              Abort = fun ex -> completion.TrySetException ex |> ignore }

        let registered =
            lock gate (fun () ->
                if isClosed () then
                    false
                else
                    waiters.Add waiter
                    true)

        if not registered then
            completion.TrySetException(disposedError ()) |> ignore
        elif cancellationToken.CanBeCanceled then
            let registration =
                cancellationToken.Register(fun () ->
                    completion.TrySetCanceled cancellationToken |> ignore
                    wake.Set())

            completion.Task.ContinueWith(
                (fun (_: Task) -> registration.Dispose()),
                TaskContinuationOptions.ExecuteSynchronously
            )
            |> ignore

        wake.Set()
        completion.Task

    member this.WhenSettled(read: unit -> 'T voption) : Task<'T> =
        this.WhenSettled(read, CancellationToken.None)

    /// The failures of <c>Post</c>ed work since the last call, oldest first (the newest 256 are kept).
    member _.TakePostErrors() : exn[] = drain postErrors

    /// The failures of graph pumps since the last call, oldest first (the newest 256 are kept).
    member _.TakePumpErrors() : exn[] = drain pumpErrors

    /// Stops the loop, faults what is pending and disposes the graph. Idempotent.
    member _.Dispose() =
        Volatile.Write(&stopping, true)
        wake.Set()

        if not (obj.ReferenceEquals(Thread.CurrentThread, thread)) then
            thread.Join()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
