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
      Abort: exn -> unit
      /// Ends with its own faults only (a project's), never with the host's
      Apart: bool }

/// Work on the graph thread failed, so what a parked wait is waiting for may never come.
type GraphFailedException(what: string, inner: exn) =
    inherit Exception($"%s{what} failed on the graph thread: %s{inner.Message}", inner)

/// Failures a project has claimed: its own waits end with them (ProjectGraph's fault count), so the host's apart waits
/// need not. Any failure not claimed may have stranded any wait, and ends them all.
module internal GraphClaims =
    let private claimed = System.Runtime.CompilerServices.ConditionalWeakTable<exn, obj>()

    let claim (ex: exn) = claimed.AddOrUpdate(ex, null)

    /// Whether `ex` itself was claimed. What it wraps doesn't count: a failure that wraps a project's may have left work
    /// of its own undone. The host is handed the claimed exception itself (a guarded effect's report, a post's reraise).
    let isClaimed (ex: exn) = not (isNull ex) && fst (claimed.TryGetValue ex)

[<AbstractClass; Sealed>]
type internal GraphThread private () =
    /// Tells this thread's host that an effect failed
    [<DefaultValue; ThreadStatic>]
    static val mutable private report: exn -> unit

    static member Report
        with get () = GraphThread.report
        and set (v: exn -> unit) = GraphThread.report <- v

module internal GraphEffects =
    /// An effect whose failure reaches its host. Ranvier keeps a failed effect's exception on the effect, where nothing
    /// reads it, and a pending read is no failure.
    let guarded (body: unit -> unit) =
        createEffect (fun () ->
            try
                body ()
            with
            | NotReadyException _ -> reraise ()
            | ex ->
                GraphThread.Report ex
                reraise ())

    /// A guarded effect whose failure also reaches `owner` (its project's waits)
    let guardedFor (owner: exn -> unit) (body: unit -> unit) =
        createEffect (fun () ->
            try
                body ()
            with
            | NotReadyException _ -> reraise ()
            | ex ->
                GraphClaims.claim ex
                owner ex
                GraphThread.Report ex
                reraise ())

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
/// An exception thrown by <c>Post</c>ed work is caught, written to stderr, and kept (the newest 256) for
/// <c>TakePostErrors</c>. It is never rethrown, so one failing item cannot fault an unrelated <c>Read</c>. A <c>Read</c>
/// faults its own task. An exception escaping <c>Graph.Pump</c> is kept apart, for <c>TakePumpErrors</c>. What Ranvier
/// records in <c>Root.Errors</c> is written to stderr after the pump that recorded it, and so is the failure of an effect
/// made with <c>GraphEffects.guarded</c>.
/// </para>
/// <para>
/// After any of these, the <c>WhenSettled</c> calls still parked once the waiters have run fault with
/// <c>GraphFailedException</c>: what the failure left undone may be what they wait for, and nothing else would end
/// them. Later calls wait as usual. A call made apart (a project's wait, which checks its own project's faults) is ended
/// only by a failure no project claimed (GraphClaims): the host is shared, and a fault one project claimed must fail
/// nothing of another's.
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
    // For tests: thrown by the next pump in place of its work
    let mutable nextPumpFails: exn = null

    let maxKeptErrors = 256

    let disposedError () = ObjectDisposedException name

    let isStopping () = Volatile.Read &stopping
    let isClosed () = Volatile.Read &closed

    // Graph thread only: set while a pump runs, taken after it. `unclaimed`: a failure no project claimed was among them
    let mutable failure: exn = null
    let mutable unclaimed = false
    let mutable rootErrorsSeen = 0

    let failed (kind: string) (ex: exn) =
        eprintfn "[ranvierautocomplete] graph %s, %s failed: %O" name kind ex
        failure <- GraphFailedException(kind, ex)

        if not (GraphClaims.isClaimed ex) then
            unclaimed <- true

    let keep (queue: ConcurrentQueue<exn>) (kind: string) (ex: exn) =
        failed kind ex
        queue.Enqueue ex
        let mutable dropped = Unchecked.defaultof<exn>

        while queue.Count > maxKeptErrors && queue.TryDequeue &dropped do
            ()

    let noteRootErrors () =
        for ex in graph.Root.ErrorsFrom rootErrorsSeen do
            rootErrorsSeen <- rootErrorsSeen + 1
            failed "a graph scope" ex

    /// Ends the waits a failure may have stranded. The waiters have just run: what could still settle has. A failure a
    /// project claimed ends the shared waits only, as that project's own waits end with its fault count; one no project
    /// claimed (a pump that stopped mid-flush, a scope's error, someone else's post) ends the apart ones too, but only
    /// those the waiters just ran (`ran`): one registered since has not been tried, and gets its turn next pass.
    let failParked (ran: HashSet<Waiter>) =
        if not (isNull failure) then
            let ex = failure
            let all = unclaimed
            failure <- null
            unclaimed <- false

            let parked =
                lock gate (fun () ->
                    let ends (w: Waiter) = not w.Apart || (all && ran.Contains w)
                    let ended = waiters.FindAll(ends).ToArray()
                    waiters.RemoveAll(ends) |> ignore
                    ended)

            for w in parked do
                w.Abort ex

    let drain (queue: ConcurrentQueue<exn>) =
        let taken = List<exn>()
        let mutable e = Unchecked.defaultof<exn>

        while queue.TryDequeue &e do
            taken.Add e

        taken.ToArray()

    /// Runs every parked waiter once; returns those it ran
    let runWaiters () : HashSet<Waiter> =
        let snapshot = lock gate (fun () -> waiters.ToArray())

        if snapshot.Length > 0 then
            let finished = snapshot |> Array.filter (fun w -> w.Step())

            if finished.Length > 0 then
                lock gate (fun () ->
                    for w in finished do
                        waiters.Remove w |> ignore)

        HashSet<Waiter>(snapshot, HashIdentity.Reference)

    let loop (ready: ManualResetEventSlim) () =
        let dispatcher =
            { new IGraphDispatcher with
                member _.Post _ = wake.Set() }

        let mutable activation = Unchecked.defaultof<IDisposable>
        GraphThread.Report <- failed "an effect"

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
                            match Interlocked.Exchange(&nextPumpFails, null) with
                            | null -> graph.Pump() |> ignore
                            | ex -> raise ex
                        with ex ->
                            keep pumpErrors "a pump" ex

                        // Work run by this pump may have disposed the host; parked reads must then fault, not settle.
                        if not (isStopping ()) then
                            noteRootErrors ()
                            let ran = runWaiters ()
                            failParked ran
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

    /// The reserved size of the graph thread's stack. Several hundred thousand rows in a chain is an estimate from where
    /// the default stack overflowed; only 4000 rows have been run.
    static member StackSize: int = 256 * 1024 * 1024

    /// Starts a host whose loop thread is named <c>name</c>.
    static member Start(name: string) : GraphHost =
        let host = new GraphHost(name)
        host.Launch()
        host

    member private this.Launch() =
        use ready = new ManualResetEventSlim(false)
        // Ranvier's notification walks recurse once per dependent, and the rows of a file chain: a few hundred bytes a row, so
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
                        keep postErrors "posted work" ex)
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
        this.WhenSettled(read, cancellationToken, false)

    /// As the other overloads; `apart` keeps the call out of the host's faults: it ends only by `read` (which checks
    /// the faults it depends on), cancellation or disposal.
    member _.WhenSettled(read: unit -> 'T voption, cancellationToken: CancellationToken, apart: bool) : Task<'T> =
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
              Abort = fun ex -> completion.TrySetException ex |> ignore
              Apart = apart }

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

    /// Makes the next pump throw `ex` instead of running, as a pump that stops mid-flush does. For tests.
    member internal _.FailNextPump(ex: exn) : unit =
        Volatile.Write(&nextPumpFails, ex)
        wake.Set()

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
