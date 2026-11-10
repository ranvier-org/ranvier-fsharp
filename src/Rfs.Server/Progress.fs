namespace Rfs.Server

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open Ionide.LanguageServerProtocol.Types
open Ranvier
open Rfs.Checker

/// What the status bar's progress says: nothing, or what is pending
type ProgressStatus =
    | Settled
    | Busy of what: string

/// Where a period's sends are. Only the outbox's consumer reads or writes it.
type internal PeriodState =
    | Creating
    | Begun
    | Ended

/// One period of the progress: its token, its state and what it says. Only the outbox's consumer reads or writes them.
type internal Period(token: string, what: string) =
    member _.Token = token
    member val State = Creating with get, set
    member val Message = what with get, set

/// <summary>
/// One global work-done progress, created by the server, for as long as anything is pending: a project loading, a check
/// under way for a row of the Publisher, or a project whose restart failed (named until it is reloaded).
/// </summary>
/// <remarks>
/// <para>
/// The status is one memo on the host's graph. Its effect notes what is wanted and when; the progress follows once the
/// wanted status has held for <c>BeginHold</c> (to begin, or to say something else) or <c>EndHold</c> (to end), so a
/// shorter flip sends nothing. A period's title is fixed at its begin; what is pending is its message, and a change of
/// it is a report in the same period.
/// </para>
/// <para>
/// The sends go on the Publisher's outbox, in order with its lists, and none of them waits for the client. The create
/// is sent and its item returns; once the client answers it, the begin is queued as an item of its own. The period's
/// state decides what is sent: a begin only while it is creating, a report or an end only once begun. An end that comes
/// first ends the period unbegun, and an error answer, or none within <c>CreateTimeout</c>, leaves it so.
/// </para>
/// </remarks>
type internal Progress(host: GraphHost, workspace: Workspace, publisher: Publisher, client: RanvierClient, beginHold: TimeSpan) =
    // Graph thread
    let mutable status: Memo<ProgressStatus> = Unchecked.defaultof<_>
    let mutable root: Owner = Unchecked.defaultof<_>
    let mutable wanted = Settled
    let mutable wantedSince = 0L
    let mutable shown = Settled
    let mutable periods = 0
    let mutable changes = 0
    let mutable closed = false
    let mutable period: Period option = None
    let mutable timer: Timer = null
    // 1 once a period's begin is written to the client, until its end is about to be: the mark is on screen
    let mutable showing = 0
    // Completes when the mark shows; replaced when it goes
    let mutable markShown = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)

    let log (text: string) = eprintfn "[ranvierautocomplete] %s" text

    let progress (p: Period) (value: JObject) : Task =
        let body = JObject(JProperty("token", p.Token), JProperty("value", value))
        client.Notify("$/progress", box body) |> Async.StartAsTask :> Task

    let statusNow () =
        match workspace.DeadNow() with
        | _ :: _ as dead -> Busy("Restart failed: " + String.Join(", ", dead |> List.map (snd >> Path.GetFileName)))
        | [] ->
            if workspace.LoadingNow() then Busy "Loading projects"
            elif publisher.CheckingNow() then Busy "Checking"
            else Settled

    /// Outbox's consumer
    let beginPeriod (p: Period) : Task =
        if p.State = Creating then
            p.State <- Begun

            (progress p (JObject(JProperty("kind", "begin"), JProperty("title", Progress.Title), JProperty("message", p.Message))))
                .ContinueWith(fun (sent: Task) ->
                    if not sent.IsCompletedSuccessfully then
                        log $"sending the progress's begin failed: %O{sent.Exception}"
                    elif p.State = Begun then
                        Interlocked.Exchange(&showing, 1) |> ignore
                        Volatile.Read(&markShown).TrySetResult() |> ignore)
        else
            Task.CompletedTask

    /// Graph thread
    let start (what: string) =
        periods <- periods + 1
        let p = Period($"rfs/progress/%d{periods}", what)
        period <- Some p

        publisher.Enqueue(
            Notify(fun () ->
                (client.CreateProgress p.Token |> Async.StartAsTask)
                    .WaitAsync(Progress.CreateTimeout)
                    .ContinueWith(fun (answer: Task<Result<unit, Ionide.LanguageServerProtocol.JsonRpc.Error>>) ->
                        if answer.IsCompletedSuccessfully then
                            match answer.Result with
                            | Ok() -> publisher.Enqueue(Notify(fun () -> beginPeriod p))
                            | Error e -> log $"the client refused the progress token: %s{e.Message}"
                        else
                            log $"creating the progress token failed: %O{answer.Exception}")
                |> ignore

                Task.CompletedTask)
        )

    /// Graph thread
    let report (what: string) =
        match period with
        | Some p ->
            publisher.Enqueue(
                Notify(fun () ->
                    p.Message <- what

                    if p.State = Begun then
                        progress p (JObject(JProperty("kind", "report"), JProperty("message", what)))
                    else
                        Task.CompletedTask)
            )
        | None -> ()

    /// Graph thread
    let finish () =
        match period with
        | Some p ->
            period <- None

            publisher.Enqueue(
                Notify(fun () ->
                    let was = p.State
                    p.State <- Ended
                    if Interlocked.Exchange(&showing, 0) = 1 then
                        Volatile.Write(&markShown, TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously))

                    if was = Begun then
                        progress p (JObject(JProperty("kind", "end")))
                    else
                        Task.CompletedTask)
            )
        | None -> ()

    let holdFor (s: ProgressStatus) =
        match s with
        | Busy _ -> beginHold
        | Settled -> Progress.EndHold

    /// Graph thread: once one more change is wanted, after `after`. The one timer is re-armed, so a burst leaves one.
    let wake (after: TimeSpan) =
        if not closed then
            timer.Change(after, Timeout.InfiniteTimeSpan) |> ignore

    /// Graph thread: makes the progress follow the wanted status once it has held long enough, else wakes when it will
    /// have
    let follow () =
        if not closed && wanted <> shown then
            let need = holdFor wanted
            let held = Stopwatch.GetElapsedTime wantedSince

            if held >= need then
                match shown, wanted with
                | Settled, Busy what -> start what
                | Busy _, Busy what -> report what
                | Busy _, Settled -> finish ()
                | Settled, Settled -> ()

                shown <- wanted
            else
                wake (need - held)

    do
        timer <-
            new Timer(fun _ ->
                try
                    host.Post follow
                with :? ObjectDisposedException ->
                    ())

        host
            .Read(fun () ->
                root <-
                    createRoot (fun owner ->
                        status <- createMemo (fun _ -> statusNow ())

                        createEffect (fun () ->
                            let now = status.Value

                            if now <> wanted then
                                wanted <- now
                                wantedSince <- Stopwatch.GetTimestamp()
                                changes <- changes + 1
                                wake (holdFor now))

                        owner))
            .Wait()

    /// The title of every period; its message says what is pending
    static member Title: string = "F#"

    /// How long something must have been pending before the progress begins, or says something else: the default
    static member BeginHold: TimeSpan = TimeSpan.FromMilliseconds 100.

    new(host, workspace, publisher, client) = new Progress(host, workspace, publisher, client, Progress.BeginHold)

    /// How long everything must have settled before the progress ends
    static member EndHold: TimeSpan = TimeSpan.FromMilliseconds 100.

    /// How long the client may take to answer a create before its period is given up
    static member CreateTimeout: TimeSpan = TimeSpan.FromSeconds 10.

    /// Whether a period's begin has reached the client and its end hasn't been sent: what marks an answer from the last
    /// state. Any thread.
    member _.ShowingNow() : bool = Volatile.Read &showing = 1

    /// Completes once the mark shows (at once while it does). Any thread.
    member _.WhenShown() : Task = Volatile.Read(&markShown).Task

    /// What the progress follows now. For tests.
    member _.Status() : Task<ProgressStatus> = host.Read(fun () -> status.Value)

    /// How many times the wanted status changed. For tests.
    member _.Changes() : Task<int> = host.Read(fun () -> changes)

    /// Ends a period that is shown, on the outbox before it closes, and begins nothing more. Call it before the
    /// Publisher's Dispose.
    member _.Dispose() =
        try
            host
                .Read(fun () ->
                    closed <- true
                    timer.Dispose()

                    match shown with
                    | Busy _ -> finish ()
                    | Settled -> ()

                    shown <- Settled
                    root.Dispose())
                .Wait(TimeSpan.FromSeconds 5.)
            |> ignore
        with
        | :? ObjectDisposedException
        | :? AggregateException -> ()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

/// <summary>
/// One <c>window/showMessage</c> (Warning) for each project whose restart failed, naming it, whatever the client's
/// capabilities: the progress says so too, but only to a client that declared it. Once per graph: a reload makes a new
/// one.
/// </summary>
type internal RestartNotices(host: GraphHost, workspace: Workspace, publisher: Publisher, client: RanvierClient) =
    // Graph thread
    let mutable root: Owner = Unchecked.defaultof<_>
    let mutable told: ProjectGraph list = []

    do
        host
            .Read(fun () ->
                root <-
                    createRoot (fun owner ->
                        createEffect (fun () ->
                            let dead = workspace.DeadNow()

                            for graph, project in dead do
                                if not (told |> List.exists (fun g -> obj.ReferenceEquals(g, graph))) then
                                    let text =
                                        $"%s{Path.GetFileName project} could not be checked again after a reference changed. Reload the project to check it."

                                    publisher.Enqueue(
                                        Notify(fun () ->
                                            client.WindowShowMessage { Type = MessageType.Warning; Message = text }
                                            |> Async.StartAsTask
                                            :> Task)
                                    )

                            told <- dead |> List.map fst)

                        owner))
            .Wait()

    member _.Dispose() =
        try
            host.Post(fun () -> root.Dispose())
        with :? ObjectDisposedException ->
            ()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
