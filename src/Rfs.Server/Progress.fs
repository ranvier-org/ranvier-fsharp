namespace Rfs.Server

open System
open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open Ranvier
open Rfs.Checker

/// What the status bar's progress says: nothing, or what is pending
type ProgressStatus =
    | Settled
    | Busy of title: string

/// <summary>
/// One global work-done progress, created by the server, for as long as anything is pending: a project loading, a check
/// under way for a row of the Publisher, or a project whose restart failed (named in the title until it is reloaded).
/// </summary>
/// <remarks>
/// The status is one memo on the host's graph. Its effect notes what is wanted and when; the progress follows once the
/// wanted status has held for <c>BeginHold</c> (to begin, or change title) or <c>EndHold</c> (to end), so a shorter flip
/// sends nothing. A title change ends the progress and begins one with the new title. The sends go on the Publisher's
/// outbox, in order with its lists: a period's create is awaited there before its begin, and when the client answers it
/// with an error, or not within <c>CreateTimeout</c>, the period sends nothing more.
/// </remarks>
type internal Progress(host: GraphHost, workspace: Workspace, publisher: Publisher, client: RanvierClient) =
    // Graph thread
    let mutable status: Memo<ProgressStatus> = Unchecked.defaultof<_>
    let mutable root: Owner = Unchecked.defaultof<_>
    let mutable wanted = Settled
    let mutable wantedSince = 0L
    let mutable shown = Settled
    let mutable periods = 0
    // The shown period's token, and whether its create succeeded, which the outbox's consumer sets and reads
    let mutable period: (string * bool ref) option = None

    let log (text: string) = eprintfn "[ranvierautocomplete] %s" text

    let progress (token: string) (value: JObject) : Task =
        let p = JObject(JProperty("token", token), JProperty("value", value))
        client.Notify("$/progress", box p) |> Async.StartAsTask :> Task

    let statusNow () =
        match workspace.DeadNow() with
        | _ :: _ as dead ->
            Busy("Restart failed: " + String.Join(", ", dead |> List.map Path.GetFileName))
        | [] ->
            if workspace.LoadingNow() then Busy "Loading projects"
            elif publisher.CheckingNow() then Busy "Checking"
            else Settled

    /// Graph thread
    let start (title: string) =
        periods <- periods + 1
        let token = $"rfs/progress/%d{periods}"
        let created = ref false
        period <- Some(token, created)

        publisher.Enqueue(
            Notify(fun () ->
                task {
                    try
                        let! answer = (client.CreateProgress token |> Async.StartAsTask).WaitAsync Progress.CreateTimeout

                        match answer with
                        | Ok() ->
                            created.Value <- true
                            do! progress token (JObject(JProperty("kind", "begin"), JProperty("title", title)))
                        | Error e -> log $"the client refused the progress token: %s{e.Message}"
                    with ex ->
                        log $"creating the progress token failed: %s{ex.Message}"
                })
        )

    /// Graph thread
    let finish () =
        match period with
        | Some(token, created) ->
            period <- None

            publisher.Enqueue(
                Notify(fun () ->
                    if created.Value then
                        progress token (JObject(JProperty("kind", "end")))
                    else
                        Task.CompletedTask)
            )
        | None -> ()

    let holdFor (s: ProgressStatus) =
        match s with
        | Busy _ -> Progress.BeginHold
        | Settled -> Progress.EndHold

    /// Graph thread: makes the progress follow the wanted status once it has held long enough, else wakes when it will
    /// have
    let rec follow () =
        if wanted <> shown then
            let need = holdFor wanted
            let held = Stopwatch.GetElapsedTime wantedSince

            if held >= need then
                finish ()

                match wanted with
                | Busy title -> start title
                | Settled -> ()

                shown <- wanted
            else
                wake (need - held)

    and wake (after: TimeSpan) =
        Task
            .Delay(after)
            .ContinueWith(fun (_: Task) ->
                try
                    host.Post follow
                with :? ObjectDisposedException ->
                    ())
        |> ignore

    do
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
                                wake (holdFor now))

                        owner))
            .Wait()

    /// How long something must have been pending before the progress begins, or its title changes
    static member BeginHold: TimeSpan = TimeSpan.FromMilliseconds 100.

    /// How long everything must have settled before the progress ends
    static member EndHold: TimeSpan = TimeSpan.FromMilliseconds 100.

    /// How long the client may take to answer a create; the outbox's later sends wait behind it
    static member CreateTimeout: TimeSpan = TimeSpan.FromSeconds 10.

    /// What the progress follows now. For tests.
    member _.Status() : Task<ProgressStatus> = host.Read(fun () -> status.Value)

    member _.Dispose() =
        try
            host.Post(fun () -> root.Dispose())
        with :? ObjectDisposedException ->
            ()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
