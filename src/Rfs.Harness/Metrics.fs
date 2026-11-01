namespace Rfs.Harness

open System
open System.Diagnostics
open System.IO
open System.Diagnostics.Tracing
open System.Threading
open System.Threading.Tasks
open Microsoft.Diagnostics.NETCore.Client
open Microsoft.Diagnostics.Tracing
open Microsoft.Diagnostics.Tracing.Parsers
open Microsoft.Diagnostics.Tracing.Parsers.Clr

type MetricsSnapshot =
    { PeakWorkingSetMb: float
      AllocatedMb: float
      Gen2: int }

/// Watches one process for the length of a run: peak working set by polling, allocations and gen-2 GCs from an
/// EventPipe session on the runtime provider.
type MetricsCollector private (pid: int) =
    let mb (bytes: float) = bytes / 1048576.
    let gcKeyword = 0x1L
    let mutable peakWorkingSet = 0L
    let mutable allocated = 0.
    let mutable gen2 = 0
    let stop = new CancellationTokenSource()

    let sample (proc: Process) =
        proc.Refresh()
        let current = proc.WorkingSet64
        peakWorkingSet <- max peakWorkingSet current

    // The session comes before the poller so a failure here leaves nothing running.
    // GCAllocationTick fires about every 100 KB of allocation, so the sum is approximate.
    let session =
        DiagnosticsClient(pid)
            .StartEventPipeSession(
                [ EventPipeProvider("Microsoft-Windows-DotNETRuntime", EventLevel.Verbose, gcKeyword) ],
                false
            )

    let source = new EventPipeEventSource(session.EventStream)

    let process' =
        try
            Some(Process.GetProcessById pid)
        with :? ArgumentException ->
            None

    let poll =
        Task.Run(fun () ->
            task {
                match process' with
                | None -> ()
                | Some proc ->
                    use timer = new PeriodicTimer(TimeSpan.FromMilliseconds 100.)

                    try
                        sample proc

                        while! timer.WaitForNextTickAsync stop.Token do
                            sample proc
                    with
                    | :? OperationCanceledException -> ()
                    // the process left; the peak so far stands
                    | :? InvalidOperationException -> ()
            }
            :> Task)

    let events =
        source.Clr.add_GCAllocationTick (fun e -> allocated <- allocated + float e.AllocationAmount64)
        source.Clr.add_GCStart (fun e -> if e.Depth = 2 then gen2 <- gen2 + 1)
        Task.Run(fun () -> source.Process() |> ignore)

    /// Starts watching `pid`; the EventPipe session is established when this returns.
    static member Start(pid: int) = new MetricsCollector(pid)

    /// Like Start, but a missing process or a refused session gives None and a warning on stderr, so a run's
    /// latency data survives without metrics.
    static member TryStart(pid: int) : MetricsCollector option =
        if pid <= 0 then
            None
        else
            try
                Some(MetricsCollector.Start pid)
            with e ->
                eprintfn "warning: metrics unavailable for process %d: %s" pid e.Message
                None

    /// Ends the session, waits for the buffered events to be read, and returns the totals.
    member _.Stop() : MetricsSnapshot =
        stop.Cancel()
        poll.Wait()

        try
            session.Stop()
        with
        // the process may already be gone
        | :? ServerNotAvailableException
        | :? EndOfStreamException
        | :? TimeoutException -> ()

        events.Wait()
        source.Dispose()
        session.Dispose()

        match process' with
        | Some proc ->
            try
                sample proc
            with :? InvalidOperationException ->
                ()

            proc.Dispose()
        | None -> ()

        { PeakWorkingSetMb = mb (float peakWorkingSet)
          AllocatedMb = mb allocated
          Gen2 = gen2 }
