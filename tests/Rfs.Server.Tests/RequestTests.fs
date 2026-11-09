module Rfs.Server.Tests.RequestTests

open System
open System.Threading
open System.Threading.Tasks
open Expecto
open Ranvier
open Rfs.Checker

let private within (t: Task<'T>) : Task<'T> =
    task {
        let! first = Task.WhenAny(t :> Task, Task.Delay(TimeSpan.FromSeconds 30.))

        if not (obj.ReferenceEquals(first, t)) then
            failwith "timed out"

        return! t
    }

let private outcome (t: Task<'T>) : Task<Result<'T, exn>> =
    task {
        try
            let! v = within t
            return Ok v
        with e ->
            return Error e
    }

let private started () =
    let host = GraphHost.Start "request"

    host,
    { new IAsyncDisposable with
        member _.DisposeAsync() =
            host.Dispose()
            ValueTask.CompletedTask }

/// A turn of the host after everything posted so far
let private turn (host: GraphHost) = host.Read ignore

[<Tests>]
let tests =
    testList
        "Request"
        [ testTask "a request waits while what it reads is pending, and answers once it is ready" {
              let host, stop = started ()
              use _ = stop
              let! (source: AsyncSource<int>) = host.Read(fun () -> createAsyncSource<int> ())
              let answer = Request.run host (fun _ -> Task.FromResult(source.Value * 2)) CancellationToken.None
              do! turn host
              do! Task.Delay 100
              Expect.isFalse answer.IsCompleted "pending"
              host.Post(fun () -> source.Settle 21)
              let! v = within answer
              Expect.equal v 42 "ready"
          }

          testTask "new input runs the body again, and a guard that fails waits for it" {
              let host, stop = started ()
              use _ = stop
              let! (input: Signal<int>) = host.Read(fun () -> createSignal 0)
              let mutable runs = 0

              let answer =
                  Request.run
                      host
                      (fun flight ->
                          runs <- runs + 1
                          let v = input.Value
                          if v < 3 then Request.waitForNextTurn flight else Task.FromResult v)
                      CancellationToken.None

              for v in 1..3 do
                  do! turn host
                  Expect.isFalse answer.IsCompleted $"waits at %d{v - 1}"
                  host.Post(fun () -> input.Value <- v)

              let! v = within answer
              Expect.equal v 3 "the input that passed the guard"
              Expect.equal runs 4 "once per input"
          }

          testTask "a cancel ends that request's wait only, and the work it shares goes on" {
              let host, stop = started ()
              use _ = stop
              let release = TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously)
              let mutable sharedToken = CancellationToken.None
              let mutable flights = 0

              let! (shared: AsyncMemo<int>) =
                  host.Read(fun () ->
                      createAsync (fun _ ct ->
                          flights <- flights + 1
                          sharedToken <- ct
                          release.Task))

              let cts = new CancellationTokenSource()
              let first = Request.run host (fun _ -> Task.FromResult shared.Value) cts.Token
              let second = Request.run host (fun _ -> Task.FromResult(shared.Value + 1)) CancellationToken.None
              do! turn host
              cts.Cancel()
              let! cancelled = outcome first

              Expect.isTrue
                  (match cancelled with
                   | Error(e: exn) when (e :? OperationCanceledException) -> true
                   | _ -> false)
                  "the cancelled request is cancelled"

              do! turn host
              do! turn host
              Expect.isFalse sharedToken.IsCancellationRequested "the shared flight goes on"
              Expect.isFalse second.IsCompleted "the other request still waits"
              release.SetResult 1
              let! v = within second
              Expect.equal v 2 "the other request answers"
              Expect.equal flights 1 "from the one flight"
          }

          testTask "a fault fails that request alone: another project's on the same host answers" {
              let host, stop = started ()
              use _ = stop
              let! (a: AsyncSource<int>), (b: AsyncSource<int>) = host.Read(fun () -> createAsyncSource<int> (), createAsyncSource<int> ())
              let onA = Request.run host (fun _ -> Task.FromResult a.Value) CancellationToken.None
              let onB = Request.run host (fun _ -> Task.FromResult b.Value) CancellationToken.None
              do! turn host
              host.Post(fun () -> a.Fail(InvalidOperationException "project A's check failed"))
              // Work on the graph thread that fails, as a restart of project A would, faults the host's parked waits
              host.Post(fun () -> failwith "project A's restart failed")
              let! failed = outcome onA

              Expect.isTrue
                  (match failed with
                   | Error(e: exn) -> e :? InvalidOperationException && e.Message = "project A's check failed"
                   | _ -> false)
                  "A's request fails with A's fault"

              do! turn host
              Expect.isFalse onB.IsCompleted "B's request still waits"
              host.Post(fun () -> b.Settle 7)
              let! v = within onB
              Expect.equal v 7 "B's request answers"
              host.TakePostErrors() |> ignore
          }

          testTask "a finish no longer in effect under the lock releases it and waits for the next turn" {
              let host, stop = started ()
              use _ = stop
              let! (turnOf: Signal<int>) = host.Read(fun () -> createSignal 0)
              // What the published result is: changed off the body's reads, as a finish that lands between the settle and the lock
              let mutable published = 0
              let gate = new SemaphoreSlim(1, 1)
              let readOn = Collections.Concurrent.ConcurrentQueue<int>()

              let acquire (ct: CancellationToken) : Task<IDisposable> =
                  task {
                      do! gate.WaitAsync ct
                      return { new IDisposable with member _.Dispose() = gate.Release() |> ignore }
                  }

              do! gate.WaitAsync()

              let answer =
                  Request.run
                      host
                      (fun flight ->
                          turnOf.Value |> ignore
                          let seen = Volatile.Read &published

                          Task.Run<int>(fun () ->
                              Request.live
                                  acquire
                                  (fun () -> host.Read(fun () -> Volatile.Read &published = seen))
                                  (fun () ->
                                      readOn.Enqueue seen
                                      seen)
                                  flight))
                      CancellationToken.None

              do! turn host
              Volatile.Write(&published, 1)
              gate.Release() |> ignore
              do! Task.Delay 200
              Expect.isFalse answer.IsCompleted "it waits rather than read a result no longer published"
              let! got = gate.WaitAsync(TimeSpan.FromSeconds 5.)
              Expect.isTrue got "the lock was released"
              gate.Release() |> ignore
              host.Post(fun () -> turnOf.Value <- 1)
              let! v = within answer
              Expect.equal v 1 "the next turn answers from the result published"
              Expect.sequenceEqual readOn [ 1 ] "nothing was read on the result no longer published"
          }

          testTask "after 100,000 requests, cancelled or answered, none of their nodes stays alive" {
              let host, stop = started ()
              use _ = stop
              let! (input: Signal<int>) = host.Read(fun () -> createSignal 1)
              let kept = Collections.Generic.List<WeakReference>()

              let run (n: int) =
                  let held = obj ()

                  if n % 1000 < 2 then
                      kept.Add(WeakReference held)

                  let cts = new CancellationTokenSource()

                  let t =
                      Request.run
                          host
                          (fun flight ->
                              let v = input.Value
                              GC.KeepAlive held
                              if n % 2 = 0 then Request.waitForNextTurn flight else Task.FromResult v)
                          cts.Token

                  t, cts

              for batch in 0..99 do
                  let started = [| for n in batch * 1000 .. batch * 1000 + 999 -> run n |]
                  // Every root is made and its body has run before the cancels
                  do! turn host

                  for _, cts in started do
                      cts.Cancel()

                  let tasks = started |> Array.map fst

                  try
                      do! Task.WhenAll tasks |> within
                  with _ ->
                      ()

                  Expect.isTrue (tasks |> Array.forall _.IsCompleted) "every request ended"

              do! turn host
              do! turn host
              GC.Collect()
              GC.WaitForPendingFinalizers()
              GC.Collect()
              let alive = kept |> Seq.filter _.IsAlive |> Seq.length
              Expect.equal alive 0 $"of %d{kept.Count} requests sampled, alive"
          } ]
