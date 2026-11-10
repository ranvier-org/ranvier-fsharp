module Rfs.Checker.Tests.GraphHostTests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open Expecto
open Ranvier
open Rfs.Checker

let private within (seconds: int) (task: Task<'T>) : 'T =
    if not (task.Wait (TimeSpan.FromSeconds (float seconds))) then
        failtest "timed out waiting for task"

    task.Result

let private readyOf (memo: AsyncMemo<'T>) () : 'T voption =
    match memo.TryValue with
    | Ready v -> ValueSome v
    | _ -> ValueNone

let private faultsWithDisposed (t: Task) =
    Expect.throwsT<AggregateException> (fun () -> t.Wait 5000 |> ignore) "faults"
    Expect.isTrue (t.Exception.InnerException :? ObjectDisposedException) "ObjectDisposedException"

[<Tests>]
let tests =
    testList
        "GraphHost"
        [ test "async memo settles" {
              use host = GraphHost.Start "settles"

              let memo =
                  host.Read (fun () ->
                      createAsync (fun _ _ ->
                          task {
                              do! Task.Delay 10
                              return 42
                          }))
                  |> within 10

              Expect.equal (host.WhenSettled (readyOf memo) |> within 10) 42 "settled value"
          }

          test "post from many threads is ordered per poster" {
              use host = GraphHost.Start "ordered"
              let seen = ResizeArray<struct (int * int)>()
              let posters = 8
              let perPoster = 200

              let threads =
                  [ for p in 0 .. posters - 1 ->
                        Thread (fun () ->
                            for i in 0 .. perPoster - 1 do
                                host.Post (fun () -> seen.Add (struct (p, i))))
                  ]

              threads |> List.iter (fun t -> t.Start ())
              threads |> List.iter (fun t -> t.Join ())
              let snapshot = host.Read (fun () -> Seq.toArray seen) |> within 10
              Expect.equal snapshot.Length (posters * perPoster) "every post ran"

              for p in 0 .. posters - 1 do
                  let mine =
                      snapshot
                      |> Array.filter (fun (struct (q, _)) -> q = p)
                      |> Array.map (fun (struct (_, i)) -> i)

                  Expect.equal mine [| 0 .. perPoster - 1 |] $"poster %d{p} in order"
          }

          test "work runs on one thread that is not the caller" {
              use host = GraphHost.Start "thread"
              let a = host.Read (fun () -> Thread.CurrentThread.ManagedThreadId) |> within 10
              let b = host.Read (fun () -> Thread.CurrentThread.ManagedThreadId) |> within 10
              Expect.equal a b "same thread"
              Expect.notEqual a Thread.CurrentThread.ManagedThreadId "not the caller"
          }

          test "CancelPrevious cancels in-flight" {
              use host = GraphHost.Start "cancel"
              let tokens = ConcurrentQueue<CancellationToken>()
              let gate1 = TaskCompletionSource<int> ()
              let started1 = TaskCompletionSource ()
              let flight1Task = gate1.Task.ContinueWith (fun (t: Task<int>) -> t.Result)
              let flight1 = flight1Task :> Task

              let signal, memo =
                  host.Read (fun () ->
                      let s = createSignal 1

                      let m =
                          createAsync (fun _ ct ->
                              let v = s.Value
                              tokens.Enqueue ct

                              if v = 1 then
                                  started1.TrySetResult () |> ignore
                                  flight1Task
                              else
                                  Task.Delay(10).ContinueWith (fun (_: Task) -> v))

                      s, m)
                  |> within 10

              let settled = host.WhenSettled (readyOf memo)
              Expect.isTrue (started1.Task.Wait (TimeSpan.FromSeconds 10.)) "first flight started"
              host.Post (fun () -> signal.Value <- 2)
              Expect.equal (within 10 settled) 2 "only the second value"

              let first = tokens.ToArray().[0]
              Expect.isTrue first.IsCancellationRequested "first flight was cancelled"

              // The cancelled flight completes only now, after the second value settled; its result must not publish.
              gate1.SetResult 1
              Expect.isTrue (flight1.Wait (TimeSpan.FromSeconds 10.)) "first flight completed late"

              for _ in 1..20 do
                  Expect.equal (host.Read (fun () -> memo.Peek) |> within 10) 2 "late result of cancelled flight ignored"
          }

          test "WhenSettled keeps waiting when the read throws NotReadyException" {
              use host = GraphHost.Start "notready"
              let gate = TaskCompletionSource<int> ()
              let memo = host.Read (fun () -> createAsync (fun _ _ -> gate.Task)) |> within 10
              let t = host.WhenSettled (fun () -> ValueSome memo.Value)
              Thread.Sleep 50
              Expect.isFalse t.IsCompleted "still pending"
              gate.SetResult 5
              Expect.equal (within 10 t) 5 "settled once ready"
          }

          test "dispose faults pending Read and WhenSettled with ObjectDisposedException" {
              // Disposed from inside posted work on the host thread, so no timing is involved.
              let host = GraphHost.Start "dispose"
              let flag = ref false
              let never = host.WhenSettled (fun () -> ValueNone: int voption)
              // Would settle on the next pump, but the pump that disposes the host must fault it instead.
              let wouldSettle = host.WhenSettled (fun () -> if flag.Value then ValueSome 1 else ValueNone)
              use blocker = new ManualResetEventSlim ()
              use entered = new ManualResetEventSlim ()

              host.Post (fun () ->
                  entered.Set ()
                  blocker.Wait ()
                  flag.Value <- true
                  host.Dispose ())

              entered.Wait ()
              let read = host.Read (fun () -> 1)
              Expect.isFalse read.IsCompleted "read queued behind the blocker"
              blocker.Set ()

              faultsWithDisposed never
              faultsWithDisposed wouldSettle
              faultsWithDisposed read
              faultsWithDisposed (host.Read (fun () -> 1))
              faultsWithDisposed (host.WhenSettled (fun () -> ValueSome 1))
              Expect.throwsT<ObjectDisposedException> (fun () -> host.Post ignore) "Post after dispose"
              host.Dispose ()
          }

          test "an exception in posted work does not kill the loop" {
              use host = GraphHost.Start "throws"
              host.Post (fun () -> failwith "boom")
              Expect.equal (host.Read (fun () -> 7) |> within 10) 7 "loop survives"
              let errors = host.TakePostErrors ()
              Expect.equal errors.Length 1 "one recorded"
              Expect.equal errors.[0].Message "boom" "message"

              let failing = host.Read (fun () -> failwith "read boom")
              Expect.throwsT<AggregateException> (fun () -> failing.Wait 5000 |> ignore) "Read faults"
              Expect.equal (host.Read (fun () -> 8) |> within 10) 8 "still alive"
              Expect.isEmpty (host.TakePostErrors ()) "Read failures are not post errors"
          }

          // I4: what a failure on the graph thread leaves behind may never settle
          testTask "a failing post fails what is parked, and later waits are not touched" {
              let host = GraphHost.Start "post-fails-waits"

              try
                  let parked = host.WhenSettled(fun () -> (ValueNone: int voption))
                  host.Read (fun () -> ()) |> within 10
                  host.Post (fun () -> failwith "post boom")
                  let! finished = Task.WhenAny(parked, Task.Delay 10000)
                  Expect.isTrue (obj.ReferenceEquals(finished, parked)) "the wait ended"
                  Expect.isTrue parked.IsFaulted "the wait faults"
                  Expect.stringContains parked.Exception.InnerException.Message "post boom" "with what failed"
                  Expect.equal (host.WhenSettled(fun () -> ValueSome 3) |> within 10) 3 "a wait after it settles"
                  Expect.equal (host.TakePostErrors ()).Length 1 "kept"
              finally
                  host.Dispose ()
          }

          testTask "a failing effect fails what is parked" {
              let host = GraphHost.Start "effect-fails-waits"

              try
                  let! (signal: Signal<int>) =
                      host.Read (fun () ->
                          let s = createSignal 0
                          GraphEffects.guarded (fun () -> if s.Value = 1 then failwith "effect boom")
                          s)

                  let parked = host.WhenSettled(fun () -> (ValueNone: int voption))
                  host.Read (fun () -> ()) |> within 10
                  host.Post (fun () -> signal.Value <- 1)
                  let! finished = Task.WhenAny(parked, Task.Delay 10000)
                  Expect.isTrue (obj.ReferenceEquals(finished, parked)) "the wait ended"
                  Expect.isTrue parked.IsFaulted "the wait faults"
                  Expect.stringContains parked.Exception.InnerException.Message "effect boom" "with what failed"
              finally
                  host.Dispose ()
          }

          // I1 of slice 6's review: a failure no project claims may have stranded an apart wait too
          for kind in [ "post"; "scope"; "pump"; "effect" ] do
            testTask $"a failure no project claims ends the apart waits too: %s{kind}" {
                  let host = GraphHost.Start("apart-" + kind)

                  try
                      let! (signal: Signal<int>) =
                          host.Read(fun () ->
                              let s = createSignal 0
                              createRoot (fun _ -> GraphEffects.guarded (fun () -> if s.Value = 1 then failwith "effect boom")) |> ignore
                              s)

                      let apart = host.WhenSettled((fun () -> (ValueNone: int voption)), CancellationToken.None, true)
                      host.Read(fun () -> ()) |> within 10

                      match kind with
                      | "post" -> host.Post(fun () -> failwith "post boom")
                      | "scope" -> host.Graph.Dispatch(Action(fun () -> failwith "scope boom"))
                      | "pump" -> host.FailNextPump(InvalidOperationException "pump boom")
                      | _ -> host.Post(fun () -> signal.Value <- 1)

                      let! finished = Task.WhenAny(apart, Task.Delay 10000)
                      Expect.isTrue (obj.ReferenceEquals(finished, apart)) $"the apart wait ended (%s{kind})"
                      Expect.isTrue apart.IsFaulted $"and faulted (%s{kind})"
                  finally
                      host.Dispose()
            }

          testTask "a failure a project claimed ends the shared waits, not the apart ones" {
              let host = GraphHost.Start "apart-claimed"

              try
                  let! (signal: Signal<int>) =
                      host.Read(fun () ->
                          let s = createSignal 0
                          createRoot (fun _ -> GraphEffects.guardedFor ignore (fun () -> if s.Value = 1 then failwith "claimed boom")) |> ignore
                          s)

                  let apart = host.WhenSettled((fun () -> (ValueNone: int voption)), CancellationToken.None, true)
                  let shared = host.WhenSettled(fun () -> (ValueNone: int voption))
                  host.Read(fun () -> ()) |> within 10
                  host.Post(fun () -> signal.Value <- 1)
                  let! finished = Task.WhenAny(shared, Task.Delay 10000)
                  Expect.isTrue (obj.ReferenceEquals(finished, shared)) "the shared wait ended"
                  Expect.isTrue shared.IsFaulted "and faulted"
                  host.Read(fun () -> ()) |> within 10
                  Expect.isFalse apart.IsCompleted "the apart wait goes on"

                  let claimedPost = InvalidOperationException "claimed post boom"
                  host.Post(fun () ->
                      GraphClaims.claim claimedPost
                      raise claimedPost)

                  host.Read(fun () -> ()) |> within 10
                  host.Read(fun () -> ()) |> within 10
                  Expect.isFalse apart.IsCompleted "a claimed post doesn't end it either"
              finally
                  host.Dispose()
          }

          test "WhenSettled with an unsatisfied read waits without spinning and honours cancellation" {
              use host = GraphHost.Start "cancel-wait"
              let mutable reads = 0
              use cts = new CancellationTokenSource ()

              let t =
                  host.WhenSettled (
                      (fun () ->
                          Interlocked.Increment &reads |> ignore
                          ValueNone: int voption),
                      cts.Token
                  )

              Thread.Sleep 200
              Expect.isLessThan (Volatile.Read &reads) 5 "no busy loop"
              cts.Cancel ()
              Expect.throwsT<AggregateException> (fun () -> t.Wait 5000 |> ignore) "cancelled"
              Expect.isTrue t.IsCanceled "task canceled"
          }

          test "WhenSettled re-reads after a post changes state" {
              use host = GraphHost.Start "reread"
              let flag = ref false
              let t = host.WhenSettled (fun () -> if flag.Value then ValueSome "ok" else ValueNone)
              Thread.Sleep 50
              host.Post (fun () -> flag.Value <- true)
              Expect.equal (within 10 t) "ok" "settled after post"
          }

          test "Dispose from another thread faults a parked WhenSettled" {
              let host = GraphHost.Start "dispose-x"
              let parked = host.WhenSettled (fun () -> ValueNone: int voption)
              host.Dispose ()
              faultsWithDisposed parked
          }

          test "kept post errors are capped at the newest 256" {
              use host = GraphHost.Start "cap"

              for i in 1..300 do
                  host.Post (fun () -> failwith $"e%d{i}")

              host.Read (fun () -> ()) |> within 10
              let errors = host.TakePostErrors ()
              Expect.equal errors.Length 256 "capped"
              Expect.equal errors.[255].Message "e300" "newest kept"
              Expect.equal errors.[0].Message "e45" "oldest dropped"
              Expect.isEmpty (host.TakePumpErrors ()) "no pump errors"
          }

          test "Dispose is idempotent" {
              let host = GraphHost.Start "idem"
              host.Dispose ()
              host.Dispose ()
          } ]
