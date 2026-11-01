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

              let signal, memo =
                  host.Read (fun () ->
                      let s = createSignal 1

                      let m =
                          createAsync (fun _ ct ->
                              let v = s.Value
                              tokens.Enqueue ct

                              if v = 1 then
                                  started1.TrySetResult () |> ignore
                                  gate1.Task
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

              // The cancelled flight completing late must not publish.
              gate1.SetResult 1
              Thread.Sleep 50
              let after = host.Read (fun () -> memo.Peek) |> within 10
              Expect.equal after 2 "late result of cancelled flight ignored"
          }

          test "dispose faults pending Read and WhenSettled with ObjectDisposedException" {
              let host = GraphHost.Start "dispose"
              let settled = host.WhenSettled (fun () -> ValueNone: int voption)
              use blocker = new ManualResetEventSlim ()
              use entered = new ManualResetEventSlim ()

              host.Post (fun () ->
                  entered.Set ()
                  blocker.Wait ())

              entered.Wait ()
              let read = host.Read (fun () -> 1)
              Expect.isFalse read.IsCompleted "read queued behind the blocker"
              let disposer = Task.Run (fun () -> host.Dispose ())
              Thread.Sleep 50
              blocker.Set ()
              Expect.isTrue (disposer.Wait (TimeSpan.FromSeconds 10.)) "dispose returned"

              faultsWithDisposed settled
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

          test "Dispose is idempotent" {
              let host = GraphHost.Start "idem"
              host.Dispose ()
              host.Dispose ()
          } ]
