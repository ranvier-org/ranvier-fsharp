/// A minimal shape of the lost wake-up Task 3 met (fix round 1, I2): a memo with a cutoff comparer that reads an async
/// memo directly, a key memo that keeps its previous value while equal, and an async consumer of the key that returns
/// its last value while the key is the same.
module Rfs.Checker.Tests.RanvierWakeTests

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Expecto
open Ranvier
open Rfs.Checker

type private Shape =
    { Source: Signal<int>
      Upstream: AsyncMemo<int>
      Consumer: AsyncMemo<int> }

/// `direct`: the cutoff memo reads the async memo itself; otherwise through a plain memo, the workaround.
let private build (direct: bool) (random: Random) : Shape =
    let source = createSignal 0

    // Like a file's finish: its input is a memo over async rows, and it publishes its last value inline (an
    // already-completed task) while that input is unchanged
    let rows =
        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
            let v = source.Value
            Task.Run<int>(fun () -> v / 3))

    // Pending while the rows are, and the same value again once they settle
    let input = createMemo (fun _ -> rows.Value)
    let upstream = ref Unchecked.defaultof<AsyncMemo<int>>

    upstream.Value <-
        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
            let v = input.Value
            let delay = random.Next 3

            if upstream.Value.Runs > 1 && upstream.Value.Peek = v then
                Task.FromResult v
            else
                Task.Run<int>(fun () ->
                    if delay > 0 then Thread.Sleep delay
                    v))

    let upstream = upstream.Value

    let read =
        if direct then
            fun () -> upstream.Value
        else
            let plain = createMemo (fun _ -> upstream.Value)
            fun () -> plain.Value

    // Equal for values in the same pair: the cutoff holds about half the time
    let byPair =
        { new IEqualityComparer<int> with
            member _.Equals(a, b) = a / 2 = b / 2
            member _.GetHashCode v = v / 2 }

    let cutoff = createMemoWithComparer byPair (fun _ -> read ())

    let key =
        createMemo (fun previous ->
            let v = cutoff.Value

            match previous with
            | ValueSome p when p / 2 = v / 2 -> p
            | _ -> v)

    let consumer = ref Unchecked.defaultof<AsyncMemo<int>>

    consumer.Value <-
        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
            let k = key.Value
            let previous = consumer.Value.Peek

            if consumer.Value.Runs > 1 && previous / 2 = k / 2 then
                Task.FromResult previous
            else
                Task.Run<int>(fun () -> k))

    createEffect (fun () -> consumer.Value.TryValue |> ignore)

    { Source = source
      Upstream = upstream
      Consumer = consumer.Value }

/// How many of `rounds` writes never settled within the timeout.
let private lostWakeUps (direct: bool) (rounds: int) =
    task {
        use host = GraphHost.Start "wake"
        let random = Random 7
        let! shape = host.Read(fun () -> build direct random)
        let mutable lost = 0

        for round in 1..rounds do
            do! host.Read(fun () -> shape.Source.Value <- round)

            let settled =
                host.WhenSettled(fun () ->
                    match shape.Upstream.TryValue, shape.Consumer.TryValue with
                    | Ready u, Ready c when u = round / 3 && c / 2 = u / 2 -> ValueSome()
                    | _ -> ValueNone)

            let! winner = Task.WhenAny(settled, Task.Delay(TimeSpan.FromSeconds 2.))

            if not (obj.ReferenceEquals(winner, settled)) then
                lost <- lost + 1
                // Wake the graph so the next round starts from a settled state
                do! host.Read(fun () -> shape.Consumer.TryValue |> ignore)

        return lost
    }

[<Tests>]
let tests =
    testList
        "RanvierWake"
        [ testTask "a cutoff memo read through a plain memo always wakes its readers" {
              let! lost = lostWakeUps false 300
              Expect.equal lost 0 "every write settles"
          }

          testTask "a cutoff memo reading an async memo directly always wakes its readers" {
              let! lost = lostWakeUps true 300
              Expect.equal lost 0 "every write settles"
          } ]
