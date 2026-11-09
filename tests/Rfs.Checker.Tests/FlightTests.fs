module Rfs.Checker.Tests.FlightTests

open System
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Declarations
open Ranvier
open Rfs.Checker
open Rfs.Checker.Tests.Deps

let private d = fileOf "D.fs"
let private e = fileOf "E.fs"

let private samePath (a: string) (b: string) =
    String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

/// Waits, on the thread the hook runs on, until the flight's token is cancelled. False if it never was.
let private untilCancelled (ct: CancellationToken) =
    let clock = Diagnostics.Stopwatch.StartNew()

    while not ct.IsCancellationRequested && clock.Elapsed < TimeSpan.FromSeconds 60. do
        Thread.Sleep 2

    ct.IsCancellationRequested

/// The errors each of the graph's error channels holds.
let internal errorsOf (host: GraphHost) =
    task {
        let! root = host.Read(fun () -> Graph.Current.Root.Errors |> Seq.toArray)
        return [ yield! host.TakePostErrors(); yield! host.TakePumpErrors(); yield! root ]
    }

/// Holds the first flight of `kind` until `edit 2` supersedes the one `edit 1` started, then lets it go on. Returns once
/// the graph settled, having checked that the flight ended at its check and that nothing surfaced.
let internal supersedeOne
    (host: GraphHost)
    (onFlight: (string -> string -> CancellationToken -> unit) -> unit)
    (skipped: string -> int64)
    (kind: string)
    (edit: int -> unit)
    (settled: unit -> Task)
    : Task =
    task {
        let entered = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
        let cancelled = TaskCompletionSource<bool>(TaskCreationOptions.RunContinuationsAsynchronously)
        let armed = ref 1

        onFlight (fun k _ ct ->
            if k = kind && Interlocked.Exchange(&armed.contents, 0) = 1 then
                entered.SetResult(())
                cancelled.SetResult(untilCancelled ct))

        let before = skipped kind
        edit 1
        do! entered.Task |> settle
        edit 2
        let! wasCancelled = cancelled.Task |> settle
        Expect.isTrue wasCancelled $"the newer edit cancelled the %s{kind} flight"
        do! settled ()
        Expect.isGreaterThan (skipped kind) before $"the %s{kind} flight ended at its check"
        let! errors = errorsOf host
        Expect.isEmpty errors $"the %s{kind} flight's cancellation surfaced nowhere"
    }

let private withAfter (text: string) (n: int) =
    text.Replace("let after = 42", $"let after = %d{42 + n}")

/// A burst of edits of D.fs in which each finish but the last is held at `holdAt` until the next edit supersedes it.
let private superseded (holdAt: string) =
    testTask $"a finish superseded at %s{holdAt} does not run" {
        let! s = startWith Declarations
        use _ = (s :> IAsyncDisposable)
        let burst = 5
        let flights = ref 0
        let ran = ref 0
        let allCancelled = ref true

        s.Graph.OnFlight(fun kind path ct ->
            if samePath path d then
                if kind = "finish.run" then
                    Interlocked.Increment &ran.contents |> ignore
                elif kind = holdAt && Interlocked.Increment &flights.contents < burst && not (untilCancelled ct) then
                    allCancelled.Value <- false)

        let original = s.Texts[d]
        let mutable version = 0L

        for i in 1 .. burst do
            let seen = Volatile.Read &flights.contents
            s.Texts[d] <- withAfter original i
            version <- s.Graph.SetText(d, s.Texts[d])

            if i < burst then
                let deadline = DateTime.UtcNow.AddSeconds 60.

                while Volatile.Read &flights.contents = seen do
                    if DateTime.UtcNow > deadline then
                        failtest $"no finish of edit %d{i}"

                    do! Task.Delay 2

        let! struct (v, _) = s.Graph.SettledDiagnostics d |> settle
        Expect.equal v version "settled on the last edit"
        do! whenPromoted s.Graph s.Spec
        Expect.isTrue allCancelled.Value "each held finish was superseded"
        Expect.equal ran.Value 1 $"only the last of the %d{burst} finishes of the file ran"
        let! text = s.Graph.ReadFile(d, (fun (v: FileView) -> v.Text), CancellationToken.None) |> settle
        Expect.equal text s.Texts[d] "the last edit's text"
        let! diagnostics = s.Graph.AllDiagnostics() |> settle
        let! diff = oracleDiff s diagnostics
        Expect.isEmpty diff "the final state equals a full check"
    }

/// Probes the checker with D's and E's current parses: it refuses D (SupersededLineDirectivesException) when an older
/// parse's line directives were left, and E's scoped setup (StaleDependenciesException) when the context's latest parse
/// of D or its dependency snapshot is an older one's.
let private probeNewestLeft (s: Session) =
    task {
        let ctx = s.Graph.Context

        let probe (file: string) =
            s.Graph.ReadFile(
                file,
                (fun (v: FileView) ->
                    let before = [ for u in v.Upstream[1..] -> u :?> FileSignature ]
                    DeclarationChecker.CheckFile(ctx, before, v.Parse) |> ignore
                    before.Length),
                CancellationToken.None
            )
            |> settle

        let! _ = probe d
        let! read = probe e
        Expect.isLessThan read (s.Spec.SourceFiles |> List.findIndex (samePath e)) "E is set up from fewer than every earlier file, so the probe reads the snapshot"
    }

/// An older flight of `kind` passes its check and is held there; a newer edit launches the next flight, which is given
/// time to reach the lock; then the older goes on. The newer must write after it.
let private passedCheck (kind: string) =
    testTask $"an older %s{kind} flight past its check writes before a newer one" {
        let! s = startWith FileLevel
        use _ = (s :> IAsyncDisposable)
        let held = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
        let newerArrived = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
        let release = new ManualResetEventSlim(false)
        let armed = ref 1
        let arrivals = ref 0
        let isD path = kind = "dependencies" || samePath path d

        s.Graph.OnFlight(fun k path _ ->
            if isD path then
                if k = kind + ".checked" && Interlocked.Exchange(&armed.contents, 0) = 1 then
                    held.SetResult(())
                    release.Wait(TimeSpan.FromSeconds 60.) |> ignore
                elif k = kind && Interlocked.Increment &arrivals.contents = 2 then
                    newerArrived.TrySetResult(()) |> ignore)

        let original = s.Texts[d]
        s.Graph.SetText(d, "# 1 \"elsewhere.fsy\"\n" + original) |> ignore
        do! held.Task |> settle
        s.Texts[d] <- withAfter original 1
        let version = s.Graph.SetText(d, s.Texts[d])
        do! newerArrived.Task |> settle
        // Time for the newer flight to take the lock, were it free
        do! Task.Delay 300
        release.Set()
        let! struct (v, _) = s.Graph.SettledDiagnostics d |> settle
        Expect.equal v version "settled on the newer edit"
        let! _ = s.Graph.AllDiagnostics() |> settle
        do! probeNewestLeft s
        let! errors = errorsOf s.Host
        Expect.isEmpty errors "nothing surfaced"
    }

[<Tests>]
let tests =
    // Sequenced: the order test parses D.fs with a line directive, which the process keeps for the file
    testSequenced
    <| testList
        "Flight tokens"
        [ superseded "finish"
          superseded "finish.lock"
          passedCheck "parse"
          passedCheck "dependencies"

          testTask "an older parse that reaches the lock after a newer one leaves the newest's writes" {
              let! s = startWith FileLevel
              use _ = (s :> IAsyncDisposable)
              let entered = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
              let release = new ManualResetEventSlim(false)
              let left = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
              let armed = ref 1

              s.Graph.OnFlight(fun kind path _ ->
                  if kind = "parse" && samePath path d && Interlocked.Exchange(&armed.contents, 0) = 1 then
                      entered.SetResult(())
                      release.Wait(TimeSpan.FromSeconds 60.) |> ignore
                      left.SetResult(()))

              // The older text has a line directive and the newer has none, so whichever wrote last shows
              let original = s.Texts[d]
              s.Graph.SetText(d, "# 1 \"elsewhere.fsy\"\n" + original) |> ignore
              do! entered.Task |> settle
              s.Texts[d] <- withAfter original 1
              let version = s.Graph.SetText(d, s.Texts[d])
              let! struct (v, _) = s.Graph.SettledDiagnostics d |> settle
              Expect.equal v version "the newer parse went first and the file settled on it"
              let! _ = s.Graph.AllDiagnostics() |> settle
              let measures = s.Graph.MeasuresStarted
              release.Set()
              do! left.Task |> settle
              // Time for the older flight to reach the lock and, were it to, parse
              do! Task.Delay 300
              let ctx = s.Graph.Context

              // The checker refuses these when the older parse's writes are the ones left
              let probe (file: string) =
                  s.Graph.ReadFile(
                      file,
                      (fun (v: FileView) ->
                          let before = [ for u in v.Upstream[1..] -> u :?> FileSignature ]
                          DeclarationChecker.CheckFile(ctx, before, v.Parse) |> ignore
                          before.Length),
                      CancellationToken.None
                  )
                  |> settle

              // D's line directives: SupersededLineDirectivesException
              let! _ = probe d
              // The latest parse of D, earlier than E, against the dependency snapshot: StaleDependenciesException
              let! read = probe e
              let earlier = s.Spec.SourceFiles |> List.findIndex (samePath e)
              Expect.isLessThan read earlier "E is set up from fewer than every earlier file, so the probe reads the snapshot"
              Expect.equal s.Graph.MeasuresStarted measures "the older flight did not parse"
              let! errors = errorsOf s.Host
              Expect.isEmpty errors "the cancellation surfaced nowhere"
          }

          for kind, mode in [ "parse", FileLevel; "dependencies", FileLevel; "split", Declarations; "finish", Declarations ] do
              testTask $"a cancelled %s{kind} flight publishes nothing and is no fault" {
                  let! s = startWith mode
                  use _ = (s :> IAsyncDisposable)
                  let original = s.Texts[d]
                  let version = ref 0L

                  let edit (n: int) =
                      s.Texts[d] <- withAfter original n
                      version.Value <- s.Graph.SetText(d, s.Texts[d])

                  let settled () : Task =
                      task {
                          let! struct (v, _) = s.Graph.SettledDiagnostics d |> settle
                          Expect.equal v version.Value "settled on the newest edit"
                          do! whenPromoted s.Graph s.Spec
                      }

                  do! supersedeOne s.Host s.Graph.OnFlight s.Graph.SkippedFlights kind edit settled
                  let! diagnostics = s.Graph.AllDiagnostics() |> settle
                  let! diff = oracleDiff s diagnostics
                  Expect.isEmpty diff "the newest edit's result equals a full check"
              }

          // Edits as they come: superseded finishes are skipped only when they overlap, so this asserts the result alone
          testTask "a burst of edits as they come settles on the last" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let ran = ref 0

              s.Graph.OnFlight(fun kind path _ ->
                  if kind = "finish.run" && samePath path d then
                      Interlocked.Increment &ran.contents |> ignore)

              let original = s.Texts[d]
              let mutable version = 0L
              let edits = 40

              for i in 1 .. edits do
                  s.Texts[d] <- withAfter original i
                  version <- s.Graph.SetText(d, s.Texts[d])
                  do! Task.Delay 1

              let! struct (v, _) = s.Graph.SettledDiagnostics d |> settle
              Expect.equal v version "settled on the last edit"
              do! whenPromoted s.Graph s.Spec
              let skipped = s.Graph.SkippedFlights "finish"
              Diagnostics.Trace.WriteLine $"burst: %d{edits} edits, %d{ran.Value} finishes of the file, %d{skipped} skipped in the project"
              let! diagnostics = s.Graph.AllDiagnostics() |> settle
              let! diff = oracleDiff s diagnostics
              Expect.isEmpty diff "the final state equals a full check"
          } ]
