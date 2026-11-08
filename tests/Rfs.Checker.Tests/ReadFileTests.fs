module Rfs.Checker.Tests.ReadFileTests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Checker.Tests.Deps

let private a = fileOf "A.fs"
let private c = fileOf "C.fs"

let private eventually (what: string) (check: unit -> bool) : Task =
    task {
        let deadline = DateTime.UtcNow.AddSeconds 60.

        while not (check ()) do
            if DateTime.UtcNow > deadline then
                failtest $"timed out waiting for %s{what}"

            do! Task.Delay 10
    }

/// The object the fork holds while it checks the file, as the graph finds it
let private checkLock (s: Session) (file: string) : obj =
    let flags = Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic
    let checks = typeof<FSharp.Compiler.Declarations.ProjectContext>.GetProperty("Checks", flags).GetValue s.Graph.Context
    checks.GetType().GetMethod("FileLock", flags).Invoke(checks, [| box file |])

let private swallowCancellation (t: Task) =
    task {
        try
            do! t
            return false
        with :? OperationCanceledException ->
            return true
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "ReadFile"
        [ testTask "reads the latest text, not an older settled one" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let text = s.Texts[a].Replace("let baseValue = 10", "let baseValue = 11")
              let version = s.Graph.SetText(a, text)
              let! (view: FileView) = s.Graph.ReadFile(a, id, CancellationToken.None) |> settle
              Expect.equal view.Text text "the edit's text"
              Expect.equal view.Version version "the edit's version"
          }

          testTask "a read holds a finish off the file until it is done" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let entered = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
              let release = new ManualResetEventSlim(false)
              let ran = ConcurrentQueue<_>()
              let _sub = s.Graph.Ran |> Observable.subscribe ran.Enqueue

              let before = s.Texts[a]

              let held =
                  s.Graph.ReadFile(
                      a,
                      (fun v ->
                          entered.SetResult(())
                          release.Wait(TimeSpan.FromSeconds 60.) |> ignore
                          v.Text),
                      CancellationToken.None
                  )

              do! entered.Task |> settle
              let ticks = s.Graph.FinishTicks
              let after = before.Replace("let baseValue = 10", "let baseValue = 11")
              let version = s.Graph.SetText(a, after)
              // The declarations are checked (checks share the lock); the finish needs it alone
              do! eventually "the edited declaration to be checked" (fun () -> ran.Count > 0)
              let settled = s.Graph.SettledDiagnostics a
              do! Task.Delay 500
              Expect.isFalse settled.IsCompleted "the file does not settle while the read holds it"
              Expect.equal s.Graph.FinishTicks ticks "no finish ran during the read"
              release.Set()
              let! text = held |> settle
              Expect.equal text before "the read saw the text it settled on"
              let! struct (v, _) = settled |> settle
              Expect.equal v version "the file then settles on the edit"
              Expect.isGreaterThan s.Graph.FinishTicks ticks "and the finish ran"
          }

          testTask "a read of a file checked whole needs no lock" {
              let! s = startWith FileLevel
              use _ = (s :> IAsyncDisposable)
              let! text = s.Graph.ReadFile(c, (fun v -> v.Text), CancellationToken.None) |> settle
              Expect.equal text s.Texts[c] "the text"
          }

          testTask "a signature file has no result to read" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              Expect.throwsT<ArgumentException> (fun () -> s.Graph.ReadFile(fileOf "E.fsi", id, CancellationToken.None) |> ignore) "signature"
          }

          testTask "a read refuses to run holding the file's check lock" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)

              lock (checkLock s a) (fun () ->
                  Expect.throwsT<InvalidOperationException>
                      (fun () -> s.Graph.ReadFile(a, id, CancellationToken.None) |> ignore)
                      "m2 guard")
          }

          testTask "cancelling the token ends the wait" {
              let! s = startWith FileLevel
              use _ = (s :> IAsyncDisposable)
              let hold = new ManualResetEventSlim(false)
              s.Graph.OnScopedCheck(fun path -> if path = c then hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)
              s.Graph.SetText(c, s.Texts[c] + "\nlet extra = 1\n") |> ignore
              let cts = new CancellationTokenSource()
              let waiting = s.Graph.ReadFile(c, id, cts.Token)
              do! Task.Delay 300
              Expect.isFalse waiting.IsCompleted "the file is held unsettled"
              cts.Cancel()
              let! cancelled = swallowCancellation waiting |> settle
              hold.Set()
              Expect.isTrue cancelled "canceled"
          }

          testTask "disposing the graph ends the wait" {
              let! s = startWith FileLevel
              use _ = (s :> IAsyncDisposable)
              let hold = new ManualResetEventSlim(false)
              s.Graph.OnScopedCheck(fun path -> if path = c then hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)
              s.Graph.SetText(c, s.Texts[c] + "\nlet extra = 1\n") |> ignore
              let waiting = s.Graph.ReadFile(c, id, CancellationToken.None)
              do! Task.Delay 300
              Expect.isFalse waiting.IsCompleted "the file is held unsettled"
              s.Graph.Dispose()
              let! cancelled = swallowCancellation waiting |> settle
              hold.Set()
              Expect.isTrue cancelled "canceled"
          } ]
