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

          testTask "a read holds a finish off the file until it is done, and an edit meanwhile runs it again" {
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
                          entered.TrySetResult(()) |> ignore
                          release.Wait(TimeSpan.FromSeconds 60.) |> ignore
                          v.Text),
                      CancellationToken.None
                  )

              do! entered.Task |> settle
              let ticks = s.Graph.FinishTicks
              let after = before.Replace("let baseValue = 10", "let baseValue = 11")
              let version = s.Graph.SetText(a, after)
              let settled = s.Graph.SettledDiagnostics a
              do! Task.Delay 700
              // A check or shift undoes the last finish in place, so none runs while the read holds the file
              Expect.equal ran.Count 0 "no declaration check ran during the read"
              Expect.isFalse settled.IsCompleted "the file does not settle while the read holds it"
              Expect.equal s.Graph.FinishTicks ticks "no finish ran during the read"
              release.Set()
              let! text = held |> settle
              // A request node: the edit runs it again on the new text, and what the read on the older text gave is
              // dropped, as a superseded flight's is
              Expect.equal text after "the read answers for the text the edit left, once the file settles on it"
              let! struct (v, _) = settled |> settle
              Expect.equal v version "the file then settles on the edit"
              Expect.isGreaterThan s.Graph.FinishTicks ticks "and the finish ran"
          }

          testTask "a read that waits for the lock reads again when a newer result came meanwhile" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let reached = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
              let go = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
              let mutable calls = 0

              // The first settle of the read is held before the lock is asked for
              s.Graph.OnReadSettled(fun () ->
                  if Interlocked.Increment &calls = 1 then
                      reached.SetResult(())
                      go.Task :> Task
                  else
                      Task.CompletedTask)

              let read = s.Graph.ReadFile(a, (fun v -> v.Text), CancellationToken.None)
              do! reached.Task |> settle
              // A whole edit, finish included, lands between the settle and the lock
              let after = s.Texts[a].Replace("let baseValue = 10", "let baseValue = 11")
              let version = s.Graph.SetText(a, after)
              let! struct (settledVersion, _) = s.Graph.SettledDiagnostics a |> settle
              Expect.equal settledVersion version "the file settled on the edit"
              go.SetResult(())
              let! text = read |> settle
              Expect.equal text after "the read answers from the result published when it got the lock, not the one it settled on"
          }

          // A consistency test: it passes with the lock shared too. `a read holds a finish off the file until it is done`
          // is the pin for the exclusive lock.
          testTask "queries inside one read agree across an edit, in a signed file" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let efsi = fileOf "E.fsi"
              let efs = fileOf "E.fs"
              let! _ = edit s efsi "module Deps.E\n\n/// Adds one\nval f: x: int -> int\n\nval g: int\n"
              let! _ = edit s efs "module Deps.E\n\nlet f y = y + 1\n\nlet g = f 1\n"
              let entered = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)
              let release = new ManualResetEventSlim(false)

              let tip (v: FileView) =
                  let (FSharp.Compiler.EditorServices.ToolTipText elements) =
                      v.Result.CheckResults.GetToolTip(3, 5, "let f y = y + 1", [ "f" ], FSharp.Compiler.Tokenization.FSharpTokenTag.Identifier)

                  [ for e in elements do
                        match e with
                        | FSharp.Compiler.EditorServices.ToolTipElement.Group items ->
                            for i in items do
                                yield i.MainDescription.Text

                                match i.XmlDoc with
                                | FSharp.Compiler.Symbols.FSharpXmlDoc.FromXmlText x -> yield! x.GetElaboratedXmlLines()
                                | _ -> ()
                        | _ -> () ]

              let held =
                  s.Graph.ReadFile(
                      efs,
                      (fun v ->
                          let one = tip v
                          entered.TrySetResult(()) |> ignore
                          release.Wait(TimeSpan.FromSeconds 60.) |> ignore
                          one, tip v),
                      CancellationToken.None
                  )

              do! entered.Task |> settle
              s.Graph.SetText(efs, "module Deps.E\n\nlet f y = y + 1\n\nlet g = f 2\n") |> ignore
              do! Task.Delay 700
              release.Set()
              let! one, two = held |> settle
              Expect.isNonEmpty one "a tooltip"
              Expect.equal two one "the tooltip does not change while the read holds the file"
              Expect.exists one (fun (l: string) -> l.Contains "Adds one") "with the signature's documentation"
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

          testTask "the check results refuse to build holding the file's check lock" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let! (view: FileView) = s.Graph.ReadFile(a, id, CancellationToken.None) |> settle

              s.Graph.Context.HoldingCheckLock(
                  a,
                  fun () -> Expect.throwsT<InvalidOperationException> (fun () -> view.Result.CheckResults |> ignore) "m2 guard"
              )
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
