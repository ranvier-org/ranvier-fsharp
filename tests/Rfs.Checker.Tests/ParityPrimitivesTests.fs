module Rfs.Checker.Tests.ParityPrimitivesTests

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Checker
open Rfs.Checker.Tests.Deps
open Rfs.Harness
open Rfs.Projects

let private a = fileOf "A.fs"
let private eSig = fileOf "E.fsi"
let private eImpl = fileOf "E.fs"

let private multiDir = Path.Combine(Path.GetDirectoryName depsDir, "Multi")
let private libProject = Path.Combine(multiDir, "Lib", "Lib.fsproj")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private topProject = Path.Combine(multiDir, "Top", "Top.fsproj")

let private readUsesOf (graph: ProjectGraph) (file: string) : Task<FileUses> =
    task {
        let! (turn: TurnRead<unit>) =
            graph.ReadInOneTurn(
                (fun () ->
                    { Loading = false
                      Projects = [| graph, [ file ] |]
                      Name = None
                      Extra = () }),
                CancellationToken.None
            )
            |> settle

        match turn with
        | TurnRead.Read(_, [| _, [| TurnFile.Indexed(_, _, uses) |] |])
        | TurnRead.Read(_, [| _, [| TurnFile.Signature(_, _, uses) |] |]) -> return uses
        | other -> return failtest $"unexpected TurnRead: %A{other}"
    }

let private keyAtSubstring (view: FileView) (needle: string) (identOffsetInNeedle: int) (identText: string) : string =
    let lines = view.Text.Split '\n' |> Array.map (fun l -> l.TrimEnd '\r')

    let lineIdx =
        lines
        |> Array.findIndex (fun l -> l.Contains(needle, StringComparison.Ordinal))

    let colStart = lines[lineIdx].IndexOf(needle, StringComparison.Ordinal) + identOffsetInNeedle
    let colEnd = colStart + identText.Length
    let line1 = lineIdx + 1
    let check = view.CheckResultsAt(line1, colEnd)

    match check.GetSymbolUseAtLocation(line1, colEnd, lines[lineIdx], [ identText ]) with
    | Some su ->
        match SymbolUses.keyOf su.Symbol with
        | Some k -> k
        | None -> failtest $"SymbolUses.keyOf returned None for %s{identText} (%A{su.Symbol})"
    | None -> failtest $"no symbol found at %d{line1}:%d{colEnd} for %s{identText} in '%s{lines[lineIdx]}'"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "ParityPrimitives"
        [ testTask "LatestParse returns immediately for both .fs and .fsi while typecheck flights are blocked" {
              let! (s: Session) = startWith FileMode.Declarations
              use _ = (s :> IAsyncDisposable)

              let gate = new ManualResetEventSlim(false)

              s.Graph.OnFlight(fun kind _ _ ->
                  if kind = "dependencies" || kind = "scoped" || kind = "declaration" || kind = "finish" then
                      gate.Wait(TimeSpan.FromSeconds 30.) |> ignore)

              let fastA = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
              let fastESig = new CancellationTokenSource(TimeSpan.FromSeconds 5.)

              try
                  let newA = s.Texts[a] + "\nlet addedInA = 999\n"
                  let vA = s.Graph.SetText(a, newA)
                  let settledA = s.Graph.SettledDiagnostics a

                  let! (viewA: TextView) = s.Graph.LatestParse(a, fastA.Token) |> settle
                  Expect.equal viewA.Version vA "LatestParse on .fs returns new version immediately"
                  Expect.equal viewA.Text newA "LatestParse on .fs returns new text"
                  Expect.isFalse viewA.Parse.ParseHadErrors "LatestParse .fs ParseTree is valid"
                  Expect.isFalse settledA.IsCompleted ".fs typecheck is still blocked while LatestParse already answered"

                  let newESig = s.Texts[eSig] + "\nval addedSigVal: int\n"
                  let vESig = s.Graph.SetText(eSig, newESig)
                  let settledESig = s.Graph.SettledDiagnostics eSig

                  let! (viewESig: TextView) = s.Graph.LatestParse(eSig, fastESig.Token) |> settle
                  Expect.equal viewESig.Version vESig "LatestParse on .fsi returns new version immediately"
                  Expect.equal viewESig.Text newESig "LatestParse on .fsi returns new text"
                  Expect.isFalse viewESig.Parse.ParseHadErrors "LatestParse .fsi ParseTree is valid"
                  Expect.isFalse settledESig.IsCompleted ".fsi settle is still blocked while LatestParse already answered"
              finally
                  fastA.Dispose()
                  fastESig.Dispose()
                  s.Graph.OnFlight(fun _ _ _ -> ())
                  gate.Set()
                  gate.Dispose()

              let! (_: IReadOnlyDictionary<string, FSharpDiagnostic[]>) = s.Graph.AllDiagnostics() |> settle
              ()
          }

          testTask "ReadSignature returns consistent TextView, FileSignature with SemanticClassification, and FileUses, and rejects .fs" {
              let! (s: Session) = startWith FileMode.Declarations
              use _ = (s :> IAsyncDisposable)

              Expect.throwsT<ArgumentException>
                  (fun () -> s.Graph.ReadSignature(a, CancellationToken.None) |> ignore)
                  "ReadSignature rejects implementation .fs file"

              let updatedSig =
                  """module Deps.E

type SigRecord = { Count: int }

type ISigContract =
    abstract Ping: unit -> int

type SigImplementer =
    interface ISigContract

val user: int
val compute: unit -> int
val unrelated: int
"""

              let updatedImpl =
                  """module Deps.E

type SigRecord = { Count: int }

type ISigContract =
    abstract Ping: unit -> int

type SigImplementer =
    interface ISigContract with
        member _.Ping() = 1

let user = 2
let compute () = 6
let unrelated = 7
"""

              let vSig = s.Graph.SetText(eSig, updatedSig)
              let _ = s.Graph.SetText(eImpl, updatedImpl)

              let! (struct (tv: TextView, fileSig: FileSignature, uses: FileUses)) =
                  s.Graph.ReadSignature(eSig, CancellationToken.None) |> settle

              let! (iSigKey: string) =
                  s.Graph.ReadFile(
                      eImpl,
                      (fun (view: FileView) -> keyAtSubstring view "type ISigContract =" 5 "ISigContract"),
                      CancellationToken.None
                  )
                  |> settle

              Expect.equal tv.Version vSig "ReadSignature returns settled version of .fsi"
              Expect.equal tv.Text updatedSig "ReadSignature returns updated .fsi text"
              Expect.isNonEmpty fileSig.SemanticClassification "ReadSignature FileSignature carries non-empty SemanticClassification"
              Expect.isGreaterThan uses.Keys 0 "ReadSignature FileUses indexed signature symbols"
              Expect.isEmpty (uses.ImplementationsOf iSigKey) "signature file FileUses does not index non-concrete signature declarations as implementations"
          }

          testTask "LatestParsesInOneTurn reads multiple graphs atomically without waiting on typecheck, handles loading and disposal" {
              Fixtures.build topProject
              let! (libSpec: ProjectSpec) = ProjectSpec.load libProject
              let! (appSpec: ProjectSpec) = ProjectSpec.load appProject
              let host = GraphHost.Start "latest-parses-multi"

              try
                  let! (lib: ProjectGraph) = ProjectGraph.CreateWithMode(host, libSpec, FileMode.Declarations)
                  let! (_: IReadOnlyDictionary<string, FSharpDiagnostic[]>) = lib.AllDiagnostics() |> settle
                  let! (app: ProjectGraph) = ProjectGraph.CreateWithMode(host, appSpec, FileMode.Declarations, [ lib ])
                  let! (_: IReadOnlyDictionary<string, FSharpDiagnostic[]>) = app.AllDiagnostics() |> settle
                  do! whenPromoted lib libSpec
                  do! whenPromoted app appSpec

                  let libFile = libSpec.SourceFiles |> List.head
                  let appFile = appSpec.SourceFiles |> List.head
                  let libOrig = File.ReadAllText libFile
                  let appOrig = File.ReadAllText appFile

                  let release = new ManualResetEventSlim(false)

                  lib.OnFlight(fun kind _ _ ->
                      if not (kind.StartsWith("parse", StringComparison.Ordinal)) then
                          release.Wait(TimeSpan.FromSeconds 30.) |> ignore)

                  app.OnFlight(fun kind _ _ ->
                      if not (kind.StartsWith("parse", StringComparison.Ordinal)) then
                          release.Wait(TimeSpan.FromSeconds 30.) |> ignore)

                  try
                      let libNew = libOrig + "\nlet fromLibTurn = 123\n"
                      let appNew = appOrig + "\nlet fromAppTurn = 456\n"
                      let vLib = lib.SetText(libFile, libNew)
                      let vApp = app.SetText(appFile, appNew)
                      let settledApp = app.SettledDiagnostics appFile

                      let mutable isLoading = true

                      let waitingTask =
                          app.LatestParsesInOneTurn(
                              (fun () ->
                                  if isLoading then
                                      struct (true, [||])
                                  else
                                      struct (false, [| lib, [ libFile ]; app, [ appFile ] |])),
                              CancellationToken.None
                          )

                      do! host.Read(fun () -> ())
                      Expect.isFalse waitingTask.IsCompleted "LatestParsesInOneTurn waits while loading is true"

                      host.Post(fun () -> isLoading <- false)

                      let! (turnRes: (ProjectGraph * (string * TextView)[])[] voption) = waitingTask |> settle

                      match turnRes with
                      | ValueSome [| (g1, [| (p1, tv1) |]); (g2, [| (p2, tv2) |]) |] ->
                          Expect.isTrue (obj.ReferenceEquals(g1, lib)) "first graph is lib"
                          Expect.isTrue (obj.ReferenceEquals(g2, app)) "second graph is app"
                          Expect.equal p1 libFile "lib file path"
                          Expect.equal p2 appFile "app file path"
                          Expect.equal tv1.Version vLib "lib latest parse version while typecheck is blocked"
                          Expect.equal tv2.Version vApp "app latest parse version while typecheck is blocked"
                          Expect.equal tv1.Text libNew "lib latest parse text"
                          Expect.equal tv2.Text appNew "app latest parse text"
                          Expect.isFalse settledApp.IsCompleted "typecheck still blocked while LatestParsesInOneTurn completed"
                      | other -> failtest $"unexpected LatestParsesInOneTurn result: %A{other}"
                  finally
                      lib.OnFlight(fun _ _ _ -> ())
                      app.OnFlight(fun _ _ _ -> ())
                      release.Set()
                      release.Dispose()

                  // Cross-host graph throws InvalidOperationException
                  let otherHost = GraphHost.Start "other-host"

                  try
                      let! (otherLib: ProjectGraph) = ProjectGraph.CreateWithMode(otherHost, libSpec, FileMode.FileLevel)

                      let! (crossHostEx: bool) =
                          task {
                              try
                                  let! (_: (ProjectGraph * (string * TextView)[])[] voption) =
                                      app.LatestParsesInOneTurn(
                                          (fun () -> struct (false, [| otherLib, [ libFile ] |])),
                                          CancellationToken.None
                                      )
                                      |> settle

                                  return false
                              with :? InvalidOperationException ->
                                  return true
                          }

                      Expect.isTrue crossHostEx "LatestParsesInOneTurn rejects graphs on another GraphHost"
                  finally
                      otherHost.Dispose()

                  // Disposed graph in scope returns ValueNone
                  (app :> IDisposable).Dispose()

                  let! (afterDispose: (ProjectGraph * (string * TextView)[])[] voption) =
                      lib.LatestParsesInOneTurn(
                          (fun () -> struct (false, [| lib, [ libFile ]; app, [ appFile ] |])),
                          CancellationToken.None
                      )
                      |> settle

                  Expect.isTrue afterDispose.IsNone "disposed graph in scope yields ValueNone"
              finally
                  host.Dispose()
          }

          testTask "LatestParse, ReadSignature, and LatestParsesInOneTurn track Restart and FailRestart" {
              Fixtures.build topProject
              let! (libSpec: ProjectSpec) = ProjectSpec.load libProject
              let! (appSpec: ProjectSpec) = ProjectSpec.load appProject
              let tempDir = Path.Combine(Path.GetTempPath(), "rfs-parity-restart-" + Guid.NewGuid().ToString("N"))
              Directory.CreateDirectory tempDir |> ignore
              let tempUseFsi = Path.Combine(tempDir, "Use.fsi")
              let tempUseFs = Path.Combine(tempDir, "Use.fs")
              let libFile = Path.Combine(multiDir, "Lib", "Lib.fs")
              let libOrig = File.ReadAllText libFile
              let appOrig = File.ReadAllText(appSpec.SourceFiles |> List.head)

              let sigOrig =
                  """module App.Use

val y: int
val w: int
val z: int
val shape: Lib.Shapes.Shape
val sized: int
"""

              File.WriteAllText(tempUseFsi, sigOrig)
              File.WriteAllText(tempUseFs, appOrig)

              let appSpecWithSig = { appSpec with SourceFiles = [ tempUseFsi; tempUseFs ] }
              let host = GraphHost.Start "parity-restart-tracking"

              try
                  let! (lib: ProjectGraph) = ProjectGraph.CreateWithMode(host, libSpec, FileMode.FileLevel)
                  let! (_: IReadOnlyDictionary<string, FSharpDiagnostic[]>) = lib.AllDiagnostics() |> settle
                  let! (app: ProjectGraph) = ProjectGraph.CreateWithMode(host, appSpecWithSig, FileMode.FileLevel, [ lib ])
                  let! (_: IReadOnlyDictionary<string, FSharpDiagnostic[]>) = app.AllDiagnostics() |> settle

                  // Part A: LatestParse held on pre-restart parse flight wakes and completes on post-restart node
                  let mutable holdFirstFsParse = 1
                  let enteredFsParse = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                  app.OnFlight(fun kind path ct ->
                      if kind = "parse" && String.Equals(path, tempUseFs, StringComparison.OrdinalIgnoreCase) && Interlocked.CompareExchange(&holdFirstFsParse, 0, 1) = 1 then
                          enteredFsParse.TrySetResult(()) |> ignore

                          try
                              Task.Delay(Timeout.Infinite, ct).GetAwaiter().GetResult()
                          with :? OperationCanceledException ->
                              ())

                  let newSig1 = sigOrig + "\nval afterRestartVal: int\n"
                  let newFs1 = appOrig + "\nlet afterRestartVal = 1\n"
                  let _ = app.SetText(tempUseFsi, newSig1)
                  let vUseFs = app.SetText(tempUseFs, newFs1)
                  let! () = enteredFsParse.Task |> settle

                  let lpCts = new CancellationTokenSource(TimeSpan.FromSeconds 8.)

                  try
                      let lpTask = app.LatestParse(tempUseFs, lpCts.Token)
                      do! host.Read(fun () -> ())
                      Expect.isFalse lpTask.IsCompleted "LatestParse pending before Restart"

                      let _ = lib.SetText(libFile, libOrig + "\nlet libRestart1 = 101\n")
                      let! (tv: TextView) = lpTask
                      let! (_: IReadOnlyDictionary<string, FSharpDiagnostic[]>) = app.AllDiagnostics() |> settle
                      let! restartsAfterLp = host.Read(fun () -> app.Restarts)
                      Expect.equal tv.Version vUseFs "LatestParse completes with latest version after Restart"
                      Expect.equal tv.Text newFs1 "LatestParse returns latest text after Restart"
                      Expect.isGreaterThanOrEqual restartsAfterLp 1L "App restarted during LatestParse"
                  finally
                      lpCts.Dispose()
                      app.OnFlight(fun _ _ _ -> ())

                  // Part B: ReadSignature held on pre-restart parse flight wakes and completes on post-restart node
                  let mutable holdFirstFsiParse = 1
                  let enteredFsiParse = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                  app.OnFlight(fun kind path ct ->
                      if kind = "parse" && String.Equals(path, tempUseFsi, StringComparison.OrdinalIgnoreCase) && Interlocked.CompareExchange(&holdFirstFsiParse, 0, 1) = 1 then
                          enteredFsiParse.TrySetResult(()) |> ignore

                          try
                              Task.Delay(Timeout.Infinite, ct).GetAwaiter().GetResult()
                          with :? OperationCanceledException ->
                              ())

                  let newSig2 = newSig1 + "\nval secondRestartVal: int\n"
                  let newFs2 = newFs1 + "\nlet secondRestartVal = 2\n"
                  let vUseFsi2 = app.SetText(tempUseFsi, newSig2)
                  let _ = app.SetText(tempUseFs, newFs2)
                  let! () = enteredFsiParse.Task |> settle

                  let rsCts = new CancellationTokenSource(TimeSpan.FromSeconds 8.)

                  try
                      let rsTask = app.ReadSignature(tempUseFsi, rsCts.Token)
                      do! host.Read(fun () -> ())
                      Expect.isFalse rsTask.IsCompleted "ReadSignature pending before Restart"

                      let _ = lib.SetText(libFile, libOrig + "\nlet libRestart1 = 101\nlet libRestart2 = 202\n")
                      let! (struct (sigTv: TextView, sigRes: FileSignature, sigUses: FileUses)) = rsTask
                      let! restartsAfterRs = host.Read(fun () -> app.Restarts)
                      Expect.equal sigTv.Version vUseFsi2 "ReadSignature completes with latest version after Restart"
                      Expect.equal sigTv.Text newSig2 "ReadSignature returns latest text after Restart"
                      Expect.isNonEmpty sigRes.SemanticClassification "ReadSignature carries post-restart semantic classification"
                      Expect.isGreaterThan sigUses.Keys 0 "ReadSignature carries post-restart FileUses"
                      Expect.isGreaterThanOrEqual restartsAfterRs 2L "App restarted during ReadSignature"
                  finally
                      rsCts.Dispose()
                      app.OnFlight(fun _ _ _ -> ())

                  // Part C: FailRestart wakes and faults LatestParse, ReadSignature, and LatestParsesInOneTurn
                  let holdFailGate = new ManualResetEventSlim(false)

                  app.OnFlight(fun kind _ ct ->
                      if kind = "parse" then
                          try
                              holdFailGate.Wait(ct)
                          with :? OperationCanceledException ->
                              ())

                  let failCts = new CancellationTokenSource(TimeSpan.FromSeconds 8.)

                  try
                      let _ = app.SetText(tempUseFsi, newSig2 + "\nval afterFail: int\n")
                      let _ = app.SetText(tempUseFs, newFs2 + "\nlet afterFail = 3\n")
                      let lpFail = app.LatestParse(tempUseFs, failCts.Token)
                      let rsFail = app.ReadSignature(tempUseFsi, failCts.Token)

                      let turnFail =
                          app.LatestParsesInOneTurn(
                              (fun () -> struct (false, [| app, [ tempUseFs ] |])),
                              failCts.Token
                          )

                      do! host.Read(fun () -> ())
                      app.FailRestart(InvalidOperationException "simulated restart failure")

                      let expectFaulted (label: string) (t: Task<'T>) =
                          task {
                              try
                                  let! _ = t
                                  failtest $"%s{label} should have faulted on FailRestart"
                              with
                              | :? InvalidOperationException
                              | :? GraphFailedException -> ()
                          }

                      do! expectFaulted "LatestParse" lpFail
                      do! expectFaulted "ReadSignature" rsFail
                      do! expectFaulted "LatestParsesInOneTurn" turnFail

                      let turnAfterFail =
                          app.LatestParsesInOneTurn(
                              (fun () -> struct (false, [| app, [ tempUseFs ] |])),
                              failCts.Token
                          )

                      do! expectFaulted "LatestParsesInOneTurn after FailRestart" turnAfterFail
                  finally
                      app.OnFlight(fun _ _ _ -> ())
                      holdFailGate.Set()
                      holdFailGate.Dispose()
                      failCts.Dispose()
              finally
                  host.Dispose()

                  try
                      Directory.Delete(tempDir, true)
                  with _ ->
                      ()
          }

          testTask "FileUses.ImplementationsOf finds interface impls, object expressions, and abstract class overrides" {
              let! (s: Session) = startWith FileMode.Declarations
              use _ = (s :> IAsyncDisposable)

              let code =
                  """module Deps.A

let baseValue = 10

type IWorker =
    abstract Work: int -> int
    abstract Tag: string

type ISubWorker =
    inherit IWorker
    abstract SubWork: int -> int

[<AbstractClass>]
type BaseJob() =
    abstract Execute: int -> string
    abstract Code: int
    default _.Code = 0

[<AbstractClass>]
type MidJob() =
    inherit BaseJob()
    abstract Step: int -> int

type ConcreteWorker() =
    interface IWorker with
        member _.Work x = x + 1
        member _.Tag = "concrete"

type ConcreteSubWorker() =
    interface ISubWorker with
        member _.Work x = x + 10
        member _.Tag = "subconcrete"
        member _.SubWork x = x + 20

type ConcreteJob() =
    inherit BaseJob()
    override _.Execute n = string n
    override _.Code = 42

type LeafJob() =
    inherit MidJob()
    override _.Execute n = $"leaf:%d{n}"
    override _.Step n = n + 1

let makeAnonWorker () =
    { new IWorker with
        member _.Work x = x * 2
        member _.Tag = "anon" }

let makeAnonSubWorker () =
    { new ISubWorker with
        member _.Work x = x * 3
        member _.Tag = "anonsub"
        member _.SubWork x = x * 4 }

let makeAnonJob () =
    { new BaseJob() with
        override _.Execute n = $"anon:%d{n}" }

let makeAnonMidJob () =
    { new MidJob() with
        override _.Execute n = $"anonMid:%d{n}"
        override _.Step n = n + 2 }
"""

              let _ = s.Graph.SetText(a, code)

              let! (keys: string * string * string * string * string * string * string * string * string * string) =
                  s.Graph.ReadFile(
                      a,
                      (fun (view: FileView) ->
                          let iWorkerKey = keyAtSubstring view "type IWorker =" 5 "IWorker"
                          let iSubWorkerKey = keyAtSubstring view "type ISubWorker =" 5 "ISubWorker"
                          let workSlotKey = keyAtSubstring view "abstract Work:" 9 "Work"
                          let tagSlotKey = keyAtSubstring view "abstract Tag:" 9 "Tag"
                          let baseJobKey = keyAtSubstring view "type BaseJob() =" 5 "BaseJob"
                          let midJobKey = keyAtSubstring view "type MidJob() =" 5 "MidJob"
                          let concreteJobKey = keyAtSubstring view "type ConcreteJob() =" 5 "ConcreteJob"
                          let leafJobKey = keyAtSubstring view "type LeafJob() =" 5 "LeafJob"
                          let executeSlotKey = keyAtSubstring view "abstract Execute:" 9 "Execute"
                          let codeSlotKey = keyAtSubstring view "abstract Code:" 9 "Code"
                          iWorkerKey, iSubWorkerKey, workSlotKey, tagSlotKey, baseJobKey, midJobKey, concreteJobKey, leafJobKey, executeSlotKey, codeSlotKey),
                      CancellationToken.None
                  )
                  |> settle

              let iWorkerKey, iSubWorkerKey, workSlotKey, tagSlotKey, baseJobKey, midJobKey, concreteJobKey, leafJobKey, executeSlotKey, codeSlotKey = keys
              let! (uses: FileUses) = readUsesOf s.Graph a

              let linesOf (ranges: range[]) =
                  ranges |> Array.map (fun (r: range) -> r.StartLine) |> Array.sort

              // IWorker implemented by ConcreteWorker (24), ConcreteSubWorker transitively via ISubWorker (29),
              // object expression { new IWorker } (46), and object expression { new ISubWorker } transitively (51);
              // NOT by non-concrete interface ISubWorker (9)!
              let iWorkerImpls = uses.ImplementationsOf iWorkerKey |> linesOf
              Expect.equal iWorkerImpls [| 24; 29; 46; 51 |] "IWorker concrete implementations include transitive class and object expression impls and exclude interface ISubWorker"

              // IWorker.Work implemented on ConcreteWorker (26), ConcreteSubWorker (31), anon IWorker (47), anon ISubWorker (52)
              Expect.equal (uses.ImplementationsOf workSlotKey |> linesOf) [| 26; 31; 47; 52 |] "IWorker.Work implementations"

              // IWorker.Tag implemented on ConcreteWorker (27), ConcreteSubWorker (32), anon IWorker (48), anon ISubWorker (53)
              Expect.equal (uses.ImplementationsOf tagSlotKey |> linesOf) [| 27; 32; 48; 53 |] "IWorker.Tag property implementations"

              // BaseJob implemented by ConcreteJob (35), LeafJob transitively via MidJob (40),
              // object expression { new BaseJob() } (57), and object expression { new MidJob() } transitively (61);
              // NOT by [<AbstractClass>] MidJob (20)!
              let baseJobImpls = uses.ImplementationsOf baseJobKey |> linesOf
              Expect.equal baseJobImpls [| 35; 40; 57; 61 |] "BaseJob concrete implementations include transitive LeafJob and { new MidJob() } and exclude abstract MidJob"

              // Meanwhile SubtypesOf stays strictly immediate (single-step hierarchy)
              Expect.equal
                  (uses.SubtypesOf iWorkerKey |> Array.map (fun x -> x.Key) |> Array.sort)
                  ([| iSubWorkerKey; (uses.SubtypesOf iWorkerKey |> Array.find (fun x -> x.Name = "ConcreteWorker")).Key |] |> Array.sort)
                  "SubtypesOf IWorker stays strictly immediate (ISubWorker and ConcreteWorker, not ConcreteSubWorker)"

              Expect.equal
                  (uses.SubtypesOf baseJobKey |> Array.map (fun x -> x.Key) |> Array.sort)
                  ([| concreteJobKey; midJobKey |] |> Array.sort)
                  "SubtypesOf BaseJob stays strictly immediate (ConcreteJob and MidJob, not LeafJob)"

              Expect.equal
                  (uses.SubtypesOf midJobKey |> Array.map (fun x -> x.Key))
                  [| leafJobKey |]
                  "SubtypesOf MidJob has immediate LeafJob"

              // BaseJob.Execute overridden on ConcreteJob (37), LeafJob (42), anon BaseJob (58), anon MidJob (62)
              Expect.equal (uses.ImplementationsOf executeSlotKey |> linesOf) [| 37; 42; 58; 62 |] "BaseJob.Execute override implementations"

              // BaseJob.Code has default on BaseJob (17) and override on ConcreteJob (38)
              Expect.contains (uses.ImplementationsOf codeSlotKey |> linesOf) 38 "BaseJob.Code override on ConcreteJob"
          }

          testTask "FileUses.TypeOf and SubtypesOf index both BaseType and DeclaredInterfaces with full and selection ranges" {
              let! (s: Session) = startWith FileMode.Declarations
              use _ = (s :> IAsyncDisposable)

              let code =
                  """module Deps.A

let baseValue = 10

type IAlpha =
    abstract A: int

type IBeta =
    abstract B: string

[<AbstractClass>]
type AbstractRoot() =
    abstract Root: unit -> unit

type ChildNode() =
    inherit AbstractRoot()
    override _.Root() = ()
    interface IAlpha with
        member _.A = 1
    interface IBeta with
        member _.B = "b"

[<Struct>]
type ValBox =
    val X: int
"""

              let _ = s.Graph.SetText(a, code)

              let! (keys: string * string * string * string * string) =
                  s.Graph.ReadFile(
                      a,
                      (fun (view: FileView) ->
                          keyAtSubstring view "type IAlpha =" 5 "IAlpha",
                          keyAtSubstring view "type IBeta =" 5 "IBeta",
                          keyAtSubstring view "type AbstractRoot() =" 5 "AbstractRoot",
                          keyAtSubstring view "type ChildNode() =" 5 "ChildNode",
                          keyAtSubstring view "type ValBox =" 5 "ValBox"),
                      CancellationToken.None
                  )
                  |> settle

              let iAlphaKey, iBetaKey, rootKey, childKey, valBoxKey = keys
              let! (uses: FileUses) = readUsesOf s.Graph a

              match uses.TypeOf childKey with
              | ValueSome entry ->
                  Expect.equal entry.Name "ChildNode" "TypeOf Name"
                  Expect.equal entry.Glyph SymbolGlyph.Class "ChildNode Glyph is Class"
                  Expect.contains entry.SuperKeys rootKey "ChildNode SuperKeys includes BaseType AbstractRoot"
                  Expect.contains entry.SuperKeys iAlphaKey "ChildNode SuperKeys includes interface IAlpha"
                  Expect.contains entry.SuperKeys iBetaKey "ChildNode SuperKeys includes interface IBeta"
                  Expect.equal entry.SuperKeys.Length 3 "plain System.Object is not polluting SuperKeys"
                  Expect.equal entry.SelectionRange.StartLine 15 "SelectionRange is on ChildNode header line"
                  Expect.isLessThan entry.Range.StartLine entry.Range.EndLine "full Range spans multi-line type definition"
                  Expect.equal entry.Range.EndLine 21 "full Range reaches end of ChildNode body"
              | ValueNone -> failtest "expected TypeOf ChildNode"

              match uses.TypeOf iAlphaKey with
              | ValueSome entry -> Expect.equal entry.Glyph SymbolGlyph.Interface "IAlpha Glyph is Interface"
              | ValueNone -> failtest "expected TypeOf IAlpha"

              match uses.TypeOf valBoxKey with
              | ValueSome entry -> Expect.equal entry.Glyph SymbolGlyph.Struct "ValBox Glyph is Struct"
              | ValueNone -> failtest "expected TypeOf ValBox"

              Expect.equal
                  (uses.SubtypesOf rootKey |> Array.map (fun x -> x.Key))
                  [| childKey |]
                  "SubtypesOf AbstractRoot finds ChildNode"

              Expect.equal
                  (uses.SubtypesOf iAlphaKey |> Array.map (fun x -> x.Key))
                  [| childKey |]
                  "SubtypesOf IAlpha finds ChildNode"

              Expect.equal
                  (uses.SubtypesOf iBetaKey |> Array.map (fun x -> x.Key))
                  [| childKey |]
                  "SubtypesOf IBeta finds ChildNode"
          }

          testTask "FileUses.EnclosingCallable returns innermost nested callable and respects exact start/end boundaries" {
              let! (s: Session) = startWith FileMode.Declarations
              use _ = (s :> IAsyncDisposable)

              let code =
                  """module Deps.A

open System

let baseValue = 10

let outerFunc (x: int) =
    let beforeInner = x + 1
    let innerFunc (y: int) =
        y * 2
    innerFunc beforeInner

type Calculator() =
    member _.Compute(a: int) =
        let helper (b: int) = b + a
        helper 10

[<AbstractClass>]
type BaseCtor(v: int) =
    member _.V = v

type DerivedCtor(seed: int) =
    inherit BaseCtor(seed + 1)
    do ignore (seed + 2)
    let ctorHelper (k: int) = k + seed
    member _.Run() = ctorHelper 3
"""

              let _ = s.Graph.SetText(a, code)
              let! (uses: FileUses) = readUsesOf s.Graph a

              let nameOf (opt: CallableDecl voption) =
                  opt |> ValueOption.map (fun (c: CallableDecl) -> c.Name)

              // Outside all callables (on `open System`, line 3)
              Expect.isTrue (uses.EnclosingCallable(Position.mkPos 3 5)).IsNone "open declaration has no enclosing callable"

              // Inside `outerFunc` before `innerFunc` (line 8) -> `outerFunc`
              let outerAt8 = uses.EnclosingCallable(Position.mkPos 8 10)
              Expect.isTrue outerAt8.IsSome "position on line 8 is inside outerFunc"
              Expect.equal (nameOf outerAt8) (ValueSome "outerFunc") "line 8 enclosing callable is outerFunc"

              // Inside `innerFunc` body (line 10, `y * 2`) -> MUST be `innerFunc`, NOT `outerFunc`!
              let innerAt10 = uses.EnclosingCallable(Position.mkPos 10 10)
              Expect.isTrue innerAt10.IsSome "position on line 10 is inside innerFunc"
              Expect.equal (nameOf innerAt10) (ValueSome "innerFunc") "innermost nested callable wins over outerFunc"

              let innerRange = innerAt10.Value.Range

              // Exact boundary checks on `innerFunc.Range.Start` and `innerFunc.Range.End` (kills `<` vs `<=` off-by-one mutants!)
              let atInnerStart = uses.EnclosingCallable innerRange.Start
              Expect.equal (nameOf atInnerStart) (ValueSome "innerFunc") "exact Range.Start is inside innerFunc"

              let atInnerEnd = uses.EnclosingCallable innerRange.End
              Expect.equal (nameOf atInnerEnd) (ValueSome "innerFunc") "exact Range.End is inside innerFunc"

              // One column before `innerRange.Start` (same line 9, col - 1) is still inside `outerFunc`, NOT `innerFunc`
              let justBeforeInner = Position.mkPos innerRange.StartLine (innerRange.StartColumn - 1)
              Expect.equal (nameOf (uses.EnclosingCallable justBeforeInner)) (ValueSome "outerFunc") "1 column before innerFunc.Start falls back to outerFunc"

              // One column after `innerRange.End` on line 10 is outside `innerFunc` (still inside `outerFunc`)
              let justAfterInner = Position.mkPos innerRange.EndLine (innerRange.EndColumn + 1)
              Expect.equal (nameOf (uses.EnclosingCallable justAfterInner)) (ValueSome "outerFunc") "1 column after innerFunc.End falls back to outerFunc"

              // Inside class member `Compute` vs nested `helper` (line 15 vs line 16)
              let inHelper = uses.EnclosingCallable(Position.mkPos 15 32)
              Expect.equal (nameOf inHelper) (ValueSome "helper") "nested helper inside method wins"

              let inCompute = uses.EnclosingCallable(Position.mkPos 16 10)
              Expect.equal (nameOf inCompute) (ValueSome "Compute") "method body after helper returns Compute"

              // ImplicitCtor covers `inherit BaseCtor(seed + 1)` (line 23) and `do ignore (seed + 2)` (line 24),
              // while narrower `ctorHelper` (line 25) and `Run` (line 26) still win inside their bodies.
              // Constructor CallableDecl.Name uses the declaring type's display name ("DerivedCtor") while Glyph is Constructor and Key carries "|.ctor".
              let inInherit = uses.EnclosingCallable(Position.mkPos 23 22)
              Expect.isTrue inInherit.IsSome "inherit argument has enclosing implicit constructor"
              Expect.equal (nameOf inInherit) (ValueSome "DerivedCtor") "inherit argument enclosing callable uses type display name DerivedCtor"
              Expect.equal inInherit.Value.Glyph SymbolGlyph.Constructor "inherit argument enclosing callable has Constructor glyph"
              Expect.stringContains inInherit.Value.Key "DerivedCtor" "inherit enclosing .ctor belongs to DerivedCtor"
              Expect.stringContains inInherit.Value.Key "|.ctor" "inherit enclosing .ctor key carries |.ctor"

              let inDo = uses.EnclosingCallable(Position.mkPos 24 16)
              Expect.equal (nameOf inDo) (ValueSome "DerivedCtor") "class do binding enclosing callable uses type display name DerivedCtor"
              Expect.equal inDo.Value.Glyph SymbolGlyph.Constructor "class do binding enclosing callable has Constructor glyph"
              Expect.equal inDo.Value.Key inInherit.Value.Key "class do binding resolves to same DerivedCtor .ctor"

              let inCtorHelper = uses.EnclosingCallable(Position.mkPos 25 32)
              Expect.equal (nameOf inCtorHelper) (ValueSome "ctorHelper") "class let function narrower than .ctor wins"

              let inRun = uses.EnclosingCallable(Position.mkPos 26 24)
              Expect.equal (nameOf inRun) (ValueSome "Run") "class member narrower than .ctor wins"

              // Footprint includes relational and callable tables beyond raw symbol uses
              let struct (keyedBytes, rawBytes) = uses.Footprint
              Expect.isGreaterThan keyedBytes 6000L "Footprint keyed byte estimate includes relational, subtype, and callable tables"
              Expect.isGreaterThan rawBytes 0L "Footprint raw byte estimate is positive"
          }

          testCase "AstIndex.build handles 4000-deep pipeline and nested bindings on a 256KB stack"
          <| fun () ->
              let src =
                  "module Deep\nlet topFn x =\n    (let deepLeaf () = x in deepLeaf ())"
                  + String.replicate 4000 " |> id"

              let checker = FSharpChecker.Create()
              let parseOpts = { FSharpParsingOptions.Default with SourceFiles = [| "Deep.fs" |] }
              let parseRes =
                  checker.ParseFile("Deep.fs", SourceText.ofString src, parseOpts)
                  |> Async.RunSynchronously

              let mutable callableCount = -1
              let mutable failure: exn option = None

              let worker =
                  System.Threading.Thread(
                      System.Threading.ThreadStart(fun () ->
                          try
                              let info = AstIndex.build (Some parseRes.ParseTree)
                              callableCount <- info.CallableRanges.Count
                          with ex ->
                              failure <- Some ex),
                      262144
                  )

              worker.Start()
              worker.Join()
              Expect.isNone failure "AstIndex.build must not overflow or fail on a 256KB stack"
              Expect.equal callableCount 2 "Records both top-level binding and nested function inside deep pipeline" ]
