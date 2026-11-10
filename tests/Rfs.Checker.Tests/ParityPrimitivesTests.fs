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
open FSharp.Compiler.EditorServices
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

val user: int
val compute: unit -> int
val unrelated: int
"""

              let updatedImpl =
                  """module Deps.E

type SigRecord = { Count: int }

let user = 2
let compute () = 6
let unrelated = 7
"""

              let vSig = s.Graph.SetText(eSig, updatedSig)
              let _ = s.Graph.SetText(eImpl, updatedImpl)

              let! (struct (tv: TextView, fileSig: FileSignature, uses: FileUses)) =
                  s.Graph.ReadSignature(eSig, CancellationToken.None) |> settle

              Expect.equal tv.Version vSig "ReadSignature returns settled version of .fsi"
              Expect.equal tv.Text updatedSig "ReadSignature returns updated .fsi text"
              Expect.isNonEmpty fileSig.SemanticClassification "ReadSignature FileSignature carries non-empty SemanticClassification"
              Expect.isGreaterThan uses.Keys 0 "ReadSignature FileUses indexed signature symbols"
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
                  lib.OnFlight(fun _ _ _ -> release.Wait(TimeSpan.FromSeconds 30.) |> ignore)
                  app.OnFlight(fun _ _ _ -> release.Wait(TimeSpan.FromSeconds 30.) |> ignore)

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

                      do! Task.Delay 80
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

          testTask "FileUses.ImplementationsOf finds interface impls, object expressions, and abstract class overrides" {
              let! (s: Session) = startWith FileMode.Declarations
              use _ = (s :> IAsyncDisposable)

              let code =
                  """module Deps.A

let baseValue = 10

type IWorker =
    abstract Work: int -> int
    abstract Tag: string

[<AbstractClass>]
type BaseJob() =
    abstract Execute: int -> string
    abstract Code: int
    default _.Code = 0

type ConcreteWorker() =
    interface IWorker with
        member _.Work x = x + 1
        member _.Tag = "concrete"

type ConcreteJob() =
    inherit BaseJob()
    override _.Execute n = string n
    override _.Code = 42

let makeAnonWorker () =
    { new IWorker with
        member _.Work x = x * 2
        member _.Tag = "anon" }

let makeAnonJob () =
    { new BaseJob() with
        override _.Execute n = $"anon:%d{n}" }
"""

              let _ = s.Graph.SetText(a, code)

              let! (keys: string * string * string * string * string * string) =
                  s.Graph.ReadFile(
                      a,
                      (fun (view: FileView) ->
                          let iWorkerKey = keyAtSubstring view "type IWorker =" 5 "IWorker"
                          let workSlotKey = keyAtSubstring view "abstract Work:" 9 "Work"
                          let tagSlotKey = keyAtSubstring view "abstract Tag:" 9 "Tag"
                          let baseJobKey = keyAtSubstring view "type BaseJob() =" 5 "BaseJob"
                          let executeSlotKey = keyAtSubstring view "abstract Execute:" 9 "Execute"
                          let codeSlotKey = keyAtSubstring view "abstract Code:" 9 "Code"
                          iWorkerKey, workSlotKey, tagSlotKey, baseJobKey, executeSlotKey, codeSlotKey),
                      CancellationToken.None
                  )
                  |> settle

              let iWorkerKey, workSlotKey, tagSlotKey, baseJobKey, executeSlotKey, codeSlotKey = keys
              let! (uses: FileUses) = readUsesOf s.Graph a

              let linesOf (ranges: range[]) =
                  ranges |> Array.map (fun (r: range) -> r.StartLine) |> Array.sort

              // IWorker implemented by ConcreteWorker (line 15) and object expression (line 26)
              let iWorkerImpls = uses.ImplementationsOf iWorkerKey |> linesOf
              Expect.contains iWorkerImpls 15 "IWorker implementations include ConcreteWorker type"
              Expect.contains iWorkerImpls 26 "IWorker implementations include object expression"

              // IWorker.Work implemented on line 17 and line 27
              Expect.equal (uses.ImplementationsOf workSlotKey |> linesOf) [| 17; 27 |] "IWorker.Work implementations"

              // IWorker.Tag implemented on line 18 and line 28
              Expect.equal (uses.ImplementationsOf tagSlotKey |> linesOf) [| 18; 28 |] "IWorker.Tag property implementations"

              // BaseJob implemented/subclassed by ConcreteJob (line 20) and object expression (line 31)
              let baseJobImpls = uses.ImplementationsOf baseJobKey |> linesOf
              Expect.contains baseJobImpls 20 "BaseJob implementations include ConcreteJob"
              Expect.contains baseJobImpls 31 "BaseJob implementations include object expression"

              // BaseJob.Execute overridden on line 22 and line 32
              Expect.equal (uses.ImplementationsOf executeSlotKey |> linesOf) [| 22; 32 |] "BaseJob.Execute override implementations"

              // BaseJob.Code overridden on line 23
              Expect.contains (uses.ImplementationsOf codeSlotKey |> linesOf) 23 "BaseJob.Code override on ConcreteJob"
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
                  Expect.equal entry.Glyph FSharpGlyph.Class "ChildNode Glyph is Class"
                  Expect.contains entry.SuperKeys rootKey "ChildNode SuperKeys includes BaseType AbstractRoot"
                  Expect.contains entry.SuperKeys iAlphaKey "ChildNode SuperKeys includes interface IAlpha"
                  Expect.contains entry.SuperKeys iBetaKey "ChildNode SuperKeys includes interface IBeta"
                  Expect.equal entry.SuperKeys.Length 3 "plain System.Object is not polluting SuperKeys"
                  Expect.equal entry.SelectionRange.StartLine 15 "SelectionRange is on ChildNode header line"
                  Expect.isLessThan entry.Range.StartLine entry.Range.EndLine "full Range spans multi-line type definition"
                  Expect.equal entry.Range.EndLine 21 "full Range reaches end of ChildNode body"
              | ValueNone -> failtest "expected TypeOf ChildNode"

              match uses.TypeOf iAlphaKey with
              | ValueSome entry -> Expect.equal entry.Glyph FSharpGlyph.Interface "IAlpha Glyph is Interface"
              | ValueNone -> failtest "expected TypeOf IAlpha"

              match uses.TypeOf valBoxKey with
              | ValueSome entry -> Expect.equal entry.Glyph FSharpGlyph.Struct "ValBox Glyph is Struct"
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
          } ]
