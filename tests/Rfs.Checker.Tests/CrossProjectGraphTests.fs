module Rfs.Checker.Tests.CrossProjectGraphTests

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Checker.Tests.Deps
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects

let private multiDir = Path.Combine(Path.GetDirectoryName depsDir, "Multi")
let private libProject = Path.Combine(multiDir, "Lib", "Lib.fsproj")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private topProject = Path.Combine(multiDir, "Top", "Top.fsproj")
let private shapes = Path.Combine(multiDir, "Lib", "Shapes.fs")
let private signed = Path.Combine(multiDir, "Lib", "Signed.fs")
let private libFs = Path.Combine(multiDir, "Lib", "Lib.fs")
let private useFs = Path.Combine(multiDir, "App", "Use.fs")

let private built =
    lazy
        (let psi = ProcessStartInfo("dotnet", $"build \"{topProject}\" -v q --nologo")
         psi.RedirectStandardOutput <- true
         psi.RedirectStandardError <- true
         use p = Process.Start psi
         let out = p.StandardOutput.ReadToEndAsync()
         let err = p.StandardError.ReadToEndAsync()
         p.WaitForExit()

         if p.ExitCode <> 0 then
             failwith $"building the Multi fixture failed: {out.Result}{err.Result}")

type private Multi =
    { Host: GraphHost
      Oracle: OracleClient
      Lib: ProjectGraph
      LibSpec: ProjectSpec
      App: ProjectGraph
      AppSpec: ProjectSpec
      Texts: System.Collections.Generic.Dictionary<string, string> }

    interface IAsyncDisposable with
        member s.DisposeAsync() =
            s.Host.Dispose()
            (s.Oracle :> IAsyncDisposable).DisposeAsync()

let private startMulti (mode: FileMode) : Task<Multi> =
    task {
        built.Force()
        let! libSpec = ProjectSpec.load libProject
        let! appSpec = ProjectSpec.load appProject
        let host = GraphHost.Start "multi"
        let! (lib: ProjectGraph) = ProjectGraph.CreateWithMode(host, libSpec, mode)
        let! _ = lib.AllDiagnostics() |> settle
        let! (app: ProjectGraph) = ProjectGraph.CreateWithMode(host, appSpec, mode, [ lib ])
        let! _ = app.AllDiagnostics() |> settle
        do! whenPromoted lib libSpec
        do! whenPromoted app appSpec
        let! oracle = OracleClient.start ()
        let texts = System.Collections.Generic.Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for f in libSpec.SourceFiles @ appSpec.SourceFiles do
            texts[f] <- File.ReadAllText f

        return
            { Host = host
              Oracle = oracle
              Lib = lib
              LibSpec = libSpec
              App = app
              AppSpec = appSpec
              Texts = texts }
    }

let private graphOf (s: Multi) (file: string) =
    if s.LibSpec.SourceFiles |> List.exists (fun f -> String.Equals(f, file, StringComparison.OrdinalIgnoreCase)) then s.Lib else s.App

let private set (s: Multi) (file: string) (text: string) =
    s.Texts[file] <- text
    (graphOf s file).SetText(file, text) |> ignore

let private change (s: Multi) (file: string) (oldText: string) (newText: string) =
    if not (s.Texts[file].Contains oldText) then
        failtest $"%s{Path.GetFileName file} does not contain %s{oldText}"

    set s file (s.Texts[file].Replace(oldText, newText))

let private settled (s: Multi) =
    task {
        let! _ = s.Lib.AllDiagnostics() |> settle
        let! app = s.App.AllDiagnostics() |> settle
        return [ for kv in app do yield! kv.Value |> Seq.map FcsDiagnostics.ofFcs ]
    }

/// App's diagnostics equal the oracle's for the current texts, where App's are all it reports of App
let private expectAppOracle (s: Multi) (what: string) =
    task {
        let! actual = settled s
        let! whole = s.Oracle.Check appProject (s.Texts |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
        let inApp (d: Diagnostic) = String.Equals(d.File, LspClient.NormalizePath useFs, StringComparison.OrdinalIgnoreCase)
        let diff = Diagnostics.compare (whole |> List.filter inApp) actual
        Expect.isEmpty diff $"""%s{what}: App equals a full check:{Environment.NewLine}%s{String.concat Environment.NewLine diff}"""
        return actual
    }

let private settleHolding (hold: ManualResetEventSlim) (t: Task<'T>) =
    task {
        hold.Set()
        return! t |> settle
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "CrossProject"
        [ testTask "a kept file holding an earlier file's old entity sees one entity" {
              let! s = start ()
              use _ = (s :> IAsyncDisposable)
              let a, b, c = fileOf "A.fs", fileOf "B.fs", fileOf "C.fs"
              let! _ = edit s b (s.Texts[b] + "\nlet shapeOf () : Shape = Circle 1\n")
              let! r = edit s c (s.Texts[c] + "\n    let viaB = area (Deps.B.shapeOf ())\n")
              do! expectOracle s r.Diagnostics "the source"
              let! r = edit s a (s.Texts[a] + "\nlet extra = 1\n")
              do! expectOracle s r.Diagnostics "A.fs's signature changed, B.fs's did not"
              let! r = replace s a "x * 2" "x * 3"
              do! expectOracle s r.Diagnostics "A.fs's body changed, a new Shape"
              let! r = edit s c (s.Texts[c] + "\n    let viaB2 = area (Deps.B.shapeOf ()) + 1\n")
              do! expectOracle s r.Diagnostics "C.fs edited after: A.fs's new Shape, B.fs's old"
          }

          testTask "a body edit of a promoted file checks nothing whole" {
              let! s = start ()
              use _ = (s :> IAsyncDisposable)
              let a = fileOf "A.fs"
              s.Graph.Promote a
              let! _ = s.Graph.WhenPromoted(a, CancellationToken.None) |> settle
              let! _ = s.Graph.AllDiagnostics() |> settle
              let whole = s.Graph.FileLevelChecks
              let seen = System.Collections.Concurrent.ConcurrentQueue<string>()
              s.Graph.OnScopedCheck(fun f -> seen.Enqueue(Path.GetFileName f))
              let! r = replace s a "baseValue = 10" "baseValue = 11"
              do! expectOracle s r.Diagnostics "a body"
              Expect.equal (s.Graph.FileLevelChecks - whole) 0L $"no file is checked whole: %A{List.ofSeq seen}"
          }

          testTask "signature edit in Lib updates App without build" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              let! clean = expectAppOracle s "as on disk"
              Expect.isEmpty clean "App is clean"
              change s libFs "let f (x: int) : int = x + 1" "let f (x: int) : string = string x"
              let! broken = expectAppOracle s "f returns a string"
              Expect.exists broken (fun (d: Diagnostic) ->d.Range.Start.Line = 6) "y + 1 no longer adds up"
              change s libFs "let f (x: int) : string = string x" "let f (x: int) : int = x + 1"
              let! fixedAgain = expectAppOracle s "f returns an int"
              Expect.isEmpty fixedAgain "App is clean again"
          }

          testTask "signature edit in Lib updates App, by declaration" {
              let! s = startMulti Declarations
              use _ = (s :> IAsyncDisposable)
              change s libFs "let f (x: int) : int = x + 1" "let f (x: int) : string = string x"
              let! broken = expectAppOracle s "f returns a string"
              Expect.isNonEmpty broken "y + 1 no longer adds up"
              do! whenPromoted s.App s.AppSpec
              let! mode = s.App.Mode useFs
              Expect.equal mode Declarations "App is promoted again after the restart"
              change s useFs "let w = y + 1" "let w = y + \"1\""
              let! agreed = expectAppOracle s "App edited by declaration, on the new context"
              Expect.isEmpty agreed "y + \"1\" adds up"
          }

          testTask "body edit in Lib re-checks nothing in App" {
              let! s = startMulti Declarations
              use _ = (s :> IAsyncDisposable)
              let fileLevel, declarations = s.App.FileLevelChecks, s.App.CheckedCount
              change s libFs "x + 1" "x + 3"
              let! _ = expectAppOracle s "a body in Lib.fs"
              change s shapes "3 * r * r" "4 * r * r"
              let! _ = expectAppOracle s "a body in Shapes.fs, whose entities are new"
              do! whenPromoted s.Lib s.LibSpec
              Expect.equal (s.App.FileLevelChecks - fileLevel) 0L "no file-level check"
              Expect.equal (s.App.CheckedCount - declarations) 0L "no declaration check"
          }

          testTask "a Lib edit that keeps the fingerprint with new entities leaves App one Lib" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              // New Shape and area, the same print: App keeps its context
              change s shapes "3 * r * r" "4 * r * r"
              let! _ = expectAppOracle s "new entities, the same fingerprint"
              let checks = s.App.FileLevelChecks
              // Lib.fs alone changes print: its signature holds the kept Shape, and the assembly must be one Lib
              change s libFs "let shapeOf () : Shape = Circle 1" "let shapeOf () : Shape = Circle 1\n\nlet other = 1"
              let! kept = expectAppOracle s "a later file's signature on the kept Shape"
              Expect.isEmpty kept "area shape: one Shape"
              Expect.isGreaterThan s.App.FileLevelChecks checks "App restarted"
              // Shapes.fs's print changes: Lib.fs keeps its print, on a new Shape. Stale, App would not know Triangle
              change s useFs "let sized =" "let t = Lib.Shapes.Triangle 3\n\nlet sized ="
              let! unknown = expectAppOracle s "App asks for a case Lib does not have yet"
              Expect.isNonEmpty unknown "no Triangle yet"
              change s shapes "    | Square of int" "    | Square of int\n    | Triangle of int"
              let! known = expectAppOracle s "Shapes.fs's print changed, Lib.fs's did not"
              Expect.isFalse (known |> List.exists (fun (d: Diagnostic) ->d.Severity = 1)) $"Triangle, and one Shape: %A{known}"
              // Again a body, then the App text: the kept assembly's Shape is still App's
              change s shapes "4 * r * r" "5 * r * r"
              change s useFs "let sized = Lib.Shapes.area shape + 1" "let sized = Lib.Shapes.area shape + 2"
              let! _ = expectAppOracle s "App edited over a kept assembly"
              ()
          }

          testTask "an attribute's argument and a literal in Lib reach App" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              change s libFs "let f (x: int)" "[<System.Obsolete(\"first\")>]\nlet f (x: int)"
              let! first = expectAppOracle s "obsolete, first"
              Expect.exists first (fun (d: Diagnostic) ->d.Message.Contains "first") "App reads the message"
              change s libFs "first" "second"
              let! second = expectAppOracle s "obsolete, second"
              Expect.exists second (fun (d: Diagnostic) ->d.Message.Contains "second") "the message changed, and only it"
              change s libFs "[<System.Obsolete(\"second\")>]\n" ""
              change s libFs "let shapeOf ()" "[<Literal>]\nlet Limit = 1\n\nlet shapeOf ()"
              change s useFs "let z = g 2" "let z = match g 2 with Limit -> 0 | 1 -> 1 | _ -> 2"
              let! unused = expectAppOracle s "literal 1: the second rule never matches"
              Expect.exists unused (fun (d: Diagnostic) ->d.Code.EndsWith "26") "rule never matched"
              change s libFs "let Limit = 1" "let Limit = 3"
              let! used = expectAppOracle s "literal 3"
              Expect.isEmpty used "every rule matches again"
          }

          testTask "an edit of Lib while App's restart is held settles on the newest" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              let hold = new ManualResetEventSlim(false)
              let entered = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

              s.App.OnScopedCheck(fun _ ->
                  entered.TrySetResult(()) |> ignore
                  hold.Wait())

              change s libFs "let f (x: int) : int = x + 1" "let f (x: int) : string = string x"
              let! () = entered.Task |> settle
              // The flight holds the context f returns a string on; the settle must not answer from it
              change s libFs "let f (x: int) : string = string x" "let f (x: int) : float = float x"
              let wait = settled s
              do! Task.Delay 300
              Expect.isFalse wait.IsCompleted "App's check for the older Lib is not the answer"
              hold.Set()
              let! actual = expectAppOracle s "the newest Lib"
              Expect.exists actual (fun (d: Diagnostic) ->d.Message.Contains "float") $"y is a float: %A{actual}"
              let! _ = wait
              ()
          }

          testTask "an edit of App while the restart is pending is not lost" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              change s libFs "let f (x: int) : int = x + 1" "let f (x: int) : string = string x"
              // Posted before the context is made: the restart carries the text and its version
              change s useFs "let w = y + 1" "let w = y + \"1\""
              let! actual = expectAppOracle s "both edits"
              Expect.isEmpty actual "y + \"1\" adds up"
              let! struct (version, _) = s.App.SettledDiagnostics useFs |> settle
              Expect.equal version 1L "the version SetText returned"
          }

          testTask "a read held over a restart ends on the old result, and the next read is on the new" {
              let! s = startMulti Declarations
              use _ = (s :> IAsyncDisposable)
              let hold = new ManualResetEventSlim(false)
              let entered = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

              let reading =
                  s.App.ReadFile(
                      useFs,
                      (fun (view: FileView) ->
                          entered.TrySetResult(()) |> ignore
                          hold.Wait()
                          view.Result.Diagnostics.Length),
                      CancellationToken.None
                  )

              let! () = entered.Task |> settle
              change s libFs "let f (x: int) : int = x + 1" "let f (x: int) : string = string x"
              let! broken = expectAppOracle s "restarted beside a held read"
              Expect.isNonEmpty broken "the restart did not wait for the reader"
              let! old = settleHolding hold reading
              Expect.equal old 0 "the reader's result is the one it was given"
              let! now = s.App.ReadFile(useFs, (fun (view: FileView) -> view.Result.Diagnostics.Length), CancellationToken.None) |> settle
              Expect.equal now (List.length broken) "the next read is on the new context"
          }

          // No oracle here: a project that does not compile has no assembly to compare with
          testTask "a Lib that does not check still gives App an assembly" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              let original = s.Texts[libFs]
              change s libFs "let shapeOf ()" "let unknown = undefinedName\n\nlet rec inferred x = nope x\n\nlet shapeOf ()"
              let! typeErrors = settled s
              Expect.isEmpty typeErrors "App reads what Lib did define"
              change s useFs "let z = g 2" "let z = g 2\n\nlet u: string = unknown"
              let! _ = settled s
              set s libFs (original.Replace("let f (x: int) : int = x + 1", "let f (x: int) : int ="))
              let! syntaxError = settled s
              Expect.isNonEmpty syntaxError "App reports what Lib lost"
              set s useFs (File.ReadAllText useFs)
              set s libFs original
              let! fixedAgain = expectAppOracle s "Lib as on disk again"
              Expect.isEmpty fixedAgain "clean again"
          }

          testTask "disposing Lib ends App's waits and App" {
              let! s = startMulti FileLevel
              use _ = (s :> IAsyncDisposable)
              change s libFs "let f (x: int) : int = x + 1" "let f (x: int) : string = string x"
              let wait = s.App.AllDiagnostics()
              s.Lib.Dispose()

              let! ended =
                  task {
                      try
                          let! _ = wait |> settle
                          return false
                      with
                      | :? OperationCanceledException -> return true
                  }

              Expect.isTrue ended "App's wait ends with Lib"
          }

          testTask "a reference through another project is in memory too" {
              built.Force()
              let! libSpec = ProjectSpec.load libProject
              let! appSpec = ProjectSpec.load appProject
              let! topSpec = ProjectSpec.load topProject
              let host = GraphHost.Start "multi-top"
              use _ = { new IAsyncDisposable with member _.DisposeAsync() = (host.Dispose(); System.Threading.Tasks.ValueTask()) }
              let! (lib: ProjectGraph) = ProjectGraph.Create(host, libSpec)
              let! (app: ProjectGraph) = ProjectGraph.Create(host, appSpec, [ lib ])
              let! (top: ProjectGraph) = ProjectGraph.Create(host, topSpec, [ app ])
              let topFs = topSpec.SourceFiles.Head
              let! clean = top.Diagnostics topFs |> settle
              Expect.isEmpty clean "clean"
              // App's own print does not change, so only Top's own read of Lib can show it
              let shapesText = File.ReadAllText shapes
              lib.SetText(shapes, shapesText + "\nlet viaLibOnly = 1\n") |> ignore
              top.SetText(topFs, File.ReadAllText topFs + "\nlet seen = Lib.Shapes.viaLibOnly + 1\n") |> ignore
              let! seen = top.Diagnostics topFs |> settle
              Expect.isEmpty seen $"Top reads Lib as it stands: %A{seen |> Array.map string}"
              // And through App's signature
              let libText = File.ReadAllText libFs
              lib.SetText(libFs, libText.Replace("let f (x: int) : int = x + 1", "let f (x: int) : string = string x")) |> ignore
              let! broken = top.Diagnostics topFs |> settle
              Expect.isNonEmpty broken "App.Use.y is a string"
          } ]
