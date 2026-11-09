module Rfs.Server.Tests.ReferencesTests

open System
open System.Collections.Generic
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.SourceCodeServices
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests.PublishTests

let private fixtures = Path.GetDirectoryName depsDir
let private d = file "D.fs"
let private multiDir = Path.Combine(fixtures, "Multi")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private libProject = Path.Combine(multiDir, "Lib", "Lib.fsproj")
let private useFs = Path.Combine(multiDir, "App", "Use.fs")
let private libFs = Path.Combine(multiDir, "Lib", "Lib.fs")
let private signedFs = Path.Combine(multiDir, "Lib", "Signed.fs")
let private signedFsi = Path.Combine(multiDir, "Lib", "Signed.fsi")

/// A location as the tests compare it: the file, and the 0-based start and end
type private Answer = Set<string * int * int * int * int>

let private position (text: string) (needle: string) (nth: int) (inside: int) =
    let mutable index = -1

    for _ in 0..nth do
        index <- text.IndexOf(needle, index + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual index 0 $"%s{needle} occurs %d{nth + 1} times"
    let before = text.Substring(0, index)
    before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1) + inside

let private request (path: string) ((line, character): int * int) (includeDeclaration: bool) : JsonNode =
    let at = JsonObject()
    at["line"] <- JsonValue.Create line
    at["character"] <- JsonValue.Create character
    let document = JsonObject()
    document["uri"] <- JsonValue.Create(Paths.toUri path)
    let context = JsonObject()
    context["includeDeclaration"] <- JsonValue.Create includeDeclaration
    let parameters = JsonObject()
    parameters["textDocument"] <- document
    parameters["position"] <- at
    parameters["context"] <- context
    parameters

let private references (s: Session) (path: string) (at: int * int) (includeDeclaration: bool) : Task<Answer> =
    task {
        let! result, _ = s.Lsp.Request("textDocument/references", request path at includeDeclaration) |> within

        match result with
        | null -> return Set.empty
        | r ->
            return
                set
                    [ for location in r.AsArray() do
                          let range = location["range"]
                          let point (name: string) (part: string) = range[name].[part].GetValue<int>()

                          yield
                              (Paths.ofUri (location["uri"].GetValue<string>())).ToLowerInvariant(),
                              point "start" "line",
                              point "start" "character",
                              point "end" "line",
                              point "end" "character" ]
    }

/// The union over the project's files of FCS's uses of the symbol at a position, with the session's texts
let private fcsUses (s: Session) (project: string) (path: string) (at: int * int) (includeDeclaration: bool) : Task<Answer> =
    task {
        let! spec = ProjectSpec.load project

        let text (f: string) =
            try
                s.Lsp.Text f
            with :? KeyNotFoundException ->
                File.ReadAllText f

        let texts = spec.SourceFiles |> List.map (fun f -> f, text f) |> Map.ofList
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
        let reader (f: string) = (let t = texts[f] in hash t, lazy t)

        let check (f: string) =
            checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, reader, f)
            |> Async.StartAsTask

        let! _, asking, _ = check path
        let line, character = at
        let lineStr, names, endColumn = (Hover.lookupOf texts[path] line character).Value
        let symbol = (asking.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names)).Value.Symbol
        let found = ResizeArray()

        for f in spec.SourceFiles do
            if not (f.EndsWith ".fsi") then
                let! _, c, _ = check f

                for u in c.GetUsesOfSymbolInFile symbol do
                    if includeDeclaration || not u.IsFromDefinition then
                        found.Add(f.ToLowerInvariant(), u.Range.StartLine - 1, u.Range.StartColumn, u.Range.EndLine - 1, u.Range.EndColumn)

        return Set.ofSeq found
    }

let private start (root: string) (project: string) (opened: string list) =
    task {
        Fixtures.build project
        let! (s: Session) = startSessionWith root [ project ] true ignore
        do! openAll s opened
        // Its texts are set by now: what a test sets on a graph comes after them
        do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
        return s
    }

/// Waits until the file is checked by declaration for its latest text: a kept row is what the tests are about
let private promoted (s: Session) (path: string) =
    task {
        let deadline = DateTime.UtcNow.AddSeconds 60.
        let mutable mode = Rfs.Checker.FileMode.FileLevel

        while mode <> Rfs.Checker.FileMode.Declarations && DateTime.UtcNow < deadline do
            do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
            let graph = (s.Server.Workspace.ProjectOf path).Value
            let! (next: Rfs.Checker.FileMode) = graph.WhenPromoted(path, CancellationToken.None) |> within
            mode <- next

            if mode <> Rfs.Checker.FileMode.Declarations then
                do! Task.Delay 25

        Expect.equal mode Rfs.Checker.FileMode.Declarations $"%s{Path.GetFileName path} is checked by declaration"
    }

let private scopeOf (s: Session) : References.Scope =
    { OwnersOf = s.Server.Workspace.OwnersOf
      ReferencingProjects = s.Server.Workspace.ReferencingProjects
      Projects = fun () -> s.Server.Workspace.Projects
      FilesOf = s.Server.Workspace.FilesOf
      IsLoading = fun () -> s.Server.Workspace.IsLoading }

/// A host for a graph that is made to go away
module ReferencesTestsHost =
    let host = lazy (Rfs.Checker.GraphHost.Start "references tests")

/// Each edit writes `n` blank lines above the definition in A and above the use in B, A first. So the states that exist
/// are A at n and B at n, or A at n + 1 and B at n; a scan that reads A and then B after edits have gone by answers A
/// at less than B, which never existed.
let private overlapping () =
    let a (n: int) = "module Proj.A\n" + String('\n', n) + "let target = 1\n"
    let b (n: int) = "module Proj.B\n" + String('\n', n) + "let user = Proj.A.target\n"
    a, b

[<Tests>]
let tests =
    testList
        "References"
        [ testTask "references equal FCS on Deps" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              let aText = s.Lsp.Text(file "A.fs")

              for needle, nth, inside in [ "baseValue", 0, 1; "scale", 0, 1; "Shape", 0, 1; "Circle", 0, 1 ] do
                  let at = position aText needle nth inside
                  let! expected = fcsUses s depsProject (file "A.fs") at true
                  Expect.isNonEmpty expected $"FCS finds %s{needle}"
                  let! actual = references s (file "A.fs") at true
                  Expect.equal (actual |> Set.filter (fun (f: string, _, _, _, _) -> not (f.EndsWith ".fsi"))) expected needle
          }

          testTask "includeDeclaration false leaves the definition out" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs" ]
              use _ = disposing s
              let at = position (s.Lsp.Text(file "A.fs")) "baseValue" 0 1
              let! (withDefinition: Answer) = references s (file "A.fs") at true
              let! expected = fcsUses s depsProject (file "A.fs") at false
              let! (without: Answer) = references s (file "A.fs") at false
              Expect.equal without expected "as FCS's uses that aren't definitions"
              Expect.equal (withDefinition.Count - without.Count) 1 "one definition"
          }

          testTask "references after a body edit of the definer" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              do! promoted s (file "A.fs")
              do! s.Lsp.DidChange(file "A.fs", 1, replace s (file "A.fs") "(baseValue + 1) * 2" "(baseValue + 1) * 3")
              do! promoted s (file "A.fs")
              let at = position (s.Lsp.Text(file "A.fs")) "baseValue" 0 1
              let! expected = fcsUses s depsProject (file "A.fs") at true
              let! actual = references s (file "A.fs") at true
              Expect.equal actual expected "kept rows are still found"
              Expect.isGreaterThan s.Server.Workspace.Projects.Head.CheckedCount 0L "checked"
          }

          testTask "references after a blank line insert" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              do! promoted s (file "A.fs")
              let graph = (s.Server.Workspace.ProjectOf(file "A.fs")).Value
              let shiftedBefore = graph.ShiftedRows
              do! s.Lsp.DidChange(file "A.fs", 1, replace s (file "A.fs") "let baseValue" "\n\nlet baseValue")
              do! promoted s (file "A.fs")
              let at = position (s.Lsp.Text(file "A.fs")) "baseValue" 0 1
              let! expected = fcsUses s depsProject (file "A.fs") at true
              let! actual = references s (file "A.fs") at true
              Expect.equal actual expected "current ranges, in the file and from the files after it"
              Expect.isGreaterThan graph.ShiftedRows shiftedBefore "rows were kept and shifted"
          }

          testTask "references across projects" {
              let! (s: Session) = start multiDir appProject [ useFs; libFs; signedFsi; signedFs ]
              use _ = disposing s
              do! promoted s useFs
              do! promoted s libFs
              let inLib () = position (s.Lsp.Text libFs) "let f (x" 0 4
              let fromApp = position (s.Lsp.Text useFs) "f 1" 0 0

              let expectBoth (what: string) =
                  task {
                      let! lib = fcsUses s libProject libFs (inLib ()) true
                      let line, start = fromApp
                      let inApp = useFs.ToLowerInvariant(), line, start, line, start + 1
                      let expected = lib.Add inApp
                      let! fromUse = references s useFs fromApp true
                      Expect.equal fromUse expected $"%s{what}: from App"
                      let! fromDefinition = references s libFs (inLib ()) true
                      Expect.equal fromDefinition expected $"%s{what}: from Lib"
                  }

              do! expectBoth "as loaded"
              do! s.Lsp.DidChange(libFs, 1, replace s libFs "let f (x" "\n\nlet f (x")
              do! promoted s libFs
              do! promoted s useFs
              do! expectBoth "Lib's new lines: App's assembly did not change"
          }

          testTask "a type's uses in a signature file come at their current ranges" {
              let! (s: Session) = start depsDir depsProject []
              let g = (s.Server.Workspace.ProjectOf d).Value
              use _ = disposing s
              g.SetText(file "E.fs", "module Deps.E\n\ntype T = { V: int }\n\nlet v = { V = 1 }\n") |> ignore
              let signature (blank: string) = "module Deps.E\n" + blank + "\ntype T = { V: int }\n\nval v: T\n"
              g.SetText(file "E.fsi", signature "") |> ignore
              let! _ = g.AllDiagnostics() |> within

              let inSignature (line: int) (answer: References.Answer) =
                  match answer with
                  | References.Answer.Found uses ->
                      uses |> List.exists (fun (struct (r, definition)) -> r.FileName.EndsWith "E.fsi" && r.StartLine = line && not definition)
                  | References.Answer.Moved -> false

              let! (first: References.Answer) = References.find (scopeOf s) g (file "E.fs") 2 5 CancellationToken.None
              Expect.isTrue (inSignature 5 first) $"`val v: T` is a use of T: %A{first}"
              g.SetText(file "E.fsi", signature "\n\n") |> ignore
              let! _ = g.AllDiagnostics() |> within
              let! (moved: References.Answer) = References.find (scopeOf s) g (file "E.fs") 2 5 CancellationToken.None
              Expect.isTrue (inSignature 7 moved) $"two lines down: %A{moved}"
          }

          testTask "a signature file's declaration comes at its current range" {
              let! (s: Session) = start multiDir appProject [ useFs; signedFsi; signedFs ]
              use _ = disposing s
              let signature () = s.Lsp.Text signedFsi
              let name = (signature ()).Split('\n') |> Array.find (fun l -> l.StartsWith "val ")
              let identifier = (name.Substring 4).Split(':', ' ')[0]
              let at () = position (s.Lsp.Text signedFs) ("let " + identifier) 0 4

              let expectAt (what: string) =
                  task {
                      let line, column = position (signature ()) ("val " + identifier) 0 4
                      let! actual = references s signedFs (at ()) true
                      let wanted = signedFsi.ToLowerInvariant(), line, column, line, column + identifier.Length
                      Expect.isTrue (actual.Contains wanted) $"%s{what}: %A{wanted} in %A{actual}"
                  }

              do! expectAt "as loaded"
              do! s.Lsp.DidChange(signedFsi, 1, replace s signedFsi ("val " + identifier) ("\n\nval " + identifier))
              do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
              do! expectAt "after a blank line in it"
          }

          testTask "an unopened file counts" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs" ]
              use _ = disposing s
              let at = position (s.Lsp.Text(file "A.fs")) "baseValue" 0 1
              let! expected = fcsUses s depsProject (file "A.fs") at true
              let! actual = references s (file "A.fs") at true
              Expect.equal actual expected "the files nobody opened answer from what they have"
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let! (mode: Rfs.Checker.FileMode) = graph.Mode d
              Expect.equal mode Rfs.Checker.FileMode.FileLevel "and asking doesn't promote them"
          }

          testTask "a cursor in a signature file answers nothing" {
              let! (s: Session) = start multiDir appProject [ useFs; signedFsi; signedFs ]
              use _ = disposing s
              let! (actual: Answer) = references s signedFsi (0, 8) true
              Expect.isEmpty actual "a recorded limit"
          }

          testTask "a class with two constructors answers: they aren't a tie" {
              let! (s: Session) = start depsDir depsProject [ d ]
              use _ = disposing s
              do! s.Lsp.DidChange(d, 1, replace s d (s.Lsp.Text d) "module Deps.D\n\ntype C(x: int) =\n    new() = C(0)\n\nlet y = C()\n")
              let! (expected: Answer) = fcsUses s depsProject d (2, 5) true
              Expect.equal expected.Count 3 "the type and its two calls"
              let! (actual: Answer) = references s d (2, 5) true
              Expect.equal actual expected "as FCS"
          }

          testTask "a key two definitions share answers nothing" {
              let! (s: Session) = start depsDir depsProject [ d ]
              use _ = disposing s
              do! s.Lsp.DidChange(d, 1, replace s d (s.Lsp.Text d) "module Deps.D\n\nlet x = 1\nlet x = 2\nlet y = x\n")
              let! (actual: Answer) = references s d (2, 4) true
              Expect.isEmpty actual "never the two merged"
          }

          testTask "a local answers from the asking file, with current ranges after a blank line" {
              let! (s: Session) = start depsDir depsProject [ d ]
              use _ = disposing s
              do! promoted s d
              let text = "module Deps.D\n\nlet outer =\n    let inner = 1\n    inner + inner\n\nlet last = 2\n"
              do! s.Lsp.DidChange(d, 1, replace s d (s.Lsp.Text d) text)
              do! promoted s d
              let! (first: Answer) = references s d (3, 8) true
              let row (shift: int) = set [ for line, c in [ 3, 8; 4, 4; 4, 12 ] -> d.ToLowerInvariant(), line + shift, c, line + shift, c + 5 ]
              Expect.equal first (row 0) "its three tokens"
              do! s.Lsp.DidChange(d, 2, replace s d "let outer" "\n\nlet outer")
              do! promoted s d
              let! (shifted: Answer) = references s d (5, 8) true
              Expect.equal shifted (row 2) "two lines down"
          }

          testTask "overlapping edits end, and never answer a state that didn't exist" {
              let a, b = overlapping ()
              let! (s: Session) = start depsDir depsProject []
              use _ = disposing s
              let aFile, bFile = file "A.fs", file "B.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              graph.SetText(aFile, a 0) |> ignore
              graph.SetText(bFile, b 0) |> ignore
              let! _ = graph.AllDiagnostics() |> within
              let scope = { scopeOf s with FilesOf = fun _ -> [ aFile; bFile ] }
              let stop = new CancellationTokenSource()
              let mutable found = 0
              let mutable moved = 0
              let broken = System.Collections.Concurrent.ConcurrentBag<string>()
              let mutable edits = 0
              let mutable both = 0

              let asker () =
                  Task.Run<unit>(fun () ->
                      task {
                          while not stop.IsCancellationRequested do
                              // The definition's line moves with the edits: B's use is always its last line but one
                              let n = Volatile.Read &edits
                              let line = n + 1
                              let! (answer: References.Answer) = References.find scope graph bFile line 18 CancellationToken.None

                              match answer with
                              | References.Answer.Moved -> Interlocked.Increment &moved |> ignore
                              | References.Answer.Found uses ->
                                  Interlocked.Increment &found |> ignore
                                  let lineIn (name: string) = uses |> List.tryPick (fun (struct (r, _)) -> if r.FileName = name then Some r.StartLine else None)

                                  match lineIn aFile, lineIn bFile with
                                  | Some inA, Some inB ->
                                      Interlocked.Increment &both |> ignore
                                      // module line, then n blank lines, then the row: its 1-based line is n + 2
                                      let aAt, bAt = inA - 2, inB - 2

                                      if aAt - bAt <> 0 && aAt - bAt <> 1 then
                                          broken.Add $"A at %d{aAt} with B at %d{bAt}"
                                  | _ -> ()
                      })

              let askers = [ for _ in 1..3 -> asker () ]

              for n in 1..60 do
                  graph.SetText(aFile, a n) |> ignore
                  graph.SetText(bFile, b n) |> ignore
                  Volatile.Write(&edits, n)
                  do! Task.Delay 40

              let! _ = graph.AllDiagnostics() |> within
              do! Task.Delay 200
              stop.Cancel()
              let! _ = (Task.WhenAll askers).WaitAsync(TimeSpan.FromSeconds 110.)
              printfn "overlapping edits: %d found, %d of them in both files, %d moved" found both moved
              Expect.isGreaterThan both 0 "requests ended, and some saw both files"
              Expect.isEmpty broken "only states that existed"
          }

          testTask "typing during a request answers the settled final text, not a mix and not empty" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; file "B.fs"; d ]
              use _ = disposing s
              let aFile, bFile = file "A.fs", file "B.fs"
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! promoted s d
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              let gate = new ManualResetEventSlim(false)
              let entered = new ManualResetEventSlim(false)

              // D's lock is held, so its index can't be built: the request can't answer until it is let go
              let held =
                  graph.ReadFile(
                      d,
                      (fun _ ->
                          entered.Set()
                          gate.Wait(TimeSpan.FromSeconds 60.) |> ignore),
                      CancellationToken.None
                  )

              let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
              let pending = references s aFile at true

              // Each edit moves B's uses down, and every other one adds another use
              for n in 1..12 do
                  let extra = if n % 2 = 0 then $"let again%d{n} = baseValue\n\n" else "\n"
                  do! s.Lsp.DidChange(bFile, n, replace s bFile "type Counter() =" (extra + "type Counter() ="))
                  do! Task.Delay 30

              do! Task.Delay 300
              Expect.isFalse pending.IsCompleted "it waits for D"
              gate.Set()
              let! _ = held |> within
              let! (answer: Answer) = pending |> within
              let! expected = fcsUses s depsProject aFile at true
              Expect.equal (expected |> Set.filter (fun (f, _, _, _, _) -> f = bFile.ToLowerInvariant()) |> Set.count) 7 "B's use and six new ones"
              Expect.equal (answer |> Set.filter (fun (f: string, _, _, _, _) -> not (f.EndsWith ".fsi"))) expected "FCS's uses of the final texts"
          }

          testTask "cancelling ends the request during the scan, with a read held" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              let aFile = file "A.fs"
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! promoted s aFile
              do! promoted s d
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              let gate = new ManualResetEventSlim(false)
              let entered = new ManualResetEventSlim(false)

              // The asking file is free, and the scan waits on the last file read
              let held =
                  graph.ReadFile(
                      d,
                      (fun _ ->
                          entered.Set()
                          gate.Wait(TimeSpan.FromSeconds 60.) |> ignore),
                      CancellationToken.None
                  )

              let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
              let cancelled = new CancellationTokenSource()
              let mutable reached = false

              let scope =
                  { scopeOf s with
                      FilesOf =
                          fun g ->
                              reached <- true
                              s.Server.Workspace.FilesOf g }

              let pending = References.find scope graph aFile (fst at) (snd at) cancelled.Token
              do! Task.Delay 300
              Expect.isTrue reached "the request got to the scan"
              Expect.isFalse pending.IsCompleted "and waits for the held file"
              cancelled.Cancel()

              let! outcome =
                  task {
                      try
                          let! (_: References.Answer) = pending.WaitAsync(TimeSpan.FromSeconds 20.)
                          return "answered"
                      with :? OperationCanceledException ->
                          return "cancelled"
                  }

              gate.Set()
              let! _ = held |> within
              Expect.equal outcome "cancelled" "the token is honoured while a file is waited for"
          }

          testTask "a project gone, or the workspace loading, answers Moved and not a partial list" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              let aFile = file "A.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              let! (loaded: References.Answer) = References.find (scopeOf s) graph aFile (fst at) (snd at) CancellationToken.None
              Expect.isFalse loaded.IsMoved "answers while loaded"
              let! (loading: References.Answer) = References.find { scopeOf s with IsLoading = fun () -> true } graph aFile (fst at) (snd at) CancellationToken.None
              Expect.isTrue loading.IsMoved "a load is under way"

              // A referencing project that goes away during the scan
              let other = ProjectSpec.sequential (ProjectSpec.load depsProject).Result
              let! (right: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(ReferencesTestsHost.host.Value, { other with ProjectFile = Path.Combine(Path.GetDirectoryName depsProject, "Right.fsproj") })
              right.Dispose()

              let scope =
                  { scopeOf s with
                      ReferencingProjects = fun _ -> [ right ]
                      FilesOf = fun g -> if obj.ReferenceEquals(g, right) then [ aFile ] else s.Server.Workspace.FilesOf g }

              let! (gone: References.Answer) = References.find scope graph aFile (fst at) (snd at) CancellationToken.None
              Expect.isTrue gone.IsMoved "a project went away"
          }

          testTask "a load that starts during the turn, or another set of projects by its end, answers Moved" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs" ]
              use _ = disposing s
              let aFile = file "A.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              let mutable asks = 0

              // Not loading when the turn starts, loading when it ends: the workspace changes off the graph thread
              let started =
                  { scopeOf s with
                      IsLoading =
                          fun () ->
                              asks <- asks + 1
                              asks > 1 }

              let! (loadStarted: References.Answer) = References.find started graph aFile (fst at) (snd at) CancellationToken.None
              Expect.isTrue loadStarted.IsMoved $"the turn's end saw a load: %A{loadStarted}"
              let other = ProjectSpec.sequential (ProjectSpec.load depsProject).Result
              let! (app: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(graph.Host, { other with ProjectFile = Path.Combine(Path.GetDirectoryName depsProject, "Back.fsproj") })
              let mutable referencing = 0

              let cameBack =
                  { scopeOf s with
                      ReferencingProjects =
                          fun _ ->
                              referencing <- referencing + 1
                              if referencing <= 1 then [] else [ app ] }

              let! (projectBack: References.Answer) = References.find cameBack graph aFile (fst at) (snd at) CancellationToken.None
              app.Dispose()
              Expect.isTrue projectBack.IsMoved $"a project came back during the turn: %A{projectBack}"
          }

          for promote in [ true; false ] do
              testTask $"""a mix no state had isn't answered, when a dependent's uses change and change back (%s{if promote then "by declaration" else "file level"})""" {
                  let aText (forB: string) (forC: string) (extra: string) =
                      $"module Proj.A\n\nlet t = 1\n\nmodule ForB =\n    let %s{forB} = 5\n\nmodule ForC =\n    let %s{forC} = 5\n\nlet extra = %s{extra}\n"

                  let using (name: string) = $"module Proj.%s{name}\n\nopen Proj.A\nopen Proj.A.For%s{name}\n\nlet v = t\n"
                  let dir = Directory.CreateTempSubdirectory "rfs-mix-"

                  try
                      let path (name: string) = Path.Combine(dir.FullName, name)
                      let a, b, c = path "A.fs", path "B.fs", path "C.fs"
                      File.WriteAllText(a, aText "other" "t" "0")
                      File.WriteAllText(b, using "B")
                      File.WriteAllText(c, using "C")
                      let! deps = ProjectSpec.load depsProject
                      let host = Rfs.Checker.GraphHost.Start "mix"
                      let! (g: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(host, { ProjectSpec.sequential deps with ProjectFile = path "P.fsproj"; SourceFiles = [ a; b; c ] })

                      let settle () =
                          task {
                              let! _ = g.AllDiagnostics() |> within

                              if promote then
                                  for f in [ a; b; c ] do
                                      let! _ = g.WhenPromoted(f, CancellationToken.None) |> within
                                      ()
                          }

                      if promote then
                          for f in [ a; b; c ] do
                              g.Promote f

                      do! settle ()
                      let mutable asks = 0

                      // The read asks for the files at each turn it tries, on the graph thread, and reads them in order
                      // until one is pending: A's index is built at the first turn, B's at a later one, then C's. The
                      // first edit lands once A's is read and the second at the turn after, as the passes once read
                      // A B [edit] C A [edit] B C: at each of the three states one file says otherwise than at the
                      // others. Neither edit waits to settle: the read must wait for that itself.
                      let scope: References.Scope =
                          { OwnersOf = fun f -> if List.contains f [ a; b; c ] then [ g ] else []
                            ReferencingProjects = fun _ -> []
                            Projects = fun () -> [ g ]
                            FilesOf =
                              fun _ ->
                                  asks <- asks + 1
                                  if asks = 3 then g.SetText(a, aText "t" "other" "0") |> ignore
                                  if asks = 5 then g.SetText(a, aText "other" "other" "t") |> ignore
                                  [ a; b; c ]
                            IsLoading = fun () -> false }

                      let! (answer: References.Answer) = References.find scope g a 2 4 CancellationToken.None
                      g.Dispose()
                      (host :> IDisposable).Dispose()
                      Expect.isGreaterThanOrEqual asks 5 "both edits were made during the request"

                      match answer with
                      | References.Answer.Moved -> failtest "nothing went away: the read waits for the last state"
                      | References.Answer.Found uses ->
                          let lines (name: string) = uses |> List.filter (fun (struct (r, _)) -> r.FileName = name) |> List.map (fun (struct (r, _)) -> r.StartLine) |> List.sort
                          Expect.equal (lines a) [ 3; 11 ] "A's definition, and the use the last text has"
                          Expect.equal (lines b) [ 6 ] "B's use"
                          Expect.equal (lines c) [ 6 ] "C's use"
                  finally
                      try
                          dir.Delete true
                      with :? IOException | :? UnauthorizedAccessException ->
                          ()
              }

          testTask "a reload answers ContentModified from a request waiting on it, and shutdown answers nothing" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              let aFile = file "A.fs"
              do! promoted s d
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              // Of a name D doesn't have: an index of D, once built, needs no lock, and the request below must wait on D's
              Expect.isFalse ((s.Lsp.Text d).Contains "area") "D skipped"
              let! (before: Answer) = references s aFile (position (s.Lsp.Text aFile) "area" 0 1) true
              Expect.isNonEmpty before "answers while loaded"
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let gate = new ManualResetEventSlim(false)
              let entered = new ManualResetEventSlim(false)

              let held =
                  graph.ReadFile(
                      d,
                      (fun _ ->
                          entered.Set()
                          gate.Wait(TimeSpan.FromSeconds 60.) |> ignore),
                      CancellationToken.None
                  )

              let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
              let pending = references s aFile at true
              do! Task.Delay 500
              Expect.isFalse pending.IsCompleted "it waits for the held file"
              let reload = s.Server.Workspace.Reload depsProject

              let! outcome =
                  task {
                      try
                          let! (answer: Answer) = pending
                          return $"answered %d{answer.Count}"
                      with ex ->
                          return ex.Message
                  }

              gate.Set()
              do! reload
              Expect.stringContains outcome "-32801" "ContentModified, not a list that looks complete"
              do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
              (s.Server :> IDisposable).Dispose()
              let! (afterShutdown: Answer) = references s aFile at true
              Expect.isEmpty afterShutdown "after shutdown"
          }

          testTask "a fault in a file's index fails the request with an internal error, and is logged" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs" ]
              use _ = disposing s
              let aFile = file "A.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              graph.OnFlight(fun kind path _ -> if kind = "uses" && path.EndsWith "D.fs" then failwith "an injected index fault")
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              let log = new StringWriter()
              let stderr = Console.Error
              Console.SetError(TextWriter.Synchronized log)

              let! outcome =
                  task {
                      try
                          try
                              let! (answer: Answer) = references s aFile at true
                              return $"answered %d{answer.Count}"
                          with ex ->
                              return ex.Message
                      finally
                          Console.SetError stderr
                  }

              // LspResult.internalError, as every request handler fails: Ionide.LanguageServerProtocol gives it -32602
              Expect.stringContains outcome "\"code\":-32602" "the handlers' internal error, not ContentModified"
              Expect.stringContains outcome "an injected index fault" "with the fault's message"
              Expect.stringContains (log.ToString()) "an injected index fault" "logged"
          }

          testTask "a member more generic than its signature answers nothing" {
              let! (s: Session) = start depsDir depsProject []
              use _ = disposing s
              let g = (s.Server.Workspace.ProjectOf d).Value
              g.SetText(file "E.fsi", "module Deps.E\n\ntype T =\n    new: unit -> T\n    member One: x: int -> int\n") |> ignore
              g.SetText(file "E.fs", "module Deps.E\n\ntype T() =\n    member _.One(x) = x\n") |> ignore
              g.SetText(d, "module Deps.D\n\nlet z = Deps.E.T().One 1\n") |> ignore
              let! _ = g.AllDiagnostics() |> within
              let! (fromDefinition: References.Answer) = References.find (scopeOf s) g (file "E.fs") 3 14 CancellationToken.None
              let! (fromUse: References.Answer) = References.find (scopeOf s) g d 2 21 CancellationToken.None
              Expect.equal (string fromDefinition) (string (References.Answer.Found [])) "from the definition"
              Expect.equal (string fromUse) (string (References.Answer.Found [])) "from the use"
          }

          testTask "a named argument in another file than its method's answers nothing, and in its file its uses" {
              let! (s: Session) = start depsDir depsProject []
              use _ = disposing s
              let g = (s.Server.Workspace.ProjectOf d).Value
              g.SetText(file "A.fs", "module Proj.A\n\ntype T =\n    static member M(name: int) = name\n\nlet here = T.M(name = 1)\n") |> ignore
              g.SetText(file "B.fs", "module Proj.B\n\nlet there = Proj.A.T.M(name = 2)\n") |> ignore
              let! _ = g.AllDiagnostics() |> within
              let! (elsewhere: References.Answer) = References.find (scopeOf s) g (file "B.fs") 2 24 CancellationToken.None
              Expect.equal (string elsewhere) (string (References.Answer.Found [])) "another file"
              let! (inItsFile: References.Answer) = References.find (scopeOf s) g (file "A.fs") 5 16 CancellationToken.None

              match inItsFile with
              | References.Answer.Found uses -> Expect.equal uses.Length 3 "its parameter's own, and the argument"
              | References.Answer.Moved -> failtest "nothing moved"
          }

          // I2: a stack overflow here ends the test run, as it ended the server
          testTask "resolve in a row of 4000 lets answers" {
              let dir = Directory.CreateTempSubdirectory "rfs-large-"

              try
                  let path = Path.Combine(dir.FullName, "F.fs")
                  let lets = System.Text.StringBuilder("module Proj.F\n\nlet deep () =\n")

                  for r in 0..3999 do
                      lets.Append($"    let a%d{r} = %d{r}\n") |> ignore

                  lets.Append("    a0\n\nlet after = 1\n") |> ignore
                  File.WriteAllText(path, lets.ToString())
                  let! deps = ProjectSpec.load depsProject
                  let host = Rfs.Checker.GraphHost.Start "large"
                  let! (g: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(host, { ProjectSpec.sequential deps with ProjectFile = Path.Combine(dir.FullName, "P.fsproj"); SourceFiles = [ path ] })
                  g.Promote path
                  let! (_: Rfs.Checker.FileMode) = g.WhenPromoted(path, CancellationToken.None).WaitAsync(TimeSpan.FromMinutes 4.)
                  let line = 3 + 4000
                  let! (items: Ionide.LanguageServerProtocol.Types.CompletionItem[]) = Completion.at g path line 6 CancellationToken.None
                  Expect.isNonEmpty items "the list answers"
                  let item = items |> Array.find (fun i -> i.Label = "a0")
                  let! (back: Ionide.LanguageServerProtocol.Types.CompletionItem) = Completion.resolve g item path line 6 CancellationToken.None
                  Expect.isSome back.Detail "resolve answers"
                  g.Dispose()
                  (host :> IDisposable).Dispose()
              finally
                  try
                      dir.Delete true
                  with :? IOException | :? UnauthorizedAccessException ->
                      ()
          }

          // I3: a demoted file keeps its last finish, and a reader of it must get that finish's text and parse
          testTask "a file demoted with no new text answers every feature as a fresh graph does" {
              let dir = Directory.CreateTempSubdirectory "rfs-kept-"

              try
                  let before = "module Deps.A\n\nlet f (x: int) = x + 1\n\nlet first = f 1\n\nlet second = \"two\"\n\nlet third = first\n"
                  let after = "module Deps.A\n\nlet f (x: int) = x + 1\n\nlet inserted = \"a string\"\n\nlet first = f 1\n\nlet second = \"two\"\n\nlet third = first\n\nlet g2 = third + 1\n"
                  let! deps = ProjectSpec.load depsProject

                  let specIn (name: string) (text: string) =
                      let sub = Directory.CreateDirectory(Path.Combine(dir.FullName, name))
                      let path = Path.Combine(sub.FullName, "F.fs")
                      File.WriteAllText(path, text)
                      path, { ProjectSpec.sequential deps with ProjectFile = Path.Combine(sub.FullName, "P.fsproj"); SourceFiles = [ path ] }

                  let path, spec = specIn "kept" before
                  let freshPath, freshSpec = specIn "fresh" after
                  let host = Rfs.Checker.GraphHost.Start "kept"
                  let! (g: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(host, spec)
                  let! (fresh: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(host, freshSpec)
                  let! _ = g.AllDiagnostics() |> within
                  let! _ = fresh.AllDiagnostics() |> within
                  g.Promote path
                  let! _ = g.WhenPromoted(path, CancellationToken.None) |> within
                  g.SetText(path, after) |> ignore
                  let! _ = g.AllDiagnostics() |> within
                  let! _ = g.WhenPromoted(path, CancellationToken.None) |> within
                  g.Demote path
                  let! _ = g.AllDiagnostics() |> within
                  let! (text: string), (parsed: string) = g.ReadFile(path, (fun v -> v.Text, v.Parse.ParseTree.Range.End.ToString()), CancellationToken.None) |> within
                  let! (freshParsed: string) = fresh.ReadFile(freshPath, (fun v -> v.Parse.ParseTree.Range.End.ToString()), CancellationToken.None) |> within
                  Expect.equal text after "the view's text is the result's"
                  Expect.equal parsed freshParsed "the view's parse is the result's"

                  let scope (graph: Rfs.Checker.ProjectGraph) (s: ProjectSpec) : References.Scope =
                      { OwnersOf = fun p -> if List.contains (Path.GetFullPath p) s.SourceFiles then [ graph ] else []
                        ReferencingProjects = fun _ -> []
                        Projects = fun () -> [ graph ]
                        FilesOf = fun _ -> s.SourceFiles
                        IsLoading = fun () -> false }

                  let answers (graph: Rfs.Checker.ProjectGraph) (s: ProjectSpec) (p: string) (l: int) (c: int) =
                      task {
                          let! hover = Hover.at graph p l c CancellationToken.None
                          let! definition = Definition.at (fun _ -> [ graph ]) graph p l c CancellationToken.None
                          let! completion = Completion.at graph p l c CancellationToken.None
                          let! references = References.find (scope graph s) graph p l c CancellationToken.None

                          let referenced =
                              match references with
                              | References.Answer.Moved -> [ "moved" ]
                              | References.Answer.Found uses ->
                                  uses |> List.map (fun (struct (r, isDefinition)) -> $"%d{r.StartLine}:%d{r.StartColumn}:%b{isDefinition}") |> List.sort

                          return
                              hover,
                              definition |> Option.map (fun r -> r.StartLine, r.StartColumn),
                              completion |> Array.map _.Label,
                              referenced
                      }

                  let tokens =
                      [ for i, line in after.Split '\n' |> Array.indexed do
                            for m in System.Text.RegularExpressions.Regex.Matches(line, "[A-Za-z_0-9]+") do
                                i, m.Index + 1, m.Value ]

                  for l, c, name in tokens do
                      let! ours = answers g spec path l c
                      let! reference = answers fresh freshSpec freshPath l c
                      Expect.equal ours reference $"%s{name} at %d{l}:%d{c}"

                  g.Dispose()
                  fresh.Dispose()
                  (host :> IDisposable).Dispose()
              finally
                  try
                      dir.Delete true
                  with :? IOException | :? UnauthorizedAccessException ->
                      ()
          }

          testTask "a 4000-row file promotes and answers completion, and so does a row of 4000 lets" {
              let dir = Directory.CreateTempSubdirectory "rfs-large-"

              try
                  let path = Path.Combine(dir.FullName, "F.fs")
                  let rows = System.Text.StringBuilder("module Proj.F\n\ntype R = { Name: string; Size: int }\n\n")

                  for r in 0..3999 do
                      rows.Append($"let v%d{r} (x: int) (r: R) = if x > %d{r} then (r.Size + x, r.Name, List.map (fun y -> y + x) [ x; %d{r} ]) else (x, r.Name, [])\n") |> ignore

                  rows.Append("let last (r: R) = r.S\n") |> ignore
                  let text = rows.ToString()
                  File.WriteAllText(path, text)
                  let! deps = ProjectSpec.load depsProject
                  let host = Rfs.Checker.GraphHost.Start "large"
                  let! (g: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(host, { ProjectSpec.sequential deps with ProjectFile = Path.Combine(dir.FullName, "P.fsproj"); SourceFiles = [ path ] })
                  g.Promote path
                  let! (mode: Rfs.Checker.FileMode) = g.WhenPromoted(path, CancellationToken.None).WaitAsync(TimeSpan.FromMinutes 8.)
                  Expect.equal mode Rfs.Checker.FileMode.Declarations "promoted"
                  let lines = text.Split '\n'
                  let! (items: Ionide.LanguageServerProtocol.Types.CompletionItem[]) = Completion.at g path (lines.Length - 2) 20 CancellationToken.None
                  Expect.isNonEmpty items "it answers"
                  let lets = System.Text.StringBuilder("module Proj.F\n\nlet deep =\n")

                  for r in 0..3999 do
                      lets.Append($"    let a%d{r} = %d{r}\n") |> ignore

                  lets.Append("    a0\n") |> ignore
                  let deep = lets.ToString()
                  g.SetText(path, deep) |> ignore
                  let! (inner: Ionide.LanguageServerProtocol.Types.CompletionItem[]) = Completion.at g path (deep.Split('\n').Length - 2) 6 CancellationToken.None
                  Expect.isNonEmpty inner "a row of 4000 lets answers"
                  g.Dispose()
                  (host :> IDisposable).Dispose()
              finally
                  try
                      dir.Delete true
                  with :? IOException | :? UnauthorizedAccessException ->
                      ()
          } ]
