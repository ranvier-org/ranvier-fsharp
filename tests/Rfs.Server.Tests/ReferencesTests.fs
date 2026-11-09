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
      IsLoading = fun () -> s.Server.Workspace.IsLoading
      BeforeRead = fun _ -> Task.CompletedTask }

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

          testTask "edits that move no use still answer, at each pass" {
              let a, b = overlapping ()
              let! (s: Session) = start depsDir depsProject []
              use _ = disposing s
              let aFile, bFile = file "A.fs", file "B.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              graph.SetText(aFile, a 0) |> ignore
              let text (i: int) = b 0 + $"let far = %d{i}\n"
              graph.SetText(bFile, text 0) |> ignore
              let! _ = graph.AllDiagnostics() |> within
              let mutable edits = 0

              // Runs once a pass: every pass finds another result of B, of another version, with the same uses
              let scope =
                  { scopeOf s with
                      FilesOf =
                          fun _ ->
                              edits <- edits + 1
                              graph.SetText(bFile, text edits) |> ignore
                              [ aFile; bFile ] }

              let! (answer: References.Answer) = References.find scope graph bFile 1 18 CancellationToken.None

              match answer with
              | References.Answer.Found uses -> Expect.equal uses.Length 2 "the definition and the use"
              | References.Answer.Moved -> failtest "nothing it answers moved"
          }

          testTask "edits that move a use every pass answer Moved" {
              let a, b = overlapping ()
              let! (s: Session) = start depsDir depsProject []
              use _ = disposing s
              let aFile, bFile = file "A.fs", file "B.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              graph.SetText(aFile, a 0) |> ignore
              graph.SetText(bFile, b 0) |> ignore
              let! _ = graph.AllDiagnostics() |> within
              let mutable edits = 0

              let scope =
                  { scopeOf s with
                      FilesOf =
                          fun _ ->
                              edits <- edits + 1
                              graph.SetText(aFile, a edits) |> ignore
                              [ aFile; bFile ] }

              let! (answer: References.Answer) = References.find scope graph bFile 1 18 CancellationToken.None
              Expect.isTrue answer.IsMoved $"no two passes agree: %A{answer}, after %d{edits} edits"
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

          testTask "a load that ends during the request, or another set of projects by its end, answers Moved" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs" ]
              use _ = disposing s
              let aFile = file "A.fs"
              let graph = (s.Server.Workspace.ProjectOf aFile).Value
              let at = position (s.Lsp.Text aFile) "baseValue" 0 1
              let mutable loading = true

              // The load ends once the scan is under way: only a read before the snapshot sees it
              let ended =
                  { scopeOf s with
                      IsLoading = fun () -> loading
                      BeforeRead =
                          fun _ ->
                              loading <- false
                              Task.CompletedTask }

              let! (loadEnded: References.Answer) = References.find ended graph aFile (fst at) (snd at) CancellationToken.None
              Expect.isTrue loadEnded.IsMoved $"the snapshot was taken during a load: %A{loadEnded}"
              let other = ProjectSpec.sequential (ProjectSpec.load depsProject).Result
              let! (app: Rfs.Checker.ProjectGraph) = Rfs.Checker.ProjectGraph.Create(ReferencesTestsHost.host.Value, { other with ProjectFile = Path.Combine(Path.GetDirectoryName depsProject, "Back.fsproj") })
              let mutable asks = 0

              let cameBack =
                  { scopeOf s with
                      ReferencingProjects =
                          fun _ ->
                              asks <- asks + 1
                              if asks <= 1 then [] else [ app ] }

              let! (projectBack: References.Answer) = References.find cameBack graph aFile (fst at) (snd at) CancellationToken.None
              app.Dispose()
              Expect.isTrue projectBack.IsMoved $"a project came back during the passes: %A{projectBack}"
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
                      let mutable readsOfB = 0
                      let mutable readsOfC = 0

                      // Read A B [edit] C A [edit] B C: A's uses are the same at the texts it is read at, B's at its, and C's at
                      // its, and at each of the three states one of the files, not read at it, says otherwise
                      let edit (text: string) : Task =
                          task {
                              g.SetText(a, text) |> ignore
                              do! settle ()
                          }

                      let scope: References.Scope =
                          { OwnersOf = fun f -> if List.contains f [ a; b; c ] then [ g ] else []
                            ReferencingProjects = fun _ -> []
                            Projects = fun () -> [ g ]
                            FilesOf = fun _ -> [ a; b; c ]
                            IsLoading = fun () -> false
                            BeforeRead =
                              fun file ->
                                  if file = c then
                                      readsOfC <- readsOfC + 1
                                      if readsOfC = 1 then edit (aText "t" "other" "0") else Task.CompletedTask
                                  elif file = b then
                                      readsOfB <- readsOfB + 1
                                      if readsOfB = 2 then edit (aText "other" "other" "t") else Task.CompletedTask
                                  else
                                      Task.CompletedTask }

                      let! (answer: References.Answer) = References.find scope g a 2 4 CancellationToken.None
                      g.Dispose()
                      (host :> IDisposable).Dispose()

                      match answer with
                      | References.Answer.Moved -> ()
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
              let! (before: Answer) = references s aFile at true
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
