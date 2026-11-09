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
      FilesOf = s.Server.Workspace.FilesOf }

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
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; file "B.fs"; d ]
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

          testTask "cancelling ends the request between files" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let cancelled = new CancellationTokenSource()
              cancelled.Cancel()

              let! outcome =
                  task {
                      try
                          let! (_: References.Answer) = References.find (scopeOf s) graph d 2 4 cancelled.Token
                          return "answered"
                      with :? OperationCanceledException ->
                          return "cancelled"
                  }

              Expect.equal outcome "cancelled" "the token is honoured"
          }

          testTask "a project going away answers nothing for it, and shutdown answers nothing" {
              let! (s: Session) = start depsDir depsProject [ file "A.fs"; d ]
              use _ = disposing s
              let aText = s.Lsp.Text(file "A.fs")
              let at = position aText "baseValue" 0 1
              let! (before: Answer) = references s (file "A.fs") at true
              Expect.isNonEmpty before "answers while loaded"
              let pending = references s (file "A.fs") at true
              let reload = s.Server.Workspace.Reload depsProject
              let! (_: Answer) = pending
              do! reload
              do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
              (s.Server :> IDisposable).Dispose()
              let! (afterShutdown: Answer) = references s (file "A.fs") at true
              Expect.isEmpty afterShutdown "after shutdown"
          } ]
