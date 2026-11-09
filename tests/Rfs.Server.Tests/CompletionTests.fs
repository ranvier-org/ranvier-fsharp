module Rfs.Server.Tests.CompletionTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.SourceCodeServices
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests
open Rfs.Server.Tests.PublishTests

/// The 0-based line and character just after the `nth` (from 0) occurrence of `needle` in `text`
let private after (text: string) (needle: string) (nth: int) =
    let mutable index = -1

    for _ in 0..nth do
        index <- text.IndexOf(needle, index + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual index 0 $"%s{needle} occurs %d{nth + 1} times"
    let before = text.Substring(0, index + needle.Length)
    let line = before.Split('\n').Length - 1
    line, before.Length - (before.LastIndexOf '\n' + 1)

let private atParams (path: string) (line: int, character: int) : JsonNode =
    let position = JsonObject()
    position["line"] <- JsonValue.Create line
    position["character"] <- JsonValue.Create character
    let parameters = JsonObject()
    let document = JsonObject()
    document["uri"] <- JsonValue.Create(Paths.toUri path)
    parameters["textDocument"] <- document
    parameters["position"] <- position
    parameters :> JsonNode

/// The items answered, or None when the answer is null
let private completionItems (s: Session) (path: string) (at: int * int) : Task<JsonObject[] option> =
    task {
        let! result, _ = s.Lsp.Request("textDocument/completion", atParams path at) |> within

        return
            match result with
            | null -> None
            | r -> Some [| for i in r["items"].AsArray() -> i.AsObject() |]
    }

let private completionNames (s: Session) (path: string) (at: int * int) : Task<Set<string>> =
    task {
        let! (items: JsonObject[] option) = completionItems s path at
        Expect.isSome items "a list"
        return items.Value |> Array.map (fun i -> i["label"].GetValue<string>()) |> Set.ofArray
    }

let private resolved (s: Session) (item: JsonObject) : Task<JsonObject> =
    task {
        let! result, _ = s.Lsp.Request("completionItem/resolve", item.DeepClone()) |> within
        return result.AsObject()
    }

/// What a from-scratch check of the project, with `texts` as the sources, lists at the position through the same port
let private expected (s: Session) (texts: Map<string, string>) (path: string) ((line, character): int * int) : Task<Set<string>> =
    task {
        let! spec = ProjectSpec.load s.Project
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
        let sourceReader (f: string) = (let t = texts[f] in hash t, lazy t)

        let! parse, check, _ =
            checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, sourceReader, path)
            |> Async.StartAsTask

        return
            Completion.itemsOf check parse texts[path] line character
            |> Array.map _.NameInList
            |> Set.ofArray
    }

let private diskTexts () =
    [ a; b; c; file "D.fs" ] |> List.map (fun f -> f, File.ReadAllText f) |> Map.ofList

/// Waits until the file is checked by declaration for its latest text
let private promoted (s: Session) (path: string) =
    task {
        let graph = (s.Server.Workspace.ProjectOf path).Value
        let! mode = graph.WhenPromoted(path, CancellationToken.None) |> within
        Expect.equal mode Rfs.Checker.FileMode.Declarations $"%s{Path.GetFileName path} is checked by declaration"
    }

let private d = file "D.fs"

/// `oldName` is the row to rename; nothing after it depends on it, so the rows after it are kept
let private keptText =
    "module Deps.D\n\nlet oldName = 1\n\nlet between = 2\n\nlet kept =\n    let inner = 3\n    inner\n\nlet last = 4\n"

/// A session with D.fs open as `text`, checked by declaration
let private openD (text: string) =
    task {
        let! s = startSession () |> within
        do! s.Lsp.DidOpen(d, text)
        do! eventually "the open publish" (fun () -> hasVersion s d 0)
        do! promoted s d
        return s
    }

let private rename (s: Session) (version: int) (find: string) (text: string) =
    task {
        do! s.Lsp.DidChange(d, version, replace s d find text)
        do! eventually "the edit's publish" (fun () -> hasVersion s d version)
        do! promoted s d
    }

let private augmentedText =
    "module Deps.D\n\nlet earlier = 1\n\ntype T() =\n    member _.A = 1\n\ntype T with\n    member _.B = 2\n\nlet useT = T().A\n\nlet last = 4\n"

let private lib = Path.Combine(Path.GetDirectoryName depsDir, "Multi", "Lib", "Lib.fs")
let private useFs = Path.Combine(Path.GetDirectoryName depsDir, "Multi", "App", "Use.fs")
let private appProject = Path.Combine(Path.GetDirectoryName depsDir, "Multi", "App", "App.fsproj")

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Completion"
        [ testTask "completion equals FCS on Deps positions" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a; b; c ]
              do! eventually "the open publishes" (fun () -> hasVersion s a 0 && hasVersion s b 0 && hasVersion s c 0)
              do! promoted s a
              do! promoted s b
              let texts = diskTexts ()

              let positions =
                  [ a, after texts[a] "let doubled = sc" 0
                    a, after texts[a] "(ba" 0
                    b, after texts[b] "Counter()." 0
                    b, after texts[b] "Util." 0
                    b, after texts[b] "c.Sta" 0
                    c, after texts[c] "desc" 1
                    c, after texts[c] "area (Cir" 0 ]

              for path, at in positions do
                  let! (actual: Set<string>) = completionNames s path at
                  let! reference = expected s texts path at
                  Expect.isNonEmpty reference $"FCS lists something at %A{at} in %s{Path.GetFileName path}"
                  Expect.equal actual reference $"completion at %A{at} in %s{Path.GetFileName path} equals FCS"

              let! (counter: Set<string>) = completionNames s b (after texts[b] "Counter()." 0)
              Expect.contains counter "Next" "the augmentation's member"
              Expect.contains counter "Start" "the type's own member"
          }

          testTask "completion inside a string answers nothing" {
              let! s = openD "module Deps.D\n\nlet text = \"abc\"\n\nlet last = 4\n"
              use _ = disposing s
              let! (items: JsonObject[] option) = completionItems s d (after (s.Lsp.Text d) "\"ab" 0)
              Expect.equal (items |> Option.map _.Length |> Option.defaultValue 0) 0 "no items"
          }

          testTask "completion in a kept row sees a renamed earlier value" {
              let! s = openD keptText
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! rename s 1 "let oldName" "let newName"
              let adHoc, adopted, ran = graph.AdHocChecks, graph.AdoptedChecks, graph.CheckedCount
              let text = s.Lsp.Text d
              let at = fst (after text "    inner" 0), 4
              let! (names: Set<string>) = completionNames s d at
              Expect.contains names "newName" "the current name"
              Expect.isFalse (names.Contains "oldName") "not the name it replaced"
              Expect.equal (graph.AdHocChecks - adHoc) 1L "the kept row was checked for the query"
              Expect.equal graph.AdoptedChecks adopted "and not adopted"
              Expect.equal graph.CheckedCount ran "no check of the graph ran"
              let! reference = expected s (diskTexts () |> Map.add d text) d at
              Expect.equal names reference "equals FCS"
          }

          testTask "ad hoc check is not adopted" {
              let! s = openD keptText
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! rename s 1 "let oldName" "let newName"
              let publishes = (publishesOf s d).Length
              let adopted = graph.AdoptedChecks
              let line = fst (after (s.Lsp.Text d) "    inner" 0)
              let! _ = completionNames s d (line, 4)
              let! _ = completionNames s d (line, 4)
              Expect.equal graph.AdHocChecks 2L "each request checks it again: nothing kept it"
              Expect.equal graph.AdoptedChecks adopted "neither was adopted"
              Expect.equal (publishesOf s d).Length publishes "nothing was published"

              // A real edit of the row is checked, adopted, and from then on needs no ad hoc check
              do! rename s 2 "let inner = 3" "let inner = 5"
              Expect.isGreaterThan graph.AdoptedChecks adopted "the edit's check was adopted"
              let! (names: Set<string>) = completionNames s d (line, 4)
              Expect.contains names "newName" "the row checked in the current environment"
              Expect.equal graph.AdHocChecks 2L "and answered from the published results"
              do! expectEqualOracle s [ d ] d 2
          }

          testTask "an ad hoc check leaves the published results of a signed file as they were" {
              let! s = startSession () |> within
              use _ = disposing s
              let efsi, efs = file "E.fsi", file "E.fs"
              do! s.Lsp.DidOpen(efsi, "module Deps.E\n\n/// Adds one\nval f: x: int -> int\n\nval g: int\n")
              do! s.Lsp.DidOpen(efs, "module Deps.E\n\nlet f y = y + 1\n\nlet other = 1\n\nlet g =\n    let inner = f 1\n    inner\n")
              do! eventually "the open publish" (fun () -> hasVersion s efs 0)
              do! promoted s efs
              let graph = (s.Server.Workspace.ProjectOf efs).Value
              do! s.Lsp.DidChange(efs, 1, replace s efs "let other = 1" "let other2 = 1")
              do! eventually "the edit's publish" (fun () -> hasVersion s efs 1)
              do! promoted s efs
              let hover () =
                  graph.ReadFile(
                      efs,
                      (fun view -> Hover.tooltipOf view.Result.CheckResults view.Text 2 4),
                      CancellationToken.None
                  )

              let! (before: string option) = hover () |> within
              Expect.stringContains before.Value "Adds one" "with the signature's documentation"
              let line = fst (after (s.Lsp.Text efs) "    inner" 0)
              let! (names: Set<string>) = completionNames s efs (line, 4)
              Expect.contains names "other2" "the current name"
              Expect.equal graph.AdHocChecks 1L "checked ad hoc"
              let! (later: string option) = hover () |> within
              Expect.equal later before "the conformed items are still conformed: nothing undid the finish"
          }

          testTask "an ad hoc check leaves what the finish solved solved" {
              let! s = openD "module Deps.D\n\nlet earlier = 1\n\nlet items = ResizeArray()\n\nlet later =\n    let inner = 3\n    inner\n"
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! rename s 1 "let earlier" "let earlier2"

              let hover () =
                  graph.ReadFile(d, (fun view -> Hover.tooltipOf view.Result.CheckResults view.Text (fst (after view.Text "let items" 0)) 6), CancellationToken.None)

              let! (before: string option) = hover () |> within
              let line = fst (after (s.Lsp.Text d) "    inner" 0)
              let! (names: Set<string>) = completionNames s d (line, 4)
              Expect.contains names "earlier2" "the current name"
              Expect.isGreaterThan graph.AdHocChecks 0L "checked ad hoc"
              let! (later: string option) = hover () |> within
              Expect.equal later before "the variable the finish defaulted is still solved in the published results"
          }

          testTask "completion in a file with an augmentation equals FCS, in and after a stale segment" {
              let! s = openD augmentedText
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let positions (text: string) =
                  [ after text "T()." 0
                    fst (after text "member _.B = 2" 0), (snd (after text "member _.B = 2" 0)) - 1 ]

              let assertEqualFcs () =
                  task {
                      let text = s.Lsp.Text d

                      for at in positions text do
                          let! (actual: Set<string>) = completionNames s d at
                          let! reference = expected s (diskTexts () |> Map.add d text) d at
                          Expect.isNonEmpty reference $"FCS lists something at %A{at}"
                          Expect.equal actual reference $"at %A{at} equals FCS"
                  }

              do! assertEqualFcs ()
              Expect.equal graph.AdHocChecks 0L "nothing is stale yet"
              let! (names: Set<string>) = completionNames s d (after (s.Lsp.Text d) "T()." 0)
              Expect.contains names "A" "the type's own member"
              Expect.contains names "B" "the augmentation's member"

              do! rename s 1 "let earlier" "let earlier2"
              let before = graph.AdHocChecks
              do! assertEqualFcs ()
              // `useT` on its own, then the augmentation from the type it extends: alone it would add to the published type
              Expect.equal (graph.AdHocChecks - before) 3L "both rows are kept and stale"

              // The ad hoc checks of the segment left the type as it was: one member B, and diagnostics as the oracle's
              let! (again: Set<string>) = completionNames s d (after (s.Lsp.Text d) "T()." 0)
              Expect.contains again "B" "still there"

              let! (overloads: int) =
                  graph.ReadFile(
                      d,
                      (fun view ->
                          let line, character = after view.Text "T()." 0

                          Completion.itemsOf view.Result.CheckResults view.Parse view.Text line character
                          |> Array.find (fun i -> i.NameInList = "B")
                          |> _.Description
                          |> fun (FSharp.Compiler.EditorServices.ToolTipText elements) ->
                              elements
                              |> List.sumBy (function
                                  | FSharp.Compiler.EditorServices.ToolTipElement.Group items -> items.Length
                                  | _ -> 0)),
                      CancellationToken.None
                  )
                  |> within

              Expect.equal overloads 1 "the published type has the one member"
              do! s.Lsp.DidChange(d, 2, replace s d "let useT = T().A" "let useT = T().B")
              do! eventually "the next edit's publish" (fun () -> hasVersion s d 2)
              do! expectEqualOracle s [ d ] d 2
          }

          // A check folds environments, whose replays hide a type's members in place under the FileStart's Sync, which no
          // query takes: only the read's exclusive hold of the FileStart's lock keeps them from overlapping a query.
          testTask "a read holds off the checks of the file, whose environment folds hide members" {
              let! s = openD augmentedText
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let entered = new ManualResetEventSlim(false)
              let release = new ManualResetEventSlim(false)

              let held =
                  graph.ReadFile(
                      d,
                      (fun _ ->
                          entered.Set()
                          release.Wait(TimeSpan.FromSeconds 60.)),
                      CancellationToken.None
                  )

              let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
              let ran = graph.CheckedCount
              do! s.Lsp.DidChange(d, 1, replace s d "let earlier" "let earlier2")
              do! Task.Delay 1000
              Expect.equal graph.CheckedCount ran "no check runs while the read holds the file"
              release.Set()
              let! _ = held |> within
              do! eventually "the edit's publish" (fun () -> hasVersion s d 1)
              Expect.isGreaterThan graph.CheckedCount ran "and it runs once the read is done"
          }

          testTask "a completion waits for a check in flight and answers for its text" {
              let! s = openD keptText
              use _ = disposing s
              let gate = new ManualResetEventSlim(false)
              let entered = new ManualResetEventSlim(false)

              DeclarationChecker.AfterUndoHook <-
                  fun path ->
                      if path.EndsWith "D.fs" then
                          entered.Set()
                          gate.Wait(TimeSpan.FromSeconds 60.) |> ignore

              try
                  do! s.Lsp.DidChange(d, 1, replace s d "let oldName" "let newName")
                  let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
                  let line = fst (after (s.Lsp.Text d) "    inner" 0)
                  let pending = completionNames s d (line, 4)
                  do! Task.Delay 500
                  Expect.isFalse pending.IsCompleted "the check holds the file: no answer from a text before it"
                  gate.Set()
                  let! (names: Set<string>) = pending
                  Expect.contains names "newName" "the answer is for the text the check is of"
                  Expect.isFalse (names.Contains "oldName") "not the one before"
              finally
                  gate.Set()
                  DeclarationChecker.AfterUndoHook <- ignore
          }

          testTask "completions overlapping edits each see the name their edit left" {
              let! s = openD augmentedText
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let mutable name = "earlier"
              let pending = ResizeArray()

              for i in 1..15 do
                  let next = $"earlier%d{i}"
                  do! s.Lsp.DidChange(d, i, replace s d $"let %s{name} " $"let %s{next} ")
                  name <- next
                  let text = s.Lsp.Text d
                  // Not waited for: the next edit races this request's ad hoc check
                  pending.Add(i, completionNames s d (after text "T()." 0))
                  pending.Add(i, completionNames s d (after text "let useT = T" 0))

              let suffix (n: string) = if n = "earlier" then 0 else int (n.Substring 7)

              for i, answer in pending do
                  let! (names: Set<string>) = answer |> within

                  if names.Contains "A" then
                      Expect.contains names "B" "the query never sees the type while a replay hides its member"
                  else
                      let seen = names |> Set.filter (fun n -> n.StartsWith "earlier") |> Set.toList
                      Expect.equal seen.Length 1 $"one name for the row: %A{seen}"
                      Expect.isGreaterThanOrEqual (suffix seen.Head) i "the answer is for the request's edit or a later one"

              Expect.isGreaterThan graph.AdHocChecks 0L "the kept row was checked for the requests"
              do! eventually "the last edit's publish" (fun () -> hasVersion s d 15)
              do! expectEqualOracle s [ d ] d 15
          }

          testTask "completion in a stale window equals FCS, and the window keeps its solution" {
              let! s = openD "module Deps.D\n\nlet earlier = 1\n\nlet r = ref []\n\nlet fill () = r.Value <- [ 1 ]\n\nlet size () = r.Value.Length\n\nlet later = 42\n"
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! rename s 1 "let earlier" "let earlier2"
              let text = s.Lsp.Text d

              for at in [ after text "size () = r.Value." 0; after text "fill () = r." 0 ] do
                  let! (actual: Set<string>) = completionNames s d at
                  let! reference = expected s (diskTexts () |> Map.add d text) d at
                  Expect.isNonEmpty reference $"FCS lists something at %A{at}"
                  Expect.equal actual reference $"at %A{at} equals FCS"

              Expect.equal graph.AdHocChecks 3L "`size` on its own after the window closed, and `fill` from the leader"
              let! (afterAll: Set<string>) = completionNames s d (after text "size () = r.Value." 0)
              Expect.contains afterAll "Length" "the list's members: the check of the row after the leader solved its variable"
              do! s.Lsp.DidChange(d, 2, replace s d "let later = 42" "let later = 43")
              do! eventually "the next edit's publish" (fun () -> hasVersion s d 2)
              do! expectEqualOracle s [ d ] d 2
          }

          testTask "completion in a stale row of a nested module equals FCS, and the module keeps its type" {
              let! s = openD "module Deps.D\n\nlet earlier = 1\n\nmodule Inner =\n    let a = 1\n    let b =\n        let c = 2\n        c\n\nlet afterInner = Inner.a\n\nlet last = 1\n"
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              do! rename s 1 "let earlier" "let earlier2"
              let text = s.Lsp.Text d
              let line = fst (after text "        c" 0)

              for at in [ (line, 8); after text "afterInner = Inner." 0 ] do
                  let! (actual: Set<string>) = completionNames s d at
                  let! reference = expected s (diskTexts () |> Map.add d text) d at
                  Expect.isNonEmpty reference $"FCS lists something at %A{at}"
                  Expect.equal actual reference $"at %A{at} equals FCS"

              Expect.isGreaterThan graph.AdHocChecks 0L "checked ad hoc"
              let inner = after text "afterInner = Inner." 0

              let! published =
                  graph.ReadFile(d, (fun view -> Completion.itemsOf view.Result.CheckResults view.Parse view.Text (fst inner) (snd inner) |> Array.map _.NameInList |> Set.ofArray), CancellationToken.None)
                  |> within

              Expect.equal published (Set.ofList [ "a"; "b" ]) "the published module holds what it held"
              do! s.Lsp.DidChange(d, 2, replace s d "let last = 1" "let last = 2")
              do! eventually "the next edit's publish" (fun () -> hasVersion s d 2)
              do! expectEqualOracle s [ d ] d 2
          }

          testTask "a line insert takes no ad hoc check: shifted rows hold the same names" {
              let! s = openD keptText
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let shifted = graph.ShiftedRows
              do! rename s 1 "let oldName" "\nlet oldName"
              Expect.isGreaterThan graph.ShiftedRows shifted "the rows were moved, not checked again"
              let text = s.Lsp.Text d
              let at = fst (after text "    inner" 0), 4
              let! (names: Set<string>) = completionNames s d at
              let! reference = expected s (diskTexts () |> Map.add d text) d at
              Expect.equal names reference "equals FCS"
              Expect.equal graph.AdHocChecks 0L "answered from the published results"
          }

          testTask "file-level files use their check results" {
              for text in
                  [ "module rec Deps.D\n\nlet a = 1\n\nlet b = a\n"
                    "namespace Deps.P\n\ntype T() =\n    member _.A = 1\n\ntype T with\n    member _.B = 2\n\nnamespace Deps.Q\n\nopen Deps.P\n\nmodule M =\n    let use1 = T().A\n" ] do
                  let! s = startSession () |> within
                  use _ = disposing s
                  do! s.Lsp.DidOpen(d, text)
                  do! eventually "the open publish" (fun () -> hasVersion s d 0)
                  let graph = (s.Server.Workspace.ProjectOf d).Value
                  let! mode = graph.WhenPromoted(d, CancellationToken.None) |> within
                  Expect.equal mode Rfs.Checker.FileMode.FileLevel "stays file level"
                  let at = if text.Contains "T()." then after text "T()." 0 else after text "let b = " 0
                  let! (actual: Set<string>) = completionNames s d at
                  let! reference = expected s (diskTexts () |> Map.add d text) d at
                  Expect.isNonEmpty reference "FCS lists something"
                  Expect.equal actual reference "equals FCS"
                  Expect.equal graph.AdHocChecks 0L "no ad hoc check"
          }

          testTask "resolve returns documentation" {
              let! s = openD "module Deps.D\n\n/// The documented one\nlet documented = 1\n\nlet use1 = docu\n\nlet use2 = List.ma\n"
              use _ = disposing s
              let text = s.Lsp.Text d
              let! (items: JsonObject[] option) = completionItems s d (after text "use1 = docu" 0)
              let item = items.Value |> Array.find (fun i -> i["label"].GetValue<string>() = "documented")
              let! (own: JsonObject) = resolved s item
              let value = own["documentation"].["value"].GetValue<string>()
              Expect.stringContains value "The documented one" "the doc comment"
              Expect.stringContains value "documented" "the signature"
              Expect.stringContains (own["detail"].GetValue<string>()) "documented" "the signature in the detail"

              let! (items: JsonObject[] option) = completionItems s d (after text "use2 = List." 0)
              let item = items.Value |> Array.find (fun i -> i["label"].GetValue<string>() = "map")
              let! (fromFile: JsonObject) = resolved s item
              Expect.stringContains (fromFile["documentation"].["value"].GetValue<string>()) "Builds a new collection" "FSharp.Core's XML file"
          }

          testTask "resolve of an item whose file moved on leaves it as it is" {
              let! s = openD keptText
              use _ = disposing s
              let text = s.Lsp.Text d
              let line = fst (after text "    inner" 0)
              let! (items: JsonObject[] option) = completionItems s d (line, 4)
              let item = items.Value |> Array.find (fun i -> i["label"].GetValue<string>() = "oldName")
              do! rename s 1 "let oldName" "let newName"
              let! (later: JsonObject) = resolved s item
              Expect.equal (later["label"].GetValue<string>()) "oldName" "the item"
              Expect.isNull later["documentation"] "no documentation for what is gone"
          }

          // Not a behaviour to keep: the fingerprint across projects reads no XmlDoc, so an edit of Lib's doc comment gives
          // App no new context. Task 16's location ruling meets the same staleness for ranges.
          testTask "resolve on an App item shows Lib's docs as of Lib's last signature change" {
              Fixtures.build appProject
              let! (s: Session) = startSessionWith (Path.Combine(Path.GetDirectoryName depsDir, "Multi")) [ appProject ] true ignore
              use _ = (s :> IAsyncDisposable)
              do! s.Lsp.DidOpen(lib, File.ReadAllText lib)
              do! s.Lsp.DidOpen(useFs, File.ReadAllText useFs)
              do! eventually "App's first publish" (fun () -> (publishesOf s useFs).Length > 0)
              do! eventually "Lib's first publish" (fun () -> hasVersion s lib 0)
              let app = (s.Server.Workspace.ProjectOf useFs).Value
              let at = after (s.Lsp.Text useFs) "let y = f" 0

              let docs () =
                  task {
                      let! _ = app.Diagnostics useFs |> within
                      let! (items: JsonObject[] option) = completionItems s useFs at
                      let item = items.Value |> Array.find (fun i -> i["label"].GetValue<string>() = "f")
                      let! (r: JsonObject) = resolved s item
                      return r["documentation"].["value"].GetValue<string>()
                  }

              // A signature change makes App a new context, whose assembly carries the docs of that moment
              do! s.Lsp.DidChange(lib, 1, replace s lib "let f (x: int)" "/// Old docs\nlet f (x: int)")
              do! s.Lsp.DidChange(lib, 2, replace s lib "let shapeOf ()" "let added = 1\n\nlet shapeOf ()")
              do! eventually "Lib's publish" (fun () -> hasVersion s lib 2)
              let! (first: string) = docs ()
              Expect.stringContains first "Old docs" "the docs of the signature App was made from"

              do! s.Lsp.DidChange(lib, 3, replace s lib "Old docs" "New docs")
              do! eventually "Lib's publish" (fun () -> hasVersion s lib 3)
              let! (second: string) = docs ()
              Expect.stringContains second "Old docs" "stale: the edit changed no fingerprint, so App kept the old assembly"
          }

          testTask "a completion while its project reloads answers null" {
              let! s = startSession () |> within
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let gate = new ManualResetEventSlim(false)
              graph.OnScopedCheck(fun path -> if path.EndsWith "D.fs" then gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)
              let text = File.ReadAllText d
              s.Server.Workspace.Open(d, text.Replace("42", "43"), 1) |> ignore
              let pending = completionItems s d (after text "let fi" 0)
              do! Task.Delay 500
              Expect.isFalse pending.IsCompleted "the file cannot settle while its check is held"
              do! s.Server.Workspace.Reload s.Project |> within
              let! (actual: JsonObject[] option) = pending |> within
              gate.Set()
              Expect.isNone actual "the project went away: no list"
          }

          testTask "completion and resolve after the shutdown answer nothing" {
              let! s = startSession () |> within
              use _ = disposing s
              (s.Server :> IDisposable).Dispose()
              let server = s.Server :> ILspServer

              let request: CompletionParams =
                  { TextDocument = { Uri = Paths.toUri a }
                    Position = { Line = 2u; Character = 5u }
                    WorkDoneToken = None
                    PartialResultToken = None
                    Context = None }

              let! result = Async.StartAsTask(server.TextDocumentCompletion request) |> within

              match result with
              | Ok None -> ()
              | other -> failtest $"expected no list, got %A{other}"

              let item = { CompletionItem.Create "x" with Data = Some(Completion.dataOf a 2 5) }
              let! back = Async.StartAsTask(server.CompletionItemResolve item) |> within

              match back with
              | Ok(same: CompletionItem) -> Expect.equal same.Label "x" "the item as it came"
              | other -> failtest $"expected the item, got %A{other}"
          } ]
