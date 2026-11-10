module Rfs.Server.Tests.HoverTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Types
open FSharp.Compiler.SourceCodeServices
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests
open Rfs.Server.Tests.PublishTests

/// The 0-based line and character of the `nth` (from 0) occurrence of `needle` in `text`, `inside` characters into it
let private position (text: string) (needle: string) (nth: int) (inside: int) =
    let mutable index = -1

    for _ in 0..nth do
        index <- text.IndexOf(needle, index + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual index 0 $"%s{needle} occurs %d{nth + 1} times"
    let before = text.Substring(0, index)
    let line = before.Split('\n').Length - 1
    line, index - (before.LastIndexOf '\n' + 1) + inside

let private hoverParams (path: string) (line: int, character: int) : JsonNode =
    let position = JsonObject()
    position["line"] <- JsonValue.Create line
    position["character"] <- JsonValue.Create character
    let parameters = JsonObject()
    let document = JsonObject()
    document["uri"] <- JsonValue.Create(Paths.toUri path)
    parameters["textDocument"] <- document
    parameters["position"] <- position
    parameters :> JsonNode

let private hoverText (s: Session) (path: string) (at: int * int) : Task<string option> =
    task {
        let! result, _ = s.Lsp.Request("textDocument/hover", hoverParams path at) |> within

        return
            match result with
            | null -> None
            | r -> (let v = r["contents"].AsObject()["value"] in Some(v.GetValue<string>()))
    }

/// What a from-scratch check of the project, with `texts` as the sources, makes of the hover at the position, formatted by
/// the same port
let private expected (s: Session) (texts: Map<string, string>) (path: string) ((line, character): int * int) : Task<string option> =
    task {
        let! spec = ProjectSpec.load s.Project
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
        let sourceReader (f: string) = (let t = texts[f] in hash t, lazy t)

        let! _, check, _ =
            checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, sourceReader, path)
            |> Async.StartAsTask

        return Hover.tooltipOf check texts[path] line character
    }

let private diskTexts () =
    [ a; b; c; file "D.fs" ] |> List.map (fun f -> f, File.ReadAllText f) |> Map.ofList

let private hoverRequest (path: string) (line: int, character: int) : HoverParams =
    { TextDocument = { Uri = Paths.toUri path }
      Position = { Line = uint32 line; Character = uint32 character }
      WorkDoneToken = None }

/// Waits until the file is checked by declaration for its latest text
let private promoted (s: Session) (path: string) =
    task {
        let graph = (s.Server.Workspace.ProjectOf path).Value
        let! mode = graph.WhenPromoted(path, CancellationToken.None) |> within
        Expect.equal mode Rfs.Checker.FileMode.Declarations $"%s{Path.GetFileName path} is checked by declaration"
    }

let private signedFsi = "module Deps.E\n\n/// Adds one\nval f: x: int -> int\n\nval g: int\n"
let private signedFs (k: int) = $"module Deps.E\n\nlet f y = y + 1\n\nlet g = f %d{k}\n"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Hover"
        [ testTask "hover equals FCS" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a; b; c ]
              do! eventually "the open publishes" (fun () -> hasVersion s a 0 && hasVersion s b 0 && hasVersion s c 0)
              let texts = diskTexts ()
              let aText, bText, cText = texts[a], texts[b], texts[c]

              let positions =
                  [ a, position aText "baseValue" 0 2
                    a, position aText "scale" 1 1
                    b, position bText "length" 1 1
                    c, position cText "describe" 1 2
                    c, position cText "area" 2 1 ]

              for path, at in positions do
                  let! actual = hoverText s path at
                  let! reference = expected s texts path at
                  Expect.isSome reference $"FCS has a tooltip at %A{at} in %s{Path.GetFileName path}"
                  Expect.equal actual reference $"hover at %A{at} in %s{Path.GetFileName path} equals FCS"
          }

          testTask "hover during edits is consistent" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let at = position (s.Lsp.Text a) "baseValue" 0 2

              for i in 1..20 do
                  let literal = if i % 2 = 0 then "10" else "\"x\""
                  let current = s.Lsp.Text a
                  let find = if i % 2 = 0 then "\"x\"" else "10"
                  do! s.Lsp.DidChange(a, i, replace s a find literal)
                  Expect.notEqual (s.Lsp.Text a) current "the edit changed the text"
                  let! actual = hoverText s a at
                  let! reference = expected s (diskTexts () |> Map.add a (s.Lsp.Text a)) a at
                  Expect.equal actual reference $"edit %d{i}: the hover is the tooltip of the text it followed"
          }

          testTask "hover in a kept row after an earlier row's body edit" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! promoted s a
              let graph = (s.Server.Workspace.ProjectOf a).Value
              let ran = System.Collections.Concurrent.ConcurrentQueue<_>()
              let _sub = graph.Ran |> Observable.subscribe (fun (k: FSharp.Compiler.Declarations.DeclarationKey, _) -> ran.Enqueue k.Name)
              let! before = hoverText s a (position (s.Lsp.Text a) "scale" 1 1)
              Expect.isSome before "a tooltip before"
              do! s.Lsp.DidChange(a, 1, replace s a "x * 2" "x * 3")
              do! eventually "the edit's publish" (fun () -> hasVersion s a 1)
              do! promoted s a
              Expect.isNonEmpty (ran.ToArray()) "the edited row was checked"
              Expect.isFalse (ran.ToArray() |> Array.contains "grouped") "grouped was kept, not checked again"
              let texts = diskTexts () |> Map.add a (s.Lsp.Text a)

              // `doubled` and `grouped` come after the edited row, and are kept when it changes
              for needle, nth, inside in [ "scale", 1, 1; "baseValue", 2, 2; "grouped", 0, 2; "doubled", 0, 2 ] do
                  let at = position (s.Lsp.Text a) needle nth inside
                  let! actual = hoverText s a at
                  let! reference = expected s texts a at
                  Expect.isSome reference $"FCS has a tooltip for %s{needle}"
                  Expect.equal actual reference $"%s{needle} in a kept row equals FCS"
          }

          testTask "hover in a row shifted by a line insert" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! promoted s a
              let graph = (s.Server.Workspace.ProjectOf a).Value
              let shifted = graph.ShiftedRows
              do! s.Lsp.DidChange(a, 1, replace s a "let baseValue = 10" "\nlet baseValue = 10")
              do! eventually "the edit's publish" (fun () -> hasVersion s a 1)
              do! promoted s a
              Expect.isGreaterThan graph.ShiftedRows shifted "the rows after the insert were moved, not checked again"
              let texts = diskTexts () |> Map.add a (s.Lsp.Text a)

              for needle, nth, inside in [ "grouped", 0, 2; "doubled", 0, 2; "area", 0, 1 ] do
                  let at = position (s.Lsp.Text a) needle nth inside
                  let! actual = hoverText s a at
                  let! reference = expected s texts a at
                  Expect.isSome reference $"FCS has a tooltip for %s{needle}"
                  Expect.equal actual reference $"%s{needle} in a shifted row equals FCS"
          }

          // A consistency test: it passes with the FileStart's lock shared too. The pin for the exclusive lock is
          // `a read holds a finish off the file until it is done` in Rfs.Checker.Tests.
          testTask "hovers overlapping edits of a signed file all equal a settled tooltip" {
              let! s = startSession () |> within
              use _ = disposing s
              let efsi, efs = file "E.fsi", file "E.fs"
              do! s.Lsp.DidOpen(efsi, signedFsi)
              do! s.Lsp.DidOpen(efs, signedFs 1)
              do! eventually "the open publish" (fun () -> hasVersion s efs 0)
              do! promoted s efs
              let at = (2, 4)
              let! reference = expected s (diskTexts () |> Map.add efsi signedFsi |> Map.add efs (signedFs 1)) efs at
              Expect.isSome reference "FCS has a tooltip for f"
              Expect.stringContains reference.Value "Adds one" "with the signature's documentation"

              // Every edit re-checks `g`, which undoes the last finish of the file; the hovers are not waited for
              let hovers = ResizeArray()

              for i in 1..30 do
                  do! s.Lsp.DidChange(efs, i, replace s efs $"let g = f %d{i}" $"let g = f %d{i + 1}")
                  hovers.Add(hoverText s efs at)

              // Answered from the last state while an edit is pending, marked: f's row is kept, so the tooltip is the same
              let unmarked (t: string option) =
                  t |> Option.map (fun t -> if t.StartsWith(Hover.StaleMark + "\n\n") then t.Substring((Hover.StaleMark + "\n\n").Length) else t)

              for h in hovers do
                  let! actual = h |> within
                  Expect.equal (unmarked actual) reference "an answer for a settled text, or the same one marked: f keeps its signature's name and docs"
          }

          testTask "hover shows the documentation of a referenced assembly's member" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! s.Lsp.DidChange(a, 1, replace s a "let grouped = (baseValue + 1) * 2" "let grouped = (baseValue + 1) * 2\n\nlet mapped = List.map scale [ 1; 2 ]")
              let! actual = hoverText s a (position (s.Lsp.Text a) "List.map" 0 6)
              Expect.isSome actual "a tooltip for List.map"
              Expect.stringContains actual.Value "Builds a new collection" "with FSharp.Core's documentation from its XML file"
          }

          testTask "hover on an operator answers, and in a string or a comment answers nothing" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)

              do!
                  s.Lsp.DidChange(
                      a,
                      1,
                      replace s a "let grouped = (baseValue + 1) * 2" "let grouped = (baseValue + 1) * 2\n\nlet text = \"baseValue scale\"\n\n// baseValue in a comment\n(* scale *)"
                  )

              let text = s.Lsp.Text a
              let! op = hoverText s a (position text "+ 1" 0 0)
              Expect.isSome op "a tooltip for +"
              let! inString = hoverText s a (position text "\"baseValue" 0 3)
              Expect.isNone inString "nothing inside a string"
              let! inComment = hoverText s a (position text "// baseValue" 0 5)
              Expect.isNone inComment "nothing inside a line comment"
              let! inBlock = hoverText s a (position text "(* scale" 0 4)
              Expect.isNone inBlock "nothing inside a block comment"
              let! code = hoverText s a (position text "let grouped" 0 6)
              Expect.isSome code "code still answers"
          }

          testTask "hover inside conditional compilation blocks answers as a lookup of the line does" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)

              let added =
                  [ "let grouped = (baseValue + 1) * 2"
                    ""
                    "#if DEBUG"
                    "let inDebug = baseValue + 1"
                    "#else"
                    "let notDebug = baseValue + 2"
                    "#endif"
                    ""
                    "#if NEVER_DEFINED"
                    "let inactive = baseValue + 3"
                    "#else"
                    "let inElse = baseValue + 4"
                    "#endif"
                    ""
                    "#if !NEVER_DEFINED"
                    "let inNot = baseValue + 5"
                    "#endif"
                    ""
                    "let afterBlocks = baseValue + 6" ]

              do! s.Lsp.DidChange(a, 1, replace s a "let grouped = (baseValue + 1) * 2" (String.Join("\n", added)))
              let text = s.Lsp.Text a
              let texts = diskTexts () |> Map.add a text

              let lookup (needle: string) =
                  let line, character = position text needle 0 1
                  let lines = text.Split '\n'
                  let lineText = lines[line].TrimEnd '\r'
                  line, character, lineText, needle.Length + lineText.IndexOf needle

              let! spec = ProjectSpec.load s.Project
              let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
              let sourceReader (f: string) = (let t = texts[f] in hash t, lazy t)

              let! _, (check: FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults), _ =
                  checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, sourceReader, a)
                  |> Async.StartAsTask

              // What FSAC gets from lexing the line alone and asking for the name it finds there
              let ofLine (needle: string) =
                  let line, _, lineText, endColumn = lookup needle
                  Hover.format (check.GetToolTip(line + 1, endColumn, lineText, [ needle ], FSharp.Compiler.Tokenization.FSharpTokenTag.Identifier))

              for needle, answers in
                  [ "inDebug", true
                    "inElse", true
                    "inNot", true
                    "afterBlocks", true
                    // Not compiled, so nothing is known about it
                    "inactive", false ] do
                  let line, character, _, _ = lookup needle
                  let! (actual: string option) = hoverText s a (line, character)
                  Expect.equal actual (ofLine needle) $"%s{needle} as a lookup of its line"
                  Expect.equal actual.IsSome answers $"%s{needle}: %A{actual}"
          }

          testTask "a comment or string opened on an earlier line still answers nothing" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)

              let added =
                  [ "let grouped = (baseValue + 1) * 2"
                    ""
                    "(* spans lines"
                    "   #if NEVER_DEFINED"
                    "   baseValue in a comment"
                    "*)"
                    "let afterComment = baseValue + 1"
                    ""
                    "let multi = \"\"\""
                    "   #if NEVER_DEFINED"
                    "   baseValue in a string"
                    "\"\"\""
                    "let afterString = baseValue + 2" ]

              do! s.Lsp.DidChange(a, 1, replace s a "let grouped = (baseValue + 1) * 2" (String.Join("\n", added)))
              let text = s.Lsp.Text a
              let! inComment = hoverText s a (position text "baseValue in a comment" 0 2)
              Expect.isNone inComment "nothing inside a block comment from above"
              let! inString = hoverText s a (position text "baseValue in a string" 0 2)
              Expect.isNone inString "nothing inside a string from above"
              let! afterComment = hoverText s a (position text "afterComment" 0 2)
              Expect.isSome afterComment "the comment ended"
              let! afterString = hoverText s a (position text "afterString" 0 2)
              Expect.isSome afterString "the string ended"
          }

          test "a documentation file written after a miss is read" {
              let dir = Directory.CreateTempSubdirectory "rfs-hover-"

              try
                  let dll = Path.Combine(dir.FullName, "Lib.dll")
                  let doc = FSharp.Compiler.Symbols.FSharpXmlDoc.FromXmlFile(dll, "M:Lib.f")
                  Expect.isNone (Hover.docText doc) "no file yet"

                  File.WriteAllText(
                      Path.ChangeExtension(dll, ".xml"),
                      "<doc><members><member name=\"M:Lib.f\"><summary>Does f</summary></member></members></doc>"
                  )

                  Expect.equal (Hover.docText doc) (Some "Does f") "the file is read once it is there"
              finally
                  dir.Delete true
          }

          test "a documentation file that cannot be read gives no documentation, and is read once it can be" {
              let dir = Directory.CreateTempSubdirectory "rfs-hover-"

              try
                  let dll = Path.Combine(dir.FullName, "Denied.dll")
                  let xml = FileInfo(Path.ChangeExtension(dll, ".xml"))
                  let doc = FSharp.Compiler.Symbols.FSharpXmlDoc.FromXmlFile(dll, "M:Denied.f")
                  File.WriteAllText(xml.FullName, "<doc><members><member name=\"M:Denied.f\"><summary>Does f</summary></member></members></doc>")
                  let security = xml.GetAccessControl()

                  let rule =
                      System.Security.AccessControl.FileSystemAccessRule(
                          System.Security.Principal.WindowsIdentity.GetCurrent().User,
                          System.Security.AccessControl.FileSystemRights.Read,
                          System.Security.AccessControl.AccessControlType.Deny
                      )

                  security.AddAccessRule rule
                  xml.SetAccessControl security

                  try
                      Expect.throwsT<UnauthorizedAccessException> (fun () -> File.ReadAllText xml.FullName |> ignore) "the read is denied"
                      Expect.isNone (Hover.docText doc) "no documentation, and no exception"
                  finally
                      security.RemoveAccessRule rule |> ignore
                      xml.SetAccessControl security

                  Expect.equal (Hover.docText doc) (Some "Does f") "not remembered as empty"
              finally
                  dir.Delete true
          }

          testTask "a hover right after an edit answers from the edit's text" {
              for round in 1..5 do
                  let! s = startSession () |> within
                  use _ = disposing s
                  do! openAll s [ a ]
                  do! eventually "the open publish" (fun () -> hasVersion s a 0)
                  let at = position (s.Lsp.Text a) "baseValue" 0 2
                  // Back to back: the hover is sent before the edit could have been applied
                  do! s.Lsp.DidChange(a, 1, replace s a "10" "\"ten\"")
                  let! actual = hoverText s a at
                  Expect.isSome actual "a tooltip"
                  Expect.stringContains actual.Value "string" $"round %d{round}: the answer is for the edited text"
          }

          testTask "a hover waits for the edits queued before it" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let at = position (s.Lsp.Text a) "baseValue" 0 2
              let gate = new ManualResetEventSlim(false)
              s.Server.Publisher.Hold gate
              do! s.Lsp.DidChange(a, 1, replace s a "10" "\"ten\"")
              // The edit is queued behind the held consumer, not applied
              do! Task.Delay 300
              let pending = hoverText s a at
              do! Task.Delay 500
              Expect.isFalse pending.IsCompleted "the hover does not answer from the text before the edit"
              gate.Set()
              let! (actual: string option) = pending |> within
              Expect.stringContains actual.Value "string" "the answer is for the edited text"
          }

          testTask "a hover cancelled while it waits is cancelled" {
              let! s = startSession () |> within
              use _ = disposing s
              let gate = new ManualResetEventSlim(false)
              s.Server.Publisher.Hold gate
              let cts = new CancellationTokenSource()
              let server = s.Server :> ILspServer
              let pending = Async.StartAsTask(server.TextDocumentHover(hoverRequest a (2, 5)), cancellationToken = cts.Token)
              do! Task.Delay 300
              Expect.isFalse pending.IsCompleted "waiting for the queue"
              cts.Cancel()

              let! outcome =
                  task {
                      try
                          let! _ = pending
                          return "answered"
                      with :? OperationCanceledException ->
                          return "cancelled"
                  }
                  |> within

              gate.Set()
              Expect.equal outcome "cancelled" "the wait ended with the request"
          }

          testTask "a hover while its project reloads answers null" {
              let! s = startSession () |> within
              use _ = disposing s
              let d = file "D.fs"
              let graph = (s.Server.Workspace.ProjectOf d).Value
              let gate = new ManualResetEventSlim(false)
              graph.OnScopedCheck(fun path -> if path.EndsWith "D.fs" then gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)
              let text = File.ReadAllText d
              s.Server.Workspace.Open(d, text.Replace("42", "43"), 1) |> ignore
              let pending = hoverText s d (position text "fill" 0 1)
              do! Task.Delay 500
              Expect.isFalse pending.IsCompleted "the file cannot settle while its check is held"
              // The reload hides the project while it loads again
              do! s.Server.Workspace.Reload s.Project |> within
              let! (actual: string option) = pending |> within
              gate.Set()
              Expect.isNone actual "the project went away: no hover"
          }

          testTask "a hover after the shutdown answers null" {
              let! s = startSession () |> within
              use _ = disposing s
              (s.Server :> IDisposable).Dispose()
              let server = s.Server :> ILspServer
              let! result = Async.StartAsTask(server.TextDocumentHover(hoverRequest a (2, 5))) |> within

              match result with
              | Ok None -> ()
              | other -> failtest $"expected no hover, got %A{other}"
          } ]
