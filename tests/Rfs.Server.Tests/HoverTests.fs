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
              let! before = hoverText s a (position (s.Lsp.Text a) "scale" 1 1)
              Expect.isSome before "a tooltip before"
              do! s.Lsp.DidChange(a, 1, replace s a "x * 2" "x * 3")
              do! eventually "the edit's publish" (fun () -> hasVersion s a 1)
              let texts = diskTexts () |> Map.add a (s.Lsp.Text a)

              // `doubled` and `grouped` come after the edited row, and are kept when it changes
              for needle, nth, inside in [ "scale", 1, 1; "baseValue", 2, 2; "grouped", 0, 2; "doubled", 0, 2 ] do
                  let at = position (s.Lsp.Text a) needle nth inside
                  let! actual = hoverText s a at
                  let! reference = expected s texts a at
                  Expect.isSome reference $"FCS has a tooltip for %s{needle}"
                  Expect.equal actual reference $"%s{needle} in a kept row equals FCS"
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
