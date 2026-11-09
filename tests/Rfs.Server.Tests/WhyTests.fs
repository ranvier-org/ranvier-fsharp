module Rfs.Server.Tests.WhyTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests.PublishTests

/// The 0-based line and character of the `nth` (from 0) occurrence of `needle` in `text`, `inside` characters into it
let private position (text: string) (needle: string) (nth: int) (inside: int) =
    let mutable index = -1

    for _ in 0..nth do
        index <- text.IndexOf(needle, index + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual index 0 $"%s{needle} occurs %d{nth + 1} times"
    let before = text.Substring(0, index)
    before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1) + inside

let private whyParams (path: string) ((line, character): int * int) : JsonNode =
    let at = JsonObject()
    at["line"] <- JsonValue.Create line
    at["character"] <- JsonValue.Create character
    let document = JsonObject()
    document["uri"] <- JsonValue.Create(Paths.toUri path)
    let parameters = JsonObject()
    parameters["textDocument"] <- document
    parameters["position"] <- at
    parameters

let private why (s: Session) (path: string) (at: int * int) : Task<JsonNode> =
    task {
        let! (result: JsonNode), _ = s.Lsp.Request("rfs/why", whyParams path at) |> within
        return result
    }

/// Waits until the file is checked by declaration for its latest text
let private promoted (s: Session) (path: string) =
    task {
        do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
        let graph = (s.Server.Workspace.ProjectOf path).Value
        let! mode = graph.WhenPromoted(path, CancellationToken.None) |> within
        Expect.equal mode Rfs.Checker.FileMode.Declarations $"%s{Path.GetFileName path} is checked by declaration"
    }

/// A.fs open and checked by declaration
let private openA () =
    task {
        let! s = startSession () |> within
        do! openAll s [ a ]
        do! eventually "the open publish" (fun () -> hasVersion s a 0)
        do! promoted s a
        return s
    }

#if RANVIER_TRACE
/// `scale` given another return type, which `doubled` uses and `grouped` does not
let private afterScaleEdit () =
    task {
        let! s = openA ()
        do! s.Lsp.DidChange(a, 1, replace s a "let scale (x: int) : int = x * 2" "let scale (x: int) : int64 = int64 x * 2L")
        do! eventually "the edit's publish" (fun () -> hasVersion s a 1)
        do! promoted s a
        return s
    }
#endif

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Why"
        [
#if RANVIER_TRACE
          testTask "why names the changed dependency" {
              let! s = afterScaleEdit ()
              use _ = disposing s
              let! (answer: JsonNode) = why s a (position (s.Lsp.Text a) "let doubled" 0 5)
              Expect.isNotNull answer "a declaration answers"
              Expect.isTrue (answer["traced"].GetValue<bool>()) "the build is traced"
              Expect.stringContains (answer["declaration"].GetValue<string>()) "doubled" "the declaration is named"
              let chain = answer["chain"].GetValue<string>()
              // A node's path writes a label's backslashes doubled
              let escaped = a.Replace("\\", "\\\\")
              let label (key: string) = $"fingerprint %s{escaped} %s{key}"
              Expect.stringContains chain (label "Deps.A.scale") "the chain names the dependency that changed"
              Expect.isFalse (chain.Contains(label "Deps.A.baseValue")) "and not one that did not"
          }
#else
          testTask "untraced build says so" {
              let! s = openA ()
              use _ = disposing s
              let! (answer: JsonNode) = why s a (position (s.Lsp.Text a) "let doubled" 0 5)
              Expect.isFalse (answer["traced"].GetValue<bool>()) "the build is untraced"
              Expect.equal (answer["chain"].GetValue<string>()) "" "and has no chain"
          }
#endif
          testTask "a position in no declaration answers nothing" {
              let! s = openA ()
              use _ = disposing s
              // The blank line between two declarations
              let line, _ = position (s.Lsp.Text a) "let baseValue" 0 0
              let! (answer: JsonNode) = why s a (line + 1, 0)
              Expect.isNull answer "no declaration, no answer"
          }
          testTask "a signature file answers nothing" {
              let! s = startSession () |> within
              use _ = disposing s
              let fsi = file "E.fsi"
              do! openAll s [ fsi ]
              let! (answer: JsonNode) = why s fsi (position (s.Lsp.Text fsi) "val" 0 0)
              Expect.isNull answer "a signature file has no declaration nodes"
          }
          testTask "a file checked whole answers nothing" {
              let! s = startSession () |> within
              use _ = disposing s
              // Never opened, so never promoted
              let! (answer: JsonNode) = why s c (position (File.ReadAllText c) "let total" 0 5)
              Expect.isNull answer "a file checked whole has no declaration nodes"
          } ]
