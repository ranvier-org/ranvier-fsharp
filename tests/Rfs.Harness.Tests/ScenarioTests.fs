module Rfs.Harness.Tests.ScenarioTests

open System
open System.IO
open Expecto
open Rfs.Lsp
open Rfs.Harness

let private withCorpus (files: (string * string) list) (body: string -> 'a) =
    let root = Path.Combine(Path.GetTempPath(), "rfs-scenario-" + Guid.NewGuid().ToString("N"))

    try
        for (path, content) in files do
            let full = Path.Combine(root, "small", path)
            Directory.CreateDirectory(Path.GetDirectoryName full) |> ignore
            File.WriteAllText(full, content)

        body root
    finally
        if Directory.Exists root then
            Directory.Delete(root, true)

let private load (root: string) (json: string) =
    let path = Path.Combine(root, "scenario.json")
    File.WriteAllText(path, json)
    Scenario.load path root

let private scenarioJson (steps: string) =
    $$"""{ "name": "s", "corpus": "small", "project": "src/P.fsproj", "kind": "body", "steps": [ {{steps}} ] }"""

let private files =
    [ "src/P.fsproj", "<Project/>"
      "src/A.fs", "module A\r\nlet x = 1\r\nlet y = 22\r\n" ]

let private pos line character : Position = { Line = line; Character = character }

[<Tests>]
let tests =
    testList
        "Scenario"
        [ test "loads body scenario" {
              withCorpus files (fun root ->
                  let json =
                      scenarioJson
                          """ { "open": "src/A.fs" },
                              { "edit": { "file": "src/A.fs",
                                          "range": { "start": { "line": 1, "character": 8 }, "end": { "line": 1, "character": 9 } },
                                          "text": "2", "repeat": 20, "mode": "toggle" } },
                              { "save": "src/A.fs" },
                              { "hover": { "file": "src/A.fs", "position": { "line": 2, "character": 4 } } },
                              { "completion": { "file": "src/A.fs", "position": { "line": 2, "character": 9 } } },
                              { "definition": { "file": "src/A.fs", "position": { "line": 0, "character": 0 } } },
                              { "references": { "file": "src/A.fs", "position": { "line": 3, "character": 0 } } },
                              { "openMany": [ "src/A.fs" ] } """

                  let scenario =
                      match load root json with
                      | Ok s -> s
                      | Error e -> failtestf "expected Ok, got Error %s" e

                  Expect.equal scenario.Name "s" "name"
                  Expect.equal scenario.Corpus "small" "corpus"
                  Expect.equal scenario.Project "src/P.fsproj" "project"
                  Expect.equal scenario.Kind "body" "kind"

                  let expectedEdit: Edit =
                      { Range = { Start = pos 1 8; End = pos 1 9 }
                        Text = "2" }

                  Expect.equal
                      scenario.Steps
                      [ Step.Open "src/A.fs"
                        Step.Edit("src/A.fs", expectedEdit, 20, Toggle)
                        Step.Save "src/A.fs"
                        Step.Hover("src/A.fs", pos 2 4)
                        Step.Completion("src/A.fs", pos 2 9)
                        Step.Definition("src/A.fs", pos 0 0)
                        Step.References("src/A.fs", pos 3 0)
                        Step.OpenMany [ "src/A.fs" ] ]
                      "steps")
          }

          test "edit defaults to one applied repetition" {
              withCorpus files (fun root ->
                  let json =
                      scenarioJson
                          """ { "edit": { "file": "src/A.fs",
                                          "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 0 } },
                                          "text": "x" } } """

                  match load root json with
                  | Ok { Steps = [ Step.Edit(_, _, repeat, mode) ] } ->
                      Expect.equal (repeat, mode) (1, Apply) "defaults"
                  | other -> failtestf "unexpected %A" other)
          }

          test "rejects position past end of line" {
              withCorpus files (fun root ->
                  // Line 1 is "let x = 1" (9 chars) before its CRLF.
                  let past = scenarioJson """ { "hover": { "file": "src/A.fs", "position": { "line": 1, "character": 10 } } } """
                  let atEnd = scenarioJson """ { "hover": { "file": "src/A.fs", "position": { "line": 1, "character": 9 } } } """
                  Expect.isError (load root past) "column 10 is past the end of the line"
                  Expect.isOk (load root atEnd) "column 9 is the end of the line"
                  let pastLines = scenarioJson """ { "hover": { "file": "src/A.fs", "position": { "line": 4, "character": 0 } } } """
                  Expect.isError (load root pastLines) "file has lines 0..3")
          }

          test "rejects position past end of line inside an edit range" {
              withCorpus files (fun root ->
                  let json =
                      scenarioJson
                          """ { "edit": { "file": "src/A.fs",
                                          "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 99 } },
                                          "text": "" } } """

                  Expect.isError (load root json) "range end is past the line")
          }

          test "rejects missing file" {
              withCorpus files (fun root ->
                  match load root (scenarioJson """ { "open": "src/Nope.fs" } """) with
                  | Error message -> Expect.stringContains message "Nope.fs" "names the file"
                  | Ok _ -> failtest "expected Error")
          }

          test "rejects unknown step key and unknown kind" {
              withCorpus files (fun root ->
                  Expect.isError (load root (scenarioJson """ { "frobnicate": "src/A.fs" } """)) "unknown step"

                  let badKind =
                      """{ "name": "s", "corpus": "small", "project": "src/P.fsproj", "kind": "nope", "steps": [] }"""

                  Expect.isError (load root badKind) "unknown kind")
          }

          test "rejects malformed json" {
              withCorpus files (fun root -> Expect.isError (load root "{ not json") "malformed")
          }

          testList
              "wrong-typed json returns Error"
              [ for (name, steps, expected) in
                    [ "string line",
                      """ { "hover": { "file": "src/A.fs", "position": { "line": "3", "character": 0 } } } """,
                      "line"
                      "edit is a string", """ { "edit": "x" } """, "object"
                      "edit is a number", """ { "edit": 5 } """, "object"
                      "step is a string", """ "open" """, "one key" ] do
                    test name {
                        withCorpus files (fun root ->
                            match load root (scenarioJson steps) with
                            | Error message -> Expect.stringContains message expected "useful message"
                            | Ok _ -> failtest "expected Error")
                    } ] ]
