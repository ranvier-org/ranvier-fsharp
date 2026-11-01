module Rfs.Checkpoint.Tests.CheckpointTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Rfs.Checkpoint
open Rfs.Harness
open Rfs.Lsp

let private fixturesDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures")
        if Directory.Exists(Path.Combine(candidate, "Deps")) then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private freshDir (prefix: string) =
    let dir = Path.Combine(AppContext.BaseDirectory, prefix + "-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    dir

/// Where `needle` starts in `text`, as an LSP position; positions come from the fixture's current text so the test
/// survives edits to it elsewhere.
let private positionOf (text: string) (needle: string) : Position =
    let lines = text.Split('\n')

    match lines |> Array.tryFindIndex (fun l -> l.Contains needle) with
    | Some line ->
        { Line = line
          Character = lines[line].IndexOf needle }
    | None -> failtest $"fixture text has no '%s{needle}'"

let private rangeJson (start: Position) (length: int) =
    let pos (p: Position) =
        JsonObject([ Collections.Generic.KeyValuePair("line", JsonValue.Create p.Line :> JsonNode)
                     Collections.Generic.KeyValuePair("character", JsonValue.Create p.Character :> JsonNode) ])

    let o = JsonObject()
    o["start"] <- pos start
    o["end"] <- pos { start with Character = start.Character + length }
    o

let private toggle (file: string) (start: Position) (length: int) (text: string) (repeat: int) : JsonNode =
    let edit = JsonObject()
    edit["file"] <- JsonValue.Create file
    edit["range"] <- rangeJson start length
    edit["text"] <- JsonValue.Create text
    edit["repeat"] <- JsonValue.Create repeat
    edit["mode"] <- JsonValue.Create "toggle"
    let step = JsonObject()
    step["edit"] <- edit
    step

/// Five edits of Deps/A.fs: a literal changed and back, then a return type made wrong, right and wrong again.
let private writeDepsScenario (corpus: string) (dir: string) =
    let a = File.ReadAllText(Path.Combine(fixturesDir, "Deps", "A.fs"))
    let literal = positionOf a "= 10"
    let returnType = positionOf a ": int = x * 2"
    let o = JsonObject()
    o["name"] <- JsonValue.Create "deps-five"
    o["corpus"] <- JsonValue.Create corpus
    o["project"] <- JsonValue.Create "Deps.fsproj"
    o["kind"] <- JsonValue.Create "body"
    let opener = JsonObject()
    opener["open"] <- JsonValue.Create "A.fs"

    o["steps"] <-
        JsonArray(
            [| opener :> JsonNode
               toggle "A.fs" { literal with Character = literal.Character + 2 } 2 "11" 2
               toggle "A.fs" { returnType with Character = returnType.Character + 2 } 3 "int64" 3 |]
        )

    let path = Path.Combine(dir, "deps-five.json")
    File.WriteAllText(path, o.ToJsonString())
    path

let private readResults (dir: string) =
    [ for f in Directory.GetFiles(dir, "*.json") -> Report.resultOfJson (File.ReadAllText f) ]

/// Answers every check with the same made-up error, which no full check of Deps reports.
let private wrongEngine: EngineFactory =
    { Name = "wrong"
      Create =
        fun spec ->
            let bogus: Diagnostic =
                { File = List.head spec.SourceFiles
                  Range =
                    { Start = { Line = 0; Character = 0 }
                      End = { Line = 0; Character = 1 } }
                  Severity = 1
                  Code = "FS0001"
                  Message = "not from the compiler"
                  Source = "F# Compiler" }

            let result =
                Task.FromResult
                    { Diagnostics = [ bogus ]
                      DeclsChecked = None }

            Task.FromResult
                { new IEngine with
                    member _.Check() = result
                    member _.SetText(_, _) = result
                  interface IDisposable with
                      member _.Dispose() = () } }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Checkpoint"
        [ testTask "checkpoint on Deps" {
              let work = freshDir "deps"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code = Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out |]

              let results = readResults out
              let failures =
                  [ for r in results do
                        for s in r.Steps do
                            match s.Error, s.OracleDiff with
                            | None, Some [] -> ()
                            | e, d -> $"%s{r.Server} %s{s.Step}: %A{e} %A{d}" ]

              Expect.isEmpty failures "every step matches the oracle"
              Expect.equal code 0 "exit code"
              Expect.equal (results |> List.map _.Server |> List.sort) [ "decl"; "file-slim" ] "one result per engine"

              for r in results do
                  Expect.equal r.Scenario "deps-five" "scenario name"
                  Expect.equal r.Steps.Length 5 $"%s{r.Server}: one step per edit"
                  Expect.isGreaterThan r.ColdMs 0. $"%s{r.Server}: cold check timed"
                  Expect.isGreaterThan r.AllocatedMb 0. $"%s{r.Server}: allocations recorded"
                  Expect.isGreaterThan r.PeakWorkingSetMb 0. $"%s{r.Server}: peak working set recorded"
                  Expect.isTrue (File.Exists(Path.Combine(out, Report.fileName r))) "named like the harness's files"

              let decl = results |> List.find (fun r -> r.Server = "decl")
              let slim = results |> List.find (fun r -> r.Server = "file-slim")
              Expect.isTrue (decl.Steps |> List.forall (fun s -> s.DeclsChecked |> Option.exists (fun n -> n > 0))) "decl counts its checks"
              Expect.isTrue (slim.Steps |> List.forall (fun s -> s.DeclsChecked.IsNone)) "file-slim has no declaration count"
          }

          testTask "an oracle mismatch is a step error and fails the run" {
              let work = freshDir "wrong"
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code = Checkpoint.execute [ wrongEngine ] [| "--scenario"; scenario; "--corpus"; fixturesDir; "--out"; out |]

              Expect.notEqual code 0 "exit code"
              let result = readResults out |> List.exactlyOne
              Expect.equal result.Server "wrong" "server name"
              Expect.equal result.Steps.Length 5 "every edit recorded"

              for s in result.Steps do
                  Expect.isSome s.Error $"%s{s.Step}: error"
                  Expect.stringContains s.Error.Value "oracle mismatch" $"%s{s.Step}: why"
                  Expect.isNonEmpty (defaultArg s.OracleDiff []) $"%s{s.Step}: diff recorded"
          }

          testTask "refuses to measure a corpus that does not check clean" {
              let work = freshDir "broken"
              let corpus = Path.Combine(work, "corpus")
              let deps = Path.Combine(corpus, "Deps")
              Directory.CreateDirectory deps |> ignore

              for f in Directory.GetFiles(Path.Combine(fixturesDir, "Deps")) do
                  File.Copy(f, Path.Combine(deps, Path.GetFileName f))

              let a = Path.Combine(deps, "A.fs")
              File.WriteAllText(a, File.ReadAllText(a).Replace("let baseValue = 10", "let baseValue : string = 10"))
              let out = Path.Combine(work, "out")
              let scenario = writeDepsScenario "Deps" work

              let! code = Checkpoint.execute Engines.all [| "--scenario"; scenario; "--corpus"; corpus; "--out"; out |]

              Expect.equal code 3 "exit code for a broken corpus"
              Expect.isFalse (Directory.Exists out && Directory.GetFiles(out).Length > 0) "nothing measured"
          } ]
