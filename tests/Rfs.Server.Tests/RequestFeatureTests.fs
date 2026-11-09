module Rfs.Server.Tests.RequestFeatureTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests.PublishTests

let private fixturesRoot = Path.GetDirectoryName depsDir
let private repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)

/// A copy of the Deps fixture without obj and bin, in a temp root of its own
let private copyDeps () : string =
    let root = Path.Combine(Path.GetTempPath(), "rfs-req-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let target = Path.Combine(root, "Deps")
    Directory.CreateDirectory target |> ignore
    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(root, "global.json"))

    for f in Directory.GetFiles depsDir do
        File.Copy(f, Path.Combine(target, Path.GetFileName f))

    target

/// The 0-based line and character of the `nth` (from 0) occurrence of `needle` in `text`, `inside` characters into it
let private position (text: string) (needle: string) (nth: int) (inside: int) =
    let mutable index = -1

    for _ in 0..nth do
        index <- text.IndexOf(needle, index + 1, StringComparison.Ordinal)

    let before = text.Substring(0, index)
    let line = before.Split('\n').Length - 1
    line, index - (before.LastIndexOf '\n' + 1) + inside

let private features = [ "hover"; "definition"; "completion"; "resolve"; "why" ]

/// Where each feature asks in D.fs: a use of `start`, after `r.`, and in `fill`
let private at (feature: string) (text: string) =
    match feature with
    | "completion"
    | "resolve" -> position text "r.Value <-" 0 2
    | "why" -> position text "let fill" 0 5
    | _ -> position text "start" 1 1

let private resolveItem (path: string) (line: int, character: int) =
    { CompletionItem.Create "Value" with Data = Some(Completion.dataOf path line character) }

/// The feature's request over the connection, as the client sends it
let private overLsp (s: Session) (feature: string) (path: string) (line: int, character: int) : Task<JsonNode> =
    let parameters =
        if feature = "resolve" then
            JsonNode.Parse(
                $"""{{"label":"Value","data":{{"path":%s{JsonValue.Create(path).ToJsonString()},"line":%d{line},"character":%d{character}}}}}"""
            )
        else
            JsonNode.Parse(
                $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri path).ToJsonString()}}},"position":{{"line":%d{line},"character":%d{character}}}}}"""
            )

    let method =
        match feature with
        | "resolve" -> "completionItem/resolve"
        | "why" -> "rfs/why"
        | f -> "textDocument/" + f

    task {
        let! result, _ = s.Lsp.Request(method, parameters) |> within
        return result
    }

/// The feature itself, on the graph that holds the file
let private direct (s: Session) (feature: string) (path: string) (line: int, character: int) (ct: CancellationToken) : Task<obj> =
    let graph = (s.Server.Workspace.ProjectOf path).Value

    task {
        match feature with
        | "hover" ->
            let! r = Hover.at graph path line character ct
            return box r
        | "definition" ->
            let! r = Definition.at s.Server.Workspace.OwnersOf graph path line character ct
            return box r
        | "completion" ->
            let! r = Completion.at graph path line character ct
            return box r
        | "resolve" ->
            let! r = Completion.resolve graph (resolveItem path (line, character)) path line character ct
            return box r
        | _ ->
            let! r = Why.at graph path line character ct
            return box r
    }

/// Holds every check of D.fs set up from its dependencies until `gate` is set, counting them
let private holdD (s: Session) (d: string) (gate: ManualResetEventSlim) =
    let held = ref 0

    (s.Server.Workspace.ProjectOf d).Value.OnScopedCheck(fun path ->
        if path.EndsWith "D.fs" then
            Interlocked.Increment held |> ignore
            gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)

    held

/// An edit after which a hover on `start` answers otherwise: a string, not an int
let private edited (text: string) =
    text.Replace("42", "43").Replace("baseValue + 1", "\"one\"")

[<Tests>]
let tests =
    testSequenced
    <| testList
        "RequestFeature"
        [ for feature in features do
              testTask $"%s{feature}: a close during the request answers for the text the close leaves" {
                  let! s = startSession () |> within
                  use _ = disposing s
                  let d = file "D.fs"
                  let disk = File.ReadAllText d
                  let gate = new ManualResetEventSlim(false)
                  let held = holdD s d gate

                  try
                      s.Server.Workspace.Open(d, edited disk, 1) |> ignore
                      do! eventually "D's check held" (fun () -> held.Value >= 1)
                      let pending = overLsp s feature d (at feature disk)
                      do! Task.Delay 300
                      Expect.isFalse pending.IsCompleted "the request waits for D"
                      s.Server.Workspace.Close d |> ignore
                      gate.Set()
                      let! (answer: JsonNode) = pending

                      if feature <> "why" then
                          Expect.isNotNull answer "an answer, for the text on disk"

                      if feature = "hover" then
                          let shown = answer["contents"].["value"].GetValue<string>()
                          Expect.stringContains shown "val start: int" "the disk text's answer"
                  finally
                      gate.Set()
              }

              testTask $"%s{feature}: a cancel ends the request's wait, and a repeat answers from the work under way" {
                  let! s = startSession () |> within
                  use _ = disposing s
                  let d = file "D.fs"
                  let disk = File.ReadAllText d
                  let gate = new ManualResetEventSlim(false)
                  let held = holdD s d gate

                  try
                      s.Server.Workspace.Open(d, edited disk, 1) |> ignore
                      do! eventually "D's check held" (fun () -> held.Value >= 1)
                      let cts = new CancellationTokenSource()
                      let first = direct s feature d (at feature disk) cts.Token
                      do! Task.Delay 300
                      Expect.isFalse first.IsCompleted "the request waits for D"
                      cts.Cancel()

                      let! cancelled =
                          task {
                              try
                                  let! _ = first |> within
                                  return false
                              with
                              | :? OperationCanceledException -> return true
                              | :? AggregateException as e when (e.InnerException :? OperationCanceledException) -> return true
                          }

                      Expect.isTrue cancelled "the cancelled request ends, cancelled"
                      let checks = held.Value
                      let second = direct s feature d (at feature disk) CancellationToken.None
                      do! Task.Delay 300
                      Expect.isFalse second.IsCompleted "the repeat waits for the same check"
                      Expect.equal held.Value checks "no check of D started again"
                      gate.Set()
                      let! answer = second |> within

                      if feature <> "why" then
                          Expect.isNotNull answer "the repeat answers"
                  finally
                      gate.Set()
              }

              testTask $"%s{feature}: a file leaving the project during the wait answers as for a file no project holds" {
                  let dir = copyDeps ()
                  let project = Path.Combine(dir, "Deps.fsproj")
                  let d = Path.Combine(dir, "D.fs")
                  let! s = startSessionWith dir [ project ] true ignore |> within
                  use _ = disposing s
                  let disk = File.ReadAllText d
                  let gate = new ManualResetEventSlim(false)
                  let held = holdD s d gate

                  try
                      s.Server.Workspace.Open(d, edited disk, 1) |> ignore
                      do! eventually "D's check held" (fun () -> held.Value >= 1)
                      let pending = overLsp s feature d (at feature disk)
                      do! Task.Delay 300
                      Expect.isFalse pending.IsCompleted "the request waits for D"

                      File.WriteAllText(
                          project,
                          (File.ReadAllText project).Replace("    <Compile Include=\"D.fs\" />\r\n", "").Replace("    <Compile Include=\"D.fs\" />\n", "")
                      )

                      do! s.Server.Workspace.FilesChanged([ project, false ]).WaitAsync(TimeSpan.FromSeconds 120.)
                      let! (answer: JsonNode) = pending

                      if feature = "resolve" then
                          Expect.equal (answer["label"].GetValue<string>()) "Value" "the item as it was"
                      else
                          Expect.isNull answer "nothing"

                      let! (later: JsonNode) = overLsp s feature d (at feature disk)

                      if feature <> "resolve" then
                          Expect.isNull later "and nothing after"
                  finally
                      gate.Set()
              } 

              testTask $"%s{feature}: a project that fails to load during the wait answers as for a file no project holds" {
                  let dir = copyDeps ()
                  let project = Path.Combine(dir, "Deps.fsproj")
                  let d = Path.Combine(dir, "D.fs")
                  let! s = startSessionWith dir [ project ] true ignore |> within
                  use _ = disposing s
                  let disk = File.ReadAllText d
                  let gate = new ManualResetEventSlim(false)
                  let held = holdD s d gate

                  try
                      s.Server.Workspace.Open(d, edited disk, 1) |> ignore
                      do! eventually "D's check held" (fun () -> held.Value >= 1)
                      let pending = overLsp s feature d (at feature disk)
                      do! Task.Delay 300
                      Expect.isFalse pending.IsCompleted "the request waits for D"
                      File.WriteAllText(project, "<Project Sdk=\"Microsoft.NET.Sdk\"><PropertyGroup>")
                      do! s.Server.Workspace.FilesChanged([ project, false ]).WaitAsync(TimeSpan.FromSeconds 120.)
                      let! (answer: JsonNode) = pending

                      if feature = "resolve" then
                          Expect.equal (answer["label"].GetValue<string>()) "Value" "the item as it was"
                      else
                          Expect.isNull answer "nothing"
                  finally
                      gate.Set()
              }

          testTask "a read refused for another parse's #line directives is tried again, and answers once they are back" {
              let! s = startSession () |> within
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf a).Value
              let stale = ref 1
              let refused = ref 0

              let read =
                  graph.ReadFile(
                      a,
                      (fun v ->
                          if Volatile.Read &stale.contents = 1 then
                              Interlocked.Increment refused |> ignore
                              raise (FSharp.Compiler.Declarations.SupersededLineDirectivesException "stale directives")

                          v.Text),
                      CancellationToken.None
                  )

              do! eventually "a refused read" (fun () -> refused.Value >= 1)
              // Nothing the read depends on changes when the directives come back
              Volatile.Write(&stale.contents, 0)
              let! text = read.WaitAsync(TimeSpan.FromSeconds 20.)
              Expect.equal text (File.ReadAllText a) "the read answers"
              Expect.isGreaterThan refused.Value 1 "after trying again"
          }

          testTask "a read refused for #line directives for as long as a check waits fails with it, which answers nothing" {
              let! s = startSession () |> within
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf a).Value

              let read =
                  graph.ReadFile(a, (fun _ -> raise (FSharp.Compiler.Declarations.SupersededLineDirectivesException "stale directives")), CancellationToken.None)

              let! (failed: exn option) =
                  task {
                      try
                          let! (_: unit) = read.WaitAsync(TimeSpan.FromSeconds 30.)
                          return None
                      with e ->
                          return Some e
                  }

              Expect.isTrue
                  (match failed with
                   | Some e -> e :? FSharp.Compiler.Declarations.SupersededLineDirectivesException
                   | None -> false)
                  $"the request fails with it: %A{failed}"
          }

          testTask "a definition that lands in a signature file answers when another project on the host faults" {
              let! s = startSession () |> within
              use _ = disposing s
              let efs = file "E.fs"
              let efsi = file "E.fsi"
              let graph = (s.Server.Workspace.ProjectOf efs).Value

              let! (key: string) =
                  graph.ReadFile(
                      efs,
                      (fun v ->
                          let u =
                              v.Result.CheckResults.GetAllUsesOfAllSymbolsInFile()
                              |> Seq.find (fun u -> u.IsFromDefinition && u.Symbol.DisplayName = "compute")

                          (FSharp.Compiler.Declarations.DeclarationChecker.SymbolKey u.Symbol).Value),
                      CancellationToken.None
                  )
                  |> within

              let! spec = Rfs.Projects.ProjectSpec.load depsProject

              let! (other: Rfs.Checker.ProjectGraph) =
                  Rfs.Checker.ProjectGraph.Create(graph.Host, { Rfs.Projects.ProjectSpec.sequential spec with ProjectFile = Path.Combine(depsDir, "Other.fsproj") })
                  |> within

              let gate = new ManualResetEventSlim(false)
              let held = ref 0

              graph.OnScopedCheck(fun path ->
                  if path.EndsWith "E.fsi" then
                      Interlocked.Increment held |> ignore
                      gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)

              try
                  graph.SetText(efsi, File.ReadAllText efsi + "\n") |> ignore
                  do! eventually "E.fsi's check held" (fun () -> held.Value >= 1)
                  let pending = graph.CurrentLocation(a, efsi, key, CancellationToken.None)
                  do! Task.Delay 300
                  Expect.isFalse pending.IsCompleted "it waits for E.fsi"
                  // Fails on the graph thread, which faults every wait parked on the host
                  other.FailRestart(InvalidOperationException "the other project's restart failed")
                  do! Task.Delay 300
                  gate.Set()
                  let! (found: FSharp.Compiler.Text.range option) = pending |> within
                  Expect.isSome found "the signature's location"
                  Expect.stringContains found.Value.FileName "E.fsi" "in E.fsi"
              finally
                  gate.Set()
                  other.Dispose()
                  graph.Host.TakePostErrors() |> ignore
          }
        ]
