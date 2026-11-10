module Rfs.Server.Tests.PublishFixTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests.PublishTests

let private fixturesRoot = Path.GetDirectoryName depsDir
let private repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)

/// A copy of a fixture directory without obj and bin, in a temp root of its own, so it restores and loads on its own
let private copyFixtureDir (name: string) : string =
    let root = Path.Combine(Path.GetTempPath(), "rfs-pub-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let target = Path.Combine(root, name)
    Directory.CreateDirectory target |> ignore
    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(root, "global.json"))

    for f in Directory.GetFiles(Path.Combine(fixturesRoot, name)) do
        File.Copy(f, Path.Combine(target, Path.GetFileName f))

    target

/// Two projects that share S.fs as a linked file; X.fs is only in the second. The root, both projects and S.fs.
let private sharedFixture () : string * string * string * string =
    let root = Path.Combine(Path.GetTempPath(), "rfs-pub-" + Guid.NewGuid().ToString("N").Substring(0, 8))

    for d in [ "shared"; "p1"; "p2" ] do
        Directory.CreateDirectory(Path.Combine(root, d)) |> ignore

    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(root, "global.json"))
    File.Copy(Path.Combine(depsDir, "Directory.Build.props"), Path.Combine(root, "Directory.Build.props"))
    File.Copy(Path.Combine(depsDir, "Directory.Packages.props"), Path.Combine(root, "Directory.Packages.props"))
    File.WriteAllText(Path.Combine(root, "shared", "S.fs"), "module S\n\nlet value = 1\n")
    File.WriteAllText(Path.Combine(root, "p2", "X.fs"), "module X\n\nlet y : int = S.value\n")

    let project (compile: string list) =
        "<Project Sdk=\"Microsoft.NET.Sdk\">\n  <PropertyGroup>\n    <TargetFramework>net10.0</TargetFramework>\n  </PropertyGroup>\n  <ItemGroup>\n"
        + String.Join("", compile |> List.map (fun c -> $"    <Compile Include=\"%s{c}\" />\n"))
        + "  </ItemGroup>\n</Project>\n"

    File.WriteAllText(Path.Combine(root, "p1", "P1.fsproj"), project [ @"..\shared\S.fs" ])
    File.WriteAllText(Path.Combine(root, "p2", "P2.fsproj"), project [ @"..\shared\S.fs"; "X.fs" ])
    root, Path.Combine(root, "p1", "P1.fsproj"), Path.Combine(root, "p2", "P2.fsproj"), Path.GetFullPath(Path.Combine(root, "shared", "S.fs"))

/// The edit that appends `text` to the client's text of `path`
let private atEnd (s: Session) (path: string) (text: string) : Edit =
    let lines = (s.Lsp.Text path).Split('\n')
    let position = { Line = lines.Length - 1; Character = lines[lines.Length - 1].Length }
    { Range = { Start = position; End = position }; Text = text }

let private lastOf (s: Session) (path: string) =
    let _, _, d = Array.last (publishesOf s path)
    d

let private equalsOracle (expected: Diagnostic list) (actual: Diagnostic list) =
    Diagnostics.compare expected actual |> List.isEmpty

[<Tests>]
let fixTests =
    testSequenced
    <| testList
        "Publish fixes"
        [ testTask "500 back-to-back edits keep the editor's text" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! s.Lsp.DidChange(a, 1, atEnd s a "\n// ")
              let stop = ref false
              let started = ref 0

              // The thread pool is busy, as it is when a real editor types, and its global queue is never empty, so no
              // thread takes work from another's local queue. Each burner goes to the back of that queue between spins:
              // one that keeps its thread would hold up the test's own polling too, as the pool adds threads slowly.
              let burners: Task list =
                  [ for _ in 1 .. Environment.ProcessorCount * 2 ->
                        Task.Run<unit>(fun () ->
                            task {
                                Interlocked.Increment started |> ignore

                                while not stop.Value do
                                    Thread.SpinWait 2000
                                    do! Task.Yield()
                            }) ]

              try
                  do! eventually "the burners" (fun () -> started.Value = burners.Length)

                  for i in 2..501 do
                      do! s.Lsp.DidChange(a, i, atEnd s a (string (char (97 + i % 26))))

                  // Tells a server that is behind from one whose text went wrong
                  let state = ref "no document"

                  try
                      do!
                          eventually "the server's text" (fun () ->
                              match s.Server.Documents.TryFind a with
                              | Some d ->
                                  let client = s.Lsp.Text a
                                  state.Value <- $"server at version %d{d.LspVersion}, %d{d.Text.Length} chars; client %d{client.Length} chars"
                                  d.LspVersion = 501 && d.Text = client
                              | None -> false)
                  with ex ->
                      failtest $"%s{ex.Message}: %s{state.Value}"
              finally
                  stop.Value <- true

              do! Task.WhenAll burners
              do! eventually "the last publish" (fun () -> hasVersion s a 501)
          }

          testTask "a document keeps one row and one last list, not one per edit" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)

              for v in 1..5 do
                  do! s.Lsp.DidChange(a, v, atEnd s a "\n// ")

              do! eventually "the last edit's publish" (fun () -> hasVersion s a 5)
              let! retained = s.Server.Publisher.Retained()
              Expect.equal retained 2 "one row, one last list"
          }

          testTask "a foreign publish does not suppress the file's own" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let hold = new ManualResetEventSlim false
              let entered = ref false
              let armed = ref true

              // Holds version 1's parse, so its own list is decided after the foreign one is queued
              (s.Server.Workspace.ProjectOf a).Value.OnFlight(fun kind path _ ->
                  if armed.Value && kind = "parse" && String.Equals(path, a, StringComparison.OrdinalIgnoreCase) then
                      armed.Value <- false
                      entered.Value <- true
                      hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)

              do! s.Lsp.DidChange(a, 1, replace s a "baseValue = 10" "baseValue = \"s\"")
              do! eventually "version 1's parse" (fun () -> entered.Value)
              let counts = s.Counts.Count

              let position = { Line = 0; Character = 0 }

              let foreign =
                  { File = a
                    Range = { Start = position; End = position }
                    Severity = 1
                    Code = "FS9999"
                    Message = "reported by another file's check"
                    Source = "F# Compiler" }

              // Version 1 is the document's current one while its own result is held
              do! s.Server.Publisher.PublishForeign(a, [ foreign ])
              do! eventually "the foreign list" (fun () -> publishesOf s a |> Array.exists (fun (_, v, d) -> v = Some 1 && d.Length = 1 && d.Head.Code = "FS9999"))
              hold.Set()
              let! expected = oracleFor s [ a ] a
              Expect.isFalse (List.isEmpty expected) "the edit breaks A"
              do! eventually "A's own diagnostics" (fun () -> equalsOracle expected (lastOf s a))
              do! eventually "the notification of the own publish" (fun () -> s.Counts.Count > counts)
          }

          testTask "close takes the unsaved text out and the dependent follows" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a; c ]
              do! eventually "the open publishes" (fun () -> hasVersion s a 0 && hasVersion s c 0)
              do! s.Lsp.DidChange(a, 1, replace s a "(s: Shape) : int =" "(s: Shape) : unit =")
              let! broken = oracleFor s [ a; c ] c
              Expect.isFalse (List.isEmpty broken) "the unsaved edit breaks C"
              do! eventually "C broken" (fun () -> equalsOracle broken (lastOf s c))
              let before = (publishesOf s c).Length
              do! s.Lsp.Notify("textDocument/didClose", textDocument [ "uri", JsonValue.Create(Paths.toUri a) ])
              let! onDisk = oracleFor s [] c
              do! eventually "a publish for C" (fun () -> (publishesOf s c).Length > before)
              do! eventually "C equals the oracle with A as on disk" (fun () -> equalsOracle onDisk (lastOf s c))
              Expect.isEmpty (lastOf s a) "A is cleared"
          }

          testTask "an edit to a shared file reaches the open files of every owner" {
              let root, p1, p2, shared = sharedFixture ()
              let x = Path.Combine(Path.GetDirectoryName p2, "X.fs")
              let! s = startSessionWith root [ p1; p2 ] true ignore |> within
              use _ = disposing s
              do! openAll s [ shared; x ]
              do! eventually "the open publishes" (fun () -> hasVersion s shared 0 && hasVersion s x 0)
              Expect.equal (s.Server.Workspace.OwnersOf shared).Length 2 "two owners"
              do! s.Lsp.DidChange(shared, 1, replace s shared "value = 1" "value = \"s\"")
              let! all = s.Oracle.Check p2 (Map [ shared, s.Lsp.Text shared ])
              let expected = all |> List.filter (fun d -> d.File.EndsWith "X.fs")
              Expect.isFalse (List.isEmpty expected) "the edit breaks X"
              do! eventually "X equals the oracle" (fun () -> equalsOracle expected (lastOf s x))
          }

          testTask "a signature conformance error is attributed as the oracle does, and clears" {
              let dir = copyFixtureDir "SigConform"
              let project = Path.Combine(dir, "SigConform.fsproj")
              let fs = Path.Combine(dir, "A.fs")
              let fsi = Path.Combine(dir, "A.fsi")
              let! s = startSessionWith dir [ project ] true ignore |> within
              use _ = disposing s
              do! openAll s [ fs; fsi ]
              do! eventually "the open publishes" (fun () -> hasVersion s fs 0 && hasVersion s fsi 0)
              do! s.Lsp.DidChange(fs, 1, replace s fs "let f (x: int) y = x + y" "let f (x: int) (y: int64) = int64 x + y")
              let! forFs = oracleFor s [ fs ] fs
              let! forFsi = oracleFor s [ fs ] fsi
              Expect.isFalse (List.isEmpty (forFs @ forFsi)) "the constraint mismatch is reported somewhere"
              do! eventually "both files equal the oracle" (fun () -> equalsOracle forFs (lastOf s fs) && equalsOracle forFsi (lastOf s fsi))
              do! s.Lsp.DidChange(fs, 2, replace s fs "let f (x: int) (y: int64) = int64 x + y" "let f (x: int) y = x + y")
              do! eventually "both files clear" (fun () -> hasVersion s fs 2 && (lastOf s fs).IsEmpty && (lastOf s fsi).IsEmpty)
          }

          testTask "a reload keeps no state for the replaced graph" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! s.Server.Publisher.Flushed()
              let! before = s.Server.Publisher.RetainedGraphs()
              let counts = s.Counts.Count
              let published = (publishesOf s a).Length
              do! s.Server.Workspace.Reload depsProject |> within
              do! eventually "the publish after the reload" (fun () -> (publishesOf s a).Length > published && s.Counts.Count > counts)
              do! s.Server.Publisher.Flushed()
              let! after = s.Server.Publisher.RetainedGraphs()
              Expect.equal after before "state is kept for the live graph only"
          }

          testTask "rapid edits: a list held in the outbox is replaced by a newer version's" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let holds = ConcurrentDictionary<int, TaskCompletionSource>()
              let entered = ConcurrentDictionary<int, bool>()

              for v in [ 1; 2 ] do
                  holds[v] <- TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)

              s.Server.Publisher.BeforePublish(fun lsp ->
                  entered[lsp] <- true

                  match holds.TryGetValue lsp with
                  | true, hold -> hold.Task :> Task
                  | _ -> Task.CompletedTask)

              do! s.Lsp.DidChange(a, 1, replace s a "baseValue = 10" "baseValue = \"s\"")
              do! eventually "version 1 settled" (fun () -> entered.ContainsKey 1)
              do! s.Lsp.DidChange(a, 2, replace s a "baseValue = \"s\"" "baseValue = 11")
              // Version 2's list is queued behind the held one: its effect ran in the pump that settled it
              do! eventually "the server at version 2" (fun () -> (s.Server.Documents.TryFind a |> Option.map _.LspVersion) = Some 2)
              do! s.Server.Publisher.Barrier()
              let! _ = (s.Server.Workspace.ProjectOf a).Value.Diagnostics a |> within
              do! s.Server.Publisher.Barrier()
              holds[1].SetResult()
              do! eventually "version 2 at the hook" (fun () -> entered.ContainsKey 2)
              holds[2].SetResult()
              do! eventually "version 2 published" (fun () -> hasVersion s a 2)
              Expect.isFalse (hasVersion s a 1) "the older result is dropped"
          }

          testTask "typing during promotion" {
              let hold = new ManualResetEventSlim false
              let building = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)

              // Promotion is held inside its build, and the edits are sent once it is there
              let prepare (server: RanvierServer) =
                  server.Workspace.OnGraphCreated(fun graph ->
                      graph.OnBeginFile(fun path ->
                          if String.Equals(path, a, StringComparison.OrdinalIgnoreCase) then
                              building.TrySetResult() |> ignore
                              hold.Wait(TimeSpan.FromSeconds 90.) |> ignore))

              let! s = startSessionWith depsDir [ depsProject ] true prepare |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! building.Task.WaitAsync(TimeSpan.FromSeconds 120.)
              let graph = (s.Server.Workspace.ProjectOf a).Value
              let promoted = graph.WhenPromoted(a, CancellationToken.None)

              for i in 1..5 do
                  let edit =
                      if i % 2 = 1 then
                          replace s a "baseValue = 10" "baseValue = \"s\""
                      else
                          replace s a "baseValue = \"s\"" "baseValue = 10"

                  do! s.Lsp.DidChange(a, i, edit)

              do! eventually "the publish of version 5" (fun () -> hasVersion s a 5)
              Expect.isFalse promoted.IsCompleted "the edits came before promotion completed"
              hold.Set()
              let! _ = promoted |> within
              // Promotion's last pump queued whatever it changed
              do! s.Server.Publisher.Flushed()
              let versions = publishesOf s a |> Array.choose (fun (_, v, _) -> v)
              Expect.equal versions (Array.sort versions) "versions only rise"
              Expect.equal (Array.last versions) 5 "the last publish is the last edit"
              do! expectEqualOracle s [ a ] a 5
          }

          testTask "a document opened before the load is published and promoted when the project loads" {
              let! s = startSessionWith depsDir [ depsProject ] false ignore |> within
              use _ = disposing s
              let broken = (File.ReadAllText a).Replace("baseValue = 10", "baseValue = \"s\"")
              do! s.Lsp.DidOpen(a, broken)
              do! eventually "the empty publish" (fun () -> hasVersion s a 0)
              Expect.isEmpty (lastOf s a) "no project owns it yet"
              let before = (publishesOf s a).Length
              do! s.Load()
              do! eventually "the publish after the load" (fun () -> (publishesOf s a).Length > before)
              let! expected = oracleFor s [ a ] a
              Expect.isFalse (List.isEmpty expected) "the open text is broken"
              do! eventually "the oracle's diagnostics" (fun () -> equalsOracle expected (lastOf s a))
              let! mode = (s.Server.Workspace.ProjectOf a).Value.WhenPromoted(a, CancellationToken.None) |> within
              Expect.equal mode Rfs.Checker.FileMode.Declarations "promoted"
          }

          testTask "a file a reload drops gets an empty publish" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let cFile = Path.Combine(dir, "C.fs")
              let! s = startSessionWith dir [ project ] true ignore |> within
              use _ = disposing s
              do! s.Lsp.DidOpen(cFile, File.ReadAllText cFile + "\nlet bad : int = \"s\"\n")
              do! eventually "an error in C" (fun () -> hasVersion s cFile 0 && not (lastOf s cFile).IsEmpty)

              File.WriteAllText(
                  project,
                  (File.ReadAllText project).Replace("    <Compile Include=\"C.fs\" />\r\n", "").Replace("    <Compile Include=\"C.fs\" />\n", "")
              )

              do! s.Server.Workspace.Reload project |> within
              do! eventually "the empty publish" (fun () -> (lastOf s cFile).IsEmpty)
          }
        ]

/// A copy of Deps whose C.fs, after a #line directive naming A.fs, reports an error that lands in A.fs: a file's
/// diagnostics in another file. A and C open, then C closed when `close`. The session, A, C, the project and the
/// foreign message.
let private foreignSession (close: bool) : Task<Session * string * string * string * string> =
    task {
        let dir = copyFixtureDir "Deps"
        let project = Path.Combine(dir, "Deps.fsproj")
        let aFile = Path.Combine(dir, "A.fs")
        let cFile = Path.Combine(dir, "C.fs")
        File.AppendAllText(cFile, $"\n# 1 \"%s{aFile}\"\nmodule Foreign =\n    let bad : string = Deps.A.baseValue\n")
        let! s = startSessionWith dir [ project ] true ignore |> within
        do! openAll s [ aFile; cFile ]
        do! eventually "the open publishes" (fun () -> hasVersion s aFile 0 && hasVersion s cFile 0)
        let foreign (d: Diagnostic) = d.Message.Contains "string"
        do! eventually "C's error in A" (fun () -> lastOf s aFile |> List.exists foreign)
        let message = (lastOf s aFile |> List.find foreign).Message

        if close then
            do! s.Lsp.Notify("textDocument/didClose", textDocument [ "uri", JsonValue.Create(Paths.toUri cFile) ])
            do! eventually "C is cleared" (fun () -> publishesOf s cFile |> Array.exists (fun (_, v, _) -> v = None))
            do! s.Server.Publisher.Flushed()
            Expect.contains (lastOf s aFile |> List.map _.Message) message "C's error in A stays: C on disk still reports it"

        return s, aFile, cFile, project, message
    }

/// Drops `name` from the project file
let private dropFromProject (project: string) (name: string) =
    File.WriteAllText(
        project,
        (File.ReadAllText project)
            .Replace($"    <Compile Include=\"%s{name}\" />\r\n", "")
            .Replace($"    <Compile Include=\"%s{name}\" />\n", "")
    )

let private weakGraph (s: Session) (path: string) : WeakReference = WeakReference((s.Server.Workspace.ProjectOf path).Value)

let private collected (r: WeakReference) =
    for _ in 1..3 do
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

    not r.IsAlive

/// Drops the graph's event log, which a Debug build of Ranvier keeps with every value its nodes published (as
/// ReferencesIndexTests does). A no-op for an untraced Ranvier.
let private dropTrace (host: Rfs.Checker.GraphHost) : Task =
    host.Read(fun () ->
        let graph = host.Graph

        let traceModule =
            [ "Ranvier.TraceModule"; "Ranvier.Trace" ]
            |> List.tryPick (fun name ->
                match typeof<Ranvier.Graph>.Assembly.GetType name with
                | null -> None
                | t -> Option.ofObj (t.GetMethod "lastSeq") |> Option.map (fun m -> t, m))

        match traceModule with
        | Some(t, lastSeq) ->
            let last = lastSeq.Invoke(null, [| box graph |])
            t.GetMethod("checkpoint").Invoke(null, [| box graph; last; box "dropped" |]) |> ignore
        | None -> ())

/// The index of the first publish of `path` at `version`, in arrival order
let private indexOf (s: Session) (path: string) (version: int option) =
    s.Published.ToArray()
    |> Array.findIndex (fun (p, v, _) -> v = version && String.Equals(p, LspClient.NormalizePath path, StringComparison.OrdinalIgnoreCase))

[<Tests>]
let rowTests =
    testSequenced
    <| testList
        "Publish rows"
        [ testTask "nothing is published for a closed document after its empty list" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let graph = (s.Server.Workspace.ProjectOf a).Value
              // Closed before version 1 has settled: the settled result comes after the close
              do! s.Lsp.DidChange(a, 1, replace s a "baseValue = 10" "baseValue = \"s\"")
              do! s.Lsp.Notify("textDocument/didClose", textDocument [ "uri", JsonValue.Create(Paths.toUri a) ])
              do! eventually "the empty publish" (fun () -> publishesOf s a |> Array.exists (fun (_, v, _) -> v = None))
              let! _ = graph.Diagnostics a |> within
              do! s.Server.Publisher.Flushed()
              do! Task.Delay 500
              do! s.Server.Publisher.Flushed()
              let after = publishesOf s a |> Array.skipWhile (fun (_, v, _) -> v <> None)
              Expect.equal after.Length 1 "the empty list is the last publish"
              let _, _, last = after[0]
              Expect.isEmpty last "and it is empty"
          }

          testTask "publishes leave in the order they were queued, across two files" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a; b ]
              do! eventually "the open publishes" (fun () -> hasVersion s a 0 && hasVersion s b 0)
              let hold = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
              let entered = ref false

              s.Server.Publisher.BeforePublish(fun lsp ->
                  if lsp = 1 then
                      entered.Value <- true
                      hold.Task
                  else
                      Task.CompletedTask)

              do! s.Lsp.DidChange(a, 1, atEnd s a "\n// a")
              do! eventually "A's list at the hook" (fun () -> entered.Value)
              do! s.Lsp.DidChange(b, 7, atEnd s b "\n// b")
              do! eventually "the server at version 7" (fun () -> (s.Server.Documents.TryFind b |> Option.map _.LspVersion) = Some 7)
              do! s.Server.Publisher.Barrier()
              let! _ = (s.Server.Workspace.ProjectOf b).Value.Diagnostics b |> within
              do! s.Server.Publisher.Barrier()
              do! Task.Delay 300
              Expect.isFalse (hasVersion s b 7) "B's list waits behind A's"
              hold.SetResult()
              do! eventually "both lists" (fun () -> hasVersion s a 1 && hasVersion s b 7)
              Expect.isLessThan (indexOf s a (Some 1)) (indexOf s b (Some 7)) "A's list, queued first, arrives first"
          }

          testTask "a fault in one project leaves another's publishes alone" {
              let root, p1, p2, shared = sharedFixture ()
              let x = Path.Combine(Path.GetDirectoryName p2, "X.fs")
              let! s = startSessionWith root [ p1; p2 ] true ignore |> within
              use _ = disposing s
              do! openAll s [ shared; x ]
              do! eventually "the open publishes" (fun () -> hasVersion s shared 0 && hasVersion s x 0)
              let first = (s.Server.Workspace.OwnersOf shared).Head
              let second = (s.Server.Workspace.ProjectOf x).Value
              Expect.isFalse (obj.ReferenceEquals(first, second)) "S is published from P1, X from P2"
              let hold = new ManualResetEventSlim false
              let entered = ref false
              let armed = ref true

              // X's edit is waiting to settle when P1 fails
              second.OnFlight(fun kind path _ ->
                  if armed.Value && kind = "parse" && path.EndsWith "X.fs" then
                      armed.Value <- false
                      entered.Value <- true
                      hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)

              do! s.Lsp.DidChange(x, 1, replace s x "let y : int" "let y : string")
              do! eventually "X's parse" (fun () -> entered.Value)
              do! s.Server.Publisher.Barrier()
              do! Task.Delay 200
              first.FailRestart(InvalidOperationException "an injected restart fault")
              do! s.Server.Publisher.Barrier()
              do! s.Server.Publisher.Barrier()
              hold.Set()
              let! all = s.Oracle.Check p2 (Map [ x, s.Lsp.Text x ])
              let expected = all |> List.filter (fun d -> d.File.EndsWith "X.fs")
              Expect.isFalse (List.isEmpty expected) "the edit breaks X"
              do! eventually "X's version 1" (fun () -> hasVersion s x 1)
              Expect.isEmpty (Diagnostics.compare expected (lastOf s x)) "X equals the oracle"
              let before = (publishesOf s shared).Length
              do! s.Lsp.DidChange(shared, 1, atEnd s shared "\n// s")
              do! s.Server.Publisher.Flushed()
              do! Task.Delay 1000
              do! s.Server.Publisher.Flushed()
              Expect.equal (publishesOf s shared).Length before "S, in the failed project, gets nothing new"
              let! faulted = s.Server.Publisher.Faulted shared
              Expect.isTrue faulted "and S's fault is logged"
          }

          testTask "a close that a load overtakes still takes the unsaved text out" {
              let! s = startSessionWith depsDir [ depsProject ] false ignore |> within
              use _ = disposing s
              do! openAll s [ a; c ]
              do! eventually "the empty publishes" (fun () -> hasVersion s a 0 && hasVersion s c 0)
              do! s.Lsp.DidChange(a, 1, replace s a "(s: Shape) : int =" "(s: Shape) : unit =")
              do! eventually "the server at version 1" (fun () -> (s.Server.Documents.TryFind a |> Option.map _.LspVersion) = Some 1)
              let! broken = oracleFor s [ a; c ] c
              Expect.isFalse (List.isEmpty broken) "the unsaved edit breaks C"
              // The load registers the project, with A's unsaved text, after Close has looked at the disk and before
              // its post runs
              s.Server.Publisher.OnClose(fun _ -> s.Load().Wait())
              s.Server.Publisher.Close a
              s.Server.Publisher.OnClose ignore
              do! s.Server.Publisher.Barrier()
              let graph = (s.Server.Workspace.ProjectOf c).Value
              let! (settled: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) = graph.Diagnostics c |> within
              let! onDisk = oracleFor s [] c
              Expect.isTrue (List.isEmpty onDisk) "C is clean with A as on disk"
              Expect.isEmpty (settled |> Array.map _.Message |> List.ofArray) "the graph checks C against A on disk"
          }

          testTask "a closed file's diagnostics in an open file follow it while it reports any" {
              let! (s, aFile, _, _, message) = foreignSession true
              use _ = disposing s
              // C is closed: only its row's check says that A's edit takes its error away
              do! s.Lsp.DidChange(aFile, 1, replace s aFile "baseValue = 10" "baseValue = \"s\"")
              do! eventually "A's version 1 without C's error" (fun () -> hasVersion s aFile 1 && not (lastOf s aFile |> List.exists (fun d -> d.Message = message)))
          }

          testTask "a closed file's diagnostics in an open file go when a reload drops it, and so does the old graph" {
              let! (s, aFile, _, project, message) = foreignSession true
              use _ = disposing s
              let old = weakGraph s aFile
              dropFromProject project "C.fs"

              do! s.Server.Workspace.Reload project |> within
              do! s.Server.Publisher.Flushed()
              do! eventually "A without C's error" (fun () -> not (lastOf s aFile |> List.exists (fun d -> d.Message = message)))
              // The closed row went with the reload, so nothing of the publisher holds the replaced graph
              do! dropTrace s.Server.Host
              Expect.isTrue (collected old) "the replaced graph is collected"
          }

          testTask "an open file's diagnostics in another open file go when a reload drops it" {
              let! (s, aFile, cFile, project, message) = foreignSession false
              use _ = disposing s
              dropFromProject project "C.fs"
              do! s.Server.Workspace.Reload project |> within
              do! s.Server.Publisher.Flushed()
              do! eventually "C's empty list" (fun () -> (lastOf s cFile).IsEmpty)
              do! eventually "A without C's error" (fun () -> not (lastOf s aFile |> List.exists (fun d -> d.Message = message)))
          }

          testTask "work after a pump sees every effect of the pump's flush, whatever order they were made in" {
              let host = Rfs.Checker.GraphHost.Start "after-pump"
              use _ = { new IAsyncDisposable with member _.DisposeAsync() = (host.Dispose(); ValueTask.CompletedTask) }

              for countingFirst in [ true; false ] do
                  let adopted = ref 0
                  let seen = Collections.Concurrent.ConcurrentQueue<int>()

                  let! (signal: Ranvier.Signal<int>) =
                      host.Read(fun () ->
                          let signal = Ranvier.Api.createSignal 0

                          let counting () =
                              Ranvier.Api.createEffect (fun () ->
                                  if signal.Value > 0 then
                                      GraphTurn.afterPump host (fun () -> seen.Enqueue adopted.Value))

                          let adopting () =
                              Ranvier.Api.createEffect (fun () ->
                                  if signal.Value > 0 then
                                      adopted.Value <- adopted.Value + 1)

                          Ranvier.Api.createRoot (fun _ ->
                              if countingFirst then
                                  counting ()
                                  adopting ()
                              else
                                  adopting ()
                                  counting ())
                          |> ignore

                          signal)

                  host.Post(fun () -> signal.Value <- 1)
                  do! eventually "the count" (fun () -> seen.Count = 1)
                  Expect.equal (seen.ToArray()) [| 1 |] $"the count includes the adoption (counting effect made first: %b{countingFirst})"
          } ]

/// A held parse of `path` in `graph`: set the event to release it
let private holdParse (graph: Rfs.Checker.ProjectGraph) (path: string) : ManualResetEventSlim * bool ref =
    let hold = new ManualResetEventSlim false
    let entered = ref false
    let armed = ref true

    graph.OnFlight(fun kind file _ ->
        if armed.Value && kind = "parse" && file.EndsWith(Path.GetFileName path) then
            armed.Value <- false
            entered.Value <- true
            hold.Wait(TimeSpan.FromSeconds 60.) |> ignore)

    hold, entered

[<Tests>]
let workspaceGraphTests =
    testSequenced
    <| testList
        "Workspace graph"
        [ testTask "a fault in one project fails none of another's waits" {
              let root, p1, p2, shared = sharedFixture ()
              let x = Path.Combine(Path.GetDirectoryName p2, "X.fs")
              // Two declarations, so X is checked by declaration and has Results
              File.WriteAllText(x, "module X\n\nlet y : int = S.value\n\nlet w = 2\n")
              let! s = startSessionWith root [ p1; p2 ] true ignore |> within
              use _ = disposing s
              do! openAll s [ shared; x ]
              do! eventually "the open publishes" (fun () -> hasVersion s shared 0 && hasVersion s x 0)
              let first = (s.Server.Workspace.OwnersOf shared).Head
              let second = (s.Server.Workspace.ProjectOf x).Value
              let! _ = second.WhenPromoted(x, CancellationToken.None) |> within
              let hold, entered = holdParse second x
              do! s.Lsp.DidChange(x, 1, replace s x "let y : int" "let y : string")
              do! eventually "X's parse" (fun () -> entered.Value)
              do! s.Server.Publisher.Barrier()

              // Every kind of wait on P2, parked while X is held
              let settled = second.SettledDiagnostics x
              let diagnostics = second.Diagnostics x
              let all = second.AllDiagnostics()
              let promoted = second.WhenPromoted(x, CancellationToken.None)
              let results = second.Results x

              // The first context's wait of a project that references P2
              let p3 = Path.Combine(root, "p3")
              Directory.CreateDirectory p3 |> ignore
              let z = Path.Combine(p3, "Z.fs")
              File.WriteAllText(z, "module Z\n\nlet z : int = 1\n")

              let output =
                  second.Spec.OtherOptions
                  |> List.tryPick (fun o -> if o.StartsWith "-o:" then Some(o.Substring 3) else None)
                  |> Option.defaultValue (Path.Combine(Path.GetDirectoryName p2, "P2.dll"))

              let spec3 =
                  { second.Spec with
                      ProjectFile = Path.Combine(p3, "P3.fsproj")
                      SourceFiles = [ z ]
                      OtherOptions = (second.Spec.OtherOptions |> List.filter (fun o -> not (o.StartsWith "-o:"))) @ [ "-o:" + Path.Combine(p3, "P3.dll"); "-r:" + output ]
                      ReferencedProjects = [ p2 ]
                      ReferencedFrameworks = [ "" ]
                      GeneratedFiles = [] }

              let created = Rfs.Checker.ProjectGraph.Create(s.Server.Host, spec3, [ second ])
              do! Task.Delay 300
              first.FailRestart(InvalidOperationException "an injected restart fault")
              do! s.Server.Publisher.Barrier()
              do! s.Server.Publisher.Barrier()
              do! Task.Delay 300
              hold.Set()

              let succeeds (what: string) (t: Task) =
                  task {
                      try
                          do! t.WaitAsync(TimeSpan.FromSeconds 60.)
                      with ex ->
                          failtest $"%s{what} failed: %s{ex.Message}"
                  }

              do! succeeds "SettledDiagnostics" settled
              do! succeeds "Diagnostics" diagnostics
              do! succeeds "AllDiagnostics" all
              do! succeeds "WhenPromoted" promoted
              do! succeeds "Results" results
              do! succeeds "the first context's wait" created
              let! graph3 = created
              (graph3 :> IDisposable).Dispose()
              do! eventually "X's version 1" (fun () -> hasVersion s x 1)
              // The failed project's own waits fail with it
              let! (own: Task) = Task.WhenAny(first.Diagnostics shared :> Task, Task.Delay 30000)
              Expect.isTrue own.IsFaulted "a wait of the failed project fails"
          }

          testTask "a faulted file's findings leave the other files' lists" {
              // X, in P2, reports an error in S, which P1 publishes; then P2 fails, and P1 goes on
              let root, p1, p2, shared = sharedFixture ()
              let x = Path.Combine(Path.GetDirectoryName p2, "X.fs")
              File.WriteAllText(x, $"module X\n\nlet y : int = S.value\n\n# 1 \"%s{shared}\"\nmodule Foreign =\n    let bad : string = 1\n")
              let! s = startSessionWith root [ p1; p2 ] true ignore |> within
              use _ = disposing s
              do! openAll s [ shared; x ]
              do! eventually "the open publishes" (fun () -> hasVersion s shared 0 && hasVersion s x 0)
              let foreign (d: Diagnostic) = d.Message.Contains "string"
              do! eventually "X's error in S" (fun () -> lastOf s shared |> List.exists foreign)
              let second = (s.Server.Workspace.ProjectOf x).Value
              second.FailRestart(InvalidOperationException "an injected restart fault")
              do! eventually "X's fault" (fun () -> s.Server.Publisher.Faulted x |> fun t -> t.Result)
              do! eventually "S without X's error" (fun () -> not (lastOf s shared |> List.exists foreign))
          }

          testTask "during a reload a file is never sent another file's findings from the old graph" {
              let! (s, aFile, cFile, project, message) = foreignSession false
              use _ = disposing s
              let release = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
              s.Server.Workspace.BeforeLoadEnd(fun () -> release.Task)
              dropFromProject project "C.fs"
              let before = (publishesOf s aFile).Length
              let reload = s.Server.Workspace.Reload project
              // A settles on the new graph, and is sent again, before the reload ends
              do! eventually "A's list on the new graph" (fun () -> (publishesOf s aFile).Length > before)
              do! s.Server.Publisher.Flushed()
              let sent = publishesOf s aFile |> Array.skip before

              for _, _, list in sent do
                  Expect.isFalse (list |> List.exists (fun d -> d.Message = message)) "A is never sent C's old-graph error"

              release.SetResult()
              do! reload |> within
              do! eventually "C's empty list" (fun () -> (lastOf s cFile).IsEmpty)
          }

          testTask "closed files changed on disk together settle in one pass" {
              let dir = copyFixtureDir "Deps"
              let project = Path.Combine(dir, "Deps.fsproj")
              let aFile = Path.Combine(dir, "A.fs")
              let bFile = Path.Combine(dir, "B.fs")
              let cFile = Path.Combine(dir, "C.fs")
              let! s = startSessionWith dir [ project ] true ignore |> within
              use _ = disposing s
              do! openAll s [ cFile ]
              do! eventually "C's publish" (fun () -> hasVersion s cFile 0)
              let! passes = s.Server.Publisher.DiskPasses()
              File.AppendAllText(aFile, "\n// a\n")
              File.AppendAllText(bFile, "\n// b\n")

              let change (path: string) = $$"""{"uri":{{JsonValue.Create(Paths.toUri path).ToJsonString()}},"type":2}"""
              do! s.Lsp.Notify("workspace/didChangeWatchedFiles", JsonNode.Parse($$"""{"changes":[{{change aFile}},{{change bFile}}]}"""))
              do! eventually "the pass" (fun () -> s.Server.Publisher.DiskPasses().Result > passes)
              do! Task.Delay 1000
              let! after = s.Server.Publisher.DiskPasses()
              Expect.equal (after - passes) 1 "one pass for both files"
              let graph = (s.Server.Workspace.ProjectOf cFile).Value
              let! _ = graph.Diagnostics cFile |> within
              ()
          }

          testTask "a counted fault that leaves a file pending takes its findings out of other files' lists" {
              // X, in P2, reports an error in S, which P1 publishes; X's next check is held, and P2's work fails meanwhile
              let root, p1, p2, shared = sharedFixture ()
              let x = Path.Combine(Path.GetDirectoryName p2, "X.fs")
              File.WriteAllText(x, $"module X\n\nlet y : int = S.value\n\n# 1 \"%s{shared}\"\nmodule Foreign =\n    let bad : string = 1\n")
              let! s = startSessionWith root [ p1; p2 ] true ignore |> within
              use _ = disposing s
              do! openAll s [ shared; x ]
              do! eventually "the open publishes" (fun () -> hasVersion s shared 0 && hasVersion s x 0)
              let foreign (d: Diagnostic) = d.Message.Contains "string"
              do! eventually "X's error in S" (fun () -> lastOf s shared |> List.exists foreign)
              let second = (s.Server.Workspace.ProjectOf x).Value
              let hold, entered = holdParse second x
              do! s.Lsp.DidChange(x, 1, replace s x "let y : int" "let y  : int")
              do! eventually "X's parse" (fun () -> entered.Value)
              second.FailPost(InvalidOperationException "an injected post fault")
              do! eventually "S without X's error, X still pending" (fun () -> not (lastOf s shared |> List.exists foreign))
              Expect.isFalse (hasVersion s x 1) "X's check is still held"
              hold.Set()
          }

          testTask "a project's own parked wait fails through its fault count" {
              let root, p1, p2, shared = sharedFixture ()
              let x = Path.Combine(Path.GetDirectoryName p2, "X.fs")
              let! s = startSessionWith root [ p1; p2 ] true ignore |> within
              use _ = disposing s
              do! openAll s [ shared; x ]
              do! eventually "the open publishes" (fun () -> hasVersion s shared 0 && hasVersion s x 0)
              let second = (s.Server.Workspace.ProjectOf x).Value
              let hold, entered = holdParse second x
              do! s.Lsp.DidChange(x, 1, replace s x "let y : int" "let y  : int")
              do! eventually "X's parse" (fun () -> entered.Value)
              do! s.Server.Publisher.Barrier()
              let parked = second.Diagnostics x
              do! Task.Delay 200
              Expect.isFalse parked.IsCompleted "parked on the held parse"
              second.FailPost(InvalidOperationException "an injected post fault")
              let! finished = Task.WhenAny(parked :> Task, Task.Delay 10000)
              Expect.isTrue (obj.ReferenceEquals(finished, parked)) "the wait ended"
              Expect.isTrue (parked.Exception.InnerException :? Rfs.Checker.GraphFailedException) "with the project's fault"
              hold.Set()
          }

          testTask "an effect of a project's nodes that fails is counted to that project" {
              let! (s, _, cFile, _, _) = foreignSession false
              use _ = disposing s
              let graph = (s.Server.Workspace.ProjectOf cFile).Value
              let before = graph.FaultCount
              let armed = ref true

              graph.OnFlight(fun kind file _ ->
                  if armed.Value && kind = "parse" && file.EndsWith "C.fs" then
                      raise (InvalidOperationException "an injected fault in C"))

              do! s.Lsp.DidChange(cFile, 1, atEnd s cFile "\n// c")
              do! eventually "the fault counted" (fun () -> graph.FaultCount > before)
              armed.Value <- false
          } ]
