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

          testTask "a foreign publish does not suppress the file's own" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let hold = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
              let entered = ref false

              s.Server.Publisher.BeforePublish(fun lsp ->
                  if lsp = 1 then
                      entered.Value <- true
                      hold.Task :> Task
                  else
                      Task.CompletedTask)

              do! s.Lsp.DidChange(a, 1, replace s a "baseValue = 10" "baseValue = \"s\"")
              do! eventually "version 1 settled" (fun () -> entered.Value)
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
              hold.SetResult()
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
              let before = s.Server.Publisher.RetainedGraphs
              let counts = s.Counts.Count
              let published = (publishesOf s a).Length
              do! s.Server.Workspace.Reload depsProject |> within
              do! eventually "the publish after the reload" (fun () -> (publishesOf s a).Length > published && s.Counts.Count > counts)
              do! s.Server.Publisher.Flushed()
              Expect.equal s.Server.Publisher.RetainedGraphs before "state is kept for the live graph only"
          }

          testTask "rapid edits: an older result held at the gate is not published" {
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
              do! eventually "version 2 settled" (fun () -> entered.ContainsKey 2)
              holds[1].SetResult()
              do! Task.Delay 300
              holds[2].SetResult()
              do! eventually "version 2 published" (fun () -> hasVersion s a 2)
              Expect.isFalse (hasVersion s a 1) "the older result is dropped"
          }

          testTask "typing during promotion" {
              let hold = new ManualResetEventSlim false

              let prepare (server: RanvierServer) =
                  server.Workspace.OnGraphCreated(fun graph ->
                      graph.OnBeginFile(fun path ->
                          if String.Equals(path, a, StringComparison.OrdinalIgnoreCase) then
                              hold.Wait(TimeSpan.FromSeconds 90.) |> ignore))

              let! s = startSessionWith depsDir [ depsProject ] true prepare |> within
              use _ = disposing s
              do! openAll s [ a ]
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
              do! Task.Delay 500
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
