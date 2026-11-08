module Rfs.Server.Tests.PublishTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Harness
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests

let internal within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 120.)

let internal depsDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures", "Deps")
        if Directory.Exists candidate then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let internal depsProject = Path.Combine(depsDir, "Deps.fsproj")
let internal file (name: string) = Path.Combine(depsDir, name)
let internal a = file "A.fs"
let internal b = file "B.fs"
let internal c = file "C.fs"

/// A running server with Deps loaded, and everything it published
type internal Session =
    { Lsp: LspClient
      Server: RanvierServer
      Oracle: OracleClient
      /// path, version, diagnostics, in arrival order
      Published: ConcurrentQueue<string * int option * Diagnostic list>
      /// the uri of each publish, as sent
      Uris: ConcurrentQueue<string>
      /// the count of each rfs/declarationsChecked
      Counts: ConcurrentQueue<int>
      Owner: IDisposable
      /// the first project
      Project: string
      /// loads the projects
      Load: unit -> Task }

    interface IAsyncDisposable with
        member s.DisposeAsync() =
            s.Owner.Dispose()
            (s.Oracle :> IAsyncDisposable).DisposeAsync()

let internal startSessionWith (root: string) (projects: string list) (loadNow: bool) (prepare: RanvierServer -> unit) : Task<Session> =
    task {
        let! lsp, owner, server = TestServer.startCapturing ()
        let published = ConcurrentQueue()
        let uris = ConcurrentQueue()
        let counts = ConcurrentQueue()

        lsp.Diagnostics.Subscribe
            { new IObserver<_> with
                member _.OnNext((_, path, version, list)) = published.Enqueue((path, version, list))
                member _.OnError _ = ()
                member _.OnCompleted() = () }
        |> ignore

        lsp.Notifications.Subscribe
            { new IObserver<Received> with
                member _.OnNext r =
                    match r.Message["method"].GetValue<string>() with
                    | "textDocument/publishDiagnostics" -> uris.Enqueue(r.Message["params"].["uri"].GetValue<string>())
                    | "rfs/declarationsChecked" -> counts.Enqueue(r.Message["params"].["count"].GetValue<int>())
                    | _ -> ()

                member _.OnError _ = ()
                member _.OnCompleted() = () }
        |> ignore

        let! _ = lsp.Initialize(Paths.toUri root, JsonNode.Parse "{}") |> within

        let load () : Task =
            let documents = JsonArray()

            for p in projects do
                let d = JsonObject()
                d["Uri"] <- JsonValue.Create(Paths.toUri p)
                documents.Add d

            let parameters = JsonObject()
            parameters["TextDocuments"] <- documents
            task {
                let! _ = lsp.Request("fsharp/workspaceLoad", parameters) |> within
                ()
            }

        let! running = server
        prepare running

        if loadNow then
            do! load ()

        let! oracle = OracleClient.start ()

        return
            { Lsp = lsp
              Server = running
              Oracle = oracle
              Published = published
              Uris = uris
              Counts = counts
              Owner = owner
              Project = projects.Head
              Load = load }
    }

let internal startSession () = startSessionWith depsDir [ depsProject ] true ignore

let internal eventually (what: string) (check: unit -> bool) : Task =
    task {
        let deadline = DateTime.UtcNow.AddSeconds 60.

        while not (check ()) do
            if DateTime.UtcNow > deadline then
                failtest $"timed out waiting for %s{what}"

            do! Task.Delay 25
    }

let internal publishesOf (s: Session) (path: string) =
    s.Published.ToArray()
    |> Array.filter (fun (p, _, _) -> String.Equals(p, LspClient.NormalizePath path, StringComparison.OrdinalIgnoreCase))

let internal openAll (s: Session) (paths: string list) =
    task {
        for p in paths do
            do! s.Lsp.DidOpen(p, File.ReadAllText p)
    }

let internal hasVersion (s: Session) (path: string) (version: int) =
    publishesOf s path |> Array.exists (fun (_, v, _) -> v = Some version)

/// The edit that replaces the first `find` in the client's text of `path`
let internal replace (s: Session) (path: string) (find: string) (text: string) : Edit =
    let current = s.Lsp.Text path
    let index = current.IndexOf(find, StringComparison.Ordinal)
    Expect.isGreaterThanOrEqual index 0 $"%s{find} is in %s{path}"

    let position (offset: int) =
        let before = current.Substring(0, offset)
        let line = before.Split('\n').Length - 1
        { Line = line; Character = offset - (before.LastIndexOf '\n' + 1) }

    { Range = { Start = position index; End = position (index + find.Length) }; Text = text }

/// The oracle's diagnostics for `path` when the client's texts of `paths` are the project's
let internal oracleFor (s: Session) (paths: string list) (path: string) =
    task {
        let overrides = paths |> List.map (fun p -> p, s.Lsp.Text p) |> Map.ofList
        let! all = s.Oracle.Check s.Project overrides
        let own = LspClient.NormalizePath path
        return all |> List.filter (fun d -> String.Equals(d.File, own, StringComparison.OrdinalIgnoreCase))
    }

let internal expectEqualOracle (s: Session) (paths: string list) (path: string) (version: int) =
    task {
        let! expected = oracleFor s paths path

        let _, _, actual =
            publishesOf s path |> Array.filter (fun (_, v, _) -> v = Some version) |> Array.last

        let diff = Diagnostics.compare expected actual
        Expect.isEmpty diff $"%s{Path.GetFileName path} at version %d{version} equals the oracle"
    }

/// `{ "textDocument": { ... } }`, with the given members
let internal textDocument (members: (string * JsonValue) list) : JsonNode =
    let inner = JsonObject()

    for k, v in members do
        inner[k] <- v

    let outer = JsonObject()
    outer["textDocument"] <- inner
    outer :> JsonNode

let internal disposing (s: Session) =
    { new IAsyncDisposable with
        member _.DisposeAsync() = (s :> IAsyncDisposable).DisposeAsync() }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Publish"
        [ testTask "diagnostics equal oracle after edits" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! expectEqualOracle s [ a ] a 0
              let mutable valueBroken = false
              let mutable returnBroken = false

              for i in 1..10 do
                  let edit =
                      if i % 2 = 1 then
                          // body edit: the type of the value
                          valueBroken <- not valueBroken
                          if valueBroken then replace s a "baseValue = 10" "baseValue = \"s\"" else replace s a "baseValue = \"s\"" "baseValue = 10"
                      else
                          // return type edit
                          returnBroken <- not returnBroken

                          if returnBroken then
                              replace s a "(s: Shape) : int =" "(s: Shape) : string ="
                          else
                              replace s a "(s: Shape) : string =" "(s: Shape) : int ="

                  do! s.Lsp.DidChange(a, i, edit)
                  do! eventually $"the publish of version %d{i}" (fun () -> hasVersion s a i)
                  do! expectEqualOracle s [ a ] a i
          }

          testTask "rapid edits publish only the latest" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              do! s.Lsp.DidChange(a, 2, replace s a "baseValue = 10" "baseValue = \"s\"")
              do! s.Lsp.DidChange(a, 3, replace s a "baseValue = \"s\"" "baseValue = 11")
              do! s.Lsp.DidChange(a, 4, replace s a "baseValue = 11" "baseValue = \"t\"")
              do! eventually "the publish of version 4" (fun () -> hasVersion s a 4)
              do! Task.Delay 1000
              let versions = publishesOf s a |> Array.choose (fun (_, v, _) -> v)
              Expect.equal (Array.last versions) 4 "the last publish is version 4"
              Expect.equal versions (Array.sort versions) "versions only rise"
              Expect.equal (versions |> Array.distinct |> Array.length) versions.Length "one publish per version"
          }

          testTask "close publishes empty" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! s.Lsp.DidChange(a, 1, replace s a "baseValue = 10" "baseValue = \"s\"")
              do! eventually "an error in A" (fun () -> publishesOf s a |> Array.exists (fun (_, v, d) -> v = Some 1 && not d.IsEmpty))
              let before = (publishesOf s a).Length

              let close =
                  textDocument [ "uri", JsonValue.Create(Paths.toUri a) ]

              do! s.Lsp.Notify("textDocument/didClose", close)
              do! eventually "the empty publish" (fun () -> (publishesOf s a).Length > before)
              let _, _, last = Array.last (publishesOf s a)
              Expect.isEmpty last "closing clears the diagnostics"
          }

          testTask "lowercase percent-encoded drive uri" {
              let! s = startSession () |> within
              use _ = disposing s

              let uri =
                  if OperatingSystem.IsWindows() then
                      "file:///c%3A" + (Paths.toUri a).Substring("file:///C:".Length)
                  else
                      Paths.toUri a

              let document =
                  textDocument [ "uri", JsonValue.Create uri
                                 "languageId", JsonValue.Create "fsharp"
                                 "version", JsonValue.Create 7
                                 "text", JsonValue.Create(File.ReadAllText a) ]

              do! s.Lsp.Notify("textDocument/didOpen", document)
              do! eventually "a publish" (fun () -> hasVersion s a 7)
              Expect.contains (s.Uris.ToArray()) uri "diagnostics come back under the URI as sent"
              Expect.isTrue (s.Uris.ToArray() |> Array.forall ((=) uri)) "and only under it"
          }

          testTask "dependent open file updates" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a; b; c ]
              do! eventually "the open publishes" (fun () -> hasVersion s a 0 && hasVersion s b 0 && hasVersion s c 0)
              let before = (publishesOf s c).Length
              do! s.Lsp.DidChange(a, 1, replace s a "(s: Shape) : int =" "(s: Shape) : unit =")
              let! expected = oracleFor s [ a; b; c ] c
              Expect.isFalse (List.isEmpty expected) "the edit breaks C"
              do! eventually "a publish for C" (fun () -> (publishesOf s c).Length > before)

              do!
                  eventually "C equals the oracle" (fun () ->
                      let _, _, last = Array.last (publishesOf s c)
                      Diagnostics.compare expected last |> List.isEmpty)
          }

          testTask "typing during promotion" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]

              for i in 1..5 do
                  let edit =
                      if i % 2 = 1 then
                          replace s a "baseValue = 10" "baseValue = \"s\""
                      else
                          replace s a "baseValue = \"s\"" "baseValue = 10"

                  do! s.Lsp.DidChange(a, i, edit)

              do! eventually "the publish of version 5" (fun () -> hasVersion s a 5)
              do! Task.Delay 500
              let versions = publishesOf s a |> Array.choose (fun (_, v, _) -> v)
              Expect.equal versions (Array.sort versions) "versions only rise"
              Expect.equal (Array.last versions) 5 "the last publish is the last edit"
              do! expectEqualOracle s [ a ] a 5
          }

          testTask "declarationsChecked follows each publish" {
              let! s = startSession () |> within
              use _ = disposing s
              do! openAll s [ a ]
              do! eventually "the open publish" (fun () -> hasVersion s a 0)
              let graph = (s.Server.Workspace.ProjectOf a).Value
              let! _ = graph.WhenPromoted(a, CancellationToken.None) |> within
              // the first edit after promotion also carries the promotion's own checks
              do! s.Lsp.DidChange(a, 1, replace s a "* 2\n" "* 3\n")
              do! eventually "the first notification" (fun () -> hasVersion s a 1 && s.Counts.Count >= 2)
              let seen = s.Counts.Count
              do! s.Lsp.DidChange(a, 2, replace s a "* 3\n" "* 4\n")
              do! eventually "the next notification" (fun () -> hasVersion s a 2 && s.Counts.Count > seen)
              Expect.equal (s.Counts.ToArray()[seen]) 1 "one declaration checked for a body edit"
          }
        ]
