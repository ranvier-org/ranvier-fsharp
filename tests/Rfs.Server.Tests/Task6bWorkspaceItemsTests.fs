module Rfs.Server.Tests.Task6bWorkspaceItemsTests

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Ionide.LanguageServerProtocol.Types
open Ionide.ProjInfo
open Rfs.Checker
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 90.)
let private withinT (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 90.)

let private disposing (d: IDisposable) =
    { new IAsyncDisposable with
        member _.DisposeAsync() =
            d.Dispose()
            ValueTask() }

type private TempDir(dir: string) =
    let cleanup () =
        try
            Directory.Delete(dir, true)
        with _ ->
            ()

    member _.Dir = dir

    interface IDisposable with
        member _.Dispose() = cleanup ()

    interface IAsyncDisposable with
        member _.DisposeAsync() =
            cleanup ()
            ValueTask()

let private makeTempRoot () =
    let dir = Path.Combine(Path.GetTempPath(), "rfs-t6b-srv-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory dir |> ignore
    new TempDir(dir)

let private findRepoRoot () =
    let rec up (d: DirectoryInfo) =
        if isNull d then
            failwith "Could not locate RanvierFSharp.slnx from AppContext.BaseDirectory"
        else
            let candidate = Path.Combine(d.FullName, "RanvierFSharp.slnx")
            if File.Exists candidate then d.FullName else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private writeText (path: string) (text: string) =
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

let private parseContent (node: JsonNode) : JsonNode =
    let text =
        match node["content"] with
        | null -> node["Content"].GetValue<string>()
        | c -> c.GetValue<string>()

    JsonNode.Parse text

[<Tests>]
let slnxAndWorkspaceItemsTests =
    testSequenced
    <| testList
        "Task6b.SlnxAndWorkspaceItems"
        [ test "peeks real RanvierFSharp.slnx with leaf folder names, non-empty stable GUIDs, and all 13 fsprojs" {
              let repoRoot = findRepoRoot ()
              let slnxPath = Path.Combine(repoRoot, "RanvierFSharp.slnx")
              let found = WorkspacePeek.peek repoRoot 2 [ ".git"; "artifacts"; ".worktrees" ]

              let slnData =
                  found
                  |> List.tryPick (function
                      | WorkspacePeek.Interesting.Solution(p, d) when String.Equals(p, slnxPath, StringComparison.OrdinalIgnoreCase) -> Some d
                      | _ -> None)

              Expect.isSome slnData "RanvierFSharp.slnx parsed as Solution"
              let items = slnData.Value.Items
              Expect.equal (items |> List.map _.Name) [ "src"; "tests" ] "Folder Names are leaf segments ('src', 'tests'), never '/src/'"
              Expect.isTrue (items |> List.forall (fun i -> i.Guid <> Guid.Empty)) "Every folder has a non-empty GUID"

              // Repeated parse produces identical deterministic GUIDs
              match WorkspacePeek.tryParseSlnx slnxPath with
              | Error e -> failtest $"tryParseSlnx failed: {e}"
              | Ok again ->
                  Expect.equal (again.Items |> List.map _.Guid) (items |> List.map _.Guid) "GUIDs are deterministic across calls"

              let projects = WorkspacePeek.projectsOf [ WorkspacePeek.Interesting.Solution(slnxPath, slnData.Value) ]
              Expect.equal projects.Length 13 "All 13 .fsproj projects resolved across /src/ and /tests/ folders"
              Expect.isTrue (projects |> List.forall File.Exists) "Every resolved project path exists on disk"

              // Verify JSON serialization in Responses.workspacePeek matches FSAC schema
              let peekJson = JsonNode.Parse(Responses.workspacePeek [ WorkspacePeek.Interesting.Solution(slnxPath, slnData.Value) ])
              Expect.equal (peekJson["Kind"].GetValue<string>()) "workspacePeek" "workspacePeek Kind"
              let solNode = peekJson["Data"].["Found"].[0]
              Expect.equal (solNode["Type"].GetValue<string>()) "solution" "solution Type"
              let jsonFolders = solNode["Data"].["Items"].AsArray()
              Expect.equal (jsonFolders[0].["Name"].GetValue<string>()) "src" "JSON folder Name is 'src'"
              Expect.equal (jsonFolders[0].["Kind"].["Kind"].GetValue<string>()) "folder" "JSON folder Kind"
              Expect.equal (jsonFolders[0].["Kind"].["Data"].["Items"].AsArray().Count) 7 "7 src projects"
              Expect.equal (jsonFolders[1].["Kind"].["Data"].["Items"].AsArray().Count) 6 "6 test projects"
          }

          test "tryParseSlnx materialises implicit ancestor folders, merges duplicate folders, orders Folders->Files->Projects, preserves explicit Id GUIDs, and handles malformed XML gracefully" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              let p (rel: string) = Path.GetFullPath(Path.Combine(root, rel.Replace('/', Path.DirectorySeparatorChar)))

              let slnx = p "Complex.slnx"
              let explicitFolderGuid = Guid.Parse "11111111-2222-3333-4444-555555555555"
              let explicitProjGuid = Guid.Parse "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

              writeText
                  slnx
                  $"""<Solution>
  <Project Path="RootProj\RootProj.fsproj" Id="{explicitProjGuid}" />
  <File Path="Directory.Build.props" />
  <Folder Name="/a/b/c/">
    <Project Path="src/Deep/Deep.fsproj" />
    <File Path="docs/deep.md" />
  </Folder>
  <Folder Name="\a\b" Id="{explicitFolderGuid}">
    <File Path="docs/mid1.md" />
    <Project Path="src/Mid/Mid.fsproj" />
  </Folder>
  <Folder Name="a/b/">
    <File Path="docs/mid1.md" />
    <File Path="docs/mid2.md" />
    <Project Path="src/Mid/Mid.fsproj" />
    <Project Path="src/Mid2/Mid2.fsproj" />
  </Folder>
</Solution>"""

              match WorkspacePeek.tryParseSlnx slnx with
              | Error e -> failtest $"Expected Ok from Complex.slnx, got Error: {e}"
              | Ok sd ->
                  // Root items: Folders first ("Solution Items" from root <File>, then "a"), then Root MSBuildFormat Projects ("RootProj.fsproj")
                  Expect.equal
                      (sd.Items |> List.map _.Name)
                      [ "Solution Items"; "a"; p "RootProj/RootProj.fsproj" ]
                      "Root orders Folders (Solution Items, a) before Projects (RootProj)"

                  let rootProjItem = sd.Items[2]
                  Expect.equal rootProjItem.Guid explicitProjGuid "Explicit Project Id GUID preserved"

                  match sd.Items[0].Kind with
                  | InspectSln.SolutionItemKind.Folder(children, files) ->
                      Expect.isEmpty children "Solution Items has no child items"
                      Expect.equal files [ p "Directory.Build.props" ] "Root <File> resolved into Solution Items"
                  | other -> failtest $"Expected Folder for Solution Items, got %A{other}"

                  // Check implicit ancestor "a" -> merged child "b" -> grandchild "c"
                  let folderA = sd.Items[1]
                  Expect.equal folderA.Name "a" "Implicit ancestor 'a' has leaf Name 'a'"
                  Expect.notEqual folderA.Guid Guid.Empty "Implicit ancestor 'a' gets non-empty synthesized GUID"

                  match folderA.Kind with
                  | InspectSln.SolutionItemKind.Folder([ folderB ], aFiles) ->
                      Expect.isEmpty aFiles "Folder 'a' has no direct files"
                      Expect.equal folderB.Name "b" "Folder 'b' has leaf Name 'b'"
                      Expect.equal folderB.Guid explicitFolderGuid "Merged folder 'b' preserves explicit Id GUID"

                      match folderB.Kind with
                      | InspectSln.SolutionItemKind.Folder(bChildren, bFiles) ->
                          // bFiles deduplicated in declaration order
                          Expect.equal bFiles [ p "docs/mid1.md"; p "docs/mid2.md" ] "Merged folder 'b' deduplicates Files in order"
                          // bChildren: child Folder "c" first, then deduplicated Projects in declaration order!
                          Expect.equal
                              (bChildren |> List.map _.Name)
                              [ "c"; p "src/Mid/Mid.fsproj"; p "src/Mid2/Mid2.fsproj" ]
                              "Folder 'b' orders child Folder 'c' before deduplicated Projects"

                          match bChildren[0].Kind with
                          | InspectSln.SolutionItemKind.Folder(cChildren, cFiles) ->
                              Expect.equal cFiles [ p "docs/deep.md" ] "Grandchild 'c' has deep.md"
                              Expect.equal (cChildren |> List.map _.Name) [ p "src/Deep/Deep.fsproj" ] "Grandchild 'c' has Deep.fsproj"
                          | other -> failtest $"Expected Folder 'c', got %A{other}"
                      | other -> failtest $"Expected Folder 'b', got %A{other}"
                  | other -> failtest $"Expected Folder 'a' with single child 'b', got %A{other}"

              // Malformed .slnx does not throw or crash WorkspacePeek.peek
              let brokenDir = Path.Combine(root, "broken")
              Directory.CreateDirectory brokenDir |> ignore
              writeText (Path.Combine(brokenDir, "Broken.slnx")) "<Solution><Folder Name=\"/unclosed\">"
              writeText (Path.Combine(brokenDir, "Fallback.fsproj")) "<Project Sdk=\"Microsoft.NET.Sdk\" />"

              Expect.isError (WorkspacePeek.tryParseSlnx (Path.Combine(brokenDir, "Broken.slnx"))) "Malformed XML returns Error"
              let peekedBroken = WorkspacePeek.peek brokenDir 1 []
              Expect.equal (WorkspacePeek.projectsOf peekedBroken) [ Path.Combine(brokenDir, "Fallback.fsproj") ] "Malformed .slnx gracefully falls back to directory discovery"
          }

          test "WorkspacePeek.projectsOf prefers .slnx over same-stem .sln migration pair, and falls back to directories on unrelated solutions" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              let p (rel: string) = Path.GetFullPath(Path.Combine(root, rel.Replace('/', Path.DirectorySeparatorChar)))

              let projSlnxOnly = p "OnlyInSlnx/OnlyInSlnx.fsproj"
              let projSlnOnly = p "OnlyInSln/OnlyInSln.fsproj"
              let projExtra = p "Extra/Extra.fsproj"
              writeText projSlnxOnly "<Project Sdk=\"Microsoft.NET.Sdk\" />"
              writeText projSlnOnly "<Project Sdk=\"Microsoft.NET.Sdk\" />"
              writeText projExtra "<Project Sdk=\"Microsoft.NET.Sdk\" />"

              writeText
                  (p "App.slnx")
                  """<Solution>
  <Folder Name="/src/">
    <Project Path="OnlyInSlnx/OnlyInSlnx.fsproj" />
  </Folder>
</Solution>"""

              writeText
                  (p "App.sln")
                  ("Microsoft Visual Studio Solution File, Format Version 12.00\r\n"
                   + "Project(\"{F2A71F9B-5D33-465A-A702-920D77279786}\") = \"OnlyInSln\", \"OnlyInSln\\OnlyInSln.fsproj\", \"{11111111-1111-1111-1111-111111111111}\"\r\n"
                   + "EndProject\r\nGlobal\r\nEndGlobal\r\n")

              let pairPeek = WorkspacePeek.peek root 2 []
              Expect.equal (WorkspacePeek.projectsOf pairPeek) [ projSlnxOnly ] "Same-dir same-stem App.slnx + App.sln prefers App.slnx"

              // Add an unrelated second solution -> falls back to all discovered directories
              writeText
                  (p "Unrelated.slnx")
                  """<Solution>
  <Project Path="Extra/Extra.fsproj" />
</Solution>"""

              let multiPeek = WorkspacePeek.peek root 2 []
              Expect.equal
                  (WorkspacePeek.projectsOf multiPeek |> Set.ofList)
                  (set [ projSlnxOnly; projSlnOnly; projExtra ])
                  "Multiple unrelated solutions fall back to directory .fsproj discovery"
          }

          testTask "Server emits ordered Items in notifyWorkspace & fsharp/project, ignores None/Content content edits, and swaps Entry.Spec on disk Created/Deleted without recreating ProjectGraph" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              File.Copy(Path.Combine(findRepoRoot (), "global.json"), Path.Combine(root, "global.json"))
              let projDir = Path.Combine(root, "Svc")
              Directory.CreateDirectory projDir |> ignore
              let p (rel: string) = Path.GetFullPath(Path.Combine(projDir, rel.Replace('/', Path.DirectorySeparatorChar)))

              let readme = p "README.md"
              let domainFs = p "Domain.fs"
              let appSettings = p "appsettings.json"
              let docsExisting = p "docs/intro.md"
              let fsproj = p "Svc.fsproj"

              writeText readme "# Svc\n"
              writeText domainFs "module Domain\nlet answer = 42\n"
              writeText appSettings "{\"env\":\"dev\"}\n"
              writeText docsExisting "# Intro\n"

              writeText
                  fsproj
                  $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <DisableImplicitFSharpCoreReference>true</DisableImplicitFSharpCoreReference>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="FSharp.Core" Version="9.0.100" ExcludeAssets="all" />
    <Reference Include="FSharp.Core">
      <HintPath>{Environment.GetEnvironmentVariable "RFS_TEST_FCS_DIR" |> Option.ofObj |> Option.defaultValue @"C:\Users\shaya\RiderProjects\fickle-fsharp-i1\artifacts\bin\FSharp.Compiler.Service\Release\net10.0\"}FSharp.Core.dll</HintPath>
    </Reference>
  </ItemGroup>
  <ItemGroup>
    <None Include="README.md" />
    <Compile Include="Domain.fs" />
    <Compile Include="extra/*.fs" />
    <Content Include="appsettings.json" CopyToOutputDirectory="PreserveNewest" />
    <None Include="docs/**/*.md" LinkBase="Manual" />
  </ItemGroup>
</Project>"""

              let! (lsp: LspClient), (owner: IDisposable), (serverTask: Task<RanvierServer>) = TestServer.startCapturing ()
              use _ = disposing owner
              let workspaceNotices = ConcurrentQueue<JsonNode>()

              lsp.Notifications.Subscribe
                  { new IObserver<Received> with
                      member _.OnNext r =
                          if r.Message["method"].GetValue<string>() = "fsharp/notifyWorkspace" then
                              workspaceNotices.Enqueue(parseContent r.Message["params"])

                      member _.OnError _ = ()
                      member _.OnCompleted() = () }
              |> ignore

              let! _ = lsp.Initialize(Paths.toUri root, JsonNode.Parse "{}", watchers = true) |> within
              let! (server: RanvierServer) = serverTask |> within
              server.WatchDelayForTests <- TimeSpan.FromMilliseconds 25.

              let graphsCreated = ref 0
              server.Workspace.OnGraphCreated(fun _ -> Interlocked.Increment graphsCreated |> ignore)

              let loadReq = JsonNode.Parse($$"""{"TextDocuments":[{"Uri":{{JsonValue.Create(Paths.toUri fsproj).ToJsonString()}}}]}""")
              let! _ = lsp.Request("fsharp/workspaceLoad", loadReq) |> within

              let (graphBefore: ProjectGraph) = (server.Workspace.ProjectOf domainFs).Value
              Expect.equal graphsCreated.Value 1 "Initial cold load created 1 ProjectGraph"
              Expect.isEmpty (server.Workspace.OwnersOf readme) "None file is NEVER owned by any ProjectGraph"
              Expect.isEmpty (server.Workspace.OwnersOf appSettings) "Content file is NEVER owned by any ProjectGraph"

              // 1. Verify fsharp/project and initial fsharp/notifyWorkspace carry ordered Items + Metadata while Files is Compile-only
              let projReq = JsonNode.Parse($$"""{ "Project": { "Uri": {{JsonValue.Create(Paths.toUri fsproj).ToJsonString()}} } }""")
              let! (projReply: JsonNode), _ = lsp.Request("fsharp/project", projReq) |> within
              let projData = (parseContent projReply).["Data"]
              let filesList = [ for n in projData["Files"].AsArray() -> n.GetValue<string>() ]
              Expect.equal filesList [ domainFs ] "fsharp/project Files is strictly Compile-only"

              let itemsTuples (data: JsonNode) =
                  [ for n in data["Items"].AsArray() ->
                        n["Name"].GetValue<string>(),
                        n["FilePath"].GetValue<string>(),
                        n["VirtualPath"].GetValue<string>() ]

              Expect.equal
                  (itemsTuples projData)
                  [ "None", readme, "README.md"
                    "Compile", domainFs, "Domain.fs"
                    "Content", appSettings, "appsettings.json"
                    "None", docsExisting, "Manual/intro.md" ]
                  "fsharp/project Items has interleaved None, Compile, Content, and LinkBase None"

              let contentMeta = (projData["Items"].AsArray()).[2].["Metadata"]
              Expect.equal (contentMeta["CopyToOutputDirectory"].GetValue<string>()) "PreserveNewest" "Content Metadata dictionary populated"

              // 2. Content edits (didOpen, didChange, didClose, and watched Changed) on None & Content perform ZERO ProjectGraph / ProjectSpec work!
              let flightCount = ref 0
              graphBefore.OnFlight(fun _ _ _ -> Interlocked.Increment flightCount |> ignore)
              let noticesBeforeEdits = workspaceNotices.Count
              let! diskPassesBefore = server.Publisher.DiskPasses() |> within

              do! lsp.DidOpen(readme, "# Edited in memory\n")
              do!
                  lsp.DidChange(
                      readme,
                      1,
                      { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 0; Character = 19 } }
                        Text = "# Edited twice" }
                  )

              let closeReadme = JsonNode.Parse($$"""{ "textDocument": { "uri": {{JsonValue.Create(Paths.toUri readme).ToJsonString()}} } }""")
              do! lsp.Notify("textDocument/didClose", closeReadme)
              do! server.Publisher.Barrier() |> withinT

              writeText readme "# Edited on disk\n"
              writeText appSettings "{\"env\":\"prod\"}\n"
              let changedMsg =
                  JsonNode.Parse(
                      $$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri readme).ToJsonString()}},"type":2},{"uri":{{JsonValue.Create(Paths.toUri appSettings).ToJsonString()}},"type":2}]}"""
                  )
              do! lsp.Notify("workspace/didChangeWatchedFiles", changedMsg)
              do! Task.Delay 150

              let! diskPassesAfterEdits = server.Publisher.DiskPasses() |> within
              Expect.equal flightCount.Value 0 "Zero parse/check flights triggered by None/Content edits"
              Expect.equal graphsCreated.Value 1 "Zero ProjectGraph recreations on None/Content edits"
              Expect.equal workspaceNotices.Count noticesBeforeEdits "Zero notifyWorkspace emissions on None/Content content edits"
              Expect.equal diskPassesAfterEdits diskPassesBefore "Zero Publisher.DiskPasses on None/Content Changed events"
              Expect.isTrue (obj.ReferenceEquals(graphBefore, (server.Workspace.ProjectOf domainFs).Value)) "Same ProjectGraph instance after edits"

              // 3. Disk file Creation (type = 1) of a None file matching docs/**/*.md swaps Entry.Spec and emits notifyWorkspace WITHOUT recreating ProjectGraph!
              let newGuide = p "docs/guide.md"
              writeText newGuide "# New Guide\n"
              let createdMsg =
                  JsonNode.Parse($$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri newGuide).ToJsonString()}},"type":1}]}""")
              do! lsp.Notify("workspace/didChangeWatchedFiles", createdMsg)

              let waitNotice countBefore =
                  task {
                      let deadline = DateTime.UtcNow.AddSeconds 30.
                      while workspaceNotices.Count <= countBefore && DateTime.UtcNow < deadline do
                          do! Task.Delay 25
                      Expect.isGreaterThan workspaceNotices.Count countBefore "Expected fsharp/notifyWorkspace after None file Created/Deleted"
                  }

              do! waitNotice noticesBeforeEdits
              let graphAfterCreate = (server.Workspace.ProjectOf domainFs).Value
              let! diskPassesAfterCreate = server.Publisher.DiskPasses() |> within
              Expect.isTrue (obj.ReferenceEquals(graphBefore, graphAfterCreate)) "ProjectGraph instance preserved across None file creation!"
              Expect.equal graphsCreated.Value 1 "Still 1 ProjectGraph created total"
              Expect.equal flightCount.Value 0 "Zero FCS flights during graph-preserving Spec swap on Created!"
              Expect.equal diskPassesAfterCreate diskPassesBefore "Zero Publisher.DiskPasses during None file Created!"

              let lastCreateNotice = workspaceNotices.ToArray() |> Array.last
              Expect.equal (lastCreateNotice["Kind"].GetValue<string>()) "project" "Emitted Kind=project notifyWorkspace"
              Expect.contains (itemsTuples lastCreateNotice["Data"]) ("None", newGuide, "Manual/guide.md") "notifyWorkspace contains newly created Manual/guide.md"

              let! (projAfterCreate: JsonNode), _ = lsp.Request("fsharp/project", projReq.DeepClone()) |> within
              let afterCreateData = (parseContent projAfterCreate).["Data"]
              Expect.contains (itemsTuples afterCreateData) ("None", newGuide, "Manual/guide.md") "fsharp/project reflects newly created Manual/guide.md"

              // 4. Disk file Deletion (type = 3) of docs/guide.md swaps Entry.Spec and removes item WITHOUT recreating ProjectGraph!
              let noticesBeforeDelete = workspaceNotices.Count
              File.Delete newGuide
              let deletedMsg =
                  JsonNode.Parse($$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri newGuide).ToJsonString()}},"type":3}]}""")
              do! lsp.Notify("workspace/didChangeWatchedFiles", deletedMsg)
              do! waitNotice noticesBeforeDelete

              let graphAfterDelete = (server.Workspace.ProjectOf domainFs).Value
              let! diskPassesAfterDelete = server.Publisher.DiskPasses() |> within
              Expect.isTrue (obj.ReferenceEquals(graphBefore, graphAfterDelete)) "ProjectGraph instance preserved across None file deletion!"
              Expect.equal graphsCreated.Value 1 "Still 1 ProjectGraph created total after deletion"
              Expect.equal flightCount.Value 0 "Zero FCS flights during graph-preserving Spec swap on Deleted!"
              Expect.equal diskPassesAfterDelete diskPassesBefore "Zero Publisher.DiskPasses during None file Deleted!"

              let lastDeleteNotice = workspaceNotices.ToArray() |> Array.last
              Expect.isFalse
                  (itemsTuples lastDeleteNotice["Data"] |> List.exists (fun (_, fp, _) -> String.Equals(fp, newGuide, StringComparison.OrdinalIgnoreCase)))
                  "Deleted Manual/guide.md removed from notifyWorkspace Items"

              // 5. Creating a new .fs file matching <Compile Include="extra/*.fs" /> DOES change SourceFiles and triggers full reload!
              let addedFs = p "extra/Added.fs"
              writeText addedFs "module Added\nlet y = Domain.answer + 1\n"
              let createdCompileMsg =
                  JsonNode.Parse($$"""{"changes":[{"uri":{{JsonValue.Create(Paths.toUri addedFs).ToJsonString()}},"type":1}]}""")
              let noticesBeforeReload = workspaceNotices.Count
              do! lsp.Notify("workspace/didChangeWatchedFiles", createdCompileMsg)

              let deadlineReload = DateTime.UtcNow.AddSeconds 60.
              while (graphsCreated.Value < 2 || (server.Workspace.OwnersOf addedFs).IsEmpty || server.Workspace.IsLoading)
                    && DateTime.UtcNow < deadlineReload do
                  do! Task.Delay 50

              do! server.Publisher.Barrier() |> withinT

              Expect.equal graphsCreated.Value 2 "Wildcard Compile creation changed SourceFiles and triggered full reload"
              Expect.equal (server.Workspace.OwnersOf addedFs).Length 1 "Newly created wildcard Compile file is owned by reloaded ProjectGraph"
              Expect.isGreaterThan workspaceNotices.Count noticesBeforeReload "Reload emitted workspace notifications"
          } ]
