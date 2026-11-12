module Rfs.Server.Tests.CodeActionsAndFormattingTests

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 90.)
let private withinFast (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 10.)
let private withinUnit (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 90.)

let private disposing (d: IDisposable) =
    { new IAsyncDisposable with
        member _.DisposeAsync() =
            d.Dispose()
            ValueTask() }

let private sameFile (a: string) (b: string) =
    if String.IsNullOrWhiteSpace a || String.IsNullOrWhiteSpace b then
        false
    else
        String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

let private findPos (source: string) (needle: string) (nth: int) (inside: int) : int * int =
    let mutable idx = -1

    for _ in 0..nth do
        idx <- source.IndexOf(needle, idx + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual idx 0 $"Expected '%s{needle}' occurrence %d{nth} in source:\n%s{source}"
    let target = idx + inside
    let before = source.Substring(0, target)
    let lines = before.Split('\n')
    let line = lines.Length - 1
    let col = lines[lines.Length - 1].TrimEnd('\r').Length
    line, col

let private mkRange (sl: int) (sc: int) (el: int) (ec: int) : Ionide.LanguageServerProtocol.Types.Range =
    { Start = { Line = uint32 sl; Character = uint32 sc }
      End = { Line = uint32 el; Character = uint32 ec } }

let private posRange (line: int) (col: int) : Ionide.LanguageServerProtocol.Types.Range =
    mkRange line col line col

let private applyLspEdits (source: string) (edits: TextEdit[]) : string =
    let sorted =
        edits
        |> Array.sortByDescending (fun e ->
            int e.Range.Start.Line,
            int e.Range.Start.Character,
            int e.Range.End.Line,
            int e.Range.End.Character)

    let mutable current = source

    for e in sorted do
        let edit: Rfs.Lsp.Edit =
            { Range =
                { Start = { Line = int e.Range.Start.Line; Character = int e.Range.Start.Character }
                  End = { Line = int e.Range.End.Line; Character = int e.Range.End.Character } }
              Text = e.NewText }

        current <- Rfs.Lsp.Document.apply current edit

    current

let private editsByPath (wsEdit: WorkspaceEdit) : Map<string, int option * TextEdit[]> =
    match wsEdit.DocumentChanges with
    | Some docChanges when docChanges.Length > 0 ->
        docChanges
        |> Seq.choose (function
            | U4.C1 tde ->
                let edits =
                    tde.Edits
                    |> Array.map (function
                        | U2.C1 te -> te
                        | U2.C2 ate -> { Range = ate.Range; NewText = ate.NewText })

                Some(LspClient.NormalizePath(Paths.ofUri tde.TextDocument.Uri), (tde.TextDocument.Version, edits))
            | _ -> None)
        |> Map.ofSeq
    | _ ->
        match wsEdit.Changes with
        | Some m ->
            m
            |> Seq.map (fun kv -> LspClient.NormalizePath(Paths.ofUri kv.Key), (None, kv.Value))
            |> Map.ofSeq
        | None -> Map.empty

let private applySingleFileAction (path: string) (source: string) (action: CodeAction) : string =
    Expect.isSome action.Edit $"Expected CodeAction '%s{action.Title}' to carry a WorkspaceEdit"
    let byPath = editsByPath action.Edit.Value
    let norm = LspClient.NormalizePath path

    match byPath |> Map.tryFind norm with
    | None -> failtest $"Expected edits for '%s{norm}' in CodeAction '%s{action.Title}', got keys: %A{byPath |> Map.keys |> List.ofSeq}"
    | Some(_, edits) -> applyLspEdits source edits

let private extractActions (resOpt: TextDocumentCodeActionResult option) : CodeAction[] =
    match resOpt with
    | None -> [||]
    | Some arr ->
        arr
        |> Array.choose (function
            | U2.C2 ca -> Some ca
            | U2.C1 _ -> None)

let private findAction (titleSubstr: string) (actions: CodeAction[]) : CodeAction =
    match actions |> Array.tryFind (fun a -> a.Title.Equals(titleSubstr, StringComparison.Ordinal)) with
    | Some exact -> exact
    | None ->
        match actions |> Array.tryFind (fun a -> a.Title.Contains(titleSubstr, StringComparison.Ordinal)) with
        | Some hit -> hit
        | None ->
            let titles = actions |> Array.map _.Title |> String.concat ", "
            failtest $"Expected CodeAction matching '%s{titleSubstr}', got: [%s{titles}]"

let private mkParams (path: string) (range: Ionide.LanguageServerProtocol.Types.Range) (diags: Ionide.LanguageServerProtocol.Types.Diagnostic[]) (only: string[] option) : CodeActionParams =
    { TextDocument = { Uri = Paths.toUri path }
      Range = range
      Context =
        { Diagnostics = diags
          Only = only
          TriggerKind = None }
      WorkDoneToken = None
      PartialResultToken = None }

let private assertNoCompilerRetention (label: string) (root: obj) =
    let seen = HashSet<obj>(HashIdentity.Reference)

    let rec walk (path: string) (o: obj) =
        if not (isNull o) && seen.Add o then
            let t = o.GetType()

            if
                not t.IsArray
                && t.Namespace <> null
                && (t.Namespace.StartsWith("FSharp.Compiler", StringComparison.Ordinal)
                    || t.FullName = "Rfs.Checker.FileView"
                    || t.FullName = "Rfs.Checker.TextView"
                    || t.FullName = "Rfs.Checker.FileCheck")
            then
                failtest $"%s{label} retains forbidden compiler/view object %s{t.FullName} at %s{path}"

            match o with
            | :? string
            | :? Type
            | :? Assembly
            | :? Task
            | :? CancellationTokenSource
            | :? WaitHandle
            | :? Ranvier.Graph
            | :? Rfs.Server.Documents
            | :? Rfs.Server.Workspace
            | :? Rfs.Server.RanvierClient
            | :? Rfs.Server.RanvierServer -> ()
            | _ when obj.ReferenceEquals(t.Assembly, typeof<Rfs.Checker.ProjectGraph>.Assembly) -> ()
            | :? Delegate as d ->
                if
                    not (isNull d.Target)
                    && d.Target.GetType().Namespace <> null
                    && d.Target.GetType().Namespace.StartsWith("Rfs.Server", StringComparison.Ordinal)
                then
                    walk $"%s{path}.Target" d.Target
            | :? Newtonsoft.Json.Linq.JToken -> ()
            | :? Array as a when not (t.GetElementType().IsPrimitive) ->
                let mutable idx = 0

                for elem in a do
                    walk $"%s{path}[%d{idx}]" elem
                    idx <- idx + 1
            | :? System.Collections.IEnumerable as seqObj when t.Namespace <> null && t.Namespace.StartsWith("System.Collections", StringComparison.Ordinal) ->
                let mutable idx = 0

                for elem in seqObj do
                    walk $"%s{path}[%d{idx}]" elem
                    idx <- idx + 1
            | _ when t.IsPrimitive || t.IsEnum -> ()
            | _ ->
                let isRanvier = obj.ReferenceEquals(t.Assembly, typeof<Ranvier.Graph>.Assembly)

                for f in t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) do
                    let skipRanvierEdge =
                        isRanvier
                        && (f.Name = "sources"
                            || f.Name = "observers"
                            || f.Name = "parentLink"
                            || f.Name = "sink"
                            || f.Name = "owner"
                            || f.Name = "scope"
                            || f.Name = "traceLog")

                    if not f.FieldType.IsPointer && not skipRanvierEdge then
                        let v =
                            try
                                f.GetValue o
                            with _ ->
                                null

                        walk $"%s{path}.%s{f.Name}" v

    walk label root

let private withSingleProject
    (promote: bool)
    (extraFlags: string list)
    (files: (string * string) list)
    (body: GraphHost -> ProjectGraph -> (string -> string) -> References.Scope -> (string -> int option) -> Task<unit>)
    : Task<unit> =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-task7-"

        try
            let filePath (name: string) = Path.Combine(dir.FullName, name)

            for name, text in files do
                File.WriteAllText(filePath name, text)

            let paths = files |> List.map (fst >> filePath)
            let! deps = ProjectSpec.load PublishTests.depsProject
            let seqSpec = ProjectSpec.sequential deps

            let spec =
                { seqSpec with
                    ProjectFile = filePath "Task7.fsproj"
                    SourceFiles = paths
                    GeneratedFiles = []
                    OtherOptions = seqSpec.OtherOptions @ extraFlags }

            use host = GraphHost.Start "task7"
            let! graph = ProjectGraph.Create(host, spec)

            if promote then
                for p in paths do
                    if not (p.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then
                        graph.Promote p

            let! _ = graph.AllDiagnostics() |> within

            if promote then
                for p in paths do
                    if not (p.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then
                        let! _ = graph.WhenPromoted(p, CancellationToken.None) |> within
                        ()

            let! graphThreadId = host.Read(fun () -> Thread.CurrentThread.ManagedThreadId) |> within

            let assertGraphThread (memberName: string) =
                if Thread.CurrentThread.ManagedThreadId <> graphThreadId then
                    failtest
                        $"%s{memberName} called off GraphHost thread (actual=%d{Thread.CurrentThread.ManagedThreadId}, expected=%d{graphThreadId})"

            let scope: References.Scope =
                { OwnersOf =
                    fun f ->
                        assertGraphThread "Scope.OwnersOf"
                        if paths |> List.exists (sameFile f) then [ graph ] else []
                  ReferencingProjects =
                    fun _ ->
                        assertGraphThread "Scope.ReferencingProjects"
                        []
                  Projects =
                    fun () ->
                        assertGraphThread "Scope.Projects"
                        [ graph ]
                  FilesOf =
                    fun _ ->
                        assertGraphThread "Scope.FilesOf"
                        paths
                  IsLoading =
                    fun () ->
                        assertGraphThread "Scope.IsLoading"
                        false }

            let versionNow (f: string) =
                assertGraphThread "Documents.TryVersionNow"

                match paths |> List.tryFindIndex (sameFile f) with
                | Some idx -> Some(idx + 7)
                | None -> None

            do! body host graph filePath scope versionNow
            graph.Dispose()
        finally
            try
                dir.Delete true
            with _ ->
                ()
    }

let private enabledConfig =
    { FSharpConfig.Default with
        UnusedOpensAnalyzer = true
        UnusedDeclarationsAnalyzer = true
        SimplifyNameAnalyzer = true
        ResolveNamespaces = true
        UnionCaseStubGeneration = true
        UnionCaseStubGenerationBody = "failwith \"Not Implemented\""
        RecordStubGeneration = true
        RecordStubGenerationBody = "failwith \"Not Implemented\""
        InterfaceStubGeneration = true
        InterfaceStubGenerationObjectIdentifier = "self"
        InterfaceStubGenerationMethodBody = "failwith \"todo\""
        AbstractClassStubGeneration = true
        AbstractClassStubGenerationObjectIdentifier = "this"
        AbstractClassStubGenerationMethodBody = "failwith \"Not Implemented\""
        IndentationSize = 4 }

let private task7Sequenced = testSequencedGroup "Task7Suite"

[<Tests>]
let publisherAndCapabilitiesWireTests =
    task7Sequenced
    <| testList
        "Task7.PublisherTagsAndAnalyzers"
        [ testTask "Initialize advertises CodeActionProvider, DocumentFormattingProvider, and DocumentRangeFormattingProvider" {
              let! captured = TestServer.startCapturing ()
              let (lsp: LspClient), owner, _ = captured
              use _ = disposing owner

              let initParams =
                  JsonNode.Parse(
                      $"""{{"processId":{Environment.ProcessId},"rootUri":"{Paths.toUri PublishTests.depsDir}","capabilities":{{}}}}"""
                  )

              let! initPair = lsp.Request("initialize", initParams) |> within
              let initResult: JsonNode = fst initPair
              let caps = initResult["capabilities"]
              Expect.isNotNull caps["codeActionProvider"] "codeActionProvider advertised"
              let kinds = caps["codeActionProvider"].["codeActionKinds"].AsArray() |> Seq.map (fun n -> n.GetValue<string>()) |> Set.ofSeq
              Expect.contains kinds "quickfix" "quickfix advertised"
              Expect.contains kinds "refactor" "refactor advertised"
              Expect.contains kinds "refactor.rewrite" "refactor.rewrite advertised"
              Expect.contains kinds "source.organizeImports" "source.organizeImports advertised"
              Expect.isTrue (caps["documentFormattingProvider"].GetValue<bool>()) "documentFormattingProvider advertised"
              Expect.isTrue (caps["documentRangeFormattingProvider"].GetValue<bool>()) "documentRangeFormattingProvider advertised"
          }

          testTask "Publisher emits DiagnosticTag.Unnecessary on FS1182, DiagnosticTag.Deprecated on FS0044, and toggles FSAC0001/0002/0003 via didChangeConfiguration with zero compiler retention" {
              let dir = Directory.CreateTempSubdirectory "rfs-task7-pub-"

              try
                  let projFile = Path.Combine(dir.FullName, "PubProj.fsproj")
                  let srcFile = Path.Combine(dir.FullName, "Mod.fs")

                  let projXml =
                      """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <WarnOn>1182</WarnOn>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Mod.fs" />
  </ItemGroup>
</Project>
"""

                  let srcText =
                      """module PubProj.Mod

open System
open System.Text

[<Obsolete("use newApi")>]
let legacyApi () = 10

let run (unusedArg: int) =
    let unusedLocal = 99
    let dt = System.DateTime.MinValue
    legacyApi () + dt.Year
"""

                  File.WriteAllText(projFile, projXml)
                  File.WriteAllText(srcFile, srcText)

                  File.Copy(Path.Combine(PublishFixTests.repoRoot, "global.json"), Path.Combine(dir.FullName, "global.json"), true)
                  let restorePsi = ProcessStartInfo("dotnet", $"restore \"{projFile}\" --disable-build-servers")
                  restorePsi.WorkingDirectory <- dir.FullName
                  restorePsi.UseShellExecute <- false
                  restorePsi.CreateNoWindow <- true
                  restorePsi.RedirectStandardOutput <- true
                  restorePsi.RedirectStandardError <- true
                  restorePsi.Environment.Remove "MSBUILD_EXE_PATH" |> ignore
                  restorePsi.Environment.Remove "MSBuildExtensionsPath" |> ignore
                  restorePsi.Environment.Remove "MSBuildSDKsPath" |> ignore
                  restorePsi.Environment.Remove "DOTNET_HOST_PATH" |> ignore
                  restorePsi.Environment["DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER"] <- "1"
                  restorePsi.Environment["MSBUILDDISABLENODEREUSE"] <- "1"
                  let restoreProc = Process.Start restorePsi
                  use _restoreDisp = disposing restoreProc
                  do! restoreProc.WaitForExitAsync() |> withinUnit
                  Expect.equal restoreProc.ExitCode 0 "dotnet restore succeeded"

                  let! captured = TestServer.startCapturing ()
                  let (lsp: LspClient), owner, serverTask = captured
                  use _ = disposing owner
                  let! runningRaw = serverTask
                  let running: RanvierServer = runningRaw

                  let rawPublishes = ConcurrentQueue<JsonObject>()

                  use _sub =
                      disposing (
                          lsp.Notifications.Subscribe
                              { new IObserver<Received> with
                                  member _.OnNext r =
                                      if
                                          not (isNull r.Message["method"])
                                          && r.Message["method"].GetValue<string>() = "textDocument/publishDiagnostics"
                                      then
                                          rawPublishes.Enqueue(r.Message["params"].AsObject())

                                  member _.OnError _ = ()
                                  member _.OnCompleted() = () }
                      )

                  let! _ = lsp.Initialize(Paths.toUri dir.FullName, JsonNode.Parse "{}") |> within

                  let loadParams =
                      JsonNode.Parse($"""{{"TextDocuments":[{{"Uri":"{Paths.toUri projFile}"}}]}}""")

                  let! _ = lsp.Request("fsharp/workspaceLoad", loadParams) |> within
                  do! lsp.DidOpen(srcFile, srcText)

                  let latestDiags () =
                      let arr = rawPublishes.ToArray()

                      arr
                      |> Array.tryFindBack (fun p -> sameFile (Paths.ofUri (p["uri"].GetValue<string>())) srcFile)
                      |> Option.map (fun p -> p["diagnostics"].AsArray())

                  let waitForPublish (desc: string) (pred: JsonArray -> bool) =
                      task {
                          let deadline = DateTime.UtcNow.AddSeconds 30.

                          while not (latestDiags () |> Option.exists pred) do
                              if DateTime.UtcNow > deadline then
                                  let dump =
                                      latestDiags ()
                                      |> Option.map (fun a -> a.ToJsonString())
                                      |> Option.defaultValue "<none>"

                                  failtest $"Timed out waiting for %s{desc}. Latest diagnostics: %s{dump}"

                              do! Task.Delay 25

                          return (latestDiags ()).Value
                      }

                  let! initialDiags =
                      waitForPublish "initial FS1182 & FS0044 diagnostics" (fun arr ->
                          arr |> Seq.exists (fun d -> d["code"].GetValue<string>() = "FS1182")
                          && arr |> Seq.exists (fun d -> d["code"].GetValue<string>() = "FS0044"))

                  let tagsOf (code: string) (arr: JsonArray) =
                      arr
                      |> Seq.filter (fun d -> not (isNull d["code"]) && d["code"].GetValue<string>() = code)
                      |> Seq.map (fun d ->
                          match d["tags"] with
                          | null -> []
                          | t -> t.AsArray() |> Seq.map (fun x -> x.GetValue<int>()) |> List.ofSeq)
                      |> List.ofSeq

                  Expect.isNonEmpty (tagsOf "FS1182" initialDiags) "FS1182 present"
                  Expect.allEqual (tagsOf "FS1182" initialDiags) [ 1 ] "FS1182 carries DiagnosticTag.Unnecessary (1)"
                  Expect.equal (tagsOf "FS0044" initialDiags) [ [ 2 ] ] "FS0044 carries DiagnosticTag.Deprecated (2)"

                  let hasAnalyzerDiag (arr: JsonArray) =
                      arr
                      |> Seq.exists (fun d ->
                          not (isNull d["code"])
                          && d["code"].GetValue<string>().StartsWith("FSAC", StringComparison.Ordinal))

                  Expect.isFalse (hasAnalyzerDiag initialDiags) "Built-in FSAC0001/0002/0003 analyzers are OFF by default"

                  // Enable all 3 built-in open-file analyzers via workspace/didChangeConfiguration
                  do!
                      lsp.Configure(
                          JsonNode.Parse
                              """{"FSharp":{"unusedOpensAnalyzer":true,"simplifyNameAnalyzer":true,"unusedDeclarationsAnalyzer":true}}"""
                      )

                  let! analyzedDiagsRaw =
                      waitForPublish "FSAC0001, FSAC0002, and FSAC0003 diagnostics" (fun arr ->
                          arr |> Seq.exists (fun d -> d["code"].GetValue<string>() = "FSAC0001")
                          && arr |> Seq.exists (fun d -> d["code"].GetValue<string>() = "FSAC0002")
                          && arr |> Seq.exists (fun d -> d["code"].GetValue<string>() = "FSAC0003"))

                  let analyzedDiags: JsonArray = analyzedDiagsRaw

                  let expectedAnalyzers =
                      [ "FSAC0001", "Unused open statement"
                        "FSAC0002", "This qualifier is redundant"
                        "FSAC0003", "This value is unused" ]

                  expectedAnalyzers
                  |> List.iter (fun (code, expectedMsg) ->
                      let matches =
                          analyzedDiags
                          |> Seq.filter (fun d -> d["code"].GetValue<string>() = code)
                          |> List.ofSeq

                      Expect.isNonEmpty matches $"Expected %s{code} diagnostic"

                      matches
                      |> List.iter (fun d ->
                          Expect.equal (d["severity"].GetValue<int>()) 4 $"%s{code} has Hint severity (4)"
                          Expect.equal (d["source"].GetValue<string>()) "F# Analyzers" $"%s{code} has source 'F# Analyzers'"
                          Expect.equal (d["message"].GetValue<string>()) expectedMsg $"%s{code} message"
                          let tags = d["tags"].AsArray() |> Seq.map (fun x -> x.GetValue<int>()) |> List.ofSeq
                          Expect.equal tags [ 1 ] $"%s{code} carries DiagnosticTag.Unnecessary (1)"))

                  // Verify ZERO FSharp.Compiler.* or FileView retention in Publisher on the GraphHost thread
                  do!
                      running.Host.Read(fun () -> assertNoCompilerRetention "Publisher" running.Publisher)
                      |> withinUnit

                  // Disable analyzers via workspace/didChangeConfiguration -> FSAC0001/0002/0003 clear cleanly
                  do!
                      lsp.Configure(
                          JsonNode.Parse
                              """{"FSharp":{"unusedOpensAnalyzer":false,"simplifyNameAnalyzer":false,"unusedDeclarationsAnalyzer":false}}"""
                      )

                  let! clearedDiags =
                      waitForPublish "FSAC000* cleared after disabling analyzers" (fun arr -> not (hasAnalyzerDiag arr))

                  Expect.isFalse (hasAnalyzerDiag clearedDiags) "FSAC0001/0002/0003 cleared when disabled"
                  Expect.isNonEmpty (tagsOf "FS1182" clearedDiags) "Compiler FS1182 remains published"
                  do! lsp.Shutdown() |> withinUnit
              finally
                  try
                      dir.Delete true
                  with _ ->
                      ()
          } ]

[<Tests>]
let codeActionFamiliesTests =
    task7Sequenced
    <| testList
        "Task7.CodeActions12Families"
        [ testTask "Family 1: Remove unused open (single QuickFix + source.organizeImports remove-all) in promoted mode" {
              let src =
                  "module Task7.Fam1\n\nopen System\nopen System.IO\nopen System.Text\n\nlet now () = DateTime.UtcNow\nlet second = 2\n"

              do!
                  withSingleProject true [] [ "Fam1.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam1.fs"
                          let lineIo, colIo = findPos src "open System.IO" 0 5

                          let! res =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lineIo colIo) [||] None)
                                  CancellationToken.None
                              |> within

                          let actions = extractActions res
                          let single = findAction "Remove unused open" actions
                          Expect.equal single.Kind (Some CodeActionKind.QuickFix) "Single unused open is QuickFix"
                          let afterSingle = applySingleFileAction file src single

                          Expect.equal
                              afterSingle
                              "module Task7.Fam1\n\nopen System\nopen System.Text\n\nlet now () = DateTime.UtcNow\nlet second = 2\n"
                              "Deletes entire 'open System.IO' line including newline"

                          // Also verify versioned DocumentChanges carries versionNow (index 0 + 7 = 7)
                          let byPath = editsByPath single.Edit.Value
                          Expect.equal (fst byPath[LspClient.NormalizePath file]) (Some 7) "Captures in-turn document version"

                          // Verify source.organizeImports removes ALL unused opens while keeping used `open System`
                          let! orgRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange 0 0) [||] (Some [| "source.organizeImports" |]))
                                  CancellationToken.None
                              |> within

                          let orgActions = extractActions orgRes
                          let removeAll = findAction "Remove all unused opens" orgActions
                          Expect.equal removeAll.Kind (Some "source.organizeImports") "Organize imports kind"
                          let afterAll = applySingleFileAction file src removeAll

                          Expect.equal
                              afterAll
                              "module Task7.Fam1\n\nopen System\n\nlet now () = DateTime.UtcNow\nlet second = 2\n"
                              "Removes both unused opens cleanly"
                      })
          }

          testTask "Family 2: Unused value (FS1182 / FSAC0003) emits both 'Replace with _' and 'Prefix with _'" {
              let src =
                  "module Task7.Fam2\n\nlet compute () =\n    let unusedItem = 42\n    let ``unused binding`` = 7\n    100\n\nlet helper = 1\n"

              do!
                  withSingleProject true [ "--warnon:1182" ] [ "Fam2.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam2.fs"
                          let l, c = findPos src "unusedItem" 0 2

                          let! res =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange l c) [||] None)
                                  CancellationToken.None
                              |> within

                          let actions = extractActions res
                          let replaceAct = findAction "Replace with _" actions
                          let prefixAct = findAction "Prefix with _" actions

                          Expect.equal
                              (applySingleFileAction file src replaceAct)
                              "module Task7.Fam2\n\nlet compute () =\n    let _ = 42\n    let ``unused binding`` = 7\n    100\n\nlet helper = 1\n"
                              "Replace with _ produces valid discard binding"

                          Expect.equal
                              (applySingleFileAction file src prefixAct)
                              "module Task7.Fam2\n\nlet compute () =\n    let _unusedItem = 42\n    let ``unused binding`` = 7\n    100\n\nlet helper = 1\n"
                              "Prefix with _ prefixes identifier with underscore"

                          let bl, bc = findPos src "``unused binding``" 0 4

                          let! bRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange bl bc) [||] None)
                                  CancellationToken.None
                              |> within

                          let bPrefixAct = findAction "Prefix with _" (extractActions bRes)

                          Expect.equal
                              (applySingleFileAction file src bPrefixAct)
                              "module Task7.Fam2\n\nlet compute () =\n    let unusedItem = 42\n    let ``_unused binding`` = 7\n    100\n\nlet helper = 1\n"
                              "Prefix with _ places underscore inside double backticks"
                      })
          }

          testTask "Family 3: Simplify redundant qualified name (FSAC0002) strips redundant prefix" {
              let src =
                  "module Task7.Fam3\n\nopen System\n\nlet epoch () = System.DateTime.UnixEpoch\nlet extra = 1\n"

              do!
                  withSingleProject true [] [ "Fam3.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam3.fs"
                          let l, c = findPos src "System.DateTime" 0 2

                          let! res =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange l c) [||] None)
                                  CancellationToken.None
                              |> within

                          let actions = extractActions res
                          let simplify = findAction "Remove redundant qualifier" actions

                          Expect.equal
                              (applySingleFileAction file src simplify)
                              "module Task7.Fam3\n\nopen System\n\nlet epoch () = DateTime.UnixEpoch\nlet extra = 1\n"
                              "Strips redundant 'System.' qualifier"
                      })
          }

          testTask "Family 4: Resolve FS0039 — (4a) spellcheck suggestions and (4b) unopened namespace open + fully-qualified fixes with config gating" {
              let libSrc =
                  "namespace Acme.Widgets\n\nmodule Helpers =\n    let buildWidget (n: int) = n + 1\n"

              let appSrc =
                  "module Task7.Fam4\n\nopen System\n\nlet targetCount = 10\nlet badSpell = targetCoun + 1\nlet makeBuf () = StringBuilder()\nlet useHelper () = buildWidget 5\n"

              do!
                  withSingleProject true [] [ "Lib.fs", libSrc; "App.fs", appSrc ] (fun _ graph path scope versionNow ->
                      task {
                          let appFile = path "App.fs"

                          // (4a) Spellcheck suggestion works even with FSharpConfig.Default (ResolveNamespaces = false)
                          let ls, cs = findPos appSrc "targetCoun + 1" 0 3

                          let! spellRes =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  appFile
                                  (mkParams appFile (posRange ls cs) [||] None)
                                  CancellationToken.None
                              |> within

                          let spellAct = findAction "Replace with 'targetCount'" (extractActions spellRes)

                          Expect.equal
                              (applySingleFileAction appFile appSrc spellAct)
                              (appSrc.Replace("targetCoun + 1", "targetCount + 1"))
                              "Spellcheck suggestion replaces identifier"

                          // (4b) Gating check: when ResolveNamespaces = false, StringBuilder() produces no 'open System.Text'
                          let lb, cb = findPos appSrc "StringBuilder" 0 3

                          let! gatedRes =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  appFile
                                  (mkParams appFile (posRange lb cb) [||] None)
                                  CancellationToken.None
                              |> within

                          Expect.isFalse
                              (extractActions gatedRes |> Array.exists (fun a -> a.Title = "open System.Text"))
                              "ResolveNamespaces = false gates unopened namespace fixes"

                          // Enabled ResolveNamespaces = true on BCL type StringBuilder
                          let! nsRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  appFile
                                  (mkParams appFile (posRange lb cb) [||] None)
                                  CancellationToken.None
                              |> within

                          let nsActions = extractActions nsRes
                          let openSysText = findAction "open System.Text" nsActions
                          let fqSysText = findAction "System.Text.StringBuilder" nsActions

                          Expect.equal
                              (applySingleFileAction appFile appSrc openSysText)
                              "module Task7.Fam4\n\nopen System\nopen System.Text\n\nlet targetCount = 10\nlet badSpell = targetCoun + 1\nlet makeBuf () = StringBuilder()\nlet useHelper () = buildWidget 5\n"
                              "Inserts 'open System.Text' right after existing 'open System' with matching indentation"

                          Expect.equal
                              (applySingleFileAction appFile appSrc fqSysText)
                              (appSrc.Replace("StringBuilder()", "System.Text.StringBuilder()"))
                              "Qualifies 'System.Text.StringBuilder' in place"

                          // Also verify project-defined module value resolution (`Acme.Widgets.Helpers.buildWidget`)
                          let lh, ch = findPos appSrc "buildWidget" 0 3

                          let! helperRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  appFile
                                  (mkParams appFile (posRange lh ch) [||] None)
                                  CancellationToken.None
                              |> within

                          let helperActions = extractActions helperRes
                          let openHelpers = findAction "open Acme.Widgets.Helpers" helperActions
                          let fqHelpers = findAction "Acme.Widgets.Helpers.buildWidget" helperActions

                          Expect.isTrue
                              ((applySingleFileAction appFile appSrc openHelpers).Contains("open Acme.Widgets.Helpers\n"))
                              "Opens project module Acme.Widgets.Helpers"

                          Expect.equal
                              (applySingleFileAction appFile appSrc fqHelpers)
                              (appSrc.Replace("buildWidget 5", "Acme.Widgets.Helpers.buildWidget 5"))
                              "Qualifies project module value Acme.Widgets.Helpers.buildWidget"
                      })
          }

          testTask "Family 5: Add 'rec' keyword on self-referencing let binding" {
              let src =
                  "module Task7.Fam5\n\nlet factorial n =\n    if n <= 1 then 1\n    else n * factorial (n - 1)\n\nlet other = 0\n"

              do!
                  withSingleProject true [] [ "Fam5.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam5.fs"
                          let l, c = findPos src "factorial" 1 3

                          let! res =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange l c) [||] None)
                                  CancellationToken.None
                              |> within

                          let recAct = findAction "Make 'factorial' recursive" (extractActions res)

                          Expect.equal
                              (applySingleFileAction file src recAct)
                              "module Task7.Fam5\n\nlet rec factorial n =\n    if n <= 1 then 1\n    else n * factorial (n - 1)\n\nlet other = 0\n"
                              "Inserts 'rec ' immediately after 'let '"
                      })
          }

          testTask "Family 6: Add 'new' keyword to IDisposable construction (FS0760) including precedence parens on chained member" {
              let src =
                  "module Task7.Fam6\n\nlet plain () = System.IO.MemoryStream()\nlet chained () = System.IO.MemoryStream().Length\n"

              do!
                  withSingleProject true [] [ "Fam6.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam6.fs"
                          let l1, c1 = findPos src "System.IO.MemoryStream()" 0 5

                          let! res1 =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange l1 c1) [||] None)
                                  CancellationToken.None
                              |> within

                          let act1 = findAction "Add 'new' keyword" (extractActions res1)

                          Expect.equal
                              (applySingleFileAction file src act1)
                              "module Task7.Fam6\n\nlet plain () = new System.IO.MemoryStream()\nlet chained () = System.IO.MemoryStream().Length\n"
                              "Inserts 'new ' before standalone IDisposable constructor"

                          let l2, c2 = findPos src "System.IO.MemoryStream().Length" 0 5

                          let! res2 =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange l2 c2) [||] None)
                                  CancellationToken.None
                              |> within

                          let act2 = findAction "Add 'new' keyword" (extractActions res2)

                          Expect.equal
                              (applySingleFileAction file src act2)
                              "module Task7.Fam6\n\nlet plain () = System.IO.MemoryStream()\nlet chained () = (new System.IO.MemoryStream()).Length\n"
                              "Wraps '(new ...)' when chained with '.Length'"
                      })
          }

          testTask "Family 7: Operator & syntax quick-fixes (FS0020 mutable '<-' vs '|> ignore', '==' -> '=', '!=' -> '<>', missing '=' in type)" {
              let srcFs20 =
                  "module Task7.Fam7A\n\nlet mutateCase () =\n    let mutable counter = 0\n    counter = 5\n    counter\n\nlet ignoreCase () =\n    1 + 2\n    ()\n"

              do!
                  withSingleProject true [] [ "Fam7A.fs", srcFs20 ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam7A.fs"
                          let lm, cm = findPos srcFs20 "counter = 5" 0 2

                          let! resMut =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lm cm) [||] None)
                                  CancellationToken.None
                              |> within

                          let mutActions = extractActions resMut
                          let mutAct = findAction "Use '<-' to mutate value" mutActions

                          Expect.equal
                              (applySingleFileAction file srcFs20 mutAct)
                              (srcFs20.Replace("counter = 5", "counter <- 5"))
                              "Replaces '=' with '<-' on mutable local"

                          let li, ci = findPos srcFs20 "1 + 2" 0 2

                          let! resIgn =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange li ci) [||] None)
                                  CancellationToken.None
                              |> within

                          let ignActions = extractActions resIgn
                          Expect.isFalse (ignActions |> Array.exists (fun a -> a.Title.Contains("<-"))) "Non-mutable FS0020 does not suggest '<-'"
                          let ignAct = findAction "Pipe to 'ignore'" ignActions

                          Expect.equal
                              (applySingleFileAction file srcFs20 ignAct)
                              (srcFs20.Replace("1 + 2", "1 + 2 |> ignore"))
                              "Appends ' |> ignore' on FS0020 expression"
                      })

              let srcOps =
                  "module Task7.Fam7B\n\nlet checkEq (a: int) (b: int) = a == b\nlet checkNeq (a: int) (b: int) = a != b\n"

              do!
                  withSingleProject false [] [ "Fam7B.fs", srcOps ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam7B.fs"
                          let leq, ceq = findPos srcOps "==" 0 1

                          let! resEq =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange leq ceq) [||] None)
                                  CancellationToken.None
                              |> within

                          let actEq = findAction "Replace '==' with '='" (extractActions resEq)
                          Expect.equal (applySingleFileAction file srcOps actEq) (srcOps.Replace("a == b", "a = b")) "Replaces '==' with '='"

                          let lneq, cneq = findPos srcOps "!=" 0 1

                          let! resNeq =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lneq cneq) [||] None)
                                  CancellationToken.None
                              |> within

                          let actNeq = findAction "Replace '!=' with '<>'" (extractActions resNeq)
                          Expect.equal (applySingleFileAction file srcOps actNeq) (srcOps.Replace("a != b", "a <> b")) "Replaces '!=' with '<>'"
                      })

              let srcTypeEq = "module Task7.Fam7C\n\ntype Boxed { Value: int }\n"

              do!
                  withSingleProject false [] [ "Fam7C.fs", srcTypeEq ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam7C.fs"
                          let lt, ct = findPos srcTypeEq "{ Value" 0 0

                          let! resType =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lt ct) [||] None)
                                  CancellationToken.None
                              |> within

                          let actType = findAction "Add '='" (extractActions resType)

                          Expect.equal
                              (applySingleFileAction file srcTypeEq actType)
                              "module Task7.Fam7C\n\ntype Boxed = { Value: int }\n"
                              "Inserts missing '=' in record type definition"
                      })
          }

          testTask "Family 8: Add explicit type annotation (Refactor) on unannotated let binding and bare parameter" {
              let src =
                  "module Task7.Fam8\n\nlet answer = 42\nlet increment x = x + 1\n"

              do!
                  withSingleProject true [] [ "Fam8.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam8.fs"
                          let lv, cv = findPos src "answer" 0 2

                          let! resVal =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lv cv) [||] None)
                                  CancellationToken.None
                              |> within

                          let actVal = findAction "Add explicit type annotation" (extractActions resVal)
                          Expect.equal actVal.Kind (Some CodeActionKind.Refactor) "Explicit type annotation is Refactor"

                          Expect.equal
                              (applySingleFileAction file src actVal)
                              "module Task7.Fam8\n\nlet answer: int = 42\nlet increment x = x + 1\n"
                              "Annotates unannotated value binding"

                          let lp, cp = findPos src "increment x" 0 10

                          let! resParam =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lp cp) [||] None)
                                  CancellationToken.None
                              |> within

                          let actParam = findAction "Add explicit type annotation" (extractActions resParam)

                          Expect.equal
                              (applySingleFileAction file src actParam)
                              "module Task7.Fam8\n\nlet answer = 42\nlet increment (x: int) = x + 1\n"
                              "Wraps bare parameter x as (x: int)"
                      })
          }

          testTask "Family 9: Implement interface & abstract class stubs honoring FSharpConfig settings and inserting 'with' when absent" {
              let src =
                  """module Task7.Fam9

type IWorker =
    abstract Compute: count: int -> string

type Worker() =
    interface IWorker

[<AbstractClass>]
type BaseRunner() =
    abstract Run: step: int -> int

let mkRunner () = { new BaseRunner() }
"""

              do!
                  withSingleProject true [] [ "Fam9.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam9.fs"
                          let li, ci = findPos src "interface IWorker" 0 12

                          // Gated off when InterfaceStubGeneration = false
                          let! gatedRes =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange li ci) [||] None)
                                  CancellationToken.None
                              |> within

                          Expect.isFalse
                              (extractActions gatedRes |> Array.exists (fun a -> a.Title = "Implement interface"))
                              "InterfaceStubGeneration = false gates interface stub"

                          // Enabled with custom ObjectIdentifier = "self" and MethodBody = "failwith \"todo\""
                          let! ifaceRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange li ci) [||] None)
                                  CancellationToken.None
                              |> within

                          let ifaceAct = findAction "Implement interface" (extractActions ifaceRes)
                          let afterIface = applySingleFileAction file src ifaceAct
                          Expect.stringContains afterIface "interface IWorker with" "Inserts missing 'with' keyword"
                          Expect.stringContains afterIface "member self.Compute" "Uses InterfaceStubGenerationObjectIdentifier 'self'"
                          Expect.stringContains afterIface "failwith \"todo\"" "Uses InterfaceStubGenerationMethodBody"

                          // Abstract class object expression stub
                          let la, ca = findPos src "new BaseRunner()" 0 6

                          let! absRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange la ca) [||] None)
                                  CancellationToken.None
                              |> within

                          let absAct = findAction "Implement abstract members" (extractActions absRes)
                          let afterAbs = applySingleFileAction file src absAct
                          Expect.stringContains afterAbs "new BaseRunner() with" "Inserts 'with' on abstract class object expression"
                          Expect.stringContains afterAbs "override this.Run" "Generates override stub with AbstractClassStubGenerationObjectIdentifier"
                      })
          }

          testTask "Family 10: Generate missing union cases in match (FS0025) aligned with existing pipes and gated by config" {
              let src =
                  """module Task7.Fam10

type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Origin

let area (s: Shape) =
    match s with
    | Circle r -> r * r
"""

              do!
                  withSingleProject true [] [ "Fam10.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam10.fs"
                          let lm, cm = findPos src "match s with" 0 6

                          // Gated off by default
                          let! gated =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lm cm) [||] None)
                                  CancellationToken.None
                              |> within

                          Expect.isFalse
                              (extractActions gated |> Array.exists (fun a -> a.Title = "Generate union pattern match cases"))
                              "UnionCaseStubGeneration = false gates union case stub"

                          let! res =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lm cm) [||] None)
                                  CancellationToken.None
                              |> within

                          let act = findAction "Generate union pattern match cases" (extractActions res)
                          let updated = applySingleFileAction file src act

                          Expect.equal
                              (updated.Replace("\r\n", "\n"))
                              ("module Task7.Fam10\n\ntype Shape =\n    | Circle of radius: float\n    | Rectangle of width: float * height: float\n    | Origin\n\nlet area (s: Shape) =\n    match s with\n    | Circle r -> r * r\n    | Rectangle(_, _) -> failwith \"Not Implemented\"\n    | Origin -> failwith \"Not Implemented\"\n")
                              "Generates only uncovered union cases aligned with existing '|' pipe"
                      })
          }

          testTask "Family 11: Generate missing record fields (FS0764) gated by config" {
              let src =
                  """module Task7.Fam11

type Account =
    { Id: int
      Owner: string
      Active: bool }

let mkAccount () =
    { Id = 1 }
"""

              do!
                  withSingleProject true [] [ "Fam11.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Fam11.fs"
                          let lr, cr = findPos src "{ Id = 1 }" 0 2

                          let! gated =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lr cr) [||] None)
                                  CancellationToken.None
                              |> within

                          Expect.isFalse
                              (extractActions gated |> Array.exists (fun a -> a.Title = "Generate record stub"))
                              "RecordStubGeneration = false gates record stub"

                          let! res =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lr cr) [||] None)
                                  CancellationToken.None
                              |> within

                          let act = findAction "Generate record stub" (extractActions res)
                          let updated = applySingleFileAction file src act
                          Expect.stringContains updated "Id = 1" "Preserves existing assigned field Id"
                          Expect.stringContains updated "Owner = failwith \"Not Implemented\"" "Inserts missing field Owner"
                          Expect.stringContains updated "Active = failwith \"Not Implemented\"" "Inserts missing field Active"
                          let ownerOccurrences = updated.Split("Id =").Length - 1
                          Expect.equal ownerOccurrences 1 "Does not duplicate already-assigned field Id"
                      })
          }

          testTask "Family 12: Sync paired .fsi signature file (FS0034 val update + missing binding Refactor) with cross-file in-turn versioned WorkspaceEdit" {
              let fsiSrc =
                  "module Task7.Fam12\n\nval compute: x: int -> int\n"

              let fsSrc =
                  "module Task7.Fam12\n\nlet compute (x: int) : string = string x\n\nlet addedHelper (y: int) : bool = y > 0\n"

              do!
                  withSingleProject true [] [ "Fam12.fsi", fsiSrc; "Fam12.fs", fsSrc ] (fun _ graph path scope versionNow ->
                      task {
                          let fsiFile = path "Fam12.fsi"
                          let fsFile = path "Fam12.fs"
                          let normFsi = LspClient.NormalizePath fsiFile

                          // (12a) FS0034 mismatched val signature update
                          let lc, cc = findPos fsSrc "compute" 0 2

                          let! resUpdate =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  fsFile
                                  (mkParams fsFile (posRange lc cc) [||] None)
                                  CancellationToken.None
                              |> within

                          let actUpdate = findAction "Update val in signature file" (extractActions resUpdate)
                          let editsMapUpdate = editsByPath actUpdate.Edit.Value
                          Expect.isTrue (editsMapUpdate.ContainsKey normFsi) "Cross-file WorkspaceEdit targets Fam12.fsi"
                          let fsiVerUpdate, fsiEditsUpdate = editsMapUpdate[normFsi]
                          Expect.equal fsiVerUpdate (Some 7) "Captures Fam12.fsi document version (index 0 + 7 = 7) in-turn"
                          let updatedFsi1 = applyLspEdits fsiSrc fsiEditsUpdate
                          Expect.equal updatedFsi1 "module Task7.Fam12\n\nval compute: x: int -> string\n" "Updates val signature in Fam12.fsi"

                          // (12b) Add missing binding to signature file
                          let la, ca = findPos fsSrc "addedHelper" 0 3

                          let! resAdd =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  fsFile
                                  (mkParams fsFile (posRange la ca) [||] None)
                                  CancellationToken.None
                              |> within

                          let actAdd = findAction "Add binding to signature file" (extractActions resAdd)
                          Expect.equal actAdd.Kind (Some CodeActionKind.Refactor) "Add binding to signature file is Refactor"
                          let editsMapAdd = editsByPath actAdd.Edit.Value
                          let fsiVerAdd, fsiEditsAdd = editsMapAdd[normFsi]
                          Expect.equal fsiVerAdd (Some 7) "Captures Fam12.fsi document version in-turn"
                          let updatedFsi2 = applyLspEdits fsiSrc fsiEditsAdd
                          Expect.stringContains updatedFsi2 "val compute: x: int -> int" "Preserves existing val compute"
                          Expect.stringContains updatedFsi2 "val addedHelper: y: int -> bool" "Appends missing val addedHelper to Fam12.fsi"
                      })
          } ]

[<Tests>]
let formattingAndWireTests =
    testSequenced
    <| testList
        "Task7.FormattingAndWire"
        [ testTask "Formatting.formatDocument & formatRange handle .fs, .fsi, minimal edits, range slicing, unchanged [||], and non-blocking LatestParse" {
              let fsiSrc = "module Task7.Fmt\n\nval   alpha :  int ->   int\n"

              let fsSrc =
                  "module Task7.Fmt\n\n// header stays untouched\nlet   alpha (x:int)=x+1\n\nlet keptClean = 10\n\nlet   beta (y:int)=y*2\n// footer stays untouched\n"

              let fsFormatted =
                  "module Task7.Fmt\n\n// header stays untouched\nlet alpha (x: int) = x + 1\n\nlet keptClean = 10\n\nlet beta (y: int) = y * 2\n// footer stays untouched\n"

              let fsiFormatted = "module Task7.Fmt\n\nval alpha: int -> int\n"

              do!
                  withSingleProject true [] [ "Fmt.fsi", fsiSrc; "Fmt.fs", fsSrc ] (fun _ graph path _ _ ->
                      task {
                          let fsiFile = path "Fmt.fsi"
                          let fsFile = path "Fmt.fs"
                          let seenCalls = ConcurrentQueue<string * bool>()

                          let fakeRunner: Formatting.FormattingRunner =
                              fun fp text isSig _opts ct ->
                                  ct.ThrowIfCancellationRequested()
                                  seenCalls.Enqueue(fp, isSig)

                                  if isSig then
                                      Task.FromResult(Some fsiFormatted)
                                  elif text = fsFormatted then
                                      Task.FromResult(Some fsFormatted)
                                  else
                                      Task.FromResult(Some fsFormatted)

                          let fmtOpts: FormattingOptions =
                              { TabSize = 4u
                                InsertSpaces = true
                                TrimTrailingWhitespace = None
                                InsertFinalNewline = None
                                TrimFinalNewlines = None }

                          // 1. formatDocument on .fs produces minimal line-bounded edit (not touching line 0..2 or line 8)
                          let! docEditsOpt =
                              Formatting.formatDocument (Some fakeRunner) graph fsFile fmtOpts CancellationToken.None |> within

                          Expect.isSome docEditsOpt "formatDocument returns Some edits"
                          let docEdits = docEditsOpt.Value
                          Expect.isNonEmpty docEdits "formatDocument returns non-empty edits when unformatted"
                          Expect.isGreaterThan (int docEdits[0].Range.Start.Line) 2 "Minimal edit skips unchanged header lines 0..2"
                          Expect.equal (applyLspEdits fsSrc docEdits) fsFormatted "Applying formatDocument edits yields exact formatted .fs"

                          // 2. formatDocument on .fsi passes isSignature = true
                          let! fsiEditsOpt =
                              Formatting.formatDocument (Some fakeRunner) graph fsiFile fmtOpts CancellationToken.None |> within

                          Expect.isSome fsiEditsOpt "formatDocument on .fsi returns Some edits"
                          Expect.equal (applyLspEdits fsiSrc fsiEditsOpt.Value) fsiFormatted "Applying formatDocument edits yields exact formatted .fsi"
                          Expect.isTrue (seenCalls.ToArray() |> Array.exists (fun (p, isSig) -> sameFile p fsiFile && isSig)) "isSignature=true passed for .fsi"

                          // 3. formatRange on ONLY `alpha` (line 3) formats `alpha` while leaving unformatted `beta` (line 7) untouched!
                          let! rangeAlphaOpt =
                              Formatting.formatRange (Some fakeRunner) graph fsFile (mkRange 3 0 3 23) fmtOpts CancellationToken.None |> within

                          Expect.isSome rangeAlphaOpt "formatRange returns Some edits"
                          let afterAlphaOnly = applyLspEdits fsSrc rangeAlphaOpt.Value
                          Expect.stringContains afterAlphaOnly "let alpha (x: int) = x + 1" "Formats alpha inside requested range"
                          Expect.stringContains afterAlphaOnly "let   beta (y:int)=y*2" "Leaves beta outside requested range untouched"

                          // 4. formatRange on already-formatted `keptClean` (line 5) returns Some [||]
                          let! rangeCleanOpt =
                              Formatting.formatRange (Some fakeRunner) graph fsFile (mkRange 5 0 5 18) fmtOpts CancellationToken.None |> within

                          Expect.equal rangeCleanOpt (Some [||]) "formatRange returns Some [||] when requested range is already formatted"

                          // 5. Unchanged whole document returns Some [||]
                          use blockGate = new ManualResetEventSlim(false)
                          graph.OnFlight(fun kind _ _ ->
                              if kind.StartsWith("finish", StringComparison.Ordinal) then
                                  blockGate.Wait(TimeSpan.FromSeconds 30.) |> ignore)

                          try
                              graph.SetText(fsFile, fsFormatted) |> ignore

                              // Hold finish flight so typecheck cannot finish — proving formatDocument uses non-blocking LatestParse!
                              let! unchangedEditsOpt =
                                  Formatting.formatDocument (Some fakeRunner) graph fsFile fmtOpts CancellationToken.None
                                  |> withinFast

                              Expect.equal unchangedEditsOpt (Some [||]) "Unchanged document returns Some [||] without waiting on typecheck"
                          finally
                              graph.OnFlight(fun _ _ _ -> ())
                              blockGate.Set()
                      })
          }

          testTask "Out-of-process CLI runner: missing Fantomas returns None fast, real spawned CLI formats .fs/.fsi, non-zero exit returns None, and cancellation kills child process" {
              let emptyDir = Directory.CreateTempSubdirectory "rfs-nofantomas-"
              let fakeBinDir = Directory.CreateTempSubdirectory "rfs-fakefantomas-"

              try
                  Formatting.clearToolCacheForTests ()
                  let dummyFs = Path.Combine(emptyDir.FullName, "Sample.fs")
                  File.WriteAllText(dummyFs, "module Sample\nlet  x=1\n")

                  let fmtOpts: FormattingOptions =
                      { TabSize = 4u
                        InsertSpaces = true
                        TrimTrailingWhitespace = None
                        InsertFinalNewline = None
                        TrimFinalNewlines = None }

                  // 1. Missing Fantomas on PATH and no .config/dotnet-tools.json -> returns None quickly
                  let sw = Stopwatch.StartNew()
                  let! missingRes = Formatting.runWithSearchPath (Some emptyDir.FullName) dummyFs "module Sample\nlet  x=1\n" false fmtOpts CancellationToken.None |> withinFast
                  sw.Stop()
                  Expect.isNone missingRes "Missing Fantomas returns None gracefully"
                  Expect.isLessThan sw.Elapsed.TotalSeconds 3.0 "Missing Fantomas probe returns fast"

                  // 2. Real out-of-process executable script named `fantomas.cmd` (Windows) / `fantomas` (Unix)
                  Formatting.clearToolCacheForTests ()
                  let pidMarker = Path.Combine(fakeBinDir.FullName, "child.pid")

                  if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                      let psScript = Path.Combine(fakeBinDir.FullName, "fake_fantomas.ps1")

                      File.WriteAllText(
                          psScript,
                          $"""param([string]$targetFile)
$txt = [System.IO.File]::ReadAllText($targetFile)
if ($txt.Contains("SYNTAX_ERROR")) {{ exit 1 }}
if ($txt.Contains("HANG_FOREVER")) {{
    [System.IO.File]::WriteAllText('{pidMarker.Replace("'", "''")}', $PID.ToString())
    Start-Sleep -Seconds 120
    exit 0
}}
$ext = [System.IO.Path]::GetExtension($targetFile)
$formatted = $txt.Replace("let  x=1", "let x = 1 /*" + $ext + "*/")
[System.IO.File]::WriteAllText($targetFile, $formatted)
exit 0
"""
                      )

                      let cmdPath = Path.Combine(fakeBinDir.FullName, "fantomas.cmd")
                      File.WriteAllText(cmdPath, $"@echo off\r\npwsh -NoProfile -ExecutionPolicy Bypass -File \"{psScript}\" %%*\r\n")
                  else
                      let shPath = Path.Combine(fakeBinDir.FullName, "fantomas")

                      File.WriteAllText(
                          shPath,
                          $"""#!/usr/bin/env bash
targetFile="$1"
if grep -q "SYNTAX_ERROR" "$targetFile"; then exit 1; fi
if grep -q "HANG_FOREVER" "$targetFile"; then
  echo $$ > "{pidMarker}"
  sleep 120
  exit 0
fi
sed -i 's/let  x=1/let x = 1/g' "$targetFile"
"""
                      )

                  // 2a. Out-of-process formatting on .fs and .fsi
                  let! cliFs = Formatting.runWithSearchPath (Some fakeBinDir.FullName) dummyFs "module Sample\nlet  x=1\n" false fmtOpts CancellationToken.None |> within
                  Expect.isSome cliFs "Out-of-process fantomas CLI formatted .fs"
                  Expect.stringContains cliFs.Value "let x = 1" "Formatted output returned from temp .fs file"

                  let dummyFsi = Path.Combine(emptyDir.FullName, "Sample.fsi")
                  let! cliFsi = Formatting.runWithSearchPath (Some fakeBinDir.FullName) dummyFsi "module Sample\nlet  x=1\n" true fmtOpts CancellationToken.None |> within
                  Expect.isSome cliFsi "Out-of-process fantomas CLI formatted .fsi"
                  if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                      Expect.stringContains cliFsi.Value "/*.fsi*/" "Temp file used .fsi extension for signature files"

                  // 2b. Non-zero exit code -> returns None gracefully
                  let! cliErr = Formatting.runWithSearchPath (Some fakeBinDir.FullName) dummyFs "SYNTAX_ERROR" false fmtOpts CancellationToken.None |> within
                  Expect.isNone cliErr "Non-zero Fantomas exit code returns None without crashing"

                  // 2c. Cancellation kills spawned child process tree
                  let cts = new CancellationTokenSource()
                  use _ctsDisp = disposing cts
                  let runningTask = Formatting.runWithSearchPath (Some fakeBinDir.FullName) dummyFs "HANG_FOREVER" false fmtOpts cts.Token

                  let deadline = DateTime.UtcNow.AddSeconds 15.

                  while not (File.Exists pidMarker) && DateTime.UtcNow < deadline do
                      do! Task.Delay 25

                  Expect.isTrue (File.Exists pidMarker) "Child process started and wrote PID marker"
                  let childPid = Int32.Parse(File.ReadAllText(pidMarker).Trim())
                  cts.Cancel()

                  let! cancelledCleanly =
                      task {
                          try
                              let! _ = runningTask |> withinFast
                              return true
                          with :? OperationCanceledException ->
                              return true
                      }

                  Expect.isTrue cancelledCleanly "Formatting cancellation completed promptly"

                  // Wait briefly for OS process table cleanup and verify child process exited
                  let killDeadline = DateTime.UtcNow.AddSeconds 5.
                  let mutable childAlive = true

                  while childAlive && DateTime.UtcNow < killDeadline do
                      try
                          let p = Process.GetProcessById childPid
                          childAlive <- not p.HasExited
                      with :? ArgumentException ->
                          childAlive <- false

                      if childAlive then
                          do! Task.Delay 50

                  Expect.isFalse childAlive $"Cancelled Fantomas child process (PID %d{childPid}) was killed"
              finally
                  Formatting.clearToolCacheForTests ()

                  try
                      emptyDir.Delete true
                  with _ ->
                      ()

                  try
                      fakeBinDir.Delete true
                  with _ ->
                      ()
          }

          testTask "Live JSON-RPC wire round-trips for textDocument/codeAction, textDocument/formatting, and textDocument/rangeFormatting" {
              let! captured = TestServer.startCapturing ()
              let (lsp: LspClient), owner, serverTask = captured
              use _ = disposing owner
              let! runningRaw = serverTask
              let running: RanvierServer = runningRaw

              running.FormattingRunnerOverride <-
                  Some(fun _ text _ _ ct ->
                      ct.ThrowIfCancellationRequested()
                      Task.FromResult(Some(text.Replace("let   bad=42", "let bad = 42"))))

              let! _ =
                  lsp.Initialize(
                      Paths.toUri PublishTests.depsDir,
                      JsonNode.Parse """{"unusedOpensAnalyzer":true}"""
                  )
                  |> within

              let loadParams =
                  JsonNode.Parse($"""{{"TextDocuments":[{{"Uri":"{Paths.toUri PublishTests.depsProject}"}}]}}""")

              let! _ = lsp.Request("fsharp/workspaceLoad", loadParams) |> within

              let cPath = PublishTests.c
              let wireSrc = "module Deps.C\n\nopen System.Text\n\nlet   bad=42\nlet useA = Deps.A.a + bad\n"
              do! lsp.DidOpen(cPath, wireSrc)

              // 1. Live wire textDocument/codeAction on unused `open System.Text`
              let caReq =
                  JsonNode.Parse(
                      $"""{{"textDocument":{{"uri":"{Paths.toUri cPath}"}},"range":{{"start":{{"line":2,"character":5}},"end":{{"line":2,"character":5}}}},"context":{{"diagnostics":[]}}}}"""
                  )

              let! caPair = lsp.Request("textDocument/codeAction", caReq) |> within
              let caResp: JsonNode = fst caPair
              Expect.isNotNull caResp "textDocument/codeAction returned non-null array"
              let caTitles = caResp.AsArray() |> Seq.map (fun n -> n["title"].GetValue<string>()) |> List.ofSeq
              Expect.contains caTitles "Remove unused open" "Wire codeAction returned 'Remove unused open'"

              // 2. Live wire textDocument/formatting
              let fmtReq =
                  JsonNode.Parse(
                      $"""{{"textDocument":{{"uri":"{Paths.toUri cPath}"}},"options":{{"tabSize":4,"insertSpaces":true}}}}"""
                  )

              let! fmtPair = lsp.Request("textDocument/formatting", fmtReq) |> within
              let fmtResp: JsonNode = fst fmtPair
              Expect.isNotNull fmtResp "textDocument/formatting returned edits"
              Expect.isGreaterThan (fmtResp.AsArray().Count) 0 "textDocument/formatting returned non-empty TextEdit[]"

              // 3. Live wire textDocument/rangeFormatting
              let rfmtReq =
                  JsonNode.Parse(
                      $"""{{"textDocument":{{"uri":"{Paths.toUri cPath}"}},"range":{{"start":{{"line":4,"character":0}},"end":{{"line":4,"character":12}}}},"options":{{"tabSize":4,"insertSpaces":true}}}}"""
                  )

              let! rfmtPair = lsp.Request("textDocument/rangeFormatting", rfmtReq) |> within
              let rfmtResp: JsonNode = fst rfmtPair
              Expect.isNotNull rfmtResp "textDocument/rangeFormatting returned edits"
              Expect.isGreaterThan (rfmtResp.AsArray().Count) 0 "textDocument/rangeFormatting returned non-empty TextEdit[]"

              do! lsp.Shutdown() |> withinUnit
          } ]

let private hasBareLf (s: string) : bool =
    let mutable found = false

    for i in 0 .. s.Length - 1 do
        if s[i] = '\n' && (i = 0 || s[i - 1] <> '\r') then
            found <- true

    found

[<Tests>]
let reviewRegressionsTests =
    task7Sequenced
    <| testList
        "Task7.ReviewRegressions"
        [ testTask "Finding 1: Promoted open file shifted declarations retain FS1182, FS0760, FS0020, and same-file FS0039 Open quick-fixes" {
              let initialSrc =
                  """module Task7.ShiftedPromoted

module SameFileHelpers =
    type SameFileColor =
        | Crimson
        | Cobalt

    let sameFileWidget (n: int) = n + 10

let firstUnused () =
    let unusedShifted = 42
    1

let secondDisp () =
    System.IO.MemoryStream()

let thirdMut () =
    let mutable count = 0
    count = 5
    count
"""
                      .Replace("\r\n", "\n")

              do!
                  withSingleProject true [ "--warnon:1182" ] [ "Shifted.fs", initialSrc ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "Shifted.fs"
                          let! modeBefore = graph.Mode file |> within
                          Expect.equal modeBefore FileMode.Declarations "File promoted to declaration mode"
                          let shiftedBefore = graph.ShiftedRows
                          let adHocBefore = graph.AdHocChecks

                          let editedSrc =
                              initialSrc.Replace(
                                  "module Task7.ShiftedPromoted\n\n",
                                  "module Task7.ShiftedPromoted\n\nlet addedAbove = 1\n\n"
                              )

                          graph.SetText(file, editedSrc) |> ignore
                          let! _ = graph.SettledDiagnostics file |> within
                          Expect.isGreaterThan graph.ShiftedRows shiftedBefore "Lower declarations shifted without re-checking"

                          // 1a. FS1182 on shifted declaration (CheckResultsAt returns ViewResults with empty Diagnostics)
                          let lu, cu = findPos editedSrc "unusedShifted" 0 3

                          let! res1182 =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lu cu) [||] None)
                                  CancellationToken.None
                              |> within

                          Expect.isGreaterThan graph.AdHocChecks adHocBefore "CheckResultsAt exercised DeclarationChecker.ViewResults path"
                          let acts1182 = extractActions res1182
                          let prefixAct = findAction "Prefix with _" acts1182
                          Expect.isSome prefixAct.Diagnostics "FS1182 quick-fix on shifted decl carries Diagnostics"
                          Expect.stringContains (applySingleFileAction file editedSrc prefixAct) "let _unusedShifted = 42" "FS1182 fires on shifted decl"

                          // 1b. FS0760 on shifted declaration
                          let ld, cd = findPos editedSrc "System.IO.MemoryStream()" 0 5

                          let! res760 =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange ld cd) [||] None)
                                  CancellationToken.None
                              |> within

                          let newAct = findAction "Add 'new' keyword" (extractActions res760)
                          Expect.stringContains (applySingleFileAction file editedSrc newAct) "new System.IO.MemoryStream()" "FS0760 fires on shifted decl"

                          // 1c. FS0020 on shifted declaration
                          let lm, cm = findPos editedSrc "count = 5" 0 2

                          let! res20 =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lm cm) [||] None)
                                  CancellationToken.None
                              |> within

                          let mutAct = findAction "Use '<-' to mutate value" (extractActions res20)
                          Expect.stringContains (applySingleFileAction file editedSrc mutAct) "count <- 5" "FS0020 fires on shifted decl"

                          // 1d. Same-file FS0039 ResolveNamespaces on kept & shifted declarations where CheckResultsAt returns ViewResults (empty ownSigForFile)
                          let withSameFileRefs =
                              editedSrc
                              + "\nlet fourthSameFile () =\n    sameFileWidget 3\n\nlet fifthSameFileCase () =\n    Crimson\n"

                          graph.SetText(file, withSameFileRefs) |> ignore
                          let! _ = graph.SettledDiagnostics file |> within

                          // First: mutate addedAbove on the SAME line (moved.Length = 0 -> fourthSameFile & fifthSameFileCase stay kept with stale EnvBefore, forcing CheckResultsAt onto ViewResults!)
                          let adHocBefore2 = graph.AdHocChecks
                          let staleEnvSameFileSrc = withSameFileRefs.Replace("let addedAbove = 1", "let addedAbove = 999")
                          graph.SetText(file, staleEnvSameFileSrc) |> ignore
                          let! _ = graph.SettledDiagnostics file |> within

                          let ls0, cs0 = findPos staleEnvSameFileSrc "sameFileWidget 3" 0 4

                          let! resView =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange ls0 cs0) [||] None)
                                  CancellationToken.None
                              |> within

                          Expect.isGreaterThan graph.AdHocChecks adHocBefore2 "Same-file FS0039 query exercised ViewResults"
                          let _ = findAction "open SameFileHelpers" (extractActions resView)
                          let _ = findAction "SameFileHelpers.sameFileWidget" (extractActions resView)

                          // Second: insert blank lines AFTER firstUnused (outside prologue!) so fourthSameFile & fifthSameFileCase themselves shift without re-checking!
                          let shiftedBefore2 = graph.ShiftedRows

                          let shiftedSameFileSrc =
                              staleEnvSameFileSrc.Replace(
                                  "let firstUnused () =\n    let unusedShifted = 42\n    1\n",
                                  "let firstUnused () =\n    let unusedShifted = 42\n    1\n\n\n\n"
                              )

                          graph.SetText(file, shiftedSameFileSrc) |> ignore
                          let! _ = graph.SettledDiagnostics file |> within
                          Expect.isGreaterThan graph.ShiftedRows shiftedBefore2 "Same-file FS0039 declarations shifted without re-checking"

                          let ls, cs = findPos shiftedSameFileSrc "sameFileWidget 3" 0 4

                          let! res39 =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange ls cs) [||] None)
                                  CancellationToken.None
                              |> within

                          let acts39 = extractActions res39
                          let openSame = findAction "open SameFileHelpers" acts39
                          let fqSame = findAction "SameFileHelpers.sameFileWidget" acts39
                          let afterOpenSame = applySingleFileAction file shiftedSameFileSrc openSame
                          Expect.stringContains afterOpenSame "open SameFileHelpers\n" "Inserts 'open SameFileHelpers' for same-file module"
                          Expect.stringContains (applySingleFileAction file shiftedSameFileSrc fqSame) "SameFileHelpers.sameFileWidget 3" "Qualifies same-file module value"

                          let lc, cc = findPos shiftedSameFileSrc "    Crimson" 0 6

                          let! resCase =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lc cc) [||] None)
                                  CancellationToken.None
                              |> within

                          let actsCase = extractActions resCase
                          let _ = findAction "open SameFileHelpers" actsCase
                          let fqCase = findAction "SameFileHelpers.SameFileColor.Crimson" actsCase
                          Expect.stringContains (applySingleFileAction file shiftedSameFileSrc fqCase) "SameFileHelpers.SameFileColor.Crimson" "Qualifies same-file union case"
                      })
          }

          testTask "Finding 2: Open B.fs updates FSAC0001 when upstream A.fs changes without editing B.fs, and cancels CTS cleanly" {
              let dir = Directory.CreateTempSubdirectory "rfs-task7-upstream-"

              try
                  let projFile = Path.Combine(dir.FullName, "UpstreamProj.fsproj")
                  let aFile = Path.Combine(dir.FullName, "A.fs")
                  let bFile = Path.Combine(dir.FullName, "B.fs")

                  let projXml =
                      """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="A.fs" />
    <Compile Include="B.fs" />
  </ItemGroup>
</Project>
"""

                  let aSrc1 = "namespace Upstream\n\nmodule ModA =\n    let valA = 1\n"
                  let aSrc2 = "namespace Upstream\n\nmodule ModA =\n    let otherVal = 1\n\n[<AutoOpen>]\nmodule Fallback =\n    let valA = 2\n"
                  let bSrc = "module Upstream.B\n\nopen Upstream.ModA\n\nlet compute () = valA + 1\n"

                  File.WriteAllText(projFile, projXml)
                  File.WriteAllText(aFile, aSrc1)
                  File.WriteAllText(bFile, bSrc)

                  File.Copy(Path.Combine(PublishFixTests.repoRoot, "global.json"), Path.Combine(dir.FullName, "global.json"), true)
                  let restorePsi = ProcessStartInfo("dotnet", $"restore \"{projFile}\" --disable-build-servers")
                  restorePsi.WorkingDirectory <- dir.FullName
                  restorePsi.UseShellExecute <- false
                  restorePsi.CreateNoWindow <- true
                  restorePsi.RedirectStandardOutput <- true
                  restorePsi.RedirectStandardError <- true
                  restorePsi.Environment.Remove "MSBUILD_EXE_PATH" |> ignore
                  restorePsi.Environment.Remove "MSBuildExtensionsPath" |> ignore
                  restorePsi.Environment.Remove "MSBuildSDKsPath" |> ignore
                  restorePsi.Environment.Remove "DOTNET_HOST_PATH" |> ignore
                  restorePsi.Environment["DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER"] <- "1"
                  restorePsi.Environment["MSBUILDDISABLENODEREUSE"] <- "1"
                  let restoreProc = Process.Start restorePsi
                  use _restoreDisp = disposing restoreProc
                  do! restoreProc.WaitForExitAsync() |> withinUnit
                  Expect.equal restoreProc.ExitCode 0 "dotnet restore succeeded"

                  let! captured = TestServer.startCapturing ()
                  let (lsp: LspClient), owner, serverTask = captured
                  use _ = disposing owner
                  let! runningRaw = serverTask
                  let running: RanvierServer = runningRaw

                  let bPublishes = ConcurrentQueue<JsonArray>()

                  use _sub =
                      disposing (
                          lsp.Notifications.Subscribe
                              { new IObserver<Received> with
                                  member _.OnNext r =
                                      if
                                          not (isNull r.Message["method"])
                                          && r.Message["method"].GetValue<string>() = "textDocument/publishDiagnostics"
                                      then
                                          let pObj = r.Message["params"].AsObject()

                                          if sameFile (Paths.ofUri (pObj["uri"].GetValue<string>())) bFile then
                                              bPublishes.Enqueue(pObj["diagnostics"].AsArray())

                                  member _.OnError _ = ()
                                  member _.OnCompleted() = () }
                      )

                  let! _ =
                      lsp.Initialize(
                          Paths.toUri dir.FullName,
                          JsonNode.Parse """{"unusedOpensAnalyzer":true}"""
                      )
                      |> within

                  let loadParams =
                      JsonNode.Parse($"""{{"TextDocuments":[{{"Uri":"{Paths.toUri projFile}"}}]}}""")

                  let! _ = lsp.Request("fsharp/workspaceLoad", loadParams) |> within
                  do! lsp.DidOpen(aFile, aSrc1)
                  do! lsp.DidOpen(bFile, bSrc)
                  do! running.Publisher.Flushed() |> withinUnit

                  let latestB () = bPublishes.ToArray() |> Array.tryLast

                  let waitForB (desc: string) (pred: JsonArray -> bool) =
                      task {
                          let deadline = DateTime.UtcNow.AddSeconds 15.

                          while not (latestB () |> Option.exists pred) do
                              if DateTime.UtcNow > deadline then
                                  let dump = latestB () |> Option.map _.ToJsonString() |> Option.defaultValue "<none>"
                                  failtest $"Timed out waiting for B.fs %s{desc}. Latest: %s{dump}"

                              do! Task.Delay 25

                          return (latestB ()).Value
                      }

                  let hasFsac1 (arr: JsonArray) =
                      arr |> Seq.exists (fun d -> not (isNull d["code"]) && d["code"].GetValue<string>() = "FSAC0001")

                  let! initialB = waitForB "initial clean publish" (fun arr -> arr.Count = 0)
                  Expect.isFalse (hasFsac1 initialB) "Initially open Upstream.ModA is used in B.fs"

                  let replaceAllEdit (oldText: string) (newText: string) : Edit =
                      let oldLines = oldText.Split '\n'
                      let lastIdx = max 0 (oldLines.Length - 1)
                      let lastLen = oldLines[lastIdx].TrimEnd('\r').Length

                      { Range =
                          { Start = { Line = 0; Character = 0 }
                            End = { Line = lastIdx; Character = lastLen } }
                        Text = newText }

                  // Edit ONLY A.fs (B.fs is NOT edited; B.fs Epoch stays unchanged!)
                  do! lsp.DidChange(aFile, 2, replaceAllEdit aSrc1 aSrc2)
                  let! staleCheckB = waitForB "FSAC0001 after upstream A.fs moved valA to [<AutoOpen>]" hasFsac1
                  Expect.isTrue (hasFsac1 staleCheckB) "B.fs publishes FSAC0001 when upstream A.fs makes open Upstream.ModA unused"

                  // Edit ONLY A.fs back -> FSAC0001 on B.fs must clear without editing B.fs!
                  do! lsp.DidChange(aFile, 3, replaceAllEdit aSrc2 aSrc1)
                  let! restoredB = waitForB "FSAC0001 cleared after upstream A.fs restored valA" (fun arr -> not (hasFsac1 arr))
                  Expect.isFalse (hasFsac1 restoredB) "B.fs clears stale FSAC0001 when upstream A.fs restores ModA.valA"

                  do!
                      running.Host.Read(fun () -> assertNoCompilerRetention "Publisher" running.Publisher)
                      |> withinUnit

                  do! lsp.Notify("textDocument/didClose", JsonNode.Parse($"""{{"textDocument":{{"uri":"{Paths.toUri bFile}"}}}}"""))
                  do! lsp.Notify("textDocument/didClose", JsonNode.Parse($"""{{"textDocument":{{"uri":"{Paths.toUri aFile}"}}}}"""))
                  do! lsp.Shutdown() |> withinUnit
              finally
                  try
                      dir.Delete true
                  with _ ->
                      ()
          }

          testTask "Finding 3: Family 5 'Add rec' supports mutual recursion from both FS0576 on 'let' and FS0039 on sibling binding" {
              let src =
                  """module Task7.MutRec

let isEven n =
    if n = 0 then true else isOdd (n - 1)
and isOdd n =
    if n = 0 then false else isEven (n - 1)
"""
                      .Replace("\r\n", "\n")

              do!
                  withSingleProject false [] [ "MutRec.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "MutRec.fs"

                          // 3a. Cursor on 'let' where FS0576 is reported
                          let lLet, cLet = findPos src "let isEven" 0 1

                          let! resLet =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lLet cLet) [||] None)
                                  CancellationToken.None
                              |> within

                          let actFrom576 = findAction "Make 'isEven' recursive" (extractActions resLet)
                          let expectedRec = src.Replace("let isEven n =", "let rec isEven n =")
                          Expect.equal (applySingleFileAction file src actFrom576) expectedRec "FS0576 on 'let' inserts ' rec'"

                          // 3b. Cursor on 'isOdd' inside isEven's body where FS0039 is reported
                          let lOdd, cOdd = findPos src "isOdd (n - 1)" 0 2

                          let! resOdd =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lOdd cOdd) [||] None)
                                  CancellationToken.None
                              |> within

                          let actFrom39 = findAction "Make 'isEven' recursive" (extractActions resOdd)
                          Expect.equal (applySingleFileAction file src actFrom39) expectedRec "FS0039 on sibling 'isOdd' inserts ' rec' after leading 'let'"
                      })
          }

          testTask "Finding 4: Family 1 (Remove unused open) and Family 3 (Remove redundant qualifier) fire on-demand under FSharpConfig.Default" {
              let src =
                  "module Task7.OnDemand\n\nopen System\nopen System.IO\n\nlet emptyStr = System.String.Empty\nlet plain = 42\n"

              do!
                  withSingleProject true [] [ "OnDemand.fs", src ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "OnDemand.fs"

                          // Family 1 on-demand with FSharpConfig.Default (UnusedOpensAnalyzer = false)
                          let lo, co = findPos src "open System.IO" 0 6

                          let! resOpen =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lo co) [||] None)
                                  CancellationToken.None
                              |> within

                          let rmOpen = findAction "Remove unused open" (extractActions resOpen)

                          Expect.equal
                              (applySingleFileAction file src rmOpen)
                              "module Task7.OnDemand\n\nopen System\n\nlet emptyStr = System.String.Empty\nlet plain = 42\n"
                              "On-demand Remove unused open works under FSharpConfig.Default"

                          // Family 3 on-demand with FSharpConfig.Default (SimplifyNameAnalyzer = false)
                          let lq, cq = findPos src "System.String.Empty" 0 3

                          let! resQual =
                              CodeActions.provideWithCapabilities
                                  FSharpConfig.Default
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange lq cq) [||] None)
                                  CancellationToken.None
                              |> within

                          let rmQual = findAction "Remove redundant qualifier" (extractActions resQual)

                          Expect.equal
                              (applySingleFileAction file src rmQual)
                              "module Task7.OnDemand\n\nopen System\nopen System.IO\n\nlet emptyStr = String.Empty\nlet plain = 42\n"
                              "On-demand Remove redundant qualifier works under FSharpConfig.Default"
                      })
          }

          testTask "Finding 5: Formatting.formatDocument and formatRange re-verify TextView.Version after runner finishes (retrying once, returning None if mutated twice)" {
              let src1 = "module Task7.FmtRace\n\nlet   x=1\n"
              let src2 = "module Task7.FmtRace\n\nlet   x=2\n\nlet   y=3\n"

              do!
                  withSingleProject true [] [ "FmtRace.fs", src1 ] (fun _ graph path _ _ ->
                      task {
                          let file = path "FmtRace.fs"

                          let fmtOpts: FormattingOptions =
                              { TabSize = 4u
                                InsertSpaces = true
                                TrimTrailingWhitespace = None
                                InsertFinalNewline = None
                                TrimFinalNewlines = None }

                          // Case A: Mutates once during attempt 1 -> retries on attempt 2 and formats src2 cleanly
                          let mutable docCalls = 0

                          let oneRaceRunner: Formatting.FormattingRunner =
                              fun _ text _ _ _ ->
                                  task {
                                      docCalls <- docCalls + 1

                                      if docCalls = 1 then
                                          graph.SetText(file, src2) |> ignore
                                          let! _ = graph.LatestParse(file, CancellationToken.None)
                                          ()

                                      return Some(text.Replace("let   x=1", "let x = 1").Replace("let   x=2", "let x = 2").Replace("let   y=3", "let y = 3"))
                                  }

                          let! retriedDoc =
                              Formatting.formatDocument (Some oneRaceRunner) graph file fmtOpts CancellationToken.None |> within

                          Expect.equal docCalls 2 "formatDocument retried once after mid-flight version change"
                          Expect.isSome retriedDoc "formatDocument succeeded on second attempt"
                          Expect.equal (applyLspEdits src2 retriedDoc.Value) "module Task7.FmtRace\n\nlet x = 2\n\nlet y = 3\n" "Edits match src2, not stale src1"

                          // Case B: formatRange also retries once on mid-flight edit
                          graph.SetText(file, src1) |> ignore
                          let! _ = graph.LatestParse(file, CancellationToken.None)
                          let mutable rangeCalls = 0

                          let oneRaceRangeRunner: Formatting.FormattingRunner =
                              fun _ text _ _ _ ->
                                  task {
                                      rangeCalls <- rangeCalls + 1

                                      if rangeCalls = 1 then
                                          graph.SetText(file, src2) |> ignore
                                          let! _ = graph.LatestParse(file, CancellationToken.None)
                                          ()

                                      return Some(text.Replace("let   x=1", "let x = 1").Replace("let   x=2", "let x = 2").Replace("let   y=3", "let y = 3"))
                                  }

                          let! retriedRange =
                              Formatting.formatRange (Some oneRaceRangeRunner) graph file (mkRange 2 0 2 10) fmtOpts CancellationToken.None |> within

                          Expect.equal rangeCalls 2 "formatRange retried once after mid-flight version change"
                          Expect.isSome retriedRange "formatRange succeeded on second attempt"
                          Expect.equal (applyLspEdits src2 retriedRange.Value) "module Task7.FmtRace\n\nlet x = 2\n\nlet   y=3\n" "Range edits match src2"

                          // Case C: Mutates on BOTH attempts -> returns None rather than stale edits
                          let mutable alwaysCalls = 0

                          let alwaysMutatingRunner: Formatting.FormattingRunner =
                              fun _ text _ _ _ ->
                                  task {
                                      alwaysCalls <- alwaysCalls + 1
                                      let nextText = $"module Task7.FmtRace\n\nlet   x=%d{alwaysCalls + 10}\n"
                                      graph.SetText(file, nextText) |> ignore
                                      let! _ = graph.LatestParse(file, CancellationToken.None)
                                      return Some(text.Replace("  ", " "))
                                  }

                          let! dropped =
                              Formatting.formatDocument (Some alwaysMutatingRunner) graph file fmtOpts CancellationToken.None |> within

                          Expect.equal alwaysCalls 2 "Stopped after 2 attempts"
                          Expect.isNone dropped "Returns None when document keeps mutating across both formatting attempts"
                      })
          }

          testTask "Finding 6: Remove all unused opens coalesces contiguous deleted lines at EOF without trailing newline into a single non-overlapping TextEdit" {
              let srcNoEofNl = "module Task7.EofOpens\nopen System.IO\nopen System.Text"

              do!
                  withSingleProject false [] [ "EofOpens.fs", srcNoEofNl ] (fun _ graph path scope versionNow ->
                      task {
                          let file = path "EofOpens.fs"

                          let! orgRes =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  file
                                  (mkParams file (posRange 0 0) [||] (Some [| "source.organizeImports" |]))
                                  CancellationToken.None
                              |> within

                          let removeAll = findAction "Remove all unused opens" (extractActions orgRes)
                          let _, edits = (editsByPath removeAll.Edit.Value)[LspClient.NormalizePath file]
                          Expect.equal edits.Length 1 "Contiguous unused opens at EOF coalesce into a single non-overlapping TextEdit"
                          Expect.equal (applyLspEdits srcNoEofNl edits) "module Task7.EofOpens" "Applying coalesced edit yields exact 'module Task7.EofOpens'"
                      })
          }

          testTask "Finding 7: Multiline DU and record type headers missing '=' emit 'Add =' at end of header line" {
              let duSrc = "module Task7.MultiDu\n\ntype Color\n    | Red\n    | Blue\n"
              let recSrc = "module Task7.MultiRec\n\ntype Boxed\n    { Value: int }\n"

              do!
                  withSingleProject false [] [ "MultiDu.fs", duSrc; "MultiRec.fs", recSrc ] (fun _ graph path scope versionNow ->
                      task {
                          let duFile = path "MultiDu.fs"
                          let lDu, cDu = findPos duSrc "| Red" 0 0

                          let! resDu =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  duFile
                                  (mkParams duFile (posRange lDu cDu) [||] None)
                                  CancellationToken.None
                              |> within

                          let actDu = findAction "Add '='" (extractActions resDu)

                          Expect.equal
                              (applySingleFileAction duFile duSrc actDu)
                              "module Task7.MultiDu\n\ntype Color =\n    | Red\n    | Blue\n"
                              "Inserts ' =' at end of multiline DU header"

                          let recFile = path "MultiRec.fs"
                          let lRec, cRec = findPos recSrc "{ Value" 0 0

                          let! resRec =
                              CodeActions.provideWithCapabilities
                                  enabledConfig
                                  true
                                  versionNow
                                  scope
                                  graph
                                  recFile
                                  (mkParams recFile (posRange lRec cRec) [||] None)
                                  CancellationToken.None
                              |> within

                          let actRec = findAction "Add '='" (extractActions resRec)

                          Expect.equal
                              (applySingleFileAction recFile recSrc actRec)
                              "module Task7.MultiRec\n\ntype Boxed =\n    { Value: int }\n"
                              "Inserts ' =' at end of multiline record header"
                      })
          }

          testTask "Finding 8: QuickFix CodeActions attach Diagnostics for FSAC0001, FSAC0002, FSAC0003, FS0366, and FS0365" {
              let srcIface =
                  """module Task7.DiagIface

open System
open System.IO

let private unusedTopVal = 123
let redQual = System.String.Empty

type IGreeter =
    abstract Greet: unit -> string

type BadGreeter() =
    member _.Greet() = ""
    interface IGreeter

type BareGreeter() =
    interface IGreeter
"""
                      .Replace("\r\n", "\n")

              let srcAbsSub =
                  """module Task7.DiagAbsSub

[<AbstractClass>]
type AbstractOp() =
    abstract Exec: unit -> int

type BadSub() =
    inherit AbstractOp()
"""
                      .Replace("\r\n", "\n")

              let srcAbsObj =
                  """module Task7.DiagAbsObj

[<AbstractClass>]
type AbstractOp() =
    abstract Exec: unit -> int

let badObj () = { new AbstractOp() with member _.ToString() = "" }
"""
                      .Replace("\r\n", "\n")

              do!
                  withSingleProject
                      true
                      []
                      [ "DiagIface.fs", srcIface
                        "DiagAbsSub.fs", srcAbsSub
                        "DiagAbsObj.fs", srcAbsObj ]
                      (fun _ graph path scope versionNow ->
                          task {
                              let ifaceFile = path "DiagIface.fs"
                              let absSubFile = path "DiagAbsSub.fs"
                              let absObjFile = path "DiagAbsObj.fs"

                              let checkDiagCode (act: CodeAction) (expectedCode: string) =
                                  Expect.equal act.Kind (Some CodeActionKind.QuickFix) $"Expected '%s{act.Title}' to be QuickFix"
                                  Expect.isSome act.Diagnostics $"Expected '%s{act.Title}' to carry Diagnostics"

                                  let codes =
                                      act.Diagnostics.Value
                                      |> Array.choose (fun d ->
                                          d.Code
                                          |> Option.map (function
                                              | U2.C1 i -> string i
                                              | U2.C2 s -> s))

                                  Expect.contains codes expectedCode $"Expected '%s{act.Title}' Diagnostics to contain %s{expectedCode}"

                              // FSAC0001
                              let l1, c1 = findPos srcIface "open System.IO" 0 5
                              let! r1 = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph ifaceFile (mkParams ifaceFile (posRange l1 c1) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Remove unused open" (extractActions r1)) "FSAC0001"

                              // FSAC0002
                              let l2, c2 = findPos srcIface "System.String.Empty" 0 3
                              let! r2 = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph ifaceFile (mkParams ifaceFile (posRange l2 c2) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Remove redundant qualifier" (extractActions r2)) "FSAC0002"

                              // FSAC0003
                              let l3, c3 = findPos srcIface "unusedTopVal" 0 3
                              let! r3 = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph ifaceFile (mkParams ifaceFile (posRange l3 c3) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Prefix with _" (extractActions r3)) "FSAC0003"

                              // FS0366 on class interface implementations (both misplaced-member and bare `interface IGreeter`)
                              let l366, c366 = findPos srcIface "interface IGreeter" 0 12
                              let! r366 = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph ifaceFile (mkParams ifaceFile (posRange l366 c366) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Implement interface" (extractActions r366)) "FS0366"

                              let lBare, cBare = findPos srcIface "interface IGreeter" 1 12
                              let! rBare = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph ifaceFile (mkParams ifaceFile (posRange lBare cBare) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Implement interface" (extractActions rBare)) "FS0366"

                              // FS0365 on class inheriting abstract class
                              let lSub, cSub = findPos srcAbsSub "inherit AbstractOp()" 0 10
                              let! rSub = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph absSubFile (mkParams absSubFile (posRange lSub cSub) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Implement abstract members" (extractActions rSub)) "FS0365"

                              // FS0365 on object expression implementing abstract class
                              let l365, c365 = findPos srcAbsObj "new AbstractOp()" 0 5
                              let! r365 = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph absObjFile (mkParams absObjFile (posRange l365 c365) [||] None) CancellationToken.None |> within
                              checkDiagCode (findAction "Implement abstract members" (extractActions r365)) "FS0365"
                          })
          }

          testTask "Finding 9: Out-of-process Formatting preserves ancestor .editorconfig chain up to root=true and guards tempDir root" {
              let wsDir = Directory.CreateTempSubdirectory "rfs-edcfg-chain-"
              let fakeBinDir = Directory.CreateTempSubdirectory "rfs-edcfg-bin-"

              try
                  Formatting.clearToolCacheForTests ()

                  // Ancestor chain: wsDir/.editorconfig (root = true, indent_size = 2) -> wsDir/src/.editorconfig (max_line_length = 88, no root=true) -> wsDir/src/sub/Code.fs
                  let srcDir = Directory.CreateDirectory(Path.Combine(wsDir.FullName, "src"))
                  let subDir = Directory.CreateDirectory(Path.Combine(srcDir.FullName, "sub"))
                  File.WriteAllText(Path.Combine(wsDir.FullName, ".editorconfig"), "root = true\n\n[*.{fs,fsi}]\nindent_size = 2\n")
                  File.WriteAllText(Path.Combine(srcDir.FullName, ".editorconfig"), "[*.{fs,fsi}]\nmax_line_length = 88\n")
                  let codeFs = Path.Combine(subDir.FullName, "Code.fs")
                  File.WriteAllText(codeFs, "module Code\nlet x = 1\n")

                  // Also test a standalone dir whose single .editorconfig omits `root = true`: topmost copied .editorconfig in tempDir MUST get `root = true`
                  let noRootWs = Directory.CreateTempSubdirectory "rfs-edcfg-noroot-"
                  use _cleanNoRoot =
                      disposing
                          { new IDisposable with
                              member _.Dispose() =
                                  try
                                      noRootWs.Delete true
                                  with _ ->
                                      () }

                  File.WriteAllText(Path.Combine(noRootWs.FullName, ".editorconfig"), "[*.{fs,fsi}]\nindent_size = 6\n")
                  let noRootFs = Path.Combine(noRootWs.FullName, "Standalone.fs")
                  File.WriteAllText(noRootFs, "module Standalone\nlet x = 1\n")

                  let probeScript = Path.Combine(fakeBinDir.FullName, "probe_fantomas.ps1")

                  File.WriteAllText(
                      probeScript,
                      """param([string]$targetFile)
$cur = [System.IO.Path]::GetDirectoryName($targetFile)
$seen = New-Object System.Collections.Generic.List[string]
$hitRoot = $false
for ($i = 0; $i -lt 6 -and $null -ne $cur; $i++) {
    $cfg = [System.IO.Path]::Combine($cur, ".editorconfig")
    if ([System.IO.File]::Exists($cfg)) {
        $raw = [System.IO.File]::ReadAllText($cfg).Replace("`r`n", "`n").Trim()
        $seen.Add($raw)
        if ($raw -match '(?m)^\s*root\s*=\s*true\b') {
            $hitRoot = $true
            break
        }
    }
    $parent = [System.IO.Directory]::GetParent($cur)
    $cur = if ($null -eq $parent) { $null } else { $parent.FullName }
}
$summary = ($seen -join " || ") + " | hitRoot=" + $hitRoot
$orig = [System.IO.File]::ReadAllText($targetFile)
[System.IO.File]::WriteAllText($targetFile, $orig + "// " + $summary + "`n")
exit 0
"""
                  )

                  if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                      File.WriteAllText(
                          Path.Combine(fakeBinDir.FullName, "fantomas.cmd"),
                          $"@echo off\r\npwsh -NoProfile -ExecutionPolicy Bypass -File \"{probeScript}\" %%*\r\n"
                      )
                  else
                      let sh = Path.Combine(fakeBinDir.FullName, "fantomas")
                      File.WriteAllText(sh, $"#!/usr/bin/env bash\npwsh -NoProfile -File \"{probeScript}\" \"$@\"\n")

                  let fmtOpts: FormattingOptions =
                      { TabSize = 4u
                        InsertSpaces = true
                        TrimTrailingWhitespace = None
                        InsertFinalNewline = None
                        TrimFinalNewlines = None }

                  let! chainOut =
                      Formatting.runWithSearchPath (Some fakeBinDir.FullName) codeFs "module Code\nlet x = 1\n" false fmtOpts CancellationToken.None
                      |> within

                  Expect.isSome chainOut "Formatting succeeded with nested .editorconfig chain"
                  Expect.stringContains chainOut.Value "max_line_length = 88" "Child .editorconfig preserved in temp hierarchy"
                  Expect.stringContains chainOut.Value "indent_size = 2" "Parent .editorconfig preserved above child in temp hierarchy"
                  Expect.stringContains chainOut.Value "hitRoot=True" "Upward walk stopped at root=true inside tempDir"

                  Formatting.clearToolCacheForTests ()

                  let! noRootOut =
                      Formatting.runWithSearchPath (Some fakeBinDir.FullName) noRootFs "module Standalone\nlet x = 1\n" false fmtOpts CancellationToken.None
                      |> within

                  Expect.isSome noRootOut "Formatting succeeded for rootless .editorconfig"
                  Expect.stringContains noRootOut.Value "indent_size = 6" "Copied .editorconfig content preserved"
                  Expect.stringContains noRootOut.Value "hitRoot=True" "Topmost copied .editorconfig guarded with root=true"
              finally
                  Formatting.clearToolCacheForTests ()

                  try
                      wsDir.Delete true
                  with _ ->
                      ()

                  try
                      fakeBinDir.Delete true
                  with _ ->
                      ()
          }

          testTask "Finding 10: Synthesized multi-line CodeActions preserve CRLF line endings on CRLF .fs and .fsi buffers" {
              let fsiCrlf = "module Task7.Crlf\r\n\r\nval existing: int\r\n"

              let fsCrlf =
                  String.concat
                      "\r\n"
                      [ "module Task7.Crlf"
                        ""
                        "open System"
                        ""
                        "let existing = 1"
                        "let useSb () = StringBuilder()"
                        ""
                        "type IWork ="
                        "    abstract DoIt: unit -> int"
                        ""
                        "type Impl() ="
                        "    interface IWork"
                        ""
                        "type Choice2 = | One | Two"
                        "let pick (c: Choice2) ="
                        "    match c with"
                        "    | One -> 1"
                        ""
                        "type Rec2 = { A: int; B: int }"
                        "let mkRec () = { A = 1 }"
                        ""
                        "let newBinding (z: int) : int = z + 1"
                        "" ]

              do!
                  withSingleProject true [] [ "Crlf.fsi", fsiCrlf; "Crlf.fs", fsCrlf ] (fun _ graph path scope versionNow ->
                      task {
                          let fsiFile = path "Crlf.fsi"
                          let fsFile = path "Crlf.fs"
                          let normFsi = LspClient.NormalizePath fsiFile

                          // 1. Open insert on CRLF
                          let lo, co = findPos fsCrlf "StringBuilder()" 0 3
                          let! rOpen = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph fsFile (mkParams fsFile (posRange lo co) [||] None) CancellationToken.None |> within
                          let afterOpen = applySingleFileAction fsFile fsCrlf (findAction "open System.Text" (extractActions rOpen))
                          Expect.isFalse (hasBareLf afterOpen) "open insert preserves CRLF without bare LF"

                          // 2. Implement interface on CRLF
                          let li, ci = findPos fsCrlf "interface IWork" 0 11
                          let! rIface = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph fsFile (mkParams fsFile (posRange li ci) [||] None) CancellationToken.None |> within
                          let afterIface = applySingleFileAction fsFile fsCrlf (findAction "Implement interface" (extractActions rIface))
                          Expect.isFalse (hasBareLf afterIface) "Implement interface stub preserves CRLF without bare LF"

                          // 3. Union cases stub on CRLF
                          let lu, cu = findPos fsCrlf "match c with" 0 6
                          let! rUc = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph fsFile (mkParams fsFile (posRange lu cu) [||] None) CancellationToken.None |> within
                          let afterUc = applySingleFileAction fsFile fsCrlf (findAction "Generate union pattern match cases" (extractActions rUc))
                          Expect.isFalse (hasBareLf afterUc) "Union pattern match stub preserves CRLF without bare LF"

                          // 4. Record stub on CRLF
                          let lr, cr = findPos fsCrlf "{ A = 1 }" 0 2
                          let! rRec = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph fsFile (mkParams fsFile (posRange lr cr) [||] None) CancellationToken.None |> within
                          let afterRec = applySingleFileAction fsFile fsCrlf (findAction "Generate record stub" (extractActions rRec))
                          Expect.isFalse (hasBareLf afterRec) "Record stub preserves CRLF without bare LF"

                          // 5. Add binding to CRLF .fsi signature file
                          let ls, cs = findPos fsCrlf "newBinding" 0 3
                          let! rSig = CodeActions.provideWithCapabilities enabledConfig true versionNow scope graph fsFile (mkParams fsFile (posRange ls cs) [||] None) CancellationToken.None |> within
                          let addSigAct = findAction "Add binding to signature file" (extractActions rSig)
                          let _, sigEdits = (editsByPath addSigAct.Edit.Value)[normFsi]
                          let afterSig = applyLspEdits fsiCrlf sigEdits
                          Expect.isFalse (hasBareLf afterSig) ".fsi AddSigVal preserves CRLF without bare LF"
                      })
          } ]
