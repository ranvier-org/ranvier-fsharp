namespace Rfs.Server

open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open System.Threading.Tasks
open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.JsonRpc
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Checker

/// Ported from FSAC's `Helpers` (Common.fs)
module Helpers =
    let notImplemented<'t> = async.Return LspResult.notImplemented<'t>
    let ignoreNotification = async.Return(())

open Helpers

type RanvierServer(client: RanvierClient, host: GraphHost) =
    let mutable disposed = 0
    let configGate = obj ()
    let mutable config = FSharpConfig.Default
    let workspace = new Workspace(host, client)
    let documents = Documents(host)
    let publisher = new Publisher(host, workspace, client, documents, fun () -> Volatile.Read &config)
    // The hovers given on settled results, for answers from the last state while a check is pending
    let lastAnswers = Hover.LastAnswers()
    // The references and definitions given on settled states, for answers from the last state marked by the progress
    let lastUses = References.LastAnswers()
    let lastDefinitions = Definition.LastAnswers()
    // Each open file's shape being made for its latest parse, so the last-state path doesn't lex
    let preparing = System.Collections.Concurrent.ConcurrentDictionary<string, CancellationTokenSource>(StringComparer.OrdinalIgnoreCase)
    let notices = new RestartNotices(host, workspace, publisher, client)

    do
        workspace.OnLoaded(fun graph ->
            task {
                publisher.Loaded graph
                do! Hover.warm graph (workspace.FilesOf graph)
            } :> Task)

    /// Applies one batch of watched-file changes, off the graph thread; then ends `held`, the loading the batch's
    /// project changes held while they waited (FilesChanged holds it from its start)
    let applyWatched (changes: (string * FileChangeType) list) (held: IDisposable) : Task =
        task {
            try
                try
                    let applying = workspace.FilesChanged changes
                    held.Dispose()
                    do! applying

                    let diskSources =
                        [ for path, changeType in changes do
                              let ext = Path.GetExtension path

                              if
                                  changeType <> FileChangeType.Deleted
                                  && (String.Equals(ext, ".fs", StringComparison.OrdinalIgnoreCase)
                                      || String.Equals(ext, ".fsi", StringComparison.OrdinalIgnoreCase))
                                  && not (workspace.OwnersOf path).IsEmpty
                              then
                                  yield path ]

                    if not diskSources.IsEmpty then
                        publisher.DiskChanged diskSources
                with ex ->
                    eprintfn "[ranvierautocomplete] watched-file change failed: %O" ex
            finally
                held.Dispose()
        }

    // Graph thread: the watched-file changes not yet applied, and how many notifications brought them. A burst (a branch
    // switch, a git pull) is applied as one batch once the notifications have been quiet for a while, so each project
    // reloads once and the files changed settle once. A project-level change waiting there counts as loading, so a read
    // in one turn waits for the reload rather than answer from the graph it will replace.
    let watched = Collections.Generic.List<string * FileChangeType>()
    let mutable arrivals: Ranvier.Signal<int> = Unchecked.defaultof<_>
    let mutable holding: IDisposable = null
    let mutable watchDelay = RanvierServer.WatchDelay()

    let isProjectLevel (path: string) =
        let ext = Path.GetExtension path
        ext = ".fsproj" || ext = ".props" || ext = ".targets" || Path.GetFileName path = "project.assets.json"

    /// Graph thread: the debounce, made at the first notification
    let ensureWatch () =
        if isNull (box arrivals) then
            Ranvier.Api.createRoot (fun _ ->
                arrivals <- Ranvier.Api.createSignal 0
                let quiet = Ranvier.TimedCombinators.debounce watchDelay (fun () -> arrivals.Value) host.Graph

                Ranvier.Api.createEffect (fun () ->
                    if quiet.Value > 0 && watched.Count > 0 then
                        let batch = List.ofSeq watched
                        let held = if isNull holding then { new IDisposable with member _.Dispose() = () } else holding
                        holding <- null
                        watched.Clear()
                        Task.Run(fun () -> applyWatched batch held) |> ignore))
            |> ignore

    let mutable rootPath: string option = None
    let tokenCache = SemanticTokens.TokenDeltaCache()
    let mutable canPullConfiguration = false
    let mutable canRegisterWatchers = false
    let mutable canShowProgress = false
    let mutable canDocumentChanges = false
    let mutable progress: Progress option = None

    let updateConfig (token: JToken) =
        lock configGate (fun () ->
            let updated = FSharpConfig.UpdateFromJson (Volatile.Read &config) token
            Volatile.Write(&config, updated))

        publisher.NotifyConfigChanged()

    let pullConfiguration () =
        async {
            if canPullConfiguration then
                try
                    let req: ConfigurationParams =
                        { Items = [| { ScopeUri = None; Section = Some "FSharp" } |] }

                    match! (client :> ILspClient).WorkspaceConfiguration req with
                    | Ok items when not (isNull items) && items.Length > 0 ->
                        updateConfig items[0]
                    | _ -> ()
                with ex ->
                    eprintfn "[ranvierautocomplete] workspace/configuration failed: %O" ex
        }

    let dispose () =
        if Threading.Interlocked.Exchange(&disposed, 1) = 0 then
            // Before the outbox closes: a period shown gets its end
            progress |> Option.iter _.Dispose()
            notices.Dispose()

            // And sent before the shutdown is answered: the connection may close after the answer
            try
                publisher.Flushed().Wait(TimeSpan.FromSeconds 2.) |> ignore
            with _ ->
                ()

            publisher.Dispose()
            workspace.Dispose()
            host.Dispose()

    let plain (content: string) : LspResult<PlainNotification> = Ok { Content = content }

    // JSON-RPC's InternalError. LspResult.internalError in Ionide.LanguageServerProtocol 0.7.0 sends -32602 (InvalidParams)
    let internalError (message: string) : LspResult<'T> = Error(JsonRpc.Error.Create(-32603, message))

    let rec unwrapRootExn (e: exn) : exn =
        match e with
        | :? AggregateException as a when a.InnerExceptions.Count = 1 -> unwrapRootExn a.InnerException
        | e -> e

    let handleReadExn (what: string) (none: 'T) (ex: exn) : LspResult<'T> =
        match unwrapRootExn ex with
        | :? OperationCanceledException
        | :? ObjectDisposedException
        // The file's #line directives stayed another parse's for as long as a check waits for them
        | :? FSharp.Compiler.Declarations.SupersededLineDirectivesException -> Ok none
        | e ->
            eprintfn "[ranvierautocomplete] %s failed: %O" what e
            internalError e.Message

    /// A request that reads a source file: `none` when no project holds it, or the project goes away (a reload hides it
    /// while it loads again) or the server shuts down meanwhile. A request the client cancelled never gets to the handler:
    /// the library cancels the async itself. The edits before the request may still be queued; its answer is for the text
    /// they leave.

    let readingCore
        (allowSignature: bool)
        (what: string)
        (none: 'T)
        (path: unit -> string)
        (run: ProjectGraph -> string -> CancellationToken -> Task<'T>)
        : Async<LspResult<'T>> =
        async {
            let! ct = Async.CancellationToken

            try
                do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask
                let path = path ()

                let owners =
                    if not allowSignature && path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                        []
                    else
                        workspace.OwnersOf path

                match owners with
                | [] -> return Ok none
                | graph :: _ ->
                    let! answer = run graph path ct |> Async.AwaitTask
                    return Ok answer
            with ex ->
                return handleReadExn what none ex
        }

    let reading what none path run = readingCore false what none path run
    let readingAny what none path run = readingCore true what none path run

    let makeScope () : References.Scope =
        { OwnersOf = fun path -> workspace.OwnersNow(Path.GetFullPath path)
          ReferencingProjects = workspace.ReferencingNow
          Projects = workspace.ProjectsNow
          FilesOf = workspace.FilesNow
          IsLoading = workspace.LoadingNow }

    let readingItem
        (what: string)
        (none: 'T)
        (uri: unit -> string)
        (run: ProjectGraph -> CancellationToken -> Task<'T>)
        : Async<LspResult<'T>> =
        async {
            let! ct = Async.CancellationToken

            try
                do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask
                let fullPath = Path.GetFullPath(Paths.ofUri (uri ()))

                let! candidateOpt =
                    host.WhenSettled(
                        (fun () ->
                            if workspace.LoadingNow() then
                                ValueNone
                            else
                                let candidates =
                                    match workspace.OwnersNow fullPath with
                                    | [] -> workspace.ProjectsNow()
                                    | owners -> owners

                                match candidates with
                                | [] -> ValueSome None
                                | graph :: _ -> ValueSome(Some graph)),
                        ct
                    )
                    |> Async.AwaitTask

                match candidateOpt with
                | None -> return Ok none
                | Some graph ->
                    let! answer = run graph ct |> Async.AwaitTask
                    return Ok answer
            with ex ->
                return handleReadExn what none ex
        }

    /// FSAC's `AutomaticWorkspaceInit`: peek the root, load the single solution if there is one, else every project
    let automaticLoad (root: string) =
        task {
            try
                let cfg = Volatile.Read &config
                let found = WorkspacePeek.peek root cfg.WorkspaceModePeekDeepLevel cfg.ExcludeProjectDirectories
                let! _ = workspace.Load(WorkspacePeek.projectsOf found)
                ()
            with ex ->
                eprintfn "[ranvierautocomplete] automatic workspace load failed: %O" ex
        }

    /// Asks the client to report what changes on disk: only an editor knows about a `git pull` or a generator run. A client
    /// that can't register watchers reports what it chooses to, if anything, and the rest stays stale until a restart.
    let registerWatchers () =
        if canRegisterWatchers then
            let watcher (glob: string) = JObject(JProperty("globPattern", glob))

            let options =
                JObject(
                    JProperty(
                        "watchers",
                        JArray(
                            [ "**/*.{fs,fsi}"
                              "**/*.fsproj"
                              "**/*.props"
                              "**/*.targets"
                              "**/project.assets.json"
                              "**/*.{sln,slnx}"
                              "**/*" ]
                            |> List.map watcher
                        )
                    )
                )

            let registration: Registration =
                { Id = "rfs-watched-files"
                  Method = "workspace/didChangeWatchedFiles"
                  RegisterOptions = Some(options :> JToken) }

            async {
                try
                    match! client.ClientRegisterCapability { Registrations = [| registration |] } with
                    | Ok() -> ()
                    | Error e -> eprintfn "[ranvierautocomplete] the client refused the file watchers: %s" e.Message
                with ex ->
                    eprintfn "[ranvierautocomplete] registering the file watchers failed: %O" ex
            }
            |> Async.Start
        else
            eprintfn "[ranvierautocomplete] the client cannot register file watchers: changes on disk it does not report go unseen"

    /// How long watched-file notifications must be quiet before their changes are applied: `RFS_WATCH_DEBOUNCE_MS`,
    /// else 200 ms
    static member WatchDelay() : TimeSpan =
        match Environment.GetEnvironmentVariable "RFS_WATCH_DEBOUNCE_MS" with
        | null
        | "" -> TimeSpan.FromMilliseconds 200.
        | text ->
            match Double.TryParse text with
            | true, ms when ms >= 0. -> TimeSpan.FromMilliseconds ms
            | _ -> TimeSpan.FromMilliseconds 200.

    /// How long this server's watched-file notifications must be quiet; set before the first one. For tests.
    member internal _.WatchDelayForTests
        with set (delay: TimeSpan) = watchDelay <- delay

    member _.Client = client
    member _.Config: FSharpConfig = Volatile.Read &config
    member _.Host = host
    member _.Workspace = workspace
    member internal _.Documents = documents
    member internal _.Publisher = publisher
    member val internal FormattingRunnerOverride: Formatting.FormattingRunner option = None with get, set

    /// How many references and definitions were answered from the last state. For tests.
    member internal _.LastStateAnswers: int64 = lastUses.Answered + lastDefinitions.Answered

    /// How long something must be pending before the progress begins: set before `initialized`. For tests.
    member val internal ProgressBeginHold: TimeSpan = Progress.BeginHold with get, set

    /// Graph thread, read in a request's turn: a check is pending and the progress that marks a last-state answer has
    /// begun on the client
    member private _.MarkShown() : bool =
        match progress with
        | Some p -> p.ShowingNow() && publisher.CheckingNow()
        | None -> false

    /// Completes once the progress that marks a last-state answer shows
    member private _.WhenShown() : Task =
        match progress with
        | Some p -> p.WhenShown()
        | None -> Task.Delay Timeout.Infinite

    /// After a change: the shape of the file's next parse, made once it lands
    member private _.Prepare(path: string) : unit =
        // Only a file something held could answer from: else nothing reads the shape
        if canShowProgress && (lastUses.Names path || lastDefinitions.Names path || lastAnswers.HasAnswered path) then
            let cts = new CancellationTokenSource()

            preparing.AddOrUpdate(
                path,
                cts,
                fun _ (previous: CancellationTokenSource) ->
                    previous.Cancel()
                    cts
            )
            |> ignore

            for graph in workspace.OwnersOf path do
                if not (path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then
                    LastState.prepare lastAnswers graph path cts.Token |> ignore

    /// What the progress says now, or None when the client did not declare the capability. For tests.
    member internal _.ProgressStatus() : Task<ProgressStatus option> =
        match progress with
        | Some p -> task { let! s = p.Status() in return Some s }
        | None -> Task.FromResult None

    /// How many times the status the progress follows changed; 0 without the capability. For tests.
    member internal _.ProgressChanges() : Task<int> =
        match progress with
        | Some p -> p.Changes()
        | None -> Task.FromResult 0

    /// Exercises `handleReadExn` (unwrapping `AggregateException` and swallowing benign turn aborts). For tests.
    member internal _.HandleReadExn(what: string, none: 'T, ex: exn) : LspResult<'T> =
        handleReadExn what none ex

    member _.FSharpWorkspacePeek(p: WorkspacePeekRequest) : Async<LspResult<PlainNotification>> =
        async {
            try
                let cfg = Volatile.Read &config
                let deep = if p.Deep > 0 then p.Deep else cfg.WorkspaceModePeekDeepLevel
                let excluded =
                    if isNull p.ExcludedDirs then cfg.ExcludeProjectDirectories else List.ofArray p.ExcludedDirs

                let found = WorkspacePeek.peek p.Directory deep excluded
                return plain (Responses.workspacePeek found)
            with ex ->
                return internalError ex.Message
        }

    member _.FSharpWorkspaceLoad(p: WorkspaceLoadParms) : Async<LspResult<PlainNotification>> =
        async {
            try
                let projects = p.TextDocuments |> Array.map (fun t -> Paths.ofUri t.Uri) |> List.ofArray
                let! _ = workspace.Load projects |> Async.AwaitTask
                return plain (Responses.workspaceLoad true)
            with ex ->
                return handleReadExn "fsharp/workspaceLoad" { Content = Responses.workspaceLoad false } ex
        }

    /// `rfs/why`: why the declaration at the position was checked again. Waits like the queries do.
    member _.RfsWhy(p: WhyParams) : Async<LspResult<WhyAnswer option>> =
        reading "rfs/why" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            Why.at graph path (int p.Position.Line) (int p.Position.Character) ct)

    member _.FSharpProject(p: ProjectParms) : Async<LspResult<PlainNotification>> =
        async {
            try
                let projPath = Path.GetFullPath(Paths.ofUri p.Project.Uri)

                match workspace.SpecOf projPath with
                | Some spec -> return plain (Responses.project spec)
                | None ->
                    let! _ = workspace.Load [ projPath ] |> Async.AwaitTask

                    match workspace.SpecOf projPath with
                    | Some spec -> return plain (Responses.project spec)
                    | None -> return internalError $"Project '%s{projPath}' failed to load."
            with ex ->
                return internalError (unwrapRootExn ex).Message
        }

    member _.FSharpSignature(p: TextDocumentPositionParams) : Async<LspResult<PlainNotification option>> =
        readingAny "fsharp/signature" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            task {
                let! s = IonideEndpoints.signature graph path (int p.Position.Line) (int p.Position.Character) ct
                return s |> Option.map (fun c -> { Content = c })
            })

    member _.FSharpSignatureData(p: TextDocumentPositionParams) : Async<LspResult<PlainNotification option>> =
        readingAny "fsharp/signatureData" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            task {
                let! s = IonideEndpoints.signatureData graph path (int p.Position.Line) (int p.Position.Character) ct
                return s |> Option.map (fun c -> { Content = c })
            })

    member _.FSharpLineLens(p: ProjectParms) : Async<LspResult<PlainNotification option>> =
        readingAny "fsharp/lineLens" None (fun () -> Paths.ofUri p.Project.Uri) (fun graph path ct ->
            task {
                let! s = IonideEndpoints.lineLens graph path ct
                return s |> Option.map (fun c -> { Content = c })
            })

    member _.FSharpPipelineHint(p: PipelineHintRequest) : Async<LspResult<PlainNotification option>> =
        readingAny "fsharp/pipelineHint" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            task {
                let! s = IonideEndpoints.pipelineHint graph path ct
                return s |> Option.map (fun c -> { Content = c })
            })

    member _.FSharpF1Help(p: TextDocumentPositionParams) : Async<LspResult<PlainNotification option>> =
        readingAny "fsharp/f1Help" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            task {
                let! s = IonideEndpoints.f1Help graph path (int p.Position.Line) (int p.Position.Character) ct
                return s |> Option.map (fun c -> { Content = c })
            })

    member _.FSharpDocumentation(p: TextDocumentPositionParams) : Async<LspResult<PlainNotification option>> =
        readingAny "fsharp/documentation" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            task {
                let! s =
                    IonideEndpoints.documentation
                        workspace.OwnersOf
                        graph
                        path
                        (int p.Position.Line)
                        (int p.Position.Character)
                        ct

                return s |> Option.map (fun c -> { Content = c })
            })

    member _.FSharpDocumentationSymbol(p: DocumentationForSymbolRequest) : Async<LspResult<PlainNotification option>> =
        async {
            let! ct = Async.CancellationToken

            try
                do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask

                let! s =
                    IonideEndpoints.documentationSymbol
                        host
                        workspace.ProjectsNow
                        workspace.LoadingNow
                        p.XmlSig
                        p.Assembly
                        ct
                    |> Async.AwaitTask

                return Ok(s |> Option.map (fun c -> { Content = c }))
            with ex ->
                return handleReadExn "fsharp/documentationSymbol" None ex
        }

    member _.FSharpDocumentationGenerator(p: TextDocumentPositionParams) : Async<LspResult<unit>> =
        async {
            let! res =
                readingAny "fsharp/documentationGenerator" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                    IonideEndpoints.documentationGeneratorWithCapabilities
                        canDocumentChanges
                        documents.TryVersionNow
                        graph
                        path
                        (int p.Position.Line)
                        (int p.Position.Character)
                        ct)

            match res with
            | Error e -> return Error e
            | Ok None -> return Ok()
            | Ok(Some edit) ->
                try
                    let! _ =
                        (client :> ILspClient).WorkspaceApplyEdit
                            { Label = Some "Generate Xml Documentation"
                              Edit = edit }

                    return Ok()
                with ex ->
                    return handleReadExn "fsharp/documentationGenerator" () ex
        }

    member _.FSharpCompilerLocation(_: obj) : Async<LspResult<PlainNotification>> =
        async.Return(plain (IonideEndpoints.compilerLocation ()))

    member _.FSharpLoadAnalyzers(_: obj) : Async<LspResult<unit>> =
        async.Return(Ok())

    interface ILspServer with
        member _.Initialize(p: InitializeParams) =
            async {
                rootPath <-
                    match p.RootUri with
                    | Some uri -> Some(Paths.ofUri uri)
                    | None -> p.RootPath |> Option.map Path.GetFullPath

                match p.InitializationOptions with
                | Some options -> updateConfig options
                | None -> ()

                canPullConfiguration <-
                    p.Capabilities.Workspace
                    |> Option.bind _.Configuration
                    |> Option.defaultValue false

                canRegisterWatchers <-
                    p.Capabilities.Workspace
                    |> Option.bind _.DidChangeWatchedFiles
                    |> Option.bind _.DynamicRegistration
                    |> Option.defaultValue false

                canShowProgress <-
                    p.Capabilities.Window
                    |> Option.bind _.WorkDoneProgress
                    |> Option.defaultValue false

                canDocumentChanges <-
                    p.Capabilities.Workspace
                    |> Option.bind _.WorkspaceEdit
                    |> Option.bind _.DocumentChanges
                    |> Option.defaultValue false

                let inlineValueCap =
                    match (Volatile.Read &config).InlineValues.Enabled with
                    | Some false -> None
                    | _ -> Some(U3.C1 true)

                let capabilities =
                    { ServerCapabilities.Default with
                        TextDocumentSync =
                            Some
                            <| U2.C1
                                { TextDocumentSyncOptions.Default with
                                    OpenClose = Some true
                                    Change = Some TextDocumentSyncKind.Incremental }
                        HoverProvider = Some(U2.C1 true)
                        DefinitionProvider = Some(U2.C1 true)
                        DeclarationProvider = Some(U3.C1 true)
                        TypeDefinitionProvider = Some(U3.C1 true)
                        ImplementationProvider = Some(U3.C1 true)
                        ReferencesProvider = Some(U2.C1 true)
                        DocumentHighlightProvider = Some(U2.C1 true)
                        DocumentSymbolProvider = Some(U2.C1 true)
                        WorkspaceSymbolProvider =
                            Some(
                                U2.C2
                                    { ResolveProvider = Some true
                                      WorkDoneProgress = None }
                            )
                        CallHierarchyProvider = Some(U3.C1 true)
                        TypeHierarchyProvider = Some(U3.C1 true)
                        FoldingRangeProvider = Some(U3.C1 true)
                        SelectionRangeProvider = Some(U3.C1 true)
                        RenameProvider =
                            Some(
                                U2.C2
                                    { PrepareProvider = Some true
                                      WorkDoneProgress = None }
                            )
                        InlayHintProvider =
                            Some(
                                U3.C2
                                    { ResolveProvider = Some false
                                      WorkDoneProgress = None }
                            )
                        CodeActionProvider =
                            Some(
                                U2.C2
                                    { CodeActionKinds =
                                        Some
                                            [| CodeActionKind.QuickFix
                                               CodeActionKind.Refactor
                                               CodeActionKind.RefactorRewrite
                                               "source.organizeImports" |]
                                      ResolveProvider = Some false
                                      WorkDoneProgress = None }
                            )
                        DocumentFormattingProvider = Some(U2.C1 true)
                        DocumentRangeFormattingProvider = Some(U2.C1 true)
                        CodeLensProvider =
                            Some
                                { ResolveProvider = Some true
                                  WorkDoneProgress = None }
                        InlineValueProvider = inlineValueCap
                        SignatureHelpProvider =
                            Some
                                { TriggerCharacters = Some [| "("; ","; " " |]
                                  RetriggerCharacters = Some [| ","; ")"; " " |]
                                  WorkDoneProgress = None }
                        CompletionProvider =
                            Some
                                { TriggerCharacters = Some [| "."; "'"; "{" |]
                                  AllCommitCharacters = None
                                  ResolveProvider = Some true
                                  CompletionItem = None
                                  WorkDoneProgress = None }
                        SemanticTokensProvider =
                            Some(
                                U2.C1
                                    { Legend = SemanticTokens.legend
                                      Range = Some(U2.C1 true)
                                      Full = Some(U2.C2 { Delta = Some true })
                                      WorkDoneProgress = Some false }
                            ) }

                return
                    Ok
                        { Capabilities = capabilities
                          ServerInfo = Some { Name = "ranvierautocomplete"; Version = Some(RanvierServer.Version) } }
            }

        member this.Initialized(_) =
            async {
                if canShowProgress then
                    progress <- Some(new Progress(host, workspace, publisher, client, this.ProgressBeginHold))

                registerWatchers ()

                async {
                    do! pullConfiguration ()

                    match rootPath, (Volatile.Read &config).AutomaticWorkspaceInit with
                    | Some root, true ->
                        // The load takes as long as the cold checks; the connection keeps serving meanwhile
                        automaticLoad root |> ignore
                    | _ -> ()
                }
                |> Async.Start
            }

        member _.Shutdown() =
            async {
                dispose ()
                return Ok()
            }

        member _.Exit() = ignoreNotification

        // These three queue their work before returning, not inside the async the library starts on the thread pool,
        // so the queue holds the editor's order.
        member _.TextDocumentDidOpen(p: DidOpenTextDocumentParams) =
            try
                let d = p.TextDocument
                publisher.Open(d.Uri, Paths.ofUri d.Uri, d.Version, d.Text)
            with ex ->
                eprintfn "[ranvierautocomplete] didOpen failed: %O" ex

            ignoreNotification

        member this.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
            try
                let apply (text: string) (change: TextDocumentContentChangeEvent) =
                    match change with
                    | U2.C1 partial ->
                        let position (x: Position) = { Rfs.Lsp.Line = int x.Line; Rfs.Lsp.Character = int x.Character }

                        Rfs.Lsp.Document.apply
                            text
                            { Range = { Start = position partial.Range.Start; End = position partial.Range.End }
                              Text = partial.Text }
                    | U2.C2 whole -> whole.Text

                let version = p.TextDocument.Version
                let path = Paths.ofUri p.TextDocument.Uri
                publisher.Change(path, version, (fun text -> p.ContentChanges |> Array.fold apply text))
                this.Prepare path
            with ex ->
                eprintfn "[ranvierautocomplete] didChange failed: %O" ex

            ignoreNotification

        member _.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
            try
                let path = Paths.ofUri p.TextDocument.Uri
                tokenCache.Remove path
                publisher.Close path
            with ex ->
                eprintfn "[ranvierautocomplete] didClose failed: %O" ex

            ignoreNotification

        member _.TextDocumentDidSave(_) = ignoreNotification

        member _.WorkspaceDidChangeConfiguration(p: DidChangeConfigurationParams) =
            try
                if not (isNull (box p)) && not (isNull p.Settings) then
                    updateConfig p.Settings

                pullConfiguration () |> Async.Start
            with ex ->
                eprintfn "[ranvierautocomplete] didChangeConfiguration failed: %O" ex

            ignoreNotification
        member _.WorkspaceDidChangeWatchedFiles(p: DidChangeWatchedFilesParams) =
            async {
                try
                    let changes =
                        [ for c in p.Changes do
                              let path = Paths.ofUri c.Uri

                              if workspace.IsRelevantWatchedChange(path, c.Type) then
                                  yield path, c.Type ]

                    if not changes.IsEmpty then
                        host.Post(fun () ->
                            ensureWatch ()

                            if isNull holding && changes |> List.exists (fst >> isProjectLevel) then
                                holding <- workspace.HoldLoading()

                            watched.AddRange changes
                            arrivals.Value <- arrivals.Value + 1)
                with ex ->
                    eprintfn "[ranvierautocomplete] watched-file change failed: %O" ex
            }

        member _.CallHierarchyIncomingCalls(p: CallHierarchyIncomingCallsParams) =
            let scope = makeScope ()

            readingItem "callHierarchy/incomingCalls" None (fun () -> p.Item.Uri) (fun graph ct ->
                CallHierarchy.incoming scope graph p.Item ct)

        member _.CallHierarchyOutgoingCalls(p: CallHierarchyOutgoingCallsParams) =
            let scope = makeScope ()

            readingItem "callHierarchy/outgoingCalls" None (fun () -> p.Item.Uri) (fun graph ct ->
                CallHierarchy.outgoing scope graph p.Item ct)
        member _.CodeActionResolve(_) = notImplemented
        member _.CodeLensResolve(lens: CodeLens) =
            async {
                let! ct = Async.CancellationToken

                try
                    do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask
                    let scope = makeScope ()
                    let! resolved = CodeLens.resolve scope workspace.OwnersOf lens ct |> Async.AwaitTask
                    return Ok resolved
                with ex ->
                    return handleReadExn "codeLens/resolve" lens ex
            }
        member _.CompletionItemResolve(item: CompletionItem) =
            match Completion.positionOf item with
            | None -> async.Return(Ok item)
            | Some(path, line, character) ->
                reading "completionItem/resolve" item (fun () -> path) (fun graph path ct ->
                    Completion.resolve graph item path line character ct)

        member _.DocumentLinkResolve(_) = notImplemented
        member _.InlayHintResolve(_) = notImplemented
        member _.TextDocumentCodeAction(p: CodeActionParams) =
            let scope = makeScope ()

            readingAny "codeAction" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                CodeActions.provideWithCapabilities
                    (Volatile.Read &config)
                    canDocumentChanges
                    documents.TryVersionNow
                    scope
                    graph
                    path
                    p
                    ct)
        member _.TextDocumentCodeLens(p: CodeLensParams) =
            readingAny "codeLens" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                CodeLens.lenses (Volatile.Read &config) graph path ct)
        member _.TextDocumentColorPresentation(_) = notImplemented
        member _.TextDocumentCompletion(p: CompletionParams) =
            reading "completion" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let kwEnabled = (Volatile.Read &config).KeywordsAutocomplete

                    let! items =
                        Completion.atWithKeywords
                            kwEnabled
                            graph
                            path
                            (int p.Position.Line)
                            (int p.Position.Character)
                            ct

                    return
                        Some(
                            U2.C2
                                { IsIncomplete = false
                                  Items = items
                                  ItemDefaults = None }
                        )
                })

        member _.TextDocumentDeclaration(p: DeclarationParams) =
            readingAny "declaration" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let! found = Declaration.at workspace.OwnersOf graph path (int p.Position.Line) (int p.Position.Character) ct

                    return
                        found
                        |> Option.map (fun r ->
                            let position (line: int) (column: int) = { Line = uint32 (line - 1); Character = uint32 column }

                            U2.C1(
                                U2.C1
                                    { Uri = Paths.toUri r.FileName
                                      Range = { Start = position r.StartLine r.StartColumn; End = position r.EndLine r.EndColumn } }
                            ))
                })

        member this.TextDocumentDefinition(p: DefinitionParams) =
            readingAny "definition" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let line, character = int p.Position.Line, int p.Position.Character

                    let! found =
                        if canShowProgress && not (path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then
                            Definition.answer lastDefinitions lastAnswers workspace.LoadingNow this.MarkShown this.WhenShown workspace.ProjectsNow workspace.OwnersOf graph path line character ct
                        else
                            Definition.at workspace.OwnersOf graph path line character ct

                    return
                        found
                        |> Option.map (fun r ->
                            let position (line: int) (column: int) = { Line = uint32 (line - 1); Character = uint32 column }

                            U2.C1(
                                U2.C1
                                    { Uri = Paths.toUri r.FileName
                                      Range = { Start = position r.StartLine r.StartColumn; End = position r.EndLine r.EndColumn } }
                            ))
                })

        member _.TextDocumentDiagnostic(_) = notImplemented
        member _.TextDocumentDocumentColor(_) = notImplemented
        member _.TextDocumentDocumentHighlight(p: DocumentHighlightParams) =
            readingAny "documentHighlight" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                DocumentHighlight.at graph path (int p.Position.Line) (int p.Position.Character) ct)
        member _.TextDocumentDocumentLink(_) = notImplemented
        member _.TextDocumentDocumentSymbol(p: DocumentSymbolParams) =
            readingAny "documentSymbol" None (fun () -> Paths.ofUri p.TextDocument.Uri) DocumentSymbols.hierarchical

        member _.TextDocumentFoldingRange(p: FoldingRangeParams) =
            readingAny "foldingRange" None (fun () -> Paths.ofUri p.TextDocument.Uri) Folding.ranges

        member this.TextDocumentFormatting(p: DocumentFormattingParams) =
            readingAny "formatting" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                Formatting.formatDocument this.FormattingRunnerOverride graph path p.Options ct)
        member _.TextDocumentHover(p: HoverParams) =
            reading "hover" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let! text =
                        Hover.answer lastAnswers workspace.LoadingNow workspace.OwnersOf graph path (int p.Position.Line) (int p.Position.Character) ct

                    return
                        text
                        |> Option.map (fun t ->
                            { Contents = U3.C1 { Kind = MarkupKind.Markdown; Value = t }
                              Range = None })
                })

        member _.TextDocumentImplementation(p: ImplementationParams) =
            async {
                let scope: References.Scope =
                    { OwnersOf = fun path -> workspace.OwnersNow(Path.GetFullPath path)
                      ReferencingProjects = workspace.ReferencingNow
                      Projects = workspace.ProjectsNow
                      FilesOf = workspace.FilesNow
                      IsLoading = workspace.LoadingNow }

                let moved = Error(JsonRpc.Error.Create(-32801, "The files changed while implementations were read"))

                let! answer =
                    readingAny "implementation" (Ok None) (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                        task {
                            let line, character = int p.Position.Line, int p.Position.Character

                            match! Implementation.find scope graph path line character ct with
                            | Error() -> return moved
                            | Ok ranges ->
                                let position (line: int) (column: int) = { Line = uint32 (line - 1); Character = uint32 column }

                                let mapped =
                                    ranges
                                    |> Array.map (fun r ->
                                        { Uri = Paths.toUri r.FileName
                                          Range = { Start = position r.StartLine r.StartColumn; End = position r.EndLine r.EndColumn } })

                                return
                                    match mapped with
                                    | [||] -> Ok None
                                    | [| single |] -> Ok(Some(U2.C1(U2.C1 single)))
                                    | multiple -> Ok(Some(U2.C1(U2.C2 multiple)))
                        })

                match answer with
                | Ok(Ok None) when workspace.IsLoading -> return moved
                | _ -> return answer |> Result.bind id
            }
        member _.TextDocumentInlayHint(p: InlayHintParams) =
            readingAny "inlayHint" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                InlayHints.provide (Volatile.Read &config).InlayHints graph path p.Range ct)

        member _.TextDocumentInlineValue(p: InlineValueParams) =
            readingAny "inlineValue" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                InlineValue.provide (Volatile.Read &config).InlineValues graph path p.Range ct)

        member _.TextDocumentLinkedEditingRange(_) = notImplemented
        member _.TextDocumentMoniker(_) = notImplemented
        member _.TextDocumentOnTypeFormatting(_) = notImplemented
        member _.TextDocumentPrepareCallHierarchy(p: CallHierarchyPrepareParams) =
            let scope = makeScope ()

            readingAny "prepareCallHierarchy" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                CallHierarchy.prepare scope graph path (int p.Position.Line) (int p.Position.Character) ct)

        member _.TextDocumentPrepareRename(p: PrepareRenameParams) =
            readingAny "prepareRename" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                Rename.prepare
                    (fun f -> workspace.OwnersNow(Path.GetFullPath f))
                    graph
                    path
                    (int p.Position.Line)
                    (int p.Position.Character)
                    ct)

        member _.TextDocumentPrepareTypeHierarchy(p: TypeHierarchyPrepareParams) =
            let scope = makeScope ()

            readingAny "prepareTypeHierarchy" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                TypeHierarchy.prepare scope graph path (int p.Position.Line) (int p.Position.Character) ct)

        member this.TextDocumentRangeFormatting(p: DocumentRangeFormattingParams) =
            readingAny "rangeFormatting" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                Formatting.formatRange this.FormattingRunnerOverride graph path p.Range p.Options ct)
        member this.TextDocumentReferences(p: ReferenceParams) =
            async {
                // Read on the graph thread, tracked, in the turn that reads the files
                let scope: References.Scope =
                    { OwnersOf = fun path -> workspace.OwnersNow(Path.GetFullPath path)
                      ReferencingProjects = workspace.ReferencingNow
                      Projects = workspace.ProjectsNow
                      FilesOf = workspace.FilesNow
                      IsLoading = workspace.LoadingNow }

                let moved = Error(JsonRpc.Error.Create(-32801, "The files changed while references were read"))

                let! answer =
                    reading "references" (Ok None) (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                        task {
                            let line, character = int p.Position.Line, int p.Position.Character

                            let! found =
                                if canShowProgress then
                                    References.answer lastUses lastAnswers workspace.LoadingNow this.MarkShown this.WhenShown scope graph path line character ct
                                else
                                    References.find scope graph path line character ct

                            match found with
                            // LSP's ContentModified. Ionide's bundled client shows nothing for it and doesn't ask again
                            | References.Answer.Moved -> return moved
                            | References.Answer.Found uses ->
                                let position (line: int) (column: int) = { Line = uint32 (line - 1); Character = uint32 column }

                                return
                                    Ok(
                                        Some
                                            [| for struct (r, isDefinition) in uses do
                                                   if p.Context.IncludeDeclaration || not isDefinition then
                                                       { Uri = Paths.toUri r.FileName
                                                         Range = { Start = position r.StartLine r.StartColumn; End = position r.EndLine r.EndColumn } } |]
                                    )
                        })

                // No project holds the file, which during a load says nothing of the file
                match answer with
                | Ok(Ok None) when workspace.IsLoading -> return moved
                | _ -> return answer |> Result.bind id
            }
        member _.TextDocumentRename(p: RenameParams) =
            async {
                let scope = makeScope ()

                let! res =
                    readingAny "rename" (Ok None) (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                        Rename.renameWithCapabilities
                            canDocumentChanges
                            documents.TryVersionNow
                            scope
                            graph
                            path
                            (int p.Position.Line)
                            (int p.Position.Character)
                            p.NewName
                            ct)

                match res with
                | Error e -> return Error e
                | Ok(Error msg) -> return Error(JsonRpc.Error.Create(-32602, msg))
                | Ok(Ok editOpt) -> return Ok editOpt
            }
        member _.TextDocumentSelectionRange(p: SelectionRangeParams) =
            readingAny "selectionRange" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                SelectionRange.atPositions graph path p.Positions ct)
        member _.TextDocumentSemanticTokensFull(p: SemanticTokensParams) =
            readingAny "semanticTokens/full" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                SemanticTokens.full tokenCache graph path ct)

        member _.TextDocumentSemanticTokensFullDelta(p: SemanticTokensDeltaParams) =
            readingAny "semanticTokens/full/delta" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                SemanticTokens.fullDelta tokenCache graph path p.PreviousResultId ct)

        member _.TextDocumentSemanticTokensRange(p: SemanticTokensRangeParams) =
            readingAny "semanticTokens/range" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                SemanticTokens.range graph path p.Range ct)
        member _.TextDocumentSignatureHelp(p: SignatureHelpParams) =
            reading "signatureHelp" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                let triggerChar =
                    p.Context
                    |> Option.bind _.TriggerCharacter
                    |> Option.bind (fun s -> if String.IsNullOrEmpty s then None else Some s[0])

                SignatureHelp.atWithTrigger triggerChar workspace.OwnersOf graph path (int p.Position.Line) (int p.Position.Character) ct)

        member _.TextDocumentTypeDefinition(p: TypeDefinitionParams) =
            readingAny "typeDefinition" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let! found = TypeDefinition.at workspace.OwnersOf graph path (int p.Position.Line) (int p.Position.Character) ct

                    return
                        found
                        |> Option.map (fun r ->
                            let position (line: int) (column: int) = { Line = uint32 (line - 1); Character = uint32 column }

                            U2.C1(
                                U2.C1
                                    { Uri = Paths.toUri r.FileName
                                      Range = { Start = position r.StartLine r.StartColumn; End = position r.EndLine r.EndColumn } }
                            ))
                })
        member _.TextDocumentWillSave(_) = ignoreNotification
        member _.TextDocumentWillSaveWaitUntil(_) = notImplemented
        member _.TypeHierarchySubtypes(p: TypeHierarchySubtypesParams) =
            let scope = makeScope ()

            readingItem "typeHierarchy/subtypes" None (fun () -> p.Item.Uri) (fun graph ct ->
                TypeHierarchy.subtypes scope graph p.Item ct)

        member _.TypeHierarchySupertypes(p: TypeHierarchySupertypesParams) =
            let scope = makeScope ()

            readingItem "typeHierarchy/supertypes" None (fun () -> p.Item.Uri) (fun graph ct ->
                TypeHierarchy.supertypes scope graph p.Item ct)

        member _.WindowWorkDoneProgressCancel(_) = ignoreNotification
        member _.WorkspaceDiagnostic(_) = notImplemented
        member _.WorkspaceDidChangeWorkspaceFolders(_) = ignoreNotification
        member _.WorkspaceDidCreateFiles(_) = ignoreNotification
        member _.WorkspaceDidDeleteFiles(_) = ignoreNotification
        member _.WorkspaceDidRenameFiles(_) = ignoreNotification
        member _.WorkspaceExecuteCommand(_) = notImplemented

        member _.WorkspaceSymbol(p: WorkspaceSymbolParams) =
            async {
                let! ct = Async.CancellationToken

                try
                    do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask
                    let query = if isNull p.Query then "" else p.Query

                    let! answer =
                        WorkspaceSymbols.search
                            host
                            workspace.ProjectsNow
                            workspace.LoadingNow
                            workspace.FilesNow
                            query
                            ct
                        |> Async.AwaitTask

                    return Ok answer
                with ex ->
                    return handleReadExn "workspace/symbol" (Some(U2.C2 [||])) ex
            }

        member _.WorkspaceSymbolResolve(sym: WorkspaceSymbol) =
            async.Return(Ok(WorkspaceSymbols.resolve sym))
        member _.WorkspaceWillCreateFiles(_) = notImplemented
        member _.WorkspaceWillDeleteFiles(_) = notImplemented
        member _.WorkspaceWillRenameFiles(_) = notImplemented
        member _.NotebookDocumentDidOpen(_) = ignoreNotification
        member _.NotebookDocumentDidClose(_) = ignoreNotification
        member _.NotebookDocumentDidSave(_) = ignoreNotification
        member _.NotebookDocumentDidChange(_) = ignoreNotification
        member _.CancelRequest(_) = ignoreNotification
        member _.Progress(_) = ignoreNotification
        member _.SetTrace(_) = ignoreNotification

    static member val Version: string =
        let asm = typeof<RanvierServer>.Assembly

        match Attribute.GetCustomAttribute(asm, typeof<Reflection.AssemblyInformationalVersionAttribute>) with
        | :? Reflection.AssemblyInformationalVersionAttribute as attr when not (String.IsNullOrWhiteSpace attr.InformationalVersion) ->
            attr.InformationalVersion.Split('+')[0]
        | _ ->
            match asm.GetName().Version with
            | null -> "0.1.0"
            | v -> v.ToString()

    interface IDisposable with
        member _.Dispose() = dispose ()

/// <summary>
/// Reads a stream on a thread of its own. Standard input and anonymous pipes have no asynchronous read: `ReadAsync` blocks
/// the pool thread it runs on, until the next message. The connection's dispatch is queued on that thread's local queue
/// just before it blocks there, and other pool threads only take from a local queue when the global one is empty, so
/// while the pool stays busy no message that was read is handled. Here `ReadAsync` holds no pool thread, and that thread
/// goes on to run what it queued.
/// </summary>
/// <remarks>Does not own the stream. A read cancelled while it is blocked loses what it then reads.</remarks>
type internal ThreadReadStream(inner: Stream) =
    inherit Stream()

    let reads = new BlockingCollection<struct (Memory<byte> * TaskCompletionSource<int>)>()

    let reader =
        Thread(
            (fun () ->
                for struct (buffer, finished) in reads.GetConsumingEnumerable() do
                    try
                        finished.TrySetResult(inner.Read buffer.Span) |> ignore
                    with ex ->
                        finished.TrySetException ex |> ignore),
            IsBackground = true,
            Name = "server input"
        )

    do reader.Start()

    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Flush() = ()
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(_: byte[], _: int, _: int) = raise (NotSupportedException())
    override _.Read(buffer: byte[], offset: int, count: int) = inner.Read(buffer, offset, count)

    override _.ReadAsync(buffer: Memory<byte>, ct: CancellationToken) : ValueTask<int> =
        let finished = TaskCompletionSource<int>(TaskCreationOptions.RunContinuationsAsynchronously)
        let registration = ct.Register(fun () -> finished.TrySetCanceled ct |> ignore)
        finished.Task.ContinueWith((fun (_: Task<int>) -> registration.Dispose()), TaskScheduler.Default) |> ignore
        reads.Add(struct (buffer, finished))
        ValueTask<int> finished.Task

    override this.ReadAsync(buffer: byte[], offset: int, count: int, ct: CancellationToken) : Task<int> =
        this.ReadAsync(Memory(buffer, offset, count), ct).AsTask()

    override _.Dispose(disposing: bool) =
        if disposing then
            reads.CompleteAdding()

        base.Dispose disposing

module Server =
    /// The methods this server answers. Every other method gets the library's MethodNotFound error.
    let private implemented =
        set
            [ "initialize"
              "initialized"
              "shutdown"
              "exit"
              "textDocument/didOpen"
              "textDocument/didChange"
              "textDocument/didClose"
              "textDocument/didSave"
              "workspace/didChangeConfiguration"
              "workspace/didChangeWatchedFiles"
              "textDocument/hover"
              "textDocument/definition"
              "textDocument/declaration"
              "textDocument/typeDefinition"
              "textDocument/implementation"
              "textDocument/references"
              "textDocument/documentHighlight"
              "textDocument/signatureHelp"
              "textDocument/completion"
              "completionItem/resolve"
              "textDocument/documentSymbol"
              "workspace/symbol"
              "workspaceSymbol/resolve"
              "textDocument/prepareCallHierarchy"
              "callHierarchy/incomingCalls"
              "callHierarchy/outgoingCalls"
              "textDocument/prepareTypeHierarchy"
              "typeHierarchy/supertypes"
              "typeHierarchy/subtypes"
              "textDocument/foldingRange"
              "textDocument/selectionRange"
              "textDocument/prepareRename"
              "textDocument/rename"
              "textDocument/inlayHint"
              "textDocument/codeAction"
              "textDocument/formatting"
              "textDocument/rangeFormatting"
              "textDocument/codeLens"
              "codeLens/resolve"
              "textDocument/inlineValue"
              "textDocument/semanticTokens/full"
              "textDocument/semanticTokens/full/delta"
              "textDocument/semanticTokens/range" ]

    let private handlings () =
        defaultRequestHandlings ()
        |> Map.filter (fun name _ -> implemented.Contains name)
        |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspacePeek p))
        |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspaceLoad p))
        |> Map.add "fsharp/project" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpProject p))
        |> Map.add "fsharp/signature" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpSignature p))
        |> Map.add "fsharp/signatureData" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpSignatureData p))
        |> Map.add "fsharp/lineLens" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpLineLens p))
        |> Map.add "fsharp/pipelineHint" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpPipelineHint p))
        |> Map.add "fsharp/f1Help" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpF1Help p))
        |> Map.add "fsharp/documentation" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpDocumentation p))
        |> Map.add "fsharp/documentationSymbol" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpDocumentationSymbol p))
        |> Map.add "fsharp/documentationGenerator" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpDocumentationGenerator p))
        |> Map.add "fsharp/compilerLocation" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpCompilerLocation p))
        |> Map.add "fsharp/loadAnalyzers" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpLoadAnalyzers p))
        |> Map.add "rfs/why" (serverRequestHandling (fun (s: RanvierServer) p -> s.RfsWhy p))

    /// Runs the server, telling `created` about the server the connection makes. For tests.
    let internal runWith (created: RanvierServer -> unit) (input: Stream) (output: Stream) : int =
        use reading = new ThreadReadStream(input)

        let reason =
            start
                (handlings ())
                reading
                output
                (fun (notify, request) -> new RanvierClient(notify, request))
                (fun client ->
                    let server = new RanvierServer(client, GraphHost.Start "server")
                    created server
                    server)
                defaultRpc

        int reason

    let run (input: Stream) (output: Stream) : int = runWith ignore input output
