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
    let workspace = new Workspace(host, client)
    let documents = Documents(host)
    let publisher = new Publisher(host, workspace, client, documents)
    let notices = new RestartNotices(host, workspace, publisher, client)

    do
        workspace.OnLoaded(fun graph ->
            publisher.Loaded graph
            Hover.warm graph (workspace.FilesOf graph) |> ignore)

    /// Applies one batch of watched-file changes, off the graph thread; then ends `held`, the loading the batch's
    /// project changes held while they waited (FilesChanged holds it from its start)
    let applyWatched (changes: (string * bool) list) (held: IDisposable) : Task =
        task {
            try
                try
                    let applying = workspace.FilesChanged changes
                    held.Dispose()
                    do! applying
                    publisher.DiskChanged [ for path, deleted in changes do if not deleted then yield path ]
                with ex ->
                    eprintfn "[ranvierautocomplete] watched-file change failed: %O" ex
            finally
                held.Dispose()
        }

    // Graph thread: the watched-file changes not yet applied, and how many notifications brought them. A burst (a branch
    // switch, a git pull) is applied as one batch once the notifications have been quiet for a while, so each project
    // reloads once and the files changed settle once. A project-level change waiting there counts as loading, so a read
    // in one turn waits for the reload rather than answer from the graph it will replace.
    let watched = Collections.Generic.List<string * bool>()
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
    let mutable automaticInit = false
    let mutable peekDepth = 2
    let mutable excludedDirs: string list = [ ".git"; "node_modules"; ".ionide" ]
    let mutable canRegisterWatchers = false
    let mutable canShowProgress = false
    let mutable progress: Progress option = None

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

    /// A request that reads a source file: `none` when no project holds it, or the project goes away (a reload hides it
    /// while it loads again) or the server shuts down meanwhile. A request the client cancelled never gets to the handler:
    /// the library cancels the async itself. The edits before the request may still be queued; its answer is for the text
    /// they leave.

    let reading (what: string) (none: 'T) (path: unit -> string) (run: ProjectGraph -> string -> CancellationToken -> Task<'T>) : Async<LspResult<'T>> =
        async {
            let! ct = Async.CancellationToken

            try
                do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask
                let path = path ()

                match (if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then [] else workspace.OwnersOf path) with
                | [] -> return Ok none
                | graph :: _ ->
                    let! answer = run graph path ct |> Async.AwaitTask
                    return Ok answer
            with ex ->
                let rec root (e: exn) =
                    match e with
                    | :? AggregateException as a when a.InnerExceptions.Count = 1 -> root a.InnerException
                    | e -> e

                match root ex with
                | :? OperationCanceledException
                | :? ObjectDisposedException
                // The file's #line directives stayed another parse's for as long as a check waits for them
                | :? FSharp.Compiler.Declarations.SupersededLineDirectivesException -> return Ok none
                | e ->
                    eprintfn "[ranvierautocomplete] %s failed: %O" what e
                    return internalError e.Message
        }

    /// FSAC's `AutomaticWorkspaceInit`: peek the root, load the single solution if there is one, else every project
    let automaticLoad (root: string) =
        task {
            try
                let found = WorkspacePeek.peek root peekDepth excludedDirs
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
                            [ "**/*.{fs,fsi}"; "**/*.fsproj"; "**/*.props"; "**/*.targets"; "**/project.assets.json" ]
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
    member _.Host = host
    member _.Workspace = workspace
    member internal _.Documents = documents
    member internal _.Publisher = publisher

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

    member _.FSharpWorkspacePeek(p: WorkspacePeekRequest) : Async<LspResult<PlainNotification>> =
        async {
            try
                let found = WorkspacePeek.peek p.Directory p.Deep (List.ofArray p.ExcludedDirs)
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
                return internalError ex.Message
        }

    /// `rfs/why`: why the declaration at the position was checked again. Waits like the queries do.
    member _.RfsWhy(p: WhyParams) : Async<LspResult<WhyAnswer option>> =
        reading "rfs/why" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
            Why.at graph path (int p.Position.Line) (int p.Position.Character) ct)

    interface ILspServer with
        member _.Initialize(p: InitializeParams) =
            async {
                rootPath <-
                    match p.RootUri with
                    | Some uri -> Some(Paths.ofUri uri)
                    | None -> p.RootPath |> Option.map Path.GetFullPath

                match p.InitializationOptions with
                | Some options when options.HasValues ->
                    match options["AutomaticWorkspaceInit"] with
                    | null -> ()
                    | v -> automaticInit <- v.ToObject<bool>()

                    match options["WorkspaceModePeekDeepLevel"] with
                    | null -> ()
                    | v -> peekDepth <- v.ToObject<int>()

                    match options["ExcludeProjectDirectories"] with
                    | null -> ()
                    | v -> excludedDirs <- [ for d in v -> d.ToObject<string>() ]
                | _ -> ()

                canRegisterWatchers <-
                    p.Capabilities.Workspace
                    |> Option.bind _.DidChangeWatchedFiles
                    |> Option.bind _.DynamicRegistration
                    |> Option.defaultValue false

                canShowProgress <-
                    p.Capabilities.Window
                    |> Option.bind _.WorkDoneProgress
                    |> Option.defaultValue false

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
                        ReferencesProvider = Some(U2.C1 true)
                        CompletionProvider =
                            Some
                                { TriggerCharacters = Some [| "."; "'" |]
                                  AllCommitCharacters = None
                                  ResolveProvider = Some true
                                  CompletionItem = None
                                  WorkDoneProgress = None } }

                return
                    Ok
                        { Capabilities = capabilities
                          ServerInfo = Some { Name = "ranvierautocomplete"; Version = None } }
            }

        member _.Initialized(_) =
            async {
                if canShowProgress then
                    progress <- Some(new Progress(host, workspace, publisher, client))

                registerWatchers ()

                match rootPath, automaticInit with
                | Some root, true ->
                    // The load takes as long as the cold checks; the connection keeps serving meanwhile
                    automaticLoad root |> ignore
                | _ -> ()
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

        member _.TextDocumentDidChange(p: DidChangeTextDocumentParams) =
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
                publisher.Change(Paths.ofUri p.TextDocument.Uri, version, (fun text -> p.ContentChanges |> Array.fold apply text))
            with ex ->
                eprintfn "[ranvierautocomplete] didChange failed: %O" ex

            ignoreNotification

        member _.TextDocumentDidClose(p: DidCloseTextDocumentParams) =
            try
                publisher.Close(Paths.ofUri p.TextDocument.Uri)
            with ex ->
                eprintfn "[ranvierautocomplete] didClose failed: %O" ex

            ignoreNotification

        member _.TextDocumentDidSave(_) = ignoreNotification
        member _.WorkspaceDidChangeConfiguration(_) = ignoreNotification
        member _.WorkspaceDidChangeWatchedFiles(p: DidChangeWatchedFilesParams) =
            async {
                try
                    let changes =
                        [ for c in p.Changes -> Paths.ofUri c.Uri, (c.Type = FileChangeType.Deleted) ]

                    host.Post(fun () ->
                        ensureWatch ()

                        if isNull holding && changes |> List.exists (fst >> isProjectLevel) then
                            holding <- workspace.HoldLoading()

                        watched.AddRange changes
                        arrivals.Value <- arrivals.Value + 1)
                with ex ->
                    eprintfn "[ranvierautocomplete] watched-file change failed: %O" ex
            }

        member _.CallHierarchyIncomingCalls(_) = notImplemented
        member _.CallHierarchyOutgoingCalls(_) = notImplemented
        member _.CodeActionResolve(_) = notImplemented
        member _.CodeLensResolve(_) = notImplemented
        member _.CompletionItemResolve(item: CompletionItem) =
            match Completion.positionOf item with
            | None -> async.Return(Ok item)
            | Some(path, line, character) ->
                reading "completionItem/resolve" item (fun () -> path) (fun graph path ct ->
                    Completion.resolve graph item path line character ct)

        member _.DocumentLinkResolve(_) = notImplemented
        member _.InlayHintResolve(_) = notImplemented
        member _.TextDocumentCodeAction(_) = notImplemented
        member _.TextDocumentCodeLens(_) = notImplemented
        member _.TextDocumentColorPresentation(_) = notImplemented
        member _.TextDocumentCompletion(p: CompletionParams) =
            reading "completion" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let! items = Completion.at graph path (int p.Position.Line) (int p.Position.Character) ct

                    return
                        Some(
                            U2.C2
                                { IsIncomplete = false
                                  Items = items
                                  ItemDefaults = None }
                        )
                })

        member _.TextDocumentDeclaration(_) = notImplemented
        member _.TextDocumentDefinition(p: DefinitionParams) =
            reading "definition" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let! found = Definition.at workspace.OwnersOf graph path (int p.Position.Line) (int p.Position.Character) ct

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
        member _.TextDocumentDocumentHighlight(_) = notImplemented
        member _.TextDocumentDocumentLink(_) = notImplemented
        member _.TextDocumentDocumentSymbol(_) = notImplemented
        member _.TextDocumentFoldingRange(_) = notImplemented
        member _.TextDocumentFormatting(_) = notImplemented
        member _.TextDocumentHover(p: HoverParams) =
            reading "hover" None (fun () -> Paths.ofUri p.TextDocument.Uri) (fun graph path ct ->
                task {
                    let! text = Hover.at workspace.OwnersOf graph path (int p.Position.Line) (int p.Position.Character) ct

                    return
                        text
                        |> Option.map (fun t ->
                            { Contents = U3.C1 { Kind = MarkupKind.Markdown; Value = t }
                              Range = None })
                })

        member _.TextDocumentImplementation(_) = notImplemented
        member _.TextDocumentInlayHint(_) = notImplemented
        member _.TextDocumentInlineValue(_) = notImplemented
        member _.TextDocumentLinkedEditingRange(_) = notImplemented
        member _.TextDocumentMoniker(_) = notImplemented
        member _.TextDocumentOnTypeFormatting(_) = notImplemented
        member _.TextDocumentPrepareCallHierarchy(_) = notImplemented
        member _.TextDocumentPrepareRename(_) = notImplemented
        member _.TextDocumentPrepareTypeHierarchy(_) = notImplemented
        member _.TextDocumentRangeFormatting(_) = notImplemented
        member _.TextDocumentReferences(p: ReferenceParams) =
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
                            let! found = References.find scope graph path (int p.Position.Line) (int p.Position.Character) ct

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
        member _.TextDocumentRename(_) = notImplemented
        member _.TextDocumentSelectionRange(_) = notImplemented
        member _.TextDocumentSemanticTokensFull(_) = notImplemented
        member _.TextDocumentSemanticTokensFullDelta(_) = notImplemented
        member _.TextDocumentSemanticTokensRange(_) = notImplemented
        member _.TextDocumentSignatureHelp(_) = notImplemented
        member _.TextDocumentTypeDefinition(_) = notImplemented
        member _.TextDocumentWillSave(_) = ignoreNotification
        member _.TextDocumentWillSaveWaitUntil(_) = notImplemented
        member _.TypeHierarchySubtypes(_) = notImplemented
        member _.TypeHierarchySupertypes(_) = notImplemented
        member _.WindowWorkDoneProgressCancel(_) = ignoreNotification
        member _.WorkspaceDiagnostic(_) = notImplemented
        member _.WorkspaceDidChangeWorkspaceFolders(_) = ignoreNotification
        member _.WorkspaceDidCreateFiles(_) = ignoreNotification
        member _.WorkspaceDidDeleteFiles(_) = ignoreNotification
        member _.WorkspaceDidRenameFiles(_) = ignoreNotification
        member _.WorkspaceExecuteCommand(_) = notImplemented
        member _.WorkspaceSymbol(_) = notImplemented
        member _.WorkspaceSymbolResolve(_) = notImplemented
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
              "textDocument/references"
              "textDocument/completion"
              "completionItem/resolve" ]

    let private handlings () =
        defaultRequestHandlings ()
        |> Map.filter (fun name _ -> implemented.Contains name)
        |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspacePeek p))
        |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspaceLoad p))
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
