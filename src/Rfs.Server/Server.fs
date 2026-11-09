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
open Rfs.Checker

/// Ported from FSAC's `Helpers` (Common.fs)
module Helpers =
    let notImplemented<'t> = async.Return LspResult.notImplemented<'t>
    let ignoreNotification = async.Return(())

open Helpers

type RanvierServer(client: RanvierClient, host: GraphHost) =
    let mutable disposed = 0
    let workspace = new Workspace(host, client)
    let documents = Documents()
    let publisher = new Publisher(workspace, client, documents)

    do
        workspace.OnSetText(fun graph path version lspVersion -> documents.Record(graph, path, version, lspVersion))
        workspace.OnLoaded publisher.Loaded

    let mutable rootPath: string option = None
    let mutable automaticInit = false
    let mutable peekDepth = 2
    let mutable excludedDirs: string list = [ ".git"; "node_modules"; ".ionide" ]

    let dispose () =
        if Threading.Interlocked.Exchange(&disposed, 1) = 0 then
            publisher.Dispose()
            workspace.Dispose()
            host.Dispose()

    let plain (content: string) : LspResult<PlainNotification> = Ok { Content = content }

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

    member _.Client = client
    member _.Host = host
    member _.Workspace = workspace
    member internal _.Documents = documents
    member internal _.Publisher = publisher

    member _.FSharpWorkspacePeek(p: WorkspacePeekRequest) : Async<LspResult<PlainNotification>> =
        async {
            try
                let found = WorkspacePeek.peek p.Directory p.Deep (List.ofArray p.ExcludedDirs)
                return plain (Responses.workspacePeek found)
            with ex ->
                return LspResult.internalError ex.Message
        }

    member _.FSharpWorkspaceLoad(p: WorkspaceLoadParms) : Async<LspResult<PlainNotification>> =
        async {
            try
                let projects = p.TextDocuments |> Array.map (fun t -> Paths.ofUri t.Uri) |> List.ofArray
                let! _ = workspace.Load projects |> Async.AwaitTask
                return plain (Responses.workspaceLoad true)
            with ex ->
                return LspResult.internalError ex.Message
        }

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

                let capabilities =
                    { ServerCapabilities.Default with
                        TextDocumentSync =
                            Some
                            <| U2.C1
                                { TextDocumentSyncOptions.Default with
                                    OpenClose = Some true
                                    Change = Some TextDocumentSyncKind.Incremental }
                        HoverProvider = Some(U2.C1 true) }

                return
                    Ok
                        { Capabilities = capabilities
                          ServerInfo = Some { Name = "ranvierautocomplete"; Version = None } }
            }

        member _.Initialized(_) =
            async {
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

                    do! workspace.FilesChanged changes |> Async.AwaitTask
                with ex ->
                    eprintfn "[ranvierautocomplete] watched-file change failed: %O" ex
            }

        member _.CallHierarchyIncomingCalls(_) = notImplemented
        member _.CallHierarchyOutgoingCalls(_) = notImplemented
        member _.CodeActionResolve(_) = notImplemented
        member _.CodeLensResolve(_) = notImplemented
        member _.CompletionItemResolve(_) = notImplemented
        member _.DocumentLinkResolve(_) = notImplemented
        member _.InlayHintResolve(_) = notImplemented
        member _.TextDocumentCodeAction(_) = notImplemented
        member _.TextDocumentCodeLens(_) = notImplemented
        member _.TextDocumentColorPresentation(_) = notImplemented
        member _.TextDocumentCompletion(_) = notImplemented
        member _.TextDocumentDeclaration(_) = notImplemented
        member _.TextDocumentDefinition(_) = notImplemented
        member _.TextDocumentDiagnostic(_) = notImplemented
        member _.TextDocumentDocumentColor(_) = notImplemented
        member _.TextDocumentDocumentHighlight(_) = notImplemented
        member _.TextDocumentDocumentLink(_) = notImplemented
        member _.TextDocumentDocumentSymbol(_) = notImplemented
        member _.TextDocumentFoldingRange(_) = notImplemented
        member _.TextDocumentFormatting(_) = notImplemented
        member _.TextDocumentHover(p: HoverParams) =
            async {
                let! ct = Async.CancellationToken

                try
                    // The edits before this request may still be queued; its answer is for the text they leave
                    do! publisher.Barrier().WaitAsync(TimeSpan.FromMinutes 10., ct) |> Async.AwaitTask
                    let path = Paths.ofUri p.TextDocument.Uri

                    match (if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then [] else workspace.OwnersOf path) with
                    | [] -> return Ok None
                    | graph :: _ ->
                        let! text =
                            Hover.at graph path (int p.Position.Line) (int p.Position.Character) ct
                            |> Async.AwaitTask

                        return
                            Ok(
                                text
                                |> Option.map (fun t ->
                                    { Contents = U3.C1 { Kind = MarkupKind.Markdown; Value = t }
                                      Range = None })
                            )
                with ex ->
                    let rec root (e: exn) =
                        match e with
                        | :? AggregateException as a when a.InnerExceptions.Count = 1 -> root a.InnerException
                        | e -> e

                    match root ex with
                    // The project went away (a reload hides it while it loads again), or the server is shutting down.
                    // A request the client cancelled never gets here: the library cancels the async itself.
                    | :? OperationCanceledException
                    | :? ObjectDisposedException -> return Ok None
                    | e ->
                        eprintfn "[ranvierautocomplete] hover failed: %O" e
                        return LspResult.internalError e.Message
            }

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
        member _.TextDocumentReferences(_) = notImplemented
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
              "textDocument/hover" ]

    let private handlings () =
        defaultRequestHandlings ()
        |> Map.filter (fun name _ -> implemented.Contains name)
        |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspacePeek p))
        |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspaceLoad p))

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
