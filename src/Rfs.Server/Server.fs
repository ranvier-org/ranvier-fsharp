namespace Rfs.Server

open System
open System.IO
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
    let mutable rootPath: string option = None
    let mutable automaticInit = false
    let mutable peekDepth = 2
    let mutable excludedDirs: string list = [ ".git"; "node_modules"; ".ionide" ]

    let dispose () =
        if Threading.Interlocked.Exchange(&disposed, 1) = 0 then
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
                                    Change = Some TextDocumentSyncKind.Incremental } }

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

        member _.TextDocumentDidOpen(_) = ignoreNotification
        member _.TextDocumentDidChange(_) = ignoreNotification
        member _.TextDocumentDidClose(_) = ignoreNotification
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
        member _.TextDocumentHover(_) = notImplemented
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
              "workspace/didChangeWatchedFiles" ]

    let private handlings () =
        defaultRequestHandlings ()
        |> Map.filter (fun name _ -> implemented.Contains name)
        |> Map.add "fsharp/workspacePeek" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspacePeek p))
        |> Map.add "fsharp/workspaceLoad" (serverRequestHandling (fun (s: RanvierServer) p -> s.FSharpWorkspaceLoad p))

    let run (input: Stream) (output: Stream) : int =
        let reason =
            start
                (handlings ())
                input
                output
                (fun (notify, request) -> new RanvierClient(notify, request))
                (fun client -> new RanvierServer(client, GraphHost.Start "server"))
                defaultRpc

        int reason
