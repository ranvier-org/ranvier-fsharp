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

    let dispose () =
        if Threading.Interlocked.Exchange(&disposed, 1) = 0 then
            host.Dispose()

    member _.Client = client
    member _.Host = host

    interface ILspServer with
        member _.Initialize(_: InitializeParams) =
            async {
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

        member _.Initialized(_) = ignoreNotification

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
        member _.WorkspaceDidChangeWatchedFiles(_) = ignoreNotification

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
        defaultRequestHandlings () |> Map.filter (fun name _ -> implemented.Contains name)

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
