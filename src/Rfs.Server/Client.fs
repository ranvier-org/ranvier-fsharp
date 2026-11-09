namespace Rfs.Server

open Ionide.LanguageServerProtocol
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types

/// The server's side of the connection to the editor, ported from FSAC's FSharpLspClient with only the notifications
/// this server sends.
type RanvierClient(sendServerNotification: ClientNotificationSender, sendServerRequest: ClientRequestSender) =
    inherit LspClient()

    override _.TextDocumentPublishDiagnostics(p: PublishDiagnosticsParams) =
        sendServerNotification "textDocument/publishDiagnostics" (box p) |> Async.Ignore

    override _.WindowShowMessage(p: ShowMessageParams) =
        sendServerNotification "window/showMessage" (box p) |> Async.Ignore

    override _.ClientRegisterCapability(p: RegistrationParams) =
        sendServerRequest.Send "client/registerCapability" (box p)

    /// A custom `fsharp/*` or `rfs/*` notification
    member _.Notify(method: string, payload: obj) =
        sendServerNotification method payload |> Async.Ignore
