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

    override _.WorkspaceConfiguration(p: ConfigurationParams) =
        sendServerRequest.Send "workspace/configuration" (box p)

    /// `window/workDoneProgress/create` for `token`
    member _.CreateProgress(token: string) : Async<Result<unit, JsonRpc.Error>> =
        sendServerRequest.Send "window/workDoneProgress/create" (box (Newtonsoft.Json.Linq.JObject(Newtonsoft.Json.Linq.JProperty("token", token))))

    /// A custom `fsharp/*` or `rfs/*` notification
    member _.Notify(method: string, payload: obj) =
        sendServerNotification method payload |> Async.Ignore
