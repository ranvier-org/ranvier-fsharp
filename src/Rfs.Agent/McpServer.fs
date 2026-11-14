namespace Rfs.Agent

open System
open System.Collections.Generic
open System.IO
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open ModelContextProtocol
open ModelContextProtocol.Protocol
open ModelContextProtocol.Server
open Rfs.Server

module internal McpServerHost =

    let private parseJsonSchema (text: string) : JsonElement =
        use doc = JsonDocument.Parse text
        doc.RootElement.Clone()

    let private alwaysLoadMeta (alwaysLoad: bool) : JsonObject =
        if alwaysLoad then
            JsonObject([ KeyValuePair<string, JsonNode>("anthropic/alwaysLoad", JsonValue.Create true) ])
        else
            null

    let private blocks (text: string) =
        ResizeArray<ContentBlock> [ TextContentBlock(Text = text) :> ContentBlock ]

    let private plainResult (text: string) =
        CallToolResult(Content = blocks text)

    let private errorResult (msg: string) =
        CallToolResult(Content = blocks msg, IsError = Nullable true)

    let private copyArgsToDaemonRequest (op: string) (req: CallToolRequestParams) : JsonObject =
        let o = JsonObject()
        o["op"] <- JsonValue.Create op

        if not (isNull req) && not (isNull req.Arguments) then
            for KeyValue(k, v) in req.Arguments do
                try
                    o[k] <- JsonNode.Parse(v.GetRawText())
                with _ ->
                    ()

        o

    let createOptions
        (dispatch: JsonObject -> CancellationToken -> Task<Result<string, string>>)
        (alwaysLoad: bool)
        : McpServerOptions =
        let meta = alwaysLoadMeta alwaysLoad

        let makeTool (name: string) (description: string) (schemaJson: string) (op: string) =
            let tool = Tool(Name = name, Description = description, InputSchema = parseJsonSchema schemaJson, Meta = meta)

            let run (p: CallToolRequestParams) (ct: CancellationToken) : Task<CallToolResult> =
                task {
                    try
                        let reqObj = copyArgsToDaemonRequest op p

                        match! dispatch reqObj ct with
                        | Ok txt -> return plainResult txt
                        | Error err -> return errorResult err
                    with ex ->
                        return errorResult $"%s{name} failed: %s{ex.Message}"
                }

            tool, run

        let toolsAndHandlers =
            [ makeTool
                "rfs_check"
                "Incremental F# compiler diagnostics across a file, project, or workspace, including downstream referencing projects."
                """{"type":"object","properties":{"file":{"type":"string","description":"Optional .fs/.fsi/.fsproj path"},"project":{"type":"string","description":"Optional project name or path"},"severity":{"type":"string","enum":["error","warning","all"]},"downstream":{"type":"boolean","description":"Check referencing projects too (default true)"}}}"""
                "check"
              makeTool
                "rfs_symbol"
                "Inferred F# type signature, XML doc, definition, type definition, and implementations at 1-based line + symbol name or column."
                """{"type":"object","properties":{"file":{"type":"string"},"line":{"type":"integer","minimum":1},"column":{"type":"integer","minimum":1},"symbol":{"type":"string","description":"Identifier or operator name near line"}},"required":["file","line"]}"""
                "symbol"
              makeTool
                "rfs_references"
                "Semantic workspace-wide references or call hierarchy (uses/incoming/outgoing) for an F# symbol at 1-based line."
                """{"type":"object","properties":{"file":{"type":"string"},"line":{"type":"integer","minimum":1},"column":{"type":"integer","minimum":1},"symbol":{"type":"string"},"mode":{"type":"string","enum":["all","uses","incoming","outgoing"]},"maxResults":{"type":"integer","minimum":1,"maximum":500}},"required":["file","line"]}"""
                "references"
              makeTool
                "rfs_search"
                "Syntax outline of one F# file (when file is set) or fast symbol search across all loaded F# projects."
                """{"type":"object","properties":{"query":{"type":"string"},"file":{"type":"string"},"maxResults":{"type":"integer","minimum":1,"maximum":500}}}"""
                "search"
              makeTool
                "rfs_rename"
                "Semantically rename an F# symbol across projects cleanly; applies edits and re-checks compiler diagnostics."
                """{"type":"object","properties":{"file":{"type":"string"},"line":{"type":"integer","minimum":1},"newName":{"type":"string"},"column":{"type":"integer","minimum":1},"symbol":{"type":"string"},"apply":{"type":"boolean","description":"Apply to disk and verify errors (default true)"}},"required":["file","line","newName"]}"""
                "rename"
              makeTool
                "rfs_status"
                "Daemon status, loaded projects, RAM usage, and Ranvier incremental check counters; set reload=true after project changes."
                """{"type":"object","properties":{"reload":{"type":"boolean"}}}"""
                "status" ]

        let toolMap =
            toolsAndHandlers
            |> List.map (fun (t, fn) -> t.Name, fn)
            |> dict

        let listTools =
            McpRequestHandler<ListToolsRequestParams, ListToolsResult>(fun _ _ ->
                let list = ResizeArray<Tool>(toolsAndHandlers |> List.map fst)
                ValueTask<ListToolsResult>(ListToolsResult(Tools = list)))

        let callTool =
            McpRequestHandler<CallToolRequestParams, CallToolResult>(fun request ct ->
                match toolMap.TryGetValue request.Params.Name with
                | true, fn -> ValueTask<CallToolResult>(fn request.Params ct)
                | false, _ -> raise (McpProtocolException($"Unknown tool '%s{request.Params.Name}'.", McpErrorCode.InvalidParams)))

        McpServerOptions(
            ServerInfo = Implementation(Name = "ranvier-fsharp", Version = AgentVersion.semver ()),
            ServerInstructions =
                "F# semantic compiler tools backed by Ranvier incremental daemon. Lines/columns are 1-based; pass 'symbol' with 'line' to avoid column counting. Prefer rfs_check after edits (incremental vs slow dotnet build), rfs_symbol for exact inferred types/defs, rfs_references for accurate workspace uses, and rfs_rename for safe multi-file renames.",
            ProtocolVersion = "2025-11-25",
            Capabilities = ServerCapabilities(Tools = ToolsCapability(ListChanged = Nullable false)),
            Handlers = McpServerHandlers(ListToolsHandler = listTools, CallToolHandler = callTool)
        )

    let runAsync (workspaceRoot: string) (alwaysLoad: bool) (ct: CancellationToken) : Task<int> =
        task {
            let wsRoot = WorkspaceRoot.canonical workspaceRoot
            let wsId = WorkspaceRoot.workspaceId wsRoot
            let pipeName = WorkspaceRoot.pipeName (ProcessRegistry.stateDir ()) wsRoot

            use rawIn = new ThreadReadStream(Console.OpenStandardInput())
            use rawOut = Console.OpenStandardOutput()
            // Protect MCP JSON-RPC stdout from any stray Console.Write calls.
            Console.SetOut Console.Error

            let! _ = ProcessRegistry.reapStaleProcessesAsync false false (ProcessRegistry.currentBinaryStamp ()) true
            let mutable procRec = ProcessRegistry.registerSelf "mcp" wsRoot pipeName (ProcessRegistry.defaultIdleTimeoutSeconds ())
            let leaseToClean: DaemonLease option = None

            use heartbeatCts = CancellationTokenSource.CreateLinkedTokenSource ct

            let heartbeat =
                Task.Run<unit>(fun () ->
                    task {
                        use timer = new PeriodicTimer(TimeSpan.FromSeconds 30.)

                        try
                            while! timer.WaitForNextTickAsync heartbeatCts.Token do
                                procRec <- ProcessRegistry.touchProcessRecord procRec DateTime.UtcNow.Ticks
                        with _ ->
                            ()
                    })

            // Pre-warm daemon in background without blocking MCP initialize handshake.
            Task.Run<unit>(fun () ->
                task {
                    try
                        let! _ = DaemonClient.ensureDaemonPipeAsync wsRoot heartbeatCts.Token
                        ()
                    with _ ->
                        ()
                })
            |> ignore

            let dispatch (req: JsonObject) (callCt: CancellationToken) : Task<Result<string, string>> =
                task {
                    procRec <- ProcessRegistry.touchProcessRecord procRec DateTime.UtcNow.Ticks

                    let reqWs =
                        match JsonUtil.str req "file" with
                        | Some f -> WorkspaceRoot.resolve (Some wsRoot) (Some f)
                        | None -> wsRoot

                    let! resp = DaemonClient.requestAsync reqWs req callCt
                    let ok = JsonUtil.boolOpt resp "ok" |> Option.defaultValue false

                    if ok then
                        return Ok(JsonUtil.strRaw resp "text" |> Option.defaultValue "")
                    else
                        return Error(JsonUtil.strRaw resp "error" |> Option.defaultValue "Daemon request failed.")
                }

            try
                let options = createOptions dispatch alwaysLoad
                use transport = new StreamServerTransport(rawIn, rawOut, "ranvier-fsharp")
                let server = McpServer.Create(transport, options)
                do! server.RunAsync ct
                return 0
            finally
                heartbeatCts.Cancel()

                try
                    heartbeat.Wait 1000 |> ignore
                with _ ->
                    ()

                ProcessRegistry.tryDeleteIfOwned procRec leaseToClean
        }
