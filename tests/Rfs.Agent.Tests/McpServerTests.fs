module Rfs.Agent.Tests.McpServerTests

open System
open System.Collections.Generic
open System.IO.Pipelines
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open ModelContextProtocol.Client
open ModelContextProtocol.Protocol
open ModelContextProtocol.Server
open Rfs.Agent

let private withInProcessClient
    (alwaysLoad: bool)
    (dispatch: JsonObject -> CancellationToken -> Task<Result<string, string>>)
    (body: McpClient -> Task)
    =
    task {
        let toServer = Pipe()
        let toClient = Pipe()
        use cts = new CancellationTokenSource()
        let options = McpServerHost.createOptions dispatch alwaysLoad
        use serverTransport = new StreamServerTransport(toServer.Reader.AsStream(), toClient.Writer.AsStream())
        let server = McpServer.Create(serverTransport, options)
        let running = server.RunAsync cts.Token
        let! client = McpClient.CreateAsync(new StreamClientTransport(toServer.Writer.AsStream(), toClient.Reader.AsStream()))

        try
            do! body client
        finally
            (client :> IAsyncDisposable).DisposeAsync().AsTask().Wait()
            cts.Cancel()

            try
                running.Wait 2000 |> ignore
            with _ ->
                ()
    }

let private callTool (client: McpClient) (name: string) (argsJson: string) =
    use doc = JsonDocument.Parse argsJson
    let dict = Dictionary<string, JsonElement>()

    for prop in doc.RootElement.EnumerateObject() do
        dict[prop.Name] <- prop.Value.Clone()

    client.CallToolAsync(CallToolRequestParams(Name = name, Arguments = dict))

[<Tests>]
let tests =
    testList "McpServerHost" [
        testTask "lists all 6 rfs_* tools with anthropic/alwaysLoad when alwaysLoad=true" {
            let dummyDispatch _ _ = Task.FromResult(Ok "ok")

            do!
                withInProcessClient true dummyDispatch (fun client ->
                    task {
                        Expect.equal client.ServerInfo.Version (AgentVersion.semver ()) "MCP ServerInfo.Version matches AgentVersion.semver()"
                        let! tools = client.ListToolsAsync()
                        let names = tools |> Seq.map _.Name |> Set.ofSeq

                        let expected =
                            set [ "rfs_check"; "rfs_symbol"; "rfs_references"; "rfs_search"; "rfs_rename"; "rfs_status" ]

                        Expect.equal names expected "exact 6 MCP tools registered"

                        for tool in tools do
                            Expect.isNotNull tool.ProtocolTool.Meta $"{tool.Name} has Meta"
                            Expect.isTrue (tool.ProtocolTool.Meta["anthropic/alwaysLoad"].GetValue<bool>()) $"{tool.Name} has anthropic/alwaysLoad=true"
                    })
        }

        testTask "omits tool Meta when alwaysLoad=false for deferred ToolSearch mode" {
            let dummyDispatch _ _ = Task.FromResult(Ok "ok")

            do!
                withInProcessClient false dummyDispatch (fun client ->
                    task {
                        let! tools = client.ListToolsAsync()
                        Expect.equal tools.Count 6 "6 tools registered"

                        for tool in tools do
                            Expect.isNull tool.ProtocolTool.Meta $"{tool.Name} has null Meta when deferred"
                    })
        }

        testTask "routes all 6 tool calls and maps Ok/Error results cleanly" {
            let seenOps = ResizeArray<string * string>()

            let dispatch (req: JsonObject) (_: CancellationToken) : Task<Result<string, string>> =
                task {
                    let op = JsonUtil.str req "op" |> Option.defaultValue ""
                    seenOps.Add(op, req.ToJsonString())

                    match op with
                    | "rename" -> return Error "Simulated rename failure"
                    | _ -> return Ok $"dispatched:%s{op}"
                }

            do!
                withInProcessClient true dispatch (fun client ->
                    task {
                        let calls =
                            [ "rfs_check", """{"file":"src/Lib.fs","severity":"error","downstream":true}"""
                              "rfs_symbol", """{"file":"src/Lib.fs","line":12,"symbol":"foo"}"""
                              "rfs_references", """{"file":"src/Lib.fs","line":12,"mode":"all"}"""
                              "rfs_search", """{"query":"bar","maxResults":10}"""
                              "rfs_status", """{"reload":false}"""
                              "rfs_rename", """{"file":"src/Lib.fs","line":12,"newName":"baz","apply":false}""" ]

                        for name, args in calls do
                            let! res = callTool client name args
                            let textBlock = Seq.head res.Content :?> TextContentBlock

                            if name = "rfs_rename" then
                                Expect.isTrue (res.IsError.GetValueOrDefault false) "Error result sets IsError=true"
                                Expect.equal textBlock.Text "Simulated rename failure" "error message preserved"
                            else
                                Expect.isFalse (res.IsError.GetValueOrDefault false) $"{name} IsError=false"
                                Expect.stringStarts textBlock.Text "dispatched:" $"{name} routed"

                        let ops = seenOps |> Seq.map fst |> Set.ofSeq
                        Expect.equal ops (set [ "check"; "symbol"; "references"; "search"; "status"; "rename" ]) "all 6 daemon ops dispatched"
                    })
        }
    ]
