namespace Rfs.Agent

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

module internal Cli =

    let private isFlag (s: string) = s.StartsWith("-", StringComparison.Ordinal)

    /// Parses `<file>`, `<file>:<line>`, or `<file>:<line>:<col>` safely on Windows (ignoring `C:` drive colon).
    let internal splitFileLineCol (raw: string) : string * int option * int option =
        let lastColon = raw.LastIndexOf ':'

        if lastColon <= 1 then
            raw, None, None
        else
            let tail = raw.Substring(lastColon + 1)

            match Int32.TryParse tail with
            | false, _ -> raw, None, None
            | true, n1 ->
                let prefix = raw.Substring(0, lastColon)
                let prevColon = prefix.LastIndexOf ':'

                if prevColon > 1 then
                    let mid = prefix.Substring(prevColon + 1)

                    match Int32.TryParse mid with
                    | true, l1 -> prefix.Substring(0, prevColon), Some l1, Some n1
                    | false, _ -> prefix, Some n1, None
                else
                    prefix, Some n1, None

    let private extractCommon (argv: string[]) =
        let mutable workspaceOpt: string option = None
        let rest = ResizeArray<string>()
        let mutable i = 0

        while i < argv.Length do
            let a = argv[i]

            if (a = "--workspace" || a = "-w") && i + 1 < argv.Length then
                workspaceOpt <- Some argv[i + 1]
                i <- i + 2
            elif a.StartsWith("--workspace=", StringComparison.Ordinal) then
                workspaceOpt <- Some(a.Substring("--workspace=".Length))
                i <- i + 1
            else
                rest.Add a
                i <- i + 1

        workspaceOpt, List.ofSeq rest

    let helpText () =
        String.Join(
            Environment.NewLine,
            [ $"rfs %s{AgentVersion.informational ()} — Ranvier F# semantic analysis CLI & MCP server"
              ""
              "Usage:"
              "  rfs [--workspace <dir>] <command> [args]"
              ""
              "Semantic commands (daemon-backed, auto-reaps stale processes):"
              "  check [<file-or-project>] [--errors-only] [--all] [--no-downstream] [--project <name>] [--fail-on-error]"
              "  symbol <file>[:<line>[:<col>]] [<line>] [<symbol-or-col>] [--symbol <name>] [--col <n>]"
              "  refs <file>[:<line>[:<col>]] [<line>] [<symbol-or-col>] [--symbol <name>] [--col <n>] [--uses-only|--incoming|--outgoing] [--max <n>]"
              "  search [<query>] [--file <file>] [--max <n>]"
              "  rename <file>[:<line>[:<col>]] [<line>] [<old-symbol>] <new-name> [--symbol <name>] [--col <n>] [--dry-run]"
              "  status [--json] [--reload]"
              ""
              "Daemon & MCP commands:"
              "  mcp [--workspace <dir>] [--no-always-load]"
              "  daemon status"
              "  daemon reap [--force] [--include-lsp]"
              "  daemon kill [--all]"
              "  daemon run [--workspace <dir>] [--idle-timeout <seconds>]" ]
        )

    let private parsePositionArgs (args: string list) : Result<string * int * int option * string option * string list, string> =
        match args with
        | [] -> Error "Expected <file> <line>."
        | first :: tail ->
            let file, inlineLine, inlineCol = splitFileLineCol first
            let mutable lineOpt = inlineLine
            let mutable colOpt = inlineCol
            let mutable symOpt: string option = None
            let extras = ResizeArray<string>()

            let mutable cur = tail

            if lineOpt.IsNone then
                match cur with
                | next :: rest when not (isFlag next) ->
                    match Int32.TryParse next with
                    | true, l ->
                        lineOpt <- Some l
                        cur <- rest
                    | false, _ -> ()
                | _ -> ()

            while not cur.IsEmpty do
                match cur with
                | ("--col" | "-c") :: v :: rest ->
                    match Int32.TryParse v with
                    | true, c -> colOpt <- Some c
                    | false, _ -> ()
                    cur <- rest
                | ("--symbol" | "-s") :: v :: rest ->
                    symOpt <- Some v
                    cur <- rest
                | flag :: rest when flag.StartsWith("--symbol=", StringComparison.Ordinal) ->
                    symOpt <- Some(flag.Substring("--symbol=".Length))
                    cur <- rest
                | flag :: rest when flag.StartsWith("--col=", StringComparison.Ordinal) ->
                    match Int32.TryParse(flag.Substring("--col=".Length)) with
                    | true, c -> colOpt <- Some c
                    | false, _ -> ()
                    cur <- rest
                | item :: rest ->
                    extras.Add item
                    cur <- rest
                | [] -> ()

            match lineOpt with
            | None -> Error $"Missing 1-based <line> number for file '%s{file}'."
            | Some line when line < 1 -> Error $"Line number must be >= 1 (got %d{line})."
            | Some line -> Ok(file, line, colOpt, symOpt, List.ofSeq extras)

    let private callDaemon (wsRoot: string) (req: JsonObject) (failOnErrorCount: bool) : Task<int> =
        task {
            try
                let! resp = DaemonClient.requestAsync wsRoot req CancellationToken.None
                let ok = JsonUtil.boolOpt resp "ok" |> Option.defaultValue false

                if ok then
                    let text = JsonUtil.strRaw resp "text" |> Option.defaultValue ""

                    if not (String.IsNullOrEmpty text) then
                        Console.Out.WriteLine text

                    if failOnErrorCount && not (text.StartsWith("0 errors,", StringComparison.Ordinal) || text.Contains("\n0 errors,", StringComparison.Ordinal)) then
                        return 1
                    else
                        return 0
                else
                    let err = JsonUtil.strRaw resp "error" |> Option.defaultValue "Unknown error."
                    Console.Error.WriteLine err
                    return 1
            with ex ->
                Console.Error.WriteLine $"rfs error: %s{ex.Message}"
                return 1
        }

    let private runDaemonCommand (wsOpt: string option) (subArgs: string list) : Task<int> =
        task {
            match subArgs with
            | []
            | [ "status" ] ->
                let leases = ProcessRegistry.listDaemonLeases ()
                let procs = ProcessRegistry.listProcessRecords ()
                Console.Out.WriteLine($"stateDir: %s{ProcessRegistry.stateDir ()}")
                Console.Out.WriteLine($"daemons (%d{leases.Length}):")

                for l in leases do
                    let! alive = ProcessRegistry.pingPipeAsync l.PipeName 600
                    Console.Out.WriteLine($"  pid=%d{l.Pid} alive=%b{alive} ws=%s{l.WorkspaceRoot}")

                Console.Out.WriteLine($"processes (%d{procs.Length}):")

                for p in procs do
                    let idleSec = int (DateTime.UtcNow - DateTime(p.LastActiveUtcTicks, DateTimeKind.Utc)).TotalSeconds
                    Console.Out.WriteLine($"  pid=%d{p.Pid} kind=%s{p.Kind} idle=%d{idleSec}s/%d{p.IdleTimeoutSeconds}s ws=%s{p.WorkspaceRoot}")

                return 0

            | "reap" :: flags ->
                let forceAll = flags |> List.exists (fun f -> f = "--force" || f = "--all")
                let includeLsp = flags |> List.exists (fun f -> f = "--include-lsp")
                let! report = ProcessRegistry.reapStaleProcessesAsync forceAll includeLsp (ProcessRegistry.currentBinaryStamp ()) true
                Console.Out.WriteLine($"checked=%d{report.CheckedRecords} reaped=%d{report.Actions.Length}")

                for a in report.Actions do
                    Console.Out.WriteLine($"  %s{ProcessRegistry.formatReason a.Reason}: pid=%d{a.Pid} kind=%s{a.Kind} ws=%s{a.WorkspaceRoot}")

                return 0

            | "kill" :: flags ->
                let killAll = flags |> List.exists (fun f -> f = "--all" || f = "-a")

                if killAll then
                    let! report = ProcessRegistry.reapStaleProcessesAsync true false (ProcessRegistry.currentBinaryStamp ()) false
                    Console.Out.WriteLine($"killed=%d{report.Actions.Length}")
                    return 0
                else
                    let wsRoot = WorkspaceRoot.resolve wsOpt None
                    let! stopped = DaemonClient.shutdownWorkspaceAsync wsRoot
                    Console.Out.WriteLine($"shutdown %s{wsRoot}: %b{stopped}")
                    return 0

            | "run" :: runArgs ->
                let mutable idleSec = ProcessRegistry.defaultIdleTimeoutSeconds ()
                let mutable foreground = false
                let mutable cur = runArgs

                while not cur.IsEmpty do
                    match cur with
                    | "--idle-timeout" :: v :: rest ->
                        match Int32.TryParse v with
                        | true, secs when secs >= 1 -> idleSec <- secs
                        | _ -> ()
                        cur <- rest
                    | "--foreground" :: rest ->
                        foreground <- true
                        cur <- rest
                    | _ :: rest -> cur <- rest
                    | [] -> ()

                let wsRoot = WorkspaceRoot.resolve wsOpt None
                use cts = new CancellationTokenSource()
                Console.CancelKeyPress.Add(fun ev ->
                    ev.Cancel <- true
                    cts.Cancel())

                return! DaemonServer.runAsync wsRoot idleSec (not foreground) cts.Token

            | other :: _ ->
                Console.Error.WriteLine($"Unknown daemon subcommand '%s{other}'.")
                return 1
        }

    let runAsync (argv: string[]) : Task<int> =
        task {
            if isNull argv || argv.Length = 0 || argv[0] = "-h" || argv[0] = "--help" then
                Console.Out.WriteLine(helpText ())
                return 0
            elif argv[0] = "--version" then
                Console.Out.WriteLine $"rfs %s{AgentVersion.informational ()}"
                return 0
            else
                let wsOpt, args = extractCommon argv

                match args with
                | [] ->
                    Console.Out.WriteLine(helpText ())
                    return 0

                | "help" :: _ ->
                    Console.Out.WriteLine(helpText ())
                    return 0

                | "daemon" :: subArgs ->
                    return! runDaemonCommand wsOpt subArgs

                | "mcp" :: mcpArgs ->
                    let alwaysLoad = not (mcpArgs |> List.exists (fun a -> a = "--no-always-load"))
                    let wsRoot = WorkspaceRoot.resolve wsOpt None
                    use cts = new CancellationTokenSource()
                    Console.CancelKeyPress.Add(fun ev ->
                        ev.Cancel <- true
                        cts.Cancel())
                    return! McpServerHost.runAsync wsRoot alwaysLoad cts.Token

                | "status" :: statusArgs ->
                    let reload = statusArgs |> List.exists (fun a -> a = "--reload" || a = "-r")
                    let json = statusArgs |> List.exists (fun a -> a = "--json")
                    let wsRoot = WorkspaceRoot.resolve wsOpt None
                    let req = JsonObject()
                    req["op"] <- JsonValue.Create "status"
                    req["reload"] <- JsonValue.Create reload
                    req["json"] <- JsonValue.Create json
                    return! callDaemon wsRoot req false

                | "check" :: checkArgs ->
                    let mutable fileOpt: string option = None
                    let mutable projOpt: string option = None
                    let mutable sev = "warning"
                    let mutable downstream = true
                    let mutable failOnError = false
                    let mutable cur = checkArgs

                    while not cur.IsEmpty do
                        match cur with
                        | "--errors-only" :: rest ->
                            sev <- "error"
                            cur <- rest
                        | "--all" :: rest ->
                            sev <- "all"
                            cur <- rest
                        | "--no-downstream" :: rest ->
                            downstream <- false
                            cur <- rest
                        | "--fail-on-error" :: rest ->
                            failOnError <- true
                            cur <- rest
                        | ("--project" | "-p") :: p :: rest ->
                            projOpt <- Some p
                            cur <- rest
                        | ("--file" | "-f") :: f :: rest ->
                            fileOpt <- Some f
                            cur <- rest
                        | x :: rest when not (isFlag x) ->
                            if x.EndsWith(".fsproj", StringComparison.OrdinalIgnoreCase) then
                                projOpt <- Some x
                            else
                                fileOpt <- Some x

                            cur <- rest
                        | _ :: rest -> cur <- rest
                        | [] -> ()

                    let wsRoot = WorkspaceRoot.resolve wsOpt (fileOpt |> Option.orElse projOpt)
                    let req = JsonObject()
                    req["op"] <- JsonValue.Create "check"
                    fileOpt |> Option.iter (fun f -> req["file"] <- JsonValue.Create f)
                    projOpt |> Option.iter (fun p -> req["project"] <- JsonValue.Create p)
                    req["severity"] <- JsonValue.Create sev
                    req["downstream"] <- JsonValue.Create downstream
                    return! callDaemon wsRoot req failOnError

                | "symbol" :: symArgs ->
                    match parsePositionArgs symArgs with
                    | Error msg ->
                        Console.Error.WriteLine msg
                        return 1
                    | Ok(file, line, colOpt, symOpt, extras) ->
                        let finalCol, finalSym =
                            match symOpt, colOpt, extras with
                            | None, None, tok :: _ ->
                                match Int32.TryParse tok with
                                | true, c -> Some c, None
                                | false, _ -> None, Some tok
                            | None, Some _, tok :: _ -> colOpt, Some tok
                            | _ -> colOpt, symOpt

                        let wsRoot = WorkspaceRoot.resolve wsOpt (Some file)
                        let req = JsonObject()
                        req["op"] <- JsonValue.Create "symbol"
                        req["file"] <- JsonValue.Create file
                        req["line"] <- JsonValue.Create line
                        finalCol |> Option.iter (fun c -> req["column"] <- JsonValue.Create c)
                        finalSym |> Option.iter (fun s -> req["symbol"] <- JsonValue.Create s)
                        return! callDaemon wsRoot req false

                | ("refs" | "references") :: refArgs ->
                    let mutable mode = "all"
                    let mutable maxResults = 100
                    let filtered = ResizeArray<string>()
                    let mutable cur = refArgs

                    while not cur.IsEmpty do
                        match cur with
                        | "--uses-only" :: rest ->
                            mode <- "uses"
                            cur <- rest
                        | "--incoming" :: rest ->
                            mode <- "incoming"
                            cur <- rest
                        | "--outgoing" :: rest ->
                            mode <- "outgoing"
                            cur <- rest
                        | "--max" :: v :: rest ->
                            match Int32.TryParse v with
                            | true, m when m >= 1 -> maxResults <- m
                            | _ -> ()
                            cur <- rest
                        | x :: rest ->
                            filtered.Add x
                            cur <- rest
                        | [] -> ()

                    match parsePositionArgs (List.ofSeq filtered) with
                    | Error msg ->
                        Console.Error.WriteLine msg
                        return 1
                    | Ok(file, line, colOpt, symOpt, extras) ->
                        let finalCol, finalSym =
                            match symOpt, colOpt, extras with
                            | None, None, tok :: _ ->
                                match Int32.TryParse tok with
                                | true, c -> Some c, None
                                | false, _ -> None, Some tok
                            | None, Some _, tok :: _ -> colOpt, Some tok
                            | _ -> colOpt, symOpt

                        let wsRoot = WorkspaceRoot.resolve wsOpt (Some file)
                        let req = JsonObject()
                        req["op"] <- JsonValue.Create "references"
                        req["file"] <- JsonValue.Create file
                        req["line"] <- JsonValue.Create line
                        finalCol |> Option.iter (fun c -> req["column"] <- JsonValue.Create c)
                        finalSym |> Option.iter (fun s -> req["symbol"] <- JsonValue.Create s)
                        req["mode"] <- JsonValue.Create mode
                        req["maxResults"] <- JsonValue.Create maxResults
                        return! callDaemon wsRoot req false

                | "search" :: searchArgs ->
                    let mutable fileOpt: string option = None
                    let mutable maxResults = 50
                    let queryParts = ResizeArray<string>()
                    let mutable cur = searchArgs

                    while not cur.IsEmpty do
                        match cur with
                        | ("--file" | "-f") :: f :: rest ->
                            fileOpt <- Some f
                            cur <- rest
                        | "--max" :: v :: rest ->
                            match Int32.TryParse v with
                            | true, m when m >= 1 -> maxResults <- m
                            | _ -> ()
                            cur <- rest
                        | x :: rest when not (isFlag x) ->
                            queryParts.Add x
                            cur <- rest
                        | _ :: rest -> cur <- rest
                        | [] -> ()

                    let query = String.Join(" ", queryParts)
                    let wsRoot = WorkspaceRoot.resolve wsOpt fileOpt
                    let req = JsonObject()
                    req["op"] <- JsonValue.Create "search"
                    req["query"] <- JsonValue.Create query
                    fileOpt |> Option.iter (fun f -> req["file"] <- JsonValue.Create f)
                    req["maxResults"] <- JsonValue.Create maxResults
                    return! callDaemon wsRoot req false

                | "rename" :: renameArgs ->
                    let mutable applyEdits = true
                    let filtered = ResizeArray<string>()
                    let mutable cur = renameArgs

                    while not cur.IsEmpty do
                        match cur with
                        | "--dry-run" :: rest ->
                            applyEdits <- false
                            cur <- rest
                        | "--apply" :: rest ->
                            applyEdits <- true
                            cur <- rest
                        | x :: rest ->
                            filtered.Add x
                            cur <- rest
                        | [] -> ()

                    match parsePositionArgs (List.ofSeq filtered) with
                    | Error msg ->
                        Console.Error.WriteLine msg
                        return 1
                    | Ok(file, line, colOpt, symOpt, extras) ->
                        let parsed =
                            match extras with
                            | [ newName ] -> Ok(colOpt, symOpt, newName)
                            | [ oldOrCol; newName ] ->
                                match colOpt, symOpt, Int32.TryParse oldOrCol with
                                | None, None, (true, c) -> Ok(Some c, None, newName)
                                | _, None, _ -> Ok(colOpt, Some oldOrCol, newName)
                                | _, Some _, _ -> Error $"Unexpected extra argument '%s{oldOrCol}' when --symbol was specified."
                            | [] -> Error "Missing <newName> argument for rename."
                            | tooMany ->
                                let joined = String.Join(" ", tooMany)
                                Error $"Unexpected arguments in rename: %s{joined}"

                        match parsed with
                        | Error msg ->
                            Console.Error.WriteLine msg
                            return 1
                        | Ok(finalCol, finalSym, newName) ->
                            let wsRoot = WorkspaceRoot.resolve wsOpt (Some file)
                            let req = JsonObject()
                            req["op"] <- JsonValue.Create "rename"
                            req["file"] <- JsonValue.Create file
                            req["line"] <- JsonValue.Create line
                            req["newName"] <- JsonValue.Create newName
                            req["apply"] <- JsonValue.Create applyEdits
                            finalCol |> Option.iter (fun c -> req["column"] <- JsonValue.Create c)
                            finalSym |> Option.iter (fun s -> req["symbol"] <- JsonValue.Create s)
                            return! callDaemon wsRoot req false

                | unknown :: _ ->
                    Console.Error.WriteLine $"Unknown command '%s{unknown}'. Run 'rfs --help' for usage."
                    return 1
        }
