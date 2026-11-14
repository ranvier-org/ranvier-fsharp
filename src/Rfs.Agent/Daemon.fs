namespace Rfs.Agent

open System
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Runtime.InteropServices
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

module internal DaemonDispatch =

    let executeAsync (engine: SemanticEngine) (daemonPid: int) (req: JsonObject) (ct: CancellationToken) : Task<JsonObject> =
        task {
            let resp = JsonObject()
            let op = JsonUtil.str req "op" |> Option.defaultValue ""

            try
                match op with
                | "ping" ->
                    resp["ok"] <- JsonValue.Create true
                    resp["pid"] <- JsonValue.Create daemonPid
                    resp["text"] <- JsonValue.Create "pong"
                    return resp

                | "status" ->
                    let reload = JsonUtil.boolOpt req "reload" |> Option.defaultValue false
                    let asJson = JsonUtil.boolOpt req "json" |> Option.defaultValue false

                    if asJson then
                        let! statusObj = engine.StatusJson(reload, daemonPid)
                        resp["ok"] <- JsonValue.Create true
                        resp["text"] <- JsonValue.Create(statusObj.ToJsonString())
                        resp["data"] <- statusObj
                    else
                        let! txt = engine.StatusText(reload, daemonPid)
                        resp["ok"] <- JsonValue.Create true
                        resp["text"] <- JsonValue.Create txt

                    return resp

                | "check" ->
                    let fileOpt = JsonUtil.str req "file"
                    let projOpt = JsonUtil.str req "project"
                    let sev = JsonUtil.str req "severity" |> Option.defaultValue "warning"
                    let downstream = JsonUtil.boolOpt req "downstream" |> Option.defaultValue true
                    let! res = engine.CheckAsync(fileOpt, projOpt, sev, downstream, ct)

                    match res with
                    | Ok txt ->
                        resp["ok"] <- JsonValue.Create true
                        resp["text"] <- JsonValue.Create txt
                    | Error msg ->
                        resp["ok"] <- JsonValue.Create false
                        resp["error"] <- JsonValue.Create msg

                    return resp

                | "symbol" ->
                    match JsonUtil.str req "file", JsonUtil.intOpt req "line" with
                    | Some file, Some line ->
                        let colOpt = JsonUtil.intOpt req "column"
                        let symOpt = JsonUtil.str req "symbol"
                        let! res = engine.SymbolAsync(file, line, colOpt, symOpt, ct)

                        match res with
                        | Ok txt ->
                            resp["ok"] <- JsonValue.Create true
                            resp["text"] <- JsonValue.Create txt
                        | Error msg ->
                            resp["ok"] <- JsonValue.Create false
                            resp["error"] <- JsonValue.Create msg

                        return resp
                    | _ ->
                        resp["ok"] <- JsonValue.Create false
                        resp["error"] <- JsonValue.Create "symbol requires 'file' (string) and 'line' (1-based integer)."
                        return resp

                | "references" ->
                    match JsonUtil.str req "file", JsonUtil.intOpt req "line" with
                    | Some file, Some line ->
                        let colOpt = JsonUtil.intOpt req "column"
                        let symOpt = JsonUtil.str req "symbol"
                        let mode = JsonUtil.str req "mode" |> Option.defaultValue "all"
                        let maxRes = JsonUtil.intOpt req "maxResults" |> Option.defaultValue 100
                        let! res = engine.ReferencesAsync(file, line, colOpt, symOpt, mode, maxRes, ct)

                        match res with
                        | Ok txt ->
                            resp["ok"] <- JsonValue.Create true
                            resp["text"] <- JsonValue.Create txt
                        | Error msg ->
                            resp["ok"] <- JsonValue.Create false
                            resp["error"] <- JsonValue.Create msg

                        return resp
                    | _ ->
                        resp["ok"] <- JsonValue.Create false
                        resp["error"] <- JsonValue.Create "references requires 'file' (string) and 'line' (1-based integer)."
                        return resp

                | "search" ->
                    let queryOpt = JsonUtil.strRaw req "query"
                    let fileOpt = JsonUtil.str req "file"
                    let maxRes = JsonUtil.intOpt req "maxResults" |> Option.defaultValue 50
                    let! res = engine.SearchAsync(queryOpt, fileOpt, maxRes, ct)

                    match res with
                    | Ok txt ->
                        resp["ok"] <- JsonValue.Create true
                        resp["text"] <- JsonValue.Create txt
                    | Error msg ->
                        resp["ok"] <- JsonValue.Create false
                        resp["error"] <- JsonValue.Create msg

                    return resp

                | "rename" ->
                    match JsonUtil.str req "file", JsonUtil.intOpt req "line", JsonUtil.str req "newName" with
                    | Some file, Some line, Some newName ->
                        let colOpt = JsonUtil.intOpt req "column"
                        let symOpt = JsonUtil.str req "symbol"
                        let applyEdits = JsonUtil.boolOpt req "apply" |> Option.defaultValue true
                        let! res = engine.RenameAsync(file, line, newName, colOpt, symOpt, applyEdits, ct)

                        match res with
                        | Ok txt ->
                            resp["ok"] <- JsonValue.Create true
                            resp["text"] <- JsonValue.Create txt
                        | Error msg ->
                            resp["ok"] <- JsonValue.Create false
                            resp["error"] <- JsonValue.Create msg

                        return resp
                    | _ ->
                        resp["ok"] <- JsonValue.Create false
                        resp["error"] <- JsonValue.Create "rename requires 'file', 'line' (1-based integer), and 'newName'."
                        return resp

                | other ->
                    resp["ok"] <- JsonValue.Create false
                    resp["error"] <- JsonValue.Create $"Unknown daemon operation: '%s{other}'."
                    return resp
            with
            | :? OperationCanceledException ->
                resp["ok"] <- JsonValue.Create false
                resp["error"] <- JsonValue.Create "Operation canceled."
                return resp
            | ex ->
                resp["ok"] <- JsonValue.Create false
                resp["error"] <- JsonValue.Create $"Internal daemon error: %s{ex.Message}"
                return resp
        }

module internal DaemonServer =

    let private acquireStartupMutex (workspaceId: string) : Mutex option =
        let name = $"Local\\rfs-daemon-start-%s{WorkspaceRoot.shortHash (ProcessRegistry.stateDir () + workspaceId) 16}"
        let m = new Mutex(false, name)

        let acquired =
            try
                m.WaitOne(TimeSpan.FromSeconds 15.)
            with :? AbandonedMutexException ->
                true

        if acquired then
            Some m
        else
            m.Dispose()
            None

    let runAsync (workspaceRoot: string) (idleTimeoutSec: int) (detachStdio: bool) (externalCt: CancellationToken) : Task<int> =
        task {
            let wsRoot = WorkspaceRoot.canonical workspaceRoot
            let wsId = WorkspaceRoot.workspaceId wsRoot
            let stateDir = ProcessRegistry.stateDir ()
            let pipeName = WorkspaceRoot.pipeName stateDir wsRoot

            ProcessRegistry.ensureDirs ()

            let logWriter =
                if detachStdio then
                    try
                        Console.SetIn TextReader.Null
                        Console.SetOut TextWriter.Null
                        try Console.OpenStandardInput().Close() with _ -> ()
                        try Console.OpenStandardOutput().Close() with _ -> ()
                        try Console.OpenStandardError().Close() with _ -> ()
                    with _ ->
                        ()

                    try
                        let fs = new FileStream(ProcessRegistry.daemonLogPath wsId, FileMode.Append, FileAccess.Write, FileShare.ReadWrite)
                        let sw = new StreamWriter(fs, AutoFlush = true)
                        Console.SetError(TextWriter.Synchronized sw)
                        Some(sw :> IDisposable)
                    with _ ->
                        Console.SetError TextWriter.Null
                        None
                else
                    None

            use _logDispose =
                { new IDisposable with
                    member _.Dispose() =
                        match logWriter with
                        | Some d -> d.Dispose()
                        | None -> () }

            // Ensure only one live daemon binds this (stateDir, workspaceRoot) pipe.
            let startupMutex = acquireStartupMutex wsId
            let mutable alreadyServing = false

            try
                match ProcessRegistry.tryReadDaemonLease (ProcessRegistry.daemonLeasePath wsId) with
                | Some existing when existing.Pid <> Environment.ProcessId && existing.BinaryStamp = ProcessRegistry.currentBinaryStamp () ->
                    let! alive = ProcessRegistry.pingPipeAsync existing.PipeName 1000
                    alreadyServing <- alive
                | _ -> ()

                if alreadyServing then
                    return 0
                else
                    let mutable procRec = ProcessRegistry.registerSelf "daemon" wsRoot pipeName idleTimeoutSec
                    let lease = ProcessRegistry.writeDaemonLease procRec
                    let mutable lastActiveTicks = DateTime.UtcNow.Ticks
                    let mutable inFlightRequests = 0
                    let daemonPid = Environment.ProcessId

                    use cts = CancellationTokenSource.CreateLinkedTokenSource externalCt
                    use engine = new SemanticEngine(wsRoot)

                    // Warm load asynchronously so "ping" replies immediately while MSBuild/FCS loads.
                    let warmTask: Task = Task.Run(Func<Task>(fun () -> engine.EnsureLoadedAsync()))

                    let recHeartbeat (activeTicks: int64) =
                        procRec <- ProcessRegistry.touchProcessRecord procRec activeTicks

                    let handleClientAsync (pipe: NamedPipeServerStream) =
                        task {
                            use pipe = pipe

                            try
                                let mutable openConn = true

                                while openConn && not cts.IsCancellationRequested do
                                    let! msgOpt = PipeFraming.readAsync pipe cts.Token

                                    match msgOpt with
                                    | None -> openConn <- false
                                    | Some req ->
                                        let now = DateTime.UtcNow.Ticks
                                        Volatile.Write(&lastActiveTicks, now)

                                        if JsonUtil.str req "op" = Some "shutdown" then
                                            let ack = JsonObject()
                                            ack["ok"] <- JsonValue.Create true
                                            ack["text"] <- JsonValue.Create $"Daemon %d{daemonPid} shutting down."
                                            do! PipeFraming.writeAsync pipe ack cts.Token
                                            cts.Cancel()
                                            openConn <- false
                                        else
                                            Interlocked.Increment(&inFlightRequests) |> ignore

                                            try
                                                let! resp = DaemonDispatch.executeAsync engine daemonPid req cts.Token
                                                do! PipeFraming.writeAsync pipe resp cts.Token
                                            finally
                                                Volatile.Write(&lastActiveTicks, DateTime.UtcNow.Ticks)
                                                Interlocked.Decrement(&inFlightRequests) |> ignore
                            with _ ->
                                ()
                        }

                    let watchdog =
                        Task.Run<unit>(fun () ->
                            task {
                                let intervalMs = if idleTimeoutSec <= 5 then 500 else 15000
                                use timer = new PeriodicTimer(TimeSpan.FromMilliseconds(float intervalMs))

                                try
                                    while! timer.WaitForNextTickAsync cts.Token do
                                        if Volatile.Read &inFlightRequests > 0 then
                                            let nowTicks = DateTime.UtcNow.Ticks
                                            Volatile.Write(&lastActiveTicks, nowTicks)
                                            recHeartbeat nowTicks
                                        else
                                            let last = Volatile.Read &lastActiveTicks
                                            recHeartbeat last
                                            let idleSpan = TimeSpan.FromTicks(max 0L (DateTime.UtcNow.Ticks - last))

                                            if idleSpan > TimeSpan.FromSeconds(float idleTimeoutSec) then
                                                cts.Cancel()
                                with
                                | :? OperationCanceledException -> ()
                                | _ -> ()
                            })

                    // Release startup mutex after lease/record written? Actually keep accept ready FIRST!
                    let mutable releasedMutex = false

                    let releaseStartup () =
                        if not releasedMutex then
                            releasedMutex <- true

                            match startupMutex with
                            | Some m ->
                                try m.ReleaseMutex() with _ -> ()
                                m.Dispose()
                            | None -> ()

                    try
                        while not cts.IsCancellationRequested do
                            let server =
                                new NamedPipeServerStream(
                                    pipeName,
                                    PipeDirection.InOut,
                                    NamedPipeServerStream.MaxAllowedServerInstances,
                                    PipeTransmissionMode.Byte,
                                    PipeOptions.Asynchronous
                                )

                            // First listener created; waiting clients can now connect safely
                            releaseStartup ()

                            try
                                do! server.WaitForConnectionAsync cts.Token
                                Volatile.Write(&lastActiveTicks, DateTime.UtcNow.Ticks)
                                Task.Run<unit>(fun () -> handleClientAsync server) |> ignore
                            with
                            | :? OperationCanceledException ->
                                server.Dispose()
                            | _ ->
                                server.Dispose()
                                if not cts.IsCancellationRequested then
                                    do! Task.Delay(50, cts.Token)
                    with :? OperationCanceledException ->
                        ()

                    releaseStartup ()

                    try
                        do! watchdog
                    with _ ->
                        ()

                    try
                        if not warmTask.IsCompleted then
                            warmTask.Wait(1000) |> ignore
                    with _ ->
                        ()

                    ProcessRegistry.tryDeleteIfOwned procRec (Some lease)
                    return 0
            finally
                match startupMutex with
                | Some m ->
                    try m.Dispose() with _ -> ()
                | None -> ()
        }

module internal Win32Spawn =

    [<Struct; StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)>]
    type STARTUPINFOW =
        val mutable cb: uint32
        val mutable lpReserved: IntPtr
        val mutable lpDesktop: IntPtr
        val mutable lpTitle: IntPtr
        val mutable dwX: uint32
        val mutable dwY: uint32
        val mutable dwXSize: uint32
        val mutable dwYSize: uint32
        val mutable dwXCountChars: uint32
        val mutable dwYCountChars: uint32
        val mutable dwFillAttribute: uint32
        val mutable dwFlags: uint32
        val mutable wShowWindow: uint16
        val mutable cbReserved2: uint16
        val mutable lpReserved2: IntPtr
        val mutable hStdInput: IntPtr
        val mutable hStdOutput: IntPtr
        val mutable hStdError: IntPtr

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type PROCESS_INFORMATION =
        val mutable hProcess: IntPtr
        val mutable hThread: IntPtr
        val mutable dwProcessId: uint32
        val mutable dwThreadId: uint32

    [<DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Unicode)>]
    extern bool private CreateProcessW(
        string lpApplicationName,
        IntPtr lpCommandLine,
        IntPtr lpProcessAttributes,
        IntPtr lpThreadAttributes,
        [<MarshalAs(UnmanagedType.Bool)>] bool bInheritHandles,
        uint32 dwCreationFlags,
        IntPtr lpEnvironment,
        string lpCurrentDirectory,
        STARTUPINFOW& lpStartupInfo,
        PROCESS_INFORMATION& lpProcessInformation
    )

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool private CloseHandle(IntPtr hObject)

    let private quoteWinArg (arg: string) =
        if String.IsNullOrEmpty arg then
            "\"\""
        else
            let sb = StringBuilder(arg.Length + 4)
            sb.Append '"' |> ignore
            let mutable bs = 0

            for ch in arg do
                if ch = '\\' then
                    bs <- bs + 1
                elif ch = '"' then
                    sb.Append('\\', bs * 2 + 1).Append '"' |> ignore
                    bs <- 0
                else
                    if bs > 0 then
                        sb.Append('\\', bs) |> ignore
                        bs <- 0

                    sb.Append ch |> ignore

            if bs > 0 then
                sb.Append('\\', bs * 2) |> ignore

            sb.Append '"' |> ignore
            sb.ToString()

    let startDetached (fileName: string) (args: string list) (workingDirectory: string) =
        let cmdLine =
            fileName :: args
            |> List.map quoteWinArg
            |> String.concat " "

        let cmdPtr = Marshal.StringToHGlobalUni cmdLine

        try
            let mutable si = Unchecked.defaultof<STARTUPINFOW>
            si.cb <- uint32 (Marshal.SizeOf<STARTUPINFOW>())
            let mutable pi = Unchecked.defaultof<PROCESS_INFORMATION>
            // CREATE_NO_WINDOW (0x08000000) | DETACHED_PROCESS (0x00000008), bInheritHandles = false
            let flags = 0x08000008u

            let ok =
                CreateProcessW(
                    null,
                    cmdPtr,
                    IntPtr.Zero,
                    IntPtr.Zero,
                    false,
                    flags,
                    IntPtr.Zero,
                    workingDirectory,
                    &si,
                    &pi
                )

            if not ok then
                let err = Marshal.GetLastWin32Error()
                invalidOp $"CreateProcessW failed starting detached daemon '%s{fileName}' (Win32 error %d{err})."

            if pi.hThread <> IntPtr.Zero then
                CloseHandle pi.hThread |> ignore

            if pi.hProcess <> IntPtr.Zero then
                CloseHandle pi.hProcess |> ignore
        finally
            Marshal.FreeHGlobal cmdPtr

module internal DaemonClient =

    let private throttledAutoReapAsync (forceCheck: bool) =
        task {
            let dir = ProcessRegistry.stateDir ()
            Directory.CreateDirectory dir |> ignore
            let stampPath = Path.Combine(dir, "last_reap.stamp")
            let now = DateTime.UtcNow

            let shouldRun =
                if forceCheck then
                    true
                else
                    try
                        if not (File.Exists stampPath) then
                            true
                        else
                            (now - File.GetLastWriteTimeUtc stampPath) > TimeSpan.FromSeconds 10.
                    with _ ->
                        true

            if shouldRun then
                try
                    File.WriteAllText(stampPath, now.Ticks.ToString())
                with _ ->
                    ()

                let! _ = ProcessRegistry.reapStaleProcessesAsync false false (ProcessRegistry.currentBinaryStamp ()) true
                ()
        }

    let private resolveDaemonCommand () : string * string list =
        let dllPath = typeof<SemanticEngine>.Assembly.Location
        let dllDir = Path.GetDirectoryName dllPath
        let exeName = if OperatingSystem.IsWindows() then "rfs.exe" else "rfs"
        let exePath = if String.IsNullOrEmpty dllDir then "" else Path.Combine(dllDir, exeName)
        let procPath = Environment.ProcessPath

        if not (String.IsNullOrEmpty procPath) && FileInfo(procPath).Name.StartsWith("rfs", StringComparison.OrdinalIgnoreCase) then
            procPath, [ "daemon"; "run" ]
        elif not (String.IsNullOrEmpty exePath) && File.Exists exePath then
            exePath, [ "daemon"; "run" ]
        elif not (String.IsNullOrEmpty procPath) && FileInfo(procPath).Name.StartsWith("dotnet", StringComparison.OrdinalIgnoreCase) then
            procPath, [ dllPath; "daemon"; "run" ]
        else
            "dotnet", [ dllPath; "daemon"; "run" ]

    let private startDetachedDaemon (workspaceRoot: string) (idleTimeoutSec: int) : unit =
        let fileName, baseArgs = resolveDaemonCommand ()

        let allArgs =
            baseArgs
            @ [ "--workspace"; workspaceRoot; "--idle-timeout"; string idleTimeoutSec ]

        if OperatingSystem.IsWindows() then
            Win32Spawn.startDetached fileName allArgs workspaceRoot
        else
            let psi = ProcessStartInfo(fileName)

            for a in allArgs do
                psi.ArgumentList.Add a

            psi.WorkingDirectory <- workspaceRoot
            psi.UseShellExecute <- false
            psi.CreateNoWindow <- true
            psi.RedirectStandardInput <- true
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true

            match Process.Start psi with
            | null -> invalidOp $"Failed to spawn daemon process '%s{fileName}'."
            | proc ->
                try proc.StandardInput.Close() with _ -> ()
                try proc.StandardOutput.Close() with _ -> ()
                try proc.StandardError.Close() with _ -> ()
                proc.Dispose()

    let ensureDaemonPipeAsync (workspaceRoot: string) (ct: CancellationToken) : Task<string> =
        task {
            let wsRoot = WorkspaceRoot.canonical workspaceRoot
            let wsId = WorkspaceRoot.workspaceId wsRoot
            let stateDir = ProcessRegistry.stateDir ()
            let expectedPipe = WorkspaceRoot.pipeName stateDir wsRoot
            let currentStamp = ProcessRegistry.currentBinaryStamp ()

            let leaseOpt = ProcessRegistry.tryReadDaemonLease (ProcessRegistry.daemonLeasePath wsId)

            let binaryMismatched =
                match leaseOpt with
                | Some l -> l.BinaryStamp <> currentStamp
                | None -> false

            // Reap immediately on binary mismatch; otherwise throttle stale scan to once per 10s
            do! throttledAutoReapAsync binaryMismatched

            let activeLease =
                match ProcessRegistry.tryReadDaemonLease (ProcessRegistry.daemonLeasePath wsId) with
                | Some l when l.BinaryStamp = currentStamp -> Some l
                | _ -> None

            let! needSpawn =
                task {
                    match activeLease with
                    | None -> return true
                    | Some l ->
                        let! ok = ProcessRegistry.pingPipeAsync l.PipeName 1500

                        if ok then
                            return false
                        else
                            let! _ = ProcessRegistry.reapStaleProcessesAsync false false currentStamp true
                            return true
                }

            if needSpawn then
                startDetachedDaemon wsRoot (ProcessRegistry.defaultIdleTimeoutSeconds ())

            let targetPipe =
                match ProcessRegistry.tryReadDaemonLease (ProcessRegistry.daemonLeasePath wsId) with
                | Some l when l.BinaryStamp = currentStamp -> l.PipeName
                | _ -> expectedPipe

            // Wait for daemon pipe listener to start accepting connections
            let sw = Stopwatch.StartNew()
            let mutable connected = false

            while not connected && sw.Elapsed < TimeSpan.FromSeconds 15. do
                ct.ThrowIfCancellationRequested()

                try
                    use probeCts = CancellationTokenSource.CreateLinkedTokenSource ct
                    probeCts.CancelAfter 800
                    use pipe = new NamedPipeClientStream(".", targetPipe, PipeDirection.InOut, PipeOptions.Asynchronous)
                    do! pipe.ConnectAsync probeCts.Token
                    connected <- true
                with
                | :? OperationCanceledException when not ct.IsCancellationRequested ->
                    do! Task.Delay(50, ct)
                | _ when not ct.IsCancellationRequested ->
                    do! Task.Delay(50, ct)

            return targetPipe
        }

    let requestAsync (workspaceRoot: string) (req: JsonObject) (ct: CancellationToken) : Task<JsonObject> =
        task {
            let! pipeName = ensureDaemonPipeAsync workspaceRoot ct
            use client = new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous)
            use connectCts = CancellationTokenSource.CreateLinkedTokenSource ct
            connectCts.CancelAfter(TimeSpan.FromSeconds 15.)
            do! client.ConnectAsync connectCts.Token

            do! PipeFraming.writeAsync client req ct

            match! PipeFraming.readAsync client ct with
            | Some resp -> return resp
            | None -> return invalidOp "Daemon closed connection before sending response."
        }

    let shutdownWorkspaceAsync (workspaceRoot: string) : Task<bool> =
        task {
            let wsRoot = WorkspaceRoot.canonical workspaceRoot
            let wsId = WorkspaceRoot.workspaceId wsRoot

            match ProcessRegistry.tryReadDaemonLease (ProcessRegistry.daemonLeasePath wsId) with
            | None -> return false
            | Some lease ->
                try
                    use cts = new CancellationTokenSource 3000
                    use client = new NamedPipeClientStream(".", lease.PipeName, PipeDirection.InOut, PipeOptions.Asynchronous)
                    do! client.ConnectAsync cts.Token
                    let req = JsonObject()
                    req["op"] <- JsonValue.Create "shutdown"
                    do! PipeFraming.writeAsync client req cts.Token
                    let! _ = PipeFraming.readAsync client cts.Token
                    return true
                with _ ->
                    return false
        }
