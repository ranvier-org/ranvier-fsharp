namespace Rfs.Agent

open System
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Runtime.InteropServices
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

[<NoComparison; NoEquality>]
type internal ProcessRecord =
    { Pid: int
      Kind: string
      WorkspaceRoot: string
      WorkspaceId: string
      PipeName: string
      ProcessStartUtcTicks: int64
      ParentPid: int option
      ParentStartUtcTicks: int64 option
      BinaryStamp: string
      StartedUtcTicks: int64
      LastActiveUtcTicks: int64
      IdleTimeoutSeconds: int
      RecordPath: string }

[<NoComparison; NoEquality>]
type internal DaemonLease =
    { WorkspaceRoot: string
      WorkspaceId: string
      PipeName: string
      Pid: int
      ProcessStartUtcTicks: int64
      BinaryStamp: string
      StartedUtcTicks: int64
      LeasePath: string }

type internal ReapReason =
    | DeadLeaseCleaned
    | RecycledPidCleaned
    | KilledBinaryMismatch
    | KilledIdleExpired
    | KilledOrphanedParent
    | KilledUnresponsive
    | KilledForced
    | KilledOrphanedLsp

[<NoComparison; NoEquality>]
type internal ReapAction =
    { Pid: int
      Kind: string
      WorkspaceRoot: string
      Reason: ReapReason }

[<NoComparison; NoEquality>]
type internal ReapReport =
    { CheckedRecords: int
      Actions: ReapAction list }

module internal ProcessRegistry =

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type private ProcessBasicInformation =
        val mutable ExitStatus: IntPtr
        val mutable PebBaseAddress: IntPtr
        val mutable AffinityMask: IntPtr
        val mutable BasePriority: IntPtr
        val mutable UniqueProcessId: IntPtr
        val mutable InheritedFromUniqueProcessId: IntPtr

    [<DllImport("ntdll.dll")>]
    extern int private NtQueryInformationProcess(
        IntPtr processHandle,
        int processInformationClass,
        ProcessBasicInformation& processInformation,
        int processInformationLength,
        int& returnLength
    )

    let tryGetParentPid (proc: Process) : int option =
        try
            if OperatingSystem.IsWindows() then
                let mutable pbi = ProcessBasicInformation()
                let mutable retLen = 0
                let status = NtQueryInformationProcess(proc.Handle, 0, &pbi, Marshal.SizeOf<ProcessBasicInformation>(), &retLen)

                if status = 0 then
                    let parent = pbi.InheritedFromUniqueProcessId.ToInt64()
                    if parent > 0L && parent <= int64 Int32.MaxValue then Some(int parent) else None
                else
                    None
            elif OperatingSystem.IsLinux() then
                let statPath = $"/proc/{proc.Id}/stat"

                if File.Exists statPath then
                    let text = File.ReadAllText statPath
                    let rparen = text.LastIndexOf ')'

                    if rparen >= 0 && rparen + 2 < text.Length then
                        let parts = text.Substring(rparen + 2).Split(' ', StringSplitOptions.RemoveEmptyEntries)
                        // After ')': [0]=state [1]=ppid
                        if parts.Length >= 2 then
                            match Int32.TryParse parts[1] with
                            | true, ppid when ppid > 0 -> Some ppid
                            | _ -> None
                        else
                            None
                    else
                        None
                else
                    None
            else
                None
        with _ ->
            None

    let tryGetProcessStartTicks (pid: int) : int64 option =
        try
            use p = Process.GetProcessById pid
            if p.HasExited then None else Some(p.StartTime.ToUniversalTime().Ticks)
        with _ ->
            None

    let stateDir () : string =
        match Environment.GetEnvironmentVariable "RFS_STATE_DIR" with
        | null
        | "" -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "ranvier-fsharp")
        | custom -> Path.GetFullPath custom

    let daemonsDir () = Path.Combine(stateDir (), "daemons")
    let procsDir () = Path.Combine(stateDir (), "procs")
    let ensureDirs () =
        Directory.CreateDirectory(daemonsDir ()) |> ignore
        Directory.CreateDirectory(procsDir ()) |> ignore

    let daemonLeasePath (workspaceId: string) =
        Path.Combine(daemonsDir (), $"%s{workspaceId}.json")

    let daemonLogPath (workspaceId: string) =
        Path.Combine(daemonsDir (), $"%s{workspaceId}.log")

    let daemonLockPath (workspaceId: string) =
        Path.Combine(daemonsDir (), $"%s{workspaceId}.spawn.lock")

    let procRecordPath (pid: int) =
        Path.Combine(procsDir (), $"%d{pid}.json")

    let defaultIdleTimeoutSeconds () : int =
        match Environment.GetEnvironmentVariable "RFS_IDLE_TIMEOUT_SECONDS" with
        | null
        | "" -> 1800
        | raw ->
            match Int32.TryParse raw with
            | true, v when v >= 1 -> v
            | _ -> 1800

    let currentBinaryStamp () : string =
        try
            let asmLoc = typeof<ProcessRecord>.Assembly.Location

            if not (String.IsNullOrWhiteSpace asmLoc) then
                let fi = FileInfo asmLoc

                if fi.Exists then
                    $"dll:%d{fi.LastWriteTimeUtc.Ticks}:%d{fi.Length}"
                else
                    "dll:missing"
            else
                match Environment.ProcessPath with
                | null
                | "" -> "unknown"
                | exeLoc ->
                    let fi = FileInfo exeLoc

                    if fi.Exists then
                        $"exe:%s{fi.Name}:%d{fi.LastWriteTimeUtc.Ticks}:%d{fi.Length}"
                    else
                        "unknown"
        with _ ->
            "unknown"

    let atomicWriteText (path: string) (content: string) : unit =
        let dir = Path.GetDirectoryName path

        if not (String.IsNullOrEmpty dir) then
            Directory.CreateDirectory dir |> ignore

        let tmp = $"%s{path}.tmp.%d{Environment.ProcessId}.%d{Guid.NewGuid().GetHashCode()}"
        File.WriteAllText(tmp, content)
        File.Move(tmp, path, overwrite = true)

    let private writeProcessJson (r: ProcessRecord) =
        let o = JsonObject()
        o["pid"] <- JsonValue.Create r.Pid
        o["kind"] <- JsonValue.Create r.Kind
        o["workspaceRoot"] <- JsonValue.Create r.WorkspaceRoot
        o["workspaceId"] <- JsonValue.Create r.WorkspaceId
        o["pipeName"] <- JsonValue.Create r.PipeName
        o["processStartUtcTicks"] <- JsonValue.Create r.ProcessStartUtcTicks

        match r.ParentPid with
        | Some p -> o["parentPid"] <- JsonValue.Create p
        | None -> ()

        match r.ParentStartUtcTicks with
        | Some t -> o["parentStartUtcTicks"] <- JsonValue.Create t
        | None -> ()

        o["binaryStamp"] <- JsonValue.Create r.BinaryStamp
        o["startedUtcTicks"] <- JsonValue.Create r.StartedUtcTicks
        o["lastActiveUtcTicks"] <- JsonValue.Create r.LastActiveUtcTicks
        o["idleTimeoutSeconds"] <- JsonValue.Create r.IdleTimeoutSeconds
        atomicWriteText r.RecordPath (o.ToJsonString())

    let registerSelf (kind: string) (workspaceRoot: string) (pipeName: string) (idleTimeoutSec: int) : ProcessRecord =
        ensureDirs ()
        use self = Process.GetCurrentProcess()
        let nowTicks = DateTime.UtcNow.Ticks
        let parentPid = tryGetParentPid self
        let parentStart = parentPid |> Option.bind tryGetProcessStartTicks
        let wsId = WorkspaceRoot.workspaceId workspaceRoot

        let record =
            { Pid = self.Id
              Kind = kind
              WorkspaceRoot = workspaceRoot
              WorkspaceId = wsId
              PipeName = pipeName
              ProcessStartUtcTicks = self.StartTime.ToUniversalTime().Ticks
              ParentPid = parentPid
              ParentStartUtcTicks = parentStart
              BinaryStamp = currentBinaryStamp ()
              StartedUtcTicks = nowTicks
              LastActiveUtcTicks = nowTicks
              IdleTimeoutSeconds = idleTimeoutSec
              RecordPath = procRecordPath self.Id }

        writeProcessJson record
        record

    let touchProcessRecord (record: ProcessRecord) (lastActiveTicks: int64) : ProcessRecord =
        let updated = { record with LastActiveUtcTicks = lastActiveTicks }

        try
            writeProcessJson updated
        with _ ->
            ()

        updated

    let writeDaemonLease (record: ProcessRecord) : DaemonLease =
        ensureDirs ()

        let lease =
            { WorkspaceRoot = record.WorkspaceRoot
              WorkspaceId = record.WorkspaceId
              PipeName = record.PipeName
              Pid = record.Pid
              ProcessStartUtcTicks = record.ProcessStartUtcTicks
              BinaryStamp = record.BinaryStamp
              StartedUtcTicks = record.StartedUtcTicks
              LeasePath = daemonLeasePath record.WorkspaceId }

        let o = JsonObject()
        o["workspaceRoot"] <- JsonValue.Create lease.WorkspaceRoot
        o["workspaceId"] <- JsonValue.Create lease.WorkspaceId
        o["pipeName"] <- JsonValue.Create lease.PipeName
        o["pid"] <- JsonValue.Create lease.Pid
        o["processStartUtcTicks"] <- JsonValue.Create lease.ProcessStartUtcTicks
        o["binaryStamp"] <- JsonValue.Create lease.BinaryStamp
        o["startedUtcTicks"] <- JsonValue.Create lease.StartedUtcTicks
        atomicWriteText lease.LeasePath (o.ToJsonString())
        lease

    let tryReadProcessRecord (path: string) : ProcessRecord option =
        try
            if not (File.Exists path) then
                None
            else
                match JsonNode.Parse(File.ReadAllText path) with
                | :? JsonObject as o ->
                    match
                        JsonUtil.intOpt o "pid",
                        JsonUtil.str o "kind",
                        JsonUtil.str o "workspaceRoot",
                        JsonUtil.str o "workspaceId",
                        JsonUtil.str o "pipeName",
                        JsonUtil.int64Opt o "processStartUtcTicks",
                        JsonUtil.str o "binaryStamp",
                        JsonUtil.int64Opt o "lastActiveUtcTicks"
                    with
                    | Some pid, Some kind, Some wsRoot, Some wsId, Some pipe, Some startTicks, Some stamp, Some lastTicks ->
                        let started = JsonUtil.int64Opt o "startedUtcTicks" |> Option.defaultValue startTicks
                        let ttl = JsonUtil.intOpt o "idleTimeoutSeconds" |> Option.defaultValue 1800

                        Some
                            { Pid = pid
                              Kind = kind
                              WorkspaceRoot = wsRoot
                              WorkspaceId = wsId
                              PipeName = pipe
                              ProcessStartUtcTicks = startTicks
                              ParentPid = JsonUtil.intOpt o "parentPid"
                              ParentStartUtcTicks = JsonUtil.int64Opt o "parentStartUtcTicks"
                              BinaryStamp = stamp
                              StartedUtcTicks = started
                              LastActiveUtcTicks = lastTicks
                              IdleTimeoutSeconds = ttl
                              RecordPath = path }
                    | _ -> None
                | _ -> None
        with _ ->
            None

    let tryReadDaemonLease (path: string) : DaemonLease option =
        try
            if not (File.Exists path) then
                None
            else
                match JsonNode.Parse(File.ReadAllText path) with
                | :? JsonObject as o ->
                    match
                        JsonUtil.str o "workspaceRoot",
                        JsonUtil.str o "workspaceId",
                        JsonUtil.str o "pipeName",
                        JsonUtil.intOpt o "pid",
                        JsonUtil.int64Opt o "processStartUtcTicks",
                        JsonUtil.str o "binaryStamp"
                    with
                    | Some wsRoot, Some wsId, Some pipe, Some pid, Some startTicks, Some stamp ->
                        let started = JsonUtil.int64Opt o "startedUtcTicks" |> Option.defaultValue startTicks

                        Some
                            { WorkspaceRoot = wsRoot
                              WorkspaceId = wsId
                              PipeName = pipe
                              Pid = pid
                              ProcessStartUtcTicks = startTicks
                              BinaryStamp = stamp
                              StartedUtcTicks = started
                              LeasePath = path }
                    | _ -> None
                | _ -> None
        with _ ->
            None

    let tryDeleteIfOwned (record: ProcessRecord) (leaseOpt: DaemonLease option) : unit =
        try
            match leaseOpt with
            | Some lease ->
                match tryReadDaemonLease lease.LeasePath with
                | Some current when current.Pid = record.Pid && current.StartedUtcTicks = lease.StartedUtcTicks ->
                    File.Delete lease.LeasePath
                | _ -> ()
            | None -> ()
        with _ ->
            ()

        try
            match tryReadProcessRecord record.RecordPath with
            | Some current when current.Pid = record.Pid && current.ProcessStartUtcTicks = record.ProcessStartUtcTicks ->
                File.Delete record.RecordPath
            | _ -> ()
        with _ ->
            ()

    let listProcessRecords () : ProcessRecord list =
        ensureDirs ()

        Directory.EnumerateFiles(procsDir (), "*.json", SearchOption.TopDirectoryOnly)
        |> Seq.choose tryReadProcessRecord
        |> List.ofSeq

    let listDaemonLeases () : DaemonLease list =
        ensureDirs ()

        Directory.EnumerateFiles(daemonsDir (), "*.json", SearchOption.TopDirectoryOnly)
        |> Seq.choose tryReadDaemonLease
        |> List.ofSeq

    [<NoComparison; NoEquality>]
    type private PidProbe =
        | Dead
        | Recycled
        | Alive of Process

    let private expectedProcessNames =
        set [ "rfs"; "dotnet"; "ranvierautocomplete"; "testhost" ]

    let private probeProcess (pid: int) (recordedStartTicks: int64) : PidProbe =
        try
            let proc = Process.GetProcessById pid

            try
                if proc.HasExited then
                    proc.Dispose()
                    Dead
                else
                    let actualStart = proc.StartTime.ToUniversalTime().Ticks
                    let nameOk = expectedProcessNames.Contains(proc.ProcessName.ToLowerInvariant())
                    let diff = Math.Abs(actualStart - recordedStartTicks)

                    if not nameOk || diff > TimeSpan.FromSeconds(2.).Ticks then
                        proc.Dispose()
                        Recycled
                    else
                        Alive proc
            with
            | :? Win32Exception
            | :? InvalidOperationException ->
                proc.Dispose()
                Recycled
        with
        | :? ArgumentException -> Dead
        | :? InvalidOperationException -> Dead

    let private safeDelete (path: string) =
        try
            if File.Exists path then
                File.Delete path
        with _ ->
            ()

    let private probeProcessWithAnyName (pid: int) (recordedStartTicks: int64) : bool =
        try
            use proc = Process.GetProcessById pid

            if proc.HasExited then
                false
            else
                let actualStart = proc.StartTime.ToUniversalTime().Ticks
                Math.Abs(actualStart - recordedStartTicks) <= TimeSpan.FromSeconds(2.).Ticks
        with _ ->
            false

    let private isParentAlive (parentPidOpt: int option) (parentStartOpt: int64 option) : bool =
        match parentPidOpt, parentStartOpt with
        | Some ppid, Some pstart -> probeProcessWithAnyName ppid pstart
        | _ -> true

    let pingPipeAsync (pipeName: string) (timeoutMs: int) : Task<bool> =
        task {
            try
                use cts = new CancellationTokenSource(timeoutMs)
                use client = new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous)
                do! client.ConnectAsync(cts.Token)
                let req = JsonObject()
                req["op"] <- JsonValue.Create "ping"
                do! PipeFraming.writeAsync client req cts.Token
                let! resp = PipeFraming.readAsync client cts.Token

                match resp with
                | Some r -> return JsonUtil.boolOpt r "ok" = Some true
                | None -> return false
            with _ ->
                return false
        }

    let private reasonLabel =
        function
        | DeadLeaseCleaned -> "dead-lease"
        | RecycledPidCleaned -> "recycled-pid"
        | KilledBinaryMismatch -> "binary-mismatch"
        | KilledIdleExpired -> "idle-expired"
        | KilledOrphanedParent -> "orphaned-parent"
        | KilledUnresponsive -> "unresponsive"
        | KilledForced -> "forced-kill"
        | KilledOrphanedLsp -> "orphaned-lsp"

    let formatReason = reasonLabel

    let private removeLeaseMatchingPid (pid: int) =
        for lease in listDaemonLeases () do
            if lease.Pid = pid then
                safeDelete lease.LeasePath

    let private safeKillTree (proc: Process) =
        try
            if not proc.HasExited then
                proc.Kill(entireProcessTree = true)
                proc.WaitForExit(3000) |> ignore
        with _ ->
            ()

    /// Scans registered daemon & MCP process records and leases, cleanly reaping dead, recycled, version-mismatched,
    /// idle-expired, orphaned-parent, or unresponsive daemons. Never kills self or recycled unrelated PIDs.
    let reapStaleProcessesAsync
        (forceAll: bool)
        (includeOrphanedLsp: bool)
        (currentStamp: string)
        (probeUnresponsiveDaemons: bool)
        : Task<ReapReport> =
        task {
            ensureDirs ()
            let actions = ResizeArray<ReapAction>()
            let mutable checkedCount = 0
            let nowTicks = DateTime.UtcNow.Ticks
            let selfPid = Environment.ProcessId

            // 1. Inspect `procs/*.json`
            for file in Directory.EnumerateFiles(procsDir (), "*.json", SearchOption.TopDirectoryOnly) do
                checkedCount <- checkedCount + 1

                match tryReadProcessRecord file with
                | None ->
                    safeDelete file
                | Some recEntry when recEntry.Pid = selfPid ->
                    ()
                | Some recEntry ->
                    match probeProcess recEntry.Pid recEntry.ProcessStartUtcTicks with
                    | Dead ->
                        safeDelete file
                        removeLeaseMatchingPid recEntry.Pid
                        actions.Add { Pid = recEntry.Pid; Kind = recEntry.Kind; WorkspaceRoot = recEntry.WorkspaceRoot; Reason = DeadLeaseCleaned }
                    | Recycled ->
                        safeDelete file
                        removeLeaseMatchingPid recEntry.Pid
                        actions.Add { Pid = recEntry.Pid; Kind = recEntry.Kind; WorkspaceRoot = recEntry.WorkspaceRoot; Reason = RecycledPidCleaned }
                    | Alive proc ->
                        use proc = proc
                        let idleAge = TimeSpan.FromTicks(max 0L (nowTicks - recEntry.LastActiveUtcTicks))
                        let upAge = TimeSpan.FromTicks(max 0L (nowTicks - recEntry.StartedUtcTicks))

                        let mutable reason: ReapReason option =
                            if forceAll then
                                Some KilledForced
                            elif recEntry.BinaryStamp <> currentStamp then
                                Some KilledBinaryMismatch
                            elif idleAge > TimeSpan.FromSeconds(float recEntry.IdleTimeoutSeconds) then
                                Some KilledIdleExpired
                            elif recEntry.Kind = "mcp" && not (isParentAlive recEntry.ParentPid recEntry.ParentStartUtcTicks) then
                                Some KilledOrphanedParent
                            else
                                None

                        if reason.IsNone && probeUnresponsiveDaemons && recEntry.Kind = "daemon" && upAge > TimeSpan.FromSeconds 15. then
                            let! ok = pingPipeAsync recEntry.PipeName 1500

                            if not ok then
                                reason <- Some KilledUnresponsive

                        match reason with
                        | Some r ->
                            safeKillTree proc
                            safeDelete file
                            removeLeaseMatchingPid recEntry.Pid
                            actions.Add { Pid = recEntry.Pid; Kind = recEntry.Kind; WorkspaceRoot = recEntry.WorkspaceRoot; Reason = r }
                        | None -> ()

            // 2. Inspect `daemons/*.json` for orphan leases (lease without a live/matching proc record)
            for lease in listDaemonLeases () do
                checkedCount <- checkedCount + 1

                if lease.Pid <> selfPid then
                    match probeProcess lease.Pid lease.ProcessStartUtcTicks with
                    | Dead ->
                        safeDelete lease.LeasePath
                        actions.Add { Pid = lease.Pid; Kind = "daemon"; WorkspaceRoot = lease.WorkspaceRoot; Reason = DeadLeaseCleaned }
                    | Recycled ->
                        safeDelete lease.LeasePath
                        actions.Add { Pid = lease.Pid; Kind = "daemon"; WorkspaceRoot = lease.WorkspaceRoot; Reason = RecycledPidCleaned }
                    | Alive proc ->
                        use proc = proc
                        let hasProcRec = File.Exists(procRecordPath lease.Pid)

                        if forceAll || lease.BinaryStamp <> currentStamp then
                            let reason = if forceAll then KilledForced else KilledBinaryMismatch
                            safeKillTree proc
                            safeDelete lease.LeasePath
                            safeDelete (procRecordPath lease.Pid)
                            actions.Add { Pid = lease.Pid; Kind = "daemon"; WorkspaceRoot = lease.WorkspaceRoot; Reason = reason }
                        elif not hasProcRec then
                            let! ok = pingPipeAsync lease.PipeName 1500

                            if not ok then
                                safeKillTree proc
                                safeDelete lease.LeasePath
                                actions.Add { Pid = lease.Pid; Kind = "daemon"; WorkspaceRoot = lease.WorkspaceRoot; Reason = KilledUnresponsive }

            // 3. Optional orphaned `ranvierautocomplete` cleanup when parent editor exited
            if includeOrphanedLsp then
                for proc in Process.GetProcessesByName "ranvierautocomplete" do
                    use proc = proc

                    if proc.Id <> selfPid then
                        checkedCount <- checkedCount + 1

                        match tryGetParentPid proc with
                        | Some ppid ->
                            let parentAlive =
                                try
                                    use parent = Process.GetProcessById ppid
                                    not parent.HasExited
                                with _ ->
                                    false

                            if not parentAlive || forceAll then
                                safeKillTree proc
                                actions.Add { Pid = proc.Id; Kind = "lsp"; WorkspaceRoot = "(external)"; Reason = KilledOrphanedLsp }
                        | None when forceAll ->
                            safeKillTree proc
                            actions.Add { Pid = proc.Id; Kind = "lsp"; WorkspaceRoot = "(external)"; Reason = KilledOrphanedLsp }
                        | None -> ()

            return
                { CheckedRecords = checkedCount
                  Actions = List.ofSeq actions }
        }
