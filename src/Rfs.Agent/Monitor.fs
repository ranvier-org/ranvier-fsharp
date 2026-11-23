namespace Rfs.Agent

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Runtime.InteropServices
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
type internal MonitorKind =
    | Daemon
    | Mcp
    | Lsp
    | Cli

    member this.Label =
        match this with
        | MonitorKind.Daemon -> "daemon"
        | MonitorKind.Mcp -> "mcp"
        | MonitorKind.Lsp -> "lsp"
        | MonitorKind.Cli -> "cli"

/// What a registry record's PID refers to now.
[<RequireQualifiedAccess>]
type internal Liveness =
    | Running
    | Exited
    | Reused

/// A finding the monitor acts on by itself: nothing can use the process or file again.
[<RequireQualifiedAccess>]
type internal Cleanup =
    | DeadRecord
    | ReusedPid
    /// A stdio server (`rfs mcp`, `ranvierautocomplete`) whose client exited.
    | ParentGone

/// A finding left to the user, because the process may still be about to serve someone.
[<RequireQualifiedAccess>]
type internal Concern =
    | Outdated
    | PastIdleTimeout
    | NoHeartbeat
    | ParentAdopted

[<RequireQualifiedAccess>]
type internal Verdict =
    | Healthy
    | Concern of Concern
    | Cleanup of Cleanup

[<NoComparison; NoEquality>]
type internal MonitorRow =
    { /// Stable across samples: PID and start time for a process, the file path for a dead record.
      Key: string
      Pid: int
      Kind: MonitorKind
      ProcessName: string
      Liveness: Liveness
      /// Backed by a record in `procs/` or a lease in `daemons/`.
      Registered: bool
      StartedUtc: DateTime option
      /// `processStartUtcTicks` of the record or lease, which a cleanup must still find before deleting it.
      RecordStartTicks: int64 option
      Workspace: string option
      Parent: ParentState
      CommandLine: string option
      CpuPercent: float
      WorkingSet: int64
      PrivateBytes: int64
      Threads: int
      IoRate: float option
      /// A daemon's own record of its last request; for other kinds, the last sample whose CPU or I/O moved.
      LastActiveUtc: DateTime option
      FirstSeenUtc: DateTime
      IdleTimeoutSeconds: int option
      HeartbeatUtc: DateTime option
      BinaryStamp: string option
      PipeName: string option
      LogPath: string option
      RecordPath: string option
      LeasePath: string option
      Verdict: Verdict }

[<NoComparison; NoEquality>]
type internal MonitorSnapshot =
    { TakenUtc: DateTime
      StateDir: string
      CurrentStamp: string
      Rows: MonitorRow array
      /// Temp files left by a record write that was interrupted.
      Leftovers: string array
      SampleMs: float }

    static member Empty =
        { TakenUtc = DateTime.UtcNow
          StateDir = ProcessRegistry.stateDir ()
          CurrentStamp = ""
          Rows = [||]
          Leftovers = [||]
          SampleMs = 0. }

[<NoComparison; NoEquality>]
type internal MonitorEvent =
    { TimeUtc: DateTime
      Text: string
      Failed: bool }

    static member Now(text: string, failed: bool) =
        { TimeUtc = DateTime.UtcNow
          Text = text
          Failed = failed }

/// Process names and hosted assemblies the monitor looks for. Tests substitute their own names.
[<NoComparison; NoEquality>]
type internal MonitorTargets =
    { RfsNames: string list
      LspNames: string list
      HostNames: string list
      RfsAssemblies: string list
      LspAssemblies: string list }

    static member Default =
        { RfsNames = [ "rfs" ]
          LspNames = [ "ranvierautocomplete" ]
          HostNames = [ "dotnet" ]
          RfsAssemblies = [ "rfs.dll" ]
          LspAssemblies = [ "ranvierautocomplete.dll" ] }

module internal ProcessProbe =

    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type private IoCounters =
        val mutable ReadOperationCount: uint64
        val mutable WriteOperationCount: uint64
        val mutable OtherOperationCount: uint64
        val mutable ReadTransferCount: uint64
        val mutable WriteTransferCount: uint64
        val mutable OtherTransferCount: uint64

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool private GetProcessIoCounters(IntPtr processHandle, IoCounters& counters)

    [<DllImport("ntdll.dll")>]
    extern int private NtQueryInformationProcess(
        IntPtr processHandle,
        int processInformationClass,
        IntPtr processInformation,
        int processInformationLength,
        int& returnLength
    )

    let private processCommandLineInformation = 60

    let startUtc (proc: Process) : DateTime option =
        try
            Some(proc.StartTime.ToUniversalTime())
        with _ ->
            None

    /// Bytes read and written, pipes included, so a stdio server's traffic shows here.
    let ioBytes (proc: Process) : int64 option =
        try
            if OperatingSystem.IsWindows() then
                let mutable counters = IoCounters()

                if GetProcessIoCounters(proc.Handle, &counters) then
                    Some(int64 (counters.ReadTransferCount + counters.WriteTransferCount + counters.OtherTransferCount))
                else
                    None
            elif OperatingSystem.IsLinux() then
                let mutable total = 0L
                let mutable found = false

                for line in File.ReadLines $"/proc/%d{proc.Id}/io" do
                    if line.StartsWith("rchar:", StringComparison.Ordinal) || line.StartsWith("wchar:", StringComparison.Ordinal) then
                        match Int64.TryParse(line.Substring(6).Trim()) with
                        | true, value ->
                            total <- total + value
                            found <- true
                        | _ -> ()

                if found then Some total else None
            else
                None
        with _ ->
            None

    let private windowsCommandLine (proc: Process) : string option =
        let mutable needed = 0
        NtQueryInformationProcess(proc.Handle, processCommandLineInformation, IntPtr.Zero, 0, &needed) |> ignore

        if needed <= 0 || needed > (1 <<< 20) then
            None
        else
            let buffer = Marshal.AllocHGlobal needed

            try
                let mutable written = 0

                if NtQueryInformationProcess(proc.Handle, processCommandLineInformation, buffer, needed, &written) <> 0 then
                    None
                else
                    // UNICODE_STRING: two 16-bit lengths, padding to pointer alignment, then the character pointer.
                    let byteLength = int (uint16 (Marshal.ReadInt16 buffer))
                    let chars = Marshal.ReadIntPtr(buffer, IntPtr.Size)

                    if byteLength = 0 || chars = IntPtr.Zero then
                        None
                    else
                        Some(Marshal.PtrToStringUni(chars, byteLength / 2))
            finally
                Marshal.FreeHGlobal buffer

    let commandLine (proc: Process) : string option =
        try
            if OperatingSystem.IsWindows() then
                windowsCommandLine proc
            elif OperatingSystem.IsLinux() then
                let raw = File.ReadAllText $"/proc/%d{proc.Id}/cmdline"
                let parts = raw.Split('\000', StringSplitOptions.RemoveEmptyEntries)
                if parts.Length = 0 then None else Some(String.Join(" ", parts))
            else
                None
        with _ ->
            None

    /// Splits a command line on whitespace, keeping double-quoted runs together.
    let tokens (commandLine: string) : string list =
        let result = ResizeArray<string>()
        let current = StringBuilder()
        let mutable quoted = false

        for c in commandLine do
            if c = '"' then
                quoted <- not quoted
            elif Char.IsWhiteSpace c && not quoted then
                if current.Length > 0 then
                    result.Add(current.ToString())
                    current.Clear() |> ignore
            else
                current.Append c |> ignore

        if current.Length > 0 then
            result.Add(current.ToString())

        List.ofSeq result

    /// The value of the first option among `names`, in `--name value` or `--name=value` form.
    let optionValue (names: string list) (args: string list) : string option =
        let rec scan =
            function
            | [] -> None
            | (arg: string) :: rest ->
                match names |> List.tryFind (fun n -> arg.StartsWith(n + "=", StringComparison.Ordinal)) with
                | Some n -> Some(arg.Substring(n.Length + 1))
                | None ->
                    match rest with
                    | value :: _ when List.contains arg names && not (value.StartsWith("-", StringComparison.Ordinal)) -> Some value
                    | _ -> scan rest

        scan args

    let tailLines (path: string) (count: int) : string array =
        try
            use stream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite ||| FileShare.Delete)
            let take = min stream.Length 16384L
            stream.Seek(-take, SeekOrigin.End) |> ignore
            let buffer = Array.zeroCreate<byte> (int take)
            stream.ReadExactly(buffer, 0, buffer.Length)

            let lines =
                Encoding.UTF8.GetString(buffer).Split('\n')
                |> Array.map (fun line -> line.TrimEnd('\r'))
                |> Array.filter (fun line -> line.Trim() <> "")

            lines[max 0 (lines.Length - count) ..]
        with _ ->
            [||]

module internal MonitorRules =

    let private isStdioServer (row: MonitorRow) =
        row.Kind = MonitorKind.Lsp || row.Kind = MonitorKind.Mcp

    let private isRegisteredService (row: MonitorRow) =
        row.Registered && (row.Kind = MonitorKind.Daemon || row.Kind = MonitorKind.Mcp)

    /// How long a record may go unwritten: four heartbeat intervals (`Daemon.fs` 15 s or 0.5 s, `McpServer.fs` 30 s).
    let heartbeatLimit (row: MonitorRow) : TimeSpan =
        match row.Kind with
        | MonitorKind.Daemon ->
            let interval =
                match row.IdleTimeoutSeconds with
                | Some ttl when ttl <= 5 -> 0.5
                | _ -> 15.

            TimeSpan.FromSeconds(max 10. (4. * interval))
        | _ -> TimeSpan.FromSeconds 120.

    /// A daemon past its idle timeout by more than a minute: its own watchdog should have stopped it.
    let idleGrace = TimeSpan.FromSeconds 60.

    /// Cleanups first, because they are certain; then concerns; anything else is healthy.
    let classify (now: DateTime) (currentStamp: string) (row: MonitorRow) : Verdict =
        match row.Liveness with
        | Liveness.Exited -> Verdict.Cleanup Cleanup.DeadRecord
        | Liveness.Reused -> Verdict.Cleanup Cleanup.ReusedPid
        | Liveness.Running ->
            match row.Parent with
            | ParentState.Exited _ when isStdioServer row -> Verdict.Cleanup Cleanup.ParentGone
            | _ ->
                let outdated =
                    isRegisteredService row
                    && (match row.BinaryStamp with
                        | Some stamp -> stamp <> currentStamp
                        | None -> false)

                let pastIdle =
                    row.Kind = MonitorKind.Daemon
                    && (match row.LastActiveUtc, row.IdleTimeoutSeconds with
                        | Some last, Some ttl -> now - last > TimeSpan.FromSeconds(float ttl) + idleGrace
                        | _ -> false)

                let limit = heartbeatLimit row

                let silent =
                    isRegisteredService row
                    && (match row.HeartbeatUtc, row.StartedUtc with
                        | Some beat, Some started -> now - beat > limit && now - started > limit
                        | _ -> false)

                if outdated then
                    Verdict.Concern Concern.Outdated
                elif pastIdle then
                    Verdict.Concern Concern.PastIdleTimeout
                elif silent then
                    Verdict.Concern Concern.NoHeartbeat
                else
                    match row.Parent with
                    | ParentState.Adopted _ when isStdioServer row -> Verdict.Concern Concern.ParentAdopted
                    | _ -> Verdict.Healthy

    let isFlagged (row: MonitorRow) =
        match row.Verdict with
        | Verdict.Concern _ -> true
        | _ -> false

    let isCleanup (row: MonitorRow) =
        match row.Verdict with
        | Verdict.Cleanup _ -> true
        | _ -> false

[<NoComparison; NoEquality>]
type private Previous =
    { CpuSeconds: float
      IoBytes: int64 option
      TakenUtc: DateTime
      LastChangeUtc: DateTime option
      FirstSeenUtc: DateTime }

/// Takes snapshots of every rfs and ranvierautocomplete process. Successive snapshots supply CPU, I/O rates and
/// observed idle time, so keep one sampler per monitor. Not thread-safe: take one snapshot at a time.
type internal MonitorSampler(targets: MonitorTargets) =
    let previous = Dictionary<string, Previous>()
    let commandLines = Dictionary<string, string option>()

    let processKey (pid: int) (started: DateTime option) =
        match started with
        | Some s -> $"p:%d{pid}:%d{s.Ticks}"
        | None -> $"p:%d{pid}"

    let commandLineOf (key: string) (proc: Process) =
        match commandLines.TryGetValue key with
        | true, cached -> cached
        | _ ->
            let value = ProcessProbe.commandLine proc
            commandLines[key] <- value
            value

    let hosts (assemblies: string list) (commandLine: string option) =
        match commandLine with
        | Some line ->
            ProcessProbe.tokens line
            |> List.exists (fun token ->
                assemblies
                |> List.exists (fun asm -> String.Equals(Path.GetFileName token, asm, StringComparison.OrdinalIgnoreCase)))
        | None -> false

    let expectedNames =
        targets.RfsNames @ targets.LspNames @ targets.HostNames
        |> List.map (fun name -> name.ToLowerInvariant())
        |> set
        |> Set.union ProcessRegistry.expectedProcessNames

    /// Running when the PID still has the recorded start time (within 2 s, as the reaper allows) and an expected name.
    let probe (pid: int) (startTicks: int64) : Liveness * Process option =
        try
            let proc = Process.GetProcessById pid

            let sameStart =
                match ProcessProbe.startUtc proc with
                | Some s -> abs (s.Ticks - startTicks) <= TimeSpan.FromSeconds(2.).Ticks
                | None -> false

            if not proc.HasExited && sameStart && expectedNames.Contains(proc.ProcessName.ToLowerInvariant()) then
                Liveness.Running, Some proc
            else
                let exited = proc.HasExited
                proc.Dispose()
                (if exited then Liveness.Exited else Liveness.Reused), None
        with _ ->
            Liveness.Exited, None

    let leftovers (now: DateTime) =
        [| for dir in [ ProcessRegistry.procsDir (); ProcessRegistry.daemonsDir () ] do
               if Directory.Exists dir then
                   for path in Directory.EnumerateFiles(dir, "*.tmp.*") do
                       let name = Path.GetFileName path
                       let marker = name.IndexOf(".tmp.", StringComparison.Ordinal)
                       let writer =
                           if marker < 0 then None
                           else
                               match name.Substring(marker + 5).Split('.') with
                               | parts when parts.Length > 0 ->
                                   match Int32.TryParse parts[0] with
                                   | true, pid -> Some pid
                                   | _ -> None
                               | _ -> None

                       let writerRunning =
                           match writer with
                           | Some pid ->
                               try
                                   use p = Process.GetProcessById pid
                                   not p.HasExited
                               with _ ->
                                   false
                           | None -> true

                       let old =
                           try
                               now - File.GetLastWriteTimeUtc path > TimeSpan.FromSeconds 60.
                           with _ ->
                               false

                       if old && not writerRunning then
                           yield path |]

    new() = MonitorSampler(MonitorTargets.Default)

    member _.Targets = targets

    member _.Take() : MonitorSnapshot =
        let watch = Stopwatch.StartNew()
        let now = DateTime.UtcNow
        let selfPid = Environment.ProcessId
        let stamp = ProcessRegistry.currentBinaryStamp ()
        let records = try ProcessRegistry.listProcessRecords () with _ -> []
        let leases = try ProcessRegistry.listDaemonLeases () with _ -> []
        let owned = ResizeArray<Process>()
        let seen = HashSet<string>()

        try
            // Live processes by PID, each with the kind its name or hosted assembly implies.
            let live = Dictionary<int, Process * MonitorKind>()

            let consider (kind: Process -> MonitorKind option) (proc: Process) =
                owned.Add proc

                if proc.Id <> selfPid && not (live.ContainsKey proc.Id) then
                    match kind proc with
                    | Some k -> live[proc.Id] <- (proc, k)
                    | None -> ()

            let rfsKind (proc: Process) =
                let key = processKey proc.Id (ProcessProbe.startUtc proc)
                let args = commandLineOf key proc |> Option.map ProcessProbe.tokens |> Option.defaultValue []

                // The first command after the executable, a hosted rfs.dll and `--workspace <dir>`.
                let rec command (rest: string list) =
                    match rest with
                    | ("--workspace" | "-w") :: _ :: tail -> command tail
                    | arg :: tail when arg.StartsWith("--workspace=", StringComparison.Ordinal) -> command tail
                    | arg :: tail when arg.EndsWith(".dll", StringComparison.OrdinalIgnoreCase) -> command tail
                    | "daemon" :: "run" :: _ -> MonitorKind.Daemon
                    | "mcp" :: _ -> MonitorKind.Mcp
                    | _ -> MonitorKind.Cli

                Some(command (List.skip (min 1 args.Length) args))

            for name in targets.RfsNames do
                for proc in Process.GetProcessesByName name do
                    consider rfsKind proc

            for name in targets.LspNames do
                for proc in Process.GetProcessesByName name do
                    consider (fun _ -> Some MonitorKind.Lsp) proc

            for name in targets.HostNames do
                for proc in Process.GetProcessesByName name do
                    consider
                        (fun p ->
                            let line = commandLineOf (processKey p.Id (ProcessProbe.startUtc p)) p

                            if hosts targets.LspAssemblies line then Some MonitorKind.Lsp
                            elif hosts targets.RfsAssemblies line then rfsKind p
                            else None)
                        proc

            let parentName (pid: int) =
                try
                    use p = Process.GetProcessById pid
                    p.ProcessName
                with _ ->
                    "?"

            let metrics (key: string) (proc: Process) =
                let cpuSeconds = try proc.TotalProcessorTime.TotalSeconds with _ -> 0.
                let io = ProcessProbe.ioBytes proc

                let cpu, ioRate, lastChange, firstSeen =
                    match previous.TryGetValue key with
                    | true, prev ->
                        let elapsed = max 0.001 (now - prev.TakenUtc).TotalSeconds
                        let cpuDelta = max 0. (cpuSeconds - prev.CpuSeconds)

                        let ioDelta =
                            match io, prev.IoBytes with
                            | Some a, Some b -> Some(max 0L (a - b))
                            | _ -> None

                        let changed = cpuDelta > 0.005 || (ioDelta |> Option.exists (fun d -> d > 0L))
                        let percent = Math.Clamp(cpuDelta / elapsed / float Environment.ProcessorCount * 100., 0., 100.)
                        percent, ioDelta |> Option.map (fun d -> float d / elapsed), (if changed then Some now else prev.LastChangeUtc), prev.FirstSeenUtc
                    | _ -> 0., None, None, now

                previous[key] <-
                    { CpuSeconds = cpuSeconds
                      IoBytes = io
                      TakenUtc = now
                      LastChangeUtc = lastChange
                      FirstSeenUtc = firstSeen }

                seen.Add key |> ignore
                cpu, ioRate, lastChange, firstSeen

            let logOption (line: string option) =
                line
                |> Option.map ProcessProbe.tokens
                |> Option.bind (ProcessProbe.optionValue [ "--logfile"; "--log-file"; "-l" ])

            let rows = ResizeArray<MonitorRow>()
            let claimed = HashSet<int>()

            let runningRow (proc: Process) (kind: MonitorKind) (record: ProcessRecord option) (lease: DaemonLease option) =
                let started = ProcessProbe.startUtc proc
                let key = processKey proc.Id started
                let cpu, ioRate, lastChange, firstSeen = metrics key proc
                let line = commandLineOf key proc
                let workspace =
                    match record, lease with
                    | Some r, _ -> Some r.WorkspaceRoot
                    | None, Some l -> Some l.WorkspaceRoot
                    | None, None -> line |> Option.map ProcessProbe.tokens |> Option.bind (ProcessProbe.optionValue [ "--workspace"; "-w" ])

                let parent =
                    match kind, record with
                    | MonitorKind.Daemon, _ -> ParentState.Detached
                    | _, Some r when r.ParentPid.IsSome && r.ParentStartUtcTicks.IsSome ->
                        if ProcessRegistry.isParentAlive r.ParentPid r.ParentStartUtcTicks then
                            ParentState.Running(r.ParentPid.Value, parentName r.ParentPid.Value)
                        else
                            ParentState.Exited r.ParentPid.Value
                    | _ -> ProcessRegistry.parentState proc

                let workspaceId =
                    match record, lease with
                    | Some r, _ -> Some r.WorkspaceId
                    | None, Some l -> Some l.WorkspaceId
                    | _ -> None

                let logPath =
                    match kind, workspaceId with
                    | MonitorKind.Daemon, Some id -> Some(ProcessRegistry.daemonLogPath id)
                    | _ -> logOption line

                { Key = key
                  Pid = proc.Id
                  Kind = kind
                  ProcessName = proc.ProcessName
                  Liveness = Liveness.Running
                  Registered = record.IsSome || lease.IsSome
                  StartedUtc = started
                  RecordStartTicks =
                    match record, lease with
                    | Some r, _ -> Some r.ProcessStartUtcTicks
                    | None, Some l -> Some l.ProcessStartUtcTicks
                    | _ -> None
                  Workspace = workspace
                  Parent = parent
                  CommandLine = line
                  CpuPercent = cpu
                  WorkingSet = (try proc.WorkingSet64 with _ -> 0L)
                  PrivateBytes = (try proc.PrivateMemorySize64 with _ -> 0L)
                  Threads = (try proc.Threads.Count with _ -> 0)
                  IoRate = ioRate
                  LastActiveUtc =
                    match kind, record with
                    | MonitorKind.Daemon, Some r -> Some(DateTime(r.LastActiveUtcTicks, DateTimeKind.Utc))
                    | _ -> lastChange
                  FirstSeenUtc = firstSeen
                  IdleTimeoutSeconds = record |> Option.map _.IdleTimeoutSeconds
                  HeartbeatUtc =
                    record |> Option.bind (fun r -> try Some(File.GetLastWriteTimeUtc r.RecordPath) with _ -> None)
                  BinaryStamp =
                    match record, lease with
                    | Some r, _ -> Some r.BinaryStamp
                    | None, Some l -> Some l.BinaryStamp
                    | _ -> None
                  PipeName =
                    match record, lease with
                    | _, Some l -> Some l.PipeName
                    | Some r, None when kind = MonitorKind.Daemon -> Some r.PipeName
                    | _ -> None
                  LogPath = logPath |> Option.filter File.Exists
                  RecordPath = record |> Option.map _.RecordPath
                  LeasePath = lease |> Option.map _.LeasePath
                  Verdict = Verdict.Healthy }

            let deadRow (pid: int) (kind: MonitorKind) (liveness: Liveness) (record: ProcessRecord option) (lease: DaemonLease option) =
                let path =
                    match record, lease with
                    | Some r, _ -> r.RecordPath
                    | None, Some l -> l.LeasePath
                    | None, None -> string pid

                let key = "f:" + path
                seen.Add key |> ignore

                let firstSeen =
                    match previous.TryGetValue key with
                    | true, prev -> prev.FirstSeenUtc
                    | _ -> now

                previous[key] <-
                    { CpuSeconds = 0.
                      IoBytes = None
                      TakenUtc = now
                      LastChangeUtc = None
                      FirstSeenUtc = firstSeen }

                { Key = key
                  Pid = pid
                  Kind = kind
                  ProcessName = ""
                  Liveness = liveness
                  Registered = true
                  StartedUtc = None
                  RecordStartTicks =
                    match record, lease with
                    | Some r, _ -> Some r.ProcessStartUtcTicks
                    | None, Some l -> Some l.ProcessStartUtcTicks
                    | _ -> None
                  Workspace =
                    match record, lease with
                    | Some r, _ -> Some r.WorkspaceRoot
                    | None, Some l -> Some l.WorkspaceRoot
                    | _ -> None
                  Parent = ParentState.Unknown
                  CommandLine = None
                  CpuPercent = 0.
                  WorkingSet = 0L
                  PrivateBytes = 0L
                  Threads = 0
                  IoRate = None
                  LastActiveUtc = record |> Option.map (fun r -> DateTime(r.LastActiveUtcTicks, DateTimeKind.Utc))
                  FirstSeenUtc = firstSeen
                  IdleTimeoutSeconds = record |> Option.map _.IdleTimeoutSeconds
                  HeartbeatUtc = None
                  BinaryStamp =
                    match record, lease with
                    | Some r, _ -> Some r.BinaryStamp
                    | None, Some l -> Some l.BinaryStamp
                    | _ -> None
                  PipeName = lease |> Option.map _.PipeName
                  LogPath = None
                  RecordPath = record |> Option.map _.RecordPath
                  LeasePath = lease |> Option.map _.LeasePath
                  Verdict = Verdict.Healthy }

            let kindOf (record: ProcessRecord) =
                match record.Kind with
                | "daemon" -> MonitorKind.Daemon
                | "mcp" -> MonitorKind.Mcp
                | _ -> MonitorKind.Cli

            for record in records do
                if record.Pid <> selfPid then
                    let lease =
                        leases
                        |> List.tryFind (fun l -> l.Pid = record.Pid && l.ProcessStartUtcTicks = record.ProcessStartUtcTicks)

                    match probe record.Pid record.ProcessStartUtcTicks with
                    | Liveness.Running, Some proc ->
                        owned.Add proc
                        claimed.Add record.Pid |> ignore
                        rows.Add(runningRow proc (kindOf record) (Some record) lease)
                    | liveness, _ -> rows.Add(deadRow record.Pid (kindOf record) liveness (Some record) lease)

            for lease in leases do
                let hasRecord =
                    records
                    |> List.exists (fun r -> r.Pid = lease.Pid && r.ProcessStartUtcTicks = lease.ProcessStartUtcTicks)

                if lease.Pid <> selfPid && not hasRecord then
                    match probe lease.Pid lease.ProcessStartUtcTicks with
                    | Liveness.Running, Some proc ->
                        owned.Add proc
                        claimed.Add lease.Pid |> ignore
                        rows.Add(runningRow proc MonitorKind.Daemon None (Some lease))
                    | liveness, _ -> rows.Add(deadRow lease.Pid MonitorKind.Daemon liveness None (Some lease))

            for KeyValue(pid, (proc, kind)) in live do
                if not (claimed.Contains pid) then
                    try
                        if not proc.HasExited then
                            rows.Add(runningRow proc kind None None)
                    with _ ->
                        ()

            for key in List.ofSeq previous.Keys do
                if not (seen.Contains key) then
                    previous.Remove key |> ignore
                    commandLines.Remove key |> ignore

            let classified =
                rows
                |> Seq.map (fun row -> { row with Verdict = MonitorRules.classify now stamp row })
                |> Array.ofSeq

            { TakenUtc = now
              StateDir = ProcessRegistry.stateDir ()
              CurrentStamp = stamp
              Rows = classified
              Leftovers = (try leftovers now with _ -> [||])
              SampleMs = watch.Elapsed.TotalMilliseconds }
        finally
            for proc in owned do
                proc.Dispose()

module internal MonitorFormat =

    let bytes (value: int64) =
        let v = float value

        if v >= 1073741824. then sprintf "%.1fG" (v / 1073741824.)
        elif v >= 1048576. then sprintf "%.0fM" (v / 1048576.)
        elif v >= 1024. then sprintf "%.0fK" (v / 1024.)
        else sprintf "%dB" value

    let rate (value: float option) =
        match value with
        | Some v when v >= 1. -> bytes (int64 v) + "/s"
        | Some _ -> "0"
        | None -> "—"

    let duration (span: TimeSpan) =
        let span = if span < TimeSpan.Zero then TimeSpan.Zero else span

        if span.TotalSeconds < 60. then sprintf "%ds" (int span.TotalSeconds)
        elif span.TotalMinutes < 60. then sprintf "%dm" (int span.TotalMinutes)
        elif span.TotalHours < 48. then sprintf "%dh%02dm" (int span.TotalHours) span.Minutes
        else sprintf "%dd" (int span.TotalDays)

    let parent (state: ParentState) =
        match state with
        | ParentState.Running(pid, name) -> $"%s{name} %d{pid}"
        | ParentState.Exited pid -> $"%d{pid} exited"
        | ParentState.Adopted pid -> $"init %d{pid}"
        | ParentState.Detached -> "detached"
        | ParentState.Unknown -> "unknown"

    /// `parent` with the process name cut to 12 cells, so the PID survives in a table column.
    let parentShort (state: ParentState) =
        match state with
        | ParentState.Running(pid, name) when name.Length > 12 -> $"%s{name.Substring(0, 11)}… %d{pid}"
        | other -> parent other

    let cleanup (c: Cleanup) =
        match c with
        | Cleanup.DeadRecord -> "process exited"
        | Cleanup.ReusedPid -> "PID reused"
        | Cleanup.ParentGone -> "parent gone"

    let concern (c: Concern) =
        match c with
        | Concern.Outdated -> "older rfs build"
        | Concern.PastIdleTimeout -> "past idle timeout"
        | Concern.NoHeartbeat -> "no heartbeat"
        | Concern.ParentAdopted -> "parent gone?"

    /// The time since the process last did anything, or `≥` the time it has been watched without moving.
    let idle (now: DateTime) (row: MonitorRow) =
        match row.Liveness, row.LastActiveUtc with
        | Liveness.Running, Some last -> duration (now - last)
        | Liveness.Running, None when now - row.FirstSeenUtc >= TimeSpan.FromSeconds 5. -> "≥" + duration (now - row.FirstSeenUtc)
        | _ -> "—"

    let uptime (now: DateTime) (row: MonitorRow) =
        match row.StartedUtc with
        | Some started -> duration (now - started)
        | None -> "—"

    let state (now: DateTime) (row: MonitorRow) =
        match row.Verdict with
        | Verdict.Cleanup c -> "clean: " + cleanup c
        | Verdict.Concern c -> "check: " + concern c
        | Verdict.Healthy ->
            if row.CpuPercent >= 1. then "busy"
            else
                match row.LastActiveUtc with
                | Some last when now - last >= TimeSpan.FromMinutes 5. -> "idle"
                | _ -> "ok"

    /// The last two segments of a workspace path, which is what tells workspaces apart in a narrow column.
    let workspace (row: MonitorRow) =
        match row.Workspace with
        | Some path ->
            let parts = path.Split([| '\\'; '/' |], StringSplitOptions.RemoveEmptyEntries)

            if parts.Length <= 2 then path
            else String.Join(string Path.DirectorySeparatorChar, parts[parts.Length - 2 ..])
        | None -> "—"

    let memory (row: MonitorRow) =
        if row.Liveness = Liveness.Running then bytes row.WorkingSet else "—"

    let cpu (row: MonitorRow) =
        if row.Liveness = Liveness.Running then sprintf "%.1f" row.CpuPercent else "—"

    let explain (row: MonitorRow) =
        match row.Verdict with
        | Verdict.Cleanup Cleanup.DeadRecord -> "The recorded process exited. Auto-clean deletes its record."
        | Verdict.Cleanup Cleanup.ReusedPid -> "Another process now holds this PID. Auto-clean deletes the record and leaves that process alone."
        | Verdict.Cleanup Cleanup.ParentGone -> "Its client exited, so nothing can talk to this stdio server again. Auto-clean stops it."
        | Verdict.Concern Concern.Outdated -> "Started from a different rfs build than this monitor. The next rfs command replaces it; k stops it now."
        | Verdict.Concern Concern.PastIdleTimeout -> "Idle past its own timeout, so its watchdog may be stuck. k stops it."
        | Verdict.Concern Concern.NoHeartbeat -> "Its record has not been refreshed recently: it may be hung. k stops it."
        | Verdict.Concern Concern.ParentAdopted -> "Re-parented to init, so its client probably exited. k stops it."
        | Verdict.Healthy -> "Nothing to do."

    let summary (snapshot: MonitorSnapshot) =
        let running = snapshot.Rows |> Array.filter (fun r -> r.Liveness = Liveness.Running)
        let memoryTotal = running |> Array.sumBy _.WorkingSet
        let cpuTotal = running |> Array.sumBy _.CpuPercent
        let flagged = snapshot.Rows |> Array.filter MonitorRules.isFlagged |> Array.length
        let cleanable = (snapshot.Rows |> Array.filter MonitorRules.isCleanup |> Array.length) + snapshot.Leftovers.Length
        running.Length, memoryTotal, cpuTotal, flagged, cleanable

    let private pad (width: int) (text: string) =
        if text.Length >= width then text.Substring(0, max 0 (width - 1)) + " " else text.PadRight width

    /// Redirected Windows output is written in the OEM code page, so plain-text output keeps to ASCII.
    let ascii (text: string) =
        text.Replace("—", "-").Replace("≥", ">=").Replace("…", "~").Replace("·", "|")

    let table (snapshot: MonitorSnapshot) : string =
        let now = snapshot.TakenUtc
        let sb = StringBuilder()
        let running, memoryTotal, cpuTotal, flagged, cleanable = summary snapshot

        sb.AppendLine(
            $"rfs monitor | %d{running} running | %s{bytes memoryTotal} | CPU %.1f{cpuTotal}%% | %d{flagged} flagged | %d{cleanable} to clean | state %s{snapshot.StateDir}"
        )
        |> ignore

        let header = [ "KIND", 7; "PID", 8; "PARENT", 22; "CPU%", 6; "MEM", 7; "IDLE", 8; "UP", 7; "STATE", 26; "WORKSPACE", 0 ]

        let line (cells: string list) =
            List.zip header cells
            |> List.map (fun ((_, width), text) -> if width = 0 then text else pad width text)
            |> String.concat ""

        sb.AppendLine(line (header |> List.map fst)) |> ignore

        for row in snapshot.Rows |> Array.sortByDescending _.WorkingSet do
            sb.AppendLine(
                line
                    [ row.Kind.Label
                      string row.Pid
                      parentShort row.Parent
                      cpu row
                      memory row
                      idle now row
                      uptime now row
                      state now row
                      (row.Workspace |> Option.defaultValue "—") ]
            )
            |> ignore

        for path in snapshot.Leftovers do
            sb.AppendLine($"leftover file: %s{path}") |> ignore

        if snapshot.Rows.Length = 0 then
            sb.AppendLine "No rfs or ranvierautocomplete processes." |> ignore

        ascii (sb.ToString().TrimEnd())

    let json (snapshot: MonitorSnapshot) (events: MonitorEvent list) : JsonObject =
        let o = JsonObject()
        let now = snapshot.TakenUtc
        o["takenUtc"] <- JsonValue.Create(snapshot.TakenUtc.ToString "O")
        o["stateDir"] <- JsonValue.Create snapshot.StateDir
        o["sampleMs"] <- JsonValue.Create(Math.Round(snapshot.SampleMs, 1))
        let rows = JsonArray()

        for row in snapshot.Rows do
            let r = JsonObject()
            r["kind"] <- JsonValue.Create row.Kind.Label
            r["pid"] <- JsonValue.Create row.Pid
            r["running"] <- JsonValue.Create(row.Liveness = Liveness.Running)
            r["registered"] <- JsonValue.Create row.Registered
            r["parent"] <- JsonValue.Create(parent row.Parent)
            r["workspace"] <- JsonValue.Create(row.Workspace |> Option.toObj)
            r["cpuPercent"] <- JsonValue.Create(Math.Round(row.CpuPercent, 2))
            r["workingSetBytes"] <- JsonValue.Create row.WorkingSet
            r["privateBytes"] <- JsonValue.Create row.PrivateBytes
            r["idle"] <- JsonValue.Create(idle now row)
            r["uptime"] <- JsonValue.Create(uptime now row)
            r["state"] <- JsonValue.Create(state now row)
            r["commandLine"] <- JsonValue.Create(row.CommandLine |> Option.toObj)
            rows.Add r

        o["processes"] <- rows
        let leftovers = JsonArray()

        for path in snapshot.Leftovers do
            leftovers.Add(JsonValue.Create path)

        o["leftoverFiles"] <- leftovers
        let actions = JsonArray()

        for e in events do
            let a = JsonObject()
            a["text"] <- JsonValue.Create e.Text
            a["failed"] <- JsonValue.Create e.Failed
            actions.Add a

        o["actions"] <- actions
        o

module internal MonitorActions =

    /// The process the row describes, if that PID still has the same name and start time.
    let private verified (row: MonitorRow) : Process option =
        match row.StartedUtc with
        | None -> None
        | Some started ->
            try
                let proc = Process.GetProcessById row.Pid

                let same =
                    try
                        not proc.HasExited
                        && String.Equals(proc.ProcessName, row.ProcessName, StringComparison.OrdinalIgnoreCase)
                        && abs (proc.StartTime.ToUniversalTime() - started).TotalSeconds < 1.
                    with _ ->
                        false

                if same then
                    Some proc
                else
                    proc.Dispose()
                    None
            with _ ->
                None

    let private stillDead (row: MonitorRow) =
        match row.RecordStartTicks with
        | None -> false
        | Some ticks ->
            try
                use proc = Process.GetProcessById row.Pid

                proc.HasExited
                || (match ProcessProbe.startUtc proc with
                    | Some s -> abs (s.Ticks - ticks) > TimeSpan.FromSeconds(2.).Ticks
                    | None -> false)
            with _ ->
                true

    /// Deletes the row's record and lease if they still name this PID and start time; returns how many it deleted.
    let removeFiles (row: MonitorRow) : int =
        let mutable removed = 0

        match row.RecordPath, row.RecordStartTicks with
        | Some path, Some ticks ->
            match ProcessRegistry.tryReadProcessRecord path with
            | Some r when r.Pid = row.Pid && r.ProcessStartUtcTicks = ticks ->
                try
                    File.Delete path
                    removed <- removed + 1
                with _ ->
                    ()
            | _ -> ()
        | _ -> ()

        match row.LeasePath, row.RecordStartTicks with
        | Some path, Some ticks ->
            match ProcessRegistry.tryReadDaemonLease path with
            | Some l when l.Pid = row.Pid && l.ProcessStartUtcTicks = ticks ->
                try
                    File.Delete path
                    removed <- removed + 1
                with _ ->
                    ()
            | _ -> ()
        | _ -> ()

        removed

    let private killTree (proc: Process) : bool =
        try
            proc.Kill(entireProcessTree = true)
            proc.WaitForExit 3000 |> ignore
            proc.HasExited
        with _ ->
            (try proc.HasExited with _ -> false)

    let private describe (row: MonitorRow) =
        match row.Workspace with
        | Some _ -> $"%s{row.Kind.Label} %d{row.Pid} (%s{MonitorFormat.workspace row})"
        | None -> $"%s{row.Kind.Label} %d{row.Pid}"

    let private removeLeftover (path: string) : MonitorEvent option =
        try
            if File.Exists path then
                File.Delete path
                Some(MonitorEvent.Now($"removed leftover file %s{Path.GetFileName path}", false))
            else
                None
        with ex ->
            Some(MonitorEvent.Now($"could not remove %s{Path.GetFileName path}: %s{ex.Message}", true))

    let private parentStillGone (row: MonitorRow) (proc: Process) =
        let record = row.RecordPath |> Option.bind ProcessRegistry.tryReadProcessRecord

        match record with
        | Some r when r.ParentPid.IsSome && r.ParentStartUtcTicks.IsSome ->
            not (ProcessRegistry.isParentAlive r.ParentPid r.ParentStartUtcTicks)
        | _ ->
            match ProcessRegistry.parentState proc with
            | ParentState.Exited _ -> true
            | _ -> false

    /// Acts on every certain finding in the snapshot: deletes dead records and leftover files, and stops stdio
    /// servers whose client exited. Each one is checked again just before acting, and nothing else is touched.
    let autoClean (snapshot: MonitorSnapshot) : MonitorEvent list =
        [ for row in snapshot.Rows do
              match row.Verdict with
              | Verdict.Cleanup Cleanup.DeadRecord
              | Verdict.Cleanup Cleanup.ReusedPid when stillDead row ->
                  if removeFiles row > 0 then
                      let reason = if row.Verdict = Verdict.Cleanup Cleanup.DeadRecord then "process exited" else "PID reused"
                      yield MonitorEvent.Now($"removed the record of %s{describe row}: %s{reason}", false)
              | Verdict.Cleanup Cleanup.ParentGone ->
                  match verified row with
                  | Some proc ->
                      use proc = proc

                      if parentStillGone row proc then
                          let parentText = MonitorFormat.parent row.Parent

                          if killTree proc then
                              removeFiles row |> ignore
                              yield MonitorEvent.Now($"stopped %s{describe row}: parent %s{parentText}", false)
                          else
                              yield MonitorEvent.Now($"could not stop %s{describe row} (parent %s{parentText})", true)
                  | None -> ()
              | _ -> ()

          for path in snapshot.Leftovers do
              match removeLeftover path with
              | Some e -> yield e
              | None -> () ]

    let private shutdownAsync (pipeName: string) (timeoutMs: int) : Task<bool> =
        task {
            try
                use cts = new CancellationTokenSource(timeoutMs)
                use client = new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous)
                do! client.ConnectAsync cts.Token
                let req = JsonObject()
                req["op"] <- JsonValue.Create "shutdown"
                do! PipeFraming.writeAsync client req cts.Token
                let! resp = PipeFraming.readAsync client cts.Token
                return resp |> Option.exists (fun r -> JsonUtil.boolOpt r "ok" = Some true)
            with _ ->
                return false
        }

    /// Stops a process: a daemon through its `shutdown` request, then by force after five seconds; anything else by
    /// force, since a stdio server has no other channel. For a dead record it deletes the files.
    let stopAsync (row: MonitorRow) : Task<MonitorEvent> =
        task {
            match row.Liveness with
            | Liveness.Exited
            | Liveness.Reused ->
                let removed = removeFiles row
                return MonitorEvent.Now($"removed %d{removed} file(s) for %s{describe row}", false)
            | Liveness.Running ->
                match verified row with
                | None -> return MonitorEvent.Now($"%s{describe row} is no longer running", false)
                | Some proc ->
                    use proc = proc

                    let! graceful =
                        task {
                            match row.Kind, row.PipeName with
                            | MonitorKind.Daemon, Some pipe ->
                                let! acked = shutdownAsync pipe 2000

                                if acked then
                                    use wait = new CancellationTokenSource(5000)

                                    try
                                        do! proc.WaitForExitAsync wait.Token
                                        return true
                                    with :? OperationCanceledException ->
                                        return false
                                else
                                    return false
                            | _ -> return false
                        }

                    let stopped = graceful || killTree proc
                    removeFiles row |> ignore
                    let how = if graceful then "shut down" else "killed"

                    if stopped then
                        return MonitorEvent.Now($"%s{how} %s{describe row}", false)
                    else
                        return MonitorEvent.Now($"could not stop %s{describe row}", true)
        }

    /// Reads a daemon's counters with the `monitor` request, which the daemon does not count as use. Daemons from
    /// other rfs builds may not know it and would count it, so ask only daemons with this monitor's binary stamp.
    let daemonInfoAsync (pipeName: string) (timeoutMs: int) : Task<Result<JsonObject * float, string>> =
        task {
            try
                use cts = new CancellationTokenSource(timeoutMs)
                use client = new NamedPipeClientStream(".", pipeName, PipeDirection.InOut, PipeOptions.Asynchronous)
                let watch = Stopwatch.StartNew()
                do! client.ConnectAsync cts.Token
                let req = JsonObject()
                req["op"] <- JsonValue.Create "monitor"
                do! PipeFraming.writeAsync client req cts.Token
                let! resp = PipeFraming.readAsync client cts.Token
                watch.Stop()

                match resp with
                | Some r when JsonUtil.boolOpt r "ok" = Some true ->
                    match r["data"] with
                    | :? JsonObject as data -> return Ok(data, watch.Elapsed.TotalMilliseconds)
                    | _ -> return Error "the daemon answered without counters"
                | Some r -> return Error(JsonUtil.strRaw r "error" |> Option.defaultValue "the daemon refused the request")
                | None -> return Error "the daemon closed the pipe"
            with
            | :? OperationCanceledException -> return Error $"no answer within %d{timeoutMs} ms"
            | ex -> return Error ex.Message
        }
