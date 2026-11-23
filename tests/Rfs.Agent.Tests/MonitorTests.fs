module Rfs.Agent.Tests.MonitorTests

open System
open System.Diagnostics
open System.IO
open System.IO.Pipes
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Ranvier
open Ranvier.Terminal
open Ranvier.Terminal.View
open Rfs.Agent

// ---------------------------------------------------------------------------------------------------------------
// Verdict rules (pure)
// ---------------------------------------------------------------------------------------------------------------

let private now = DateTime(2026, 11, 24, 12, 0, 0, DateTimeKind.Utc)
let private stamp = "dll:1:1"

let private row (kind: MonitorKind) : MonitorRow =
    { Key = "k"
      Pid = 4242
      Kind = kind
      ProcessName = "x"
      Liveness = Liveness.Running
      Registered = false
      StartedUtc = Some(now.AddHours -1.)
      RecordStartTicks = None
      Workspace = None
      Parent = ParentState.Unknown
      CommandLine = None
      CpuPercent = 0.
      WorkingSet = 0L
      PrivateBytes = 0L
      Threads = 0
      IoRate = None
      LastActiveUtc = None
      FirstSeenUtc = now
      IdleTimeoutSeconds = None
      HeartbeatUtc = None
      BinaryStamp = None
      PipeName = None
      LogPath = None
      RecordPath = None
      LeasePath = None
      Verdict = Verdict.Healthy }

let private registeredDaemon () =
    { row MonitorKind.Daemon with
        Registered = true
        Parent = ParentState.Detached
        BinaryStamp = Some stamp
        IdleTimeoutSeconds = Some 1800
        LastActiveUtc = Some(now.AddMinutes -1.)
        HeartbeatUtc = Some(now.AddSeconds -5.) }

let private classify r = MonitorRules.classify now stamp r

let private expectVerdict expected r message =
    Expect.equal (classify r) expected message

let private ruleTests =
    testList "rules" [
        test "a record whose process exited, or whose PID was reused, is cleaned" {
            expectVerdict (Verdict.Cleanup Cleanup.DeadRecord) { registeredDaemon () with Liveness = Liveness.Exited } "exited"
            expectVerdict (Verdict.Cleanup Cleanup.ReusedPid) { registeredDaemon () with Liveness = Liveness.Reused } "reused"
        }

        test "a stdio server whose parent exited is cleaned; a daemon or CLI with an exited parent is not" {
            expectVerdict (Verdict.Cleanup Cleanup.ParentGone) { row MonitorKind.Lsp with Parent = ParentState.Exited 7 } "lsp"
            expectVerdict (Verdict.Cleanup Cleanup.ParentGone) { row MonitorKind.Mcp with Parent = ParentState.Exited 7 } "mcp"
            expectVerdict Verdict.Healthy { registeredDaemon () with Parent = ParentState.Exited 7 } "daemon outlives its parent"
            expectVerdict Verdict.Healthy { row MonitorKind.Cli with Parent = ParentState.Exited 7 } "cli is never acted on"
        }

        test "a server re-parented to init, or with an unknown parent, is never cleaned" {
            expectVerdict (Verdict.Concern Concern.ParentAdopted) { row MonitorKind.Lsp with Parent = ParentState.Adopted 1 } "adopted"
            expectVerdict Verdict.Healthy { row MonitorKind.Lsp with Parent = ParentState.Unknown } "unknown"
            expectVerdict Verdict.Healthy { row MonitorKind.Lsp with Parent = ParentState.Running(9, "code") } "running"
        }

        test "a registered daemon from another rfs build is flagged, not cleaned" {
            expectVerdict (Verdict.Concern Concern.Outdated) { registeredDaemon () with BinaryStamp = Some "dll:2:2" } "outdated"
            expectVerdict Verdict.Healthy { row MonitorKind.Daemon with BinaryStamp = None } "unregistered daemon has no stamp"
        }

        test "a daemon is flagged once it is a minute past its own idle timeout" {
            expectVerdict
                (Verdict.Concern Concern.PastIdleTimeout)
                { registeredDaemon () with LastActiveUtc = Some(now.AddSeconds -1861.) }
                "past timeout and grace"

            expectVerdict Verdict.Healthy { registeredDaemon () with LastActiveUtc = Some(now.AddSeconds -1830.) } "within grace"
        }

        test "a registered daemon whose record stopped refreshing is flagged" {
            expectVerdict (Verdict.Concern Concern.NoHeartbeat) { registeredDaemon () with HeartbeatUtc = Some(now.AddSeconds -90.) } "silent"
            expectVerdict Verdict.Healthy { registeredDaemon () with HeartbeatUtc = Some(now.AddSeconds -30.) } "recent beat"

            expectVerdict
                Verdict.Healthy
                { registeredDaemon () with
                    StartedUtc = Some(now.AddSeconds -40.)
                    HeartbeatUtc = Some(now.AddSeconds -40.) }
                "too young to judge"
        }

        test "a certain cleanup wins over a concern" {
            expectVerdict
                (Verdict.Cleanup Cleanup.DeadRecord)
                { registeredDaemon () with Liveness = Liveness.Exited; BinaryStamp = Some "other" }
                "dead and outdated"
        }

        test "command line options are read in both forms" {
            let args = ProcessProbe.tokens "\"C:\\Program Files\\x\\ranvierautocomplete.exe\" --logfile=C:\\log one.txt -w ws"
            Expect.equal (ProcessProbe.optionValue [ "--logfile"; "-l" ] args) (Some "C:\\log") "equals form stops at whitespace"
            Expect.equal (ProcessProbe.optionValue [ "--workspace"; "-w" ] args) (Some "ws") "separate form"
            Expect.equal (List.head args) "C:\\Program Files\\x\\ranvierautocomplete.exe" "quoted executable kept whole"
        }
    ]

// ---------------------------------------------------------------------------------------------------------------
// Sampling and auto-clean against real processes in an isolated state directory
// ---------------------------------------------------------------------------------------------------------------

/// A copy of a long-running system program under a unique name, so the sampler can be pointed at it and nothing
/// else on the machine matches.
let private makeSleeper (dir: string) =
    // 15 characters: Linux truncates longer process names, and lookups by name would miss them.
    let name = "rfsmon-" + Guid.NewGuid().ToString("N").Substring(0, 8)

    if OperatingSystem.IsWindows() then
        let target = Path.Combine(dir, name + ".exe")
        File.Copy(Path.Combine(Environment.SystemDirectory, "PING.EXE"), target)
        name, target, [ "-n"; "120"; "127.0.0.1" ]
    else
        // A shell, not `sleep`: a multi-call coreutils binary picks the tool from its own name and exits when renamed.
        // The loop keeps the shell itself alive instead of exec'ing into `sleep`.
        let target = Path.Combine(dir, name)
        File.Copy("/bin/sh", target)
        File.SetUnixFileMode(target, UnixFileMode.UserRead ||| UnixFileMode.UserWrite ||| UnixFileMode.UserExecute)
        name, target, [ "-c"; "i=0; while [ $i -lt 120 ]; do sleep 1; i=$((i+1)); done" ]

let private startSleeper (path: string) (args: string list) =
    let psi = ProcessStartInfo path

    for a in args do
        psi.ArgumentList.Add a

    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    let proc = Process.Start psi
    proc.StandardOutput.ReadToEndAsync() |> ignore
    proc.StandardError.ReadToEndAsync() |> ignore
    proc

let private targets (name: string) =
    { RfsNames = [ name ]
      LspNames = []
      HostNames = []
      RfsAssemblies = []
      LspAssemblies = [] }

let private unusedPid () =
    let rec find candidate =
        try
            use _ = Process.GetProcessById candidate
            find (candidate + 7)
        with _ ->
            candidate

    find 3_900_001

let private writeRecord (pid: int) (kind: string) (startTicks: int64) (parent: (int * int64) option) =
    ProcessRegistry.ensureDirs ()
    let path = ProcessRegistry.procRecordPath pid
    let o = JsonObject()
    o["pid"] <- JsonValue.Create pid
    o["kind"] <- JsonValue.Create kind
    o["workspaceRoot"] <- JsonValue.Create "C:\\monitor-test\\ws"
    o["workspaceId"] <- JsonValue.Create "0123456789abcdef"
    o["pipeName"] <- JsonValue.Create "rfs-daemon-monitor-test"
    o["processStartUtcTicks"] <- JsonValue.Create startTicks

    match parent with
    | Some(ppid, pticks) ->
        o["parentPid"] <- JsonValue.Create ppid
        o["parentStartUtcTicks"] <- JsonValue.Create pticks
    | None -> ()

    o["binaryStamp"] <- JsonValue.Create(ProcessRegistry.currentBinaryStamp ())
    o["startedUtcTicks"] <- JsonValue.Create startTicks
    o["lastActiveUtcTicks"] <- JsonValue.Create DateTime.UtcNow.Ticks
    o["idleTimeoutSeconds"] <- JsonValue.Create 1800
    ProcessRegistry.atomicWriteText path (o.ToJsonString())
    path

let private withStateDir (body: string -> unit) =
    let dir = Path.Combine(Path.GetTempPath(), "rfs-monitor-test-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory dir |> ignore
    let previous = Environment.GetEnvironmentVariable "RFS_STATE_DIR"
    Environment.SetEnvironmentVariable("RFS_STATE_DIR", dir)

    try
        body dir
    finally
        Environment.SetEnvironmentVariable("RFS_STATE_DIR", previous)

        try
            Directory.Delete(dir, true)
        with _ ->
            ()

let private stop (proc: Process) =
    try
        if not proc.HasExited then
            proc.Kill true
            proc.WaitForExit 3000 |> ignore
    with _ ->
        ()

let private exitsWithin (proc: Process) (ms: int) = proc.WaitForExit ms

let private findRow (snapshot: MonitorSnapshot) (pid: int) =
    snapshot.Rows |> Array.tryFind (fun r -> r.Pid = pid)

let private samplerTests =
    testList "sampler" [
        test "a record whose process exited is removed, with its lease" {
            withStateDir (fun _ ->
                let pid = unusedPid ()
                let path = writeRecord pid "daemon" DateTime.UtcNow.Ticks None
                let sampler = MonitorSampler(targets "rfsmon-none")
                let snapshot = sampler.Take()
                let r = findRow snapshot pid |> Option.defaultWith (fun () -> failtest "record not reported")
                Expect.equal r.Verdict (Verdict.Cleanup Cleanup.DeadRecord) "verdict"
                let events = MonitorActions.autoClean snapshot
                Expect.isFalse (File.Exists path) "record deleted"
                Expect.exists events (fun e -> e.Text.Contains(string pid) && not e.Failed) "event names the PID")
        }

        test "a record whose PID now belongs to another process is removed and that process survives" {
            withStateDir (fun dir ->
                let name, exe, args = makeSleeper dir
                use proc = startSleeper exe args

                try
                    let wrongStart = proc.StartTime.ToUniversalTime().AddHours(-1.).Ticks
                    let path = writeRecord proc.Id "daemon" wrongStart None
                    let sampler = MonitorSampler(targets name)
                    let snapshot = sampler.Take()

                    let recordRow =
                        snapshot.Rows |> Array.find (fun r -> r.Pid = proc.Id && r.Liveness <> Liveness.Running)

                    Expect.equal recordRow.Verdict (Verdict.Cleanup Cleanup.ReusedPid) "verdict"
                    MonitorActions.autoClean snapshot |> ignore
                    Expect.isFalse (File.Exists path) "record deleted"
                    Expect.isFalse proc.HasExited "the process holding the PID is untouched"
                finally
                    stop proc)
        }

        test "an mcp server whose recorded parent exited is stopped" {
            withStateDir (fun dir ->
                let name, exe, args = makeSleeper dir
                use proc = startSleeper exe args

                try
                    let start = proc.StartTime.ToUniversalTime().Ticks
                    let path = writeRecord proc.Id "mcp" start (Some(unusedPid (), start - 1000L))
                    let sampler = MonitorSampler(targets name)
                    let snapshot = sampler.Take()
                    let r = findRow snapshot proc.Id |> Option.defaultWith (fun () -> failtest "mcp not reported")
                    Expect.equal r.Kind MonitorKind.Mcp "kind from the record"
                    Expect.equal r.Verdict (Verdict.Cleanup Cleanup.ParentGone) "verdict"
                    let events = MonitorActions.autoClean snapshot
                    Expect.isTrue (exitsWithin proc 5000) "stopped"
                    Expect.isFalse (File.Exists path) "record deleted"
                    Expect.exists events (fun e -> e.Text.StartsWith("stopped mcp", StringComparison.Ordinal)) "event"
                finally
                    stop proc)
        }

        test "a registered mcp server with a live parent is reported and left alone" {
            withStateDir (fun dir ->
                let name, exe, args = makeSleeper dir
                use proc = startSleeper exe args

                try
                    use self = Process.GetCurrentProcess()
                    let start = proc.StartTime.ToUniversalTime().Ticks
                    writeRecord proc.Id "mcp" start (Some(self.Id, self.StartTime.ToUniversalTime().Ticks)) |> ignore
                    let sampler = MonitorSampler(targets name)
                    sampler.Take() |> ignore
                    let snapshot = sampler.Take()
                    let r = findRow snapshot proc.Id |> Option.defaultWith (fun () -> failtest "mcp not reported")
                    Expect.equal r.Verdict Verdict.Healthy "healthy"
                    Expect.equal r.Workspace (Some "C:\\monitor-test\\ws") "workspace from the record"
                    Expect.isGreaterThan r.WorkingSet 0L "memory sampled"

                    match r.Parent with
                    | ParentState.Running(pid, _) -> Expect.equal pid self.Id "parent is the test process"
                    | other -> failtest $"parent %A{other}"

                    Expect.isEmpty (MonitorActions.autoClean snapshot) "nothing to clean"
                    Expect.isFalse proc.HasExited "untouched"
                finally
                    stop proc)
        }

        test "a process found by name, with a live parent, is reported as a CLI and left alone" {
            withStateDir (fun dir ->
                let name, exe, args = makeSleeper dir
                use proc = startSleeper exe args

                try
                    let snapshot = MonitorSampler(targets name).Take()
                    let r = findRow snapshot proc.Id |> Option.defaultWith (fun () -> failtest "process not found")
                    Expect.equal r.Kind MonitorKind.Cli "kind"
                    Expect.isFalse r.Registered "no record"
                    Expect.equal r.Verdict Verdict.Healthy "healthy"
                    MonitorActions.autoClean snapshot |> ignore
                    Expect.isFalse proc.HasExited "untouched"
                finally
                    stop proc)
        }

        test "a leftover temp file is removed only when its writer is gone and it is a minute old" {
            withStateDir (fun _ ->
                ProcessRegistry.ensureDirs ()
                let dead = unusedPid ()
                let old = Path.Combine(ProcessRegistry.procsDir (), $"%d{dead}.json.tmp.%d{dead}.42")
                let fresh = Path.Combine(ProcessRegistry.procsDir (), $"%d{dead + 1}.json.tmp.%d{dead}.43")
                File.WriteAllText(old, "{}")
                File.WriteAllText(fresh, "{}")
                File.SetLastWriteTimeUtc(old, DateTime.UtcNow.AddMinutes -5.)
                let snapshot = MonitorSampler(targets "rfsmon-none").Take()
                Expect.equal snapshot.Leftovers [| old |] "only the old file"
                MonitorActions.autoClean snapshot |> ignore
                Expect.isFalse (File.Exists old) "old removed"
                Expect.isTrue (File.Exists fresh) "fresh kept")
        }

        test "an orphaned stdio server found by name is stopped (Windows keeps the dead parent's PID)" {
            if not (OperatingSystem.IsWindows()) then
                skiptest "Linux re-parents orphans to init, which the rules flag instead of cleaning"

            withStateDir (fun dir ->
                let name, exe, args = makeSleeper dir
                let psi = ProcessStartInfo "cmd.exe"
                psi.Arguments <- $"/c start \"\" /b \"%s{exe}\" %s{String.Join(' ', args)} >nul"
                psi.UseShellExecute <- false
                psi.CreateNoWindow <- true
                use shell = Process.Start psi
                Expect.isTrue (shell.WaitForExit 10000) "launcher exits"

                let orphan =
                    let deadline = Stopwatch.StartNew()
                    let mutable found = [||]

                    while found.Length = 0 && deadline.ElapsedMilliseconds < 5000L do
                        found <- Process.GetProcessesByName name
                        if found.Length = 0 then Thread.Sleep 100

                    Expect.equal found.Length 1 "one orphan"
                    found[0]

                use orphan = orphan

                try
                    let sampler = MonitorSampler({ targets name with RfsNames = []; LspNames = [ name ] })
                    let snapshot = sampler.Take()
                    let r = findRow snapshot orphan.Id |> Option.defaultWith (fun () -> failtest "orphan not reported")
                    Expect.equal r.Kind MonitorKind.Lsp "kind"
                    Expect.equal r.Verdict (Verdict.Cleanup Cleanup.ParentGone) "verdict"
                    MonitorActions.autoClean snapshot |> ignore
                    Expect.isTrue (exitsWithin orphan 5000) "stopped"
                finally
                    stop orphan)
        }
    ]

// ---------------------------------------------------------------------------------------------------------------
// Daemon: the monitor's requests must not keep an idle daemon alive
// ---------------------------------------------------------------------------------------------------------------

let private startDaemon (stateDir: string) (workspace: string) (idleSeconds: int) =
    let rfsExe = Path.Combine(AppContext.BaseDirectory, if OperatingSystem.IsWindows() then "rfs.exe" else "rfs")

    let fileName, prefix =
        if File.Exists rfsExe then rfsExe, [] else "dotnet", [ typeof<DaemonLease>.Assembly.Location ]

    let psi = ProcessStartInfo fileName

    for a in prefix @ [ "daemon"; "run"; "--workspace"; workspace; "--idle-timeout"; string idleSeconds ] do
        psi.ArgumentList.Add a

    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Environment["RFS_STATE_DIR"] <- stateDir
    let proc = Process.Start psi
    proc.StandardOutput.ReadToEndAsync() |> ignore
    proc.StandardError.ReadToEndAsync() |> ignore
    proc

let private requestAsync (pipe: string) (op: string) (passive: bool) =
    task {
        use cts = new CancellationTokenSource 3000
        use client = new NamedPipeClientStream(".", pipe, PipeDirection.InOut, PipeOptions.Asynchronous)
        do! client.ConnectAsync cts.Token
        let req = JsonObject()
        req["op"] <- JsonValue.Create op

        if passive then
            req["passive"] <- JsonValue.Create true

        do! PipeFraming.writeAsync client req cts.Token
        let! resp = PipeFraming.readAsync client cts.Token
        return resp
    }

let private waitForPipe (pipe: string) =
    let watch = Stopwatch.StartNew()
    let mutable ready = false

    while not ready && watch.Elapsed < TimeSpan.FromSeconds 30. do
        ready <- ProcessRegistry.pingPipeAsync pipe 500 |> fun t -> t.Result

        if not ready then
            Thread.Sleep 100

    ready

let private daemonTests =
    testList "daemon" [
        testTask "passive pings and monitor requests do not keep an idle daemon alive; real requests do" {
            let stateDir = Path.Combine(Path.GetTempPath(), "rfs-monitor-daemon-" + Guid.NewGuid().ToString("N").Substring(0, 8))
            let workspace = Path.Combine(stateDir, "ws")
            Directory.CreateDirectory workspace |> ignore
            let pipe = WorkspaceRoot.pipeName stateDir workspace
            let busy = startDaemon stateDir workspace 3

            try
                // Kept busy with real pings for 5 s, past its 3 s idle timeout: it must stay up.
                Expect.isTrue (waitForPipe pipe) "daemon answers"
                let watch = Stopwatch.StartNew()

                while watch.Elapsed < TimeSpan.FromSeconds 5. do
                    let! _ = requestAsync pipe "ping" false
                    do! Task.Delay 300

                Expect.isFalse busy.HasExited "real requests keep the daemon alive"
                let! info = MonitorActions.daemonInfoAsync pipe 3000

                match info with
                | Ok(data, _) -> Expect.equal (JsonUtil.intOpt data "daemonPid") (Some busy.Id) "monitor op answers with the daemon's PID"
                | Error message -> failtest message

                // Now only passive traffic: the idle clock must keep running and the daemon must exit.
                let idle = Stopwatch.StartNew()

                while not busy.HasExited && idle.Elapsed < TimeSpan.FromSeconds 10. do
                    try
                        let! _ = requestAsync pipe "ping" true
                        let! _ = MonitorActions.daemonInfoAsync pipe 1000
                        ()
                    with _ ->
                        ()

                    do! Task.Delay 300

                Expect.isTrue busy.HasExited $"daemon exited under passive traffic (waited %.1f{idle.Elapsed.TotalSeconds} s)"
                Expect.isLessThan idle.Elapsed.TotalSeconds 8. "within the idle timeout plus the watchdog interval"
            finally
                stop busy
                busy.Dispose()

                try
                    Directory.Delete(stateDir, true)
                with _ ->
                    ()
        }
    ]

// ---------------------------------------------------------------------------------------------------------------
// Screen, rendered without a terminal
// ---------------------------------------------------------------------------------------------------------------

let private fakeRows () =
    let started = DateTime.UtcNow.AddMinutes -20.

    [| { row MonitorKind.Daemon with
           Key = "p:101"
           Pid = 101
           ProcessName = "rfs"
           Registered = true
           StartedUtc = Some started
           Workspace = Some "C:\\wk\\alpha\\repo-one"
           Parent = ParentState.Detached
           WorkingSet = 700L * 1048576L
           Verdict = Verdict.Healthy }
       { row MonitorKind.Lsp with
           Key = "p:202"
           Pid = 202
           ProcessName = "ranvierautocomplete"
           StartedUtc = Some started
           Parent = ParentState.Exited 55
           WorkingSet = 300L * 1048576L
           Verdict = Verdict.Cleanup Cleanup.ParentGone }
       { row MonitorKind.Mcp with
           Key = "p:303"
           Pid = 303
           ProcessName = "rfs"
           Registered = true
           StartedUtc = Some started
           Workspace = Some "C:\\wk\\beta\\repo-two"
           Parent = ParentState.Running(77, "claude")
           WorkingSet = 90L * 1048576L
           Verdict = Verdict.Concern Concern.Outdated } |]

let private frameLines (frame: Frame) =
    [| for y in 0 .. frame.Height - 1 ->
           let sb = StringBuilder()

           for x in 0 .. frame.Width - 1 do
               let cell = frame.Cells[y * frame.Width + x]

               if cell.Width > 0uy then
                   sb.Append(if String.IsNullOrEmpty cell.Glyph then " " else cell.Glyph) |> ignore

           sb.ToString() |]

let private shows (lines: string array) (text: string) =
    lines |> Array.exists (fun line -> line.Contains(text, StringComparison.Ordinal))

let private screenTests =
    testList "screen" [
        test "renders processes, confirms before stopping, filters and shows help" {
            use graph = new Graph()
            let stopped = ResizeArray<int>()

            let snapshot =
                { MonitorSnapshot.Empty with
                    TakenUtc = DateTime.UtcNow
                    CurrentStamp = stamp
                    Rows = fakeRows () }

            let services =
                { Take = fun () -> snapshot
                  AutoClean = fun _ -> []
                  Stop =
                    fun r ->
                        lock stopped (fun () -> stopped.Add r.Pid)
                        Task.FromResult(MonitorEvent.Now($"stopped %d{r.Pid}", false))
                  Describe = fun _ _ -> Task.FromResult [| "extra detail line" |] }

            let model = graph.Run(fun () -> MonitorModel.create services true 1000 (struct (120, 36)))
            let view = View.mount graph (fun () -> MonitorApp.root model)

            let settle () =
                for _ in 1..6 do
                    graph.Run(fun () ->
                        MonitorModel.tick 0.05 model
                        MonitorApp.update model)

                    let pending: Task list =
                        [ yield! model.Sampling |> Option.map (fun t -> t :> Task) |> Option.toList
                          yield! model.Acting |> Option.map (fun t -> t :> Task) |> Option.toList
                          yield! model.Describing |> Option.map (fun (_, t) -> t :> Task) |> Option.toList ]

                    for t in pending do
                        t.Wait 5000 |> ignore

            let render width height =
                View.dispatch (Resize(struct (width, height))) view |> ignore
                settle ()
                let frame = Frame.create width height
                Layout.render view frame
                frameLines frame

            let key (scalar: char) =
                View.dispatch (Key { Key = Key.Character; Scalar = int scalar; Modifiers = Modifiers.None; Released = false; Paste = false }) view
                |> ignore

                settle ()

            let special (k: Key) =
                View.dispatch (Key { Key = k; Scalar = 0; Modifiers = Modifiers.None; Released = false; Paste = false }) view |> ignore
                settle ()

            try
                let screen = render 120 36
                Expect.isTrue (shows screen "rfs monitor") "header"
                Expect.isTrue (shows screen "clean: parent gone") "cleanup state"
                Expect.isTrue (shows screen "check: older rfs build") "concern state"
                Expect.isTrue (shows screen "alpha\\repo-one") "workspace column"
                Expect.isTrue (shows screen "1 flagged") "header counts the flagged row"
                Expect.isTrue (shows screen "extra detail line") ("detail lines from Describe:\n" + String.Join("\n", screen))

                // Default sort puts the cleanup first, so k targets it; n cancels.
                key 'k'
                Expect.isTrue (shows (render 120 36) "Stop lsp 202?") "confirmation names the process"
                key 'n'
                Expect.isFalse (shows (render 120 36) "Stop lsp 202?") "any other key cancels"
                Expect.isEmpty stopped "nothing stopped"

                special Key.Down
                key 'k'
                key 'y'
                Expect.contains stopped 303 "confirmed stop of the selected row"
                Expect.isTrue (shows (render 120 36) "stopped 303") "event shown"

                key 'c'
                Expect.isTrue (shows (render 120 36) "Stop all 1 flagged processes?") "stop flagged asks first"
                special Key.Escape

                key '/'

                for c in "beta" do
                    key c

                special Key.Enter
                let filtered = render 120 36
                Expect.isTrue (shows filtered "processes 1 of 3") "filter caption"
                Expect.isFalse (shows filtered "repo-one") "filtered out"
                special Key.Escape

                key '?'
                Expect.isTrue (shows (render 120 36) "Auto-clean (a toggles it)") "help"
                key 'x'
                Expect.isFalse (shows (render 120 36) "Auto-clean (a toggles it)") "any key closes help"

                key 'a'
                Expect.isTrue (shows (render 120 36) "auto-clean OFF") "toggle"

                for width, height in [ 200, 60; 100, 30; 80, 24; 60, 14 ] do
                    let lines = render width height
                    Expect.equal lines.Length height $"%d{width}x%d{height} height"
                    Expect.isTrue (shows lines "rfs monitor") $"%d{width}x%d{height} header"

                Expect.isTrue (shows (render 40 10) "Enlarge the terminal") "too small"

                key 'q'
                Expect.isTrue model.Quit "q quits"
            finally
                View.dispose view
                MonitorModel.close model
        }
    ]

// ---------------------------------------------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------------------------------------------

let private runRfs (stateDir: string) (args: string list) =
    let rfsExe = Path.Combine(AppContext.BaseDirectory, if OperatingSystem.IsWindows() then "rfs.exe" else "rfs")

    let fileName, prefix =
        if File.Exists rfsExe then rfsExe, [] else "dotnet", [ typeof<DaemonLease>.Assembly.Location ]

    let psi = ProcessStartInfo fileName

    for a in prefix @ args do
        psi.ArgumentList.Add a

    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Environment["RFS_STATE_DIR"] <- stateDir
    use proc = Process.Start psi
    proc.StandardInput.Close()
    let out = proc.StandardOutput.ReadToEndAsync()
    let err = proc.StandardError.ReadToEndAsync()
    Expect.isTrue (proc.WaitForExit 30000) "rfs exits"
    proc.ExitCode, out.Result, err.Result

let private cliTests =
    testList "cli" [
        test "tui, monitor and top print a JSON snapshot with --once --json, and --clean removes a dead record" {
            let stateDir = Path.Combine(Path.GetTempPath(), "rfs-monitor-cli-" + Guid.NewGuid().ToString("N").Substring(0, 8))
            Directory.CreateDirectory(Path.Combine(stateDir, "procs")) |> ignore

            try
                for command in [ "tui"; "monitor"; "top" ] do
                    let code, out, err = runRfs stateDir [ command; "--once"; "--json" ]
                    Expect.equal code 0 $"%s{command}: %s{err}"
                    let json = JsonNode.Parse(out).AsObject()
                    Expect.isNotNull json["processes"] $"%s{command}: processes array"

                let dead = unusedPid ()
                let record = Path.Combine(stateDir, "procs", $"%d{dead}.json")

                File.WriteAllText(
                    record,
                    $"{{\"pid\":%d{dead},\"kind\":\"daemon\",\"workspaceRoot\":\"x\",\"workspaceId\":\"y\",\"pipeName\":\"z\",\"processStartUtcTicks\":1,\"binaryStamp\":\"s\",\"lastActiveUtcTicks\":1}}"
                )

                let code, out, err = runRfs stateDir [ "top"; "--once"; "--clean" ]
                Expect.equal code 0 err
                Expect.stringContains out $"cleaned: removed the record of daemon %d{dead}" "reports the cleanup"
                Expect.isFalse (File.Exists record) "record deleted"

                // Redirected output cannot host the TUI; it falls back to one snapshot.
                let code, out, _ = runRfs stateDir [ "tui" ]
                Expect.equal code 0 "fallback exit code"
                Expect.stringContains out "rfs monitor | " "fallback prints the table"
                Expect.isTrue (out |> Seq.forall (fun c -> int c < 128)) $"plain output is ASCII: %s{out}"

                let code, _, err = runRfs stateDir [ "tui"; "--interval-ms"; "5" ]
                Expect.equal code 2 "bad interval"
                Expect.stringContains err "--interval-ms" "names the option"
            finally
                try
                    Directory.Delete(stateDir, true)
                with _ ->
                    ()
        }
    ]

[<Tests>]
let tests =
    testList "Monitor" [
        ruleTests
        screenTests
        testSequencedGroup "rfs-state-dir" (testList "registry" [ samplerTests; daemonTests; cliTests ])
    ]
