module Rfs.Agent.Tests.DaemonAndReaperTests

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Rfs.Agent
open Rfs.Harness

let private findRepoRoot () =
    let rec walk (dir: DirectoryInfo) =
        if isNull dir then
            failtest "Could not locate RanvierFSharp.slnx."
        elif File.Exists(Path.Combine(dir.FullName, "RanvierFSharp.slnx")) then
            dir.FullName
        else
            walk dir.Parent

    walk (DirectoryInfo AppContext.BaseDirectory)

let private copyMultiTemp () =
    let repoRoot = findRepoRoot ()
    let multiDir = Path.Combine(repoRoot, "tests", "fixtures", "Multi")
    let tempRoot = Path.Combine(Path.GetTempPath(), "rfs-agent-daemon-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory tempRoot |> ignore
    Directory.CreateDirectory(Path.Combine(tempRoot, ".git")) |> ignore

    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(tempRoot, "global.json"))

    for cfg in [ "Directory.Build.props"; "Directory.Packages.props" ] do
        File.Copy(Path.Combine(multiDir, cfg), Path.Combine(tempRoot, cfg))

    for sub in [ "Lib"; "App" ] do
        let srcDir = Path.Combine(multiDir, sub)
        let dstDir = Path.Combine(tempRoot, sub)
        Directory.CreateDirectory dstDir |> ignore

        for f in Directory.GetFiles srcDir do
            File.Copy(f, Path.Combine(dstDir, Path.GetFileName f))

    tempRoot

let private deleteDir (dir: string) =
    try
        if Directory.Exists dir then
            Directory.Delete(dir, true)
    with _ ->
        ()

let private runRfsCli (stateDir: string) (idleSecondsOpt: int option) (args: string list) : int * string * string * int64 =
    let rfsExe = Path.Combine(AppContext.BaseDirectory, if OperatingSystem.IsWindows() then "rfs.exe" else "rfs")

    let fileName, prefixArgs =
        if File.Exists rfsExe then
            rfsExe, []
        else
            "dotnet", [ typeof<DaemonLease>.Assembly.Location ]

    let psi = ProcessStartInfo fileName

    for a in prefixArgs @ args do
        psi.ArgumentList.Add a

    psi.UseShellExecute <- false
    psi.CreateNoWindow <- true
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.Environment["RFS_STATE_DIR"] <- stateDir

    match idleSecondsOpt with
    | Some sec -> psi.Environment["RFS_IDLE_TIMEOUT_SECONDS"] <- string sec
    | None -> ()

    let sw = Stopwatch.StartNew()
    use proc = Process.Start psi
    Expect.isNotNull proc "started rfs process"
    proc.StandardInput.Close()
    let outTask = proc.StandardOutput.ReadToEndAsync()
    let errTask = proc.StandardError.ReadToEndAsync()
    let completed = Task.WhenAll(proc.WaitForExitAsync(), outTask, errTask).Wait(TimeSpan.FromSeconds 35.)
    sw.Stop()
    Expect.isTrue completed $"rfs command {String.Join(' ', args)} timed out holding stdout/stderr handles open"
    proc.ExitCode, outTask.Result.Trim(), errTask.Result.Trim(), sw.ElapsedMilliseconds

let private withIsolatedStateDir (body: string -> Task) =
    let stateDir = Path.Combine(Path.GetTempPath(), "rfs-state-test-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory stateDir |> ignore
    let prev = Environment.GetEnvironmentVariable "RFS_STATE_DIR"
    Environment.SetEnvironmentVariable("RFS_STATE_DIR", stateDir)

    task {
        try
            do! body stateDir
        finally
            try
                runRfsCli stateDir None [ "daemon"; "kill"; "--all" ] |> ignore
            with _ ->
                ()

            Environment.SetEnvironmentVariable("RFS_STATE_DIR", prev)
            deleteDir stateDir
    }

let private waitProcessExit (pid: int) (timeoutMs: int) =
    let sw = Stopwatch.StartNew()
    let mutable exited = false

    while not exited && sw.ElapsedMilliseconds < int64 timeoutMs do
        try
            use p = Process.GetProcessById pid
            exited <- p.HasExited
        with _ ->
            exited <- true

        if not exited then
            Threading.Thread.Sleep 100

    exited

[<Tests>]
let tests =
    testSequenced (
        testList "DaemonAndReaper" [
            testTask "daemon spawns detached without blocking stdio handles and warms subsequent calls" {
                let wsRoot = copyMultiTemp ()

                try
                    Fixtures.build (Path.Combine(wsRoot, "App", "App.fsproj"))

                    do!
                        withIsolatedStateDir (fun stateDir ->
                            task {
                                // 1. Cold call spawns background daemon; CLI must exit cleanly without handle hang.
                                let code1, out1, err1, _ = runRfsCli stateDir None [ "status"; "--workspace"; wsRoot; "--json" ]
                                Expect.equal code1 0 $"cold status failed: {err1}"
                                let j1 = JsonNode.Parse(out1).AsObject()
                                let pid1 = j1["daemonPid"].GetValue<int>()
                                Expect.isGreaterThan pid1 0 "valid daemon PID"

                                // 2. Warm call reuses same daemon PID
                                let code2, out2, err2, ms2 = runRfsCli stateDir None [ "status"; "--workspace"; wsRoot; "--json" ]
                                Expect.equal code2 0 $"warm status failed: {err2}"
                                let j2 = JsonNode.Parse(out2).AsObject()
                                let pid2 = j2["daemonPid"].GetValue<int>()
                                Expect.equal pid2 pid1 "warm call reuses running daemon PID"
                                Expect.isLessThan ms2 2000L $"warm CLI roundtrip was unexpectedly slow ({ms2}ms)"
                            })
                finally
                    deleteDir wsRoot
            }

            testTask "reaper cleans dead PIDs, preserves recycled unrelated PIDs, and auto-reaps binary mismatch" {
                let wsRoot = copyMultiTemp ()

                try
                    Fixtures.build (Path.Combine(wsRoot, "App", "App.fsproj"))

                    do!
                        withIsolatedStateDir (fun stateDir ->
                            task {
                                // A. Dead PID record cleanup
                                ProcessRegistry.ensureDirs ()
                                let deadProcPsi =
                                    if OperatingSystem.IsWindows() then
                                        ProcessStartInfo("cmd.exe", "/c exit 0")
                                    else
                                        let psi = ProcessStartInfo("/bin/sh")
                                        psi.ArgumentList.Add "-c"
                                        psi.ArgumentList.Add "exit 0"
                                        psi
                                deadProcPsi.CreateNoWindow <- true
                                deadProcPsi.UseShellExecute <- false
                                use deadProc = Process.Start deadProcPsi
                                deadProc.WaitForExit()
                                let deadPid = deadProc.Id
                                let deadStart = deadProc.StartTime.ToUniversalTime().Ticks
                                let wsId = WorkspaceRoot.workspaceId wsRoot
                                let pipeName = WorkspaceRoot.pipeName stateDir wsRoot

                                let deadRec: ProcessRecord =
                                    { Pid = deadPid
                                      Kind = "daemon"
                                      WorkspaceRoot = wsRoot
                                      WorkspaceId = wsId
                                      PipeName = pipeName
                                      ProcessStartUtcTicks = deadStart
                                      ParentPid = None
                                      ParentStartUtcTicks = None
                                      BinaryStamp = ProcessRegistry.currentBinaryStamp ()
                                      StartedUtcTicks = DateTime.UtcNow.Ticks
                                      LastActiveUtcTicks = DateTime.UtcNow.Ticks
                                      IdleTimeoutSeconds = 1800
                                      RecordPath = ProcessRegistry.procRecordPath deadPid }

                                let oDead = JsonObject()
                                oDead["pid"] <- JsonValue.Create deadRec.Pid
                                oDead["kind"] <- JsonValue.Create deadRec.Kind
                                oDead["workspaceRoot"] <- JsonValue.Create deadRec.WorkspaceRoot
                                oDead["workspaceId"] <- JsonValue.Create deadRec.WorkspaceId
                                oDead["pipeName"] <- JsonValue.Create deadRec.PipeName
                                oDead["processStartUtcTicks"] <- JsonValue.Create deadRec.ProcessStartUtcTicks
                                oDead["binaryStamp"] <- JsonValue.Create deadRec.BinaryStamp
                                oDead["lastActiveUtcTicks"] <- JsonValue.Create deadRec.LastActiveUtcTicks
                                ProcessRegistry.atomicWriteText deadRec.RecordPath (oDead.ToJsonString())

                                // B. Recycled/unrelated live PID safety check
                                let sleepPsi =
                                    if OperatingSystem.IsWindows() then
                                        ProcessStartInfo("pwsh", "-NoProfile -Command \"Start-Sleep 30\"")
                                    else
                                        let psi = ProcessStartInfo("sleep")
                                        psi.ArgumentList.Add "30"
                                        psi
                                sleepPsi.CreateNoWindow <- true
                                sleepPsi.UseShellExecute <- false
                                use bystander = Process.Start sleepPsi

                                try
                                    let oBystander = JsonObject()
                                    oBystander["pid"] <- JsonValue.Create bystander.Id
                                    oBystander["kind"] <- JsonValue.Create "daemon"
                                    oBystander["workspaceRoot"] <- JsonValue.Create wsRoot
                                    oBystander["workspaceId"] <- JsonValue.Create wsId
                                    oBystander["pipeName"] <- JsonValue.Create pipeName
                                    oBystander["processStartUtcTicks"] <- JsonValue.Create 0L
                                    oBystander["binaryStamp"] <- JsonValue.Create(ProcessRegistry.currentBinaryStamp ())
                                    oBystander["lastActiveUtcTicks"] <- JsonValue.Create DateTime.UtcNow.Ticks
                                    let bystanderRecPath = ProcessRegistry.procRecordPath bystander.Id
                                    ProcessRegistry.atomicWriteText bystanderRecPath (oBystander.ToJsonString())

                                    let! report = ProcessRegistry.reapStaleProcessesAsync false false (ProcessRegistry.currentBinaryStamp ()) false
                                    Expect.isFalse (File.Exists deadRec.RecordPath) "dead PID record cleaned"
                                    Expect.isFalse (File.Exists bystanderRecPath) "recycled PID record cleaned"
                                    Expect.isFalse bystander.HasExited "recycled unrelated process was NOT killed"

                                    let reasons = report.Actions |> List.map _.Reason
                                    Expect.contains reasons DeadLeaseCleaned "reported DeadLeaseCleaned"
                                    Expect.contains reasons RecycledPidCleaned "reported RecycledPidCleaned"
                                finally
                                    try
                                        if not bystander.HasExited then
                                            bystander.Kill(true)
                                            bystander.WaitForExit 2000 |> ignore
                                    with _ ->
                                        ()

                                // C. Real daemon binary-stamp mismatch auto-reap
                                let code1, out1, err1, _ = runRfsCli stateDir None [ "status"; "--workspace"; wsRoot; "--json" ]
                                Expect.equal code1 0 $"initial daemon start failed: {err1}"
                                let obj1 = JsonNode.Parse(out1).AsObject()
                                let pid1 = obj1["daemonPid"].GetValue<int>()

                                for dir in [ ProcessRegistry.daemonsDir (); ProcessRegistry.procsDir () ] do
                                    for file in Directory.GetFiles(dir, "*.json") do
                                        let node = JsonNode.Parse(File.ReadAllText file).AsObject()
                                        node["binaryStamp"] <- JsonValue.Create "stale-binary-stamp-v0"
                                        ProcessRegistry.atomicWriteText file (node.ToJsonString())

                                let code2, out2, err2, _ = runRfsCli stateDir None [ "status"; "--workspace"; wsRoot; "--json" ]
                                Expect.equal code2 0 $"status after binary mismatch failed: {err2}"
                                let obj2 = JsonNode.Parse(out2).AsObject()
                                let pid2 = obj2["daemonPid"].GetValue<int>()
                                Expect.notEqual pid2 pid1 "auto-reaper replaced stale binary daemon with fresh PID"
                                Expect.isTrue (waitProcessExit pid1 3000) "old binary-mismatched daemon was terminated"
                            })
                finally
                    deleteDir wsRoot
            }

            testTask "daemon exits automatically after short idle TTL" {
                let wsRoot = copyMultiTemp ()

                try
                    Fixtures.build (Path.Combine(wsRoot, "App", "App.fsproj"))

                    do!
                        withIsolatedStateDir (fun stateDir ->
                            task {
                                let code, outTxt, errTxt, _ = runRfsCli stateDir (Some 2) [ "status"; "--workspace"; wsRoot; "--json" ]
                                Expect.equal code 0 $"status with idle=2 failed: {errTxt}"
                                let obj = JsonNode.Parse(outTxt).AsObject()
                                let daemonPid = obj["daemonPid"].GetValue<int>()
                                Expect.isTrue (waitProcessExit daemonPid 7000) "daemon exited cleanly after 2s idle timeout"
                            })
                finally
                    deleteDir wsRoot
            }
        ]
    )
