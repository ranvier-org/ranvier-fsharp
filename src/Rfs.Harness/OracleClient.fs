namespace Rfs.Harness

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Rfs.Lsp

/// A running `Rfs.Oracle` process. Requests are answered one at a time, in the order they are made.
type OracleClient private (proc: Process, stderrTail: Queue<string>) =
    let gate = new SemaphoreSlim 1

    let tail () =
        lock stderrTail (fun () -> String.Join("\n", stderrTail))

    /// The oracle's apphost sits next to the harness because Rfs.Harness references the Rfs.Oracle project. It is run
    /// directly, not as `dotnet Rfs.Oracle.dll`: Ionide.ProjInfo looks for the SDK from the process's own directory,
    /// and under `dotnet` that is the dotnet install, which ignores the repo's global.json.
    static member private Locate() : string =
        let dir = Path.GetDirectoryName(typeof<OracleClient>.Assembly.Location)
        let exe = Path.Combine(dir, if OperatingSystem.IsWindows() then "Rfs.Oracle.exe" else "Rfs.Oracle")

        if File.Exists exe then
            exe
        else
            failwith $"Rfs.Oracle not found in {dir}; Rfs.Harness must reference the Rfs.Oracle project"

    static member start() : Task<OracleClient> =
        task {
            let psi = ProcessStartInfo(OracleClient.Locate())
            psi.RedirectStandardInput <- true
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.StandardInputEncoding <- UTF8Encoding false
            psi.StandardOutputEncoding <- UTF8Encoding false
            psi.UseShellExecute <- false
            // The oracle also sets it for itself; the child's environment is the one place it is certain to be read late enough.
            psi.Environment["FICKL_FCS_SIGNATURE_CUTOFF"] <- "0"
            let proc = Process.Start psi
            let tail = Queue<string>()

            // Undrained stderr would block the oracle once the pipe fills; the last lines explain a crash.
            proc.ErrorDataReceived.Add(fun e ->
                if not (isNull e.Data) then
                    lock tail (fun () ->
                        tail.Enqueue e.Data

                        while tail.Count > 30 do
                            tail.Dequeue() |> ignore))

            proc.BeginErrorReadLine()
            return new OracleClient(proc, tail)
        }

    /// The compiler's diagnostics for the whole project, with `overrides` (path to text) in place of what is on disk.
    member _.Check (project: string) (overrides: Map<string, string>) : Task<Diagnostic list> =
        task {
            do! gate.WaitAsync()

            try
                let request = JsonObject()
                request["project"] <- JsonValue.Create(Path.GetFullPath project)
                let files = JsonObject()

                for KeyValue(path, text) in overrides do
                    files[path] <- JsonValue.Create text

                request["overrides"] <- files

                try
                    do! proc.StandardInput.WriteLineAsync(request.ToJsonString())
                    do! proc.StandardInput.FlushAsync()
                with :? IOException ->
                    ()

                let! line = proc.StandardOutput.ReadLineAsync()

                if isNull line then
                    proc.WaitForExit()
                    return failwith $"oracle exited with code %d{proc.ExitCode}:\n%s{tail ()}"
                else
                    let response = JsonNode.Parse line

                    match response["error"] with
                    | null -> return [ for d in response["diagnostics"].AsArray() -> Rfs.Lsp.Diagnostics.ofJson d ]
                    | error -> return failwith $"oracle failed for {project}: {error.GetValue<string>()}"
            finally
                gate.Release() |> ignore
        }

    interface IAsyncDisposable with
        member _.DisposeAsync() =
            ValueTask(
                task {
                    try
                        proc.StandardInput.Close()

                        use cts = new CancellationTokenSource(TimeSpan.FromSeconds 5.)
                        do! proc.WaitForExitAsync cts.Token
                    with _ ->
                        try
                            proc.Kill true
                        with _ ->
                            ()

                    proc.Dispose()
                    gate.Dispose()
                }
            )
