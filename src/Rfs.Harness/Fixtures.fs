namespace Rfs.Harness

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO

/// Fixtures whose projects reference each other: a `-r:` must exist to load, and a real build is the check that their
/// settings mean what the tests say.
module Fixtures =
    let private buildGate = obj ()
    let private built = HashSet<string>(StringComparer.OrdinalIgnoreCase)

    /// Builds the project with `dotnet build`, once per process per project. Fails if the build does.
    let build (project: string) : unit =
        let full = Path.GetFullPath project

        lock buildGate (fun () ->
            if not (built.Contains full) then
                let psi = ProcessStartInfo("dotnet", $"build \"{full}\" -v q --nologo --disable-build-servers")
                psi.WorkingDirectory <- Path.GetDirectoryName full
                psi.UseShellExecute <- false
                psi.CreateNoWindow <- true
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true
                psi.Environment.Remove "MSBUILD_EXE_PATH" |> ignore
                psi.Environment.Remove "MSBuildExtensionsPath" |> ignore
                psi.Environment.Remove "MSBuildSDKsPath" |> ignore
                psi.Environment.Remove "DOTNET_HOST_PATH" |> ignore
                psi.Environment["DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER"] <- "1"
                psi.Environment["MSBUILDDISABLENODEREUSE"] <- "1"
                use p = Process.Start psi
                let out = p.StandardOutput.ReadToEndAsync()
                let err = p.StandardError.ReadToEndAsync()
                p.WaitForExit()

                if p.ExitCode <> 0 then
                    failwith $"building {project} failed: {out.Result}{err.Result}"

                built.Add full |> ignore)
