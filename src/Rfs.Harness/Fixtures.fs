namespace Rfs.Harness

open System.Collections.Concurrent
open System.Diagnostics
open System.Threading

/// Fixtures whose projects reference each other: a `-r:` must exist to load, and a real build is the check that their
/// settings mean what the tests say.
module Fixtures =
    let private builds = ConcurrentDictionary<string, Lazy<unit>>()

    /// Builds the project with `dotnet build`, once per process per project. Fails if the build does.
    let build (project: string) : unit =
        let run () =
            let psi = ProcessStartInfo("dotnet", $"build \"{project}\" -v q --nologo")
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            use p = Process.Start psi
            let out = p.StandardOutput.ReadToEndAsync()
            let err = p.StandardError.ReadToEndAsync()
            p.WaitForExit()

            if p.ExitCode <> 0 then
                failwith $"building {project} failed: {out.Result}{err.Result}"

        builds.GetOrAdd(project, (fun _ -> Lazy<unit>(run, LazyThreadSafetyMode.ExecutionAndPublication))).Force()
