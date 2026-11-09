namespace Rfs.Projects

open System
open System.Diagnostics
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

type ProjectSpec =
    { ProjectFile: string
      SourceFiles: string list
      OtherOptions: string list
      ReferencedProjects: string list }

module ProjectSpec =

    // Bump when the cache content or the way `load` derives it changes; a mismatch is a cache miss.
    let private formatVersion = 1

    let toJson (spec: ProjectSpec) =
        let arr (xs: string list) =
            JsonArray(xs |> List.map (fun x -> JsonValue.Create x :> JsonNode) |> Array.ofList)

        let o = JsonObject()
        o["FormatVersion"] <- JsonValue.Create formatVersion
        o["ProjectFile"] <- JsonValue.Create spec.ProjectFile
        o["SourceFiles"] <- arr spec.SourceFiles
        o["OtherOptions"] <- arr spec.OtherOptions
        o["ReferencedProjects"] <- arr spec.ReferencedProjects
        o.ToJsonString()

    let ofJson (json: string) : ProjectSpec =
        let root = JsonNode.Parse json

        if isNull root["FormatVersion"] || root["FormatVersion"].GetValue<int>() <> formatVersion then
            failwith "cache format version mismatch"

        let strings (name: string) = [ for n in root[name].AsArray() -> n.GetValue<string>() ]

        { ProjectFile = root["ProjectFile"].GetValue<string>()
          SourceFiles = strings "SourceFiles"
          OtherOptions = strings "OtherOptions"
          ReferencedProjects = strings "ReferencedProjects" }

    let private objDir (fsproj: string) =
        Path.Combine(Path.GetDirectoryName fsproj, "obj")

    let private cachePath fsproj = Path.Combine(objDir fsproj, "rfs-spec.json")
    let private assetsPath fsproj = Path.Combine(objDir fsproj, "project.assets.json")

    let private isFresh (fsproj: string) =
        let cache = cachePath fsproj
        let assets = assetsPath fsproj

        File.Exists cache
        && File.Exists assets
        && (let c = File.GetLastWriteTimeUtc cache
            c >= File.GetLastWriteTimeUtc fsproj && c >= File.GetLastWriteTimeUtc assets)

    let private restore (fsproj: string) =
        task {
            let psi = ProcessStartInfo("dotnet", $"restore \"{fsproj}\"")
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            use p = Process.Start psi
            let stdout = p.StandardOutput.ReadToEndAsync()
            let stderr = p.StandardError.ReadToEndAsync()
            do! p.WaitForExitAsync()

            if p.ExitCode <> 0 then
                failwith $"dotnet restore failed for {fsproj}:\n{stdout.Result}{stderr.Result}"
        }

    // MSBuildLocator may register only once per process.
    let private toolsPath =
        lazy (Init.init (DirectoryInfo(Path.GetDirectoryName(Environment.ProcessPath))) None)

    let private loadFresh (fsproj: string) : ProjectSpec =
        // AssemblyInfo and TargetFramework attribute files generated into obj do not take part in declaration checking.
        let generatedDir = objDir fsproj + string Path.DirectorySeparatorChar
        let loader = WorkspaceLoader.Create(toolsPath.Value, [])

        let notes = System.Collections.Concurrent.ConcurrentQueue<string>()
        loader.Notifications.Add(fun n -> notes.Enqueue(sprintf "%A" n))

        match loader.LoadProjects([ fsproj ]) |> Seq.tryHead with
        | None ->
            let details = String.Join("; ", notes)
            failwith $"Ionide.ProjInfo returned no project for {fsproj}: {details}"
        | Some p ->
            { ProjectFile = fsproj
              SourceFiles =
                p.SourceFiles
                |> List.filter (fun f -> not (f.StartsWith(generatedDir, StringComparison.OrdinalIgnoreCase)))
              OtherOptions = p.OtherOptions
              ReferencedProjects = p.ReferencedProjects |> List.map (fun r -> r.ProjectFileName) }

    /// The path `spec` references `referenced`'s assembly by, as it appears in its `-r:`: the file of the name the
    /// project outputs (its reference assembly has the same name).
    let referencePath (spec: ProjectSpec) (referenced: ProjectSpec) : string option =
        let output =
            referenced.OtherOptions
            |> List.tryPick (fun o ->
                if o.StartsWith "-o:" then Some(Path.GetFileName(o.Substring 3))
                elif o.StartsWith "--out:" then Some(Path.GetFileName(o.Substring 6))
                else None)
            |> Option.defaultValue (Path.GetFileNameWithoutExtension referenced.ProjectFile + ".dll")

        spec.OtherOptions
        |> List.tryPick (fun o ->
            if o.StartsWith "-r:" && String.Equals(Path.GetFileName(o.Substring 3), output, StringComparison.OrdinalIgnoreCase) then
                Some(o.Substring 3)
            else
                None)

    /// The `-r:` assemblies that are not on disk. A project reference whose project was never built shows up here.
    let missingReferences (spec: ProjectSpec) : string list =
        spec.OtherOptions
        |> List.choose (fun o -> if o.StartsWith "-r:" then Some(o.Substring 3) else None)
        |> List.distinct
        |> List.filter (fun r -> not (File.Exists r))

    let private requireReferences (spec: ProjectSpec) =
        match missingReferences spec with
        | [] -> spec
        | missing ->
            let listed = String.Join("\n", missing |> List.map (fun m -> "  " + m))

            failwith
                $"{spec.ProjectFile}: {missing.Length} referenced assemblies are not on disk (build the referenced projects, build/fetch-corpus.ps1 does it for the corpus):\n{listed}"

    /// Drops the cached spec, so the next `load` evaluates the project again: a change to a props or targets file the
    /// project imports does not touch the project file, which the cache's freshness check looks at.
    let invalidate (fsproj: string) : unit =
        try
            File.Delete(cachePath (Path.GetFullPath fsproj))
        with
        | :? IOException
        | :? UnauthorizedAccessException ->
            ()

    let load (fsproj: string) : Task<ProjectSpec> =
        task {
            let fsproj = Path.GetFullPath fsproj

            // With a leftover obj/project.assets.json a missing project would otherwise load as an empty one.
            if not (File.Exists fsproj) then
                raise (FileNotFoundException($"project file not found: {fsproj}", fsproj))

            let cached =
                if isFresh fsproj then
                    try
                        Some(ofJson (File.ReadAllText(cachePath fsproj)))
                    with _ ->
                        None
                else
                    None

            match cached with
            | Some spec -> return requireReferences spec
            | None ->
                if not (File.Exists(assetsPath fsproj)) then
                    do! restore fsproj

                let spec = loadFresh fsproj
                // The cache is an optimisation: a read-only or contended obj directory must not fail the load.
                let tmp = cachePath fsproj + "." + Guid.NewGuid().ToString("N") + ".tmp"

                try
                    Directory.CreateDirectory(objDir fsproj) |> ignore
                    File.WriteAllText(tmp, toJson spec)
                    File.Move(tmp, cachePath fsproj, true)
                with
                | :? IOException
                | :? UnauthorizedAccessException ->
                    try
                        File.Delete tmp
                    with _ ->
                        ()

                return requireReferences spec
        }

    /// The spec with type checking forced sequential, as an editor checks: each file sees every earlier file. In the
    /// compiler's default graph mode a file sees only the files the dependency graph links to it, which reports an
    /// unresolvable reference differently. The last --parallelcompilation flag wins, so it goes last.
    let sequential (spec: ProjectSpec) : ProjectSpec =
        { spec with
            OtherOptions =
                (spec.OtherOptions |> List.filter (fun o -> not (o.StartsWith "--parallelcompilation")))
                @ [ "--parallelcompilation-" ] }

    let toFcsOptions (spec: ProjectSpec) : FSharpProjectOptions =
        { ProjectFileName = spec.ProjectFile
          ProjectId = None
          SourceFiles = Array.ofList spec.SourceFiles
          OtherOptions = Array.ofList spec.OtherOptions
          ReferencedProjects = [||]
          IsIncompleteTypeCheckEnvironment = false
          UseScriptResolutionRules = false
          LoadTime = DateTime.Now
          UnresolvedReferences = None
          OriginalLoadReferences = []
          Stamp = None }
