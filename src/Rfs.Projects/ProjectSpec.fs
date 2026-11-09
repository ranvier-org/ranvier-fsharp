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
      ReferencedProjects: string list
      /// The framework each of ReferencedProjects resolved to, in order: the one in its `-r:`, as the evaluation lists
      /// only every framework a reference has. `?` when that can't be told; such a reference stays the file on disk.
      ReferencedFrameworks: string list
      /// The framework this evaluation is for
      TargetFramework: string
      /// What the build generates and compiles under obj (assembly attributes), in compile order, before SourceFiles.
      /// Evaluating the project writes them, so they are current as of the spec.
      GeneratedFiles: string list }

module ProjectSpec =

    // Bump when the cache content or the way `load` derives it changes; a mismatch is a cache miss.
    let private formatVersion = 6

    let toJson (spec: ProjectSpec) =
        let arr (xs: string list) =
            JsonArray(xs |> List.map (fun x -> JsonValue.Create x :> JsonNode) |> Array.ofList)

        let o = JsonObject()
        o["FormatVersion"] <- JsonValue.Create formatVersion
        o["ProjectFile"] <- JsonValue.Create spec.ProjectFile
        o["SourceFiles"] <- arr spec.SourceFiles
        o["OtherOptions"] <- arr spec.OtherOptions
        o["ReferencedProjects"] <- arr spec.ReferencedProjects
        o["ReferencedFrameworks"] <- arr spec.ReferencedFrameworks
        o["TargetFramework"] <- JsonValue.Create spec.TargetFramework
        o["GeneratedFiles"] <- arr spec.GeneratedFiles
        o.ToJsonString()

    let ofJson (json: string) : ProjectSpec =
        let root = JsonNode.Parse json

        if isNull root["FormatVersion"] || root["FormatVersion"].GetValue<int>() <> formatVersion then
            failwith "cache format version mismatch"

        let strings (name: string) = [ for n in root[name].AsArray() -> n.GetValue<string>() ]

        { ProjectFile = root["ProjectFile"].GetValue<string>()
          SourceFiles = strings "SourceFiles"
          OtherOptions = strings "OtherOptions"
          ReferencedProjects = strings "ReferencedProjects"
          ReferencedFrameworks = strings "ReferencedFrameworks"
          TargetFramework = root["TargetFramework"].GetValue<string>()
          GeneratedFiles = strings "GeneratedFiles" }

    let private objDir (fsproj: string) =
        Path.Combine(Path.GetDirectoryName fsproj, "obj")

    // The format version is in the name: two versions of the code on one obj each keep their own cache.
    let private cacheStem = $"rfs-spec.v%d{formatVersion}"

    /// Where `load` caches the spec of a project
    let cachePath fsproj = Path.Combine(objDir fsproj, cacheStem + ".json")
    let private cachePathFor fsproj (framework: string) = Path.Combine(objDir fsproj, $"%s{cacheStem}-%s{framework}.json")
    let private assetsPath fsproj = Path.Combine(objDir fsproj, "project.assets.json")

    /// Semantic version order: the numbers, then a release over a prerelease, then the prerelease's dot-separated parts
    /// (numbers by value and before words, so rc.2 is after rc.1 and rc.10 after rc.2).
    let private compareVersions (a: string) (b: string) =
        let split (v: string) =
            match v.IndexOf '-' with
            | -1 -> v, [||]
            | i -> v.Substring(0, i), v.Substring(i + 1).Split '.'

        let numeric (v: string) =
            match Version.TryParse v with
            | true, parsed -> parsed
            | _ -> Version(0, 0)

        let compareParts (x: string) (y: string) =
            match Int64.TryParse x, Int64.TryParse y with
            | (true, m), (true, n) -> compare m n
            | (true, _), _ -> -1
            | _, (true, _) -> 1
            | _ -> String.CompareOrdinal(x, y)

        let na, pa = split a
        let nb, pb = split b

        match compare (numeric na) (numeric nb) with
        | 0 ->
            match pa.Length, pb.Length with
            | 0, 0 -> 0
            | 0, _ -> 1
            | _, 0 -> -1
            | _ ->
                let n = min pa.Length pb.Length

                match Seq.init n (fun i -> compareParts pa[i] pb[i]) |> Seq.tryFind (fun c -> c <> 0) with
                | Some c -> c
                | None -> compare pa.Length pb.Length
        | c -> c

    let private majorOf (v: string) =
        match Int32.TryParse(v.Split([| '.'; '-' |]).[0]) with
        | true, m -> m
        | _ -> -1

    /// The SDK whose major version is `major`, from the versions `dotnet --list-sdks` gave: the highest by semantic
    /// version, a release over a prerelease of the same number. The error names the versions found.
    let sdkForRuntime (major: int) (installed: string list) : Result<string, string> =
        match installed |> List.filter (fun v -> majorOf v = major) with
        | [] ->
            let found = if installed.IsEmpty then "none" else String.Join(", ", installed)

            Error
                $"no .NET SDK %d{major} is installed, and this server runs on .NET %d{major}, whose MSBuild has to be of that major version. SDKs found: %s{found}"
        | some -> Ok(some |> List.reduce (fun x y -> if compareVersions y x > 0 then y else x))

    let private installedSdks () =
        let psi = ProcessStartInfo("dotnet", "--list-sdks")
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.WorkingDirectory <- Path.GetTempPath()
        use p = Process.Start psi
        let stdout = p.StandardOutput.ReadToEnd()
        p.WaitForExit()

        [ for line in stdout.Split('\n', StringSplitOptions.RemoveEmptyEntries) do
              match line.Trim().Split(' ', StringSplitOptions.RemoveEmptyEntries) with
              | [||] -> ()
              | parts -> parts[0] ]

    // ProjInfo's `Init.init` takes a directory and the SDK it finds there with `dotnet --version`, so the SDK is chosen by
    // the global.json in that directory: one is written for the exact version. Not the exe's directory, nor the working
    // directory, whose global.json (or the highest installed SDK, with none) may be of another major version than this
    // runtime, whose MSBuild then fails to load here (System.Runtime 11.0.0.0 into a net10 process). The directory is
    // named by the version, so processes share it.
    /// Makes `<dir>/global.json` pin `version`. Safe against others doing the same: a file that is already right is left
    /// alone, and an IO failure (another process replacing the file, so reading or moving onto it is refused for a
    /// moment) is tried again a few times before it is raised.
    let ensureGlobalJson (dir: string) (version: string) : unit =
        let file = Path.Combine(dir, "global.json")

        let content =
            $"""{{ "sdk": {{ "version": "%s{version}", "rollForward": "disable", "allowPrerelease": true }} }}"""

        let rec attempt (n: int) =
            try
                Directory.CreateDirectory dir |> ignore

                if not (File.Exists file) || File.ReadAllText file <> content then
                    let tmp = file + "." + Guid.NewGuid().ToString("N") + ".tmp"
                    File.WriteAllText(tmp, content)

                    try
                        File.Move(tmp, file, true)
                    with _ ->
                        (try
                            File.Delete tmp
                         with _ ->
                             ())

                        reraise ()
            with
            | :? IOException
            | :? UnauthorizedAccessException when n < 20 ->
                Threading.Thread.Sleep 25
                attempt (n + 1)

        attempt 0

    /// A function that remembers its result, but not a failure: an IO error that passes must not stay.
    let private onceSucceeded (f: unit -> 'a) : unit -> 'a =
        let gate = obj ()
        let value = ref None

        fun () ->
            lock gate (fun () ->
                match value.Value with
                | Some v -> v
                | None ->
                    let v = f ()
                    value.Value <- Some v
                    v)

    let private sdkDirectory =
        onceSucceeded (fun () ->
            match sdkForRuntime Environment.Version.Major (installedSdks ()) with
            | Error message -> failwith message
            | Ok version ->
                let dir = Path.Combine(Path.GetTempPath(), "rfs-sdk", version)
                ensureGlobalJson dir version
                dir)

    let private restore (fsproj: string) =
        task {
            let psi = ProcessStartInfo("dotnet", $"restore \"{fsproj}\"")
            // Restores with the SDK the load builds with
            psi.WorkingDirectory <- sdkDirectory ()
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
    let private toolsPath = onceSucceeded (fun () -> Init.init (DirectoryInfo(sdkDirectory ())) None)

    let private sdkOf () = sprintf "%A" (toolsPath ())

    /// The files an evaluation of the project read, and its assets file wherever the layout puts it. A method of its own:
    /// the MSBuild assemblies load when it is compiled, and `toolsPath` has to have registered them by then.
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let private filesEvaluated (fsproj: string) (properties: (string * string) list) : string list =
        use collection = new Microsoft.Build.Evaluation.ProjectCollection(dict properties)
        let project = collection.LoadProject fsproj

        [ for i in project.Imports do
              i.ImportedProject.FullPath

          match project.GetPropertyValue "ProjectAssetsFile" with
          | "" -> ()
          | assets -> assets ]

    /// Imported when they exist, so one made later is in nothing an evaluation read
    let private conventionalImports (fsproj: string) =
        let rec up (d: DirectoryInfo) =
            [ if not (isNull d) then
                  for name in [ "Directory.Build.props"; "Directory.Build.targets"; "Directory.Packages.props"; "global.json" ] do
                      Path.Combine(d.FullName, name)

                  yield! up d.Parent ]

        up (FileInfo fsproj).Directory

    /// Whether the cached spec is still what an evaluation would give: the same SDK, and nothing it read has changed since.
    /// A cache without the list, as when reading it failed, is never fresh.
    let private isFresh (fsproj: string) (cache: string) =
        try
            File.Exists cache
            && (let c = File.GetLastWriteTimeUtc cache
                let root = JsonNode.Parse(File.ReadAllText cache)

                not (isNull root["Evaluated"])
                && root["Sdk"].GetValue<string>() = sdkOf ()
                && c >= File.GetLastWriteTimeUtc fsproj
                && root["Evaluated"].AsArray()
                   |> Seq.forall (fun f ->
                       let f = f.GetValue<string>()
                       File.Exists f && c >= File.GetLastWriteTimeUtc f)
                && conventionalImports fsproj |> List.forall (fun f -> not (File.Exists f) || c >= File.GetLastWriteTimeUtc f))
        with _ ->
            false

    let private cacheJson (spec: ProjectSpec) (properties: (string * string) list) =
        let root = JsonNode.Parse(toJson spec)

        try
            let evaluated = filesEvaluated spec.ProjectFile properties
            root["Sdk"] <- JsonValue.Create(sdkOf ())
            root["Evaluated"] <- JsonArray(evaluated |> List.map (fun x -> JsonValue.Create x :> JsonNode) |> Array.ofList)
        with ex ->
            eprintfn "[ranvierautocomplete] %s: the spec cache will not be used, as what the evaluation read is not known: %s" spec.ProjectFile ex.Message

        root.ToJsonString()

    /// What the SDK's AssemblyInfo and TargetFramework generators write: assembly attributes, in a namespace holding
    /// nothing. Anything else in a file is a declaration a project's files or its referencing projects can read. A file
    /// that is not there holds nothing yet: one that is gone is gone from the build too.
    let private holdsOnlyAssemblyAttributes (file: string) =
        not (File.Exists file)
        || File.ReadLines file
           |> Seq.forall (fun line ->
               let t = line.Trim()

               t = ""
               || t.StartsWith "//"
               || t.StartsWith "namespace "
               || t.StartsWith "open "
               || (t.StartsWith "[<" && t.EndsWith ">]")
               || t = "do ()"
               || t = "do()"
               || t = "()")

    let private loadFresh (fsproj: string) (properties: (string * string) list) : ProjectSpec =
        // Attribute files generated into obj do not take part in declaration checking; what else a build generates there does
        let generatedDir = objDir fsproj + string Path.DirectorySeparatorChar
        let loader = WorkspaceLoader.Create(toolsPath (), properties)

        let notes = System.Collections.Concurrent.ConcurrentQueue<string>()
        loader.Notifications.Add(fun n -> notes.Enqueue(sprintf "%A" n))

        match loader.LoadProjects([ fsproj ]) |> Seq.tryHead with
        | None ->
            let details = String.Join("; ", notes)
            failwith $"Ionide.ProjInfo returned no project for {fsproj}: {details}"
        | Some p ->
            let isGenerated (f: string) =
                f.StartsWith(generatedDir, StringComparison.OrdinalIgnoreCase) && holdsOnlyAssemblyAttributes f

            let resolvedFramework (projectFile: string) (frameworks: string) =
                match frameworks.Split(';', StringSplitOptions.RemoveEmptyEntries) with
                | [||] -> ""
                | [| only |] -> only
                | candidates ->
                    let dir = Path.GetDirectoryName projectFile + string Path.DirectorySeparatorChar

                    let inPath =
                        p.OtherOptions
                        |> List.tryPick (fun o ->
                            if o.StartsWith "-r:" && o.Substring(3).StartsWith(dir, StringComparison.OrdinalIgnoreCase) then
                                let segments = o.Substring(3).Split(Path.DirectorySeparatorChar)
                                candidates |> Array.tryFind (fun c -> Array.contains c segments)
                            else
                                None)

                    defaultArg inPath "?"

            { ProjectFile = fsproj
              SourceFiles = p.SourceFiles |> List.filter (isGenerated >> not)
              OtherOptions = p.OtherOptions
              ReferencedProjects = p.ReferencedProjects |> List.map (fun r -> r.ProjectFileName)
              ReferencedFrameworks = p.ReferencedProjects |> List.map (fun r -> resolvedFramework r.ProjectFileName r.TargetFramework)
              TargetFramework = p.TargetFramework
              GeneratedFiles = p.SourceFiles |> List.filter isGenerated }

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

    /// Drops the cached spec, so the next `load` evaluates the project again.
    let invalidate (fsproj: string) : unit =
        try
            let fsproj = Path.GetFullPath fsproj
            let dir = objDir fsproj

            if Directory.Exists dir then
                // This version's only: `<stem>.json` and `<stem>-<framework>.json`
                for cache in Directory.GetFiles(dir, cacheStem + "*.json") do
                    let name = Path.GetFileName cache

                    if name = cacheStem + ".json" || name.StartsWith(cacheStem + "-") then
                        File.Delete cache
        with
        | :? IOException
        | :? UnauthorizedAccessException ->
            ()

    let private loadWith (fsproj: string) (cache: string) (properties: (string * string) list) : Task<ProjectSpec> =
        task {
            let fsproj = Path.GetFullPath fsproj

            // With a leftover obj/project.assets.json a missing project would otherwise load as an empty one.
            if not (File.Exists fsproj) then
                raise (FileNotFoundException($"project file not found: {fsproj}", fsproj))

            let cached =
                if isFresh fsproj cache then
                    try
                        // A build can write what it generates after the spec was cached
                        Some(ofJson (File.ReadAllText cache))
                        |> Option.filter (fun spec -> spec.GeneratedFiles |> List.forall holdsOnlyAssemblyAttributes)
                    with _ ->
                        None
                else
                    None

            match cached with
            | Some spec -> return requireReferences spec
            | None ->
                if not (File.Exists(assetsPath fsproj)) then
                    do! restore fsproj

                let spec = loadFresh fsproj properties
                // The cache is an optimisation: a read-only or contended obj directory must not fail the load.
                let tmp = cache + "." + Guid.NewGuid().ToString("N") + ".tmp"

                try
                    Directory.CreateDirectory(objDir fsproj) |> ignore
                    File.WriteAllText(tmp, cacheJson spec properties)
                    File.Move(tmp, cache, true)
                with
                | :? IOException
                | :? UnauthorizedAccessException ->
                    try
                        File.Delete tmp
                    with _ ->
                        ()

                return requireReferences spec
        }

    let load (fsproj: string) : Task<ProjectSpec> =
        let fsproj = Path.GetFullPath fsproj
        loadWith fsproj (cachePath fsproj) []

    /// The spec of a project as a reference to it resolved: for that framework, when the project's default evaluation
    /// is for another (a multi-target project takes its first).
    let loadFor (fsproj: string) (framework: string) : Task<ProjectSpec> =
        task {
            let fsproj = Path.GetFullPath fsproj
            let! spec = load fsproj

            if String.IsNullOrEmpty framework || framework = "?" || spec.TargetFramework = framework then
                return spec
            else
                return! loadWith fsproj (cachePathFor fsproj framework) [ "TargetFramework", framework ]
        }

    /// Whether a project reference can be a graph: what is not F# stays the assembly on disk.
    let isFSharpProject (project: string) =
        String.Equals(Path.GetExtension project, ".fsproj", StringComparison.OrdinalIgnoreCase)

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
