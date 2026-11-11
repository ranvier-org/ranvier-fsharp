namespace Rfs.Projects

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json.Nodes
open System.Threading.Tasks
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

type ProjectItemKind =
    | Compile
    | Content
    | NoneItem

type ProjectItem =
    { Kind: ProjectItemKind
      FilePath: string
      VirtualPath: string
      Metadata: (string * string) list }

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
      GeneratedFiles: string list
      Items: ProjectItem list }

module ProjectSpec =

    // Bump when the cache content or the way `load` derives it changes; a mismatch is a cache miss.
    let private formatVersion = 7

    let private kindToString =
        function
        | ProjectItemKind.Compile -> "Compile"
        | ProjectItemKind.Content -> "Content"
        | ProjectItemKind.NoneItem -> "None"

    let private kindOfString =
        function
        | "Compile" -> ProjectItemKind.Compile
        | "Content" -> ProjectItemKind.Content
        | "None" -> ProjectItemKind.NoneItem
        | other -> failwith $"unknown ProjectItemKind: {other}"

    let toJson (spec: ProjectSpec) =
        let arr (xs: string list) =
            JsonArray(xs |> List.map (fun x -> JsonValue.Create x :> JsonNode) |> Array.ofList)

        let itemNode (item: ProjectItem) : JsonNode =
            let io = JsonObject()
            io["Kind"] <- JsonValue.Create(kindToString item.Kind)
            io["FilePath"] <- JsonValue.Create item.FilePath
            io["VirtualPath"] <- JsonValue.Create item.VirtualPath

            let metaArr =
                JsonArray(
                    item.Metadata
                    |> List.map (fun (k, v) ->
                        let pair = JsonArray()
                        pair.Add(JsonValue.Create k :> JsonNode)
                        pair.Add(JsonValue.Create v :> JsonNode)
                        pair :> JsonNode)
                    |> Array.ofList
                )

            io["Metadata"] <- metaArr
            io :> JsonNode

        let o = JsonObject()
        o["FormatVersion"] <- JsonValue.Create formatVersion
        o["ProjectFile"] <- JsonValue.Create spec.ProjectFile
        o["SourceFiles"] <- arr spec.SourceFiles
        o["OtherOptions"] <- arr spec.OtherOptions
        o["ReferencedProjects"] <- arr spec.ReferencedProjects
        o["ReferencedFrameworks"] <- arr spec.ReferencedFrameworks
        o["TargetFramework"] <- JsonValue.Create spec.TargetFramework
        o["GeneratedFiles"] <- arr spec.GeneratedFiles
        o["Items"] <- JsonArray(spec.Items |> List.map itemNode |> Array.ofList)
        o.ToJsonString()

    let ofJson (json: string) : ProjectSpec =
        let root = JsonNode.Parse json

        if isNull root["FormatVersion"] || root["FormatVersion"].GetValue<int>() <> formatVersion then
            failwith "cache format version mismatch"

        let strings (name: string) = [ for n in root[name].AsArray() -> n.GetValue<string>() ]

        let items =
            if isNull root["Items"] then
                []
            else
                [ for n in root["Items"].AsArray() ->
                      let meta =
                          if isNull n["Metadata"] then
                              []
                          else
                              [ for pair in n["Metadata"].AsArray() ->
                                    let p = pair.AsArray()
                                    p[0].GetValue<string>(), p[1].GetValue<string>() ]

                      { Kind = kindOfString (n["Kind"].GetValue<string>())
                        FilePath = n["FilePath"].GetValue<string>()
                        VirtualPath = n["VirtualPath"].GetValue<string>()
                        Metadata = meta } ]

        { ProjectFile = root["ProjectFile"].GetValue<string>()
          SourceFiles = strings "SourceFiles"
          OtherOptions = strings "OtherOptions"
          ReferencedProjects = strings "ReferencedProjects"
          ReferencedFrameworks = strings "ReferencedFrameworks"
          TargetFramework = root["TargetFramework"].GetValue<string>()
          GeneratedFiles = strings "GeneratedFiles"
          Items = items }

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

    let private tryGetMeta (key: string) (metadata: (string * string) list) : string option =
        metadata
        |> List.tryPick (fun (k, v) ->
            if String.Equals(k, key, StringComparison.OrdinalIgnoreCase) && not (String.IsNullOrWhiteSpace v) then
                Some(v.Trim())
            else
                None)

    let private collapseSlashes (s: string) : string =
        let norm = s.Replace('\\', '/')

        if norm.IndexOf("//", StringComparison.Ordinal) < 0 then
            norm
        else
            let sb = StringBuilder(norm.Length)
            let mutable prevSlash = false

            for i = 0 to norm.Length - 1 do
                let c = norm[i]

                if c = '/' then
                    if not prevSlash then
                        sb.Append('/') |> ignore
                        prevSlash <- true
                else
                    prevSlash <- false
                    sb.Append(c) |> ignore

            sb.ToString()

    let private expandItemWellKnown (fullPath: string) (recursiveDir: string) (raw: string) : string =
        let recNorm =
            let r = (recursiveDir.Replace('\\', '/')).TrimStart('/')
            if r = "" then "" else r.TrimEnd('/') + "/"

        let fileNameNoExt = Path.GetFileNameWithoutExtension fullPath
        let ext = Path.GetExtension fullPath

        let expanded =
            raw
                .Replace("%(RecursiveDir)", recNorm, StringComparison.OrdinalIgnoreCase)
                .Replace("%(Filename)", fileNameNoExt, StringComparison.OrdinalIgnoreCase)
                .Replace("%(Extension)", ext, StringComparison.OrdinalIgnoreCase)

        (collapseSlashes expanded).TrimStart('/')

    let computeVirtualPath
        (projectDir: string)
        (fullPath: string)
        (recursiveDir: string)
        (metadata: (string * string) list)
        : string =
        let fileFull = Path.GetFullPath fullPath

        match tryGetMeta "Link" metadata with
        | Some linkRaw ->
            let linkExpanded = expandItemWellKnown fileFull recursiveDir linkRaw

            if linkExpanded <> "" then
                linkExpanded
            else
                Path.GetFileName fileFull
        | None ->
            match tryGetMeta "LinkBase" metadata with
            | Some linkBaseRaw ->
                let basePart = (expandItemWellKnown fileFull recursiveDir linkBaseRaw).TrimEnd('/')
                let recPart = (recursiveDir.Replace('\\', '/')).TrimStart('/')
                let fileName = Path.GetFileName fileFull
                let suffix = if recPart = "" then fileName else recPart.TrimEnd('/') + "/" + fileName
                if basePart = "" then suffix else basePart + "/" + suffix
            | None ->
                let projFull =
                    (Path.GetFullPath projectDir)
                        .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

                let cmp =
                    if Glob.defaultCaseSensitive () then
                        StringComparison.Ordinal
                    else
                        StringComparison.OrdinalIgnoreCase

                if
                    fileFull.Length > projFull.Length + 1
                    && fileFull.StartsWith(projFull, cmp)
                    && (fileFull[projFull.Length] = Path.DirectorySeparatorChar
                        || fileFull[projFull.Length] = Path.AltDirectorySeparatorChar)
                then
                    Path.GetRelativePath(projFull, fileFull).Replace('\\', '/')
                else
                    Path.GetFileName fileFull

    let private computeGlobHash (probes: (string * string * string) list) : string =
        if probes.IsEmpty then
            ""
        else
            let caseSensitive = Glob.defaultCaseSensitive ()
            let sb = StringBuilder()

            for bDir, inc, exc in probes do
                sb.Append(bDir).Append('\n').Append(inc).Append('\n').Append(exc).Append('\n') |> ignore
                let excOpt = if String.IsNullOrWhiteSpace exc then None else Some exc

                for full, _ in Glob.expandDisk caseSensitive bDir inc excOpt do
                    sb.Append(full.Replace('\\', '/')).Append('\n') |> ignore

                sb.Append("---\n") |> ignore

            let bytes = Encoding.UTF8.GetBytes(sb.ToString())
            Convert.ToHexString(SHA256.HashData bytes)

    type private CondToken =
        | TLParen
        | TRParen
        | TBang
        | TEq
        | TNeq
        | TAnd
        | TOr
        | TWord of string

    let private tokenizeCond (s: string) : CondToken[] =
        let tokens = ResizeArray<CondToken>()
        let mutable i = 0
        let n = s.Length

        while i < n do
            let c = s[i]

            if Char.IsWhiteSpace c then
                i <- i + 1
            elif c = '(' then
                tokens.Add TLParen
                i <- i + 1
            elif c = ')' then
                tokens.Add TRParen
                i <- i + 1
            elif c = '=' && i + 1 < n && s[i + 1] = '=' then
                tokens.Add TEq
                i <- i + 2
            elif c = '!' && i + 1 < n && s[i + 1] = '=' then
                tokens.Add TNeq
                i <- i + 2
            elif c = '!' then
                tokens.Add TBang
                i <- i + 1
            elif c = '\'' || c = '"' then
                let quote = c
                i <- i + 1
                let start = i

                while i < n && s[i] <> quote do
                    i <- i + 1

                let lit = s.Substring(start, i - start)
                if i < n && s[i] = quote then i <- i + 1
                tokens.Add(TWord lit)
            else
                let start = i

                while i < n
                      && not (Char.IsWhiteSpace s[i])
                      && s[i] <> '('
                      && s[i] <> ')'
                      && s[i] <> '!'
                      && s[i] <> '='
                      && s[i] <> '\''
                      && s[i] <> '"' do
                    i <- i + 1

                let w = s.Substring(start, i - start)

                if String.Equals(w, "And", StringComparison.OrdinalIgnoreCase) then
                    tokens.Add TAnd
                elif String.Equals(w, "Or", StringComparison.OrdinalIgnoreCase) then
                    tokens.Add TOr
                else
                    tokens.Add(TWord w)

        tokens.ToArray()

    let private evalExpandedCondition (projectDir: string) (expandedCond: string) : bool =
        try
            let toks = tokenizeCond expandedCond
            let mutable pos = 0

            let peek () = if pos < toks.Length then ValueSome toks[pos] else ValueNone

            let rec parseOr () =
                let mutable left = parseAnd ()

                while peek () = ValueSome TOr do
                    pos <- pos + 1
                    let right = parseAnd ()
                    left <- left || right

                left

            and parseAnd () =
                let mutable left = parseUnary ()

                while peek () = ValueSome TAnd do
                    pos <- pos + 1
                    let right = parseUnary ()
                    left <- left && right

                left

            and parseUnary () =
                match peek () with
                | ValueSome TBang ->
                    pos <- pos + 1
                    not (parseUnary ())
                | _ -> parsePrimary ()

            and parseAtomString () =
                match peek () with
                | ValueSome(TWord w) ->
                    pos <- pos + 1

                    if
                        (String.Equals(w, "Exists", StringComparison.OrdinalIgnoreCase)
                         || String.Equals(w, "HasTrailingSlash", StringComparison.OrdinalIgnoreCase))
                        && peek () = ValueSome TLParen
                    then
                        pos <- pos + 1
                        let arg = parseAtomString ()
                        if peek () = ValueSome TRParen then pos <- pos + 1

                        if String.Equals(w, "Exists", StringComparison.OrdinalIgnoreCase) then
                            if String.IsNullOrWhiteSpace arg then
                                "false"
                            else
                                let full =
                                    Path.GetFullPath(
                                        Path.Combine(projectDir, arg.Replace('/', Path.DirectorySeparatorChar))
                                    )

                                if File.Exists full || Directory.Exists full then "true" else "false"
                        else if arg.EndsWith("/") || arg.EndsWith("\\") then
                            "true"
                        else
                            "false"
                    else
                        w
                | _ -> ""

            and parsePrimary () =
                match peek () with
                | ValueSome TLParen ->
                    pos <- pos + 1
                    let inner = parseOr ()
                    if peek () = ValueSome TRParen then pos <- pos + 1
                    inner
                | _ ->
                    let lhs = parseAtomString ()

                    match peek () with
                    | ValueSome TEq ->
                        pos <- pos + 1
                        let rhs = parseAtomString ()
                        String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase)
                    | ValueSome TNeq ->
                        pos <- pos + 1
                        let rhs = parseAtomString ()
                        not (String.Equals(lhs, rhs, StringComparison.OrdinalIgnoreCase))
                    | _ ->
                        String.Equals(lhs.Trim(), "true", StringComparison.OrdinalIgnoreCase)

            if toks.Length = 0 then true else parseOr ()
        with _ ->
            true

    /// Evaluates MSBuild imports, ordered ProjectItems (`Compile`, `Content`, `None`), and cache glob probes in a single pass.
    /// Kept non-inlined so `toolsPath ()` has registered MSBuild assemblies before JIT compilation.
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)>]
    let private evaluateProjectDetails
        (fsproj: string)
        (properties: (string * string) list)
        (sourceFiles: string list)
        (generatedFiles: string list)
        : string list * ProjectItem list * (string * string * string) list =
        use collection = new Microsoft.Build.Evaluation.ProjectCollection(dict properties)
        let project = collection.LoadProject fsproj

        let evaluatedImports =
            [ for i in project.Imports do
                  i.ImportedProject.FullPath

              match project.GetPropertyValue "ProjectAssetsFile" with
              | "" -> ()
              | assets -> assets ]

        let metaEscape = "\u001F_RFS_ITEM_META_("

        let expandExpr (raw: string) : string =
            if String.IsNullOrEmpty raw then
                ""
            else
                let protectedStr = raw.Replace("%(", metaEscape, StringComparison.Ordinal)
                let expanded = project.ExpandString protectedStr
                expanded.Replace(metaEscape, "%(", StringComparison.Ordinal)

        let caseSensitive = Glob.defaultCaseSensitive ()
        let comparer = if caseSensitive then StringComparer.Ordinal else StringComparer.OrdinalIgnoreCase
        let pathCmp = if caseSensitive then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
        let fsprojFull = Path.GetFullPath fsproj
        let projectDir = Path.GetDirectoryName fsprojFull

        let evalCond (rawCond: string) : bool =
            if String.IsNullOrWhiteSpace rawCond then
                true
            else
                evalExpandedCondition projectDir (project.ExpandString rawCond)

        let objPrefix =
            let rawInt = project.GetPropertyValue "BaseIntermediateOutputPath"

            let objRoot =
                if String.IsNullOrWhiteSpace rawInt then
                    objDir fsprojFull
                else
                    Path.GetFullPath(Path.Combine(projectDir, rawInt))

            objRoot.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
            + string Path.DirectorySeparatorChar

        let defaultObjPrefix =
            (Path.GetFullPath(objDir fsprojFull)).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
            + string Path.DirectorySeparatorChar

        let isUnderObj (fullPath: string) =
            fullPath.StartsWith(objPrefix, pathCmp) || fullPath.StartsWith(defaultObjPrefix, pathCmp)

        let normSources = sourceFiles |> List.map Path.GetFullPath
        let normGenerated = generatedFiles |> List.map Path.GetFullPath
        let allCompileSet = HashSet<string>(normSources @ normGenerated, comparer)

        let compileBeforeSet =
            HashSet<string>(
                [ for pi in project.GetItems "CompileBefore" do
                      if not (String.IsNullOrWhiteSpace pi.EvaluatedInclude) then
                          yield Path.GetFullPath(Path.Combine(projectDir, pi.EvaluatedInclude)) ],
                comparer
            )

        let compileAfterSet =
            HashSet<string>(
                [ for pi in project.GetItems "CompileAfter" do
                      if not (String.IsNullOrWhiteSpace pi.EvaluatedInclude) then
                          yield Path.GetFullPath(Path.Combine(projectDir, pi.EvaluatedInclude)) ],
                comparer
            )

        let embeddedSet =
            HashSet<string>(
                [ for pi in project.GetItems "EmbeddedResource" do
                      if not (String.IsNullOrWhiteSpace pi.EvaluatedInclude) then
                          yield Path.GetFullPath(Path.Combine(projectDir, pi.EvaluatedInclude)) ],
                comparer
            )

        let slot1Before = normSources |> List.takeWhile (fun f -> compileBeforeSet.Contains f)
        let remAfterBefore = normSources |> List.skip slot1Before.Length

        let slot3After =
            remAfterBefore
            |> List.rev
            |> List.takeWhile (fun f -> not (compileBeforeSet.Contains f) && compileAfterSet.Contains f)
            |> List.rev

        let middleArr =
            remAfterBefore
            |> List.take (remAfterBefore.Length - slot3After.Length)
            |> Array.ofList

        let isFalse (v: string) = String.Equals(v.Trim(), "false", StringComparison.OrdinalIgnoreCase)
        let isTrue (v: string) = String.Equals(v.Trim(), "true", StringComparison.OrdinalIgnoreCase)

        let enableDefaultItems = not (isFalse (project.GetPropertyValue "EnableDefaultItems"))
        let enableDefaultContent = enableDefaultItems && not (isFalse (project.GetPropertyValue "EnableDefaultContentItems"))
        let enableDefaultNone = enableDefaultItems && isTrue (project.GetPropertyValue "EnableDefaultNoneItems")

        let probes = ResizeArray<string * string * string>()
        let probesSeen = HashSet<string>(StringComparer.Ordinal)

        let addProbe (bDir: string) (inc: string) (exc: string) =
            if not (String.IsNullOrWhiteSpace inc) then
                let key = bDir + "\u0000" + inc + "\u0000" + exc

                if probesSeen.Add key then
                    probes.Add((bDir, inc, exc))

        let standardExcludes =
            String.Join(
                ";",
                [ project.GetPropertyValue "DefaultItemExcludes"
                  project.GetPropertyValue "DefaultExcludesInProjectFolder"
                  "bin/**"
                  "obj/**"
                  ".git/**"
                  ".vs/**"
                  ".idea/**"
                  "node_modules/**"
                  "**/.git/**"
                  "**/.vs/**"
                  "**/.idea/**"
                  "**/node_modules/**"
                  "*.fsproj"
                  "*.sln"
                  "*.slnx"
                  "*.user" ]
                |> List.filter (String.IsNullOrWhiteSpace >> not)
            )

        let mutable contentList: Glob.EvaluatedItem list =
            if enableDefaultContent then
                let seenImp = HashSet<string>(comparer)

                [ for pi in project.GetItems "Content" do
                      if pi.IsImported && not (String.IsNullOrWhiteSpace pi.EvaluatedInclude) then
                          let full = Path.GetFullPath(Path.Combine(projectDir, pi.EvaluatedInclude))

                          if File.Exists full && seenImp.Add full then
                              addProbe projectDir pi.EvaluatedInclude ""

                              yield
                                  { Glob.FullPath = full
                                    Glob.RecursiveDir = pi.GetMetadataValue "RecursiveDir"
                                    Glob.Metadata = [ for m in pi.DirectMetadata -> m.Name, m.EvaluatedValue ] } ]
            else
                []

        let mutable noneList: Glob.EvaluatedItem list =
            if enableDefaultNone then
                let defaultNoneExcludes =
                    standardExcludes + ";**/*.fs;**/*.fsi;**/*.fsx;**/*.fsscript;**/*.resx"

                addProbe projectDir "**/*" defaultNoneExcludes

                Glob.evaluateDisk
                    caseSensitive
                    projectDir
                    [ Glob.Include("**/*", Some standardExcludes, [])
                      Glob.Remove "**/*.fs;**/*.fsi;**/*.fsx;**/*.fsscript;**/*.resx" ]
            else
                []

        let explicitContentPaths = HashSet<string>(comparer)
        let explicitNonePaths = HashSet<string>(comparer)
        let compileMeta = Dictionary<string, string * (string * string) list>(comparer)
        let slot2Timeline = ResizeArray<Choice<string, ProjectItemKind * string>>()
        let mutable compileCursor = 0

        let reservedAttrs =
            HashSet<string>(
                [ "Include"
                  "Exclude"
                  "Remove"
                  "Update"
                  "Condition"
                  "KeepMetadata"
                  "RemoveMetadata"
                  "KeepDuplicates" ],
                StringComparer.OrdinalIgnoreCase
            )

        let readMetadata (el: XElement) : (string * string) list =
            let mutable meta: (string * string) list = []

            for attr in el.Attributes() do
                let name = attr.Name.LocalName

                if not (reservedAttrs.Contains name) then
                    meta <- Glob.mergeMetadata meta [ name, expandExpr attr.Value ]

            for child in el.Elements() do
                let condAttr = child.Attribute(XName.Get "Condition")
                let condStr = if isNull condAttr then "" else condAttr.Value

                if evalCond condStr then
                    meta <- Glob.mergeMetadata meta [ child.Name.LocalName, expandExpr child.Value ]

            meta

        let getAttr (name: string) (el: XElement) : string =
            let a = el.Attribute(XName.Get name)
            if isNull a then "" else expandExpr a.Value

        let hasWildcard (s: string) = s.IndexOfAny([| '*'; '?' |]) >= 0

        let applyNonCompileElement (kind: ProjectItemKind) (el: XElement) =
            let inc = getAttr "Include" el
            let exc = getAttr "Exclude" el
            let rem = getAttr "Remove" el
            let upd = getAttr "Update" el
            let meta = readMetadata el

            let currentList () =
                match kind with
                | ProjectItemKind.Content -> contentList
                | _ -> noneList

            let setList (next: Glob.EvaluatedItem list) =
                match kind with
                | ProjectItemKind.Content -> contentList <- next
                | _ -> noneList <- next

            let explicitSet =
                match kind with
                | ProjectItemKind.Content -> explicitContentPaths
                | _ -> explicitNonePaths

            if not (String.IsNullOrWhiteSpace rem) then
                addProbe projectDir rem ""
                let next, _ = Glob.applyOpDisk caseSensitive projectDir (currentList ()) (Glob.Remove rem)
                let surviving = HashSet<string>(next |> List.map _.FullPath, comparer)

                explicitSet.RemoveWhere(fun p -> not (surviving.Contains p)) |> ignore
                setList next

            if not (String.IsNullOrWhiteSpace inc) then
                addProbe projectDir inc exc
                let excOpt = if String.IsNullOrWhiteSpace exc then None else Some exc
                // If an item was already in the implicit SDK pool (not yet explicitly declared), promote it to this explicit slot
                let expanded = Glob.expandDisk caseSensitive projectDir inc excOpt
                let mutable working = currentList ()

                for full, recDir in expanded do
                    if not (explicitSet.Contains full) then
                        explicitSet.Add full |> ignore
                        slot2Timeline.Add(Choice2Of2(kind, full))

                        let existingIdx =
                            working |> List.tryFindIndex (fun e -> comparer.Equals(e.FullPath, full))

                        match existingIdx with
                        | Some idx ->
                            working <-
                                working
                                |> List.mapi (fun i e ->
                                    if i = idx then
                                        { Glob.FullPath = full
                                          Glob.RecursiveDir = recDir
                                          Glob.Metadata = Glob.mergeMetadata e.Metadata meta }
                                    else
                                        e)
                        | None ->
                            working <-
                                working
                                @ [ { Glob.FullPath = full
                                      Glob.RecursiveDir = recDir
                                      Glob.Metadata = meta } ]

                setList working

            if not (String.IsNullOrWhiteSpace upd) then
                if hasWildcard upd then
                    addProbe projectDir upd ""

                let next, _ = Glob.applyOpDisk caseSensitive projectDir (currentList ()) (Glob.Update(upd, meta))
                setList next

        let xdoc = XDocument.Load fsprojFull

        if not (isNull xdoc.Root) then
            for ig in xdoc.Root.Elements() do
                if String.Equals(ig.Name.LocalName, "ItemGroup", StringComparison.Ordinal) then
                    let igCond =
                        let a = ig.Attribute(XName.Get "Condition")
                        if isNull a then "" else a.Value

                    if evalCond igCond then
                        for itemEl in ig.Elements() do
                            let itemCond =
                                let a = itemEl.Attribute(XName.Get "Condition")
                                if isNull a then "" else a.Value

                            if evalCond itemCond then
                                match itemEl.Name.LocalName with
                                | "Content" -> applyNonCompileElement ProjectItemKind.Content itemEl
                                | "None" -> applyNonCompileElement ProjectItemKind.NoneItem itemEl
                                | "EmbeddedResource" ->
                                    let rem = getAttr "Remove" itemEl
                                    let inc = getAttr "Include" itemEl
                                    let exc = getAttr "Exclude" itemEl

                                    if not (String.IsNullOrWhiteSpace rem) then
                                        addProbe projectDir rem ""

                                        embeddedSet.RemoveWhere(fun p ->
                                            fst (Glob.matchExpressions caseSensitive projectDir rem p))
                                        |> ignore

                                    if not (String.IsNullOrWhiteSpace inc) then
                                        addProbe projectDir inc exc
                                        let excOpt = if String.IsNullOrWhiteSpace exc then None else Some exc

                                        for full, _ in Glob.expandDisk caseSensitive projectDir inc excOpt do
                                            embeddedSet.Add full |> ignore
                                | "CompileBefore"
                                | "CompileAfter" ->
                                    let inc = getAttr "Include" itemEl
                                    let exc = getAttr "Exclude" itemEl
                                    let upd = getAttr "Update" itemEl
                                    let meta = readMetadata itemEl

                                    if not (String.IsNullOrWhiteSpace inc) then
                                        if hasWildcard inc || hasWildcard exc then
                                            addProbe projectDir inc exc

                                        let excOpt = if String.IsNullOrWhiteSpace exc then None else Some exc

                                        for full, recDir in Glob.expandDisk caseSensitive projectDir inc excOpt do
                                            let prevRec, prevMeta =
                                                match compileMeta.TryGetValue full with
                                                | true, v -> v
                                                | _ -> "", []

                                            let useRec = if recDir <> "" then recDir else prevRec
                                            compileMeta[full] <- (useRec, Glob.mergeMetadata prevMeta meta)

                                    if not (String.IsNullOrWhiteSpace upd) then
                                        for f in normSources do
                                            let matched, dsRec = Glob.matchExpressions caseSensitive projectDir upd f

                                            if matched then
                                                let prevRec, prevMeta =
                                                    match compileMeta.TryGetValue f with
                                                    | true, v -> v
                                                    | _ -> "", []

                                                let useRec =
                                                    match dsRec with
                                                    | ValueSome r -> r
                                                    | ValueNone -> prevRec

                                                compileMeta[f] <- (useRec, Glob.mergeMetadata prevMeta meta)
                                | "Compile" ->
                                    let inc = getAttr "Include" itemEl
                                    let exc = getAttr "Exclude" itemEl
                                    let rem = getAttr "Remove" itemEl
                                    let upd = getAttr "Update" itemEl
                                    let meta = readMetadata itemEl

                                    if hasWildcard rem then
                                        addProbe projectDir rem ""

                                    if not (String.IsNullOrWhiteSpace inc) then
                                        if hasWildcard inc || hasWildcard exc then
                                            addProbe projectDir inc exc

                                        let excOpt = if String.IsNullOrWhiteSpace exc then None else Some exc
                                        let expanded = Glob.expandDisk caseSensitive projectDir inc excOpt

                                        for full, recDir in expanded do
                                            let prevRec, prevMeta =
                                                match compileMeta.TryGetValue full with
                                                | true, v -> v
                                                | _ -> "", []

                                            let useRec = if recDir <> "" then recDir else prevRec
                                            compileMeta[full] <- (useRec, Glob.mergeMetadata prevMeta meta)

                                            let mutable foundIdx = -1
                                            let mutable k = compileCursor

                                            while k < middleArr.Length && foundIdx < 0 do
                                                if comparer.Equals(middleArr[k], full) then
                                                    foundIdx <- k

                                                k <- k + 1

                                            if foundIdx >= 0 then
                                                for m = compileCursor to foundIdx do
                                                    slot2Timeline.Add(Choice1Of2 middleArr[m])

                                                compileCursor <- foundIdx + 1

                                    if not (String.IsNullOrWhiteSpace upd) then
                                        for f in normSources do
                                            let matched, dsRec = Glob.matchExpressions caseSensitive projectDir upd f

                                            if matched then
                                                let prevRec, prevMeta =
                                                    match compileMeta.TryGetValue f with
                                                    | true, v -> v
                                                    | _ -> "", []

                                                let useRec =
                                                    match dsRec with
                                                    | ValueSome r -> r
                                                    | ValueNone -> prevRec

                                                compileMeta[f] <- (useRec, Glob.mergeMetadata prevMeta meta)
                                | _ -> ()

        while compileCursor < middleArr.Length do
            slot2Timeline.Add(Choice1Of2 middleArr[compileCursor])
            compileCursor <- compileCursor + 1

        let isVisible (metadata: (string * string) list) : bool =
            metadata
            |> List.forall (fun (k, v) ->
                not (
                    String.Equals(k, "Visible", StringComparison.OrdinalIgnoreCase)
                    && String.Equals(v.Trim(), "false", StringComparison.OrdinalIgnoreCase)
                ))

        // Enforce Cross-Item-Type Exclusivity: Compile > EmbeddedResource > Content > None
        let contentMap = Dictionary<string, Glob.EvaluatedItem>(comparer)

        for ev in contentList do
            if
                not (allCompileSet.Contains ev.FullPath)
                && not (embeddedSet.Contains ev.FullPath)
                && not (isUnderObj ev.FullPath)
                && not (comparer.Equals(ev.FullPath, fsprojFull))
            then
                contentMap[ev.FullPath] <- ev

        // Any file claimed by Content (even if Visible="false") must NEVER fall through into None
        let claimedByContent = HashSet<string>(contentMap.Keys, comparer)
        let noneMap = Dictionary<string, Glob.EvaluatedItem>(comparer)

        for ev in noneList do
            if
                not (allCompileSet.Contains ev.FullPath)
                && not (embeddedSet.Contains ev.FullPath)
                && not (claimedByContent.Contains ev.FullPath)
                && not (isUnderObj ev.FullPath)
                && not (comparer.Equals(ev.FullPath, fsprojFull))
            then
                noneMap[ev.FullPath] <- ev

        let makeCompileItem (full: string) : ProjectItem =
            let recDir, meta =
                match compileMeta.TryGetValue full with
                | true, v -> v
                | _ -> "", []

            { Kind = ProjectItemKind.Compile
              FilePath = full
              VirtualPath = computeVirtualPath projectDir full recDir meta
              Metadata = meta }

        let tryMakeNonCompile (kind: ProjectItemKind) (ev: Glob.EvaluatedItem) : ProjectItem option =
            if not (isVisible ev.Metadata) then
                None
            else
                Some
                    { Kind = kind
                      FilePath = ev.FullPath
                      VirtualPath = computeVirtualPath projectDir ev.FullPath ev.RecursiveDir ev.Metadata
                      Metadata = ev.Metadata }

        let emittedNonCompile = HashSet<string>(comparer)
        let finalItems = ResizeArray<ProjectItem>()

        // Slot 1: CompileBefore
        for f in slot1Before do
            finalItems.Add(makeCompileItem f)

        // Slot 2: Interleaved explicit .fsproj items + middle Compile files
        for entry in slot2Timeline do
            match entry with
            | Choice1Of2 compilePath -> finalItems.Add(makeCompileItem compilePath)
            | Choice2Of2(ProjectItemKind.Content, path) ->
                match contentMap.TryGetValue path with
                | true, ev when emittedNonCompile.Add path ->
                    tryMakeNonCompile ProjectItemKind.Content ev |> Option.iter finalItems.Add
                | _ -> ()
            | Choice2Of2(ProjectItemKind.NoneItem, path) ->
                match noneMap.TryGetValue path with
                | true, ev when emittedNonCompile.Add path ->
                    tryMakeNonCompile ProjectItemKind.NoneItem ev |> Option.iter finalItems.Add
                | _ -> ()
            | Choice2Of2 _ -> ()

        // Slot 3: CompileAfter
        for f in slot3After do
            finalItems.Add(makeCompileItem f)

        // Slot 4: Remaining SDK implicit Content then None, sorted deterministically by VirtualPath
        let sortByVirtualPath (xs: ResizeArray<ProjectItem>) =
            let arr = xs.ToArray()

            Array.sortInPlaceWith
                (fun (a: ProjectItem) (b: ProjectItem) ->
                    let c = StringComparer.OrdinalIgnoreCase.Compare(a.VirtualPath, b.VirtualPath)
                    if c <> 0 then c else StringComparer.Ordinal.Compare(a.VirtualPath, b.VirtualPath))
                arr

            arr

        let remContent = ResizeArray<ProjectItem>()

        for ev in contentMap.Values do
            if emittedNonCompile.Add ev.FullPath then
                tryMakeNonCompile ProjectItemKind.Content ev |> Option.iter remContent.Add

        for item in sortByVirtualPath remContent do
            finalItems.Add item

        let remNone = ResizeArray<ProjectItem>()

        for ev in noneMap.Values do
            if emittedNonCompile.Add ev.FullPath then
                tryMakeNonCompile ProjectItemKind.NoneItem ev |> Option.iter remNone.Add

        for item in sortByVirtualPath remNone do
            finalItems.Add item

        evaluatedImports, List.ofSeq finalItems, List.ofSeq probes

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
                && not (isNull root["GlobProbes"])
                && not (isNull root["GlobHash"])
                && root["Sdk"].GetValue<string>() = sdkOf ()
                && c >= File.GetLastWriteTimeUtc fsproj
                && root["Evaluated"].AsArray()
                   |> Seq.forall (fun f ->
                       let f = f.GetValue<string>()
                       File.Exists f && c >= File.GetLastWriteTimeUtc f)
                && conventionalImports fsproj |> List.forall (fun f -> not (File.Exists f) || c >= File.GetLastWriteTimeUtc f)
                && (let probes =
                        [ for p in root["GlobProbes"].AsArray() ->
                              let arr = p.AsArray()
                              arr[0].GetValue<string>(), arr[1].GetValue<string>(), arr[2].GetValue<string>() ]

                    computeGlobHash probes = root["GlobHash"].GetValue<string>()))
        with _ ->
            false

    let private cacheJson
        (spec: ProjectSpec)
        (evaluatedOpt: (string list * (string * string * string) list) option)
        =
        let root = JsonNode.Parse(toJson spec)

        match evaluatedOpt with
        | Some(evaluated, probes) ->
            root["Sdk"] <- JsonValue.Create(sdkOf ())
            root["Evaluated"] <- JsonArray(evaluated |> List.map (fun x -> JsonValue.Create x :> JsonNode) |> Array.ofList)

            root["GlobProbes"] <-
                JsonArray(
                    probes
                    |> List.map (fun (b, i, e) ->
                        let arr = JsonArray()
                        arr.Add(JsonValue.Create b :> JsonNode)
                        arr.Add(JsonValue.Create i :> JsonNode)
                        arr.Add(JsonValue.Create e :> JsonNode)
                        arr :> JsonNode)
                    |> Array.ofList
                )

            root["GlobHash"] <- JsonValue.Create(computeGlobHash probes)
        | None -> ()

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

    let private loadFresh
        (fsproj: string)
        (properties: (string * string) list)
        : ProjectSpec * (string list * (string * string * string) list) option =
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

            let sourceFiles = p.SourceFiles |> List.filter (isGenerated >> not)
            let generatedFiles = p.SourceFiles |> List.filter isGenerated

            let items, evaluatedOpt =
                try
                    let evaluatedImports, evaluatedItems, probes =
                        evaluateProjectDetails fsproj properties sourceFiles generatedFiles

                    evaluatedItems, Some(evaluatedImports, probes)
                with ex ->
                    eprintfn
                        "[ranvierautocomplete] %s: item/import evaluation failed, falling back to Compile items: %s"
                        fsproj
                        ex.Message

                    let projectDir = Path.GetDirectoryName(Path.GetFullPath fsproj)

                    let fallbackItems =
                        [ for f in sourceFiles ->
                              let full = Path.GetFullPath f

                              { Kind = ProjectItemKind.Compile
                                FilePath = full
                                VirtualPath = computeVirtualPath projectDir full "" []
                                Metadata = [] } ]

                    fallbackItems, None

            let spec =
                { ProjectFile = fsproj
                  SourceFiles = sourceFiles
                  OtherOptions = p.OtherOptions
                  ReferencedProjects = p.ReferencedProjects |> List.map (fun r -> r.ProjectFileName)
                  ReferencedFrameworks = p.ReferencedProjects |> List.map (fun r -> resolvedFramework r.ProjectFileName r.TargetFramework)
                  TargetFramework = p.TargetFramework
                  GeneratedFiles = generatedFiles
                  Items = items }

            spec, evaluatedOpt

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

                let spec, evaluatedOpt = loadFresh fsproj properties
                // The cache is an optimisation: a read-only or contended obj directory must not fail the load.
                let tmp = cache + "." + Guid.NewGuid().ToString("N") + ".tmp"

                try
                    Directory.CreateDirectory(objDir fsproj) |> ignore
                    File.WriteAllText(tmp, cacheJson spec evaluatedOpt)
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
