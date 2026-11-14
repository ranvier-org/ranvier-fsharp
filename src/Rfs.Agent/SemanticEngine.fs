namespace Rfs.Agent

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Server
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker
open Rfs.Lsp
open Rfs.Server

[<NoComparison; NoEquality>]
type internal FileDiskStamp =
    { WriteTicks: int64
      Length: int64
      ContentHash: int }

[<NoComparison; NoEquality>]
type internal FailedProject =
    { Project: string
      Reason: string }

[<Sealed>]
type internal SemanticEngine(workspaceRoot: string, ?explicitProjects: string list) =
    let startUtc = DateTime.UtcNow
    let host = GraphHost.Start $"rfs-%s{WorkspaceRoot.workspaceId workspaceRoot}"

    let dummyRequestSender =
        { new ClientRequestSender with
            member _.Send<'a> (_: string) (_: obj) : Ionide.LanguageServerProtocol.JsonRpc.AsyncLspResult<'a> =
                async.Return(Ok Unchecked.defaultof<'a>) }

    let client = new RanvierClient((fun _ _ -> async.Return(Ok())), dummyRequestSender)
    let workspace = new Workspace(host, client)
    let gate = new SemaphoreSlim(1, 1)

    let diskStamps = ConcurrentDictionary<string, FileDiskStamp>(StringComparer.OrdinalIgnoreCase)
    let promotedByGraph = ConditionalWeakTable<ProjectGraph, ConcurrentDictionary<string, byte>>()

    let maxPromotedPerProject = 24
    let mutable loaded = false
    let mutable coldLoadMs = 0L
    let mutable failedProjects: FailedProject list = []

    let scope: References.Scope =
        { OwnersOf = fun p -> workspace.OwnersNow(Path.GetFullPath p)
          ReferencingProjects = workspace.ReferencingNow
          Projects = workspace.ProjectsNow
          FilesOf = workspace.FilesNow
          IsLoading = workspace.LoadingNow }

    let relPath (p: string) = WorkspaceRoot.relativePath workspaceRoot p

    let resolveInputPath (raw: string) : string =
        let trimmed = raw.Trim().Trim('"')

        if Path.IsPathRooted trimmed then
            Path.GetFullPath trimmed
        else
            let underWs = Path.GetFullPath(Path.Combine(workspaceRoot, trimmed))

            if File.Exists underWs || Directory.Exists underWs then
                underWs
            else
                let underCwd = Path.GetFullPath trimmed
                if File.Exists underCwd || Directory.Exists underCwd then underCwd else underWs

    let computeStamp (fullPath: string) (includeHash: bool) : FileDiskStamp option =
        try
            let fi = FileInfo fullPath

            if not fi.Exists then
                None
            else
                let hash =
                    if includeHash then
                        File.ReadAllText(fullPath).GetHashCode(StringComparison.Ordinal)
                    else
                        0

                Some
                    { WriteTicks = fi.LastWriteTimeUtc.Ticks
                      Length = fi.Length
                      ContentHash = hash }
        with _ ->
            None

    let snapshotAllTrackedFiles () =
        diskStamps.Clear()

        for g in workspace.Projects do
            match workspace.SpecOf g.Spec.ProjectFile with
            | Some spec ->
                computeStamp spec.ProjectFile true
                |> Option.iter (fun s -> diskStamps[Path.GetFullPath spec.ProjectFile] <- s)

                for f in spec.SourceFiles do
                    let fp = Path.GetFullPath f
                    computeStamp fp true |> Option.iter (fun s -> diskStamps[fp] <- s)
            | None -> ()

        for rootCfg in [ "Directory.Build.props"; "Directory.Build.targets"; "Directory.Packages.props"; "global.json" ] do
            let p = Path.Combine(workspaceRoot, rootCfg)

            if File.Exists p then
                computeStamp p true |> Option.iter (fun s -> diskStamps[Path.GetFullPath p] <- s)

    let discoverProjects () : string list =
        match explicitProjects with
        | Some ps when not ps.IsEmpty -> ps |> List.map resolveInputPath |> List.distinct
        | _ ->
            match Environment.GetEnvironmentVariable "RFS_PROJECTS" with
            | env when not (String.IsNullOrWhiteSpace env) ->
                env.Split([| ';'; ',' |], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map resolveInputPath
                |> Array.distinct
                |> List.ofArray
            | _ ->
                let found =
                    WorkspacePeek.peek
                        workspaceRoot
                        4
                        [ ".git"; "node_modules"; "bin"; "obj"; ".worktrees"; "artifacts"; ".vs"; ".idea" ]

                WorkspacePeek.projectsOf found

    let loadCoreAsync (forceReload: bool) =
        task {
            if forceReload || not loaded then
                let sw = Stopwatch.StartNew()
                let projects = discoverProjects ()
                let! results = workspace.Load projects
                sw.Stop()

                failedProjects <-
                    results
                    |> List.choose (function
                        | LoadResult.Failed(p, reason) -> Some { Project = p; Reason = reason }
                        | LoadResult.Loaded _ -> None)

                snapshotAllTrackedFiles ()
                loaded <- true
                coldLoadMs <- sw.ElapsedMilliseconds
        }

    let promoteIfEligible (fullPath: string) =
        if fullPath.EndsWith(".fs", StringComparison.OrdinalIgnoreCase) then
            for g in workspace.OwnersOf fullPath do
                let setForGraph = promotedByGraph.GetOrCreateValue g

                if setForGraph.Count < maxPromotedPerProject && setForGraph.TryAdd(fullPath, 0uy) then
                    g.Promote fullPath

    let syncDiskCoreAsync (focusFiles: string list) =
        task {
            if not loaded then
                do! loadCoreAsync false

            let focusSet = HashSet<string>(StringComparer.OrdinalIgnoreCase)

            for f in focusFiles do
                if not (String.IsNullOrWhiteSpace f) then
                    focusSet.Add(Path.GetFullPath f) |> ignore

            let changes = ResizeArray<string * FileChangeType>()
            let modifiedSources = ResizeArray<string>()

            for KeyValue(path, prev) in diskStamps.ToArray() do
                if not (File.Exists path) then
                    changes.Add(path, FileChangeType.Deleted)
                    diskStamps.TryRemove path |> ignore
                else
                    let isFocus = focusSet.Contains path

                    try
                        let fi = FileInfo path
                        let metaDiffers = fi.LastWriteTimeUtc.Ticks <> prev.WriteTicks || fi.Length <> prev.Length

                        if metaDiffers || isFocus then
                            let text = File.ReadAllText path
                            let h = text.GetHashCode(StringComparison.Ordinal)

                            if metaDiffers || h <> prev.ContentHash then
                                changes.Add(path, FileChangeType.Changed)
                                diskStamps[path] <- { WriteTicks = fi.LastWriteTimeUtc.Ticks; Length = fi.Length; ContentHash = h }

                                if path.EndsWith(".fs", StringComparison.OrdinalIgnoreCase) || path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                                    modifiedSources.Add path
                    with _ ->
                        ()

            // Newly created files passed as focus that weren't in diskStamps yet
            for f in focusSet do
                if not (diskStamps.ContainsKey f) && File.Exists f then
                    changes.Add(f, FileChangeType.Created)
                    computeStamp f true |> Option.iter (fun s -> diskStamps[f] <- s)

                    if f.EndsWith(".fs", StringComparison.OrdinalIgnoreCase) then
                        modifiedSources.Add f

            if changes.Count > 0 then
                do! workspace.FilesChanged(List.ofSeq changes)

                // If a project reload occurred, refresh stamps for any newly added project files
                if changes |> Seq.exists (fun (p, _) -> p.EndsWith(".fsproj", StringComparison.OrdinalIgnoreCase) || p.EndsWith(".props", StringComparison.OrdinalIgnoreCase)) then
                    snapshotAllTrackedFiles ()

            for src in modifiedSources do
                promoteIfEligible src

            for f in focusSet do
                promoteIfEligible f
        }

    let withLock (work: unit -> Task<'T>) : Task<'T> =
        task {
            do! gate.WaitAsync()

            try
                return! work ()
            finally
                gate.Release() |> ignore
        }

    let pickGraphFor (fullPath: string) : ProjectGraph option =
        match workspace.OwnersOf fullPath with
        | g :: _ -> Some g
        | [] -> None

    let readSourceLines (fullPath: string) (cache: Dictionary<string, string[]>) : string[] =
        match cache.TryGetValue fullPath with
        | true, lines -> lines
        | _ ->
            let lines =
                try
                    File.ReadAllLines fullPath
                with _ ->
                    [||]

            cache[fullPath] <- lines
            lines

    let snippetAt (cache: Dictionary<string, string[]>) (file: string) (line1Based: int) : string =
        let lines = readSourceLines (Path.GetFullPath file) cache

        if line1Based >= 1 && line1Based <= lines.Length then
            let s = lines[line1Based - 1].Trim()
            if s.Length > 140 then s.Substring(0, 137) + "..." else s
        else
            ""

    let retryOnMoved (ct: CancellationToken) (op: unit -> Task<References.Answer>) : Task<References.Answer> =
        task {
            let mutable attempt = 0
            let mutable answer = References.Answer.Moved

            while attempt < 3 && (match answer with References.Answer.Moved -> true | References.Answer.Found _ -> false) do
                let! next = op ()
                answer <- next

                if (match answer with References.Answer.Moved -> true | References.Answer.Found _ -> false) && attempt < 2 then
                    do! Task.Delay(50, ct)

                attempt <- attempt + 1

            return answer
        }

    let severityRank =
        function
        | FSharpDiagnosticSeverity.Error -> 0
        | FSharpDiagnosticSeverity.Warning -> 1
        | FSharpDiagnosticSeverity.Info -> 2
        | FSharpDiagnosticSeverity.Hidden -> 3

    let severityLabel =
        function
        | FSharpDiagnosticSeverity.Error -> "error"
        | FSharpDiagnosticSeverity.Warning -> "warning"
        | FSharpDiagnosticSeverity.Info -> "info"
        | FSharpDiagnosticSeverity.Hidden -> "hidden"

    let singleLineMessage (msg: string) =
        let normalized = msg.Replace("\u001d", " ").Replace("\r\n", " ").Replace('\n', ' ').Trim()
        let sb = StringBuilder(normalized.Length)
        let mutable prevSpace = false

        for ch in normalized do
            if ch = ' ' || ch = '\t' then
                if not prevSpace then
                    sb.Append ' ' |> ignore
                    prevSpace <- true
            else
                sb.Append ch |> ignore
                prevSpace <- false

        sb.ToString()

    let diagCode (d: FSharpDiagnostic) =
        if String.IsNullOrWhiteSpace d.ErrorNumberText then
            $"FS%04d{d.ErrorNumber}"
        else
            d.ErrorNumberText

    let lspSeverityOf (sev: string) =
        match sev.ToLowerInvariant() with
        | "error"
        | "errors" -> 0
        | "all" -> 3
        | _ -> 1 // error + warning

    let formatDiagnostics (diags: FSharpDiagnostic seq) (maxSevRank: int) (checkedProjectsCount: int) (elapsedMs: int64) (declChecks: int64) (fileChecks: int64) =
        let filtered =
            diags
            |> Seq.distinctBy (fun d ->
                Path.GetFullPath d.FileName,
                d.StartLine,
                d.StartColumn,
                d.Severity,
                d.ErrorNumber,
                d.Message)
            |> Seq.sortBy (fun d -> severityRank d.Severity, relPath d.FileName, d.StartLine, d.StartColumn)
            |> Array.ofSeq

        let errors = filtered |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Array.length
        let warnings = filtered |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Warning) |> Array.length

        let shown =
            filtered
            |> Array.filter (fun d -> severityRank d.Severity <= maxSevRank)

        let sb = StringBuilder()
        let maxShown = 120

        for i in 0 .. min (shown.Length - 1) (maxShown - 1) do
            let d = shown[i]
            let rFile = relPath d.FileName
            let msg = singleLineMessage d.Message
            sb.AppendLine($"%s{rFile}:%d{d.StartLine}:%d{d.StartColumn + 1} %s{severityLabel d.Severity} %s{diagCode d}: %s{msg}") |> ignore

        if shown.Length > maxShown then
            sb.AppendLine($"... (+%d{shown.Length - maxShown} more diagnostics truncated)") |> ignore

        if not failedProjects.IsEmpty then
            for fp in failedProjects do
                sb.AppendLine($"project-load-failed %s{relPath fp.Project}: %s{singleLineMessage fp.Reason}") |> ignore

        sb.Append($"%d{errors} errors, %d{warnings} warnings (checked %d{checkedProjectsCount} project(s) in %d{elapsedMs}ms, declChecks=%d{declChecks}, fileChecks=%d{fileChecks})")
        |> ignore

        sb.ToString()

    let totals () =
        let graphs = workspace.Projects
        let mutable declChecked = 0L
        let mutable adopted = 0L
        let mutable fileChecked = 0L
        let mutable shifted = 0L
        let mutable totalFiles = 0

        for g in graphs do
            declChecked <- declChecked + g.CheckedCount
            adopted <- adopted + g.AdoptedChecks
            fileChecked <- fileChecked + g.FileLevelChecks
            shifted <- shifted + g.ShiftedRows
            totalFiles <- totalFiles + g.FilesOf.Length

        struct (graphs.Length, totalFiles, declChecked, adopted, fileChecked, shifted)

    let formatLocationLine (cache: Dictionary<string, string[]>) (label: string) (r: range) =
        let rp = relPath r.FileName
        let snip = snippetAt cache r.FileName r.StartLine

        if String.IsNullOrEmpty snip then
            $"%s{label}: %s{rp}:%d{r.StartLine}:%d{r.StartColumn + 1}"
        else
            $"%s{label}: %s{rp}:%d{r.StartLine}:%d{r.StartColumn + 1} | %s{snip}"

    let parseHoverMarkdown (md: string) : string * string option =
        let normalized = md.Replace("\r\n", "\n")
        let trimmed = normalized.Trim()

        if trimmed.StartsWith("```fsharp\n", StringComparison.Ordinal) then
            let closeIdx = trimmed.IndexOf("\n```", 10, StringComparison.Ordinal)

            if closeIdx >= 0 then
                let sigText = trimmed.Substring(10, closeIdx - 10).Trim()
                let rest = trimmed.Substring(closeIdx + 4).Trim()
                let doc = if String.IsNullOrWhiteSpace rest then None else Some(singleLineMessage rest)
                sigText, doc
            else
                trimmed, None
        else
            trimmed, None

    let emitDocSym (relFile: string) (qTrim: string) (cap: int) (sb: StringBuilder) =
        let mutable count = 0

        let rec loop (depth: int) (sym: DocumentSymbol) =
            if count < cap then
                let matches =
                    String.IsNullOrEmpty qTrim
                    || sym.Name.IndexOf(qTrim, StringComparison.OrdinalIgnoreCase) >= 0

                let childMatches =
                    match sym.Children with
                    | Some children -> children.Length > 0
                    | None -> false

                if matches then
                    let indent = String(' ', depth * 2)
                    let l1 = int sym.SelectionRange.Start.Line + 1
                    let c1 = int sym.SelectionRange.Start.Character + 1
                    let endL = int sym.Range.End.Line + 1
                    sb.AppendLine($"%s{indent}%A{sym.Kind} %s{sym.Name} (%s{relFile}:%d{l1}:%d{c1}-%d{endL})") |> ignore
                    count <- count + 1

                if matches || childMatches then
                    match sym.Children with
                    | Some children ->
                        for ch in children do
                            loop (depth + 1) ch
                    | None -> ()

        loop, (fun () -> count)

    member _.WorkspaceRoot = workspaceRoot
    member _.ColdLoadMs = coldLoadMs

    member _.EnsureLoadedAsync() : Task =
        withLock (fun () -> loadCoreAsync false)

    member _.StatusJson(reloadFirst: bool, daemonPid: int) : Task<JsonObject> =
        withLock (fun () ->
            task {
                if reloadFirst then
                    do! loadCoreAsync true
                elif not loaded then
                    do! loadCoreAsync false
                else
                    do! syncDiskCoreAsync []

                let struct (projCount, fileCount, declChecked, adopted, fileChecked, shifted) = totals ()
                use self = Process.GetCurrentProcess()
                let workingSetMb = Math.Round(float self.WorkingSet64 / (1024.0 * 1024.0), 1)
                let uptimeSec = Math.Round((DateTime.UtcNow - startUtc).TotalSeconds, 1)

                let projNames = JsonArray()

                for g in workspace.Projects do
                    projNames.Add(JsonValue.Create(relPath g.Spec.ProjectFile))

                let failedArr = JsonArray()

                for fp in failedProjects do
                    let fo = JsonObject()
                    fo["project"] <- JsonValue.Create(relPath fp.Project)
                    fo["reason"] <- JsonValue.Create fp.Reason
                    failedArr.Add fo

                let promotedCount =
                    workspace.Projects
                    |> List.sumBy (fun g ->
                        match promotedByGraph.TryGetValue g with
                        | true, d -> d.Count
                        | _ -> 0)

                let o = JsonObject()
                o["workspaceRoot"] <- JsonValue.Create workspaceRoot
                o["workspaceId"] <- JsonValue.Create(WorkspaceRoot.workspaceId workspaceRoot)
                o["daemonPid"] <- JsonValue.Create daemonPid
                o["uptimeSeconds"] <- JsonValue.Create uptimeSec
                o["workingSetMb"] <- JsonValue.Create workingSetMb
                o["coldLoadMs"] <- JsonValue.Create coldLoadMs
                o["loadedProjects"] <- JsonValue.Create projCount
                o["projects"] <- projNames
                o["failedProjects"] <- failedArr
                o["sourceFiles"] <- JsonValue.Create fileCount
                o["promotedFiles"] <- JsonValue.Create promotedCount
                o["declCheckedCount"] <- JsonValue.Create declChecked
                o["adoptedChecks"] <- JsonValue.Create adopted
                o["fileLevelChecks"] <- JsonValue.Create fileChecked
                o["shiftedRows"] <- JsonValue.Create shifted
                return o
            })

    member this.StatusText(reloadFirst: bool, daemonPid: int) : Task<string> =
        task {
            let! o = this.StatusJson(reloadFirst, daemonPid)
            let wsId = o["workspaceId"].GetValue<string>()
            let uptime = o["uptimeSeconds"].GetValue<float>()
            let rss = o["workingSetMb"].GetValue<float>()
            let loadedProjs = o["loadedProjects"].GetValue<int>()
            let srcFiles = o["sourceFiles"].GetValue<int>()
            let promoted = o["promotedFiles"].GetValue<int>()
            let declCnt = o["declCheckedCount"].GetValue<int64>()
            let adoptedCnt = o["adoptedChecks"].GetValue<int64>()
            let fileCnt = o["fileLevelChecks"].GetValue<int64>()
            let shiftedCnt = o["shiftedRows"].GetValue<int64>()
            let sb = StringBuilder()
            sb.AppendLine($"workspace: %s{workspaceRoot} (id=%s{wsId})") |> ignore
            sb.AppendLine($"daemon: pid=%d{daemonPid}, uptime=%.1f{uptime}s, rss=%.1f{rss}MB, coldLoadMs=%d{coldLoadMs}") |> ignore
            sb.AppendLine($"projects: %d{loadedProjs} loaded, %d{failedProjects.Length} failed, %d{srcFiles} files (%d{promoted} promoted)") |> ignore
            sb.Append($"incremental: declChecks=%d{declCnt}, adopted=%d{adoptedCnt}, fileChecks=%d{fileCnt}, shifted=%d{shiftedCnt}") |> ignore

            if not failedProjects.IsEmpty then
                sb.AppendLine() |> ignore
                for fp in failedProjects do
                    sb.AppendLine($"failed %s{relPath fp.Project}: %s{singleLineMessage fp.Reason}") |> ignore

            return sb.ToString()
        }

    member _.CheckAsync(fileOpt: string option, projectOpt: string option, severity: string, downstream: bool, ct: CancellationToken) : Task<Result<string, string>> =
        withLock (fun () ->
            task {
                let targetPathOpt = fileOpt |> Option.map resolveInputPath
                let focus = match targetPathOpt with Some p -> [ p ] | None -> []
                do! syncDiskCoreAsync focus

                let sw = Stopwatch.StartNew()
                let struct (_, _, declBefore, _, fileBefore, _) = totals ()
                let maxSev = lspSeverityOf severity

                match targetPathOpt, projectOpt with
                | Some fullPath, _ ->
                    if not (File.Exists fullPath) then
                        return Error $"File not found: %s{relPath fullPath}"
                    else
                        match workspace.OwnersOf fullPath with
                        | [] ->
                            if fullPath.EndsWith(".fsproj", StringComparison.OrdinalIgnoreCase) then
                                match workspace.Projects |> List.tryFind (fun g -> String.Equals(Path.GetFullPath g.Spec.ProjectFile, fullPath, StringComparison.OrdinalIgnoreCase)) with
                                | None -> return Error $"Project is not loaded: %s{relPath fullPath}"
                                | Some g ->
                                    let targets =
                                        if downstream then g :: workspace.ReferencingProjects g else [ g ]
                                        |> List.distinct

                                    let! dicts = Task.WhenAll(targets |> List.map (fun tg -> tg.AllDiagnostics()))
                                    let all = dicts |> Array.collect (fun d -> d.Values |> Array.concat)
                                    sw.Stop()
                                    let struct (_, _, declAfter, _, fileAfter, _) = totals ()
                                    return Ok(formatDiagnostics all maxSev targets.Length sw.ElapsedMilliseconds (declAfter - declBefore) (fileAfter - fileBefore))
                            else
                                return Error $"File does not belong to any loaded F# project: %s{relPath fullPath}"
                        | owners ->
                            if not downstream && owners.Length = 1 then
                                let g = owners.Head
                                let! fileDiags = g.Diagnostics fullPath
                                sw.Stop()
                                let struct (_, _, declAfter, _, fileAfter, _) = totals ()
                                return Ok(formatDiagnostics fileDiags maxSev 1 sw.ElapsedMilliseconds (declAfter - declBefore) (fileAfter - fileBefore))
                            else
                                let related =
                                    if downstream then
                                        owners @ (owners |> List.collect workspace.ReferencingProjects)
                                    else
                                        owners
                                    |> List.distinct

                                let! dicts = Task.WhenAll(related |> List.map (fun tg -> tg.AllDiagnostics()))
                                let all = dicts |> Array.collect (fun d -> d.Values |> Array.concat)
                                sw.Stop()
                                let struct (_, _, declAfter, _, fileAfter, _) = totals ()
                                return Ok(formatDiagnostics all maxSev related.Length sw.ElapsedMilliseconds (declAfter - declBefore) (fileAfter - fileBefore))

                | None, Some projQuery ->
                    let resolvedProj = resolveInputPath projQuery

                    let matched =
                        workspace.Projects
                        |> List.tryFind (fun g ->
                            String.Equals(Path.GetFullPath g.Spec.ProjectFile, resolvedProj, StringComparison.OrdinalIgnoreCase)
                            || String.Equals(Path.GetFileNameWithoutExtension g.Spec.ProjectFile, projQuery, StringComparison.OrdinalIgnoreCase)
                            || String.Equals(Path.GetFileName g.Spec.ProjectFile, projQuery, StringComparison.OrdinalIgnoreCase))

                    match matched with
                    | None -> return Error $"Project not found among loaded projects: %s{projQuery}"
                    | Some g ->
                        let targets = if downstream then g :: workspace.ReferencingProjects g else [ g ]
                        let targets = List.distinct targets
                        let! dicts = Task.WhenAll(targets |> List.map (fun tg -> tg.AllDiagnostics()))
                        let all = dicts |> Array.collect (fun d -> d.Values |> Array.concat)
                        sw.Stop()
                        let struct (_, _, declAfter, _, fileAfter, _) = totals ()
                        return Ok(formatDiagnostics all maxSev targets.Length sw.ElapsedMilliseconds (declAfter - declBefore) (fileAfter - fileBefore))

                | None, None ->
                    let graphs = workspace.Projects
                    let! dicts = Task.WhenAll(graphs |> List.map (fun g -> g.AllDiagnostics()))
                    let all = dicts |> Array.collect (fun d -> d.Values |> Array.concat)
                    sw.Stop()
                    let struct (_, _, declAfter, _, fileAfter, _) = totals ()
                    return Ok(formatDiagnostics all maxSev graphs.Length sw.ElapsedMilliseconds (declAfter - declBefore) (fileAfter - fileBefore))
            })

    member _.SymbolAsync(rawFile: string, line1Based: int, col1BasedOpt: int option, symbolOpt: string option, ct: CancellationToken) : Task<Result<string, string>> =
        withLock (fun () ->
            task {
                let fullPath = resolveInputPath rawFile

                if not (File.Exists fullPath) then
                    return Error $"File not found: %s{relPath fullPath}"
                else
                    do! syncDiskCoreAsync [ fullPath ]

                    match pickGraphFor fullPath with
                    | None -> return Error $"File is not in any loaded F# project: %s{relPath fullPath}"
                    | Some graph ->
                        let text = File.ReadAllText fullPath

                        match PositionResolver.resolve text line1Based col1BasedOpt symbolOpt with
                        | Error e -> return Error e
                        | Ok resolved ->
                            let! hoverOpt = Hover.at workspace.OwnersOf graph fullPath resolved.Line0 resolved.Char0 ct
                            let! defOpt = Definition.at workspace.OwnersOf graph fullPath resolved.Line0 resolved.Char0 ct
                            let! typeDefOpt = TypeDefinition.at workspace.OwnersOf graph fullPath resolved.Line0 resolved.Char0 ct
                            let! implRes = Implementation.find scope graph fullPath resolved.Line0 resolved.Char0 ct

                            let cache = Dictionary<string, string[]>(StringComparer.OrdinalIgnoreCase)
                            let sb = StringBuilder()
                            let qualName = String.Join(".", resolved.Names)
                            sb.AppendLine($"symbol: %s{qualName} (%s{relPath fullPath}:%d{resolved.Line1}:%d{resolved.Col1})") |> ignore

                            match hoverOpt with
                            | Some md ->
                                let sigText, docOpt = parseHoverMarkdown md
                                sb.AppendLine($"type: %s{singleLineMessage sigText}") |> ignore

                                match docOpt with
                                | Some doc -> sb.AppendLine($"doc: %s{doc}") |> ignore
                                | None -> ()
                            | None ->
                                sb.AppendLine("type: (no type tooltip at position)") |> ignore

                            match defOpt with
                            | Some d -> sb.AppendLine(formatLocationLine cache "def" d) |> ignore
                            | None -> ()

                            match typeDefOpt with
                            | Some td ->
                                let sameAsDef =
                                    match defOpt with
                                    | Some d ->
                                        String.Equals(Path.GetFullPath d.FileName, Path.GetFullPath td.FileName, StringComparison.OrdinalIgnoreCase)
                                        && d.StartLine = td.StartLine
                                        && d.StartColumn = td.StartColumn
                                    | None -> false

                                if not sameAsDef then
                                    sb.AppendLine(formatLocationLine cache "typedef" td) |> ignore
                            | None -> ()

                            match implRes with
                            | Ok impls when impls.Length > 0 ->
                                sb.AppendLine($"impls (%d{impls.Length}):") |> ignore

                                for i in 0 .. min (impls.Length - 1) 9 do
                                    sb.AppendLine("  " + formatLocationLine cache "impl" impls[i]) |> ignore

                                if impls.Length > 10 then
                                    sb.AppendLine($"  ... (+%d{impls.Length - 10} more)") |> ignore
                            | _ -> ()

                            return Ok(sb.ToString().TrimEnd())
            })

    member _.ReferencesAsync
        (rawFile: string, line1Based: int, col1BasedOpt: int option, symbolOpt: string option, mode: string, maxResults: int, ct: CancellationToken)
        : Task<Result<string, string>> =
        withLock (fun () ->
            task {
                let fullPath = resolveInputPath rawFile

                if not (File.Exists fullPath) then
                    return Error $"File not found: %s{relPath fullPath}"
                else
                    do! syncDiskCoreAsync [ fullPath ]

                    match pickGraphFor fullPath with
                    | None -> return Error $"File is not in any loaded F# project: %s{relPath fullPath}"
                    | Some graph ->
                        let text = File.ReadAllText fullPath

                        match PositionResolver.resolve text line1Based col1BasedOpt symbolOpt with
                        | Error e -> return Error e
                        | Ok resolved ->
                            let sw = Stopwatch.StartNew()
                            let cache = Dictionary<string, string[]>(StringComparer.OrdinalIgnoreCase)
                            let sb = StringBuilder()
                            let modeNorm = mode.Trim().ToLowerInvariant()
                            let cap = if maxResults <= 0 then 100 else min maxResults 500

                            if modeNorm = "incoming" || modeNorm = "outgoing" then
                                let! preparedOpt = CallHierarchy.prepare scope graph fullPath resolved.Line0 resolved.Char0 ct

                                match preparedOpt with
                                | None
                                | Some [||] ->
                                    let symName = String.Join(".", resolved.Names)
                                    return Error $"Symbol '%s{symName}' is not callable for call hierarchy."
                                | Some items ->
                                    let target = items[0]

                                    if modeNorm = "incoming" then
                                        let! incomingOpt = CallHierarchy.incoming scope graph target ct
                                        sw.Stop()

                                        match incomingOpt with
                                        | None -> return Error "Call hierarchy moved while reading workspace."
                                        | Some calls ->
                                            let limited = calls |> Array.truncate cap

                                            for c in limited do
                                                let callerPath = Paths.ofUri c.From.Uri
                                                let l1 = int c.From.SelectionRange.Start.Line + 1
                                                let c1 = int c.From.SelectionRange.Start.Character + 1
                                                let site =
                                                    c.FromRanges
                                                    |> Array.tryHead
                                                    |> Option.map (fun fr -> $"{int fr.Start.Line + 1}:{int fr.Start.Character + 1}")
                                                    |> Option.defaultValue $"{l1}:{c1}"
                                                sb.AppendLine($"%s{relPath callerPath}:%s{site} [caller:%s{c.From.Name}] %s{snippetAt cache callerPath l1}") |> ignore

                                            if calls.Length > cap then
                                                sb.AppendLine($"... (+%d{calls.Length - cap} more callers)") |> ignore

                                            sb.Append($"%d{calls.Length} incoming caller(s) to %s{target.Name} (%d{sw.ElapsedMilliseconds}ms)") |> ignore
                                            return Ok(sb.ToString())
                                    else
                                        let! outgoingOpt = CallHierarchy.outgoing scope graph target ct
                                        sw.Stop()

                                        match outgoingOpt with
                                        | None -> return Error "Call hierarchy moved while reading workspace."
                                        | Some calls ->
                                            let limited = calls |> Array.truncate cap

                                            for c in limited do
                                                let calleePath = Paths.ofUri c.To.Uri
                                                let l1 = int c.To.SelectionRange.Start.Line + 1
                                                let c1 = int c.To.SelectionRange.Start.Character + 1
                                                let site =
                                                    c.FromRanges
                                                    |> Array.tryHead
                                                    |> Option.map (fun fr -> $"{int fr.Start.Line + 1}:{int fr.Start.Character + 1}")
                                                    |> Option.defaultValue $"{l1}:{c1}"
                                                sb.AppendLine($"%s{relPath fullPath}:%s{site} -> %s{c.To.Name} (%s{relPath calleePath}:%d{l1}:%d{c1})") |> ignore

                                            if calls.Length > cap then
                                                sb.AppendLine($"... (+%d{calls.Length - cap} more callees)") |> ignore

                                            sb.Append($"%d{calls.Length} outgoing call(s) from %s{target.Name} (%d{sw.ElapsedMilliseconds}ms)") |> ignore
                                            return Ok(sb.ToString())
                            else
                                let includeDef = modeNorm <> "uses"

                                match! retryOnMoved ct (fun () -> References.find scope graph fullPath resolved.Line0 resolved.Char0 ct) with
                                | References.Answer.Moved -> return Error "Workspace files changed concurrently; please retry."
                                | References.Answer.Found uses ->
                                    sw.Stop()

                                    let filtered =
                                        uses
                                        |> List.filter (fun (struct (_, isDef)) -> includeDef || not isDef)
                                        |> List.sortBy (fun (struct (r, _)) -> relPath r.FileName, r.StartLine, r.StartColumn)

                                    let defCount = uses |> List.filter (fun (struct (_, d)) -> d) |> List.length
                                    let useCount = uses.Length - defCount
                                    let fileCount = uses |> List.map (fun (struct (r, _)) -> Path.GetFullPath r.FileName) |> List.distinct |> List.length
                                    let shown = filtered |> List.truncate cap

                                    for struct (r, isDef) in shown do
                                        let tag = if isDef then "def" else "use"
                                        let snip = snippetAt cache r.FileName r.StartLine
                                        sb.AppendLine($"%s{relPath r.FileName}:%d{r.StartLine}:%d{r.StartColumn + 1} [%s{tag}] %s{snip}") |> ignore

                                    if filtered.Length > cap then
                                        sb.AppendLine($"... (+%d{filtered.Length - cap} more)") |> ignore

                                    sb.Append($"%d{uses.Length} references (%d{defCount} def, %d{useCount} uses) across %d{fileCount} file(s) (%d{sw.ElapsedMilliseconds}ms)") |> ignore
                                    return Ok(sb.ToString())
            })

    member _.SearchAsync(queryOpt: string option, fileOpt: string option, maxResults: int, ct: CancellationToken) : Task<Result<string, string>> =
        withLock (fun () ->
            task {
                let cap = if maxResults <= 0 then 50 else min maxResults 500
                let query = queryOpt |> Option.defaultValue ""

                match fileOpt with
                | Some rawFile when not (String.IsNullOrWhiteSpace rawFile) ->
                    let fullPath = resolveInputPath rawFile

                    if not (File.Exists fullPath) then
                        return Error $"File not found: %s{relPath fullPath}"
                    else
                        do! syncDiskCoreAsync [ fullPath ]

                        match pickGraphFor fullPath with
                        | None -> return Error $"File is not in any loaded F# project: %s{relPath fullPath}"
                        | Some graph ->
                            let! docSyms = DocumentSymbols.hierarchical graph fullPath ct
                            let sb = StringBuilder()
                            let qTrim = query.Trim()
                            let relFile = relPath fullPath
                            let emitSym, getCount = emitDocSym relFile qTrim cap sb

                            match docSyms with
                            | Some(U2.C2 syms) ->
                                for s in syms do
                                    emitSym 0 s
                            | Some(U2.C1 infos) ->
                                for info in infos do
                                    if getCount () < cap then
                                        let l1 = int info.Location.Range.Start.Line + 1
                                        let c1 = int info.Location.Range.Start.Character + 1
                                        sb.AppendLine($"%A{info.Kind} %s{info.Name} (%s{relFile}:%d{l1}:%d{c1})") |> ignore
                            | None -> ()

                            if getCount () = 0 && sb.Length = 0 then
                                sb.Append($"No symbols found in %s{relFile}.") |> ignore

                            return Ok(sb.ToString().TrimEnd())

                | _ ->
                    do! syncDiskCoreAsync []
                    let! res = WorkspaceSymbols.search host workspace.ProjectsNow workspace.LoadingNow workspace.FilesNow query ct
                    let sb = StringBuilder()

                    match res with
                    | Some(U2.C2 syms) ->
                        let limited = syms |> Array.truncate cap

                        for sym in limited do
                            let pathStr, l1, c1 =
                                match sym.Location with
                                | U2.C1 loc ->
                                    relPath (Paths.ofUri loc.Uri),
                                    int loc.Range.Start.Line + 1,
                                    int loc.Range.Start.Character + 1
                                | U2.C2 docLoc ->
                                    relPath (Paths.ofUri docLoc.Uri), 1, 1

                            let containerPrefix =
                                match sym.ContainerName with
                                | Some c when not (String.IsNullOrWhiteSpace c) -> c + "."
                                | _ -> ""

                            sb.AppendLine($"%s{pathStr}:%d{l1}:%d{c1} %A{sym.Kind} %s{containerPrefix}%s{sym.Name}") |> ignore

                        if syms.Length > cap then
                            sb.AppendLine($"... (+%d{syms.Length - cap} more matches)") |> ignore

                        sb.Append($"%d{syms.Length} workspace symbol match(es)") |> ignore
                        return Ok(sb.ToString())
                    | Some(U2.C1 infos) ->
                        let limited = infos |> Array.truncate cap

                        for sym in limited do
                            let p = relPath (Paths.ofUri sym.Location.Uri)
                            let l1 = int sym.Location.Range.Start.Line + 1
                            let c1 = int sym.Location.Range.Start.Character + 1
                            sb.AppendLine($"%s{p}:%d{l1}:%d{c1} %A{sym.Kind} %s{sym.Name}") |> ignore

                        sb.Append($"%d{infos.Length} workspace symbol match(es)") |> ignore
                        return Ok(sb.ToString())
                    | None -> return Ok "0 workspace symbol match(es)"
            })

    member _.RenameAsync
        (rawFile: string, line1Based: int, newName: string, col1BasedOpt: int option, symbolOpt: string option, applyEdits: bool, ct: CancellationToken)
        : Task<Result<string, string>> =
        withLock (fun () ->
            task {
                let fullPath = resolveInputPath rawFile

                if not (File.Exists fullPath) then
                    return Error $"File not found: %s{relPath fullPath}"
                else
                    do! syncDiskCoreAsync [ fullPath ]

                    match pickGraphFor fullPath with
                    | None -> return Error $"File is not in any loaded F# project: %s{relPath fullPath}"
                    | Some graph ->
                        let text = File.ReadAllText fullPath

                        match PositionResolver.resolve text line1Based col1BasedOpt symbolOpt with
                        | Error e -> return Error e
                        | Ok resolved ->
                            let sw = Stopwatch.StartNew()

                            match! Rename.rename scope graph fullPath resolved.Line0 resolved.Char0 newName ct with
                            | Error msg -> return Error msg
                            | Ok None -> return Ok "Rename produced 0 edits."
                            | Ok(Some wsEdit) ->
                                let editsByFile =
                                    match wsEdit.Changes with
                                    | Some map ->
                                        [ for KeyValue(uri, edits) in map -> Paths.ofUri uri, edits ]
                                    | None -> []

                                if editsByFile.IsEmpty then
                                    return Ok "Rename produced 0 edits."
                                else
                                    let sb = StringBuilder()
                                    let touchedPaths = ResizeArray<string>()
                                    let mutable totalEdits = 0

                                    for filePath, edits in editsByFile do
                                        totalEdits <- totalEdits + edits.Length
                                        let rp = relPath filePath
                                        let sortedDesc =
                                            edits
                                            |> Array.sortByDescending (fun e -> int e.Range.Start.Line, int e.Range.Start.Character)

                                        if applyEdits then
                                            let origText = File.ReadAllText filePath
                                            let updated =
                                                sortedDesc
                                                |> Array.fold
                                                    (fun acc e ->
                                                        let rfsEdit: Rfs.Lsp.Edit =
                                                            { Range =
                                                                { Start = { Line = int e.Range.Start.Line; Character = int e.Range.Start.Character }
                                                                  End = { Line = int e.Range.End.Line; Character = int e.Range.End.Character } }
                                                              Text = e.NewText }

                                                        Document.apply acc rfsEdit)
                                                    origText

                                            // Preserve BOM presence if original file had UTF-8 BOM; otherwise write UTF-8 without BOM
                                            let rawBytes = File.ReadAllBytes filePath
                                            let hasBom = rawBytes.Length >= 3 && rawBytes[0] = 0xEFuy && rawBytes[1] = 0xBBuy && rawBytes[2] = 0xBFuy
                                            File.WriteAllText(filePath, updated, UTF8Encoding(hasBom))
                                            touchedPaths.Add filePath

                                        let lineNums =
                                            edits
                                            |> Array.map (fun e -> string (int e.Range.Start.Line + 1))
                                            |> Array.distinct

                                        let joinedLines = String.Join(", ", lineNums)
                                        sb.AppendLine($"%s{rp}: %d{edits.Length} edit(s) at line(s) %s{joinedLines}") |> ignore

                                    if applyEdits then
                                        do! syncDiskCoreAsync (List.ofSeq touchedPaths)
                                        let affectedGraphs =
                                            touchedPaths
                                            |> Seq.collect (fun p ->
                                                match workspace.OwnersOf p with
                                                | [] -> []
                                                | owners -> owners @ (owners |> List.collect workspace.ReferencingProjects))
                                            |> Seq.distinct
                                            |> List.ofSeq

                                        let! dicts = Task.WhenAll(affectedGraphs |> List.map (fun tg -> tg.AllDiagnostics()))
                                        let errs =
                                            dicts
                                            |> Array.collect (fun d -> d.Values |> Array.concat)
                                            |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

                                        sw.Stop()
                                        sb.Append($"Renamed to '%s{newName}': %d{totalEdits} edit(s) across %d{editsByFile.Length} file(s), %d{errs.Length} compiler error(s) after check (%d{sw.ElapsedMilliseconds}ms)") |> ignore

                                        if errs.Length > 0 then
                                            sb.AppendLine() |> ignore

                                            for d in Array.truncate 10 errs do
                                                sb.AppendLine($"%s{relPath d.FileName}:%d{d.StartLine}:%d{d.StartColumn + 1} error %s{diagCode d}: %s{singleLineMessage d.Message}") |> ignore
                                    else
                                        sw.Stop()
                                        sb.Append($"Dry-run rename to '%s{newName}': %d{totalEdits} edit(s) across %d{editsByFile.Length} file(s) (%d{sw.ElapsedMilliseconds}ms)") |> ignore

                                    return Ok(sb.ToString().TrimEnd())
            })

    member _.Dispose() =
        try
            workspace.Dispose()
        with _ ->
            ()

        try
            (client :> IDisposable).Dispose()
        with _ ->
            ()

        try
            host.Dispose()
        with _ ->
            ()

        gate.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
