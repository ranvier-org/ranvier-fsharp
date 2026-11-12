module Rfs.Server.Formatting

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.InteropServices
open System.Text
open System.Threading
open System.Threading.Tasks
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type FormattingRunner = string -> string -> bool -> FormattingOptions -> CancellationToken -> Task<string option>

[<RequireQualifiedAccess>]
type private ToolKind =
    | DirectExe of exePath: string
    | DotnetLocalTool of workingDir: string
    | Missing

let private toolCache = ConcurrentDictionary<string, ToolKind>(StringComparer.OrdinalIgnoreCase)

let internal clearToolCacheForTests () : unit = toolCache.Clear()

let private splitLines (text: string) : string[] =
    text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')

let private detectEol (text: string) : string =
    if text.Contains("\r\n", StringComparison.Ordinal) then "\r\n" else "\n"

let private normalizeEol (eol: string) (text: string) : string =
    let lf = text.Replace("\r\n", "\n")
    if eol = "\n" then lf else lf.Replace("\n", eol)

let private hunkToEdit
    (origLines: string[])
    (fmtLines: string[])
    (eol: string)
    (origStart: int)
    (origCount: int)
    (fmtStart: int)
    (fmtCount: int)
    : TextEdit option =
    if origCount = 0 && fmtCount = 0 then
        None
    elif origCount > 0 && fmtCount > 0 then
        let origLast = origStart + origCount - 1
        let newSlice = Array.sub fmtLines fmtStart fmtCount |> String.concat eol

        Some
            { Range =
                { Start = { Line = uint32 origStart; Character = 0u }
                  End =
                    { Line = uint32 origLast
                      Character = uint32 origLines[origLast].Length } }
              NewText = newSlice }
    elif origCount > 0 then
        // Pure line deletion
        let origLast = origStart + origCount - 1

        if origLast + 1 < origLines.Length then
            Some
                { Range =
                    { Start = { Line = uint32 origStart; Character = 0u }
                      End = { Line = uint32 (origLast + 1); Character = 0u } }
                  NewText = "" }
        elif origStart > 0 then
            Some
                { Range =
                    { Start =
                        { Line = uint32 (origStart - 1)
                          Character = uint32 origLines[origStart - 1].Length }
                      End =
                        { Line = uint32 origLast
                          Character = uint32 origLines[origLast].Length } }
                  NewText = "" }
        else
            Some
                { Range =
                    { Start = { Line = 0u; Character = 0u }
                      End =
                        { Line = uint32 origLast
                          Character = uint32 origLines[origLast].Length } }
                  NewText = "" }
    else
        // Pure line insertion (origCount = 0, fmtCount > 0)
        let inserted = Array.sub fmtLines fmtStart fmtCount |> String.concat eol

        if origStart < origLines.Length then
            Some
                { Range =
                    { Start = { Line = uint32 origStart; Character = 0u }
                      End = { Line = uint32 origStart; Character = 0u } }
                  NewText = inserted + eol }
        elif origLines.Length > 0 then
            let lastIdx = origLines.Length - 1
            let pos = { Line = uint32 lastIdx; Character = uint32 origLines[lastIdx].Length }

            Some
                { Range = { Start = pos; End = pos }
                  NewText = eol + inserted }
        else
            let pos = { Line = 0u; Character = 0u }
            Some { Range = { Start = pos; End = pos }; NewText = inserted }

let internal computeDocumentEdits (original: string) (formatted: string) : TextEdit[] =
    let eol = detectEol original
    let normFormatted = normalizeEol eol formatted

    if String.Equals(original, normFormatted, StringComparison.Ordinal) then
        [||]
    else
        let origLines = splitLines original
        let fmtLines = splitLines normFormatted

        let mutable prefix = 0

        while prefix < origLines.Length
              && prefix < fmtLines.Length
              && String.Equals(origLines[prefix], fmtLines[prefix], StringComparison.Ordinal) do
            prefix <- prefix + 1

        let mutable suffix = 0

        while suffix < (origLines.Length - prefix)
              && suffix < (fmtLines.Length - prefix)
              && String.Equals(
                  origLines[origLines.Length - 1 - suffix],
                  fmtLines[fmtLines.Length - 1 - suffix],
                  StringComparison.Ordinal
              ) do
            suffix <- suffix + 1

        let origCount = origLines.Length - prefix - suffix
        let fmtCount = fmtLines.Length - prefix - suffix

        match hunkToEdit origLines fmtLines eol prefix origCount prefix fmtCount with
        | Some edit -> [| edit |]
        | None -> [||]

/// Computes minimal line-diff hunks `(origStart, origCount, fmtStart, fmtCount)` between `origLines` and `fmtLines`.
let private diffLineHunks (origLines: string[]) (fmtLines: string[]) : (int * int * int * int) list =
    let mutable prefix = 0

    while prefix < origLines.Length
          && prefix < fmtLines.Length
          && String.Equals(origLines[prefix], fmtLines[prefix], StringComparison.Ordinal) do
        prefix <- prefix + 1

    let mutable suffix = 0

    while suffix < (origLines.Length - prefix)
          && suffix < (fmtLines.Length - prefix)
          && String.Equals(
              origLines[origLines.Length - 1 - suffix],
              fmtLines[fmtLines.Length - 1 - suffix],
              StringComparison.Ordinal
          ) do
        suffix <- suffix + 1

    let m = origLines.Length - prefix - suffix
    let n = fmtLines.Length - prefix - suffix

    if m = 0 && n = 0 then
        []
    elif m = 0 || n = 0 || int64 m * int64 n > 4_000_000L then
        [ prefix, m, prefix, n ]
    else
        let intern = Dictionary<string, int>(StringComparer.Ordinal)
        let mutable nextId = 1

        let idOf (s: string) =
            match intern.TryGetValue s with
            | true, id -> id
            | false, _ ->
                let id = nextId
                nextId <- nextId + 1
                intern[s] <- id
                id

        let a = Array.init m (fun i -> idOf origLines[prefix + i])
        let b = Array.init n (fun j -> idOf fmtLines[prefix + j])

        let stride = n + 1
        let dp = Array.zeroCreate<int> ((m + 1) * stride)

        for i = m - 1 downto 0 do
            let ai = a[i]
            let row = i * stride
            let nextRow = (i + 1) * stride

            for j = n - 1 downto 0 do
                if ai = b[j] then
                    dp[row + j] <- dp[nextRow + j + 1] + 1
                else
                    let down = dp[nextRow + j]
                    let right = dp[row + j + 1]
                    dp[row + j] <- if down >= right then down else right

        let anchors = ResizeArray<struct (int * int)>()
        let mutable i = 0
        let mutable j = 0

        while i < m && j < n do
            if a[i] = b[j] then
                anchors.Add(struct (prefix + i, prefix + j))
                i <- i + 1
                j <- j + 1
            elif dp[(i + 1) * stride + j] >= dp[i * stride + j + 1] then
                i <- i + 1
            else
                j <- j + 1

        anchors.Add(struct (prefix + m, prefix + n))

        let hunks = ResizeArray<int * int * int * int>()
        let mutable prevOrig = prefix
        let mutable prevFmt = prefix

        for struct (anchorOrig, anchorFmt) in anchors do
            let oc = anchorOrig - prevOrig
            let fc = anchorFmt - prevFmt

            if oc > 0 || fc > 0 then
                hunks.Add(prevOrig, oc, prevFmt, fc)

            prevOrig <- anchorOrig + 1
            prevFmt <- anchorFmt + 1

        hunks |> Seq.toList

let internal computeRangeEdits (original: string) (formatted: string) (lspRange: Range) : TextEdit[] =
    let eol = detectEol original
    let normFormatted = normalizeEol eol formatted

    if String.Equals(original, normFormatted, StringComparison.Ordinal) then
        [||]
    else
        let origLines = splitLines original
        let fmtLines = splitLines normFormatted
        let reqStart = int lspRange.Start.Line

        let reqEnd =
            if lspRange.End.Character = 0u && lspRange.End.Line > lspRange.Start.Line then
                int lspRange.End.Line - 1
            else
                int lspRange.End.Line

        let hunks = diffLineHunks origLines fmtLines

        [| for origStart, origCount, fmtStart, fmtCount in hunks do
               let overlaps =
                   if origCount > 0 then
                       let origLast = origStart + origCount - 1
                       origLast >= reqStart && origStart <= reqEnd
                   else
                       origStart >= reqStart && origStart <= reqEnd

               if overlaps then
                   match hunkToEdit origLines fmtLines eol origStart origCount fmtStart fmtCount with
                   | Some edit -> yield edit
                   | None -> () |]

let private tryFindDotnetLocalFantomas (startDir: string) : string option =
    let mutable current =
        if String.IsNullOrWhiteSpace startDir then
            null
        else
            try
                Path.GetFullPath startDir
            with _ ->
                null

    let mutable found = None

    while not (isNull current) && found.IsNone do
        let manifest = Path.Combine(current, ".config", "dotnet-tools.json")

        if File.Exists manifest then
            try
                let content = File.ReadAllText manifest

                if content.IndexOf("fantomas", StringComparison.OrdinalIgnoreCase) >= 0 then
                    found <- Some current
            with _ ->
                ()

        let parent = Directory.GetParent current
        current <- if isNull parent then null else parent.FullName

    found

let private tryFindOnPath (searchPath: string) : string option =
    if String.IsNullOrWhiteSpace searchPath then
        None
    else
        let candidates =
            if RuntimeInformation.IsOSPlatform OSPlatform.Windows then
                [ "fantomas.exe"; "fantomas.cmd"; "fantomas.bat" ]
            else
                [ "fantomas" ]

        let dirs = searchPath.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
        let mutable hit = None
        let mutable idx = 0

        while idx < dirs.Length && hit.IsNone do
            let dir = dirs[idx].Trim().Trim('"')

            if not (String.IsNullOrWhiteSpace dir) then
                for name in candidates do
                    if hit.IsNone then
                        try
                            let full = Path.Combine(dir, name)

                            if File.Exists full then
                                hit <- Some full
                        with _ ->
                            ()

            idx <- idx + 1

        hit

let private resolveTool (searchPathOverride: string option) (fileDir: string) : ToolKind =
    let normDir =
        if String.IsNullOrWhiteSpace fileDir then
            ""
        else
            try
                Path.GetFullPath fileDir
            with _ ->
                fileDir

    let key =
        match searchPathOverride with
        | Some custom -> $"override:%s{custom}|%s{normDir}"
        | None -> $"env|%s{normDir}"

    toolCache.GetOrAdd(
        key,
        fun _ ->
            match searchPathOverride with
            | Some customPath ->
                match tryFindOnPath customPath with
                | Some exe -> ToolKind.DirectExe exe
                | None -> ToolKind.Missing
            | None ->
                match tryFindDotnetLocalFantomas normDir with
                | Some rootDir -> ToolKind.DotnetLocalTool rootDir
                | None ->
                    let envPath = Environment.GetEnvironmentVariable "PATH" |> Option.ofObj |> Option.defaultValue ""

                    match tryFindOnPath envPath with
                    | Some exe -> ToolKind.DirectExe exe
                    | None -> ToolKind.Missing
    )

let private hasEditorConfigRootTrue (content: string) : bool =
    let mutable inSection = false
    let mutable isRoot = false

    for rawLine in content.Split '\n' do
        let line = rawLine.Trim()

        if not inSection && line.Length > 0 && line[0] <> '#' && line[0] <> ';' then
            if line[0] = '[' then
                inSection <- true
            else
                let eqIdx = line.IndexOf '='

                if eqIdx > 0 then
                    let key = line.Substring(0, eqIdx).Trim()
                    let value = line.Substring(eqIdx + 1).Trim()

                    if
                        String.Equals(key, "root", StringComparison.OrdinalIgnoreCase)
                        && String.Equals(value, "true", StringComparison.OrdinalIgnoreCase)
                    then
                        isRoot <- true

    isRoot

/// Collects ancestor directories from `startDir` upward up to the first `.editorconfig` declaring `root = true`
/// (or filesystem root), returning `(dirFullPath, (cfgContent * hasRootTrue) option) list` ordered leaf-to-root.
let private collectAncestorEditorConfigs (startDir: string) : (string * (string * bool) option) list =
    let mutable current =
        if String.IsNullOrWhiteSpace startDir then
            null
        else
            try
                Path.GetFullPath startDir
            with _ ->
                null

    let acc = ResizeArray<string * (string * bool) option>()
    let mutable stop = false

    while not (isNull current) && not stop do
        let candidate = Path.Combine(current, ".editorconfig")

        let cfgOpt =
            if File.Exists candidate then
                try
                    let content = File.ReadAllText(candidate, Encoding.UTF8)
                    Some(content, hasEditorConfigRootTrue content)
                with _ ->
                    None
            else
                None

        acc.Add(current, cfgOpt)

        match cfgOpt with
        | Some(_, true) -> stop <- true
        | _ ->
            let parent = Directory.GetParent current
            current <- if isNull parent then null else parent.FullName

    acc |> Seq.toList

let internal runWithSearchPath
    (searchPathOverride: string option)
    (filePath: string)
    (sourceText: string)
    (isSignature: bool)
    (options: FormattingOptions)
    (ct: CancellationToken)
    : Task<string option> =
    task {
        ct.ThrowIfCancellationRequested()
        let fileDir = Path.GetDirectoryName filePath |> Option.ofObj |> Option.defaultValue ""

        match resolveTool searchPathOverride fileDir with
        | ToolKind.Missing -> return None
        | tool ->
            let tempDir = Directory.CreateTempSubdirectory "rfs-fantomas-"

            try
                let ancestorsLeafToRoot = collectAncestorEditorConfigs fileDir

                let topCfgIdxOpt =
                    ancestorsLeafToRoot
                    |> List.indexed
                    |> List.choose (fun (idx, (_, cfgOpt)) -> if cfgOpt.IsSome then Some idx else None)
                    |> List.tryLast

                let workLeafDir =
                    match topCfgIdxOpt with
                    | None ->
                        let indentSize = if options.TabSize > 0u then int options.TabSize else 4
                        let indentStyle = if options.InsertSpaces then "space" else "tab"
                        let cfgText = $"root = true\n\n[*.{{fs,fsi}}]\nindent_style = %s{indentStyle}\nindent_size = %d{indentSize}\n"
                        File.WriteAllText(Path.Combine(tempDir.FullName, ".editorconfig"), cfgText, UTF8Encoding(false))
                        tempDir.FullName
                    | Some topIdx ->
                        let chainTopToLeaf = ancestorsLeafToRoot |> List.take (topIdx + 1) |> List.rev
                        let topDir = fst chainTopToLeaf.Head
                        let mutable leafDest = tempDir.FullName

                        chainTopToLeaf
                        |> List.iteri (fun idxFromTop (ancDir, cfgOpt) ->
                            let destDir =
                                if idxFromTop = 0 then
                                    tempDir.FullName
                                else
                                    let rel = Path.GetRelativePath(topDir, ancDir)
                                    Path.Combine(tempDir.FullName, rel)

                            Directory.CreateDirectory destDir |> ignore
                            leafDest <- destDir

                            match cfgOpt with
                            | Some(content, hasRoot) ->
                                let finalContent =
                                    if idxFromTop = 0 && not hasRoot then
                                        "root = true\n" + content
                                    else
                                        content

                                File.WriteAllText(Path.Combine(destDir, ".editorconfig"), finalContent, UTF8Encoding(false))
                            | None -> ())

                        leafDest

                let tempFile = Path.Combine(workLeafDir, if isSignature then "Input.fsi" else "Input.fs")
                File.WriteAllText(tempFile, sourceText, UTF8Encoding(false))

                let psi =
                    match tool with
                    | ToolKind.DirectExe exePath ->
                        if not (RuntimeInformation.IsOSPlatform OSPlatform.Windows) then
                            try
                                let mode = File.GetUnixFileMode exePath
                                File.SetUnixFileMode(exePath, mode ||| UnixFileMode.UserExecute)
                            with _ ->
                                ()

                        let info = ProcessStartInfo(exePath)
                        info.WorkingDirectory <- tempDir.FullName
                        info.ArgumentList.Add tempFile
                        info
                    | ToolKind.DotnetLocalTool rootDir ->
                        let info = ProcessStartInfo("dotnet")
                        info.WorkingDirectory <- rootDir
                        info.Environment.Remove "MSBUILD_EXE_PATH" |> ignore
                        info.Environment.Remove "MSBuildExtensionsPath" |> ignore
                        info.Environment.Remove "MSBuildSDKsPath" |> ignore
                        info.Environment.Remove "DOTNET_HOST_PATH" |> ignore
                        info.Environment["DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER"] <- "1"
                        info.ArgumentList.Add "tool"
                        info.ArgumentList.Add "run"
                        info.ArgumentList.Add "fantomas"
                        info.ArgumentList.Add tempFile
                        info
                    | ToolKind.Missing -> ProcessStartInfo()

                psi.UseShellExecute <- false
                psi.CreateNoWindow <- true
                psi.RedirectStandardOutput <- true
                psi.RedirectStandardError <- true

                let mutable proc: Process = null

                try
                    try
                        proc <- Process.Start psi

                        if isNull proc then
                            return None
                        else
                            use _killOnCancel =
                                ct.Register(fun () ->
                                    try
                                        if not proc.HasExited then
                                            proc.Kill(entireProcessTree = true)
                                    with _ ->
                                        ())

                            let outTask = proc.StandardOutput.ReadToEndAsync()
                            let errTask = proc.StandardError.ReadToEndAsync()
                            do! proc.WaitForExitAsync ct
                            let! _ = outTask
                            let! _ = errTask
                            ct.ThrowIfCancellationRequested()

                            if proc.ExitCode <> 0 || not (File.Exists tempFile) then
                                return None
                            else
                                let formatted = File.ReadAllText(tempFile, Encoding.UTF8)
                                return Some formatted
                    with
                    | :? OperationCanceledException as oce ->
                        if not (isNull proc) then
                            try
                                if not proc.HasExited then
                                    proc.Kill(entireProcessTree = true)
                            with _ ->
                                ()

                        return raise oce
                    | _ ->
                        if not (isNull proc) then
                            try
                                if not proc.HasExited then
                                    proc.Kill(entireProcessTree = true)
                            with _ ->
                                ()

                        return None
                finally
                    if not (isNull proc) then
                        proc.Dispose()
            finally
                try
                    tempDir.Delete true
                with _ ->
                    ()
    }

let defaultRunner: FormattingRunner =
    fun filePath sourceText isSignature options ct ->
        runWithSearchPath None filePath sourceText isSignature options ct

let private runVerifiedFormatting
    (runnerOverride: FormattingRunner option)
    (graph: ProjectGraph)
    (path: string)
    (options: FormattingOptions)
    (computeEdits: string -> string -> TextEdit[])
    (ct: CancellationToken)
    : Task<TextEdit[] option> =
    let isSignature = path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)
    let runner = defaultArg runnerOverride defaultRunner

    let rec loop (attempt: int) : Task<TextEdit[] option> =
        task {
            let! view = graph.LatestParse(path, ct)

            match! runner path view.Text isSignature options ct with
            | None -> return None
            | Some formatted ->
                let! after = graph.LatestParse(path, ct)

                if after.Version = view.Version then
                    return Some(computeEdits view.Text formatted)
                elif attempt < 1 then
                    return! loop (attempt + 1)
                else
                    return None
        }

    loop 0

let formatDocument
    (runnerOverride: FormattingRunner option)
    (graph: ProjectGraph)
    (path: string)
    (options: FormattingOptions)
    (ct: CancellationToken)
    : Task<TextEdit[] option> =
    runVerifiedFormatting runnerOverride graph path options computeDocumentEdits ct

let formatRange
    (runnerOverride: FormattingRunner option)
    (graph: ProjectGraph)
    (path: string)
    (lspRange: Range)
    (options: FormattingOptions)
    (ct: CancellationToken)
    : Task<TextEdit[] option> =
    runVerifiedFormatting runnerOverride graph path options (fun src fmt -> computeRangeEdits src fmt lspRange) ct
