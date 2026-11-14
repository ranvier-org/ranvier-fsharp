namespace Rfs.Agent

open System
open System.Buffers.Binary
open System.IO
open System.Reflection
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks

module AgentVersion =
    type private Marker = class end

    let informational () : string =
        let asm = typeof<Marker>.Assembly

        match Attribute.GetCustomAttribute(asm, typeof<AssemblyInformationalVersionAttribute>) with
        | :? AssemblyInformationalVersionAttribute as attr when not (String.IsNullOrWhiteSpace attr.InformationalVersion) ->
            attr.InformationalVersion
        | _ ->
            match asm.GetName().Version with
            | null -> "0.1.0"
            | v -> v.ToString()

    let semver () : string =
        let v = informational ()

        match v.IndexOf '+' with
        | i when i > 0 -> v.Substring(0, i)
        | _ -> v

module WorkspaceRoot =
    let canonical (path: string) : string =
        let full = Path.GetFullPath path
        let trimmed = full.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)

        if String.IsNullOrEmpty trimmed then
            full
        else
            trimmed

    let private hasAnyFile (dir: string) (pattern: string) : bool =
        try
            Directory.EnumerateFiles(dir, pattern, SearchOption.TopDirectoryOnly)
            |> Seq.isEmpty
            |> not
        with _ ->
            false

    /// Resolves the workspace root for a command target:
    /// 1. Explicit workspace directory if provided.
    /// 2. Walk upward up to `.git` (stopping at repo boundary), preferring the outermost directory with `.slnx`/`.sln`,
    ///    otherwise the git root, otherwise the outermost directory holding `.fsproj`, otherwise the starting directory.
    let resolve (explicitWorkspace: string option) (targetPath: string option) : string =
        match explicitWorkspace with
        | Some w when not (String.IsNullOrWhiteSpace w) ->
            let full = canonical w

            if File.Exists full then
                match Path.GetDirectoryName full with
                | null
                | "" -> canonical Environment.CurrentDirectory
                | d -> canonical d
            else
                full
        | _ ->
            let startDir =
                match targetPath with
                | Some p when not (String.IsNullOrWhiteSpace p) ->
                    let full = Path.GetFullPath p

                    if Directory.Exists full then
                        full
                    else
                        match Path.GetDirectoryName full with
                        | null
                        | "" -> Environment.CurrentDirectory
                        | d -> d
                | _ -> Environment.CurrentDirectory

            let rec walk (dir: DirectoryInfo) (slnDir: string option) (fsprojDir: string option) =
                if isNull dir then
                    slnDir |> Option.orElse fsprojDir |> Option.defaultValue startDir
                else
                    let cur = dir.FullName
                    let nextSln = if hasAnyFile cur "*.slnx" || hasAnyFile cur "*.sln" then Some cur else slnDir
                    let nextFsproj = if hasAnyFile cur "*.fsproj" then Some cur else fsprojDir

                    // `.git` is a directory in normal checkouts and a file in git worktrees.
                    if Path.Exists(Path.Combine(cur, ".git")) then
                        nextSln |> Option.defaultValue cur
                    else
                        walk dir.Parent nextSln nextFsproj

            canonical (walk (DirectoryInfo startDir) None None)

    let shortHash (text: string) (hexChars: int) : string =
        let normalized =
            if OperatingSystem.IsLinux() then
                text
            else
                text.ToLowerInvariant()

        let hash = SHA256.HashData(Encoding.UTF8.GetBytes normalized)
        let hex = Convert.ToHexString(hash).ToLowerInvariant()
        hex.Substring(0, min hexChars hex.Length)

    let workspaceId (workspaceRoot: string) : string =
        shortHash (canonical workspaceRoot) 16

    let pipeName (stateDir: string) (workspaceRoot: string) : string =
        let stateHash = shortHash (canonical stateDir) 8
        let wsHash = workspaceId workspaceRoot
        $"rfs-daemon-%s{stateHash}-%s{wsHash}"

    let relativePath (workspaceRoot: string) (fullPath: string) : string =
        try
            let rel = Path.GetRelativePath(workspaceRoot, Path.GetFullPath fullPath)

            if rel.StartsWith("..", StringComparison.Ordinal) then
                fullPath.Replace('\\', '/')
            else
                rel.Replace('\\', '/')
        with _ ->
            fullPath.Replace('\\', '/')

module JsonUtil =
    let str (o: JsonObject) (name: string) : string option =
        let mutable node: JsonNode = null

        if o.TryGetPropertyValue(name, &node) && not (isNull node) then
            try
                let s = node.GetValue<string>()
                if String.IsNullOrWhiteSpace s then None else Some s
            with _ ->
                None
        else
            None

    let strRaw (o: JsonObject) (name: string) : string option =
        let mutable node: JsonNode = null

        if o.TryGetPropertyValue(name, &node) && not (isNull node) then
            try
                Some(node.GetValue<string>())
            with _ ->
                None
        else
            None

    let intOpt (o: JsonObject) (name: string) : int option =
        let mutable node: JsonNode = null

        if o.TryGetPropertyValue(name, &node) && not (isNull node) then
            try
                Some(node.GetValue<int>())
            with _ ->
                None
        else
            None

    let int64Opt (o: JsonObject) (name: string) : int64 option =
        let mutable node: JsonNode = null

        if o.TryGetPropertyValue(name, &node) && not (isNull node) then
            try
                Some(node.GetValue<int64>())
            with _ ->
                None
        else
            None

    let boolOpt (o: JsonObject) (name: string) : bool option =
        let mutable node: JsonNode = null

        if o.TryGetPropertyValue(name, &node) && not (isNull node) then
            try
                Some(node.GetValue<bool>())
            with _ ->
                None
        else
            None

module PipeFraming =
    let private maxMessageBytes = 64 * 1024 * 1024

    let writeAsync (stream: Stream) (doc: JsonObject) (ct: CancellationToken) : Task =
        task {
            let payload = Encoding.UTF8.GetBytes(doc.ToJsonString())
            let header = Array.zeroCreate<byte> 4
            BinaryPrimitives.WriteInt32LittleEndian(header, payload.Length)
            do! stream.WriteAsync(ReadOnlyMemory<byte>(header), ct)
            do! stream.WriteAsync(ReadOnlyMemory<byte>(payload), ct)
            do! stream.FlushAsync(ct)
        }

    let readAsync (stream: Stream) (ct: CancellationToken) : Task<JsonObject option> =
        task {
            let header = Array.zeroCreate<byte> 4
            let! first = stream.ReadAsync(Memory<byte>(header), ct)

            if first = 0 then
                return None
            else
                if first < 4 then
                    do! stream.ReadExactlyAsync(Memory<byte>(header, first, 4 - first), ct)

                let length = BinaryPrimitives.ReadInt32LittleEndian(header)

                if length <= 0 || length > maxMessageBytes then
                    return invalidOp $"Invalid framed message length: {length}"
                else
                    let payload = Array.zeroCreate<byte> length
                    do! stream.ReadExactlyAsync(Memory<byte>(payload), ct)
                    let node = JsonNode.Parse(ReadOnlySpan<byte>(payload))

                    match node with
                    | :? JsonObject as o -> return Some o
                    | _ -> return invalidOp "Expected JSON object frame"
        }
