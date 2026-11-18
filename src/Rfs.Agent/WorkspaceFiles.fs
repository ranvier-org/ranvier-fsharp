namespace Rfs.Agent

open System
open System.IO
open System.Collections.Generic

module internal WorkspaceFiles =
    let exclusions = [ ".git"; "node_modules"; "bin"; "obj"; ".worktrees"; "artifacts"; ".vs"; ".idea" ]

    let isSolution path =
        match Path.GetExtension(path: string).ToLowerInvariant() with
        | ".sln" | ".slnx" -> true
        | _ -> false

    let isProject path = String.Equals(Path.GetExtension(path: string), ".fsproj", StringComparison.OrdinalIgnoreCase)

    type private Listing =
        { Stamp: DateTime
          Children: string array
          Files: Set<string> }

    /// Directory timestamps change on entry creation and removal. Reuse unchanged listings between requests.
    type Index() =
        let listings = Dictionary<string, Listing>()
        let excluded = HashSet<string>(exclusions, StringComparer.OrdinalIgnoreCase)
        let solutions = Dictionary<string, DateTime * int64>()

        member _.Scan(workspaceRoot, projectDirectories: string list) =
            let seen = HashSet<string>()
            let added, removed = ResizeArray<string>(), ResizeArray<string>()
            let mutable solutionChanged = false

            let rec visit path remainingDepth =
                if seen.Add path then
                    try
                        let dir = DirectoryInfo path
                        let stamp = dir.LastWriteTimeUtc
                        let listing =
                            match listings.TryGetValue path with
                            | true, old when old.Stamp = stamp -> old
                            | known, old ->
                                let children = ResizeArray<string>()
                                let files = ResizeArray<string>()
                                for entry in dir.EnumerateFileSystemInfos() do
                                    if entry.Attributes.HasFlag FileAttributes.Directory then
                                        if not (excluded.Contains entry.Name || entry.Attributes.HasFlag FileAttributes.ReparsePoint) then
                                            children.Add entry.FullName
                                    else
                                        match entry.Extension.ToLowerInvariant() with
                                        | ".sln" | ".slnx" | ".fsproj" | ".fs" | ".fsi" -> files.Add entry.FullName
                                        | _ -> ()
                                let current = Set.ofSeq files
                                let previous = if known then old.Files else Set.empty
                                added.AddRange(Set.difference current previous)
                                removed.AddRange(Set.difference previous current)
                                let listing = { Stamp = stamp; Files = current; Children = children.ToArray() }
                                listings[path] <- listing
                                listing
                        if remainingDepth > 0 then
                            for child in listing.Children do visit child (remainingDepth - 1)
                    with
                    | :? DirectoryNotFoundException -> seen.Remove path |> ignore
                    | :? UnauthorizedAccessException -> ()

            // Loaded projects can include recursive Compile globs below the workspace discovery depth.
            for root in projectDirectories do visit root Int32.MaxValue
            visit workspaceRoot 4
            for path in listings.Keys |> Seq.filter (seen.Contains >> not) |> Seq.toArray do
                removed.AddRange listings[path].Files
                listings.Remove path |> ignore

            for path in removed do
                if isSolution path then solutions.Remove path |> ignore
            for path in added do
                if isSolution path then solutions[path] <- DateTime.MinValue, -1L
            for path in solutions.Keys |> Seq.toArray do
                let file = FileInfo path
                if file.Exists then
                    let stamp = file.LastWriteTimeUtc, file.Length
                    if stamp <> solutions[path] then
                        solutionChanged <- true
                        solutions[path] <- stamp
            added, removed, solutionChanged
