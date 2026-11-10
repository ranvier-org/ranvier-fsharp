namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Ionide.LanguageServerProtocol.Types
open Ionide.ProjInfo
open Rfs.Checker
open Rfs.Projects

/// FSAC's `PlainNotification` (LspHelpers.fs): the payload is a JSON document in a string
type PlainNotification = { Content: string }

/// Ported from FSAC's LspHelpers.fs
type WorkspaceLoadParms =
    {
        /// Project files to load
        TextDocuments: TextDocumentIdentifier[]
    }

/// Ported from FSAC's LspHelpers.fs
type WorkspacePeekRequest =
    { Directory: string
      Deep: int
      ExcludedDirs: string array }

/// What FSAC's `workspacePeek` finds: solutions with their projects, and directories holding projects
module WorkspacePeek =
    type Interesting =
        | Directory of string * string list
        | Solution of string * InspectSln.SolutionData

    let private skipped = set [ "obj"; "bin" ]

    /// Every `.sln`/`.slnx` and every directory with `.fsproj` files in `root` and the `deep` levels of directories below
    /// it, except directories named in `excluded`.
    let peek (root: string) (deep: int) (excluded: string list) : Interesting list =
        let solutions = ResizeArray<string>()
        let directories = ResizeArray<string * string list>()

        let isExcluded (name: string) =
            skipped.Contains name
            || excluded |> List.exists (fun e -> String.Equals(e, name, StringComparison.OrdinalIgnoreCase))

        let rec scan (dir: string) (level: int) =
            try
                let files = Directory.GetFiles dir |> Array.sort

                for f in files do
                    let ext = Path.GetExtension f

                    if ext = ".sln" || ext = ".slnx" then
                        solutions.Add f

                let projects = files |> Array.filter (fun f -> Path.GetExtension f = ".fsproj") |> List.ofArray

                if not projects.IsEmpty then
                    directories.Add((dir, projects))

                if level < deep then
                    for sub in Directory.GetDirectories dir |> Array.sort do
                        if not (isExcluded (Path.GetFileName sub)) then
                            scan sub (level + 1)
            with
            | :? UnauthorizedAccessException
            | :? DirectoryNotFoundException -> ()

        scan (Path.GetFullPath root) 0

        [ for s in solutions do
              match InspectSln.tryParseSln s with
              | Ok data -> yield Solution(s, data)
              | Error e -> eprintfn "[ranvierautocomplete] %s is not a readable solution: %O" s e
          for d, projects in directories -> Directory(d, projects) ]

    /// The `.fsproj` files of the solution's projects
    let private solutionProjects (path: string) (data: InspectSln.SolutionData) : string list =
        let rec items (xs: InspectSln.SolutionItem list) =
            xs
            |> List.collect (fun x ->
                match x.Kind with
                | InspectSln.SolutionItemKind.MSBuildFormat _ ->
                    [ Path.GetFullPath(Path.Combine(Path.GetDirectoryName path, x.Name)) ]
                | InspectSln.SolutionItemKind.Folder(children, _) -> items children
                | _ -> [])

        items data.Items |> List.filter (fun p -> Path.GetExtension p = ".fsproj")

    /// What `AutomaticWorkspaceInit` loads: the projects of the single solution, else every project found
    let projectsOf (found: Interesting list) : string list =
        let solutions = found |> List.choose (function Solution(p, d) -> Some(p, d) | _ -> None)

        match solutions with
        | [ p, d ] -> solutionProjects p d
        | _ -> found |> List.collect (function Directory(_, projects) -> projects | _ -> []) |> List.distinct

/// The `fsharp/notifyWorkspace` and response contents, in the shapes of FSAC's `CommandResponse` module. Property names
/// are FSAC's (Pascal case): its serializer is not camel-cased.
module Responses =
    let private strings (xs: string seq) =
        JsonArray(xs |> Seq.map (fun x -> JsonValue.Create x :> JsonNode) |> Array.ofSeq)

    let private message (kind: string) (data: JsonNode) =
        let o = JsonObject()
        o["Kind"] <- JsonValue.Create kind
        o["Data"] <- data
        o.ToJsonString()

    let private status (finished: bool) =
        let o = JsonObject()
        o["Status"] <- JsonValue.Create(if finished then "finished" else "started")
        o

    let workspaceLoad (finished: bool) = message "workspaceLoad" (status finished)

    let projectLoading (project: string) =
        let o = JsonObject()
        o["Project"] <- JsonValue.Create project
        message "projectLoading" o

    /// FSAC's `project` response, as far as a ProjectSpec knows it: the files and references are the project's, the
    /// rest (package references, framework info) is empty.
    let project (spec: ProjectSpec) =
        let o = JsonObject()
        o["Project"] <- JsonValue.Create spec.ProjectFile
        o["Files"] <- strings spec.SourceFiles
        o["Output"] <- JsonValue.Create "null"

        o["ProjectReferences"] <-
            JsonArray(
                spec.ReferencedProjects
                |> List.map (fun r ->
                    let p = JsonObject()
                    p["RelativePath"] <- JsonValue.Create(Path.GetRelativePath(Path.GetDirectoryName spec.ProjectFile, r))
                    p["ProjectFileName"] <- JsonValue.Create r
                    p :> JsonNode)
                |> Array.ofList
            )

        o["PackageReferences"] <- JsonArray()

        o["References"] <-
            strings (
                spec.OtherOptions
                |> List.choose (fun x -> if x.StartsWith "-r:" then Some(x.Substring 3) else None)
                |> List.sortBy Path.GetFileName
            )

        o["OutputType"] <- JsonValue.Create "lib"
        let info = JsonObject()
        info["IsTestProject"] <- JsonValue.Create false
        info["Configuration"] <- JsonValue.Create ""
        info["IsPackable"] <- JsonValue.Create false
        info["TargetFramework"] <- JsonValue.Create ""
        info["TargetFrameworkIdentifier"] <- JsonValue.Create ""
        info["TargetFrameworkVersion"] <- JsonValue.Create ""
        info["RestoreSuccess"] <- JsonValue.Create true
        info["TargetFrameworks"] <- JsonArray()
        info["RunCmd"] <- null
        info["IsPublishable"] <- null
        o["Info"] <- info

        o["Items"] <-
            JsonArray(
                spec.SourceFiles
                |> List.map (fun f ->
                    let i = JsonObject()
                    i["Name"] <- JsonValue.Create "Compile"
                    i["FilePath"] <- JsonValue.Create f
                    i["VirtualPath"] <- JsonValue.Create(Path.GetRelativePath(Path.GetDirectoryName spec.ProjectFile, f))
                    i["Metadata"] <- JsonObject()
                    i :> JsonNode)
                |> Array.ofList
            )

        o["AdditionalInfo"] <- JsonObject()
        message "project" o

    /// FSAC's `GenericProjectError` (code 102)
    let projectError (project: string) (reason: string) =
        let data = JsonObject()
        data["Project"] <- JsonValue.Create project
        let o = JsonObject()
        o["Code"] <- JsonValue.Create 102
        o["Message"] <- JsonValue.Create reason
        o["AdditionalData"] <- data
        message "error" o

    let workspacePeek (found: WorkspacePeek.Interesting list) =
        let rec item (x: InspectSln.SolutionItem) : JsonNode option =
            let kind (k: string) (data: JsonNode) =
                let o = JsonObject()
                o["Kind"] <- JsonValue.Create k
                o["Data"] <- data
                Some(o :> JsonNode)

            let kindNode =
                match x.Kind with
                | InspectSln.SolutionItemKind.Unknown
                | InspectSln.SolutionItemKind.Unsupported -> None
                | InspectSln.SolutionItemKind.MSBuildFormat _ ->
                    let d = JsonObject()
                    d["Configurations"] <- JsonArray()
                    kind "msbuildFormat" d
                | InspectSln.SolutionItemKind.Folder(children, files) ->
                    let d = JsonObject()
                    d["Items"] <- JsonArray(children |> List.choose item |> Array.ofList)
                    d["Files"] <- strings files
                    kind "folder" d

            kindNode
            |> Option.map (fun k ->
                let o = JsonObject()
                o["Guid"] <- JsonValue.Create x.Guid
                o["Name"] <- JsonValue.Create x.Name
                o["Kind"] <- k
                o :> JsonNode)

        let foundNode (f: WorkspacePeek.Interesting) : JsonNode =
            let o = JsonObject()
            let data = JsonObject()

            match f with
            | WorkspacePeek.Interesting.Directory(p, fsprojs) ->
                data["Directory"] <- JsonValue.Create p
                data["Fsprojs"] <- strings fsprojs
                o["Type"] <- JsonValue.Create "directory"
            | WorkspacePeek.Interesting.Solution(p, sd) ->
                data["Path"] <- JsonValue.Create p
                data["Items"] <- JsonArray(sd.Items |> List.choose item |> Array.ofList)
                data["Configurations"] <- JsonArray()
                o["Type"] <- JsonValue.Create "solution"

            o["Data"] <- data
            o :> JsonNode

        let data = JsonObject()
        data["Found"] <- JsonArray(found |> List.map foundNode |> Array.ofList)
        message "workspacePeek" data

type LoadResult =
    | Loaded of project: string
    | Failed of project: string * reason: string

type private Entry = { Spec: ProjectSpec; Graph: ProjectGraph }

/// <summary>
/// The projects the server has loaded, each on its own <see cref="ProjectGraph"/> over one shared host.
/// </summary>
/// <remarks>
/// <para>
/// A load settles each project's cold diagnostics before it returns. The settle is bounded by a timeout
/// (<c>RFS_LOAD_TIMEOUT_MS</c>, ten minutes by default): a project that does not settle in time is <c>Failed</c> and its
/// graph is disposed, and the rest of the load goes on. Loads, reloads and project-file changes run one at a time.
/// </para>
/// <para>
/// The loaded projects are a keyed collection on the host's graph, and whether a load is under way a signal there;
/// the owners of each file, the projects and what references what are memos over them. All of it changes only on the
/// graph thread, so a read there (the members ending in <c>Now</c>) is tracked and of one turn with the files'
/// nodes. Off that thread the members read the entries as the graph thread last left them.
/// </para>
/// </remarks>
type Workspace(host: GraphHost, client: RanvierClient, ?loadTimeout: TimeSpan) =
    let timeout = defaultArg loadTimeout (Workspace.DefaultLoadTimeout())
    // Graph thread
    let openTexts = Dictionary<string, struct (string * int)>(StringComparer.OrdinalIgnoreCase)
    let mutable entries: Ranvier.KeyedCollection<string, Entry> = Unchecked.defaultof<_>
    let mutable loadingNow: Ranvier.Signal<int> = Unchecked.defaultof<_>
    let mutable entryList: Ranvier.Memo<Entry[]> = Unchecked.defaultof<_>
    let mutable ownersMemo: Ranvier.Memo<IReadOnlyDictionary<string, ProjectGraph list>> = Unchecked.defaultof<_>
    // Any thread: the entries, in load order, as the graph thread last left them
    let mutable published: Entry[] = [||]

    let keyOf (project: string) = project.ToUpperInvariant()

    do
        host
            .Read(fun () ->
                Ranvier.Api.createRoot (fun _ ->
                    entries <- Ranvier.Api.createKeyedCollection (fun (e: Entry) -> keyOf e.Spec.ProjectFile)
                    loadingNow <- Ranvier.Api.createSignal 0
                    entryList <- Ranvier.Api.createMemo (fun _ -> [| for k in entries.Rows.Keys -> entries.Rows.Get k |])

                    ownersMemo <-
                        Ranvier.Api.createMemo (fun _ ->
                            let owners = Dictionary<string, ProjectGraph list>(StringComparer.OrdinalIgnoreCase)

                            for e in entryList.Value do
                                for f in e.Spec.SourceFiles do
                                    owners[f] <- (match owners.TryGetValue f with | true, l -> l | _ -> []) @ [ e.Graph ]

                            owners :> IReadOnlyDictionary<_, _>))
                |> ignore)
            .Wait()
    let mutations = new SemaphoreSlim(1)
    let mutable loading = 0
    let lifetime = new CancellationTokenSource()
    let mutable onGraphCreated: ProjectGraph -> unit = ignore
    let mutable onLoaded: ProjectGraph -> unit = ignore
    let mutable onLoadEndHook: unit -> unit = ignore
    let mutable beforeLoadEnd: unit -> Task = fun () -> Task.CompletedTask

    let onLoadEnd () =
        try
            onLoadEndHook ()
        with ex ->
            eprintfn "[ranvierautocomplete] after-load hook failed: %O" ex

    let log (text: string) = eprintfn "[ranvierautocomplete] %s" text

    let notifyWorkspace (content: string) : Task =
        client.Notify("fsharp/notifyWorkspace", box { Content = content }) |> Async.StartAsTask :> Task

    let showMessage (text: string) : Task =
        client.WindowShowMessage { Type = MessageType.Error; Message = text } |> Async.StartAsTask :> Task

    let showWarning (text: string) : Task =
        client.WindowShowMessage { Type = MessageType.Warning; Message = text } |> Async.StartAsTask :> Task

    let isSource (spec: ProjectSpec) (path: string) =
        spec.SourceFiles |> List.exists (fun f -> String.Equals(f, path, StringComparison.OrdinalIgnoreCase))

    let snapshot () = Volatile.Read &published

    let find (project: string) =
        snapshot () |> Array.tryFind (fun e -> String.Equals(e.Spec.ProjectFile, project, StringComparison.OrdinalIgnoreCase))

    /// Graph thread
    let republish () =
        Volatile.Write(&published, Ranvier.Api.untrack (fun () -> entryList.Value))

    /// Removes the projects' entries in one graph item, and returns them
    let remove (projects: string list) : Task<Entry list> =
        host.Read(fun () ->
            let found = projects |> List.choose find

            for e in found do
                entries.Remove(keyOf e.Spec.ProjectFile) |> ignore

            republish ()
            found)

    let isLoaded (project: string) = (find project).IsSome

    /// The projects, transitively, that reference the project of `graph`, among `all`
    let referencingIn (all: Entry[]) (graph: ProjectGraph) : ProjectGraph list =
        match all |> Array.tryFind (fun e -> obj.ReferenceEquals(e.Graph, graph)) with
        | None -> []
        | Some root ->
            let found = HashSet<string>(StringComparer.OrdinalIgnoreCase)

            let rec visit (file: string) =
                for e in all do
                    if e.Spec.ReferencedProjects |> List.exists (fun r -> String.Equals(r, file, StringComparison.OrdinalIgnoreCase))
                       && found.Add e.Spec.ProjectFile then
                        visit e.Spec.ProjectFile

            visit root.Spec.ProjectFile
            [ for e in all do if found.Contains e.Spec.ProjectFile then yield e.Graph ]

    /// Loads one project (its references have been visited), and settles its cold check
    let loadGraph (spec: ProjectSpec) : Task<LoadResult> =
        task {
            let created = ref None

            try
                // The ones that are not loaded (they failed to) stay the files on disk
                // or are loaded for another framework than this project resolved to: only those are its assembly
                let references =
                    List.zip spec.ReferencedProjects spec.ReferencedFrameworks
                    |> List.choose (fun (r, framework) ->
                        match find r with
                        | Some e when ProjectSpec.isFSharpProject r && (String.IsNullOrEmpty framework || e.Spec.TargetFramework = framework) ->
                            Some e.Graph
                        | _ -> None)

                // A graph has one framework: this project's edits of them show only in a build
                let otherFrameworks =
                    List.zip spec.ReferencedProjects spec.ReferencedFrameworks
                    |> List.choose (fun (r, framework) ->
                        match find r with
                        | Some e when ProjectSpec.isFSharpProject r && not (String.IsNullOrEmpty framework) && e.Spec.TargetFramework <> framework ->
                            Some(r, framework, e.Spec.TargetFramework)
                        | _ -> None)

                for r, framework, loaded in otherFrameworks do
                    let text =
                        $"%s{spec.ProjectFile} reads %s{r} from its DLL on disk: it needs %s{framework}, and that project is loaded for %s{loaded}. Edits of it reach this project only through a build."

                    log text
                    do! showWarning text

                let! graph = ProjectGraph.Create(host, spec, references)
                created.Value <- Some graph
                onGraphCreated graph

                // Registered before the settle wait, in the graph item that reads the open texts and sets them: an `Open`
                // (a graph item too) either lands in `openTexts` first and is read here, or finds the entry and sets its
                // text after these, so older text never overwrites newer.
                do!
                    host.Read(fun () ->
                        entries.AddOrUpdate { Spec = spec; Graph = graph }
                        republish ()

                        for f in spec.SourceFiles do
                            match openTexts.TryGetValue f with
                            | true, struct (t, _) -> graph.SetText(f, t) |> ignore
                            | _ -> ())

                // The settle waits take no token of their own
                try
                    let! _ = graph.AllDiagnostics().WaitAsync(timeout, lifetime.Token)
                    ()
                with :? TimeoutException ->
                    raise (
                        TimeoutException(
                            $"settling the cold check timed out after %.0f{timeout.TotalMilliseconds} ms (RFS_LOAD_TIMEOUT_MS)"
                        )
                    )

                do! notifyWorkspace (Responses.project spec)

                try
                    onLoaded graph
                with ex ->
                    log $"%s{spec.ProjectFile}: publishing after the load failed: %O{ex}"

                return Loaded spec.ProjectFile
            with ex ->
                // The removal is a graph item, and the disposal's posts follow it
                try
                    remove [ spec.ProjectFile ] |> ignore
                with :? ObjectDisposedException ->
                    ()

                created.Value |> Option.iter (fun g -> (g :> IDisposable).Dispose())
                return Failed(spec.ProjectFile, ex.Message)
        }

    let report (project: string) (reason: string) : Task =
        task {
            log $"%s{project} failed to load: %s{reason}"
            do! notifyWorkspace (Responses.projectError project reason)
            do! showMessage $"Failed to load %s{project}: %s{reason}"
        }

    /// Loads `project`, for `framework` when a reference resolved to one, after the projects it references; the results
    /// are added to `results`. The first framework a project is loaded for is its graph's.
    let rec visitFor (visited: HashSet<string>) (results: Dictionary<string, LoadResult>) (project: string) (framework: string) : Task =
        task {
            let project = Path.GetFullPath project

            if visited.Add project then
                if isLoaded project then
                    results[project] <- Loaded project
                else
                    do! notifyWorkspace (Responses.projectLoading project)

                    let! loaded =
                        task {
                            try
                                let! spec = ProjectSpec.loadFor project framework

                                // A project of another language stays the assembly on disk
                                for r, resolved in List.zip spec.ReferencedProjects spec.ReferencedFrameworks do
                                    if ProjectSpec.isFSharpProject r then
                                        do! visitFor visited results r resolved

                                return! loadGraph spec
                            with ex ->
                                return Failed(project, ex.Message)
                        }

                    results[project] <- loaded

                    match loaded with
                    | Failed(_, reason) -> do! report project reason
                    | Loaded _ -> ()
        }

    let visit visited results project = visitFor visited results project ""

    let loadAll (projects: string list) : Task<LoadResult list> =
        task {
            let visited = HashSet<string>(StringComparer.OrdinalIgnoreCase)
            let results = Dictionary<string, LoadResult>(StringComparer.OrdinalIgnoreCase)
            do! notifyWorkspace (Responses.workspaceLoad false)

            for p in projects do
                do! visit visited results p

            do! notifyWorkspace (Responses.workspaceLoad true)
            onLoadEnd ()
            let requested = projects |> List.map Path.GetFullPath |> List.distinct
            let others = results.Keys |> Seq.filter (fun k -> not (List.exists (fun r -> String.Equals(r, k, StringComparison.OrdinalIgnoreCase)) requested)) |> List.ofSeq
            return (requested @ others) |> List.map (fun k -> results[k])
        }

    /// The loading signal follows the count: a read in one turn waiting through the load counts it as pending, and
    /// runs again when it ends
    let postLoading () =
        try
            host.Post(fun () -> loadingNow.Value <- Volatile.Read &loading)
        with :? ObjectDisposedException ->
            ()

    let exclusively (work: unit -> Task<'T>) : Task<'T> =
        task {
            Interlocked.Increment &loading |> ignore
            postLoading ()

            try
                do! mutations.WaitAsync lifetime.Token

                try
                    return! work ()
                finally
                    mutations.Release() |> ignore
            finally
                Interlocked.Decrement &loading |> ignore
                postLoading ()
        }

    /// Reloads `projects` and every project that references one of them, each once: a branch switch or a root props
    /// change names several, and one project's reload would otherwise reload what references it once per project
    /// named (N3).
    let reload (projects: string list) : Task<unit> =
        task {
            let roots = projects |> List.map Path.GetFullPath |> List.distinctBy (fun p -> p.ToUpperInvariant())
            let isRoot (p: string) = roots |> List.exists (fun r -> String.Equals(r, p, StringComparison.OrdinalIgnoreCase))
            let all = snapshot ()

            // A referencing project's context holds the old graph's assembly, and disposing it disposes theirs: they
            // are hidden with it, and load again after it, on its new graph or, if it no longer loads, the file on disk
            let referencingOf (project: string) =
                let found = List<string>()

                let rec visit (file: string) =
                    for e in all do
                        if e.Spec.ReferencedProjects |> List.exists (fun r -> String.Equals(r, file, StringComparison.OrdinalIgnoreCase))
                           && not (found.Contains e.Spec.ProjectFile) then
                            found.Add e.Spec.ProjectFile
                            visit e.Spec.ProjectFile

                visit project
                List.ofSeq found

            let byRoot = roots |> List.map (fun r -> r, referencingOf r)

            let referencing =
                byRoot
                |> List.collect snd
                |> List.filter (isRoot >> not)
                |> List.distinctBy (fun p -> p.ToUpperInvariant())

            let! removed = remove (roots @ referencing)

            for e in removed do
                (e.Graph :> IDisposable).Dispose()

            for p in roots do
                ProjectSpec.invalidate p

            let visited = HashSet<string>(StringComparer.OrdinalIgnoreCase)
            let results = Dictionary<string, LoadResult>(StringComparer.OrdinalIgnoreCase)
            do! notifyWorkspace (Responses.workspaceLoad false)

            for p in roots do
                do! visit visited results p

            // A referencing project's spec holds the `-r:` its references' outputs gave it. While one of those does not
            // load, evaluating it would fail too: it keeps its spec, and the file on disk
            for p in referencing do
                let named = byRoot |> List.filter (fun (_, refs) -> refs |> List.exists (fun x -> String.Equals(x, p, StringComparison.OrdinalIgnoreCase)))

                if named |> List.forall (fun (r, _) -> isLoaded r) then
                    ProjectSpec.invalidate p

            for p in referencing do
                do! visit visited results p

            do! beforeLoadEnd ()
            do! notifyWorkspace (Responses.workspaceLoad true)
            onLoadEnd ()
        }

    /// Counts as loading until disposed (idempotent): a project change known and not yet applied
    member _.HoldLoading() : IDisposable =
        Interlocked.Increment &loading |> ignore
        postLoading ()
        let ended = ref 0

        { new IDisposable with
            member _.Dispose() =
                if Interlocked.Exchange(&ended.contents, 1) = 0 then
                    Interlocked.Decrement &loading |> ignore
                    postLoading () }

    /// How long a load waits for a project's cold check: `RFS_LOAD_TIMEOUT_MS`, else ten minutes
    static member DefaultLoadTimeout() : TimeSpan =
        match Environment.GetEnvironmentVariable "RFS_LOAD_TIMEOUT_MS" with
        | null
        | "" -> TimeSpan.FromMinutes 10.
        | text ->
            match Double.TryParse text with
            | true, ms when ms > 0. -> TimeSpan.FromMilliseconds ms
            | _ -> TimeSpan.FromMinutes 10.

    /// Loads the projects and the projects they reference, and returns once each has settled its cold diagnostics
    /// (or failed to). The requested projects come first in the result, in order.
    member _.Load(projects: string list) : Task<LoadResult list> = exclusively (fun () -> loadAll projects)

    /// The graph of the project that has `path` as a source file (or as its project file)
    member _.ProjectOf(path: string) : ProjectGraph option =
        let path = Path.GetFullPath path

        snapshot ()
        |> Array.tryFind (fun e ->
            isSource e.Spec path || String.Equals(e.Spec.ProjectFile, path, StringComparison.OrdinalIgnoreCase))
        |> Option.map _.Graph

    member _.Projects: ProjectGraph list = snapshot () |> Array.map _.Graph |> List.ofArray

    /// Runs `hook` for each new graph before its cold check is asked for. For tests.
    member internal _.OnGraphCreated(hook: ProjectGraph -> unit) : unit = onGraphCreated <- hook

    /// Disposes the project's graph, evaluates the project again and creates a new graph with the open texts applied
    member _.Reload(project: string) : Task<unit> = exclusively (fun () -> reload [ project ])

    /// A document the editor opened (or changed): its text is the file's, and goes to the graphs of the projects with it
    /// Returns each graph with the internal version of the text.
    member _.Open(path: string, text: string, ?lspVersion: int) : (ProjectGraph * int64) list =
        let path = Path.GetFullPath path
        let lspVersion = defaultArg lspVersion 0

        // Graph thread, as the registration in loadGraph: the order of the SetText calls is the order of the texts
        openTexts[path] <- struct (text, lspVersion)

        [ for e in snapshot () do
              if isSource e.Spec path then
                  yield e.Graph, e.Graph.SetText(path, text) ]

    /// Runs `hook` for each graph that has loaded and settled, after its open documents' texts are set.
    member internal _.OnLoaded(hook: ProjectGraph -> unit) : unit = onLoaded <- hook

    /// Whether a load or reload is under way or queued: the snapshot may lack projects it hides meanwhile
    member _.IsLoading: bool = Volatile.Read &loading > 0

    /// The source files of the project of `graph`, in file order, signature files too
    member _.FilesOf(graph: ProjectGraph) : string list =
        match snapshot () |> Array.tryFind (fun e -> obj.ReferenceEquals(e.Graph, graph)) with
        | Some e -> e.Spec.SourceFiles
        | None -> []

    /// The projects, transitively, that reference the project of `graph`
    member _.ReferencingProjects(graph: ProjectGraph) : ProjectGraph list = referencingIn (snapshot ()) graph

    /// Graph thread, tracked: whether a load or reload is under way or queued
    member internal _.LoadingNow() : bool = loadingNow.Value > 0

    /// Graph thread, tracked: the graphs of the projects that have `path` (a full path) as a source file, in load order
    member internal _.OwnersNow(path: string) : ProjectGraph list =
        match ownersMemo.Value.TryGetValue path with
        | true, owners -> owners
        | _ -> []

    /// Graph thread, tracked: the projects' graphs, in load order
    member internal _.ProjectsNow() : ProjectGraph list = entryList.Value |> Array.map _.Graph |> List.ofArray

    /// Graph thread, tracked: the source files of the project of `graph`, as FilesOf
    member internal _.FilesNow(graph: ProjectGraph) : string list =
        match entryList.Value |> Array.tryFind (fun e -> obj.ReferenceEquals(e.Graph, graph)) with
        | Some e -> e.Spec.SourceFiles
        | None -> []

    /// Graph thread, tracked: as ReferencingProjects
    member internal _.ReferencingNow(graph: ProjectGraph) : ProjectGraph list = referencingIn entryList.Value graph

    /// <summary>
    /// The editor closed the document: the file on disk is the file's text again, read already by the caller
    /// (<c>ReadOnClose</c>), so the caller may be the graph thread. Returns the graphs that own the file. <c>None</c>, a
    /// file that is gone from disk or could not be read, keeps its last text in the graphs: the project no longer loads
    /// without it, and the watched-file change that reloads the project settles that.
    /// </summary>
    member _.Close(path: string, diskText: string option) : ProjectGraph list =
        let path = Path.GetFullPath path

        // Graph thread, as loadGraph and Open, so the disk text cannot overtake a newer open text
        openTexts.Remove path |> ignore

        [ for e in snapshot () do
              if isSource e.Spec path then
                  diskText |> Option.iter (fun t -> e.Graph.SetText(path, t) |> ignore)
                  yield e.Graph ]

    /// The text of `path` on disk, for <c>Close</c>; None, and a line on stderr, when it can't be read
    static member ReadOnClose(path: string) : string option =
        try
            Some(File.ReadAllText(Path.GetFullPath path))
        with
        | :? IOException
        | :? UnauthorizedAccessException as ex ->
            eprintfn "[ranvierautocomplete] %s could not be read on close: %s" path ex.Message
            None

    /// Awaits `hook` at the end of a reload, after its projects have loaded and before the load's end. For tests.
    member internal _.BeforeLoadEnd(hook: unit -> Task) : unit = beforeLoadEnd <- hook

    /// Runs `hook` when a load or reload has finished, whether its projects loaded or not.
    member internal _.OnLoadEnd(hook: unit -> unit) : unit = onLoadEndHook <- hook

    /// The graphs of the projects that have `path` as a source file, in load order
    member _.OwnersOf(path: string) : ProjectGraph list =
        let path = Path.GetFullPath path
        snapshot () |> Array.filter (fun e -> isSource e.Spec path) |> Array.map _.Graph |> List.ofArray

    /// Applies watched-file changes: a project, props, targets or assets file reloads the projects it belongs to; a
    /// source file that is not open is read from disk. `deleted` marks a deleted file.
    member _.FilesChanged(changes: (string * bool) list) : Task =
        exclusively (fun () ->
            task {
                let toReload = List<string>()
                let onDisk = List<string * string>()

                for path, deleted in changes do
                    let path = Path.GetFullPath path
                    let name = Path.GetFileName path
                    let ext = Path.GetExtension path

                    let built =
                        (ext = ".props" || ext = ".targets")
                        && path.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                           |> Array.exists (fun s -> s.Equals("obj", StringComparison.OrdinalIgnoreCase) || s.Equals("bin", StringComparison.OrdinalIgnoreCase))

                    // Restore writes these under obj, with the project.assets.json that is watched as well
                    if built then
                        ()
                    elif ext = ".fsproj" || ext = ".props" || ext = ".targets" || name = "project.assets.json" then
                        for e in snapshot () do
                            let projectDir = Path.GetDirectoryName e.Spec.ProjectFile

                            let belongs =
                                if ext = ".fsproj" then
                                    String.Equals(e.Spec.ProjectFile, path, StringComparison.OrdinalIgnoreCase)
                                elif name = "project.assets.json" then
                                    let assetsDir = Path.GetDirectoryName path

                                    String.Equals(Path.GetDirectoryName assetsDir, projectDir, StringComparison.OrdinalIgnoreCase)
                                    // The artifacts layout: artifacts/obj/<project>/project.assets.json
                                    || (String.Equals(Path.GetFileName assetsDir, Path.GetFileNameWithoutExtension e.Spec.ProjectFile, StringComparison.OrdinalIgnoreCase)
                                        && String.Equals(Path.GetFileName(Path.GetDirectoryName assetsDir), "obj", StringComparison.OrdinalIgnoreCase))
                                else
                                    // a props or targets file applies to the projects in and below its directory
                                    let dir = Path.GetDirectoryName path
                                    projectDir.StartsWith(dir, StringComparison.OrdinalIgnoreCase)

                            if belongs && not (toReload.Contains e.Spec.ProjectFile) then
                                toReload.Add e.Spec.ProjectFile
                    elif (ext = ".fs" || ext = ".fsi") && not deleted && not (onDisk |> Seq.exists (fun (p, _) -> String.Equals(p, path, StringComparison.OrdinalIgnoreCase))) then
                        try
                            onDisk.Add((path, File.ReadAllText path))
                        with
                        | :? IOException
                        | :? UnauthorizedAccessException as ex -> log $"%s{path} could not be read: %s{ex.Message}"

                // Every closed file's disk text in one graph item, so the graphs settle once for all of them (N6). A graph
                // item as Open is, so the disk text cannot overtake a text opened meanwhile.
                if onDisk.Count > 0 then
                    do!
                        host.Read(fun () ->
                            Ranvier.Api.batch (fun () ->
                                for path, text in onDisk do
                                    if not (openTexts.ContainsKey path) then
                                        for e in snapshot () do
                                            if isSource e.Spec path then
                                                e.Graph.SetText(path, text) |> ignore))

                if toReload.Count > 0 then
                    do! reload (List.ofSeq toReload)
            })

    member _.Dispose() =
        lifetime.Cancel()

        for e in snapshot () do
            (e.Graph :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
