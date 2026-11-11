namespace Rfs.Checker

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier
open Rfs.Projects

open ReadSupport

/// A project's assembly as it stands.
[<NoEquality; NoComparison>]
type internal AssemblyState =
    /// A file is not settled, or the project's own references have moved on
    | Unsettled
    | Built of ProjectAssembly

/// The referenced assemblies' fingerprints a project context was made with, by reference.
[<ReferenceEquality>]
type internal ReferencesSeen =
    { Fingerprints: string[]
      Context: ProjectContext }

module private ProjectNodes =
    /// Every file's nodes, in a root of their own so disposing it takes them all. `texts` are each file's path, text and
    /// version; `wanted` are the indexes of the files to promote. Graph thread.
    let build (project: ProjectShared) (texts: (string * string * int64)[]) (wanted: HashSet<int>) : Owner * IFileNode[] =
        createRoot (fun owner ->
            let isSignature (path: string) =
                path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

            let fileTexts =
                texts |> Array.map (fun (path, text, version) -> FileText(project, path, text, version))

            let nodes = ResizeArray<IFileNode>()
            let deps = FileDeps.create project fileTexts
            let signatureOf (j: int) = nodes[j].Signature

            fileTexts
            |> Array.iteri (fun i file ->
                nodes.Add(
                    if isSignature file.Path then
                        WholeFile.signature project i file deps signatureOf
                    else
                        WholeFile.impl project i file deps signatureOf
                ))

            // One build at a time: the file building stays the builder while it may still be building (its state
            // pending included), so neither an edit nor an earlier file starts a second build beside it; then the
            // first file, in file order, that may build
            project.Builder <-
                createMemo (fun previous ->
                    match previous with
                    | ValueSome(ValueSome b) when nodes[b].MayBuild -> ValueSome b
                    | _ -> nodes |> Seq.tryFindIndex _.MayBuild |> ValueOption.ofOption)

            let failed =
                nodes
                // The last settled answer while the file is being checked again, so the files set up from its signature
                // file do not wait for it: when the answer changes they are checked again before they settle
                |> Seq.map (fun node ->
                    createMemoWithComparer HashIdentity.Structural (fun previous ->
                        match node.Signature.TryValue, previous with
                        | Ready s, _ -> s.Signature.Failed
                        | Pending, ValueSome known -> known
                        | Pending, ValueNone -> false
                        | _ -> node.Signature.Value.Signature.Failed))
                |> Array.ofSeq

            project.FailedOf <- fun i -> failed[i]
            project.CurrentOf <- fun i -> nodes[i].Current

            // The dependencies read every file's parse, so effects start once every file's nodes exist
            for node in nodes do
                node.Activate()

            for i in wanted do
                nodes[i].Promote()

            owner, nodes.ToArray())

    /// The projects `references` reach, each once, nearest first.
    let closure (direct: 'G list) (next: 'G -> 'G list) : 'G list =
        let seen = HashSet<'G>(HashIdentity.Reference)

        let rec visit (g: 'G) =
            [ if seen.Add g then
                  yield g
                  for r in next g do
                      yield! visit r ]

        direct |> List.collect visit

/// What a request reads of a file before it takes the lock (ProjectGraph.ReadFile's `prepare`): the text and its parse,
/// which no check changes. No result, check or signature: nothing live can be read without the lock.
type TextView =
    { Parse: FSharpParseFileResults
      Text: string
      Docs: FileDocs
      /// The graph's version of the text (the one `SetText` returned)
      Version: int64 }

/// A file's settled result with the parse and text it is for.
type FileView =
    { Result: FileResult
      Parse: FSharpParseFileResults
      Text: string
      Docs: FileDocs
      /// The graph's version of the text (the one `SetText` returned)
      Version: int64
      /// The signatures the result was set up from
      Before: FileSignature list
      At: int -> int -> FSharpCheckFileResults
      /// The same object while the same result is published (ProjectGraph.DocsInTurn)
      Identity: obj
      /// The result of the declaration that holds the 1-based line, and its first line: the same object while that
      /// declaration's result is kept. None outside every declaration, and for a file checked whole.
      RowAt: int -> struct (obj * int) voption
      /// Whether the result is a whole check's (CheckFile), not a finish of declaration results
      Whole: bool
      /// The text version of this file and of each file before it, in the turn that read the result: what it is of.
      /// Empty unless asked for (ProjectGraph.ReadFile's `inputs`)
      Inputs: int64[]
      /// The graph's generation in that turn (ProjectGraph.Restarts); -1 unless asked for
      Generation: int64 }

    /// Check results for a query that reads the 1-based line and 0-based column, inside the `read` it was given to. In a file
    /// checked by declaration, a kept declaration holds the names as they were when it was checked: when an earlier result
    /// has changed since, this checks it again in the current environment and gives a view of that, adopting nothing
    /// (ProjectGraph.AdHocChecks). A file checked whole always has its CheckResults.
    member v.CheckResultsAt(line: int, column: int) : FSharpCheckFileResults = v.At line column

/// A file's latest text, read in one graph turn (ProjectGraph.LatestText)
[<NoEquality; NoComparison>]
type LatestText =
    { View: TextView
      /// The file's result is settled on this text
      Settled: bool
      /// The file's last published result in the same turn, kept while a newer one is pending; None before the first
      Published: PublishedState voption
      /// What the caller's `busy` read in the same turn, such as a reload under way
      Busy: bool
      /// What the caller's `pending` read in the same turn
      Pending: bool
      /// The latest `///` comments of each file asked for, ValueNone while its parse is pending
      Docs: FileDocs voption[] }

/// A file as it stands against a text version it had (ProjectGraph.FilesNow)
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type FileNow =
    /// Its latest text is still that version
    | Same
    /// An implementation file with a newer text: the latest text and parse, and its last published result
    | Changed of view: TextView * published: PublishedState voption
    /// A signature file with a newer text
    | SignatureChanged
    /// Its project was disposed
    | Gone

/// One file of a read made in one graph turn (ProjectGraph.ReadInOneTurn).
[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TurnFile =
    /// An implementation file's index of its published result, which is for text version `version`
    | Indexed of path: string * version: int64 * uses: FileUses
    /// A signature file's index of its latest check, which is for text version `version`
    | Signature of path: string * version: int64 * uses: FileUses
    /// Not read: its latest text, at `version`, doesn't contain the name asked for (TurnScope.Name)
    | Skipped of path: string * version: int64

/// What a read in one graph turn reads, as the caller finds it at that turn: asked once at every turn the read tries,
/// on the graph thread, in the same turn as the files.
[<NoComparison; NoEquality>]
type TurnScope<'Extra when 'Extra: equality> =
    { /// A load or reload is under way: the projects may lack those it hides, so the turn counts as pending
      Loading: bool
      /// The projects, each with the files to read of it, in the project's order
      Projects: (ProjectGraph * string list)[]
      /// When given, an implementation file whose latest text doesn't contain it is Skipped rather than read
      Name: string option
      /// Whatever else the caller read of the workspace for its answer, so it is the agreeing turn's too
      Extra: 'Extra }

/// <summary>What a read in one graph turn gives.</summary>
/// <remarks>
/// For a reader such as slice 4's request nodes: everything in `Read` is of one turn. TurnFile.Indexed's version and
/// each case's path name the file and the text its uses are for; Skipped's version is the text that decided the skip.
/// </remarks>
and [<RequireQualifiedAccess; NoComparison; NoEquality>] TurnRead<'Extra> =
    /// The agreeing scope's Extra, and each project's files in its order, every one settled on its latest text in the
    /// same turn: one state
    | Read of extra: 'Extra * files: (ProjectGraph * TurnFile[])[]
    /// A project in the scope was disposed: its nodes never change again, so no later turn could answer
    | Gone

/// Files read in one graph turn against text versions they had (ProjectGraph.FilesNow)
and [<NoEquality; NoComparison>] FilesNow =
    { /// For each graph asked, its generation in the turn and its files, in the order asked
      Graphs: struct (int64 * FileNow[])[]
      /// What the caller's `busy` and `pending` read in the same turn
      Busy: bool
      Pending: bool
      /// What the caller's `projects` (the loaded projects) read in the same turn
      Projects: ProjectGraph list }

/// <summary>
/// A project checked on a Ranvier graph, re-checking only what an edit can change.
/// </summary>
/// <remarks>
/// <para>
/// Every file starts checked whole (FileLevel). An implementation file that is promoted is built one declaration at
/// a time in the background, and then checked by declaration (Declarations) while its setup is unchanged.
/// </para>
/// <para>
/// FileLevel: each file is set up from the signatures of the files it depends on (FileDependencies over every file's
/// current parse), not from every earlier file. It is checked again when its text, its dependency list or one of those
/// signatures' fingerprints changes, so files that do not depend on each other check in parallel. A setup the checker
/// refuses because its dependency snapshot went stale, or one whose check reports an unresolved name (C3/R17), is
/// replaced by one from every earlier file.
/// </para>
/// <para>
/// Declarations: per implementation file, a keyed projection of the declarations, and per declaration its syntax,
/// content, dependencies and check; then a finish over the current results, whose diagnostics are the ones published.
/// A declaration re-checks when its syntax or place changes, when the fingerprint its check left changes for an
/// earlier declaration it depends on (by name, through TypedUses, or all of them for an unresolvable name), or when an
/// earlier declaration it depends on with no cutoff re-checks (its module header, an open or another declaration that
/// affects all later ones). Declarations whose results later checks change in place (R12), and types with the
/// augmentations that extend them (R11), are checked as a segment, in order. The file is set up from the signatures of
/// the files it depends on, as a file checked whole is. When its setup changes (a dependency's signature fingerprint,
/// its dependency list or its header: R16's restart), it is checked whole for that state, and built by declaration
/// again while it is still wanted.
/// </para>
/// <para>
/// References: a project's assembly is a memo over its files' settled signatures, cut off by its fingerprint. A project
/// that references loaded projects holds a project context made from their assemblies, every project it reaches
/// transitively, since a referenced assembly's pickle names the assemblies it references. When one of their fingerprints
/// changes, a new context is made and every file restarts on it, file-level (and promoted again if wanted): the file's
/// nodes capture the context, and its results hold the imported entities. A file of such a project is settled only
/// once the projects it references are, and its context is for their assemblies. A disposed graph disposes the graphs
/// that reference it, which would otherwise wait for it for ever.
/// </para>
/// </remarks>
and [<Sealed>] ProjectGraph private (host: GraphHost, project: ProjectShared, spec: ProjectSpec, initialFiles: IFileNode[], initialOwner: Owner, references: ProjectGraph list, firstSeen: ReferencesSeen, initialWanted: HashSet<int>) =
    let requested = Array.zeroCreate<int64> initialFiles.Length
    let lifetime = new CancellationTokenSource()
    let wanted = initialWanted
    // Replaced together by a restart, on the graph thread
    let mutable files = initialFiles
    let mutable owner = initialOwner
    let mutable seen = firstSeen
    let mutable generatedResults: Lazy<FileResult list> = Lazy<FileResult list>.CreateFromValue []
    let mutable filesNow: Signal<IFileNode[]> = Unchecked.defaultof<_>
    // Set when a restart fails, so a request node waiting on the disposed nodes runs again and fails
    let mutable restartFailed: Signal<bool> = Unchecked.defaultof<_>
    let mutable assemblyFlight: AsyncMemo<AssemblyState> = Unchecked.defaultof<_>
    let mutable assemblyMemo: Memo<AssemblyState> = Unchecked.defaultof<_>
    let mutable contextMemo: AsyncMemo<ReferencesSeen> = Unchecked.defaultof<_>
    let mutable linkOwner: Owner = Unchecked.defaultof<_>
    let dependents = ResizeArray<ProjectGraph>()
    let mutable disposed = 0
    // Restarts made, on the graph thread: every result after one is new, at the same text versions
    let mutable restarts = 0L
    let mutable afterSettle: unit -> Task = fun () -> Task.CompletedTask
    // Graph thread: runs after each file a read in one turn reads. For tests
    let mutable turnFileHook: TurnFile -> unit = ignore
    // Graph thread. A restart disposes the nodes first, so one that fails leaves nothing that could ever settle.
    let mutable restartFailure: exn = null
    // Work of this project that failed on the graph thread: a guarded effect of its nodes, or its posted work. Its own
    // waits end with it, and no other project's (N2).
    let mutable faults = 0
    let mutable lastFault: exn = null
    // Graph thread: the count as a node, written after the pump that counted, for readers that must follow it
    let mutable faultsNow: Signal<int> = Unchecked.defaultof<_>

    let fault (ex: exn) =
        lastFault <- ex
        Interlocked.Increment &faults |> ignore

        if not (isNull (box faultsNow)) then
            host.WhenSettled(
                (fun () ->
                    faultsNow.Value <- Volatile.Read &faults
                    ValueSome()),
                CancellationToken.None,
                true
            )
            |> ignore

    let post (work: unit -> unit) =
        host.Post(fun () ->
            try
                work ()
            with ex ->
                GraphClaims.claim ex
                fault ex
                reraise ())

    let restarting (work: unit -> unit) =
        try
            work ()
        with ex ->
            restartFailure <- ex

            if not (isNull (box restartFailed)) then
                restartFailed.Value <- true

            reraise ()

    // This project's faults and those of the projects it references, whose assemblies its files wait for
    let faultsSeen () =
        Volatile.Read &faults
        + (ProjectNodes.closure references (fun (g: ProjectGraph) -> g.References) |> List.sumBy _.FaultCount)

    // Every wait ends when the graph is disposed: its nodes are gone, so a wait on them could never settle. Apart from
    // the host's faults: one that a fault of this project, or of one it references, may have stranded fails with it.
    let settledWith (token: CancellationToken) (read: unit -> 'T voption) =
        let since = faultsSeen ()

        host.WhenSettled(
            (fun () ->
                let found =
                    try
                        read ()
                    with NotReadyException _ when faultsSeen () > since ->
                        ValueNone

                match found with
                | ValueNone when faultsSeen () > since ->
                    raise (GraphFailedException($"work of %s{spec.ProjectFile}", lastFault))
                | found -> found),
            token,
            true
        )

    let settled (read: unit -> 'T voption) = settledWith lifetime.Token read

    let byPath =
        let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
        initialFiles |> Array.iteri (fun i f -> d[Path.GetFullPath f.Path] <- i)
        d

    let indexOf (file: string) =
        match byPath.TryGetValue(Path.GetFullPath file) with
        | true, i -> i
        | _ -> invalidArg (nameof file) $"%s{file} is not a source file of the project"

    let isSignatureFile (f: IFileNode) =
        f.Path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

    // For DeclarationLines given a text the graph does not hold: parsing it on the graph's context would replace the
    // file's latest parse there, and the checker would then refuse every setup from dependencies after it.
    let linesContext =
        lazy (DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.RunSynchronously)

    /// Whether the projects this one references are settled, and its context is for their assemblies. Graph thread.
    let referencesCurrent () =
        references.IsEmpty
        || (match contextMemo.TryValue with
            | Failed e -> raise (InvalidOperationException($"%s{spec.ProjectFile}: making its project context failed", e))
            | _ -> ()

            let assemblies = references |> List.map (fun (r: ProjectGraph) -> r.SettledAssembly())

            assemblies |> List.forall _.IsSome
            && (assemblies |> List.map (fun a -> a.Value.Fingerprint)) = List.ofArray seen.Fingerprints)

    /// Reads file `i` once it is settled on its latest text, with every write posted to it and to the files before it
    /// applied, and the projects it references settled. Its result being settled means the files it was set up from are
    /// current too, transitively (IFileNode.Current). Every read is tracked, so a request node whose body calls it runs
    /// again when a guard that failed may pass: a write applied, a restart, a restart's failure.
    let checkRestart () =
        if not (isNull (box restartFailed)) then
            restartFailed.Value |> ignore

        if not (isNull restartFailure) then
            raise (InvalidOperationException("The project's nodes could not be made again after a reference changed.", restartFailure))

    let currentFiles () =
        checkRestart ()

        if not (isNull (box filesNow)) then
            filesNow.Value
        else
            files

    let readSettled (i: int) (read: IFileNode -> int64 -> 'T voption) : 'T voption =
        let current = currentFiles ()
        let mutable applied = true

        for j in 0..i do
            if current[j].Text.Text.Value.Version <> Volatile.Read &requested[j] then
                applied <- false

        if applied && referencesCurrent () then read current[i] (Volatile.Read &requested[i]) else ValueNone

    /// Reads every file once each is settled on its latest text.
    let settledAll (read: IFileNode -> int64 -> 'T voption) : 'T[] voption =
        let results = Array.zeroCreate files.Length
        let mutable ok = true
        let mutable i = 0

        while ok && i < files.Length do
            match readSettled i read with
            | ValueSome r -> results[i] <- r
            | ValueNone -> ok <- false

            i <- i + 1

        if ok then ValueSome results else ValueNone

    /// A graph whose every file starts file-level.
    static member Create(host: GraphHost, spec: ProjectSpec) : Task<ProjectGraph> =
        ProjectGraph.CreateWithMode(host, spec, FileLevel)

    /// A graph whose every file starts file-level, whose `-r:` to each of `references` (graphs on the same host) and the
    /// projects they reach is that project's assembly as it stands in memory. A reference to any other project stays
    /// the file on disk. Completes once the references' assemblies have settled for the first context.
    static member Create(host: GraphHost, spec: ProjectSpec, references: ProjectGraph list) : Task<ProjectGraph> =
        ProjectGraph.CreateWithMode(host, spec, FileLevel, references)

    /// A graph whose every file starts file-level; with `Declarations`, every implementation file is also promoted.
    static member internal CreateWithMode(host: GraphHost, spec: ProjectSpec, mode: Rfs.Checker.FileMode) : Task<ProjectGraph> =
        ProjectGraph.CreateWithMode(host, spec, mode, [])

    static member internal CreateWithMode(host: GraphHost, spec: ProjectSpec, mode: Rfs.Checker.FileMode, references: ProjectGraph list) : Task<ProjectGraph> =
        task {
            let options = ProjectSpec.toFcsOptions spec

            // Only the projects whose output `spec` references by path: the rest of the closure is not an assembly to it
            let linked =
                ProjectNodes.closure references (fun (g: ProjectGraph) -> g.References)
                |> List.choose (fun g -> ProjectSpec.referencePath spec g.Spec |> Option.map (fun path -> path, g))

            let! (first: ProjectAssembly[]) =
                if linked.IsEmpty then
                    Task.FromResult [||]
                else
                    // Apart from the host's faults: only those of the projects it waits for fail it
                    let faultsOf () = linked |> List.sumBy (fun (_, g: ProjectGraph) -> g.FaultCount)
                    let since = faultsOf ()

                    host.WhenSettled(
                        (fun () ->
                            let all = linked |> List.map (fun (_, g: ProjectGraph) -> g.SettledAssembly())

                            if all |> List.forall _.IsSome then
                                ValueSome(all |> List.map _.Value |> Array.ofList)
                            elif faultsOf () > since then
                                raise (GraphFailedException($"settling the references of %s{spec.ProjectFile}", InvalidOperationException "a referenced project failed"))
                            else
                                ValueNone),
                        CancellationToken.None,
                        true
                    )

            let allocated = GC.GetTotalAllocatedBytes true
            let started = Diagnostics.Stopwatch.GetTimestamp()
            let! ctx = ProjectGraph.MakeContext(spec, options, linked |> List.map fst, first)
            let project = ProjectShared ctx
            project.Post <- host.Post

            // Nothing else of the graph runs yet, so the process's allocations are the context's
            project.AddPhase("createProject", Diagnostics.Stopwatch.GetTimestamp() - started, GC.GetTotalAllocatedBytes true - allocated)
            let texts = spec.SourceFiles |> List.map (fun f -> f, File.ReadAllText f, 0L) |> Array.ofList
            let wanted = HashSet<int>()

            if mode = Declarations then
                for i in 0 .. texts.Length - 1 do
                    wanted.Add i |> ignore

            let seen = { Fingerprints = first |> Array.map _.Fingerprint; Context = ctx }
            let directs = linked |> List.map snd

            return!
                host.Read(fun () ->
                    let owner, files = ProjectNodes.build project texts wanted
                    let graph = new ProjectGraph(host, project, spec, files, owner, directs, seen, wanted)
                    graph.Attach()
                    graph.Link(options, linked |> List.map fst)

                    for r in directs do
                        r.AddDependent graph

                    graph)
        }

    static member private MakeContext(spec: ProjectSpec, options: FSharpProjectOptions, paths: string list, assemblies: ProjectAssembly[]) : Task<ProjectContext> =
        if paths.IsEmpty then
            DeclarationChecker.CreateProject options |> Async.StartAsTask
        else
            task {
                let references = Dictionary<string, ProjectAssembly>(StringComparer.Ordinal)
                List.iteri (fun i path -> references[path] <- assemblies[i]) paths

                // Pickles here, off the graph thread, and fails here
                do!
                    Task.Run(fun () ->
                        for KeyValue(path, assembly) in references do
                            if assembly.ReadsFileOnDisk then
                                eprintfn "[ranvierautocomplete] %s reads %s from disk as of its last build: that project generates types with a type provider" spec.ProjectFile path)

                return! DeclarationChecker.CreateProject(options, references) |> Async.StartAsTask
            }

    /// The attribute files the build generates into obj, checked on their own (ProjectSpec tells them by their content: a
    /// generated file with declarations is a source file): they add assembly attributes (a `namespace` holding nothing),
    /// which later files don't read, only the assembly's referencing projects. Their diagnostics are dropped:
    /// nothing is shown for a file the user has not written. Evaluating the project wrote them, so one that is gone
    /// is gone from the build too.
    static member private GeneratedResults(spec: ProjectSpec, ctx: ProjectContext) : Lazy<FileResult list> =
        lazy
            [ for path in spec.GeneratedFiles do
                  if File.Exists path then
                      DeclarationChecker.CheckFile(ctx, [], DeclarationChecker.Parse(ctx, path, File.ReadAllText path)) ]

    /// Makes the nodes that link this project to the ones it references and to those that reference it. Graph thread.
    member private this.Link(options: FSharpProjectOptions, paths: string list) =
        generatedResults <- ProjectGraph.GeneratedResults(spec, project.Context)

        linkOwner <-
            createRoot (fun o ->
                filesNow <- createSignal files
                restartFailed <- createSignal false

                if not references.IsEmpty then
                    // The context for the references as they stand; the current one while they settle or keep their fingerprints
                    contextMemo <-
                        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct ->
                            let states = Nodes.valuesOf (references |> List.map (fun (r: ProjectGraph) -> r.Assembly) |> Array.ofList)

                            let built =
                                states
                                |> Array.choose (function
                                    | Built a -> Some a
                                    | Unsettled -> None)

                            if built.Length < states.Length || (built |> Array.map _.Fingerprint) = seen.Fingerprints then
                                Task.FromResult seen
                            else
                                let started = Diagnostics.Stopwatch.GetTimestamp()

                                // Off the graph thread: on it the check would run before anything could supersede the flight
                                Task.Run<ReferencesSeen>(fun () ->
                                    task {
                                        project.FlightHook "context" spec.ProjectFile ct
                                        project.ThrowIfSuperseded("context", ct)
                                        let! ctx = ProjectGraph.MakeContext(spec, options, paths, built)
                                        project.AddPhase("createProject", Diagnostics.Stopwatch.GetTimestamp() - started, 0L)
                                        return { Fingerprints = built |> Array.map _.Fingerprint; Context = ctx }
                                    }))

                    GraphEffects.guardedFor fault (fun () ->
                        match contextMemo.TryValue with
                        | Ready next when not (obj.ReferenceEquals(next, seen)) -> project.Post(fun () -> restarting (fun () -> this.Restart next))
                        | _ -> ())

                o)

    /// The assembly's nodes, made when a project that references this one first asks: a project nothing references
    /// takes no fingerprint for them. Graph thread.
    member private _.AssemblyNodes() : Memo<AssemblyState> =
        if isNull (box assemblyMemo) then
            runWithOwner linkOwner (fun () ->
                // Off the graph thread: taking a fingerprint no check asked for describes the file, and hashes them all
                assemblyFlight <-
                    createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct ->
                        let nodes = filesNow.Value

                        // Until a restart, this project's files are checked against older assemblies
                        let current =
                            references
                            |> List.map (fun (r: ProjectGraph) ->
                                match r.Assembly.Value with
                                | Built a -> a.Fingerprint
                                | Unsettled -> "")
                            |> Array.ofList
                            |> (=) seen.Fingerprints

                        let results =
                            nodes
                            |> Array.map (fun f ->
                                let version = f.Text.Text.Value.Version

                                if f.Path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                                    f.Settled version |> ValueOption.map (fun _ -> None)
                                else
                                    f.FileResult version |> ValueOption.map Some)

                        if not current || results |> Array.exists _.IsNone then
                            Task.FromResult Unsettled
                        else
                            let signatures = nodes |> Array.map (fun f -> f.Signature.Value.Signature) |> List.ofArray
                            let results = results |> Array.choose _.Value |> List.ofArray
                            let ctx = project.Context
                            let generated = generatedResults

                            Task.Run<AssemblyState>(fun () ->
                                project.FlightHook "assembly" spec.ProjectFile ct
                                project.ThrowIfSuperseded("assembly", ct)
                                Built(DeclarationChecker.ProjectAssembly(ctx, generated.Value @ results, signatures))))

                // The cutoff at the project boundary
                assemblyMemo <-
                    createMemoWithComparer
                        { new IEqualityComparer<AssemblyState> with
                            member _.Equals(a, b) =
                                match a, b with
                                | Unsettled, Unsettled -> true
                                | Built x, Built y -> String.Equals(x.Fingerprint, y.Fingerprint, StringComparison.Ordinal)
                                | _ -> false

                            member _.GetHashCode _ = 0 }
                        (fun _ -> assemblyFlight.Value))

        assemblyMemo

    /// Every file starts again on `next`'s context, on its current text, with what is wanted wanted. Graph thread.
    member private _.Restart(next: ReferencesSeen) =
        if Volatile.Read &disposed = 0 && not (next.Fingerprints = seen.Fingerprints) then
            let texts =
                files
                |> Array.map (fun f ->
                    let t = f.Text.Text.Peek
                    f.Path, t.Text, t.Version)

            let shiftHook = project.Context.ShiftRefusedHook
            let refuseShifts = project.Context.RefuseShifts
            owner.Dispose()
            next.Context.ShiftRefusedHook <- shiftHook
            next.Context.RefuseShifts <- refuseShifts
            project.Context <- next.Context
            generatedResults <- ProjectGraph.GeneratedResults(spec, next.Context)
            // A root attaches to the scope it is made in: the links', not the effect's that asked for the restart, whose
            // next run would dispose the nodes
            let newOwner, nodes = runWithOwner linkOwner (fun () -> ProjectNodes.build project texts wanted)
            owner <- newOwner
            files <- nodes
            restarts <- restarts + 1L
            seen <- next
            filesNow.Value <- nodes

    member internal _.Spec: ProjectSpec = spec
    member internal _.References: ProjectGraph list = references
    member internal this.Assembly: Memo<AssemblyState> = this.AssemblyNodes()

    /// The project's assembly once every write posted to its files is applied, they are settled and so are the projects
    /// it references. Graph thread.
    member internal this.SettledAssembly() : ProjectAssembly voption =
        let mutable applied = true

        for j in 0 .. files.Length - 1 do
            if files[j].Text.Text.Value.Version <> Volatile.Read &requested[j] then
                applied <- false

        if applied && referencesCurrent () then
            match this.AssemblyNodes().TryValue with
            | Ready(Built a) -> ValueSome a
            | Failed e -> raise (InvalidOperationException($"%s{spec.ProjectFile}: its assembly failed", e))
            | _ -> ValueNone
        else
            ValueNone

    member internal _.AddDependent(graph: ProjectGraph) = lock dependents (fun () -> dependents.Add graph)

    member internal _.RemoveDependent(graph: ProjectGraph) = lock dependents (fun () -> dependents.Remove graph |> ignore)

    /// How many live graphs reference this one. For tests.
    member internal _.DependentCount: int = lock dependents (fun () -> dependents.Count)

    /// The first and last line (1-based) of each declaration `text` splits into as the file's text. Call it when the
    /// graph is idle, and never from the graph thread: it waits for the graph thread to read the file's parse. A text
    /// other than the one the graph holds is parsed on a second project context, created the first time and kept for
    /// the graph's lifetime (as much memory as the project's references take).
    member _.DeclarationLines(file: string, text: string) : (int * int)[] =
        let i = indexOf file

        let current =
            (host.Read(fun () ->
                match files[i].Text.Parsed.TryValue with
                | Ready p when p.Text = text -> Some p.Parse
                | _ -> None))
                .GetAwaiter()
                .GetResult()

        let parse =
            match current with
            | Some parse -> parse
            | None ->
                let ctx = linesContext.Value
                let parse = DeclarationChecker.Parse(ctx, file, text)
                // The line directives of a file are kept per process, by the latest parse of it: parse the text the
                // graph holds again, so its diagnostics render with its own.
                let held = (host.Read(fun () -> files[i].Text.Text.Peek.Text)).GetAwaiter().GetResult()
                DeclarationChecker.Parse(ctx, file, held) |> ignore
                parse

        DeclarationChecker.Split parse |> Array.map (fun d -> d.Range.StartLine, d.Range.EndLine)

    /// Sets a file's text; checks follow on the graph thread. Returns the version posted.
    member _.SetText(file: string, text: string) : int64 =
        let i = indexOf file
        let version = Interlocked.Increment &requested[i]

        // A disposed graph checks nothing
        if Volatile.Read &disposed = 0 then
            post (fun () -> files[i].Text.SetText(text, version))

        version

    /// The file's diagnostics, with the version of its text they are for, once its result is for its latest text and
    /// the files it depends on are settled.
    member _.SettledDiagnostics(file: string) : Task<struct (int64 * FSharpDiagnostic[])> =
        let i = indexOf file

        settled (fun () ->
            readSettled i (fun f version -> f.Settled version |> ValueOption.map (fun d -> struct (version, d))))

    /// <summary>
    /// Graph thread, tracked: the file's diagnostics with the version of its text, as <c>SettledDiagnostics</c> gives
    /// them, or ValueNone while they are not settled and once the graph is disposed. An effect that reads it runs again
    /// when a guard that failed may pass. Raises InvalidOperationException when the file's check, or the project's
    /// restart, failed.
    /// </summary>
    member _.TrySettledDiagnostics(file: string) : struct (int64 * FSharpDiagnostic[]) voption =
        if Volatile.Read &disposed <> 0 then
            ValueNone
        else
            let i = indexOf file
            readSettled i (fun f version -> f.Settled version |> ValueOption.map (fun d -> struct (version, d)))

    /// The file's diagnostics once its result is for its latest text and the files it depends on are settled.
    member _.Diagnostics(file: string) : Task<FSharpDiagnostic[]> =
        let i = indexOf file
        settled (fun () -> readSettled i (fun f version -> f.Settled version))

    /// Every file's diagnostics once all of them are settled on their latest texts.
    member _.AllDiagnostics() : Task<IReadOnlyDictionary<string, FSharpDiagnostic[]>> =
        settled (fun () ->
            settledAll (fun f version -> f.Settled version)
            |> ValueOption.map (fun all ->
                let d = Dictionary<string, FSharpDiagnostic[]>()

                for f, diagnostics in Array.zip files all do
                    d[f.Path] <- diagnostics

                d :> IReadOnlyDictionary<_, _>))

    /// Each declaration's current result in an implementation file checked by declaration, once settled as for
    /// `Diagnostics`. Fails with InvalidOperationException for a file checked file-level.
    member _.Results(file: string) : Task<IReadOnlyDictionary<DeclarationKey, DeclarationResult>> =
        let i = indexOf file

        if isSignatureFile files[i] then
            invalidArg (nameof file) $"%s{file} is a signature file"

        settled (fun () -> readSettled i (fun f version -> f.Results version))

    /// <summary>
    /// A request node (Request.run) on the file's published result, once it is settled on its latest text (as for
    /// `Diagnostics`): `prepare` runs off the graph thread on the text and parse, without the lock, then `read` under it on
    /// the view and what `prepare` gave.
    /// </summary>
    /// <remarks>
    /// For a file checked by declaration `read` holds the lock of the result's FileStart exclusively, so no check, shift
    /// or finish runs on it meanwhile, and the result is still the published one when the lock is taken; when it isn't,
    /// the request waits for the next turn. A new text of the file, or a newer result, runs the request again on it and
    /// cancels what ran on the older one, ad hoc checks inside it included (FileView.CheckResultsAt), so a request answers
    /// for the latest text once it has settled. A `read` refused because the file's #line directives are another parse's
    /// (a whole-checked result's check results, built on first read) fails the request at once with
    /// SupersededLineDirectivesException: the build caches its refusal, so trying again can't answer. Cancelling `ct`, or
    /// disposing the graph (a reload), cancels the task and nothing shared. Fails with ArgumentException for a signature
    /// file, which has no FileResult.
    /// </remarks>
    member this.ReadFile(file: string, prepare: TextView -> 'P, read: FileView -> 'P -> 'T, ct: CancellationToken) : Task<'T> =
        this.ReadFile(file, prepare, read, false, None, ct)

    /// ReadFile, with FileView.Inputs and Generation filled when `inputs` (empty and -1 otherwise)
    member this.ReadFile(file: string, prepare: TextView -> 'P, read: FileView -> 'P -> 'T, inputs: bool, ct: CancellationToken) : Task<'T> =
        this.ReadFile(file, prepare, read, inputs, None, ct)

    member _.ReadFile(file: string, prepare: TextView -> 'P, read: FileView -> 'P -> 'T, inputs: bool, onUnsettled: (unit -> unit) option, ct: CancellationToken) : Task<'T> =
        let wantsInputs = inputs
        let i = indexOf file

        if isSignatureFile files[i] then
            invalidArg (nameof file) $"%s{file} is a signature file"

        let readable () = readSettled i (fun f version -> (f :?> IReadableFile).Readable version)

        let fireUnsettled () =
            match onUnsettled with
            | Some f -> f ()
            | None -> ()

        let view (r: Readable) (inputs: int64[]) (generation: int64) (checks: CancellationToken) =
            { Inputs = inputs
              Generation = generation
              Result = r.Result
              Parse = r.Parsed.Parse
              Text = r.Parsed.Text
              Docs = r.Parsed.Docs
              Version = r.Parsed.Version
              Before = r.Key.Before
              Identity = r.Identity
              Whole = r.Lock.IsNone
              RowAt =
                fun line ->
                    match r.Declarations with
                    | ValueSome(f, _) ->
                        f.Checks
                        |> Array.tryFind (fun (_, c) -> c.Decl.Range.StartLine <= line && line <= c.Decl.Range.EndLine)
                        |> function
                            | Some(_, c) -> ValueSome(struct (box c.Result, c.Decl.Range.StartLine))
                            | None ->
                                if f.Checks.Length > 0 && line < (snd f.Checks[0]).Decl.Range.StartLine then
                                    ValueSome(struct (box f.Start, 1))
                                else
                                    ValueNone
                    | ValueNone -> ValueNone
              At =
                fun line column ->
                    match r.Declarations with
                    | ValueSome(f, roles) -> RowView.at project f roles line column checks
                    | ValueNone -> r.Result.CheckResults }

        let body (flight: CancellationToken) : Task<'T> =
            let readReadable () =
                try
                    readable ()
                with NotReadyException _ ->
                    fireUnsettled ()
                    ValueNone

            match readReadable () with
            | ValueNone ->
                fireUnsettled ()
                Request.waitForNextTurn flight
            | ValueSome r ->
                let hook = afterSettle
                // In the turn readSettled applied them in: file i's result is of these texts
                let inputs = if wantsInputs then Array.init (i + 1) (fun j -> files[j].Text.Text.Value.Version) else [||]
                let generation = if wantsInputs then restarts else -1L

                Task.Run<'T>(fun () ->
                    task {
                        do! hook ()
                        let v = view r inputs generation flight
                        let prepared = prepare ({ Parse = v.Parse; Text = v.Text; Docs = v.Docs; Version = v.Version }: TextView)

                        let answer () = ValueSome(read v prepared)

                        let inEffect () =
                            host.Read(fun () ->
                                match (try readable () with NotReadyException _ -> ValueNone) with
                                | ValueSome a ->
                                    if obj.ReferenceEquals(a.Identity, r.Identity) then
                                        true
                                    else
                                        fireUnsettled ()
                                        false
                                | ValueNone ->
                                    fireUnsettled ()
                                    false)

                        let! found =
                            match r.Lock with
                            | ValueNone -> Task.FromResult(answer ())
                            | ValueSome l -> Request.live (acquireExclusive l) inEffect answer flight

                        match found with
                        | ValueSome v -> return v
                        | ValueNone ->
                            fireUnsettled ()
                            return! Request.waitForNextTurn flight
                    })

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// A request node on the file's published result (the other overload), with nothing prepared off the lock.
    member this.ReadFile(file: string, read: FileView -> 'T, ct: CancellationToken) : Task<'T> =
        this.ReadFile(file, ignore, (fun view () -> read view), ct)

    /// <summary>
    /// Where the definition that `key` (DeclarationChecker.SymbolKey) names is now, for a symbol in the file `asking` whose
    /// own signature location is in the file `landed`. As FCS answers: a file after a signature sees that signature; in
    /// the implementation file, and for what it doesn't expose or when it didn't conform for errors, the definition's
    /// signature location in the file's current result says which. A range in a signature file only ever comes from the
    /// latest check of it. None when the project holds neither file, or they don't define the key, or a file defines it
    /// twice, as no current thing says which is meant: never the symbol's own location.
    /// </summary>
    /// <remarks>
    /// A location read off a symbol can be stale: a kept row holds an older check's objects, which conformance and shifts
    /// no longer reach, later files hold a signature whose print didn't change, and another project a pickle of one. This
    /// is what corrects them. A request node that reads the files' indexes (FileUses.DefinitionOf, SignatureDefinitionOf)
    /// in one graph turn, holding no lock: its answer is for the texts that turn has, which may be newer than the symbol's.
    /// </remarks>
    member this.CurrentLocation(asking: string, landed: string, key: string, ct: CancellationToken) : Task<FSharp.Compiler.Text.range option> =
        task {
            let! struct (found, _, _, _) = this.CurrentLocationAt(asking, landed, key, ct)
            return found
        }

    /// <summary>
    /// CurrentLocation, with what it was read from in its turn: the version readSettled gave for the range's file (-1 for
    /// no range), the text version of every file up to the last one read, and the graph's generation.
    /// </summary>
    member _.CurrentLocationAt
        (asking: string, landed: string, key: string, ct: CancellationToken)
        : Task<struct (FSharp.Compiler.Text.range option * int64 * int64[] * int64)> =
        // What each read of this body's run read: the file's index and the version readSettled gave
        let reads = ResizeArray<struct (int * int64)>()

        let holds (file: string) = byPath.ContainsKey(Path.GetFullPath file)
        let isSignature (file: string) = file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)
        let signature = Path.ChangeExtension(landed, ".fsi")
        let implementation = if isSignature landed then Path.ChangeExtension(landed, ".fs") else landed

        let sameFile (x: string) (y: string) =
            String.Equals(Path.GetFullPath x, Path.GetFullPath y, StringComparison.OrdinalIgnoreCase)

        let inImplementation (file: string) =
            let i = indexOf file

            readSettled i (fun f version ->
                (f :?> IReadableFile).Uses version
                |> ValueOption.map (fun struct (u, _) ->
                    reads.Add(struct (i, version))
                    u.DefinitionOf key))

        let inSignature (file: string) (conformed: bool) =
            let i = indexOf file

            readSettled i (fun f version ->
                (f :?> ISignatureFile).Uses version
                |> ValueOption.map (fun struct (_, u) ->
                    reads.Add(struct (i, version))
                    u.SignatureDefinitionOf(key, conformed)))

        let answer () : FSharp.Compiler.Text.range option voption =
            if isSignature landed && holds signature && not (sameFile asking implementation) then
                match inSignature signature false with
                | ValueSome None when holds implementation ->
                    // What the signature doesn't define. A range it does hold here is unread and old, and of what the
                    // signature just didn't answer for: nothing is better.
                    inImplementation implementation
                    |> ValueOption.map (function
                        | Some r when isSignature r.FileName -> None
                        | r -> r)
                | found -> found
            elif holds implementation then
                match inImplementation implementation with
                // The range's own file: a signature pairs with its implementation by module name, not by file name
                | ValueSome(Some r) when isSignature r.FileName -> if holds r.FileName then inSignature r.FileName true else ValueSome None
                | found -> found
            elif holds signature then
                inSignature signature false
            else
                ValueSome None

        let body (flight: CancellationToken) =
            reads.Clear()

            match answer () with
            | ValueSome found ->
                let last = if reads.Count = 0 then -1 else reads |> Seq.map (fun (struct (i, _)) -> i) |> Seq.max
                let inputs = Array.init (last + 1) (fun j -> files[j].Text.Text.Value.Version)

                let version =
                    match found with
                    | Some r when holds r.FileName ->
                        let i = indexOf r.FileName

                        match reads |> Seq.tryFind (fun (struct (j, _)) -> j = i) with
                        | Some(struct (_, v)) -> v
                        | None -> -1L
                    | _ -> -1L

                Task.FromResult(struct (found, version, inputs, restarts))
            | ValueNone -> Request.waitForNextTurn flight

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// <summary>
    /// A request node: the file's latest text and its parse, once parsed, read in one graph turn with whether the file's
    /// result is settled on that text (as ReadFile waits for) and the latest `///` comments of each of `docsOf` (a graph
    /// on this one's host, and its file), ValueNone for one whose parse is pending, its last published result, and
    /// `busy`, a tracked read of the host's graph. Never waits for a check.
    /// </summary>
    member this.LatestText(file: string, docsOf: (ProjectGraph * string)[], busy: unit -> bool, ct: CancellationToken) : Task<LatestText> =
        this.LatestText(file, docsOf, busy, (fun () -> false), ct)

    /// LatestText, with `pending`, another tracked read of the host's graph, read in the same turn
    member _.LatestText(file: string, docsOf: (ProjectGraph * string)[], busy: unit -> bool, pending: unit -> bool, ct: CancellationToken) : Task<LatestText> =
        let i = indexOf file

        if isSignatureFile files[i] then
            invalidArg (nameof file) $"%s{file} is a signature file"

        let body (flight: CancellationToken) =
            let text = files[i].Text.Text.Value
            let p = files[i].Text.Parsed.Value

            if text.Version <> Volatile.Read &requested[i] || p.Version <> text.Version then
                Request.waitForNextTurn flight
            else
                let settled =
                    match (try readSettled i (fun f version -> (f :?> IReadableFile).Readable version) with NotReadyException _ -> ValueNone) with
                    | ValueSome _ -> true
                    | ValueNone -> false

                Task.FromResult
                    { View = { Parse = p.Parse; Text = p.Text; Docs = p.Docs; Version = p.Version }
                      Settled = settled
                      Published = (files[i] :?> IReadableFile).LastPublished()
                      Busy = busy ()
                      Pending = pending ()
                      Docs = docsOf |> Array.map (fun (g: ProjectGraph, f) -> g.DocsNow f) }

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// <summary>
    /// A request node: each of `graphs` (on this one's host) with its files and the text versions they had, read in one
    /// graph turn with the graph's generation, `busy` and `pending` (tracked reads of the host's graph). A changed
    /// implementation file gives its latest text once parsed, and its last published result. Never waits for a check.
    /// </summary>
    member _.FilesNow
        (
            graphs: struct (ProjectGraph * struct (string * int64)[])[],
            busy: unit -> bool,
            pending: unit -> bool,
            projects: unit -> ProjectGraph list,
            ct: CancellationToken
        ) : Task<FilesNow> =
        for struct (g, _) in graphs do
            if not (obj.ReferenceEquals(g.Host, host)) then
                invalidArg (nameof graphs) $"%s{g.Spec.ProjectFile} is on another host: one turn can't read it"

        let body (flight: CancellationToken) =
            let read =
                graphs |> Array.map (fun struct (g, fs) -> struct (g.Restarts, fs |> Array.map (fun struct (f, version) -> g.FileNow(f, version))))

            if read |> Array.exists (fun struct (_, fs) -> fs |> Array.exists ValueOption.isNone) then
                Request.waitForNextTurn flight
            else
                Task.FromResult
                    { Graphs = read |> Array.map (fun struct (g, fs) -> struct (g, fs |> Array.map _.Value))
                      Busy = busy ()
                      Pending = pending ()
                      Projects = projects () }

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// The project's source files, in order: what FileView.Inputs and CurrentLocationAt's versions are of
    member _.FilesOf: string[] = files |> Array.map _.Path

    /// Graph thread: how many times the project's nodes were made again (a restart). Results after one are new at the
    /// same text versions.
    member _.Restarts: int64 = restarts

    /// Graph thread, inside one turn: the file against `version`, or ValueNone while a newer text is posted but not
    /// applied, or its parse is pending
    member internal _.FileNow(file: string, version: int64) : FileNow voption =
        if Volatile.Read &disposed <> 0 then
            ValueSome FileNow.Gone
        else
            match byPath.TryGetValue(Path.GetFullPath file) with
            | false, _ -> ValueSome FileNow.Gone
            | true, i ->
                let text = files[i].Text.Text.Value

                if text.Version <> Volatile.Read &requested[i] then ValueNone
                elif text.Version = version then ValueSome FileNow.Same
                elif isSignatureFile files[i] then ValueSome FileNow.SignatureChanged
                else
                    let p = files[i].Text.Parsed.Value

                    if p.Version <> text.Version then
                        ValueNone
                    else
                        ValueSome(FileNow.Changed({ Parse = p.Parse; Text = p.Text; Docs = p.Docs; Version = p.Version }, (files[i] :?> IReadableFile).LastPublished()))

    /// Whether the project has the file
    member _.Holds(file: string) : bool =
        try
            byPath.ContainsKey(Path.GetFullPath file)
        with :? ArgumentException | :? NotSupportedException | :? PathTooLongException ->
            false

    /// Whether the two graphs share a host, so one turn reads both
    member _.SharesHost(other: ProjectGraph) : bool = obj.ReferenceEquals(host, other.Host)

    /// Graph thread, inside one turn: the `///` comments of the file's latest parse, or ValueNone while it is pending
    member internal _.DocsNow(file: string) : FileDocs voption =
        try
            ValueSome files[indexOf file].Text.Docs.Value
        with NotReadyException _ ->
            ValueNone

    /// <summary>
    /// The `///` comments of each of `files` (a graph that holds it, and the file), read in one graph turn in which the
    /// file `asking` still publishes the result `identity` names (FileView.Identity): a request node. Each is of that
    /// file's latest parse (FileDocs), which no check is waited for. ValueNone when `asking` publishes another result by
    /// then, or none, so its read is to be made again.
    /// </summary>
    /// <remarks>
    /// One turn is one state across the host's graphs: a file whose latest parse changed its signature leaves `asking`
    /// unsettled in that turn, so the docs read with a settled `asking` have the declarations its check named. Every
    /// graph in `files` must be on this one's host.
    /// </remarks>
    member _.DocsInTurn(asking: string, identity: obj, files: (ProjectGraph * string)[], ct: CancellationToken) : Task<FileDocs[] voption> =
        let i = indexOf asking

        let body (flight: CancellationToken) =
            match (try readSettled i (fun f version -> (f :?> IReadableFile).Readable version) with NotReadyException _ -> ValueNone) with
            | ValueSome r when obj.ReferenceEquals(r.Identity, identity) ->
                let docs = files |> Array.map (fun (g, f) -> g.DocsNow f)

                if docs |> Array.forall _.IsSome then
                    Task.FromResult(ValueSome(docs |> Array.map _.Value))
                else
                    Request.waitForNextTurn flight
            | _ -> Task.FromResult ValueNone

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    member internal _.Host: GraphHost = host

    /// Whether Dispose has begun. Read on the graph thread inside a turn, after which none of its nodes changes.
    member internal _.IsDisposed: bool = Volatile.Read &disposed <> 0

    /// <summary>
    /// Graph thread, inside one turn: each of `paths` (files of this project) as TurnFile, or ValueNone while one is
    /// pending. Stops at the first pending file, so a first read starts one file's index build at a time, as a scan did.
    /// </summary>
    /// <remarks>
    /// A file is read with readSettled's guards: every write posted to it and the files before it applied, and the
    /// projects it references settled; and its node's own (version equals the one requested, Current). A Skipped file
    /// needs only its own latest text applied: it has no use of a name its text doesn't have, whatever its result.
    /// </remarks>
    member internal _.FilesInTurn(paths: string list, name: string option) : TurnFile[] voption =
        let read = ResizeArray<TurnFile>()
        let mutable pending = false
        let mutable rest = paths

        while not pending && not rest.IsEmpty do
            let path = rest.Head
            rest <- rest.Tail
            let i = indexOf path

            let skipped =
                match name with
                | Some n when not (isSignatureFile files[i]) ->
                    let t = files[i].Text.Text.Value

                    if t.Version <> Volatile.Read &requested[i] then
                        pending <- true
                        false
                    else
                        not (t.Text.Contains(n, StringComparison.Ordinal))
                | _ -> false

            if skipped then
                read.Add(TurnFile.Skipped(path, files[i].Text.Version))
                turnFileHook read[read.Count - 1]
            elif not pending then
                let file =
                    if isSignatureFile files[i] then
                        readSettled i (fun f version -> (f :?> ISignatureFile).Uses version |> ValueOption.map (fun struct (_, u) -> TurnFile.Signature(path, version, u)))
                    else
                        readSettled i (fun f version -> (f :?> IReadableFile).Uses version |> ValueOption.map (fun struct (u, _) -> TurnFile.Indexed(path, version, u)))

                match file with
                | ValueSome f ->
                    read.Add f
                    turnFileHook f
                | ValueNone -> pending <- true

        if pending then ValueNone else ValueSome(read.ToArray())

    /// <summary>
    /// Reads everything `scope` names in one graph turn, waiting for a turn in which all of it is settled: each project's
    /// files as TurnFile, with the project set and loading state `scope` gives in that same turn. All graphs on a host
    /// share its one thread, so one turn is one state across projects; every graph `scope` names must be on this one's host.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A file pending or a guard failing (FilesInTurn) leaves the read waiting for a later turn, with no time limit: only
    /// `ct` ends it. A faulted node, or a check that failed, faults the task with its exception, and so does a failure of
    /// work on the graph thread (GraphHost.WhenSettled). A file with F# errors has a result like any other.
    /// </para>
    /// <para>
    /// `scope` runs on the graph thread once at each turn tried, in the same turn as the files: what it reads must
    /// change only on the graph thread (Workspace's state does), so it is that turn's. A loading scope is pending. A
    /// disposed graph in the scope gives Gone at the first turn that sees it outside a load (a loading scope names no
    /// projects), as its nodes never change; a disposal wakes the waiter, as it posts to the graph thread. A fault of a
    /// project in the scope, after the read first saw it, fails the read; a fault of any other project doesn't.
    /// </para>
    /// </remarks>
    member _.ReadInOneTurn(scope: unit -> TurnScope<'Extra>, ct: CancellationToken) : Task<TurnRead<'Extra>> =
        // Each project's faults when the read first saw it: a later one may have stranded the read, and fails it
        let faultsAt = Dictionary<ProjectGraph, int>(HashIdentity.Reference)

        let step () =
            let before = scope ()

            for g, _ in before.Projects do
                match faultsAt.TryGetValue g with
                | true, n when g.FaultCount > n ->
                    raise (GraphFailedException($"work of %s{g.Spec.ProjectFile}", InvalidOperationException "a project the read reads failed"))
                | true, _ -> ()
                | _ -> faultsAt[g] <- g.FaultCount

            if before.Projects |> Array.exists (fun (g, _) -> g.IsDisposed) then
                ValueSome TurnRead.Gone
            elif before.Loading then
                ValueNone
            else
                for g, _ in before.Projects do
                    if not (obj.ReferenceEquals(g.Host, host)) then
                        invalidOp $"%s{g.Spec.ProjectFile} is on another host: one turn can't read it"

                let read = Array.zeroCreate before.Projects.Length
                let mutable pending = false
                let mutable p = 0

                while not pending && p < read.Length do
                    let g, files = before.Projects[p]

                    match g.FilesInTurn(files, before.Name) with
                    | ValueSome r -> read[p] <- (g, r)
                    | ValueNone -> pending <- true

                    p <- p + 1

                if pending then ValueNone else ValueSome(TurnRead.Read(before.Extra, read))

        host.WhenSettled(step, ct, true)

    /// Graph thread, inside one turn: each of `paths` (files of this project) with its latest parsed TextView, or
    /// ValueNone while any posted text or parse is pending. Never waits on typecheck.
    member internal _.LatestParsesOfInTurn(paths: string list) : (string * TextView)[] voption =
        let current = currentFiles ()
        let read = ResizeArray<string * TextView>()
        let mutable pending = false
        let mutable rest = paths

        while not pending && not rest.IsEmpty do
            let path = rest.Head
            rest <- rest.Tail
            let i = indexOf path
            let f = current[i]
            let text = f.Text.Text.Value

            if text.Version <> Volatile.Read &requested[i] then
                pending <- true
            else
                match f.Text.Parsed.TryValue with
                | Ready p when p.Version = text.Version ->
                    read.Add(path, { Parse = p.Parse; Text = p.Text; Docs = p.Docs; Version = p.Version })
                | Failed e -> raise (InvalidOperationException($"%s{path} failed to parse", e))
                | _ -> pending <- true

        if pending then ValueNone else ValueSome(read.ToArray())

    /// Latest text and parse for an `.fs` or `.fsi` file once parsed, never waiting on typecheck.
    member _.LatestParse(file: string, ct: CancellationToken) : Task<TextView> =
        let i = indexOf file

        let body (flight: CancellationToken) =
            let current = currentFiles ()
            let f = current[i]
            let text = f.Text.Text.Value

            if text.Version <> Volatile.Read &requested[i] then
                Request.waitForNextTurn flight
            else
                let parsed =
                    try
                        ValueSome f.Text.Parsed.Value
                    with NotReadyException _ ->
                        ValueNone

                match parsed with
                | ValueSome p when p.Version = text.Version ->
                    Task.FromResult { Parse = p.Parse; Text = p.Text; Docs = p.Docs; Version = p.Version }
                | _ ->
                    Request.waitForNextTurn flight

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// Settled `.fsi` signature file read: text/parse view, FileSignature, and FileUses in one consistent state.
    member _.ReadSignature(file: string, ct: CancellationToken) : Task<struct (TextView * FileSignature * FileUses)> =
        let i = indexOf file

        if not (isSignatureFile files[i]) then
            invalidArg (nameof file) $"%s{file} is not a signature file"

        let body (flight: CancellationToken) =
            let settled =
                try
                    readSettled i (fun f version ->
                        let p = f.Text.Parsed.Value

                        if p.Version <> version then
                            ValueNone
                        else
                            (f :?> ISignatureFile).Uses version
                            |> ValueOption.map (fun struct (sigResult, uses) ->
                                let view: TextView = { Parse = p.Parse; Text = p.Text; Docs = p.Docs; Version = p.Version }
                                struct (view, sigResult, uses)))
                with NotReadyException _ ->
                    ValueNone

            match settled with
            | ValueSome found -> Task.FromResult found
            | ValueNone -> Request.waitForNextTurn flight

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// Settled `.fsi` signature file read running `prepare` and `DeclarationChecker.SignatureUses` off the graph thread,
    /// returning plain data `'T` only while the signature result remains current.
    member _.ReadSignature<'P, 'T>
        (
            file: string,
            prepare: TextView -> 'P,
            read: TextView -> 'P -> FileUses -> (struct (FSharp.Compiler.Text.range * bool * FSharp.Compiler.Symbols.FSharpSymbol))[] -> 'T,
            ct: CancellationToken
        ) : Task<'T> =
        let i = indexOf file

        if not (isSignatureFile files[i]) then
            invalidArg (nameof file) $"%s{file} is not a signature file"

        let readable () =
            try
                readSettled i (fun f version ->
                    let p = f.Text.Parsed.Value

                    if p.Version <> version then
                        ValueNone
                    else
                        (f :?> ISignatureFile).Uses version
                        |> ValueOption.map (fun struct (sigResult, uses) ->
                            let view: TextView = { Parse = p.Parse; Text = p.Text; Docs = p.Docs; Version = p.Version }
                            struct (view, sigResult, uses, project.Context)))
            with NotReadyException _ ->
                ValueNone

        let body (flight: CancellationToken) : Task<'T> =
            match readable () with
            | ValueNone -> Request.waitForNextTurn flight
            | ValueSome(struct (view, sigResult, uses, ctx)) ->
                Task.Run<'T>(fun () ->
                    task {
                        let prepared = prepare view
                        let sigUses = DeclarationChecker.SignatureUses(ctx, sigResult)
                        let computed = read view prepared uses sigUses

                        let! stillCurrent =
                            host.Read(fun () ->
                                match readable () with
                                | ValueSome(struct (_, currentSig, _, _)) -> obj.ReferenceEquals(currentSig, sigResult)
                                | ValueNone -> false)

                        if stillCurrent then
                            return computed
                        else
                            return! Request.waitForNextTurn flight
                    })

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    member this.ReadSignature<'T>
        (
            file: string,
            read: TextView -> FileUses -> (struct (FSharp.Compiler.Text.range * bool * FSharp.Compiler.Symbols.FSharpSymbol))[] -> 'T,
            ct: CancellationToken
        ) : Task<'T> =
        this.ReadSignature(file, ignore, (fun view () uses sigUses -> read view uses sigUses), ct)

    /// Settled lookup of a signature declaration's implementation site in its paired `.fs` file in this project.
    member _.ImplementationLocation(signatureFile: string, key: string, ct: CancellationToken) : Task<FSharp.Compiler.Text.range option> =
        let holds (file: string) = byPath.ContainsKey(Path.GetFullPath file)
        let sameFile (x: string) (y: string) =
            String.Equals(Path.GetFullPath x, Path.GetFullPath y, StringComparison.OrdinalIgnoreCase)

        let implementation = Path.ChangeExtension(signatureFile, ".fs")

        let readImpl (file: string) =
            let i = indexOf file

            try
                readSettled i (fun f version ->
                    (f :?> IReadableFile).Uses version
                    |> ValueOption.map (fun struct (u, _) -> u.ImplementationDefinitionOf(key, true)))
            with NotReadyException _ ->
                ValueNone

        let answer () : FSharp.Compiler.Text.range option voption =
            if holds implementation && not (isSignatureFile files[indexOf implementation]) then
                readImpl implementation
            elif holds signatureFile then
                let sigIndex = indexOf signatureFile
                let mutable pending = false
                let mutable found: FSharp.Compiler.Text.range option = None
                let mutable idx = sigIndex + 1

                while not pending && found.IsNone && idx < files.Length do
                    if not (isSignatureFile files[idx]) then
                        try
                            match readSettled idx (fun f version -> (f :?> IReadableFile).Uses version) with
                            | ValueNone -> pending <- true
                            | ValueSome(struct (u, _)) ->
                                match u.DefinitionOf key with
                                | Some sigRange when sameFile sigRange.FileName signatureFile ->
                                    found <- u.ImplementationDefinitionOf(key, true)
                                | _ -> ()
                        with NotReadyException _ ->
                            pending <- true

                    idx <- idx + 1

                if pending then ValueNone else ValueSome found
            else
                ValueSome None

        let body (flight: CancellationToken) =
            match answer () with
            | ValueSome found -> Task.FromResult found
            | ValueNone -> Request.waitForNextTurn flight

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            return! Request.run host body linked.Token
        }

    /// Latest parsed TextViews across workspace graphs on the same GraphHost in one turn, never waiting on typecheck.
    member _.LatestParsesInOneTurn
        (scope: unit -> struct (bool * (ProjectGraph * string list)[]), ct: CancellationToken)
        : Task<(ProjectGraph * (string * TextView)[])[] voption> =
        let faultsAt = Dictionary<ProjectGraph, int>(HashIdentity.Reference)

        let step () : (ProjectGraph * (string * TextView)[])[] voption voption =
            let struct (loading, projects) = scope ()

            for g, _ in projects do
                match faultsAt.TryGetValue g with
                | true, n when g.FaultCount > n ->
                    raise (GraphFailedException($"work of %s{g.Spec.ProjectFile}", InvalidOperationException "a project the read reads failed"))
                | true, _ -> ()
                | _ -> faultsAt[g] <- g.FaultCount

            if projects |> Array.exists (fun (g, _) -> g.IsDisposed) then
                ValueSome ValueNone
            elif loading then
                ValueNone
            else
                for g, _ in projects do
                    if not (obj.ReferenceEquals(g.Host, host)) then
                        invalidOp $"%s{g.Spec.ProjectFile} is on another host: one turn can't read it"

                let read = Array.zeroCreate projects.Length
                let mutable pending = false
                let mutable p = 0

                while not pending && p < read.Length do
                    let g, paths = projects[p]

                    match g.LatestParsesOfInTurn paths with
                    | ValueSome r -> read[p] <- (g, r)
                    | ValueNone -> pending <- true

                    p <- p + 1

                if pending then ValueNone else ValueSome(ValueSome read)

        host.WhenSettled(step, ct, true)

    /// How many indexes of a file's uses were built.
    member _.UsesBuilt: int64 = project.UsesBuilt

    /// How the file's published result was checked: FileLevel while it is pending (mid-edit), and while it is CheckFile's
    /// or a demoted file's kept one.
    member _.Mode(file: string) : Task<Rfs.Checker.FileMode> =
        let i = indexOf file
        host.Read(fun () -> files[i].Mode)

    /// A declaration's key as the trace labels and `Why`'s answers write it: its module path and name, then `:Kind`, then `(n)` for the nth
    /// of the same module path, name and kind after the first. Ranvier writes a label in a node's path with `/ # [ ] @ \`
    /// each prefixed by a backslash.
    static member DescribeKey(key: DeclarationKey) : string = Labels.key key

    /// Whether this build records why nodes ran (RANVIER_TRACE), so `Why` can answer.
    static member Traced: bool =
#if RANVIER_TRACE
        true
#else
        false
#endif

    /// Why declaration `key` of `file` last checked: the cause chain of its check, as Ranvier renders it, with the
    /// fingerprints and texts that changed named by the labels `fingerprint <file> <key>`. Read on the graph thread like any
    /// other read, after the posts already queued. None in an untraced build, for a file not checked by declaration, for a
    /// declaration it does not have, and for one whose check the log holds no run of. It is a graph turn of its own after the
    /// settle that found the key, so an edit between the two is asked about under the older key; and it renders the whole
    /// retained log on the graph thread, which stalls every other query meanwhile: a traced (Debug) build's cost.
    member _.Why(file: string, key: DeclarationKey) : Task<string option> =
#if RANVIER_TRACE
        let i = indexOf file

        host.Read(fun () ->
            match files[i].CheckNode key with
            | ValueSome node ->
                let graph = Graph.Current

                // Only `why` raises ArgumentException for a node with no run in the log; `render` is left to fail loudly
                let chain =
                    try
                        ValueSome(Trace.why graph node)
                    with :? ArgumentException ->
                        ValueNone

                chain |> ValueOption.map (fun c -> Trace.render graph (box c)) |> ValueOption.toOption
            | ValueNone -> None)
#else
        Task.FromResult None
#endif

#if RANVIER_TRACE
    /// For tests: the label Ranvier's log holds for each node the labels `start`, `finish` and `filelevel` are meant
    /// for, by that word, in `file`.
    member internal _.TracedLabels(file: string) : Task<(string * string option) list> =
        let i = indexOf file

        host.Read(fun () ->
            let graph = Graph.Current

            files[i].TracedNodes()
            |> List.map (fun (role, node) -> role, (Trace.origin graph node).Label))
#endif

    /// Marks an implementation file wanted (a signature file is never promoted): it is built by declaration in the
    /// background, one file of the project at a time, and checked by declaration once that build has finished for
    /// its current text and setup. Until then, and after any change of its setup, it is checked whole. A file whose
    /// text splits into one declaration (module rec, namespace rec) stays file-level. Posts; graph thread.
    member _.Promote(file: string) : unit =
        let i = indexOf file

        post (fun () ->
            wanted.Add i |> ignore
            files[i].Promote())

    /// Clears wanted: the file's declaration results and nodes are dropped, its last result stays published until its
    /// text or setup changes, and from then on it is checked whole. Posts; graph thread.
    member _.Demote(file: string) : unit =
        let i = indexOf file

        post (fun () ->
            wanted.Remove i |> ignore
            files[i].Demote())

    /// Completes, with the mode it settled in, once promoting the file has settled for its latest text: Declarations
    /// once it is checked by declaration for that text; FileLevel when it can not be for that text and setup (its text
    /// splits into one declaration, its build failed, or the setup from its dependencies reports an unresolved name,
    /// and it is then checked whole), and at once for a file that is not wanted.
    member _.WhenPromoted(file: string, ct: CancellationToken) : Task<Rfs.Checker.FileMode> =
        let i = indexOf file

        if ct.CanBeCanceled then
            let linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            let wait = settledWith linked.Token (fun () -> readSettled i (fun f version -> f.Promoted version))
            wait.ContinueWith((fun (_: Task) -> linked.Dispose()), TaskScheduler.Default) |> ignore
            wait
        else
            settled (fun () -> readSettled i (fun f version -> f.Promoted version))

    /// Every implementation file's FileResult (FinishFile's while it is checked by declaration, CheckFile's while it
    /// is checked whole) once all files are settled on their latest texts.
    member _.AllFileResults() : Task<IReadOnlyDictionary<string, FileResult>> =
        settled (fun () ->
            settledAll (fun f version ->
                if isSignatureFile f then
                    f.Settled version |> ValueOption.map (fun _ -> None)
                else
                    f.FileResult version |> ValueOption.map Some)
            |> ValueOption.map (fun all ->
                let d = Dictionary<string, FileResult>()

                for f, result in Array.zip files all do
                    result |> Option.iter (fun r -> d[f.Path] <- r)

                d :> IReadOnlyDictionary<_, _>))


    /// How many declarations of the file are checked in segments (R11 and R12 spans), as the roles stand now. Blocks
    /// until the graph thread answers: never call it from the graph thread.
    member _.SegmentRows(file: string) : int =
        let i = indexOf file
        (host.Read(fun () -> files[i].SegmentRows())).GetAwaiter().GetResult()

    /// How many declarations of the project are checked in segments, as the roles stand now. Blocks until the graph
    /// thread answers: never call it from the graph thread.
    member _.SegmentRowsTotal: int =
        (host.Read(fun () -> files |> Array.sumBy _.SegmentRows())).GetAwaiter().GetResult()

    /// How many times CheckFile has returned, including runs whose result was never published.
    member _.FileLevelChecks: int64 = project.FileLevelChecks

    /// How many setups from a file's dependencies the checker refused because its dependency snapshot was no longer
    /// current, each then checked from every earlier file.
    member _.ChainFallbacks: int64 = project.ChainFallbacks

    /// The results FinishFile reused from a file's previous finish (FileResult.Reused), summed over every finish.
    member _.FinishReused: int64 = project.FinishReused

    /// How many times a file-level check was tried again because its file's #line directives were a newer parse's.
    member internal _.DirectiveRetries: int64 = project.DirectiveRetries

    /// How many times a file's declaration nodes were created (one per promotion of a file that splits into more than
    /// one declaration). For tests.
    member internal _.DeclarationRoots: int64 = project.DeclarationRoots

    /// How many times CheckDeclaration has returned, including runs whose result was never published: a flight
    /// superseded under CancelPrevious that finished before it saw its cancellation.
    member _.CheckedCount: int64 = project.CheckedCount

    /// How many CheckDeclaration results were published (adopted): each once, when a finish of the current text that
    /// holds it is published.
    member _.AdoptedChecks: int64 = project.AdoptedCount

    /// How many declarations were checked for a query at a position and not adopted (FileView.CheckResultsAt).
    member _.AdHocChecks: int64 = project.AdHocCount

    /// How many published results were shifted to the line their unchanged declaration moved to (W1) instead of
    /// checked again: each once, when a finish of the current text that holds it is published.
    member _.ShiftedRows: int64 = project.ShiftedCount
    /// ExtendEnv calls assembling environments before checks
    member _.EnvExtends: int64 = project.EnvExtendCount

    /// Runs `hook`, off the graph thread, with the reason each time the checker refuses to shift one of the project's
    /// declarations (W1). For diagnosis.
    member _.OnShiftRefused(hook: string -> unit) : unit = project.Context.ShiftRefusedHook <- hook

    /// Stopwatch ticks spent inside DeclarationChecker.FinishFile, summed over every call.
    member _.FinishTicks: int64 = project.FinishTicks

    /// Bytes allocated by each phase of the file-level graph (createProject, parse, fileDependencies, checkFile,
    /// checkSignatureFile), summed over every run, counted on the thread that ran it (createProject: the process's).
    member _.PhaseAllocated: IReadOnlyDictionary<string, int64> = project.PhaseAllocated

    /// Stopwatch ticks of each part of FinishFile (FileResult.PhaseTicks), summed over every call. For a file-level
    /// graph: createProject, parse, fileDependencies, checkFile (each CheckFile call whole), CheckFile's own `tail`
    /// (its CheckOneImplFile) and checkSignatureFile.
    member _.FinishPhaseTicks: IReadOnlyDictionary<string, int64> = project.FinishPhaseTicks

    /// How many published results of declarations checked on their own had a final fingerprint other than the one
    /// their check left (the D1 premise broke). Such a declaration is then checked as a segment leader.
    member _.PremiseBreaks: int64 = project.PremiseBreaks

    /// Each declaration whose final fingerprint broke the D1 premise, with its file and both fingerprints, when it is
    /// published.
    member _.PremiseBroken: IObservable<PremiseBreak> = project.PremiseBroken

    /// Each CheckDeclaration that returned, with its file, on the thread that ran it.
    member _.Ran: IObservable<DeclarationKey * string> = project.Ran

    /// Each newly published declaration result, with its file, on the graph thread.
    member _.Checked: IObservable<DeclarationKey * string> = project.Checked

    /// How many parses, dependency computations and whole-file checks have started. Read without touching the graph.
    /// For tests.
    member internal _.MeasuresStarted: int64 = project.MeasuresStarted

    /// How many of those are running. Read without touching the graph. For tests.
    member internal _.MeasuresInFlight: int64 = project.MeasuresInFlight

    /// Ends every wait in flight (they are canceled) and disposes the graph's nodes on the graph thread. Idempotent,
    /// and safe once the host is disposed, which has disposed the graph already.
    member this.Dispose() =
        if Interlocked.Exchange(&disposed, 1) = 0 then
            lifetime.Cancel()

            for d in lock dependents (fun () -> dependents.ToArray()) do
                d.Dispose()

            // A reference outlives this graph on a reload: it must not keep this context alive
            for r in references do
                r.RemoveDependent this

            try
                host.Post(fun () ->
                    owner.Dispose()
                    linkOwner.Dispose())
            with :? ObjectDisposedException ->
                ()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    /// The project's checker context. For tests.
    member internal _.Context: ProjectContext = project.Context

    /// Fails as a restart does when it throws. For tests.
    member internal _.FailRestart(ex: exn) : unit = post (fun () -> restarting (fun () -> raise ex))

    /// Fails posted work of the project, as a failing SetText would. For tests.
    member internal _.FailPost(ex: exn) : unit = post (fun () -> raise ex)

    /// How many times work of the project failed on the graph thread (its effects, its posted work). Any thread.
    member internal _.FaultCount: int = Volatile.Read &faults

    /// The faults of the project and of the projects it references, which its waits end with: a wait that ended while
    /// this stayed the same was ended by a failure no project claimed. Any thread.
    member _.FaultsSeen: int = faultsSeen ()

    /// Makes the project's own work report its faults to its waits. Graph thread, before anything of it runs.
    member private _.Attach() =
        faultsNow <- createSignal 0
        project.Post <- post
        project.Fault <- fault

    /// <summary>
    /// Graph thread, tracked: how many times work of the project failed on the graph thread (its nodes' effects, its
    /// posted work). A reader that keeps what a file last found can tell from it that the file's pending check may never
    /// settle.
    /// </summary>
    member _.FaultsNow() : int = faultsNow.Value

    /// <summary>
    /// Graph thread, tracked: how many times work of this project or of projects it references failed on the graph
    /// thread. A reader that keeps what a file last found can tell from it that the file's pending check may never
    /// settle if an upstream project faulted.
    /// </summary>
    member this.FaultsSeenNow() : int =
        faultsNow.Value
        + (ProjectNodes.closure references (fun (g: ProjectGraph) -> g.References)
           |> List.sumBy (fun (r: ProjectGraph) -> r.FaultsNow()))

    /// Graph thread, tracked: whether a restart failed, which leaves the project with no nodes until it is loaded again
    member _.RestartFailedNow() : bool =
        not (isNull (box restartFailed)) && restartFailed.Value

    /// Awaits `hook` in each ReadFile after the file settled and before it takes the lock. For tests.
    member internal _.OnReadSettled(hook: unit -> Task) : unit = afterSettle <- hook

    /// Runs `hook` off the graph thread, with the file's path, just before a setup from a file's dependencies is given
    /// to the checker. For tests.
    member internal _.OnScopedCheck(hook: string -> unit) : unit = project.ScopedCheckHook <- hook

    /// Runs `hook` off the graph thread, with the file's path, just before a promotion build sets the file up. For
    /// tests.
    member internal _.OnBeginFile(hook: string -> unit) : unit = project.BeginFileHook <- hook

    /// Runs `hook` off the graph thread, with the memo's kind (parse, dependencies, split, finish, context, assembly),
    /// its path and the flight's token, as an async memo's work starts, before it checks the token. A finish also runs
    /// it as finish.lock before it waits for the lock, and as finish.run just before FinishFile; parse and dependencies
    /// as parse.checked and dependencies.checked once past their check. For tests.
    member internal _.OnFlight(hook: string -> string -> CancellationToken -> unit) : unit = project.FlightHook <- hook

    /// Runs on the graph thread after each file a read in one turn reads (ReadInOneTurn), between that file and the next.
    member internal _.OnTurnFile(hook: TurnFile -> unit) : unit = turnFileHook <- hook

    /// How many flights of the kind found themselves superseded before doing their work. For tests.
    member internal _.SkippedFlights(kind: string) : int64 = project.SkippedFlights kind

    /// Replaces the print a window's close leaves for a declaration, as a variable moved after the close would. For
    /// tests.
    member internal _.OnClosePrint(hook: DeclarationKey -> string -> string) : unit = project.ClosePrintHook <- hook

    /// Runs `hook` on the graph thread, with the file's path, each time a promoted file's declaration setup is
    /// computed: a hook that throws fails that setup. For tests.
    member internal _.OnSetup(hook: string -> unit) : unit = project.SetupHook <- hook

    /// Weak references to a declaration result of the file, the environment it was checked in, and the FileStart of
    /// its published declaration results; empty while none are published. For tests.
    member internal _.DeclarationObjects(file: string) : Task<WeakReference list> =
        let i = indexOf file
        host.Read(fun () -> files[i].DeclarationObjects())
