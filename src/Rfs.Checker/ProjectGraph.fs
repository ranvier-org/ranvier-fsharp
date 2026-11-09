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

module private ReadSupport =
    /// Takes the lock exclusively. A grant that arrives after `ct` ended the wait is released at once.
    let acquireExclusive (l: AsyncReaderWriterLock) (ct: CancellationToken) : Task<IDisposable> =
        task {
            let grant = l.WriteAsync()

            try
                return! grant.WaitAsync ct
            with :? OperationCanceledException ->
                grant.ContinueWith(
                    (fun (t: Task<IDisposable>) -> if t.IsCompletedSuccessfully then t.Result.Dispose()),
                    TaskScheduler.Default
                )
                |> ignore

                return raise (OperationCanceledException ct)
        }

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

/// A file's settled result with the parse and text it is for.
type FileView =
    { Result: FileResult
      Parse: FSharpParseFileResults
      Text: string
      /// The graph's version of the text (the one `SetText` returned)
      Version: int64
      At: int -> int -> FSharpCheckFileResults }

    /// Check results for a query that reads the 1-based line and 0-based column, inside the `read` it was given to. In a file
    /// checked by declaration, a kept declaration holds the names as they were when it was checked: when an earlier result
    /// has changed since, this checks it again in the current environment and gives a view of that, adopting nothing
    /// (ProjectGraph.AdHocChecks). A file checked whole always has its CheckResults.
    member v.CheckResultsAt(line: int, column: int) : FSharpCheckFileResults = v.At line column

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
[<Sealed>]
type ProjectGraph private (host: GraphHost, project: ProjectShared, spec: ProjectSpec, initialFiles: IFileNode[], initialOwner: Owner, references: ProjectGraph list, firstSeen: ReferencesSeen, initialWanted: HashSet<int>) =
    let requested = Array.zeroCreate<int64> initialFiles.Length
    // Cancelled by an edit of the file: what an ad hoc check of it would answer is out of date
    let edits = Array.init initialFiles.Length (fun _ -> new CancellationTokenSource())
    let lifetime = new CancellationTokenSource()
    let wanted = initialWanted
    // Replaced together by a restart, on the graph thread
    let mutable files = initialFiles
    let mutable owner = initialOwner
    let mutable seen = firstSeen
    let mutable generatedResults: Lazy<FileResult list> = Lazy<FileResult list>.CreateFromValue []
    let mutable filesNow: Signal<IFileNode[]> = Unchecked.defaultof<_>
    let mutable assemblyFlight: AsyncMemo<AssemblyState> = Unchecked.defaultof<_>
    let mutable assemblyMemo: Memo<AssemblyState> = Unchecked.defaultof<_>
    let mutable contextMemo: AsyncMemo<ReferencesSeen> = Unchecked.defaultof<_>
    let mutable linkOwner: Owner = Unchecked.defaultof<_>
    let dependents = ResizeArray<ProjectGraph>()
    let mutable disposed = 0
    let mutable afterSettle: unit -> Task = fun () -> Task.CompletedTask

    // Every wait ends when the graph is disposed: its nodes are gone, so a wait on them could never settle
    let settled (read: unit -> 'T voption) = host.WhenSettled(read, lifetime.Token)

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
    /// current too, transitively (IFileNode.Current).
    let readSettled (i: int) (read: IFileNode -> int64 -> 'T voption) : 'T voption =
        let mutable applied = true

        for j in 0..i do
            if files[j].Text.Version <> Volatile.Read &requested[j] then
                applied <- false

        if applied && referencesCurrent () then read files[i] (Volatile.Read &requested[i]) else ValueNone

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
                    host.WhenSettled(fun () ->
                        let all = linked |> List.map (fun (_, g: ProjectGraph) -> g.SettledAssembly())

                        if all |> List.forall _.IsSome then
                            ValueSome(all |> List.map _.Value |> Array.ofList)
                        else
                            ValueNone)

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

                if not references.IsEmpty then
                    // The context for the references as they stand; the current one while they settle or keep their fingerprints
                    contextMemo <-
                        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
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

                                task {
                                    let! ctx = ProjectGraph.MakeContext(spec, options, paths, built)
                                    project.AddPhase("createProject", Diagnostics.Stopwatch.GetTimestamp() - started, 0L)
                                    return { Fingerprints = built |> Array.map _.Fingerprint; Context = ctx }
                                })

                    createEffect (fun () ->
                        match contextMemo.TryValue with
                        | Ready next when not (obj.ReferenceEquals(next, seen)) -> project.Post(fun () -> this.Restart next)
                        | _ -> ())

                o)

    /// The assembly's nodes, made when a project that references this one first asks: a project nothing references
    /// takes no fingerprint for them. Graph thread.
    member private _.AssemblyNodes() : Memo<AssemblyState> =
        if isNull (box assemblyMemo) then
            runWithOwner linkOwner (fun () ->
                // Off the graph thread: taking a fingerprint no check asked for describes the file, and hashes them all
                assemblyFlight <-
                    createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
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
                            Task.Run<AssemblyState>(fun () -> Built(DeclarationChecker.ProjectAssembly(ctx, generated.Value @ results, signatures))))

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
            if files[j].Text.Version <> Volatile.Read &requested[j] then
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
        (Interlocked.Exchange(&edits[i], new CancellationTokenSource())).Cancel()

        // A disposed graph checks nothing
        if Volatile.Read &disposed = 0 then
            host.Post(fun () -> files[i].Text.SetText(text, version))

        version

    /// The file's diagnostics, with the version of its text they are for, once its result is for its latest text and
    /// the files it depends on are settled.
    member _.SettledDiagnostics(file: string) : Task<struct (int64 * FSharpDiagnostic[])> =
        let i = indexOf file

        settled (fun () ->
            readSettled i (fun f version -> f.Settled version |> ValueOption.map (fun d -> struct (version, d))))

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

    /// Runs `read` off the graph thread on the file's published result, once it is settled on its latest text (as for
    /// `Diagnostics`). For a file checked by declaration `read` holds the lock of the result's FileStart exclusively, so no
    /// check, shift or finish runs on it meanwhile, and the result is still the published one when the lock is taken. Cancelling `ct`
    /// cancels the wait; a graph disposed (a reload) meanwhile cancels it too. Fails with ArgumentException for a
    /// signature file, which has no FileResult. `read` runs again, on the file's new text, when an edit of the file
    /// cancelled an ad hoc check inside it (FileView.CheckResultsAt): only the client's cancel or a reload ends the wait.
    member _.ReadFile(file: string, read: FileView -> 'T, ct: CancellationToken) : Task<'T> =
        let i = indexOf file

        if isSignatureFile files[i] then
            invalidArg (nameof file) $"%s{file} is a signature file"

        let readable () = readSettled i (fun f version -> (f :?> IReadableFile).Readable version)

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            let mutable answer = ValueNone

            while answer.IsNone do
                let! r = host.WhenSettled(readable, linked.Token)
                do! afterSettle ()

                let edit = Volatile.Read &edits[i]

                let run (r: Readable) (checks: CancellationToken) =
                    Task.Run(fun () ->
                        read
                            { Result = r.Result
                              Parse = r.Parsed.Parse
                              Text = r.Parsed.Text
                              Version = r.Parsed.Version
                              At =
                                fun line column ->
                                    match r.Declarations with
                                    | ValueSome(f, roles) -> RowView.at project f roles line column checks
                                    | ValueNone -> r.Result.CheckResults })

                match r.Lock with
                | ValueNone ->
                    let! v = run r linked.Token
                    answer <- ValueSome v
                | ValueSome l ->
                    let! held = acquireExclusive l linked.Token
                    use _ = held
                    // A finish may have run between the settle and the lock; one in flight now changes the inputs first
                    let! again = host.Read(fun () -> try readable () with NotReadyException _ -> ValueNone)

                    match again with
                    | ValueSome a when obj.ReferenceEquals(a.Identity, r.Identity) ->
                        use checks = CancellationTokenSource.CreateLinkedTokenSource(linked.Token, edit.Token)

                        try
                            let! v = run a checks.Token
                            answer <- ValueSome v
                        with :? OperationCanceledException when edit.IsCancellationRequested && not linked.IsCancellationRequested ->
                            ()
                    | _ -> ()

            return answer.Value
        }

    /// <summary>
    /// Where the definition that `key` (DeclarationChecker.SymbolKey) names is now, for a symbol in the file `asking` whose
    /// own signature location is `symbolLocation`. As FCS answers: a file after a signature sees that signature; in
    /// the implementation file, and for what it doesn't expose or when it didn't conform for errors, the definition's
    /// signature location in the file's current result says which. A range in the signature file is read from the latest
    /// check of it. None when the project holds neither file, or they don't define the key, or a file defines it twice,
    /// which does not compile, unless the symbol names one of them itself (a property's getter and setter answer the getter's).
    /// </summary>
    /// <remarks>
    /// A location read off a symbol can be stale: a kept row holds an older check's objects, which conformance and shifts
    /// no longer reach, later files hold a signature whose print didn't change, and another project a pickle of one. This
    /// is what corrects them. It finishes with each file before it waits for the next, so it holds no lock while it waits.
    /// Its answer is for the texts it finds, which may be newer than the symbol's.
    /// </remarks>
    member this.CurrentLocation(asking: string, symbolLocation: FSharp.Compiler.Text.range, key: string, ct: CancellationToken) : Task<FSharp.Compiler.Text.range option> =
        let holds (file: string) = byPath.ContainsKey(Path.GetFullPath file)

        let inSignature (file: string) =
            let i = indexOf file

            task {
                use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)

                let! s =
                    host.WhenSettled((fun () -> readSettled i (fun f version -> (f :?> ISignatureFile).LatestCheck version)), linked.Token)

                return! Task.Run(fun () -> DeclarationChecker.SignatureLocation(project.Context, s, key))
            }

        let inImplementation (file: string) =
            this.ReadFile(
                file,
                (fun view ->
                    let found =
                        view.Result.CheckResults.GetAllUsesOfAllSymbolsInFile ct
                        |> Seq.filter (fun u -> u.IsFromDefinition && DeclarationChecker.SymbolKey u.Symbol = Some key)
                        |> Seq.map _.Symbol
                        |> Seq.toList
                        |> List.sortBy (function
                            | :? FSharp.Compiler.Symbols.FSharpMemberOrFunctionOrValue as v when v.IsPropertyGetterMethod -> 0
                            | _ -> 1)

                    match found |> List.choose _.SignatureLocation |> List.distinct with
                    | [ only ] -> Some only
                    | first :: _ when key.Contains "|property|" -> Some first
                    | several -> several |> List.tryFind (fun r -> r = symbolLocation)),
                ct
            )

        let isSignature (file: string) = file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)
        let landed = symbolLocation.FileName
        let signature = Path.ChangeExtension(landed, ".fsi")
        let implementation = if isSignature landed then Path.ChangeExtension(landed, ".fs") else landed

        let sameFile (x: string) (y: string) =
            String.Equals(Path.GetFullPath x, Path.GetFullPath y, StringComparison.OrdinalIgnoreCase)

        task {
            if isSignature landed && holds signature && not (sameFile asking implementation) then
                let! fromSignature = inSignature signature

                match fromSignature with
                | Some _ -> return fromSignature
                | None when holds implementation -> return! inImplementation implementation
                | None -> return None
            elif holds implementation then
                let! fromImplementation = inImplementation implementation

                match fromImplementation with
                | Some r when isSignature r.FileName && holds signature -> return! inSignature signature
                | _ -> return fromImplementation
            elif holds signature then
                return! inSignature signature
            else
                return None
        }

    /// How the file's published result was checked: FileLevel while it is pending (mid-edit), and while it is CheckFile's
    /// or a demoted file's kept one.
    member _.Mode(file: string) : Task<Rfs.Checker.FileMode> =
        let i = indexOf file
        host.Read(fun () -> files[i].Mode)

    /// Marks an implementation file wanted (a signature file is never promoted): it is built by declaration in the
    /// background, one file of the project at a time, and checked by declaration once that build has finished for
    /// its current text and setup. Until then, and after any change of its setup, it is checked whole. A file whose
    /// text splits into one declaration (module rec, namespace rec) stays file-level. Posts; graph thread.
    member _.Promote(file: string) : unit =
        let i = indexOf file

        host.Post(fun () ->
            wanted.Add i |> ignore
            files[i].Promote())

    /// Clears wanted: the file's declaration results and nodes are dropped, its last result stays published until its
    /// text or setup changes, and from then on it is checked whole. Posts; graph thread.
    member _.Demote(file: string) : unit =
        let i = indexOf file

        host.Post(fun () ->
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
            let wait = host.WhenSettled((fun () -> readSettled i (fun f version -> f.Promoted version)), linked.Token)
            wait.ContinueWith((fun (_: Task) -> linked.Dispose()), TaskScheduler.Default) |> ignore
            wait
        else
            host.WhenSettled((fun () -> readSettled i (fun f version -> f.Promoted version)), lifetime.Token)

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

    /// Awaits `hook` in each ReadFile after the file settled and before it takes the lock. For tests.
    member internal _.OnReadSettled(hook: unit -> Task) : unit = afterSettle <- hook

    /// Runs `hook` off the graph thread, with the file's path, just before a setup from a file's dependencies is given
    /// to the checker. For tests.
    member internal _.OnScopedCheck(hook: string -> unit) : unit = project.ScopedCheckHook <- hook

    /// Runs `hook` off the graph thread, with the file's path, just before a promotion build sets the file up. For
    /// tests.
    member internal _.OnBeginFile(hook: string -> unit) : unit = project.BeginFileHook <- hook

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
