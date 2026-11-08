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

    /// The object the fork holds while it checks the file (ProjectChecks.FileLock, internal to the fork).
    let fileLockOf (ctx: ProjectContext) : string -> obj =
        let flags = Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic
        let checks = typeof<ProjectContext>.GetProperty("Checks", flags).GetValue ctx
        let m = checks.GetType().GetMethod("FileLock", flags)
        fun name -> m.Invoke(checks, [| box name |])

open ReadSupport

/// A file's settled result with the parse and text it is for.
type FileView =
    { Result: FileResult
      Parse: FSharpParseFileResults
      Text: string
      /// The graph's version of the text (the one `SetText` returned)
      Version: int64 }

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
/// </remarks>
[<Sealed>]
type ProjectGraph private (host: GraphHost, project: ProjectShared, spec: ProjectSpec, files: IFileNode[], owner: Owner) =
    let requested = Array.zeroCreate<int64> files.Length
    let lifetime = new CancellationTokenSource()
    let mutable disposed = 0
    let mutable afterSettle: unit -> Task = fun () -> Task.CompletedTask

    // Every wait ends when the graph is disposed: its nodes are gone, so a wait on them could never settle
    let settled (read: unit -> 'T voption) = host.WhenSettled(read, lifetime.Token)

    let byPath =
        let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
        files |> Array.iteri (fun i f -> d[Path.GetFullPath f.Path] <- i)
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

    /// Reads file `i` once it is settled on its latest text, with every write posted to it and to the files before it
    /// applied. Its result being settled means the files it was set up from are current too, transitively
    /// (IFileNode.Current).
    let readSettled (i: int) (read: IFileNode -> int64 -> 'T voption) : 'T voption =
        let mutable applied = true

        for j in 0..i do
            if files[j].Text.Version <> Volatile.Read &requested[j] then
                applied <- false

        if applied then read files[i] (Volatile.Read &requested[i]) else ValueNone

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

    /// A graph whose every file starts file-level; with `Declarations`, every implementation file is also promoted.
    static member internal CreateWithMode(host: GraphHost, spec: ProjectSpec, mode: Rfs.Checker.FileMode) : Task<ProjectGraph> =
        task {
            let allocated = GC.GetTotalAllocatedBytes true
            let started = Diagnostics.Stopwatch.GetTimestamp()
            let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.StartAsTask
            let project = ProjectShared ctx
            project.Post <- host.Post

            // Nothing else of the graph runs yet, so the process's allocations are the context's
            project.AddPhase("createProject", Diagnostics.Stopwatch.GetTimestamp() - started, GC.GetTotalAllocatedBytes true - allocated)
            let texts = spec.SourceFiles |> List.map (fun f -> f, File.ReadAllText f) |> Array.ofList

            // The nodes belong to a root of their own, so disposing the root takes the whole graph of the project
            let build () =
                    let isSignature (path: string) =
                        path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

                    let fileTexts =
                        texts |> Array.map (fun (path, text) -> FileText(project, path, text))

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

                    if mode = Declarations then
                        for node in nodes do
                            node.Promote()

                    nodes.ToArray()

            let! owner, files = host.Read(fun () -> createRoot (fun owner -> owner, build ()))
            return ProjectGraph(host, project, spec, files, owner)
        }

    /// The first and last line (1-based) of each declaration `text` splits into as the file's text. Call it when the
    /// graph is idle, and never from the graph thread: it waits for the graph thread to read the file's parse. A text
    /// other than the one the graph holds is parsed on a second project context, created the first time and kept for
    /// the graph's lifetime (as much memory as the project's references take).
    member _.DeclarationLines(file: string, text: string) : (int * int)[] =
        let node = files[indexOf file]

        let current =
            (host.Read(fun () ->
                match node.Text.Parsed.TryValue with
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
                let held = (host.Read(fun () -> node.Text.Text.Peek.Text)).GetAwaiter().GetResult()
                DeclarationChecker.Parse(ctx, file, held) |> ignore
                parse

        DeclarationChecker.Split parse |> Array.map (fun d -> d.Range.StartLine, d.Range.EndLine)

    /// Sets a file's text; checks follow on the graph thread. Returns the version posted.
    member _.SetText(file: string, text: string) : int64 =
        let i = indexOf file
        let version = Interlocked.Increment &requested[i]
        let node = files[i]

        // A disposed graph checks nothing
        if Volatile.Read &disposed = 0 then
            host.Post(fun () -> node.Text.SetText(text, version))

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

    /// `Diagnostics`). For a file checked by declaration `read` holds the lock of the result's FileStart exclusively, so no
    /// finish runs on it meanwhile, and the result is still the published one when the lock is taken. Cancelling `ct`
    /// cancels the wait; a graph disposed (a reload) meanwhile cancels it too. Fails with ArgumentException for a
    /// signature file, which has no FileResult, and with InvalidOperationException when called holding the file's
    /// check lock.
    member _.ReadFile(file: string, read: FileView -> 'T, ct: CancellationToken) : Task<'T> =
        let i = indexOf file

        if isSignatureFile files[i] then
            invalidArg (nameof file) $"%s{file} is a signature file"

        let node = files[i] :?> IReadableFile
        let fileLock = fileLockOf project.Context files[i].Path

        // m2: reading CheckResults takes the FileStart's lock and then the file's check lock; never the other way round
        let guard () =
            if Monitor.IsEntered fileLock then
                invalidOp $"%s{file}: read while holding the file's check lock"

        guard ()

        let readable () = readSettled i (fun _ version -> node.Readable version)

        task {
            use linked = CancellationTokenSource.CreateLinkedTokenSource(ct, lifetime.Token)
            let mutable answer = ValueNone

            while answer.IsNone do
                let! r = host.WhenSettled(readable, linked.Token)
                do! afterSettle ()

                let run (r: Readable) =
                    Task.Run(fun () ->

                        read
                            { Result = r.Result
                              Parse = r.Parsed.Parse
                              Text = r.Parsed.Text
                              Version = r.Parsed.Version })

                match r.Lock with
                | ValueNone ->
                    let! v = run r
                    answer <- ValueSome v
                | ValueSome l ->
                    let! held = acquireExclusive l linked.Token
                    use _ = held
                    // A finish may have run between the settle and the lock; one in flight now changes the inputs first
                    let! again = host.Read(fun () -> try readable () with NotReadyException _ -> ValueNone)

                    match again with
                    | ValueSome a when obj.ReferenceEquals(a.Identity, r.Identity) ->
                        let! v = run r
                        answer <- ValueSome v
                    | _ -> ()

            return answer.Value
        }

    /// How the file's published result was checked: FileLevel while it is pending (mid-edit), and while it is CheckFile's
    /// or a demoted file's kept one.
    member _.Mode(file: string) : Task<Rfs.Checker.FileMode> =
        let node = files[indexOf file]
        host.Read(fun () -> node.Mode)

    /// Marks an implementation file wanted (a signature file is never promoted): it is built by declaration in the
    /// background, one file of the project at a time, and checked by declaration once that build has finished for
    /// its current text and setup. Until then, and after any change of its setup, it is checked whole. A file whose
    /// text splits into one declaration (module rec, namespace rec) stays file-level. Posts; graph thread.
    member _.Promote(file: string) : unit =
        let node = files[indexOf file]
        host.Post(fun () -> node.Promote())

    /// Clears wanted: the file's declaration results and nodes are dropped, its last result stays published until its
    /// text or setup changes, and from then on it is checked whole. Posts; graph thread.
    member _.Demote(file: string) : unit =
        let node = files[indexOf file]
        host.Post(fun () -> node.Demote())

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
        let node = files[indexOf file]
        (host.Read(fun () -> node.SegmentRows())).GetAwaiter().GetResult()

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
    member _.Dispose() =
        if Interlocked.Exchange(&disposed, 1) = 0 then
            lifetime.Cancel()

            try
                host.Post(fun () -> owner.Dispose())
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
        let node = files[indexOf file]
        host.Read(fun () -> node.DeclarationObjects())
