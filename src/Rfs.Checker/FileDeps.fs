namespace Rfs.Checker

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier

/// How a file is checked.
type FileMode =
    /// Whole, by CheckFile (CheckSignatureFile for a signature file), set up from the files it depends on
    | FileLevel
    /// One declaration at a time, set up from the files it depends on
    | Declarations

[<NoEquality; NoComparison>]
type internal Versioned = { Text: string; Version: int64 }

[<ReferenceEquality>]
type internal Parsed =
    { Version: int64
      Text: string
      Parse: FSharpParseFileResults
      /// The declarations the parse splits into: set only by a promoted file's declaration nodes, empty otherwise
      Declarations: ParsedDeclaration[]
      /// The text before the first declaration: the file's module or namespace header (the whole text while
      /// Declarations is empty)
      Prologue: string }

/// What later files are checked against: a file's signature.
[<ReferenceEquality>]
type internal FileSig =
    { Signature: FileSignature
      /// The signatures the file was set up from
      Before: FileSignature list }

    member s.Fingerprint = s.Signature.Fingerprint

    /// Whether two setups read the same signature objects, and so the same entities.
    static member SameBefore(a: FileSignature list, b: FileSignature list) =
        a.Length = b.Length && List.forall2 (fun (x: FileSignature) (y: FileSignature) -> obj.ReferenceEquals(x, y)) a b

    /// What later files are checked against, given the last value. The last one stays while the file's fingerprint
    /// (compared structurally) and its setup's signature objects are the same, whatever the new check made: the files
    /// after it hold its entities, by identity, for as long as they keep their results, so its signature may not move
    /// to a new entity of the same print under them. A new setup or a new fingerprint gives the new signature: the
    /// files that depend on this one (transitively, by FileDependencies) were set up from the same files and are being
    /// checked again. A fingerprint not yet taken is not taken here: the file had no dependents when it was checked.
    static member Next(previous: FileSig voption, signature: FileSignature, before: FileSignature list) =
        match previous with
        | ValueSome p when
            obj.ReferenceEquals(p.Signature, signature)
            || (FileSig.SameBefore(p.Before, before)
                && p.Signature.HasFingerprint
                && signature.HasFingerprint
                && String.Equals(p.Fingerprint, signature.Fingerprint, StringComparison.Ordinal))
            ->
            p
        | _ -> { Signature = signature; Before = before }

    /// Equal when they are the same signature: `Next` keeps it while nothing a later file reads of it changed.
    static member Comparer =
        { new IEqualityComparer<FileSig> with
            member _.Equals(a, b) = obj.ReferenceEquals(a.Signature, b.Signature)
            member _.GetHashCode _ = 0 }

/// A declaration whose final fingerprint, after FinishFile, differs from the one its own check left (the D1 premise
/// broke), with both fingerprints.
type PremiseBreak =
    { Key: DeclarationKey
      File: string
      /// The declaration's first line (1-based)
      Line: int
      Provisional: string
      Final: string }

/// An invariant of the graph broke: a bug, never contained or degraded to a whole check.
type internal GraphInvariantException(message: string) =
    inherit Exception(message)

/// Hands each value to the observers subscribed when it is sent.
[<Sealed>]
type internal Subject<'T>() =
    let observers = ResizeArray<IObserver<'T>>()

    member _.Next(value: 'T) =
        let snapshot = lock observers (fun () -> observers.ToArray())

        for o in snapshot do
            o.OnNext value

    interface IObservable<'T> with
        member _.Subscribe o =
            lock observers (fun () -> observers.Add o)

            { new IDisposable with
                member _.Dispose() =
                    lock observers (fun () -> observers.Remove o |> ignore) }

/// Shared by every file of a project.
[<Sealed>]
type internal ProjectShared(ctx: ProjectContext) =
    let mutable context = ctx
    let mutable checkedCount = 0L
    let mutable adoptedCount = 0L
    let mutable shiftedCount = 0L
    let mutable envExtendCount = 0L
    let mutable finishTicks = 0L
    let mutable premiseBreaks = 0L
    let mutable fileLevelChecks = 0L
    let mutable measuresStarted = 0L
    let mutable measuresEnded = 0L
    let mutable chainFallbacks = 0L
    let mutable finishReused = 0L
    let mutable directiveRetries = 0L
    let mutable declarationRoots = 0L
    let mutable scopedCheckHook: string -> unit = ignore
    let mutable beginFileHook: string -> unit = ignore
    let mutable setupHook: string -> unit = ignore
    let finishPhaseTicks = Dictionary<string, int64>()
    let phaseAllocated = Dictionary<string, int64>()
    let premise = Subject<PremiseBreak>()
    let published = Subject<DeclarationKey * string>()
    let ran = Subject<DeclarationKey * string>()

    /// The context the project's current nodes capture when they are made. A restart replaces it, with the nodes.
    member _.Context
        with get () = Volatile.Read &context
        and set (value: ProjectContext) = Volatile.Write(&context, value)

    member _.CheckedCount = Interlocked.Read &checkedCount
    member _.FinishTicks = Interlocked.Read &finishTicks
    member _.PremiseBreaks = Interlocked.Read &premiseBreaks
    member _.FileLevelChecks = Interlocked.Read &fileLevelChecks
    member _.ChainFallbacks = Interlocked.Read &chainFallbacks
    /// The results FinishFile reused from a file's previous finish (FileResult.Reused), summed over every finish.
    member _.FinishReused = Interlocked.Read &finishReused
    member _.AddFinishReused(n: int) = Interlocked.Add(&finishReused, int64 n) |> ignore
    member _.DirectiveRetries = Interlocked.Read &directiveRetries

    /// Counts a CheckDeclaration that returned, and reports it (on the thread that ran it).
    member _.CountCheck(key: DeclarationKey, file: string) =
        Interlocked.Increment &checkedCount |> ignore
        ran.Next((key, file))

    member _.CountFileLevelCheck() = Interlocked.Increment &fileLevelChecks |> ignore
    member _.CountChainFallback() = Interlocked.Increment &chainFallbacks |> ignore
    member _.CountDirectiveRetry() = Interlocked.Increment &directiveRetries |> ignore
    member _.DeclarationRoots = Interlocked.Read &declarationRoots
    member _.CountDeclarationRoot() = Interlocked.Increment &declarationRoots |> ignore

    /// Runs off the graph thread, with the file's path, just before a setup from its dependencies is given to the
    /// checker. For tests.
    member _.ScopedCheckHook
        with get () = Volatile.Read &scopedCheckHook
        and set (hook: string -> unit) = Volatile.Write(&scopedCheckHook, hook)

    /// The index of the file being built by declaration (at most one per project at a time), if any. Set once every
    /// file's nodes exist. Graph thread.
    member val Builder: Memo<int voption> = Unchecked.defaultof<_> with get, set

    /// Runs off the graph thread, with the file's path, just before a promotion build sets the file up (BeginFile).
    /// For tests.
    member _.BeginFileHook
        with get () = Volatile.Read &beginFileHook
        and set (hook: string -> unit) = Volatile.Write(&beginFileHook, hook)

    /// Gives the print a window's close leaves for a declaration. For tests.
    member val ClosePrintHook: DeclarationKey -> string -> string = (fun _ print -> print) with get, set

    /// Runs on the graph thread, with the file's path, each time a promoted file's declaration setup is computed.
    /// For tests.
    member _.SetupHook
        with get () = Volatile.Read &setupHook
        and set (hook: string -> unit) = Volatile.Write(&setupHook, hook)

    /// Whether implementation file i's check failed, as a cut-off memo over its signature. Set once every file's nodes
    /// exist; read only for implementation files that have a signature file.
    member val FailedOf: int -> Memo<bool> = (fun _ -> invalidOp "the files' nodes are not made yet") with get, set

    /// File i's IFileNode.Current. Set once every file's nodes exist.
    member val CurrentOf: int -> Memo<bool> = (fun _ -> invalidOp "the files' nodes are not made yet") with get, set

    /// Runs work on the graph thread after the current run, as GraphHost.Post. Set once the host is known.
    member val Post: (unit -> unit) -> unit = ignore with get, set

    member _.AddFinishTicks(ticks: int64) = Interlocked.Add(&finishTicks, ticks) |> ignore

    /// Adds a finish's FileResult.PhaseTicks to the running totals; finishes of different files run concurrently.
    member _.AddFinishPhaseTicks(phases: IReadOnlyDictionary<string, int64>) =
        lock finishPhaseTicks (fun () ->
            for KeyValue(phase, ticks) in phases do
                finishPhaseTicks[phase] <- (match finishPhaseTicks.TryGetValue phase with | true, t -> t | _ -> 0L) + ticks)

    /// A copy of the running totals of FileResult.PhaseTicks.
    member _.FinishPhaseTicks: IReadOnlyDictionary<string, int64> =
        lock finishPhaseTicks (fun () -> Dictionary<string, int64>(finishPhaseTicks) :> IReadOnlyDictionary<_, _>)

    /// Adds the time and the bytes the calling thread allocated in one run of a phase of the file-level graph.
    member this.AddPhase(phase: string, ticks: int64, allocated: int64) =
        this.AddFinishPhaseTicks(readOnlyDict [ phase, ticks ])

        lock phaseAllocated (fun () ->
            phaseAllocated[phase] <- (match phaseAllocated.TryGetValue phase with | true, b -> b | _ -> 0L) + allocated)

    /// Adds the bytes each part of a CheckFile or FinishFile allocated (FileResult.PhaseAllocated), under `prefix`.
    member _.AddPartsAllocated(prefix: string, parts: IReadOnlyDictionary<string, int64>) =
        lock phaseAllocated (fun () ->
            for KeyValue(part, bytes) in parts do
                let phase = prefix + part
                phaseAllocated[phase] <- (match phaseAllocated.TryGetValue phase with | true, b -> b | _ -> 0L) + bytes)

    /// A copy of the running totals of the bytes each phase allocated (file-level graph only).
    member _.PhaseAllocated: IReadOnlyDictionary<string, int64> =
        lock phaseAllocated (fun () -> Dictionary<string, int64>(phaseAllocated) :> IReadOnlyDictionary<_, _>)

    /// Runs `work` on this thread and adds its time and allocations to `phase`.
    member this.Measure(phase: string, work: unit -> 'T) : 'T =
        Interlocked.Increment &measuresStarted |> ignore

        try
            let allocated = GC.GetAllocatedBytesForCurrentThread()
            let started = Diagnostics.Stopwatch.GetTimestamp()
            let result = work ()
            this.AddPhase(phase, Diagnostics.Stopwatch.GetTimestamp() - started, GC.GetAllocatedBytesForCurrentThread() - allocated)
            result
        finally
            Interlocked.Increment &measuresEnded |> ignore

    /// How many Measure calls have started, and how many of them are running. For tests.
    member _.MeasuresStarted = Interlocked.Read &measuresStarted
    member _.MeasuresInFlight = Interlocked.Read &measuresStarted - Interlocked.Read &measuresEnded

    member _.CountPremiseBreak(premiseBreak: PremiseBreak) =
        Interlocked.Increment &premiseBreaks |> ignore
        premise.Next premiseBreak

    member _.PremiseBroken = premise :> IObservable<_>
    /// Reports a check result published for the first time: each result is one CheckDeclaration, adopted.
    member _.Publish(key: DeclarationKey, file: string) =
        Interlocked.Increment &adoptedCount |> ignore
        published.Next((key, file))

    member _.AdoptedCount = Interlocked.Read &adoptedCount

    /// Counts a published result shifted to another line instead of checked again (W1).
    member _.CountShift() = Interlocked.Increment &shiftedCount |> ignore
    /// ExtendEnv calls assembling environments before checks: on an in-order re-check, about one per row.
    member _.CountEnvExtend() = Interlocked.Increment &envExtendCount |> ignore
    member _.EnvExtendCount = Interlocked.Read &envExtendCount
    member _.ShiftedCount = Interlocked.Read &shiftedCount
    member _.Checked = published :> IObservable<_>
    member _.Ran = ran :> IObservable<_>

/// Helpers for building the graph's nodes.
module internal Nodes =

    /// The async memo's last settled value while a new flight runs, so readers that only compare it with what they
    /// saw before are not woken by the flight starting. A reader that needs the new value reads the async memo.
    let latestOf (memo: AsyncMemo<'T>) : Memo<'T> =
        createMemo (fun previous ->
            match memo.TryValue, previous with
            | Ready v, _ -> v
            | Pending, ValueSome p -> p
            | _ -> memo.Value)

    /// latestOf for a memo that is pending while an async memo it reads is.
    let latestOfMemo (memo: Memo<'T>) : Memo<'T> =
        createMemo (fun previous ->
            match memo.TryValue, previous with
            | Ready v, _ -> v
            | Pending, ValueSome p -> p
            | _ -> memo.Value)

    let same (a: obj) (b: obj) = obj.ReferenceEquals(a, b)

    /// A declaration is unchanged when checking it reads the same syntax (R10) at the same place: a result's
    /// diagnostics and typed tree carry the ranges it was checked at.
    let declComparer =
        { new IEqualityComparer<ParsedDeclaration> with
            member _.Equals(a, b) =
                same a b
                || (a.IsUnchangedFrom b
                    && a.Range.StartLine = b.Range.StartLine
                    && a.Range.StartColumn = b.Range.StartColumn
                    && a.Within = b.Within)

            member _.GetHashCode d =
                hash (d.Text, d.Range.StartLine, d.Range.StartColumn) }

    /// Whether `b` is `a` at another line: the same syntax at the same column in the same module (W1). The checker
    /// shifts such a result (DeclarationChecker.Shift) instead of checking it again.
    let movedOnly (a: ParsedDeclaration) (b: ParsedDeclaration) =
        a.IsUnchangedFrom b
        && a.Range.StartLine <> b.Range.StartLine
        && a.Range.StartColumn = b.Range.StartColumn
        && a.Within = b.Within

    /// Whether two prologues are the same header but for the blank lines after it (W1): a line inserted or removed
    /// between the header and the first declaration moves the declarations, which are shifted, and nothing the setup
    /// reads, whose own ranges (the header's) are all above the line. A blank line anywhere above the header's last
    /// line moves part of the header, and sets the file up again.
    let sameHeader (a: string) (b: string) =
        let header (s: string) = s.Replace("\r\n", "\n").TrimEnd(' ', '\t', '\n', '\r')
        String.Equals(a, b, StringComparison.Ordinal) || String.Equals(header a, header b, StringComparison.Ordinal)

    /// The text before the first declaration.
    let prologueOf (text: string) (declarations: ParsedDeclaration[]) =
        if declarations.Length = 0 then
            text
        else
            let start = declarations[0].Range
            let mutable line = 1
            let mutable offset = 0

            while line < start.StartLine && offset < text.Length do
                let next = text.IndexOf('\n', offset)

                if next < 0 then
                    offset <- text.Length
                else
                    offset <- next + 1

                line <- line + 1

            text.Substring(0, min text.Length (offset + start.StartColumn))

    let keyText (key: DeclarationKey) =
        $"""%s{String.Join(".", key.ModulePath)}|%s{key.Name}|%A{key.Kind}|%d{key.Ordinal}"""

    /// Reads every async memo, so each one's flight starts, then their values: pending while any of them is.
    let valuesOfAsync (memos: AsyncMemo<'T>[]) : 'T[] =
        for m in memos do
            m.TryValue |> ignore

        memos |> Array.map _.Value

    /// valuesOfAsync for memos.
    let valuesOf (memos: Memo<'T>[]) : 'T[] =
        for m in memos do
            m.TryValue |> ignore

        memos |> Array.map _.Value

/// Lets only the newest of a node's flights run its work. A ticket is taken on the graph thread when a flight starts,
/// so a later ticket is a later input; a flight whose ticket is no longer the newest when its turn comes is cancelled.
/// The work runs under the gate's lock, so its writes to shared state (the context's latest parse and dependency
/// snapshot, the process-wide line directives) land in ticket order and the newest one's are the ones left.
[<Sealed>]
type internal Gate() =
    let sync = obj ()
    let mutable issued = 0L

    member _.Issue() = Interlocked.Increment &issued

    member _.Run(ticket: int64, work: unit -> 'T) : 'T =
        lock sync (fun () ->
            if ticket <> Interlocked.Read &issued then
                raise (OperationCanceledException "a later flight superseded this one")

            work ())

/// A source file's text and its parse.
[<Sealed>]
type internal FileText(project: ProjectShared, path: string, initial: string, version: int64) =
    let ctx = project.Context
    let text = createSignal { Text = initial; Version = version }
    let gate = Gate()

    let parsed =
        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
            let t = text.Value
            let ticket = gate.Issue()

            Task.Run<Parsed>(fun () ->
                gate.Run(
                    ticket,
                    fun () ->
                        let parse = project.Measure("parse", fun () -> DeclarationChecker.Parse(ctx, path, t.Text))

                        { Version = t.Version
                          Text = t.Text
                          Parse = parse
                          Declarations = [||]
                          Prologue = t.Text }
                )))

    member _.Path = path
    member _.Text = text
    member _.Parsed: AsyncMemo<Parsed> = parsed
    /// The version of the text set last. Graph thread.
    member _.Version = text.Peek.Version

    /// Sets the text unless a later version is already set: writes posted from different threads may arrive out of order.
    member _.SetText(t: string, version: int64) =
        if version > text.Peek.Version then
            text.Value <- { Text = t; Version = version }

/// What FileDependencies gave for one parse of every file.
[<ReferenceEquality>]
type internal FileDepsValue =
    { /// The parses, in file order: the dependency snapshot the checker holds is of these
      Parsed: Parsed[]
      Dependencies: int[][]
      /// For each file, the earlier files with the same qualified name. Deduplicating the file's module name reads
      /// them, whether or not it depends on them.
      SameName: int[][]
      /// For each file, whether a later file depends on it
      HasDependents: bool[]
      /// For each signature file, its implementation file's index (the same path without the trailing i), or -1
      ImplementationOf: int[] }

module internal FileDeps =

    let private sameNames (parsed: Parsed[]) =
        let seen = Dictionary<string, ResizeArray<int>>(StringComparer.OrdinalIgnoreCase)

        parsed
        |> Array.mapi (fun i p ->
            let name = p.Parse.ParseTree.QualifiedName.Text

            match seen.TryGetValue name with
            | true, earlier ->
                let before = earlier.ToArray()
                earlier.Add i
                before
            | _ ->
                seen[name] <- ResizeArray [ i ]
                [||])

    let private implementationsOf (parsed: Parsed[]) =
        let byPath = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

        parsed |> Array.iteri (fun i p -> byPath[p.Parse.FileName] <- i)

        parsed
        |> Array.map (fun p ->
            let path = p.Parse.FileName

            if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                match byPath.TryGetValue(path.Substring(0, path.Length - 1)) with
                | true, i -> i
                | _ -> -1
            else
                -1)

    let private hasDependents (dependencies: int[][]) =
        let has = Array.zeroCreate dependencies.Length

        for deps in dependencies do
            for d in deps do
                has[d] <- true

        has

    /// The dependencies of every file, from every file's current parse: re-run whenever any parse changes, so the
    /// checker's dependency snapshot is of the parses the value holds. Superseded flights do not run, so a late one
    /// never replaces a newer snapshot.
    let create (project: ProjectShared) (files: FileText[]) : AsyncMemo<FileDepsValue> =
        let ctx = project.Context
        let gate = Gate()

        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
            let parsed = Nodes.valuesOfAsync (files |> Array.map _.Parsed)
            let ticket = gate.Issue()

            Task.Run<FileDepsValue>(fun () ->
                gate.Run(
                    ticket,
                    fun () ->
                        let dependencies =
                            project.Measure("fileDependencies", fun () -> DeclarationChecker.FileDependencies(ctx, parsed |> Array.map _.Parse))

                        { Parsed = parsed
                          Dependencies = dependencies
                          SameName = sameNames parsed
                          HasDependents = hasDependents dependencies
                          ImplementationOf = implementationsOf parsed }
                )))

/// What a file's setup reads: the earlier files' signatures, either those of the files it depends on or every earlier
/// one, and the file's parse.
[<ReferenceEquality>]
type internal SetupKey =
    { /// The file's parse in the FileDeps value the key was read from
      Parsed: Parsed
      /// The earlier files the setup reads, ascending
      Files: int[]
      Before: FileSignature list
      Fingerprints: string[]
      SameName: int[]
      /// Whether a later file depended on the file when the key was read: if not, its check need not take its
      /// signature's fingerprint. Not part of the key's identity.
      HasDependents: bool
      /// The implementation files whose failed status (`ProjectShared.FailedOf`) the key read: an answer given while
      /// such a file is checked again is its last one, so a result of this key is settled only once each of them is
      /// current, as each of Files must be. Not part of the key's identity.
      Consulted: int[]
      /// Whether Files are the file's dependencies and fewer than every earlier file: the checker then accepts the
      /// setup only while its dependency snapshot is current
      Scoped: bool }

/// A check from a setup read from the file's dependencies.
type internal Attempt<'R> =
    | Done of SetupKey * 'R
    /// The check reported an unresolved name (C3/R17): the earlier files it did not read may define it
    | Unresolved of SetupKey
    /// The checker refused the setup: its dependency snapshot was not current for the file (this FileDeps value)
    | Stale of SetupKey * FileDepsValue

module internal Setup =

    /// How long a check waits for its file's #line directives to be its parse's again before it fails the node.
    let private directiveWait = TimeSpan.FromSeconds 10.

    /// The check, tried again while the checker refuses it because its file's #line directives are not its parse's: a
    /// newer parse of another text of the file replaced them. On this context that parse is a newer text, and the
    /// flight is cancelled once FileDeps has it; on another context, such as DeclarationLines', the directives come
    /// back when it parses the held text again. A refused check is never a result, and never fails the node while
    /// it can still be superseded.
    let runWhileDirectivesCurrent (project: ProjectShared) (ct: CancellationToken) (run: unit -> 'R) : 'R =
        let clock = System.Diagnostics.Stopwatch.StartNew()

        let rec attempt () =
            // A superseded flight's check would only queue behind the current one at the file's lock
            ct.ThrowIfCancellationRequested()

            try
                run ()
            with :? SupersededLineDirectivesException when not ct.IsCancellationRequested && clock.Elapsed < directiveWait ->
                project.CountDirectiveRetry()
                Thread.Sleep 5
                attempt ()

        attempt ()

    /// FS0039 (UndefinedName: every "is not defined"), and a record label not found
    let private unresolvedCodes = set [ 39; 1129 ]

    let hasUnresolved (diagnostics: FSharpDiagnostic[]) =
        diagnostics
        |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error && unresolvedCodes.Contains d.ErrorNumber)

    /// The same setup apart from the parse: the same signature objects (FileSig.Next), and so the same entities, with
    /// the same files of the same name.
    let sameFiles (a: SetupKey) (b: SetupKey) =
        a.Files = b.Files
        && a.Fingerprints = b.Fingerprints
        && a.SameName = b.SameName
        && FileSig.SameBefore(a.Before, b.Before)

    /// A file checked whole reads all of its parse, and the signatures.
    let same (a: SetupKey) (b: SetupKey) =
        Nodes.same a.Parsed b.Parsed && sameFiles a b

    /// The setup of file `index` from its dependencies, or from every earlier file when `chain`. Every signature it
    /// holds is the current one: it reads them pending, before any check starts. Kept as the same object while `same`
    /// holds.
    ///
    /// A file set up from a signature file without its implementation file (graph mode's ArtificialImplFile) is set up
    /// with the implementation file too while that file's check fails: a failed check adds nothing to the files after
    /// it, the signature file's contents included (CheckOneInput's PartialResultOnError), so the signature does not
    /// stand for it. Whether it fails is a cut-off memo of its own (`project.FailedOf`), so its other edits change
    /// nothing here.
    let key (project: ProjectShared) (index: int) (deps: AsyncMemo<FileDepsValue>) (signatureOf: int -> Memo<FileSig>) (chain: bool) : Memo<SetupKey> =
        createMemo (fun previous ->
            let d = deps.Value

            let files, consulted =
                if chain then
                    Array.init index id, [||]
                else
                    let listed = d.Dependencies[index]
                    let present = HashSet<int>(listed)

                    let consulted =
                        [| for j in listed do
                               let i = d.ImplementationOf[j]

                               if i >= 0 && i < index && not (present.Contains i) then
                                   yield i |]

                    let failed = consulted |> Array.filter (fun i -> (project.FailedOf i).Value)
                    (if failed.Length = 0 then listed else Array.append listed failed |> Array.sort), consulted
            let signatures = Nodes.valuesOf (files |> Array.map signatureOf)

            let key =
                { Parsed = d.Parsed[index]
                  Files = files
                  Before = signatures |> Seq.map _.Signature |> List.ofSeq
                  Fingerprints = signatures |> Array.map _.Fingerprint
                  SameName = d.SameName[index]
                  HasDependents = d.HasDependents[index]
                  Consulted = consulted
                  Scoped = files.Length < index }

            match previous with
            | ValueSome p when same p key -> p
            | _ -> key)

    /// A file's result, checked off the graph thread from the setup its dependencies give (`scopedKey`, as `key`
    /// makes it), and from every earlier file when the checker refuses that setup (its dependency snapshot went stale)
    /// or the check reports an unresolved name. Each async memo reads its key before its check starts and returns its
    /// last value while the key is the same, so a wake that changed nothing runs no check.
    let build
        (project: ProjectShared)
        (path: string)
        (index: int)
        (deps: AsyncMemo<FileDepsValue>)
        (signatureOf: int -> Memo<FileSig>)
        (scopedKey: Memo<SetupKey>)
        (run: SetupKey -> 'R)
        (diagnosticsOf: 'R -> FSharpDiagnostic[] option)
        : Memo<SetupKey * 'R> =
        let chainKey = key project index deps signatureOf true
        let scoped = ref Unchecked.defaultof<AsyncMemo<Attempt<'R>>>
        let chained = ref Unchecked.defaultof<AsyncMemo<SetupKey * 'R>>

        scoped.Value <-
            createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct ->
                let key = scopedKey.Value
                let current = deps.Value
                let previous = scoped.Value.Peek

                let reuse =
                    not (isNull (box previous))
                    && match previous with
                       | Done(p, _)
                       | Unresolved p -> same p key
                       // Refused for the snapshot it was read from: tried again once FileDeps has a newer one
                       | Stale(p, seen) -> same p key && Nodes.same seen current

                if reuse then
                    Task.FromResult previous
                else
                    let hook = project.ScopedCheckHook

                    Task.Run<Attempt<'R>>(fun () ->
                        hook path

                        try
                            let result = runWhileDirectivesCurrent project ct (fun () -> run key)

                            match diagnosticsOf result with
                            | Some diagnostics when key.Scoped && hasUnresolved diagnostics -> Unresolved key
                            | _ -> Done(key, result)
                        // Only the refusal of a snapshot that is no longer current (StaleDependenciesException): any other
                        // refusal of `before` means the key and the snapshot disagree, a graph bug, and fails the node.
                        with :? StaleDependenciesException when key.Scoped ->
                            project.CountChainFallback()
                            Stale(key, current)))

        chained.Value <-
            createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct ->
                let key = chainKey.Value
                let previous = chained.Value.Peek

                if not (isNull (box previous)) && same (fst previous) key then
                    Task.FromResult previous
                else
                    Task.Run<SetupKey * 'R>(fun () -> key, runWhileDirectivesCurrent project ct (fun () -> run key)))

        createMemo (fun _ ->
            match scoped.Value.Value with
            | Done(key, result) -> key, result
            | _ -> chained.Value.Value)

/// What a file checked by declaration is set up from (BeginFile): its setup from its dependencies, and its header.
/// Kept as the same object while both are unchanged, whatever its parse, so the file is set up again only then.
[<ReferenceEquality>]
type internal DeclSetup =
    { /// The key the setup was first read with; a later body edit keeps the setup and changes the parse
      Key: SetupKey
      /// The text before the file's first declaration
      Prologue: string }

    member s.Same(other: DeclSetup) =
        Setup.sameFiles s.Key other.Key && Nodes.sameHeader s.Prologue other.Prologue
