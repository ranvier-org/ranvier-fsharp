namespace Rfs.Checker

open System
open System.Collections.Generic
open System.Text
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier

/// Readers share the lock and a writer holds it alone; a waiting writer goes before readers that arrive after it.
[<Sealed>]
type internal AsyncReaderWriterLock() =
    let gate = obj ()
    let mutable readers = 0
    let mutable writing = false
    let waitingReaders = ResizeArray<TaskCompletionSource<IDisposable>>()
    let waitingWriters = System.Collections.Generic.Queue<TaskCompletionSource<IDisposable>>()

    let waiter () =
        TaskCompletionSource<IDisposable>(TaskCreationOptions.RunContinuationsAsynchronously)

    let rec releaseRead () =
        lock gate (fun () ->
            readers <- readers - 1

            if readers = 0 && waitingWriters.Count > 0 then
                writing <- true
                waitingWriters.Dequeue().SetResult(writeRelease ()))

    and releaseWrite () =
        lock gate (fun () ->
            writing <- false

            if waitingReaders.Count > 0 then
                readers <- waitingReaders.Count

                for w in waitingReaders do
                    w.SetResult(readRelease ())

                waitingReaders.Clear()
            elif waitingWriters.Count > 0 then
                writing <- true
                waitingWriters.Dequeue().SetResult(writeRelease ()))

    and readRelease () =
        let mutable released = 0

        { new IDisposable with
            member _.Dispose() =
                if Interlocked.Exchange(&released, 1) = 0 then
                    releaseRead () }

    and writeRelease () =
        let mutable released = 0

        { new IDisposable with
            member _.Dispose() =
                if Interlocked.Exchange(&released, 1) = 0 then
                    releaseWrite () }

    member _.ReadAsync() : Task<IDisposable> =
        lock gate (fun () ->
            if not writing && waitingWriters.Count = 0 then
                readers <- readers + 1
                Task.FromResult(readRelease ())
            else
                let w = waiter ()
                waitingReaders.Add w
                w.Task)

    member _.WriteAsync() : Task<IDisposable> =
        lock gate (fun () ->
            if not writing && readers = 0 then
                writing <- true
                Task.FromResult(writeRelease ())
            else
                let w = waiter ()
                waitingWriters.Enqueue w
                w.Task)

/// The environment after a declaration, assembled from the one before it and the declaration's result (R14). A
/// declaration checked in it holds it, so it knows the result of every earlier declaration it was checked against.
[<Sealed; AllowNullLiteral>]
type internal EnvNode(previous: EnvNode, check: RowCheck, env: DeclEnv) =
    member _.Previous = previous
    /// Null for the environment BeginFile gave
    member _.Check = check
    member _.Env = env

    /// The result of `key` this environment holds, or null.
    member this.Find(key: DeclarationKey) : RowCheck =
        let mutable node = this
        let mutable found = null

        while isNull found && not (isNull node) && not (isNull node.Check) do
            if node.Check.Key = key then
                found <- node.Check

            node <- node.Previous

        found

/// A declaration's result, with what its check read.
and [<Sealed; AllowNullLiteral>] internal RowCheck
    (
        key: DeclarationKey,
        decl: ParsedDeclaration,
        start: StartInfo,
        result: DeclarationResult,
        envBefore: EnvNode,
        named: DeclarationKey[],
        identity: DeclarationKey[],
        later: DeclarationKey[]
    ) =
    let seen = Dictionary<DeclarationKey, RowCheck>(HashIdentity.Structural)

    member _.Key = key
    member _.Decl = decl
    member _.Start = start
    member _.Result = result
    member _.EnvBefore = envBefore
    member _.Named = named
    member _.Identity = identity
    /// The keys of the declarations after it when it was checked. A result that DependsOnLaterDeclarations is current
    /// only while they are the same (R12), also once it is the last declaration and no segment re-checks it.
    member _.Later = later

    /// Whether the check reported a name it could not resolve: such a diagnostic suggests the names in scope that
    /// look like it, so the result depends on every earlier declaration, its key and its fingerprint (T4-C1).
    member val Suggests =
        result.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 39 || d.ErrorNumber = 1129)

    /// The keys of the earlier declarations the check's environment held, in file order.
    member _.EarlierKeys: DeclarationKey[] =
        let keys = ResizeArray()
        let mutable node = envBefore

        while not (isNull node) && not (isNull node.Check) do
            keys.Add node.Check.Key
            node <- node.Previous

        keys.Reverse()
        keys.ToArray()

    /// The result of an earlier declaration the check's environment held, or null. Graph thread only.
    member _.Saw(k: DeclarationKey) : RowCheck =
        match seen.TryGetValue k with
        | true, c -> c
        | _ ->
            let c = envBefore.Find k
            seen[k] <- c
            c

/// A file set up for checking: one per file while its setup from its dependencies and its header are unchanged.
and [<Sealed; AllowNullLiteral>] internal StartInfo(start: FileStart, env0: DeclEnv, setup: DeclSetup) =
    let root = EnvNode(null, null, env0)
    let lock = AsyncReaderWriterLock()

    member _.Start = start
    member _.Root = root
    /// Checks and environment folds share it; FinishFile holds it alone.
    member _.Lock = lock
    /// What the file was set up from
    member _.Setup = setup

/// The results of a segment's declarations, checked in order from the environment before its first.
[<Sealed; AllowNullLiteral>]
type internal SegmentCheck(span: DeclarationKey[], start: StartInfo, rows: IReadOnlyDictionary<DeclarationKey, RowCheck>, envBefore: EnvNode) =
    let seen = Dictionary<DeclarationKey, RowCheck>(HashIdentity.Structural)

    member _.Span = span
    member _.Start = start
    member _.Rows = rows
    member _.EnvBefore = envBefore

    member _.Saw(k: DeclarationKey) : RowCheck =
        match seen.TryGetValue k with
        | true, c -> c
        | _ ->
            let c = envBefore.Find k
            seen[k] <- c
            c

/// A declaration's nodes, created when its key enters the file and disposed when it leaves.
[<AllowNullLiteral>]
type internal Row(file: ImplShared, key: DeclarationKey, item: unit -> ParsedDeclaration) as this =
    // Keeps the earlier object while the declaration is unchanged: a memo stores an equal new value too, and checks
    // compare what they read with what they were checked from by reference.
    let decl =
        createMemoWithComparer Nodes.declComparer (fun previous ->
            let current = item ()

            match previous with
            | ValueSome p when Nodes.declComparer.Equals(p, current) -> p
            | _ -> current)

    let content =
        createMemoWithComparer HashIdentity.Structural (fun _ -> DeclarationChecker.Content decl.Value)

    let typedSignal =
        createSignalWithComparer HashIdentity.Structural ([||]: DeclarationKey[])

    let dependencies =
        createMemoWithComparer HashIdentity.Structural (fun _ ->
            NameIndex.dependencies key decl.Value.Within content.Value file.Order.Value file.Index.Value typedSignal.Value)

    let role =
        createMemoWithComparer HashIdentity.Structural (fun _ ->
            file.Roles.Value |> Map.tryFind key |> Option.defaultValue Role.Single)

    let own =
        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct -> this.RunCheck ct)

    let segment =
        createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct -> this.RunSegment ct)

    // The result as the role gives it: the declaration's own check, or its row of the segment it is in
    let mutable check = Unchecked.defaultof<Memo<RowCheck>>

    do
        check <-
            createMemo (fun _ ->
                match role.Value with
                | Role.Single -> own.Value
                | Role.Leader _ -> segment.Value.Rows[key]
                | Role.Member leader -> (file.RowOf leader).Segment.Value.Rows[key])

    let latest = Nodes.latestOfMemo check

    let token =
        createMemoWithComparer StringComparer.Ordinal (fun _ -> latest.Value.Result.Fingerprint)

    member _.Key = key
    member _.Decl = decl
    member _.Content = content
    /// The file's declarations the latest published result names in TypedUses. Written after each finish, it makes the
    /// check run again and track the fingerprints of what its result turned out to depend on.
    member _.TypedSignal = typedSignal
    member _.Dependencies = dependencies
    member _.Role = role
    member _.Check: Memo<RowCheck> = check
    member _.Segment: AsyncMemo<SegmentCheck> = segment
    /// The latest settled check, kept while a new one runs
    member _.Latest = latest
    /// The fingerprint the check left: the cutoff for the file's declarations that depend on this one
    member _.Token = token
    /// The environment after this declaration, as last assembled. Concurrent checks may write it with different
    /// chains; a reader takes it only when its previous node and result are the ones it has, so a lost write only
    /// costs an ExtendEnv.
    member val EnvCache: EnvNode = null with get, set

    /// Waits for a declaration whose check is running, so a re-check sees its new result.
    member private _.AwaitSettled(keys: seq<DeclarationKey>) =
        for k in keys do
            (file.RowOf k).Check.Value |> ignore

    /// The declarations of the file a result depends on through types, as of its latest finish (R13).
    member private _.TypedOf(c: RowCheck) =
        NameIndex.typedKeys file.Path c.Key c.Result.TypedUses

    /// Checks the declaration on its own (read only while its role is Single), unless what it was last checked from is
    /// unchanged.
    member private this.RunCheck(ct: CancellationToken) : Task<RowCheck> =
        match role.Value with
        | Role.Member _
        | Role.Leader _ -> Task.FromException<RowCheck>(InvalidOperationException $"%A{key} is checked by a segment")
        | Role.Single ->
            let decl = decl.Value
            let deps = dependencies.Value
            let start = file.StartLatest.Value
            let previous = check.Peek
            let typed = if isNull previous then [||] else this.TypedOf previous
            let typedGone = typed |> Array.exists (file.Has >> not)

            // Tracked only for a result that depends on later declarations, whose current check reads them
            let later =
                if not (isNull previous) && previous.Result.DependsOnLaterDeclarations then
                    file.KeysAfter key
                else
                    untrack (fun () -> file.KeysAfter key)

            // A result that suggests names in scope depends on every earlier declaration's fingerprint (T4-C1, M6)
            let suggested = if not (isNull previous) && previous.Suggests then file.KeysBefore key else [||]

            let tokenKeys =
                Array.concat [ deps.Named; deps.Typed; typed |> Array.filter (fun k -> file.IsBefore(k, key)); suggested ]
                |> Array.distinct

            let tokens = tokenKeys |> Array.map (fun k -> k, (file.RowOf k).Token.Value)
            let identities = deps.Identity |> Array.map (fun k -> k, (file.RowOf k).Latest.Value)

            let current =
                not (isNull previous)
                && Nodes.same previous.Start start
                && Nodes.same previous.Decl decl
                && previous.Named = deps.Named
                && previous.Identity = deps.Identity
                && not typedGone
                && tokens
                   |> Array.forall (fun (k, t) ->
                       match previous.Saw k with
                       | null -> false
                       | seen -> seen.Result.Fingerprint = t)
                && identities |> Array.forall (fun (k, c) -> Nodes.same (previous.Saw k) c)
                && (not previous.Suggests || previous.EarlierKeys = file.KeysBefore key)
                && (not previous.Result.DependsOnLaterDeclarations || previous.Later = later)

            if current then
                Task.FromResult previous
            else
                let start = file.Start.Value
                this.AwaitSettled(Seq.append tokenKeys deps.Identity)
                let earlier = file.Earlier(key, start)

                // An earlier result with free inference variables whose segment has not formed yet: checks that solve
                // them in place must not run together, so this one runs alone
                let alone = earlier |> Array.exists (fun (_, c) -> c.Result.DependsOnLaterDeclarations)

                Task.Run<RowCheck>(
                    (fun () ->
                        task {
                            use! _ = if alone then start.Lock.WriteAsync() else start.Lock.ReadAsync()
                            let env = file.Walk(start, earlier, ct)
                            let result = DeclarationChecker.CheckDeclaration(start.Start, env.Env, decl, ct)
                            file.Project.CountCheck(key, file.Path)
                            return RowCheck(key, decl, start, result, env, deps.Named, deps.Identity, later)
                        }),
                    ct
                )

    /// Checks the declarations of the segment this declaration leads, in order (R11, R12). It re-checks them all when
    /// any of them would re-check: a changed declaration, a changed dependency outside the segment, or one added or
    /// removed.
    member private this.RunSegment(ct: CancellationToken) : Task<SegmentCheck> =
        match role.Value with
        | Role.Single
        | Role.Member _ -> Task.FromException<SegmentCheck>(InvalidOperationException $"%A{key} leads no segment")
        | Role.Leader span ->
            let start = file.StartLatest.Value
            let previous = segment.Peek
            let inSpan = HashSet<DeclarationKey>(span, HashIdentity.Structural)

            let members =
                span |> Array.map (fun k -> let r = file.RowOf k in r, r.Decl.Value, r.Dependencies.Value)

            let previousOf (r: Row) =
                if isNull previous then
                    null
                else
                    match previous.Rows.TryGetValue r.Key with
                    | true, c -> c
                    | _ -> null

            let outside (keys: seq<DeclarationKey>) =
                keys |> Seq.filter (inSpan.Contains >> not) |> Seq.distinct |> Array.ofSeq

            let tokenKeys =
                outside (
                    seq {
                        for r, _, deps in members do
                            yield! deps.Named
                            yield! deps.Typed

                            match previousOf r with
                            | null -> ()
                            | c ->
                                yield! this.TypedOf c |> Array.filter (fun k -> file.IsBefore(k, key))

                                if c.Suggests then
                                    yield! file.KeysBefore r.Key
                    }
                )

            let identityKeys = outside (seq { for _, _, deps in members do yield! deps.Identity })
            let tokens = tokenKeys |> Array.map (fun k -> k, (file.RowOf k).Token.Value)
            let identities = identityKeys |> Array.map (fun k -> k, (file.RowOf k).Latest.Value)

            let current =
                not (isNull previous)
                && Nodes.same previous.Start start
                && previous.Span = span
                && members
                   |> Array.forall (fun (r, decl, deps) ->
                       match previousOf r with
                       | null -> false
                       | c ->
                           Nodes.same c.Decl decl
                           && c.Named = deps.Named
                           && c.Identity = deps.Identity
                           && this.TypedOf c |> Array.forall file.Has
                           && (not c.Suggests || c.EarlierKeys = file.KeysBefore r.Key))
                && tokens
                   |> Array.forall (fun (k, t) ->
                       match previous.Saw k with
                       | null -> false
                       | seen -> seen.Result.Fingerprint = t)
                && identities |> Array.forall (fun (k, c) -> Nodes.same (previous.Saw k) c)

            if current then
                Task.FromResult previous
            else
                let start = file.Start.Value
                this.AwaitSettled(Seq.append tokenKeys identityKeys)
                let earlier = file.Earlier(key, start)
                let laterOf = dict [ for r, _, _ in members -> r.Key, untrack (fun () -> file.KeysAfter r.Key) ]

                Task.Run<SegmentCheck>(
                    (fun () ->
                        task {
                            use! _ = start.Lock.ReadAsync()
                            let envBefore = file.Walk(start, earlier, ct)
                            let mutable env = envBefore
                            let results = Dictionary<DeclarationKey, RowCheck>(HashIdentity.Structural)

                            for r, decl, deps in members do
                                ct.ThrowIfCancellationRequested()
                                let result = DeclarationChecker.CheckDeclaration(start.Start, env.Env, decl, ct)
                                file.Project.CountCheck(r.Key, file.Path)
                                let c = RowCheck(r.Key, decl, start, result, env, deps.Named, deps.Identity, laterOf[r.Key])
                                results[r.Key] <- c
                                let next = EnvNode(env, c, DeclarationChecker.ExtendEnv(env.Env, result))
                                r.EnvCache <- next
                                env <- next

                            return SegmentCheck(span, start, results, envBefore)
                        }),
                    ct
                )

/// The file-level nodes the rows of an implementation file read.
and [<AllowNullLiteral>] internal ImplShared(project: ProjectShared, path: string) =
    let rows = Dictionary<DeclarationKey, Row>(HashIdentity.Structural)

    member _.Project: ProjectShared = project
    member _.Path: string = path
    member _.Rows = rows
    member val Order: Memo<Order> = Unchecked.defaultof<_> with get, set
    member val Index: Memo<FileIndex> = Unchecked.defaultof<_> with get, set
    member val Roles: Memo<Map<DeclarationKey, Role>> = Unchecked.defaultof<_> with get, set
    member val Start: AsyncMemo<StartInfo> = Unchecked.defaultof<_> with get, set
    member val StartLatest: Memo<StartInfo> = Unchecked.defaultof<_> with get, set
    member val FinishLatest: Memo<Finished> = Unchecked.defaultof<_> with get, set

    member _.Has(key: DeclarationKey) : bool = rows.ContainsKey key

    /// Whether `k` is a declaration before `key`, untracked: what depends on the order tracks it elsewhere.
    member this.IsBefore(k: DeclarationKey, key: DeclarationKey) : bool =
        let order = untrack (fun () -> this.Order.Value)

        match order.TryPosition k, order.TryPosition key with
        | ValueSome a, ValueSome b -> a < b
        | _ -> false

    /// The keys of the declarations before `key`, in file order: a tracked read of the order.
    /// The keys of the declarations after key, in file order: a tracked read of the order.
    member this.KeysAfter(key: DeclarationKey) : DeclarationKey[] =
        let order = this.Order.Value

        match order.TryPosition key with
        | ValueSome position -> Array.sub order.Keys (position + 1) (order.Keys.Length - position - 1)
        | ValueNone -> [||]

    member this.KeysBefore(key: DeclarationKey) : DeclarationKey[] =
        let order = this.Order.Value

        match order.TryPosition key with
        | ValueSome position -> Array.sub order.Keys 0 position
        | ValueNone -> [||]

    member _.RowOf(key: DeclarationKey) : Row =
        match rows.TryGetValue key with
        | true, row -> row
        | _ -> invalidOp $"%A{key} is not a declaration of %s{path}"

    /// The current results of the declarations before `key`, all from `start`. One with no result yet, or one from
    /// another FileStart, is waited for: the environment is assembled from every earlier result. The others are read
    /// untracked, as they are: a change to one reaches this declaration only through its dependencies.
    member this.Earlier(key: DeclarationKey, start: StartInfo) : (Row * RowCheck)[] =
        let order = untrack (fun () -> this.Order.Value)

        match order.TryPosition key with
        | ValueNone -> invalidOp $"%A{key} is not a declaration of %s{path}"
        | ValueSome position ->
            Array.init position (fun i ->
                let row = this.RowOf order.Keys[i]
                let c = row.Check.Peek

                if not (isNull c) && Nodes.same c.Start start then
                    row, c
                else
                    let c = row.Check.Value

                    if not (Nodes.same c.Start start) then
                        invalidOp $"%A{row.Key} was checked from another FileStart"

                    row, c)

    /// The environment after the given results: ExtendEnv folded over them from BeginFile's, reusing each
    /// declaration's last assembled environment while what it was assembled from is the same.
    member _.Walk(start: StartInfo, earlier: (Row * RowCheck)[], ct: CancellationToken) : EnvNode =
        let mutable node = start.Root

        for row, c in earlier do
            ct.ThrowIfCancellationRequested()
            let cached = row.EnvCache

            if not (isNull cached) && Nodes.same cached.Previous node && Nodes.same cached.Check c then
                node <- cached
            else
                let next = EnvNode(node, c, DeclarationChecker.ExtendEnv(node.Env, c.Result))
                row.EnvCache <- next
                node <- next

        node

/// What a file is finished from: the parse of its current text, its FileStart and every declaration's result.
and [<ReferenceEquality>] internal FinishInputs =
    { Parsed: Parsed
      Start: StartInfo
      Checks: (Row * RowCheck)[] }

/// A finished file: the results it was built from and FinishFile's result.
and [<ReferenceEquality>] internal Finished =
    { Version: int64
      Inputs: FinishInputs
      FileResult: FileResult }

    member f.Parsed = f.Inputs.Parsed
    member f.Start = f.Inputs.Start
    member f.Checks = f.Inputs.Checks

/// A file of the project, in project order. Graph thread.
type internal IFileNode =
    abstract Path: string
    /// How the file is checked now
    abstract Mode: FileMode
    abstract Text: FileText
    /// What later files are checked against
    abstract Signature: Memo<FileSig>
    /// Whether the signature is settled and every file its published result's setup read (SetupKey's Files and
    /// Consulted) is current too (S1-R2). Never pending.
    abstract Current: Memo<bool>
    /// The file's diagnostics once its result is for text version `version` and was set up from the current
    /// signatures of the files it read. Its setup reads FileDeps, which reads every file's parse, so it also waits for
    /// the parse of every file whose write has been applied, not only its dependencies'.
    abstract Settled: version: int64 -> FSharpDiagnostic[] voption
    /// Each declaration's current result, when settled. Raises InvalidOperationException while the file is checked
    /// whole.
    abstract Results: version: int64 -> IReadOnlyDictionary<DeclarationKey, DeclarationResult> voption
    /// The FileResult published (FinishFile's, or CheckFile's while the file is checked whole), when settled
    abstract FileResult: version: int64 -> FileResult voption
    /// How many declarations are checked in segments (R11, R12) as the roles stand.
    abstract SegmentRows: unit -> int
    /// Marks the file wanted: it is built by declaration in the background, then checked by declaration.
    abstract Promote: unit -> unit
    /// Clears wanted and drops the file's declaration nodes.
    abstract Demote: unit -> unit
    /// The mode promoting the file has settled in for text version `version`, if it has: Declarations once it is
    /// checked by declaration for that text; FileLevel when it is not wanted or cannot be (a single declaration, a
    /// failed build, or an unresolved name in its setup).
    abstract Promoted: version: int64 -> FileMode voption
    /// Whether the file may be building by declaration: wanted, and not built, failed or a single declaration for its
    /// current state, or not yet known to be. A tracked read.
    abstract MayBuild: bool
    /// For tests: a declaration result, the environment it was checked in and the FileStart of the published
    /// declaration results, as weak references; empty while none are published.
    abstract DeclarationObjects: unit -> WeakReference list
    /// Creates the effects that keep the file checked. Called once every file's nodes exist.
    abstract Activate: unit -> unit
    /// Reads the file's setup keys and checks, so a key a lost wake-up left Dirty runs again (Setup.build). Graph
    /// thread, outside any run.
    abstract Kick: unit -> unit

/// The nodes that check an implementation file one declaration at a time: created when the file is promoted, and
/// disposed with their owner when it is demoted.
[<Sealed>]
type internal DeclFile
    (
        split: Memo<Parsed>,
        setup: Memo<DeclSetup>,
        current: Memo<Finished>,
        unresolved: Signal<DeclSetup voption>,
        publish: Finished -> unit,
        segmentRows: unit -> int
    ) =
    /// The file's parse in the dependency snapshot, split into declarations
    member _.Split = split
    /// What the file is set up from
    member _.Setup = setup
    /// The finish of the current results: pending while the setup or any result is
    member _.Current = current
    /// The setup of the latest finish of the current text, while that finish was set up from fewer than every earlier
    /// file and reported a name they do not give (C3). Written once such a finish settles, so reading it checks nothing.
    member _.Unresolved = unresolved
    /// Reports each result of `finished` not reported before (AdoptedChecks). Untracked; graph thread.
    member _.Publish(finished: Finished) = publish finished
    member _.SegmentRows() = segmentRows ()

    static member ResultsOf(finished: Finished) : IReadOnlyDictionary<DeclarationKey, DeclarationResult> =
        let d = Dictionary<DeclarationKey, DeclarationResult>(HashIdentity.Structural)

        for row, c in finished.Checks do
            d[row.Key] <- c.Result

        d :> IReadOnlyDictionary<_, _>

module internal FileNodes =

    /// How long BeginFile waits for its dependency snapshot to be current before it fails the node.
    let private staleWait = TimeSpan.FromSeconds 10.

    /// Tries a setup again while the checker refuses it because its dependency snapshot is not current: a newer parse
    /// is on its way to FileDeps, and the flight is cancelled once the setup's key has it.
    let private whileStale (ct: CancellationToken) (run: unit -> 'R) : 'R =
        let clock = Diagnostics.Stopwatch.StartNew()

        let rec attempt () =
            ct.ThrowIfCancellationRequested()

            try
                run ()
            with :? StaleDependenciesException when not ct.IsCancellationRequested && clock.Elapsed < staleWait ->
                Thread.Sleep 5
                attempt ()

        attempt ()

    /// Implementation file `index`, checked by declaration: per declaration its syntax, content, dependencies and
    /// check, then a finish over the current results. It is set up (BeginFile) from the setup its dependencies give
    /// (`scopedKey`), and again only when that setup or the file's header changes; then every declaration re-checks.
    /// Its effects run only while `driven` holds, so nothing is checked by declaration unless the file is being built
    /// or is promoted.
    let impl
        (project: ProjectShared)
        (file: FileText)
        (index: int)
        (deps: Memo<FileDepsValue>)
        (scopedKey: Memo<SetupKey>)
        (driven: Memo<bool>)
        : DeclFile =
        let ctx = project.Context
        let path = file.Path
        let shared = ImplShared(project, path)

        // The file's parse in the dependency snapshot, so the split is of the parse the setup is read with
        let ownParse =
            createMemoWithComparer HashIdentity.Reference (fun _ -> deps.Value.Parsed[index])

        let parsed =
            createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
                let p = ownParse.Value

                Task.Run<Parsed>(fun () ->
                    let declarations = DeclarationChecker.Split p.Parse

                    { p with
                        Declarations = declarations
                        Prologue = Nodes.prologueOf p.Text declarations }))

        // Read through a plain memo: the setup keeps its previous value while equal (see Nodes)
        let split = createMemo (fun _ -> parsed.Value)
        let parsedLatest = Nodes.latestOf parsed

        let setup =
            createMemo (fun previous ->
                let key = scopedKey.Value
                let p = split.Value
                project.SetupHook path

                if not (obj.ReferenceEquals(p.Parse, key.Parsed.Parse)) then
                    raise (GraphInvariantException $"%s{path}: the split is not of the parse the setup is read with")

                let s = { Key = key; Prologue = p.Prologue }

                match previous with
                | ValueSome(known: DeclSetup) when known.Same s -> known
                | _ -> s)

        let projection =
            createProjectionWith
                (fun (d: ParsedDeclaration) -> d.Key)
                (fun item ->
                    let first = item ()
                    let row = Row(shared, first.Key, item)
                    shared.Rows[first.Key] <- row
                    onCleanup (fun () -> shared.Rows.Remove first.Key |> ignore)
                    fun () -> row)
                (fun () -> parsedLatest.Value.Declarations :> seq<_>)

        shared.Order <- createMemoWithComparer Order.Comparer (fun _ -> Order(projection.Keys))

        shared.Index <-
            createMemoWithComparer HashIdentity.Structural (fun _ ->
                NameIndex.ofFile [| for k in shared.Order.Value.Keys -> k, (shared.RowOf k).Content.Value |])

        // The first declaration whose latest result depends on later ones (R12), written as soon as that result settles,
        // so its segment forms before the declarations after it are checked again on their own
        let dependsOnLater = createSignalWithComparer HashIdentity.Structural (None: DeclarationKey option)

        // Declarations checked on their own whose final fingerprint differed from the one their check left (the D1
        // premise broke): treated as leaders, so what depends on them is checked in order after them. Kept while present.
        let premiseLeaders = createSignalWithComparer HashIdentity.Structural (Set.empty: Set<DeclarationKey>)

        shared.Roles <-
            createMemoWithComparer HashIdentity.Structural (fun _ ->
                let order = shared.Order.Value

                let leader =
                    Seq.append (Option.toList dependsOnLater.Value) premiseLeaders.Value
                    |> Seq.choose (fun k -> order.TryPosition k |> ValueOption.toOption |> Option.map (fun p -> p, k))
                    |> Seq.sortBy fst
                    |> Seq.tryHead
                    |> Option.map snd

                NameIndex.roles order shared.Index.Value leader)

        createEffect (fun () ->
            if driven.Value then
                let order = shared.Order.Value

                let first =
                    order.Keys
                    |> Array.tryFind (fun k ->
                        match (shared.RowOf k).Latest.TryValue with
                        | Ready c -> c.Result.DependsOnLaterDeclarations
                        | _ -> false)

                untrack (fun () -> dependsOnLater.Value <- first))

        // The file is set up once per setup: its dependencies' signatures (compared by fingerprint, never by
        // reference) and its header. Body edits, and edits behind a .fsi, change neither, so they stay
        // declaration-level. Of the file's own text only the prologue is keyed on: setting up reads the file's module
        // or namespace header, and every declaration is checked on its own against whichever setup is current.
        let self = ref Unchecked.defaultof<AsyncMemo<StartInfo>>

        let start =
            createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ ct ->
                let s = setup.Value
                let previous = self.Value.Peek

                if not (isNull previous) && Nodes.same previous.Setup s then
                    Task.FromResult previous
                else
                    // The current parse and snapshot: `s` keeps the key it was first read with
                    let key = scopedKey.Value
                    let hook = project.BeginFileHook

                    Task.Run<StartInfo>(fun () ->
                        // A flight superseded before it began does not set the file up
                        ct.ThrowIfCancellationRequested()
                        hook path
                        let fileStart, env0 = whileStale ct (fun () -> DeclarationChecker.BeginFile(ctx, key.Before, key.Parsed.Parse))
                        StartInfo(fileStart, env0, s)))

        self.Value <- start
        shared.Start <- start
        shared.StartLatest <- Nodes.latestOf start

        // What the file is finished from, kept as the same object while every part is: pending while any part is.
        // The graph REQUIRES the Ranvier late-link fix (C2): this memo, the row checks and the segments first read
        // sources that may have been dirtied during the same run, and without the fix such a reader is never woken.
        let inputs =
            createMemo (fun previous ->
                let order = shared.Order.Value
                let p = split.Value
                let s = start.Value
                let checks = order.Keys |> Array.map (fun k -> let row = shared.RowOf k in row, row.Check.Value)

                if order.Keys <> (p.Declarations |> Array.map _.Key) then
                    invalidOp $"%s{path}: the declarations are not those of the parse being finished"

                for _, c in checks do
                    if not (Nodes.same c.Start s) then
                        invalidOp $"%s{path}: %A{c.Key} was checked from another FileStart"

                match previous with
                | ValueSome i when
                    Nodes.same i.Parsed p
                    && Nodes.same i.Start s
                    && i.Checks.Length = checks.Length
                    && Array.forall2 (fun (_, a) (_, b) -> Nodes.same a b) i.Checks checks
                    ->
                    i
                | _ -> { Parsed = p; Start = s; Checks = checks })

        // The async finish reads the inputs through a memo that is never pending once set, and runs FinishFile whenever
        // it runs, so it starts only when the inputs changed.
        let stableInputs = Nodes.latestOfMemo inputs

        let finish =
            createAsyncWithPolicy FlightPolicy.CancelPrevious (fun _ _ ->
                let i = stableInputs.Value

                Task.Run<Finished>(fun () ->
                    task {
                        use! _ = i.Start.Lock.WriteAsync()
                        let watch = Diagnostics.Stopwatch.StartNew()
                        // The parse of the text being finished: its diagnostics and #nowarn scopes are the ones published.
                        let fileResult =
                            DeclarationChecker.FinishFile(i.Start.Start, i.Checks |> Array.map (fun (_, c) -> c.Result), i.Parsed.Parse)

                        project.AddFinishTicks watch.ElapsedTicks
                        project.AddFinishReused fileResult.Reused
                        project.AddFinishPhaseTicks fileResult.PhaseTicks
                        project.AddPartsAllocated("finish.", fileResult.PhaseAllocated)

                        return
                            { Version = i.Parsed.Version
                              Inputs = i
                              FileResult = fileResult }
                    }))

        shared.FinishLatest <- Nodes.latestOf finish

        // Reading the inputs first makes the finish revalidate against them: a finish of older inputs is pending here
        let current =
            createMemo (fun _ ->
                let i = inputs.Value
                let f = finish.Value

                if not (Nodes.same f.Inputs i) then
                    invalidOp $"%s{path}: the finish is not of the current results"

                f)

        // What a finish of the current text tells the rows: TypedUses (R13), and the declarations whose final
        // fingerprint broke the D1 premise.
        let premiseChecked = HashSet<RowCheck>(HashIdentity.Reference)
        let unresolved = createSignal (ValueNone: DeclSetup voption)

        createEffect (fun () ->
            if driven.Value then
                let version = file.Text.Value.Version

                match current.TryValue with
                | Ready f when f.Version = version ->
                    untrack (fun () ->
                        unresolved.Value <-
                            if f.Start.Setup.Key.Scoped && Setup.hasUnresolved f.FileResult.Diagnostics then
                                ValueSome f.Start.Setup
                            else
                                ValueNone

                        let live = HashSet<DeclarationKey>(f.Checks |> Seq.map (fun (row, _) -> row.Key), HashIdentity.Structural)
                        premiseChecked.RemoveWhere(fun c -> not (live.Contains c.Key)) |> ignore

                        for row, c in f.Checks do
                            row.TypedSignal.Value <- NameIndex.typedKeys path row.Key c.Result.TypedUses

                        // D1: a declaration is depended on through the fingerprint its check left. The finish may change
                        // it only for a result flagged DependsOnLaterDeclarations, whose R12 segment re-checks it with
                        // every later declaration. Every other result outside the R12 segment (R11 span members
                        // included) is checked: a difference is counted and the declaration leads a segment from then on
                        // (kept while it is in the file), so a later declaration that could have changed it re-checks
                        // it first.
                        let order = shared.Order.Value
                        let mutable leaders = premiseLeaders.Peek |> Set.filter live.Contains

                        let r12Start =
                            Seq.append (Option.toList dependsOnLater.Peek) leaders
                            |> Seq.choose (fun k -> order.TryPosition k |> ValueOption.toOption)
                            |> Seq.fold min Int32.MaxValue

                        for i in 0 .. min r12Start f.Checks.Length - 1 do
                            let row, c = f.Checks[i]

                            if not c.Result.DependsOnLaterDeclarations && premiseChecked.Add c then
                                let provisional = c.Result.Fingerprint
                                let final = f.FileResult.Fingerprints[row.Key]

                                if not (provisional.StartsWith "?") && not (final.StartsWith "?") && provisional <> final then
                                    project.CountPremiseBreak
                                        { Key = row.Key
                                          File = path
                                          Line = c.Decl.Range.StartLine
                                          Provisional = provisional
                                          Final = final }
                                    leaders <- leaders.Add row.Key

                        premiseLeaders.Value <- leaders)
                | _ -> ())

        // Each result is reported once, when a finish that holds it is published
        let published = Dictionary<DeclarationKey, RowCheck>(HashIdentity.Structural)

        let publish (f: Finished) =
            for row, c in f.Checks do
                match published.TryGetValue row.Key with
                | true, known when Nodes.same known c -> ()
                | _ ->
                    published[row.Key] <- c
                    project.Publish(row.Key, path)

            let live = HashSet<DeclarationKey>(f.Checks |> Seq.map (fun (row, _) -> row.Key), HashIdentity.Structural)

            for k in List.ofSeq published.Keys do
                if not (live.Contains k) then
                    published.Remove k |> ignore

        DeclFile(split, setup, current, unresolved, publish, fun () -> shared.Roles.Value |> Seq.filter (fun kv -> kv.Value <> Role.Single) |> Seq.length)

