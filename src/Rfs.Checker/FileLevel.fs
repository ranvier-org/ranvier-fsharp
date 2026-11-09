namespace Rfs.Checker

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier

/// How far promoting an implementation file has got for its current state.
type internal Build =
    /// Not wanted
    | Idle
    /// Its parse of this version splits into one declaration (every module rec and namespace rec file): never promoted
    | Single of version: int64
    /// Wanted, and not yet built by declaration for its current setup
    | Building
    /// Its build failed for its current setup and text: not built again until one of them changes
    | BuildFailed
    /// Built by declaration for its current setup
    | Built

/// What a demoted file keeps of its last finish: only what is published from it.
[<NoEquality; NoComparison>]
type internal KeptResult =
    { Version: int64
      /// The setup the finish was built on: kept only while the current one has the same files and fingerprints
      Key: SetupKey
      /// The parse the finish was of. The setup's is older once a body was edited.
      Parsed: Parsed
      Result: FileResult
      /// The lock of the FileStart it was finished on: a flight still running on that start can change the types it holds
      Lock: AsyncReaderWriterLock }

/// Which result of an implementation file is the published one.
[<NoEquality; NoComparison>]
type internal Published =
    /// CheckFile's
    | Whole
    /// The finish of its current declaration results
    | ByDeclaration of DeclFile * Finished
    /// Demoted: the last finish's FileResult, kept until the file's text or setup changes
    | Kept of KeptResult

/// A published result with what a reader of it needs.
[<NoEquality; NoComparison>]
type internal Readable =
    { Parsed: Parsed
      Result: FileResult
      /// What the result was set up from
      Key: SetupKey
      /// The lock of the FileStart a declaration-level result was finished on: a reader holds it shared, so no finish
      /// runs on that FileStart meanwhile
      Lock: AsyncReaderWriterLock voption
      /// The finish a declaration-level result is, with each declaration's role
      Declarations: (Finished * Map<DeclarationKey, Role>) voption
      /// The same object while the same result is published
      Identity: obj }

/// An implementation file's node, read for its settled published result.
type internal IReadableFile =
    /// The published result when it is for text version `version` and the files it was set up from are current
    abstract Readable: version: int64 -> Readable voption

/// A signature file's node, read for what its text says now.
type internal ISignatureFile =
    /// Its latest check when it is for text version `version`. Later files may be set up from an earlier check of the
    /// same print (FileSig.Next), whose ranges are older.
    abstract LatestCheck: version: int64 -> FileSignature voption

/// Files checked whole (FileLevel): an implementation file by CheckFile, a signature file by CheckSignatureFile, each
/// set up from the signatures of the files it depends on. An implementation file can be promoted to be checked one
/// declaration at a time.
module internal WholeFile =

    /// Whether every file the key read is current (IFileNode.Current): the files whose signatures it holds, and the
    /// implementation files whose failed status it read (SetupKey.Consulted). While a file is checked again its status
    /// is its last answer (S1-R), and a ready signature may have been computed from such an answer further up, so being
    /// ready is not enough (S1-R2). Tracked reads when run inside a memo; a settle wait re-reads after each pump.
    let readsCurrent (project: ProjectShared) (key: SetupKey) =
        let current i =
            match (project.CurrentOf i).TryValue with
            | Ready c -> c
            | _ -> false

        Array.forall current key.Files && Array.forall current key.Consulted

    /// A file's Current: its signature is settled, and so is everything the setup of the result it is from read. Cut
    /// off on the answer. A failed check counts as current: it is raised where its result is read, so nothing waits on
    /// it.
    let currentOf (project: ProjectShared) (signature: Memo<FileSig>) (key: Memo<SetupKey>) : Memo<bool> =
        createMemoWithComparer HashIdentity.Structural (fun _ ->
            match signature.TryValue, key.TryValue with
            | Ready _, Ready k -> readsCurrent project k
            | Failed _, _
            | _, Failed _ -> true
            | _ -> false)

    /// The file's result while it is for text version `version`, and was set up from the current signatures of the
    /// files it read: a memo that is ready is consistent with every node it read, the setup read those signatures
    /// pending, and they are current.
    let private settledAt (project: ProjectShared) (file: FileText) (result: Memo<SetupKey * 'R>) (version: int64) : (SetupKey * 'R) voption =
        if file.Version <> version then
            ValueNone
        else
            match result.TryValue with
            | Ready((key, _) as r) when key.Parsed.Version = version && readsCurrent project key -> ValueSome r
            | Failed e -> raise (InvalidOperationException($"%s{file.Path} failed to check", e))
            | _ -> ValueNone

    let private wholeOnly (path: string) =
        InvalidOperationException $"%s{path} is checked file-level: it has no declaration results"

    /// Implementation file `index` of the project.
    ///
    /// Checked whole by CheckFile, whose result is published only while it is for the current text, so a check of a
    /// superseded text is never published.
    ///
    /// Promote marks it wanted. A wanted file is built by declaration in the background (BeginFile, a check per
    /// declaration, FinishFile), when it is the project's builder (`ProjectShared.Builder`: one file at a time). Once
    /// the build has finished for its current text and setup, its finish is the published result, and edits that keep
    /// its setup re-check only what they change. Until then, and whenever its setup changes again (a dependency's
    /// signature, its dependency list or its header: R16's restart), CheckFile's result is the published one, and it
    /// is built again while it is still wanted. A file whose parse is one declaration gets no declaration nodes; a
    /// build that fails is not tried again until the file's setup or text changes; and the result of a build whose
    /// setup from dependencies reports an unresolved name (C3/R17) is never published.
    ///
    /// The build state is computed where it is read (`stateOf`). It moved off a memo of its own at Task 4, for a lost
    /// wake-up since fixed in Ranvier (#303). A memo would now be allowed, but nothing has measured one in its favour.
    let impl
        (project: ProjectShared)
        (index: int)
        (file: FileText)
        (deps: AsyncMemo<FileDepsValue>)
        (signatureOf: int -> Memo<FileSig>)
        : IFileNode =
        let ctx = project.Context
        let path = file.Path
        let scopedKey = Setup.key project index deps signatureOf false
        // The scope the declaration nodes' roots attach to: the graph's, not an effect's
        let graphOwner = getOwner ()

        let whole =
            Setup.build
                project
                path
                index
                deps
                signatureOf
                scopedKey
                (fun key ->
                    // A file nothing depends on takes its signature's fingerprint only if something asks for it later
                    let r =
                        project.Measure("checkFile", fun () -> DeclarationChecker.CheckFile(ctx, key.Before, key.Parsed.Parse, key.HasDependents))
                    project.AddFinishPhaseTicks r.PhaseTicks
                    project.AddPartsAllocated("checkFile.", r.PhaseAllocated)
                    project.CountFileLevelCheck()
                    r)
                (fun r -> Some r.Diagnostics)

        let wanted = createSignal false
        let decl = createSignal (ValueNone: DeclFile voption)
        let owner = ref Unchecked.defaultof<Owner>
        // The setup the latest build finished for, compared by reference with the current one
        let promoted = createSignal (ValueNone: DeclSetup voption)
        // The setup and text version a build failed for
        let failed = createSignal (ValueNone: struct (DeclSetup * int64) voption)
        let kept = createSignal (ValueNone: KeptResult voption)

        // How many declarations the file's own parse splits into, counted once per version without splitting it.
        // Read only while the file is wanted.
        let rows =
            createMemo (fun _ ->
                let p = file.Parsed.Value
                struct (p.Version, DeclarationChecker.CountDeclarations p.Parse))

        /// The declaration nodes' setup, or None when computing it failed: the failure stays in this file, which is then
        /// checked whole, as after a failed build (N1).
        let setupOf (g: DeclFile) =
            try
                ValueSome g.Setup.Value
            with
            | NotReadyException _ -> reraise ()
            // An invariant of the graph broken: a bug, never degraded to a whole check
            | :? GraphInvariantException -> reraise ()
            | e ->
                Diagnostics.Trace.WriteLine $"Rfs.Checker: setting %s{path} up by declaration failed: %O{e}"
                ValueNone

        /// The build state as it stands, from tracked reads; pending while any of them is.
        let stateOf () =
            if not wanted.Value then
                Idle
            else
                let struct (version, count) = rows.Value

                if count <= 1 then
                    Single version
                else
                    match decl.Value with
                    // The nodes are made by an effect once the file is known to have more than one declaration
                    | ValueNone -> Building
                    | ValueSome g ->
                        match setupOf g with
                        | ValueNone -> BuildFailed
                        | ValueSome setup ->
                            match promoted.Value, failed.Value with
                            | ValueSome known, _ when Nodes.same known setup -> Built
                            | _, ValueSome(struct (at, v)) when Nodes.same at setup && v = file.Text.Value.Version -> BuildFailed
                            | _ -> Building

        /// The state, or None while it is pending.
        let stateNow () =
            try
                ValueSome(stateOf ())
            with NotReadyException _ ->
                ValueNone

        /// Whether the latest finish of the current setup reported a name its dependencies do not give (C3), and the
        /// whole check of the current text from the same dependencies still does: until it no longer does, nothing is
        /// checked by declaration, as the finish could not be published (M12). Tracked reads; the whole check is read
        /// only after such a finish.
        let blockedOf (g: DeclFile) =
            match g.Unresolved.Value, setupOf g with
            | ValueSome at, ValueSome setup when Nodes.same at setup ->
                let key, _ = whole.Value
                not key.Scoped
            | _ -> false

        // Whether the declaration nodes run: while built, or while this file is the project's builder
        let driven =
            createMemo (fun _ ->
                match stateNow (), decl.Value with
                | ValueSome Built, ValueSome g ->
                    try
                        not (blockedOf g)
                    with NotReadyException _ ->
                        false
                | ValueSome Building, _ -> project.Builder.Value = ValueSome index
                | _ -> false)

        let published =
            createMemo (fun _ ->
                if wanted.Value then
                    match decl.Value with
                    | ValueSome g when stateOf () = Built && blockedOf g -> Whole
                    | ValueSome g when stateOf () = Built ->
                        let f = g.Current.Value

                        if not (Nodes.same f.Start.Setup g.Setup.Value) then
                            invalidOp $"%s{path}: the finish is not of the current setup"

                        // A name its dependencies do not give may come from an earlier file they leave out (C3): CheckFile
                        // falls back to every earlier file
                        if f.Start.Setup.Key.Scoped && Setup.hasUnresolved f.FileResult.Diagnostics then
                            Whole
                        else
                            ByDeclaration(g, f)
                    | _ -> Whole
                else
                    match kept.Value with
                    | ValueSome k when file.Text.Value.Version = k.Version && Setup.sameFiles scopedKey.Value k.Key -> Kept k
                    | _ -> Whole)

        // Cut off by fingerprint. CheckFile and FinishFile give one text and setup the same fingerprint,
        // compiler-generated names included (M2), so a promotion or a restart changes nothing after the file.
        let signature =
            createMemoWithComparer FileSig.Comparer (fun previous ->
                match published.Value with
                | ByDeclaration(_, f) -> FileSig.Next(previous, f.FileResult.Signature, f.Start.Setup.Key.Before)
                | Kept k -> FileSig.Next(previous, k.Result.Signature, k.Key.Before)
                | Whole ->
                    let key, r = whole.Value
                    FileSig.Next(previous, r.Signature, key.Before))

        // The setup of the published result
        let publishedKey =
            createMemo (fun _ ->
                match published.Value with
                | ByDeclaration(_, f) -> f.Start.Setup.Key
                | Kept k -> k.Key
                | Whole -> fst whole.Value)

        let isCurrent = currentOf project signature publishedKey

        let current (version: int64) =
            if file.Version <> version then
                ValueNone
            else
                match published.TryValue with
                | Ready p -> ValueSome p
                | Failed e -> raise (InvalidOperationException($"%s{path} failed to check", e))
                | _ -> ValueNone

        let finishedAt (version: int64) (f: Finished) =
            if f.Version = version && readsCurrent project f.Start.Setup.Key then ValueSome f else ValueNone

        let keptAt (version: int64) (k: KeptResult) =
            if k.Version = version && readsCurrent project k.Key then ValueSome k.Result else ValueNone

        /// Disposes the owner of dropped declaration nodes, once their readers have moved off them. A memo keeps its
        /// sources until it runs again, and a disposed node its last value, so the memos that read the nodes run again
        /// first: otherwise one nothing reads (driven, the builder) would keep the declaration state alive. Outside any
        /// run, on the graph thread.
        let release (o: Owner) =
            flush ()

            untrack (fun () ->
                driven.TryValue |> ignore
                published.TryValue |> ignore
                signature.TryValue |> ignore
                project.Builder.TryValue |> ignore)

            if not (isNull (box o)) then
                o.Dispose()

        let activate () =
            // Keeps the published result checked, so files that do not depend on each other check in parallel; reports
            // the declaration results of a published finish of the current text
            GraphEffects.guarded (fun () ->
                match published.Value with
                | Whole ->
                    whole.TryValue |> ignore

                    if untrack (fun () -> not wanted.Value && kept.Value.IsSome) then
                        untrack (fun () -> kept.Value <- ValueNone)
                | ByDeclaration(g, f) ->
                    if f.Version = file.Text.Value.Version then
                        untrack (fun () -> g.Publish f)
                | Kept _ -> ())

            // Makes the declaration nodes once a wanted file is known to have more than one declaration
            GraphEffects.guarded (fun () ->
                if wanted.Value then
                    let struct (_, count) = rows.Value

                    if count > 1 && untrack (fun () -> decl.Value.IsNone) then
                        untrack (fun () ->
                            let g =
                                runWithOwner graphOwner (fun () ->
                                    createRoot (fun o ->
                                        project.CountDeclarationRoot()
                                        owner.Value <- o
                                        FileNodes.impl project file index deps scopedKey driven))

                            decl.Value <- ValueSome g)
                    elif count <= 1 && untrack (fun () -> decl.Value.IsSome) then
                        // A wanted file whose text became one declaration keeps none of its declaration state (N2)
                        let o = owner.Value
                        owner.Value <- Unchecked.defaultof<_>

                        untrack (fun () ->
                            decl.Value <- ValueNone
                            promoted.Value <- ValueNone
                            failed.Value <- ValueNone)

                        project.Post(fun () -> release o))

            // Promotes the file once its build has finished for its current text and setup; records a failed build, so
            // the builder moves on to the next wanted file
            GraphEffects.guarded (fun () ->
                if driven.Value then
                    match decl.Value with
                    | ValueSome g when stateOf () = Building ->
                        let version = file.Text.Value.Version
                        let setup = g.Setup.Value

                        let outcome =
                            try
                                Ok g.Current.Value
                            with
                            | NotReadyException _ -> reraise ()
                            | e -> Error e

                        match outcome with
                        | Ok f ->
                            if f.Version = version then
                                untrack (fun () -> promoted.Value <- ValueSome f.Start.Setup)
                        | Error e ->
                            Diagnostics.Trace.WriteLine $"Rfs.Checker: building %s{path} by declaration failed: %O{e}"
                            untrack (fun () -> failed.Value <- ValueSome(struct (setup, version)))
                    | _ -> ())

        { new IFileNode with
            member _.Path = path

            // Pending (mid-edit) reads as FileLevel: the mode of the published result, not the one the file settles in
            member _.Mode =
                match published.TryValue with
                | Ready(ByDeclaration _) -> Declarations
                | _ -> FileLevel

            member _.Text = file
            member _.Signature = signature
            member _.Current = isCurrent

            member _.Settled version =
                match current version with
                | ValueSome(ByDeclaration(_, f)) -> finishedAt version f |> ValueOption.map _.FileResult.Diagnostics
                | ValueSome(Kept k) -> keptAt version k |> ValueOption.map _.Diagnostics
                | ValueSome Whole -> settledAt project file whole version |> ValueOption.map (fun (_, r) -> r.Diagnostics)
                | ValueNone -> ValueNone

            member _.Results version =
                match current version with
                | ValueSome(ByDeclaration(_, f)) -> finishedAt version f |> ValueOption.map DeclFile.ResultsOf
                | ValueSome _ -> raise (wholeOnly path)
                | ValueNone -> ValueNone

            member _.FileResult version =
                match current version with
                | ValueSome(ByDeclaration(_, f)) -> finishedAt version f |> ValueOption.map _.FileResult
                | ValueSome(Kept k) -> keptAt version k
                | ValueSome Whole -> settledAt project file whole version |> ValueOption.map snd
                | ValueNone -> ValueNone

            member _.SegmentRows() =
                match decl.Peek with
                | ValueSome g -> g.SegmentRows()
                | ValueNone -> 0

#if RANVIER_TRACE
            member _.CheckNode key =
                match decl.Peek with
                | ValueSome g -> g.CheckNodeOf key
                | ValueNone -> ValueNone

            member _.TracedNodes() =
                ("filelevel", whole :> INode)
                :: (match decl.Peek with
                    | ValueSome g -> g.TracedNodes
                    | ValueNone -> [])
#endif

            member _.Promote() =
                if not wanted.Peek then
                    batch (fun () ->
                        kept.Value <- ValueNone
                        failed.Value <- ValueNone
                        wanted.Value <- true)

            member _.Demote() =
                if wanted.Peek then
                    // What the finish published now publishes stays published until the file's text or setup changes;
                    // nothing else of the declaration state is kept
                    let last =
                        match untrack (fun () -> published.TryValue) with
                        | Ready(ByDeclaration(_, f)) ->
                            ValueSome
                                { Version = f.Version
                                  Key = f.Start.Setup.Key
                                  Parsed = f.Parsed
                                  Result = f.FileResult
                                  Lock = f.Start.Lock }
                        | _ -> ValueNone

                    batch (fun () ->
                        kept.Value <- last
                        wanted.Value <- false
                        decl.Value <- ValueNone
                        promoted.Value <- ValueNone
                        failed.Value <- ValueNone)

                    let o = owner.Value
                    owner.Value <- Unchecked.defaultof<_>
                    release o

            member _.Promoted version =
                if file.Version <> version then
                    ValueNone
                elif not wanted.Peek then
                    ValueSome FileLevel
                else
                    // Each answer that depends on the setup waits until what the setup read is current (m4): a status
                    // that flips then sets the file up again
                    match stateNow (), decl.Peek with
                    | ValueSome(Single v), _ -> if v = version then ValueSome FileLevel else ValueNone
                    | ValueSome BuildFailed, _ ->
                        match failed.Peek with
                        | ValueSome(struct (at, _)) when not (readsCurrent project at.Key) -> ValueNone
                        | _ -> ValueSome FileLevel
                    | ValueSome Built, ValueSome g when (try blockedOf g with NotReadyException _ -> true) ->
                        // Read before the finish, which is not checked while blocked (or while that is not known)
                        match (try ValueSome(blockedOf g) with NotReadyException _ -> ValueNone), whole.TryValue with
                        | ValueSome true, Ready(key, _) when key.Parsed.Version = version && readsCurrent project key -> ValueSome FileLevel
                        | _, Failed e -> raise (InvalidOperationException($"%s{path} failed to check", e))
                        | _ -> ValueNone
                    | ValueSome Built, ValueSome g ->
                        match g.Current.TryValue, published.TryValue with
                        | Ready f, Ready(ByDeclaration(_, p)) when f.Version = version && Nodes.same p f && readsCurrent project f.Start.Setup.Key ->
                            ValueSome Declarations
                        | Ready f, Ready Whole when f.Version = version && readsCurrent project f.Start.Setup.Key -> ValueSome FileLevel
                        | Failed e, _
                        | _, Failed e -> raise (InvalidOperationException($"%s{path} failed to check", e))
                        | _ -> ValueNone
                    | _ -> ValueNone

            // Pending counts as possibly building: the builder stays put while it is not known
            member _.MayBuild =
                wanted.Value
                && (match stateNow () with
                    | ValueSome Building
                    | ValueNone -> true
                    | ValueSome _ -> false)

            member _.DeclarationObjects() =
                match published.TryValue with
                | Ready(ByDeclaration(_, f)) ->
                    let _, c = Array.last f.Checks
                    [ WeakReference c; WeakReference c.EnvBefore; WeakReference f.Start.Start; WeakReference c.Result ]
                | _ -> []

            member _.Activate() = activate ()

          interface IReadableFile with
              member _.Readable version =
                  match current version with
                  | ValueSome(ByDeclaration(g, f)) ->
                      finishedAt version f
                      |> ValueOption.map (fun f ->
                          { Parsed = f.Parsed
                            Result = f.FileResult
                            Key = f.Start.Setup.Key
                            Lock = ValueSome f.Start.Lock
                            Declarations = ValueSome(f, g.Roles())
                            Identity = box f })
                  | ValueSome(Kept k) ->
                      keptAt version k
                      |> ValueOption.map (fun r ->
                          { Parsed = k.Parsed
                            Result = r
                            Key = k.Key
                            Lock = ValueSome k.Lock
                            Declarations = ValueNone
                            Identity = box k })
                  | ValueSome Whole ->
                      settledAt project file whole version
                      |> ValueOption.map (fun (key, r) ->
                          { Parsed = key.Parsed
                            Result = r
                            Key = key
                            Lock = ValueNone
                            Declarations = ValueNone
                            Identity = box r })
                  | ValueNone -> ValueNone }

    /// A signature file, file `index` of the project. Its diagnostics are its parse's: CheckSignatureFile reports none.
    let signature
        (project: ProjectShared)
        (index: int)
        (file: FileText)
        (deps: AsyncMemo<FileDepsValue>)
        (signatureOf: int -> Memo<FileSig>)
        : IFileNode =
        let ctx = project.Context

        let result =
            Setup.build
                project
                file.Path
                index
                deps
                signatureOf
                (Setup.key project index deps signatureOf false)
                (fun key ->
                    project.Measure("checkSignatureFile", fun () -> DeclarationChecker.CheckSignatureFile(ctx, key.Before, key.Parsed.Parse)))
                (fun _ -> None)

        // Cut off by fingerprint
        let signature =
            createMemoWithComparer FileSig.Comparer (fun previous ->
                let key, s = result.Value
                FileSig.Next(previous, s, key.Before))

        let isCurrent = currentOf project signature (createMemo (fun _ -> fst result.Value))

        { new IFileNode with
            member _.Path = file.Path
            member _.Mode = FileLevel
            member _.Text = file
            member _.Signature = signature
            member _.Current = isCurrent
            member _.Settled version = settledAt project file result version |> ValueOption.map (fun (key, _) -> key.Parsed.Parse.Diagnostics)
            member _.Results _ = raise (wholeOnly file.Path)
            member _.FileResult _ = ValueNone
            member _.SegmentRows() = 0
#if RANVIER_TRACE
            member _.CheckNode _ = ValueNone
            member _.TracedNodes() = [ "filelevel", result :> INode ]
#endif
            member _.Promote() = ()
            member _.Demote() = ()
            member _.Promoted version = if file.Version = version then ValueSome FileLevel else ValueNone
            member _.MayBuild = false
            member _.DeclarationObjects() = []
            member _.Activate() =
                GraphEffects.guarded (fun () -> result.TryValue |> ignore)

          interface ISignatureFile with
              member _.LatestCheck version = settledAt project file result version |> ValueOption.map snd }
