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
      Result: FileResult }

/// Which result of an implementation file is the published one.
[<NoEquality; NoComparison>]
type internal Published =
    /// CheckFile's
    | Whole
    /// The finish of its current declaration results
    | ByDeclaration of DeclFile * Finished
    /// Demoted: the last finish's FileResult, kept until the file's text or setup changes
    | Kept of KeptResult

/// Files checked whole (FileLevel): an implementation file by CheckFile, a signature file by CheckSignatureFile, each
/// set up from the signatures of the files it depends on. An implementation file can be promoted to be checked one
/// declaration at a time.
module internal WholeFile =

    /// The file's result while it is for text version `version`, and was set up from the current signatures of the
    /// files it read: a memo that is ready is consistent with every node it read, and the setup read those signatures
    /// pending.
    let private settledAt (file: FileText) (result: Memo<SetupKey * 'R>) (version: int64) : (SetupKey * 'R) voption =
        if file.Version <> version then
            ValueNone
        else
            match result.TryValue with
            | Ready((key, _) as r) when key.Parsed.Version = version -> ValueSome r
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
    /// The build state is computed where it is read (`stateOf`), never read through a memo of its own: a memo pending
    /// on such a memo, which other nodes read with TryValue, was seen not to be woken when it settled (a Ranvier lost
    /// wake-up, Task 4).
    let impl
        (project: ProjectShared)
        (index: int)
        (file: FileText)
        (deps: Memo<FileDepsValue>)
        (signatureOf: int -> Memo<FileSig>)
        : IFileNode =
        let ctx = project.Context
        let path = file.Path
        let scopedKey = Setup.key index deps signatureOf false
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
                    let r = project.Measure("checkFile", fun () -> DeclarationChecker.CheckFile(ctx, key.Before, key.Parsed.Parse))
                    project.AddFinishPhaseTicks r.PhaseTicks
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
                        let setup = g.Setup.Value

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

        // Whether the declaration nodes run: while built, or while this file is the project's builder
        let driven =
            createMemo (fun _ ->
                match stateNow () with
                | ValueSome Built -> true
                | ValueSome Building -> project.Builder.Value = ValueSome index
                | _ -> false)

        let published =
            createMemo (fun _ ->
                if wanted.Value then
                    match decl.Value with
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

        // Cut off by fingerprint, reading plain memos, never an async check directly (see Nodes). CheckFile and
        // FinishFile give one text and setup the same fingerprint, so a promotion changes nothing after the file, unless
        // the signature holds compiler-generated names: a declaration's are numbered from the declaration, so the files
        // after it are then checked again.
        let signature =
            createMemoWithComparer FileSig.Comparer (fun _ ->
                match published.Value with
                | ByDeclaration(_, f) -> FileSig.Of f.FileResult.Signature
                | Kept k -> FileSig.Of k.Result.Signature
                | Whole -> FileSig.Of (snd whole.Value).Signature)

        let current (version: int64) =
            if file.Version <> version then
                ValueNone
            else
                match published.TryValue with
                | Ready p -> ValueSome p
                | Failed e -> raise (InvalidOperationException($"%s{path} failed to check", e))
                | _ -> ValueNone

        let finishedAt (version: int64) (f: Finished) =
            if f.Version = version then ValueSome f else ValueNone

        let keptAt (version: int64) (k: KeptResult) =
            if k.Version = version then ValueSome k.Result else ValueNone

        let activate () =
            // Keeps the published result checked, so files that do not depend on each other check in parallel; reports
            // the declaration results of a published finish of the current text
            createEffect (fun () ->
                // Also keeps the signature observed when no later file reads it: a published result pending on a source
                // that had settled was seen to be woken only through the signature (a Ranvier lost wake-up)
                signature.TryValue |> ignore

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
            createEffect (fun () ->
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

                            decl.Value <- ValueSome g))

            // Promotes the file once its build has finished for its current text and setup; records a failed build, so
            // the builder moves on to the next wanted file
            createEffect (fun () ->
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

            member _.Settled version =
                match current version with
                | ValueSome(ByDeclaration(_, f)) -> finishedAt version f |> ValueOption.map _.FileResult.Diagnostics
                | ValueSome(Kept k) -> keptAt version k |> ValueOption.map _.Diagnostics
                | ValueSome Whole -> settledAt file whole version |> ValueOption.map (fun (_, r) -> r.Diagnostics)
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
                | ValueSome Whole -> settledAt file whole version |> ValueOption.map snd
                | ValueNone -> ValueNone

            member _.SegmentRows() =
                match decl.Peek with
                | ValueSome g -> g.SegmentRows()
                | ValueNone -> 0

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
                                  Result = f.FileResult }
                        | _ -> ValueNone

                    batch (fun () ->
                        kept.Value <- last
                        wanted.Value <- false
                        decl.Value <- ValueNone
                        promoted.Value <- ValueNone
                        failed.Value <- ValueNone)

                    // Readers move off the declaration nodes before their owner goes. A memo keeps its sources until it
                    // runs again, and a disposed node its last value, so the memos that read the nodes run again now:
                    // otherwise one nothing reads (driven, the builder) would keep the declaration state alive.
                    flush ()

                    untrack (fun () ->
                        driven.TryValue |> ignore
                        published.TryValue |> ignore
                        signature.TryValue |> ignore
                        project.Builder.TryValue |> ignore)

                    if not (isNull (box owner.Value)) then
                        owner.Value.Dispose()
                        owner.Value <- Unchecked.defaultof<_>

            member _.Promoted version =
                if file.Version <> version then
                    ValueNone
                elif not wanted.Peek then
                    ValueSome FileLevel
                else
                    match stateNow (), decl.Peek with
                    | ValueSome(Single v), _ -> if v = version then ValueSome FileLevel else ValueNone
                    | ValueSome BuildFailed, _ -> ValueSome FileLevel
                    | ValueSome Built, ValueSome g ->
                        match g.Current.TryValue, published.TryValue with
                        | Ready f, Ready(ByDeclaration(_, p)) when f.Version = version && Nodes.same p f -> ValueSome Declarations
                        | Ready f, Ready Whole when f.Version = version -> ValueSome FileLevel
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
                    [ WeakReference c; WeakReference c.EnvBefore; WeakReference f.Start.Start ]
                | _ -> []

            member _.Activate() = activate () }

    /// A signature file, file `index` of the project. Its diagnostics are its parse's: CheckSignatureFile reports none.
    let signature
        (project: ProjectShared)
        (index: int)
        (file: FileText)
        (deps: Memo<FileDepsValue>)
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
                (Setup.key index deps signatureOf false)
                (fun key ->
                    project.Measure("checkSignatureFile", fun () -> DeclarationChecker.CheckSignatureFile(ctx, key.Before, key.Parsed.Parse)))
                (fun _ -> None)

        // Cut off by fingerprint, reading the plain result memo, never the async check directly (see Nodes)
        let signature =
            createMemoWithComparer FileSig.Comparer (fun _ -> FileSig.Of(snd result.Value))

        { new IFileNode with
            member _.Path = file.Path
            member _.Mode = FileLevel
            member _.Text = file
            member _.Signature = signature
            member _.Settled version = settledAt file result version |> ValueOption.map (fun (key, _) -> key.Parsed.Parse.Diagnostics)
            member _.Results _ = raise (wholeOnly file.Path)
            member _.FileResult _ = ValueNone
            member _.SegmentRows() = 0
            member _.Promote() = ()
            member _.Demote() = ()
            member _.Promoted version = if file.Version = version then ValueSome FileLevel else ValueNone
            member _.MayBuild = false
            member _.DeclarationObjects() = []
            member _.Activate() = createEffect (fun () -> result.TryValue |> ignore) }
