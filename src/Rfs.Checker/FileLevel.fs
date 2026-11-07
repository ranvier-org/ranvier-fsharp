namespace Rfs.Checker

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier

/// How far promoting an implementation file has got for its current setup.
type internal Build =
    /// Not wanted
    | Idle
    /// Its parse of this version splits into one declaration (every module rec and namespace rec file): never promoted
    | Single of version: int64
    /// Wanted, and not yet built by declaration for its current setup
    | Building
    /// Built by declaration for its current setup
    | Built

/// Which result of an implementation file is the published one.
[<NoEquality; NoComparison>]
type internal Published =
    /// CheckFile's
    | Whole
    /// The finish of its current declaration results
    | ByDeclaration of DeclFile * Finished
    /// Demoted: the last finish, kept until the file's text or setup changes
    | Kept of Finished

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
    /// is built again while it is still wanted. A file whose split gives one declaration is never built, nor is the
    /// result of a build whose setup from dependencies reports an unresolved name (C3/R17) ever published.
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
        let kept = createSignal (ValueNone: Finished voption)

        let stateOf (g: DeclFile) =
            let p = g.Split.Value

            if p.Declarations.Length <= 1 then
                Single p.Version
            else
                let setup = g.Setup.Value

                match promoted.Value with
                | ValueSome known when Nodes.same known setup -> Built
                | _ -> Building

        let build =
            createMemo (fun _ ->
                if not wanted.Value then
                    Idle
                else
                    match decl.Value with
                    | ValueNone -> Idle
                    | ValueSome g -> stateOf g)

        // Read again where it is needed rather than through `build`: a memo pending on `build`, which other nodes read
        // with TryValue, was seen not to be woken when it settled (a Ranvier lost wake-up)
        let built (g: DeclFile) = stateOf g = Built

        // Whether the declaration nodes run: while built, or while this file is the project's builder
        let driven =
            createMemo (fun _ ->
                match build.TryValue with
                | Ready Built -> true
                | Ready Building -> project.Builder.Value = ValueSome index
                | _ -> false)

        let published =
            createMemo (fun _ ->
                if wanted.Value then
                    match decl.Value with
                    | ValueSome g when built g ->
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
                    | ValueSome f when file.Text.Value.Version = f.Version && Setup.sameFiles scopedKey.Value f.Start.Setup.Key -> Kept f
                    | _ -> Whole)

        // Cut off by fingerprint, reading plain memos, never an async check directly (see Nodes). CheckFile and
        // FinishFile give one text and setup the same fingerprint, so a promotion changes nothing after the file, unless
        // the signature holds compiler-generated names: a declaration's are numbered from the declaration, so the files
        // after it are then checked again.
        let signature =
            createMemoWithComparer FileSig.Comparer (fun _ ->
                match published.Value with
                | ByDeclaration(_, f)
                | Kept f -> FileSig.Of f.FileResult.Signature
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

            // Promotes the file once its build has finished for its current text and setup
            createEffect (fun () ->
                if driven.Value then
                    match build.Value, decl.Value with
                    | Building, ValueSome g ->
                        let f = g.Current.Value

                        if f.Version = file.Text.Value.Version then
                            untrack (fun () -> promoted.Value <- ValueSome f.Start.Setup)
                    | _ -> ())

        { new IFileNode with
            member _.Path = path

            member _.Mode =
                match published.TryValue with
                | Ready(ByDeclaration _) -> Declarations
                | _ -> FileLevel

            member _.Text = file
            member _.Signature = signature

            member _.Settled version =
                match current version with
                | ValueSome(ByDeclaration(_, f))
                | ValueSome(Kept f) -> finishedAt version f |> ValueOption.map _.FileResult.Diagnostics
                | ValueSome Whole -> settledAt file whole version |> ValueOption.map (fun (_, r) -> r.Diagnostics)
                | ValueNone -> ValueNone

            member _.Results version =
                match current version with
                | ValueSome(ByDeclaration(_, f)) -> finishedAt version f |> ValueOption.map DeclFile.ResultsOf
                | ValueSome _ -> raise (wholeOnly path)
                | ValueNone -> ValueNone

            member _.FileResult version =
                match current version with
                | ValueSome(ByDeclaration(_, f))
                | ValueSome(Kept f) -> finishedAt version f |> ValueOption.map _.FileResult
                | ValueSome Whole -> settledAt file whole version |> ValueOption.map snd
                | ValueNone -> ValueNone

            member _.SegmentRows() =
                match decl.Peek with
                | ValueSome g -> g.SegmentRows()
                | ValueNone -> 0

            member _.Promote() =
                if not wanted.Peek then
                    let g =
                        createRoot (fun o ->
                            owner.Value <- o
                            FileNodes.impl project file index deps scopedKey driven)

                    batch (fun () ->
                        kept.Value <- ValueNone
                        decl.Value <- ValueSome g
                        wanted.Value <- true)

            member _.Demote() =
                if wanted.Peek then
                    // The finish published now stays published until the file's text or setup changes
                    let last =
                        match untrack (fun () -> published.TryValue) with
                        | Ready(ByDeclaration(_, f)) -> ValueSome f
                        | _ -> ValueNone

                    batch (fun () ->
                        kept.Value <- last
                        wanted.Value <- false
                        decl.Value <- ValueNone
                        promoted.Value <- ValueNone)

                    // Readers move off the declaration nodes before their owner goes
                    flush ()
                    owner.Value.Dispose()
                    owner.Value <- Unchecked.defaultof<_>

            member _.Promoted version =
                if file.Version <> version then
                    false
                elif not wanted.Peek then
                    true
                else
                    match build.TryValue with
                    | Ready(Single v) -> v = version
                    | Ready Built ->
                        match decl.Peek with
                        | ValueSome g ->
                            match g.Current.TryValue with
                            | Ready f -> f.Version = version
                            | Failed e -> raise (InvalidOperationException($"%s{path} failed to check", e))
                            | _ -> false
                        | ValueNone -> false
                    | Failed e -> raise (InvalidOperationException($"%s{path} failed to check", e))
                    | _ -> false

            member _.Building =
                match build.TryValue with
                | Ready Building -> true
                | _ -> false

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
            member _.Promoted version = file.Version = version
            member _.Building = false
            member _.Activate() = createEffect (fun () -> result.TryValue |> ignore) }
