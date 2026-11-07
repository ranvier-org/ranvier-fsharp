namespace Rfs.Checker

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier

/// Files checked whole (FileLevel): an implementation file by CheckFile, a signature file by CheckSignatureFile, each
/// set up from the signatures of the files it depends on.
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

    let private node
        (file: FileText)
        (result: Memo<SetupKey * 'R>)
        (signature: Memo<FileSig>)
        (diagnostics: SetupKey * 'R -> FSharpDiagnostic[])
        (fileResult: 'R -> FileResult voption)
        : IFileNode =
        { new IFileNode with
            member _.Path = file.Path
            member _.Mode = FileLevel
            member _.Text = file
            member _.Signature = signature
            member _.Settled(_, version) = settledAt file result version |> ValueOption.map diagnostics

            member _.Results(_, _) =
                raise (InvalidOperationException $"%s{file.Path} is checked file-level: it has no declaration results")

            member _.FileResult(_, version) = settledAt file result version |> ValueOption.bind (snd >> fileResult)
            member _.SegmentRows() = 0

            // Keeps the file checked, so files that do not depend on each other check in parallel
            member _.Activate() = createEffect (fun () -> result.TryValue |> ignore) }

    /// An implementation file, file `index` of the project. Its result is CheckFile's: published only while it is for
    /// the current text, so a check of a superseded text is never published.
    let impl
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
                (fun key ->
                    let r = DeclarationChecker.CheckFile(ctx, key.Before, key.Parsed.Parse)
                    project.CountFileLevelCheck()
                    r)
                (fun r -> Some r.Diagnostics)

        let signature =
            createMemoWithComparer FileSig.Comparer (fun _ -> FileSig.Of (snd result.Value).Signature)

        node file result signature (fun (_, r) -> r.Diagnostics) ValueSome

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
                (fun key -> DeclarationChecker.CheckSignatureFile(ctx, key.Before, key.Parsed.Parse))
                (fun _ -> None)

        let signature =
            createMemoWithComparer FileSig.Comparer (fun _ -> FileSig.Of(snd result.Value))

        node file result signature (fun (key, _) -> key.Parsed.Parse.Diagnostics) (fun _ -> ValueNone)
