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

/// <summary>
/// A project checked on a Ranvier graph, re-checking only what an edit can change.
/// </summary>
/// <remarks>
/// <para>
/// A graph checks every file whole (FileLevel, as ProjectGraph.Create makes it) or one declaration at a time
/// (Declarations).
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
/// augmentations that extend them (R11), are checked as a segment, in order. Cross-file granularity is per file (R16):
/// a file is set up again from every earlier file, and re-checks whole, when the signature fingerprint of an earlier
/// file changes.
/// </para>
/// </remarks>
[<Sealed>]
type ProjectGraph private (host: GraphHost, project: ProjectShared, spec: ProjectSpec, mode: Rfs.Checker.FileMode, files: IFileNode[]) =
    let requested = Array.zeroCreate<int64> files.Length

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

    /// Declarations mode: reads each file up to `last` once every one of them is settled on its latest text and on
    /// the signatures of the files before it.
    let settledChain (last: int) (read: IFileNode -> string list * int64 -> 'T voption) : 'T[] voption =
        let results = ResizeArray()
        let mutable before = []
        let mutable ok = true
        let mutable i = 0

        while ok && i <= last do
            let file = files[i]

            match read file (before, Volatile.Read &requested[i]) with
            | ValueSome r ->
                results.Add r
                before <- file.Signature.Value.Chain
            | ValueNone -> ok <- false

            i <- i + 1

        if ok then ValueSome(results.ToArray()) else ValueNone

    /// File-level: reads file `i` once it is settled on its latest text, with every write posted to it and to the
    /// files before it applied. Its result being settled means the files it was set up from are settled too.
    let settledFile (i: int) (read: IFileNode -> string list * int64 -> 'T voption) : 'T voption =
        let mutable applied = true

        for j in 0..i do
            if files[j].Text.Version <> Volatile.Read &requested[j] then
                applied <- false

        if applied then read files[i] ([], requested[i]) else ValueNone

    /// Reads file `i` once it, and the files it was set up from, are settled on their latest texts.
    let settledOne (i: int) (read: IFileNode -> string list * int64 -> 'T voption) : 'T voption =
        match mode with
        | FileLevel -> settledFile i read
        | Declarations -> settledChain i read |> ValueOption.map (fun all -> all[i])

    /// Reads every file once each is settled on its latest text.
    let settledAll (read: IFileNode -> string list * int64 -> 'T voption) : 'T[] voption =
        match mode with
        | Declarations -> settledChain (files.Length - 1) read
        | FileLevel ->
            let results = Array.zeroCreate files.Length
            let mutable ok = true
            let mutable i = 0

            while ok && i < files.Length do
                match settledFile i read with
                | ValueSome r -> results[i] <- r
                | ValueNone -> ok <- false

                i <- i + 1

            if ok then ValueSome results else ValueNone

    /// A graph whose every file starts file-level.
    static member Create(host: GraphHost, spec: ProjectSpec) : Task<ProjectGraph> =
        ProjectGraph.CreateWithMode(host, spec, FileLevel)

    /// A graph whose every file is checked in `mode`.
    static member internal CreateWithMode(host: GraphHost, spec: ProjectSpec, mode: Rfs.Checker.FileMode) : Task<ProjectGraph> =
        task {
            let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.StartAsTask
            let project = ProjectShared ctx
            let texts = spec.SourceFiles |> List.map (fun f -> f, File.ReadAllText f) |> Array.ofList

            let! files =
                host.Read(fun () ->
                    let isSignature (path: string) =
                        path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

                    let fileTexts =
                        texts
                        |> Array.map (fun (path, text) ->
                            FileText(ctx, path, text, mode = Declarations && not (isSignature path)))

                    let nodes = ResizeArray<IFileNode>()

                    match mode with
                    | FileLevel ->
                        let deps = FileDeps.create ctx fileTexts
                        let signatureOf (j: int) = nodes[j].Signature

                        fileTexts
                        |> Array.iteri (fun i file ->
                            nodes.Add(
                                if isSignature file.Path then
                                    WholeFile.signature project i file deps signatureOf
                                else
                                    WholeFile.impl project i file deps signatureOf
                            ))

                        // The dependencies read every file's parse, so effects start once every file's nodes exist
                        for node in nodes do
                            node.Activate()
                    | Declarations ->
                        let mutable before = None

                        for file in fileTexts do
                            let node =
                                if isSignature file.Path then
                                    FileNodes.signature project file before
                                else
                                    let signatureFile =
                                        fileTexts
                                        |> Array.tryFind (fun f -> String.Equals(f.Path, file.Path + "i", StringComparison.OrdinalIgnoreCase))
                                        |> Option.map _.Path

                                    FileNodes.impl project file before signatureFile

                            nodes.Add node
                            before <- Some node.Signature

                    nodes.ToArray())

            return ProjectGraph(host, project, spec, mode, files)
        }

    /// The first and last line (1-based) of each declaration `text` splits into as the file's text. Call it when the
    /// graph is idle.
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
        host.Post(fun () -> node.Text.SetText(text, version))
        version

    /// The file's diagnostics, with the version of its text they are for, once its result is for its latest text and
    /// the files it depends on are settled.
    member _.SettledDiagnostics(file: string) : Task<struct (int64 * FSharpDiagnostic[])> =
        let i = indexOf file

        host.WhenSettled(fun () ->
            settledOne i (fun f (before, version) -> f.Settled(before, version) |> ValueOption.map (fun d -> struct (version, d))))

    /// The file's diagnostics once its result is for its latest text and the files it depends on are settled.
    member _.Diagnostics(file: string) : Task<FSharpDiagnostic[]> =
        let i = indexOf file
        host.WhenSettled(fun () -> settledOne i (fun f s -> f.Settled s))

    /// Every file's diagnostics once all of them are settled on their latest texts.
    member _.AllDiagnostics() : Task<IReadOnlyDictionary<string, FSharpDiagnostic[]>> =
        host.WhenSettled(fun () ->
            settledAll (fun f s -> f.Settled s)
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

        host.WhenSettled(fun () ->
            match mode with
            | FileLevel -> settledFile i (fun f s -> f.Results s)
            | Declarations ->
                settledChain i (fun f s ->
                    if obj.ReferenceEquals(f, files[i]) then
                        f.Results s |> ValueOption.map Some
                    else
                        f.Settled s |> ValueOption.map (fun _ -> None))
                |> ValueOption.map (fun all -> all[i].Value))

    /// How the file is checked now.
    member _.Mode(file: string) : Task<Rfs.Checker.FileMode> =
        let node = files[indexOf file]
        host.Read(fun () -> node.Mode)

    /// Every implementation file's FileResult (FinishFile's, or CheckFile's for a file checked file-level) once all
    /// files are settled on their latest texts.
    member _.AllFileResults() : Task<IReadOnlyDictionary<string, FileResult>> =
        host.WhenSettled(fun () ->
            settledAll (fun f s ->
                if isSignatureFile f then
                    f.Settled s |> ValueOption.map (fun _ -> None)
                else
                    f.FileResult s |> ValueOption.map Some)
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

    /// How many times CheckDeclaration has returned, including runs whose result was never published: a flight
    /// superseded under CancelPrevious that finished before it saw its cancellation.
    member _.CheckedCount: int64 = project.CheckedCount

    /// How many CheckDeclaration results were published (adopted): each once, when a finish of the current text that
    /// holds it is published.
    member _.AdoptedChecks: int64 = project.AdoptedCount

    /// Stopwatch ticks spent inside DeclarationChecker.FinishFile, summed over every call.
    member _.FinishTicks: int64 = project.FinishTicks

    /// Stopwatch ticks of each part of FinishFile (FileResult.PhaseTicks), summed over every call.
    member _.FinishPhaseTicks: IReadOnlyDictionary<string, int64> = project.FinishPhaseTicks

    /// How many published results of declarations checked on their own had a final fingerprint other than the one
    /// their check left (the D1 premise broke). Such a declaration is then checked as a segment leader.
    member _.PremiseBreaks: int64 = project.PremiseBreaks

    /// Each declaration whose final fingerprint broke the D1 premise, with its file, when it is published.
    member _.PremiseBroken: IObservable<DeclarationKey * string> = project.PremiseBroken

    /// Each CheckDeclaration that returned, with its file, on the thread that ran it.
    member _.Ran: IObservable<DeclarationKey * string> = project.Ran

    /// Each newly published declaration result, with its file, on the graph thread.
    member _.Checked: IObservable<DeclarationKey * string> = project.Checked

    /// The project's checker context. For tests.
    member internal _.Context: ProjectContext = project.Context

    /// Runs `hook` off the graph thread, with the file's path, just before a setup from a file's dependencies is given
    /// to the checker. For tests.
    member internal _.OnScopedCheck(hook: string -> unit) : unit = project.ScopedCheckHook <- hook
