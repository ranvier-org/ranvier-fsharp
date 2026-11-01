namespace Rfs.Checker

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Ranvier
open Rfs.Projects

/// <summary>
/// A project checked one declaration at a time on a Ranvier graph, re-checking only what an edit can change.
/// </summary>
/// <remarks>
/// <para>
/// Per implementation file: a text signal, a parse, a keyed projection of the declarations, and per declaration its
/// syntax, content, dependencies and check; then a finish over the current results, whose diagnostics are the ones
/// published, and the fingerprints and signature later files read. A declaration re-checks when its syntax or place
/// changes, when the fingerprint its check left changes for an earlier declaration it depends on (by name, through
/// TypedUses, or all of them for an unresolvable name), or when an earlier declaration it depends on with no cutoff
/// re-checks (its module header, an open or another declaration that affects all later ones). Declarations whose
/// results later checks change in place (R12), and types with the augmentations that extend them (R11), are checked
/// as a segment, in order.
/// </para>
/// <para>
/// Cross-file granularity is per file (R16): a file is set up again, and re-checks whole, when the signature
/// fingerprint of an earlier file changes.
/// </para>
/// </remarks>
[<Sealed>]
type ProjectGraph private (host: GraphHost, project: ProjectShared, files: IFileNode[]) =
    let requested = Array.zeroCreate<int64> files.Length

    let byPath =
        let d = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
        files |> Array.iteri (fun i f -> d[Path.GetFullPath f.Path] <- i)
        d

    let indexOf (file: string) =
        match byPath.TryGetValue(Path.GetFullPath file) with
        | true, i -> i
        | _ -> invalidArg (nameof file) $"%s{file} is not a source file of the project"

    /// Reads each file up to `last` once every one of them is settled on its latest text and on the signatures of
    /// the files before it.
    let settled (last: int) (read: IFileNode -> string list * int64 -> 'T voption) : 'T[] voption =
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

    static member Create(host: GraphHost, spec: ProjectSpec) : Task<ProjectGraph> =
        task {
            let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.StartAsTask
            let project = ProjectShared ctx
            let texts = spec.SourceFiles |> List.map (fun f -> f, File.ReadAllText f)

            let! files =
                host.Read(fun () ->
                    let nodes = ResizeArray<IFileNode>()
                    let indexes = ResizeArray<string * Memo<FileIndex>>()
                    let mutable before = None

                    for path, text in texts do
                        let node =
                            if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                                FileNodes.signature project path text before
                            else
                                let earlier = List.ofSeq indexes

                                let earlierIndex =
                                    createMemoWithComparer HashIdentity.Structural (fun _ ->
                                        NameIndex.ofProject [ for file, index in earlier -> file, index.Value ])

                                let signatureFile =
                                    texts
                                    |> List.tryFind (fun (f, _) -> String.Equals(f, path + "i", StringComparison.OrdinalIgnoreCase))
                                    |> Option.map fst

                                let node, index = FileNodes.impl project path text before signatureFile earlierIndex
                                indexes.Add((path, index))
                                node

                        nodes.Add node
                        before <- Some node.Signature

                    nodes.ToArray())

            return ProjectGraph(host, project, files)
        }

    /// Sets a file's text; checks follow on the graph thread.
    member _.SetText(file: string, text: string) : unit =
        let i = indexOf file
        let version = Interlocked.Increment &requested[i]
        let node = files[i]
        host.Post(fun () -> node.SetText(text, version))

    /// The file's diagnostics once its results, and those of the files before it, are for their latest texts.
    member _.Diagnostics(file: string) : Task<FSharpDiagnostic[]> =
        let i = indexOf file
        host.WhenSettled(fun () -> settled i (fun f s -> f.Settled s) |> ValueOption.map (fun all -> all[i]))

    /// Every file's diagnostics once all of them are settled on their latest texts.
    member _.AllDiagnostics() : Task<IReadOnlyDictionary<string, FSharpDiagnostic[]>> =
        host.WhenSettled(fun () ->
            settled (files.Length - 1) (fun f s -> f.Settled s)
            |> ValueOption.map (fun all ->
                let d = Dictionary<string, FSharpDiagnostic[]>()

                for f, diagnostics in Array.zip files all do
                    d[f.Path] <- diagnostics

                d :> IReadOnlyDictionary<_, _>))

    /// Each declaration's current result in an implementation file, once settled as for `Diagnostics`.
    member _.Results(file: string) : Task<IReadOnlyDictionary<DeclarationKey, DeclarationResult>> =
        let i = indexOf file

        if files[i].Path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
            invalidArg (nameof file) $"%s{file} is a signature file"

        host.WhenSettled(fun () ->
            settled i (fun f s ->
                if obj.ReferenceEquals(f, files[i]) then
                    f.Results s |> ValueOption.map Some
                else
                    f.Settled s |> ValueOption.map (fun _ -> None))
            |> ValueOption.map (fun all -> all[i].Value))

    /// How many times CheckDeclaration has run.
    member _.CheckedCount: int64 = project.CheckedCount

    /// Each newly published declaration result, with its file, on the graph thread.
    member _.Checked: IObservable<DeclarationKey * string> = project.Checked
