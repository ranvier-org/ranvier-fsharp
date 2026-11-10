namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Answers from the last state for references and definition: an answer given on a settled state, found by the place
/// the cursor had in that state's text, with every location in it mapped to the latest texts (PositionMap.mapForward).
/// The mark is the server's status-bar progress, so nothing goes in the answer, and a client without it never gets one.
/// </summary>
/// <remarks>
/// <para>
/// What an answer keeps is plain data: the answer's ranges and the text version of every file its turn read, each
/// file's graph weakly. No result, parse or graph is kept alive. The texts it is mapped from are those files' last
/// published results, read when it is given (ProjectGraph.FilesNow): a file whose text changed since must still publish
/// the result of the version the answer read, so no newer state settled since. Anything else waits.
/// </para>
/// <para>
/// A location in a row edited, new or not found, or in a file whose mapping refuses, refuses the whole answer: no
/// partial answers. Uses newly typed in a pending text may be missing, which is what the progress says.
/// </para>
/// </remarks>
module LastState =
    /// A file at a text version, its graph held weakly
    [<Struct; NoEquality; NoComparison>]
    type FileAt =
        { Graph: WeakReference<ProjectGraph>
          Path: string
          Version: int64 }

    /// A settled answer, and every file its turn read at its version: the asking file first
    [<NoEquality; NoComparison>]
    type Entry<'T> = { Files: FileAt[]; Answer: 'T }

    /// The asking file, its version, and the token asked at: line, end column, line text and long name
    type Key = (struct (string * int64 * int * int * string * string))

    let keyOf (path: string) (version: int64) (line: int) (lineStr: string, names: string list, endColumn: int) : Key =
        struct (Path.GetFullPath(path).ToUpperInvariant(), version, line, endColumn, lineStr, String.Join("\n", names))

    let fileAt (graph: ProjectGraph) (path: string) (version: int64) : FileAt =
        { Graph = WeakReference<ProjectGraph> graph
          Path = path
          Version = version }

    /// The settled answers by the place they were asked at, the least recently used dropped first
    [<Sealed>]
    type Cache<'T>(capacity: int) =
        let entries = Dictionary<Key, LinkedListNode<struct (Key * Entry<'T>)>>()
        let order = LinkedList<struct (Key * Entry<'T>)>()
        let gate = obj ()
        let mutable answered = 0L

        new() = Cache<'T>(256)

        /// How many answers were given from the last state
        member _.Answered = Interlocked.Read &answered

        member internal _.CountAnswered() = Interlocked.Increment &answered |> ignore

        member _.Count = lock gate (fun () -> entries.Count)

        member _.Add(key: Key, entry: Entry<'T>) =
            lock gate (fun () ->
                match entries.TryGetValue key with
                | true, node -> order.Remove node
                | _ -> ()

                entries[key] <- order.AddFirst(struct (key, entry))

                while entries.Count > capacity do
                    let struct (k, _) = order.Last.Value
                    order.RemoveLast()
                    entries.Remove k |> ignore)

        /// The answer asked at the key on `graph`
        member _.Find(graph: ProjectGraph, key: Key) : Entry<'T> voption =
            lock gate (fun () ->
                match entries.TryGetValue key with
                | true, node ->
                    let struct (_, entry) = node.Value

                    match entry.Files[0].Graph.TryGetTarget() with
                    | true, g when obj.ReferenceEquals(g, graph) ->
                        order.Remove node
                        order.AddFirst node
                        ValueSome entry
                    | _ -> ValueNone
                | _ -> ValueNone)

    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type Outcome<'T> =
        | Answered of 'T
        | Refused of Refusal
        /// Nothing is pending: the settled answer is the one to give
        | Settled

    /// Maps a range of a file's old text to its latest one, or refuses
    type Remap = range -> Result<range, Refusal>

    let private sameFile (a: string) (b: string) =
        String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

    /// <summary>
    /// The answer asked at the place the cursor had in the state it was given on, with its ranges mapped by `remap`, or
    /// why there is none. `busy` (a reload under way) and `pending` (anything the progress shows as pending) are read in
    /// the turn that reads the files.
    /// </summary>
    let internal answer
        (cache: Cache<'T>)
        (shapes: Hover.LastAnswers)
        (busy: unit -> bool)
        (pending: unit -> bool)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (remap: Remap -> 'T -> Result<'T, Refusal>)
        (ct: CancellationToken)
        : Task<Outcome<'T>> =
        task {
            let! latest = graph.LatestText(path, [||], busy, ct)

            if latest.Busy then
                return Outcome.Refused Refusal.Reloading
            else
                let find (text: string) (version: int64) (l: int) (c: int) =
                    Hover.lookupOf text l c |> Option.bind (fun lookup -> cache.Find(graph, keyOf path version l lookup) |> ValueOption.toOption)

                // The text as the answer had it, or the cursor mapped to the last published result's
                let found =
                    match find latest.View.Text latest.View.Version line character with
                    | Some entry -> Ok entry
                    | None ->
                        match latest.Published with
                        | ValueNone -> Error Refusal.NoSnapshot
                        | ValueSome p when p.Version = latest.View.Version -> Error Refusal.NotAnswered
                        | ValueSome p ->
                            let old = shapes.ShapeOf(p.Text, p.Parse, not p.Whole)
                            let current = if old.Rows.Length = 0 then PositionMap.textOnly latest.View.Text else shapes.ShapeOf(latest.View.Text, latest.View.Parse, true)

                            match PositionMap.map old current line character with
                            | Error refusal -> Error refusal
                            | Ok at ->
                                match find p.Text p.Version at.OldLine at.OldCharacter with
                                | Some entry -> Ok entry
                                | None -> Error Refusal.NotAnswered

                match found with
                | Error refusal -> return Outcome.Refused refusal
                | Ok entry ->
                    let graphs =
                        entry.Files
                        |> Array.map (fun f ->
                            match f.Graph.TryGetTarget() with
                            | true, g -> ValueSome(struct (g, f.Path, f.Version))
                            | _ -> ValueNone)

                    if graphs |> Array.exists ValueOption.isNone then
                        return Outcome.Refused Refusal.Moved
                    else
                        let! now = graph.FilesNow(graphs |> Array.map _.Value, busy, pending, ct)

                        let versionOf (f: FileAt) (n: FileNow) =
                            match n with
                            | FileNow.Same -> ValueSome f.Version
                            | FileNow.Changed(view, _) -> ValueSome view.Version
                            | _ -> ValueNone

                        // Every file at the answer's version, or pending on it: nothing published since
                        let stale =
                            Array.zip entry.Files now.Files
                            |> Array.tryPick (fun (f, n) ->
                                match n with
                                | FileNow.Same -> None
                                | FileNow.Changed(_, ValueSome p) when p.Version = f.Version -> None
                                | FileNow.SignatureChanged -> Some Refusal.Edited
                                | _ -> Some Refusal.Moved)

                        if now.Busy then
                            return Outcome.Refused Refusal.Reloading
                        elif not now.Pending then
                            return Outcome.Settled
                        elif versionOf entry.Files[0] now.Files[0] <> ValueSome latest.View.Version then
                            return Outcome.Refused Refusal.Moved
                        else
                            match stale with
                            | Some refusal -> return Outcome.Refused refusal
                            | None ->
                                let remapIn (r: range) : Result<range, Refusal> =
                                    let matching = Array.zip entry.Files now.Files |> Array.filter (fun (f, _) -> sameFile f.Path r.FileName)

                                    match matching with
                                    | [||] -> Error Refusal.Moved
                                    | [| _, FileNow.Same |] -> Ok r
                                    | [| _, FileNow.Changed(view, ValueSome p) |] ->
                                        let old = shapes.ShapeOf(p.Text, p.Parse, not p.Whole)
                                        let current = if old.Rows.Length = 0 then PositionMap.textOnly view.Text else shapes.ShapeOf(view.Text, view.Parse, true)

                                        match PositionMap.mapForward old current (r.StartLine - 1) r.StartColumn,
                                              PositionMap.mapForward old current (r.EndLine - 1) r.EndColumn with
                                        | Ok s, Ok e when s - (r.StartLine - 1) = e - (r.EndLine - 1) ->
                                            Ok(Range.mkRange r.FileName (Position.mkPos (s + 1) r.StartColumn) (Position.mkPos (e + 1) r.EndColumn))
                                        | Error refusal, _
                                        | _, Error refusal -> Error refusal
                                        | _ -> Error Refusal.Edited
                                    | several when several |> Array.forall (fun (_, n) -> match n with FileNow.Same -> true | _ -> false) -> Ok r
                                    | _ -> Error Refusal.Moved

                                match remap remapIn entry.Answer with
                                | Error refusal -> return Outcome.Refused refusal
                                | Ok mapped ->
                                    cache.CountAnswered()
                                    return Outcome.Answered mapped
        }

    /// <summary>
    /// The settled answer, or one from the last state while the settled one hasn't completed: both are started, and a
    /// last-state answer that comes first cancels the settled one. Every refusal waits for the settled answer, as without
    /// it.
    /// </summary>
    let internal race (settled: CancellationToken -> Task<'T>) (last: CancellationToken -> Task<Outcome<'T>>) (ct: CancellationToken) : Task<'T> =
        task {
            use settledCts = CancellationTokenSource.CreateLinkedTokenSource ct
            use lastCts = CancellationTokenSource.CreateLinkedTokenSource ct
            let settledTask = settled settledCts.Token

            let lastTask =
                task {
                    try
                        return! last lastCts.Token
                    with
                    | :? OperationCanceledException -> return Outcome.Refused Refusal.NotAnswered
                    | e ->
                        Diagnostics.Trace.WriteLine $"Rfs.Server: an answer from the last state failed: %O{e}"
                        return Outcome.Refused Refusal.NotAnswered
                }

            let! first = Task.WhenAny(settledTask :> Task, lastTask :> Task)

            if not (obj.ReferenceEquals(first, settledTask)) && not settledTask.IsCompleted then
                match lastTask.Result with
                | Outcome.Answered answer ->
                    settledCts.Cancel()
                    // Observed, so its cancellation isn't left unobserved
                    settledTask.ContinueWith(fun (t: Task<'T>) -> t.Exception |> ignore) |> ignore
                    return answer
                | _ -> return! settledTask
            else
                let! found = settledTask
                lastCts.Cancel()
                return found
        }
