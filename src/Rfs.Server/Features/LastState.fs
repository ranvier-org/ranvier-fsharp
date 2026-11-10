namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Answers from the last state for references and definition: an answer given on a settled state, found by the place
/// the cursor had in that state's text, with every location in it mapped to the latest texts (PositionMap.forward).
/// The mark is the server's status-bar progress, so nothing goes in the answer, and an answer is given only while the
/// progress has begun.
/// </summary>
/// <remarks>
/// <para>
/// What an answer keeps is plain data: the answer's ranges and, for each graph its turns read, the graph's generation
/// (ProjectGraph.Generation, a restart) and the text version of every file the answer is of, the graph held weakly.
/// Within one generation a file's result is of its text and the texts above it (readSettled), so equal versions and an
/// equal generation mean no other result was published since. No result, parse or graph is kept alive.
/// </para>
/// <para>
/// When it is given, the files are read in one turn (ProjectGraph.FilesNow): a file whose text changed since must
/// still publish the result of the version the answer read, so no newer state settled since. A location in a row
/// edited, new or not found, or in a file whose mapping refuses, refuses the whole answer: no partial answers. Uses
/// newly typed in a pending text may be missing, which is what the progress says.
/// </para>
/// </remarks>
module LastState =
    /// One graph a held answer read: its generation then, and the files the answer is of, at their versions
    [<NoEquality; NoComparison>]
    type GraphAt =
        { Graph: WeakReference<ProjectGraph>
          Generation: int64
          Files: struct (string * int64)[] }

    /// A settled answer and the graphs it is of: the asking graph first
    /// `Projects`: the loaded projects in the answer's turn, held weakly
    [<NoEquality; NoComparison>]
    type Entry<'T> =
        { Graphs: GraphAt[]
          Projects: WeakReference<ProjectGraph>[]
          Answer: 'T }

    let projectsOf (graphs: ProjectGraph list) = graphs |> List.map (fun g -> WeakReference<ProjectGraph> g) |> Array.ofList

    /// The asking file, its version, and the token asked at: line, end column, line text and long name
    type Key = (struct (string * int64 * int * int * string * string))

    let keyOf (path: string) (version: int64) (line: int) (lineStr: string, names: string list, endColumn: int) : Key =
        struct (Path.GetFullPath(path).ToUpperInvariant(), version, line, endColumn, lineStr, String.Join("\n", names))

    let private samePath (a: string) (b: string) =
        String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

    /// The graphs of an answer read in several turns, one entry per graph, the first part's graph first; None when two
    /// turns disagree on a graph's generation or a file's version
    let merge (parts: (ProjectGraph * int64 * struct (string * int64)[]) list) : GraphAt[] option =
        let graphs = List<ProjectGraph * int64 * Dictionary<string, int64>>()
        let mutable agree = true

        for g, generation, files in parts do
            let entry =
                match graphs |> Seq.tryFind (fun (o, _, _) -> obj.ReferenceEquals(o, g)) with
                | Some((_, gen, _) as e) ->
                    if gen <> generation then agree <- false
                    e
                | None ->
                    let e = (g, generation, Dictionary<string, int64>(StringComparer.OrdinalIgnoreCase))
                    graphs.Add e
                    e

            let _, _, d = entry

            for struct (path, version) in files do
                let full = Path.GetFullPath path

                match d.TryGetValue full with
                | true, v when v <> version -> agree <- false
                | _ -> d[full] <- version

        if agree then
            Some
                [| for g, generation, d in graphs ->
                       { Graph = WeakReference<ProjectGraph> g
                         Generation = generation
                         Files = [| for KeyValue(p, v) in d -> struct (p, v) |] } |]
        else
            None

    /// The settled answers by the place they were asked at, the least recently used dropped first
    [<Sealed>]
    type Cache<'T>(capacity: int) =
        let entries = Dictionary<Key, LinkedListNode<struct (Key * Entry<'T>)>>()
        let order = LinkedList<struct (Key * Entry<'T>)>()
        let gate = obj ()
        let mutable answered = 0L
        // How many held entries name each file
        let named = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)

        let note (entry: Entry<'T>) (by: int) =
            for g in entry.Graphs do
                for struct (f, _) in g.Files do
                    let full = Path.GetFullPath f

                    match named.TryGetValue full, by with
                    | (true, n), _ when n + by <= 0 -> named.Remove full |> ignore
                    | (true, n), _ -> named[full] <- n + by
                    | _, b when b > 0 -> named[full] <- b
                    | _ -> ()

        new() = Cache<'T>(256)

        /// Whether a held answer is of the file, so a change of it may be answered from
        member _.Names(path: string) = lock gate (fun () -> named.ContainsKey(Path.GetFullPath path))

        /// How many answers were given from the last state
        member _.Answered = Interlocked.Read &answered

        member internal _.CountAnswered() = Interlocked.Increment &answered |> ignore

        member _.Count = lock gate (fun () -> entries.Count)

        member _.Add(key: Key, entry: Entry<'T>) =
            lock gate (fun () ->
                match entries.TryGetValue key with
                | true, node ->
                    let struct (_, old) = node.Value
                    note old -1
                    order.Remove node
                | _ -> ()

                entries[key] <- order.AddFirst(struct (key, entry))
                note entry 1

                while entries.Count > capacity do
                    let struct (k, old) = order.Last.Value
                    note old -1
                    order.RemoveLast()
                    entries.Remove k |> ignore)

        /// The answer asked at the key on `graph`
        member _.Find(graph: ProjectGraph, key: Key) : Entry<'T> voption =
            lock gate (fun () ->
                match entries.TryGetValue key with
                | true, node ->
                    let struct (_, entry) = node.Value

                    match entry.Graphs[0].Graph.TryGetTarget() with
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
        /// Nothing is pending, or the progress hasn't begun: the settled answer is the one to give
        | Settled

    /// Maps a range of a file's old text to its latest one, or refuses
    type Remap = range -> Result<range, Refusal>

    /// The forward maps made, by old shape and new shape, kept while both shapes live
    let private forwards = ConditionalWeakTable<FileShape, ConditionalWeakTable<FileShape, ForwardMap>>()

    /// Made once per pair; a build cancelled by `ct` stores nothing
    let private forwardOf (old: FileShape) (current: FileShape) (ct: CancellationToken) : ForwardMap =
        forwards.GetValue(old, fun _ -> ConditionalWeakTable()).GetValue(current, fun c -> PositionMap.forward old c ct)

    /// The old and new shapes of a changed file, made or waited for until `ct`
    let private shapesOf (shapes: Hover.LastAnswers) (p: PublishedState) (view: TextView) (ct: CancellationToken) =
        task {
            let! old = shapes.ShapeAsync(p.Text, p.Parse, not p.Whole, ct)

            if old.Rows.Length = 0 then
                return old, PositionMap.textOnly view.Text
            else
                let! current = shapes.ShapeAsync(view.Text, view.Parse, true, ct)
                return old, current
        }

    /// <summary>
    /// Makes the shape of the file's latest parse, rows, directives and lexer states, once the parse lands: so an answer
    /// from the last state doesn't lex on its way. For a client that shows the progress, and only for a file something
    /// held could be answered from (the server decides).
    /// </summary>
    let internal prepare (shapes: Hover.LastAnswers) (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task =
        task {
            try
                let! latest = graph.LatestText(path, [||], (fun () -> false), ct)
                let! _ = shapes.ShapeAsync(latest.View.Text, latest.View.Parse, true, ct)
                ()
            with _ ->
                ()
        }

    /// A file to append each answer's phases to, from RFS_LAST_STATE_TRACE, for measuring
    let private traceFile = Environment.GetEnvironmentVariable "RFS_LAST_STATE_TRACE"
    let private traceGate = obj ()

    let private trace (path: string) (line: int) (phases: (string * float) list) (outcome: string) =
        if not (isNull traceFile) then
            let text = phases |> List.map (fun (n, ms) -> $"%s{n}=%.1f{ms}") |> String.concat " "

            let at = DateTime.UtcNow.ToString "o"
            let file = Path.GetFileName path
            lock traceGate (fun () -> File.AppendAllText(traceFile, $"%s{at} %s{file}:%d{line + 1} %s{outcome} %s{text}\n"))

    /// Whether the loaded projects now are the ones the answer's turn had
    let private sameProjects (held: WeakReference<ProjectGraph>[]) (now: ProjectGraph list) =
        held.Length = now.Length
        && held
           |> Array.forall (fun w ->
               match w.TryGetTarget() with
               | true, g -> now |> List.exists (fun n -> obj.ReferenceEquals(n, g))
               | _ -> false)

    /// <summary>
    /// The answer asked at the place the cursor had in the state it was given on, with its ranges mapped by `remap`, or
    /// why there is none. `busy` (a reload under way) and `pending` (the progress has begun) are read in the first turn,
    /// before anything is lexed, and again in the turn that reads the files, with `projects`, the loaded projects.
    /// </summary>
    let internal answer
        (cache: Cache<'T>)
        (shapes: Hover.LastAnswers)
        (busy: unit -> bool)
        (pending: unit -> bool)
        (projects: unit -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (remap: Remap -> 'T -> Result<'T, Refusal>)
        (ct: CancellationToken)
        : Task<Outcome<'T>> =
        let clock = Diagnostics.Stopwatch.StartNew()
        let phases = List<string * float>()
        let mark (name: string) = phases.Add(name, clock.Elapsed.TotalMilliseconds)

        let traced (o: Outcome<'T>) =
            trace path line (List.ofSeq phases) (match o with Outcome.Answered _ -> "answered" | Outcome.Settled -> "settled" | Outcome.Refused r -> $"%A{r}")
            o

        task {
            let! latest = graph.LatestText(path, [||], busy, pending, ct)
            mark "first-turn"

            if latest.Busy then
                return traced (Outcome.Refused Refusal.Reloading)
            elif not latest.Pending then
                return traced Outcome.Settled
            else
                let find (shape: FileShape) (version: int64) (l: int) (c: int) =
                    Hover.lookupIn shape l c |> Option.bind (fun lookup -> cache.Find(graph, keyOf path version l lookup) |> ValueOption.toOption)

                let! currentShape = shapes.ShapeAsync(latest.View.Text, latest.View.Parse, true, ct)

                // The text as the answer had it, or the cursor mapped to the last published result's
                let! found =
                    task {
                        match find currentShape latest.View.Version line character with
                        | Some entry -> return Ok entry
                        | None ->
                            match latest.Published with
                            | ValueNone -> return Error Refusal.NoSnapshot
                            | ValueSome p when p.Version = latest.View.Version -> return Error Refusal.NotAnswered
                            | ValueSome p ->
                                let! old, current = shapesOf shapes p latest.View ct

                                match PositionMap.map old current line character with
                                | Error refusal -> return Error refusal
                                | Ok at ->
                                    match find old p.Version at.OldLine at.OldCharacter with
                                    | Some entry -> return Ok entry
                                    | None -> return Error Refusal.NotAnswered
                    }

                mark "cursor"

                match found with
                | Error refusal -> return traced (Outcome.Refused refusal)
                | Ok entry ->
                    let graphs =
                        entry.Graphs
                        |> Array.map (fun g ->
                            match g.Graph.TryGetTarget() with
                            | true, target -> ValueSome(struct (target, g.Files))
                            | _ -> ValueNone)

                    if graphs |> Array.exists ValueOption.isNone then
                        return traced (Outcome.Refused Refusal.Moved)
                    else
                        let! now = graph.FilesNow(graphs |> Array.map _.Value, busy, pending, projects, ct)
                        ct.ThrowIfCancellationRequested()
                        mark "files-turn"

                        // Every graph at its generation, every file at the answer's version or pending on it
                        let stale =
                            Array.zip entry.Graphs now.Graphs
                            |> Array.tryPick (fun (g, struct (generation, files)) ->
                                if generation <> g.Generation then
                                    Some Refusal.Moved
                                else
                                    Array.zip g.Files files
                                    |> Array.tryPick (fun (struct (_, version), n) ->
                                        match n with
                                        | FileNow.Same -> None
                                        | FileNow.Changed(_, ValueSome p) when p.Version = version -> None
                                        | FileNow.SignatureChanged -> Some Refusal.Edited
                                        | _ -> Some Refusal.Moved))

                        // The asking file as the first turn read it
                        let askingNow =
                            let struct (_, files) = now.Graphs[0]

                            Array.zip entry.Graphs[0].Files files
                            |> Array.tryPick (fun (struct (f, version), n) ->
                                if samePath f path then
                                    match n with
                                    | FileNow.Same -> Some version
                                    | FileNow.Changed(view, _) -> Some view.Version
                                    | _ -> None
                                else
                                    None)

                        if now.Busy then
                            return traced (Outcome.Refused Refusal.Reloading)
                        elif not now.Pending then
                            return traced Outcome.Settled
                        elif not (sameProjects entry.Projects now.Projects) then
                            // A project loaded, reloaded or gone since: its uses may be missing
                            return traced (Outcome.Refused Refusal.Moved)
                        elif askingNow <> Some latest.View.Version then
                            return traced (Outcome.Refused Refusal.Moved)
                        else
                            match stale with
                            | Some refusal -> return traced (Outcome.Refused refusal)
                            | None ->
                                let byPath = Dictionary<string, List<FileNow>>(StringComparer.OrdinalIgnoreCase)

                                Array.zip entry.Graphs now.Graphs
                                |> Array.iter (fun (g, struct (_, files)) ->
                                    Array.zip g.Files files
                                    |> Array.iter (fun (struct (f, _), n) ->
                                        let full = Path.GetFullPath f

                                        match byPath.TryGetValue full with
                                        | true, l -> l.Add n
                                        | _ -> byPath[full] <- List [ n ]))

                                // The changed files the answer has a range in, each one's map made before mapping
                                let needed = HashSet<string>(StringComparer.OrdinalIgnoreCase)

                                remap
                                    (fun r ->
                                        needed.Add(Path.GetFullPath r.FileName) |> ignore
                                        Ok r)
                                    entry.Answer
                                |> ignore

                                let maps = Dictionary<string, ForwardMap>(StringComparer.OrdinalIgnoreCase)

                                for full in needed do
                                    match byPath.TryGetValue full with
                                    | true, matches when matches.Count = 1 ->
                                        match matches[0] with
                                        | FileNow.Changed(view, ValueSome p) ->
                                            let! old, current = shapesOf shapes p view ct
                                            maps[full] <- forwardOf old current ct
                                        | _ -> ()
                                    | _ -> ()

                                let remapIn (r: range) : Result<range, Refusal> =
                                    ct.ThrowIfCancellationRequested()
                                    let full = Path.GetFullPath r.FileName

                                    match byPath.TryGetValue full with
                                    | false, _ -> Error Refusal.Moved
                                    | true, matches ->
                                        if matches |> Seq.forall (function FileNow.Same -> true | _ -> false) then
                                            Ok r
                                        else
                                            match maps.TryGetValue full with
                                            | true, map ->
                                                match map.At (r.StartLine - 1) r.StartColumn, map.At (r.EndLine - 1) r.EndColumn with
                                                // Both ends in one row, moved by its shift
                                                | Ok(struct (s, rowS)), Ok(struct (e, rowE)) when rowS = rowE && s - (r.StartLine - 1) = e - (r.EndLine - 1) ->
                                                    Ok(Range.mkRange r.FileName (Position.mkPos (s + 1) r.StartColumn) (Position.mkPos (e + 1) r.EndColumn))
                                                | Error refusal, _
                                                | _, Error refusal -> Error refusal
                                                | _ -> Error Refusal.Edited
                                            | _ -> Error Refusal.Moved

                                let mapped = remap remapIn entry.Answer
                                mark "mapped"

                                match mapped with
                                | Error refusal -> return traced (Outcome.Refused refusal)
                                | Ok mapped ->
                                    cache.CountAnswered()
                                    return traced (Outcome.Answered mapped)
        }

    /// <summary>
    /// The settled answer, or one from the last state while the settled one hasn't completed: both are started, and a
    /// last-state answer that comes first cancels the settled one. Every refusal waits for the settled answer, as without
    /// it.
    /// </summary>
    /// <remarks>
    /// While something is pending but the progress hasn't begun (Settled), the last-state answer is asked again once
    /// `whenShown` completes: the begin follows the pending state by the progress's hold. Settled with the mark shown
    /// ends the trying: nothing is pending then.
    /// </remarks>
    let internal race
        (whenShown: unit -> Task)
        (settled: CancellationToken -> Task<'T>)
        (last: CancellationToken -> Task<Outcome<'T>>)
        (ct: CancellationToken)
        : Task<'T> =
        let last (ct: CancellationToken) =
            task {
                let mutable found = ValueNone

                while found.IsNone do
                    let shown = whenShown ()

                    match! last ct with
                    // Not shown when asked: ask again once it is
                    | Outcome.Settled when not shown.IsCompleted -> do! shown.WaitAsync ct
                    // Shown and nothing pending: the settled answer is the one
                    | o -> found <- ValueSome o

                return found.Value
            }

        task {
            use settledCts = CancellationTokenSource.CreateLinkedTokenSource ct
            use lastCts = CancellationTokenSource.CreateLinkedTokenSource ct

            try
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
                    return! settledTask
            finally
                // A settled answer, or its failure, stops the last-state work
                lastCts.Cancel()
        }
