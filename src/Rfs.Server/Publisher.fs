namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open Ranvier
open Rfs.Checker
open Rfs.Lsp
open Ionide.LanguageServerProtocol.Types
open Rfs.Projects

/// The body of `rfs/declarationsChecked`
type DeclarationsChecked = { count: int }

/// FSAC's `fsharp/documentAnalyzed` payload (`LspHelpers.fs`)
type DocumentAnalyzedNotification =
    { TextDocument: VersionedTextDocumentIdentifier }

/// An open document, or a closed file whose diagnostics in other files are still to be brought up to date. Graph
/// thread.
[<NoComparison; ReferenceEquality>]
type private Row =
    { Key: string
      Path: string
      Uri: string
      LspVersion: int
      /// The graphs that own the file, in load order; the first one's result is published
      Owners: ProjectGraph list
      Open: bool
      /// A new epoch asks for one list sent even if it equals the last one: an open, an edit, a load
      Epoch: int64 }

/// What a row's file found, by the file each diagnostic is in. Graph thread.
[<NoComparison; ReferenceEquality>]
type private Found =
    { Groups: IReadOnlyDictionary<string, Rfs.Lsp.Diagnostic list>
      /// From a check settled for the row as it is now; false while an earlier one is kept
      Current: bool
      /// A check for the row as it is now is under way: not settled, and not failed
      Waiting: bool }

    static member None: Found =
        { Groups = Dictionary<string, Rfs.Lsp.Diagnostic list>() :> IReadOnlyDictionary<_, _>
          Current = false
          Waiting = false }

[<Struct; CustomEquality; NoComparison>]
type private AnalyzerFlags =
    { UnusedOpens: bool
      SimplifyNames: bool
      UnusedDeclarations: bool }

    member this.Any = this.UnusedOpens || this.SimplifyNames || this.UnusedDeclarations

    override this.Equals(other: obj) =
        match other with
        | :? AnalyzerFlags as o ->
            this.UnusedOpens = o.UnusedOpens
            && this.SimplifyNames = o.SimplifyNames
            && this.UnusedDeclarations = o.UnusedDeclarations
        | _ -> false

    override this.GetHashCode() =
        HashCode.Combine(this.UnusedOpens, this.SimplifyNames, this.UnusedDeclarations)

    static member Off =
        { UnusedOpens = false
          SimplifyNames = false
          UnusedDeclarations = false }

module internal GraphTurn =
    /// Runs `work` on the graph thread once the current pump has returned, after every effect of its flush: a settle
    /// step, which the host runs after the pump. Graph thread. A Post from the graph thread runs at once, inside the
    /// flush. Registered during a pump, the step is done in that pump's settle steps; registered during them (by work
    /// another step did), in the next pump's. Either way it is made apart, so no fault of the host ends it unrun.
    let afterPump (host: GraphHost) (work: unit -> unit) : unit =
        host.WhenSettled(
            (fun () ->
                try
                    work ()
                with ex ->
                    eprintfn "[ranvierautocomplete] work after a pump failed: %O" ex

                ValueSome()),
            CancellationToken.None,
            true
        )
        |> ignore

/// <summary>
/// Document sync and the diagnostics the editor sees: one row per open document, whose effect reads the file's settled
/// diagnostics and queues the send, under the document's own URI and LSP version, on an ordered outbox.
/// </summary>
/// <remarks>
/// <para>
/// Open, change and close run on the graph thread, in the order they are called, so a handler that calls them before
/// its first suspension keeps the editor's order. A row's effect runs again when anything its settled read read
/// changes: a new text, a check, a dependency, a guard that failed. Settled means for the graph's latest text of the
/// file, which is the document's: the document and its text on the graphs change in one graph-thread item.
/// </para>
/// <para>
/// Each epoch's first settled list is sent even when it equals the last one, and followed by
/// <c>rfs/declarationsChecked</c>; later ones only when they differ. Closing sends the empty list, and nothing is sent
/// for the file after it until it is opened again. A file's list is the union of what each checked file found in it
/// (after a #line directive, a diagnostic can land in another file): each row's value is what its file's check found,
/// by target, and a row's effect reads every row's value. A row's value keeps its last settled check while the next is
/// pending (a fold's "last settled value for a pending row"), so a list may mix one file's older findings with another's
/// newer ones until both settle, as before rows; a row whose file has no owner finds nothing. A closed file keeps its
/// row while it reports anything to an open document, so what it reports follows its checks; then the row goes, and
/// with it what it reported. After a load, a closed row follows its owners, or goes with them.
/// </para>
/// <para>
/// A check or graph that fails, for a row's file, publishes nothing new for it and is logged once. What it reported to
/// other files goes from their lists (its own last list stays): a fault leaves no finding that will never be replaced.
/// It touches no other row: no row waits on the shared host, so nothing a fault aborts there reaches it.
/// </para>
/// </remarks>
type Publisher(host: GraphHost, workspace: Workspace, client: RanvierClient, documents: Documents, ?getConfig: unit -> FSharpConfig) =
    let readConfig = defaultArg getConfig (fun () -> FSharpConfig.Default)
    let outbox = new Outbox()
    let mutable disposed = 0
    // Graph thread
    let lastKeys = Dictionary<string, struct (Rfs.Lsp.Range * int * string * string) list>(StringComparer.OrdinalIgnoreCase)
    let lastAdopted = Dictionary<ProjectGraph, int64>(HashIdentity.Reference)
    let mutable rows: Signal<Map<string, Row>> = Unchecked.defaultof<_>
    let mutable analyzerFlags: Signal<AnalyzerFlags> = Unchecked.defaultof<_>
    let mutable epoch = 0L
    let faulted = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    let mutable onClose: string -> unit = ignore
    let mutable diskPasses = 0
    let nothing = Found.None

    let flagsOf (cfg: FSharpConfig) : AnalyzerFlags =
        { UnusedOpens = cfg.UnusedOpensAnalyzer
          SimplifyNames = cfg.SimplifyNameAnalyzer
          UnusedDeclarations = cfg.UnusedDeclarationsAnalyzer }

    let log (text: string) = eprintfn "[ranvierautocomplete] %s" text

    let keyOf (d: Rfs.Lsp.Diagnostic) = struct (d.Range, d.Severity, d.Code, d.Message)

    let tagsOfCode (code: string) : DiagnosticTag[] option =
        match code with
        | "FS1182"
        | "1182"
        | "FSAC0001"
        | "FSAC0002"
        | "FSAC0003" -> Some [| DiagnosticTag.Unnecessary |]
        | "FS0044"
        | "0044"
        | "44" -> Some [| DiagnosticTag.Deprecated |]
        | _ -> None

    let toLsp (d: Rfs.Lsp.Diagnostic) : Diagnostic =
        let position (p: Rfs.Lsp.Position) : Position =
            { Line = uint32 p.Line; Character = uint32 p.Character }

        { Range = { Start = position d.Range.Start; End = position d.Range.End }
          Severity = Some(enum<DiagnosticSeverity> d.Severity)
          Code = Some(U2.C2 d.Code)
          CodeDescription = None
          Source = Some d.Source
          Message = d.Message
          Tags = tagsOfCode d.Code
          RelatedInformation = None
          Data = None }

    let isSame (a: string) (b: string) = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let rowKey (path: string) = path.ToUpperInvariant()

    let nextEpoch () =
        epoch <- epoch + 1L
        epoch

    let rowOf (path: string) = rows.Value |> Map.tryFind (rowKey path)
    let setRow (r: Row) = rows.Value <- rows.Value |> Map.add r.Key r
    let dropRow (path: string) = rows.Value <- rows.Value |> Map.remove (rowKey path)

    /// Queues the list when it is `force`d or differs from the last one queued for the file. `own` is a file's own list
    /// at its LSP version. Graph thread.
    let send (path: string) (uri: string) (version: int option) (diagnostics: Rfs.Lsp.Diagnostic list) (force: bool) (own: bool) : unit =
        let keys = diagnostics |> List.map keyOf

        let differs =
            match lastKeys.TryGetValue path with
            | true, last -> last <> keys
            | _ -> true

        if force || differs then
            lastKeys[path] <- keys

            let parameters =
                { Uri = uri
                  Version = version
                  Diagnostics = diagnostics |> List.map toLsp |> Array.ofList }

            outbox.Enqueue(
                Publish(path, (if own then version else None), (fun () -> client.TextDocumentPublishDiagnostics parameters |> Async.StartAsTask :> Task))
            )

    /// `rfs/declarationsChecked`: the declaration checks adopted since the previous one, over all projects. Graph thread.
    let countChecks () : int =
        let mutable n = 0L
        let live = workspace.Projects

        // A reloaded project's old graph is not kept
        for g in lastAdopted.Keys |> Seq.toArray do
            if not (live |> List.exists (fun l -> obj.ReferenceEquals(l, g))) then
                lastAdopted.Remove g |> ignore

        for g in live do
            let now = g.AdoptedChecks

            n <-
                n
                + now
                - (match lastAdopted.TryGetValue g with
                   | true, before -> before
                   | _ -> 0L)

            lastAdopted[g] <- now

        int n

    /// Queues `rfs/declarationsChecked`, counted once the pump that settled the publish before it has returned: adoption
    /// is an effect too (FileLevel's `Publish`), and a flush runs its effects in an order of its own. The checks that
    /// promote a file are adopted too, so the notification after a promotion counts them with whatever edit it follows.
    /// Telling them apart in the count would lose the checks of a restart, which an edit does cause. Graph thread.
    let notifyChecks () : unit =
        GraphTurn.afterPump host (fun () ->
            let body = box { count = countChecks () }
            outbox.Enqueue(Notify(fun () -> client.Notify("rfs/declarationsChecked", body) |> Async.StartAsTask :> Task)))

    /// What the check of `source` found, by the file each diagnostic is in. Diagnostics with no file of the project,
    /// rangeless ones included, belong to the checked file.
    let groupsOf (source: string) (settled: FSharpDiagnostic[]) : IReadOnlyDictionary<string, Rfs.Lsp.Diagnostic list> =
        let target (d: Rfs.Lsp.Diagnostic) =
            if d.File <> "" && not (isSame d.File source) && not (workspace.OwnersOf d.File).IsEmpty then
                d.File
            else
                source

        let groups = Dictionary<string, Rfs.Lsp.Diagnostic list>(StringComparer.OrdinalIgnoreCase)

        for d in settled |> Array.map FcsDiagnostics.ofFcs do
            let t = target d
            groups[t] <- (match groups.TryGetValue t with | true, l -> l | _ -> []) @ [ d ]

        groups

    let mutable projection: Projection<string, Found> = Unchecked.defaultof<_>

    /// What every row's file found in `t`, in row order, with the row of `t` itself read from `own`. A tracked read of
    /// every row. Graph thread.
    let union (t: string) (own: Found) =
        let key = rowKey t

        [ for k in projection.Keys do
              let found = if k = key then own else projection.Get k

              match found.Groups.TryGetValue t with
              | true, l -> yield! l
              | _ -> () ]

    /// Whether a row's file reports anything to an open document other than itself. Graph thread.
    let reportsToOpen (path: string) (found: Found) =
        found.Groups
        |> Seq.exists (fun (KeyValue(t, l)) -> not l.IsEmpty && not (isSame t path) && (documents.TryFindNow t).IsSome)

    /// A row's value: what its file's check found, kept from the last settled check while the next one is pending, as
    /// the same object, so a pending row's re-runs change nothing downstream. A failed check keeps only the file's own
    /// group: what it reported to other files could never be replaced.
    let finding (item: unit -> Row) : unit -> Found =
        let mutable last = nothing
        let mutable failing = false
        // The owner's fault count at the last settle: a fault since, with the check still pending, may have stranded it
        let mutable faultsAt = struct (Unchecked.defaultof<ProjectGraph>, 0)

        let fail (path: string) (why: string) =
            if not failing then
                failing <- true
                faulted.Add path |> ignore
                log $"%s{path}: no diagnostics to publish, %s{why}"
                let own = Dictionary<string, Rfs.Lsp.Diagnostic list>(StringComparer.OrdinalIgnoreCase)

                match last.Groups.TryGetValue path with
                | true, l -> own[path] <- l
                | _ -> ()

                last <- { Groups = own; Current = false; Waiting = false }

        fun () ->
            let r = item ()

            // The owners now, tracked: a reload that drops the file, or replaces its graph, changes them in the item
            // that removes the old graph, so no row reads a replaced graph's findings (N-1)
            match workspace.OwnersNow r.Path with
            | [] ->
                failing <- false
                last <- nothing
                last
            | g :: _ ->
                let settled =
                    try
                        g.TrySettledDiagnostics r.Path
                    with
                    | NotReadyException _ -> ValueNone
                    | ex ->
                        fail r.Path $"its check failed: %O{ex}"
                        ValueNone

                let faults = g.FaultsSeenNow()

                match settled with
                | ValueSome(struct (_, diagnostics)) ->
                    failing <- false
                    faultsAt <- struct (g, faults)
                    last <- { Groups = groupsOf r.Path diagnostics; Current = true; Waiting = false }
                    last
                | ValueNone ->
                    let struct (at, before) = faultsAt

                    // Pending after a fault of its project or a referenced project: what it reported elsewhere may never be replaced
                    if obj.ReferenceEquals(at, g) && faults > before then
                        fail r.Path "its project or a referenced project failed on the graph thread while its check was pending"

                    if not failing && (last.Current || not last.Waiting) then
                        last <- { last with Current = false; Waiting = true }

                    last

    let post (work: unit -> unit) =
        if Volatile.Read &disposed = 0 then
            try
                host.Post work
            with :? ObjectDisposedException ->
                ()

    /// Emits `fsharp/fileParsed` once per open-row epoch as soon as the latest parse lands, without waiting on typecheck. Graph thread.
    let publishingParsed (item: unit -> Row) : unit =
        let retryTick: Signal<int64> = createSignal 0L
        let mutable parsedSent = Int64.MinValue
        let mutable inFlight: struct (ProjectGraph * int64) voption = ValueNone

        createEffect (fun () ->
            let r = item ()
            let _ = retryTick.Value
            let owners = workspace.OwnersNow r.Path

            try
                if r.Open && r.Uri <> "" && r.Epoch <> parsedSent then
                    match owners with
                    | [] -> ()
                    | g :: _ ->
                        let alreadyInFlightOnSameOwner =
                            match inFlight with
                            | ValueSome(struct (fg, fe)) -> obj.ReferenceEquals(fg, g) && fe = r.Epoch
                            | ValueNone -> false

                        if not alreadyInFlightOnSameOwner then
                            let path = r.Path
                            let uri = r.Uri
                            let targetEpoch = r.Epoch
                            inFlight <- ValueSome(struct (g, targetEpoch))

                            task {
                                try
                                    let! _ = g.LatestParse(path, CancellationToken.None)

                                    if Volatile.Read &disposed = 0 then
                                        post (fun () ->
                                            if Volatile.Read &disposed = 0 then
                                                match inFlight with
                                                | ValueSome(struct (fg, fe)) when obj.ReferenceEquals(fg, g) && fe = targetEpoch ->
                                                    inFlight <- ValueNone
                                                | _ -> ()

                                                match rowOf path with
                                                | Some now when now.Open && now.Epoch = targetEpoch && parsedSent <> targetEpoch ->
                                                    parsedSent <- targetEpoch
                                                    let body = box { Content = uri }
                                                    outbox.Enqueue(Notify(fun () -> client.Notify("fsharp/fileParsed", body) |> Async.StartAsTask :> Task))
                                                | _ -> ())
                                with _ ->
                                    if Volatile.Read &disposed = 0 then
                                        post (fun () ->
                                            if Volatile.Read &disposed = 0 then
                                                match inFlight with
                                                | ValueSome(struct (fg, fe)) when obj.ReferenceEquals(fg, g) && fe = targetEpoch ->
                                                    inFlight <- ValueNone
                                                | _ -> ()

                                                if parsedSent <> targetEpoch then
                                                    retryTick.Value <- retryTick.Value + 1L)
                            }
                            |> ignore
            with ex ->
                log $"publishing fileParsed %s{r.Path} failed: %O{ex}")

    /// Runs optional built-in open-file analyzers (`FSAC0001`, `FSAC0002`, `FSAC0003`) when enabled in `FSharpConfig`,
    /// extracting plain `Rfs.Lsp.Diagnostic` values immediately without retaining any compiler or view objects. Graph thread.
    let analyzing
        (item: unit -> Row)
        (own: Memo<Found>)
        (analyzed: Signal<struct (int64 * AnalyzerFlags * Rfs.Lsp.Diagnostic list)>)
        : unit =
        let mutable inFlight: struct (int64 * AnalyzerFlags) voption = ValueNone

        createEffect (fun () ->
            let r = item ()
            let flags = analyzerFlags.Value
            let found = own.Value
            let owners = workspace.OwnersNow r.Path

            try
                if not r.Open || not flags.Any || r.Path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                    inFlight <- ValueNone
                    let struct (_, _, prevList) = analyzed.Value

                    if not prevList.IsEmpty then
                        analyzed.Value <- struct (Int64.MinValue, AnalyzerFlags.Off, [])
                elif found.Current && r.Uri <> "" then
                    match owners with
                    | [] -> ()
                    | g :: _ ->
                        let struct (doneEpoch, doneFlags, _) = analyzed.Value

                        let alreadyDone = doneEpoch = r.Epoch && doneFlags = flags

                        let alreadyInFlight =
                            match inFlight with
                            | ValueSome(struct (fe, ff)) -> fe = r.Epoch && ff = flags
                            | ValueNone -> false

                        if not alreadyDone && not alreadyInFlight then
                            let path = r.Path
                            let targetEpoch = r.Epoch
                            let targetFlags = flags
                            inFlight <- ValueSome(struct (targetEpoch, targetFlags))

                            Task.Run(fun () ->
                                (task {
                                    try
                                        let! plainDiags =
                                            g.ReadFile(
                                                path,
                                                (fun view ->
                                                    let lines = view.Text.Split '\n'
                                                    let lastLine = max 1 lines.Length

                                                    let lastCol =
                                                        if lines.Length = 0 then
                                                            0
                                                        else
                                                            lines[lines.Length - 1].TrimEnd('\r').Length

                                                    lines, lastLine, lastCol),
                                                (fun view (lines, lastLine, lastCol) ->
                                                    StackGuard.onLargeStack (fun () ->
                                                        let check =
                                                            if view.Whole then
                                                                view.Result.CheckResultsNotKept
                                                            else
                                                                view.CheckResultsAt(lastLine, lastCol)

                                                        let getLineStr (line1Based: int) =
                                                            if line1Based >= 1 && line1Based <= lines.Length then
                                                                lines[line1Based - 1].TrimEnd '\r'
                                                            else
                                                                ""

                                                        let out = ResizeArray<Rfs.Lsp.Diagnostic>()

                                                        let mkDiag (code: string) (msg: string) (rng: FSharp.Compiler.Text.range) : Rfs.Lsp.Diagnostic =
                                                            { File = path
                                                              Range =
                                                                { Start =
                                                                    { Line = max 0 (rng.StartLine - 1)
                                                                      Character = max 0 rng.StartColumn }
                                                                  End =
                                                                    { Line = max 0 (rng.EndLine - 1)
                                                                      Character = max 0 rng.EndColumn } }
                                                              Severity = 4
                                                              Code = code
                                                              Message = msg
                                                              Source = "F# Analyzers" }

                                                        if targetFlags.UnusedOpens then
                                                            let unusedOpens =
                                                                UnusedOpens.getUnusedOpens (check, getLineStr)
                                                                |> Async.RunSynchronously

                                                            for rng in unusedOpens do
                                                                out.Add(mkDiag "FSAC0001" "Unused open statement" rng)

                                                        if targetFlags.SimplifyNames then
                                                            let simplifiable =
                                                                SimplifyNames.getSimplifiableNames (check, getLineStr)
                                                                |> Async.RunSynchronously

                                                            for sr in simplifiable do
                                                                out.Add(mkDiag "FSAC0002" "This qualifier is redundant" sr.Range)

                                                        if targetFlags.UnusedDeclarations then
                                                            let isScript =
                                                                path.EndsWith(".fsx", StringComparison.OrdinalIgnoreCase)
                                                                || path.EndsWith(".fsscript", StringComparison.OrdinalIgnoreCase)

                                                            let unusedDecls =
                                                                UnusedDeclarations.getUnusedDeclarations (check, isScript)
                                                                |> Async.RunSynchronously

                                                            for rng in unusedDecls do
                                                                out.Add(mkDiag "FSAC0003" "This value is unused" rng)

                                                        out |> Seq.toList)),
                                                CancellationToken.None
                                            )

                                        if Volatile.Read &disposed = 0 then
                                            post (fun () ->
                                                if Volatile.Read &disposed = 0 then
                                                    match inFlight with
                                                    | ValueSome(struct (fe, ff)) when fe = targetEpoch && ff = targetFlags ->
                                                        inFlight <- ValueNone
                                                    | _ -> ()

                                                    match rowOf path with
                                                    | Some now when now.Open && now.Epoch = targetEpoch && analyzerFlags.Value = targetFlags ->
                                                        analyzed.Value <- struct (targetEpoch, targetFlags, plainDiags)
                                                    | _ -> ())
                                    with ex ->
                                        match ex with
                                        | :? OperationCanceledException
                                        | :? ObjectDisposedException -> ()
                                        | _ -> log $"analyzing read %s{path} failed: %O{ex}"

                                        if Volatile.Read &disposed = 0 then
                                            post (fun () ->
                                                match inFlight with
                                                | ValueSome(struct (fe, ff)) when fe = targetEpoch && ff = targetFlags ->
                                                    inFlight <- ValueNone
                                                | _ -> ())
                                })
                                :> Task)
                            |> ignore
            with ex ->
                log $"analyzing %s{r.Path} failed: %O{ex}")

    /// The effect of one row, over the row's own value. Graph thread.
    let publishing
        (item: unit -> Row)
        (own: Memo<Found>)
        (analyzed: Signal<struct (int64 * AnalyzerFlags * Rfs.Lsp.Diagnostic list)>)
        : unit =
        let mutable sent = Int64.MinValue

        createEffect (fun () ->
            let r = item ()
            let found = own.Value
            let flags = analyzerFlags.Value

            try
                // The row is the document: close changes it in the item that queues the empty list
                if r.Open then
                    if found.Current then
                        let extraAnalyzers =
                            if not flags.Any || r.Path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                                []
                            else
                                let struct (ae, af, adiags) = analyzed.Value
                                if ae = r.Epoch && af = flags then adiags else []

                        let fresh = r.Epoch <> sent
                        sent <- r.Epoch
                        send r.Path r.Uri (Some r.LspVersion) ((union r.Path found) @ extraAnalyzers) fresh true

                        if fresh then
                            notifyChecks ()
                            let analyzedBody = box { TextDocument = { Uri = r.Uri; Version = r.LspVersion } }
                            GraphTurn.afterPump host (fun () ->
                                outbox.Enqueue(Notify(fun () -> client.Notify("fsharp/documentAnalyzed", analyzedBody) |> Async.StartAsTask :> Task)))
                elif found.Current && not (reportsToOpen r.Path found) then
                    // A closed file reports nothing to an open one: the row goes, after this pump, and with it what it
                    // reported, as for a file never opened
                    GraphTurn.afterPump host (fun () ->
                        match rowOf r.Path with
                        | Some now when obj.ReferenceEquals(now, r) -> dropRow r.Path
                        | _ -> ())
            with ex ->
                log $"publishing %s{r.Path} failed: %O{ex}")

    do
        host.Post(fun () ->
            createRoot (fun _ ->
                rows <- createSignal Map.empty
                analyzerFlags <- createSignal (flagsOf (readConfig ()))

                projection <-
                    createProjectionWith
                        (fun (r: Row) -> r.Key)
                        (fun item ->
                            let read = finding item
                            let own = createMemo (fun _ -> read ())
                            let analyzed = createSignal (struct (Int64.MinValue, AnalyzerFlags.Off, ([] : Rfs.Lsp.Diagnostic list)))
                            publishingParsed item
                            analyzing item own analyzed
                            publishing item own analyzed
                            fun () -> own.Value)
                        (fun () -> rows.Value |> Map.values :> seq<_>)

                // Keeps the projection's passes running: a key's arrival makes its row, its removal disposes it
                createEffect (fun () -> projection.Keys |> ignore))
            |> ignore)

    /// An empty list for a document that no project owns (any more); a list that is empty already is not sent again.
    /// Graph thread.
    let publishEmpty (doc: OpenDocument) (force: bool) : unit =
        if (documents.TryFind doc.Path).IsSome then
            send doc.Path doc.Uri (Some doc.LspVersion) [] force false

    let openRow (doc: OpenDocument) (owners: ProjectGraph list) =
        setRow
            { Key = rowKey doc.Path
              Path = doc.Path
              Uri = doc.Uri
              LspVersion = doc.LspVersion
              Owners = owners
              Open = true
              Epoch = nextEpoch () }

    let closedRow (path: string) (uri: string) (owners: ProjectGraph list) =
        setRow
            { Key = rowKey path
              Path = path
              Uri = uri
              LspVersion = 0
              Owners = owners
              Open = false
              Epoch = nextEpoch () }

    let openNow (uri: string) (path: string) (version: int) (text: string) =
        let document =
            { Uri = uri
              Path = path
              LspVersion = version
              Text = text }

        documents.Open document

        match workspace.Open(path, text, version) with
        | [] ->
            publishEmpty document true
            openRow document []
        | owners ->
            (fst owners.Head).Promote path
            openRow document (owners |> List.map fst)

    let changeNow (path: string) (version: int) (change: string -> string) =
        match documents.Update(path, (fun d -> { d with LspVersion = version; Text = change d.Text })) with
        | Some d -> openRow d (workspace.Open(path, d.Text, version) |> List.map fst)
        | None -> ()

    let closeNow (path: string) (diskText: string option) =
        match documents.Close path with
        | None -> ()
        | Some d ->
            let owners = workspace.Close(path, diskText)

            for g in owners do
                g.Demote path

            send path d.Uri None [] true false
            lastKeys.Remove path |> ignore

            match owners with
            | [] -> dropRow path
            | _ -> closedRow path d.Uri owners

    let diskChangedNow (paths: string list) =
        diskPasses <- diskPasses + 1

        for path in paths do
            match (documents.TryFind path).IsNone, workspace.OwnersOf path with
            | true, (_ :: _ as owners) -> closedRow path "" owners
            | _ -> ()

    let sameOwners (a: ProjectGraph list) (b: ProjectGraph list) =
        a.Length = b.Length && List.forall2 (fun x y -> obj.ReferenceEquals(x, y)) a b

    let loadedNow (graph: ProjectGraph) =
        for doc in documents.All do
            match workspace.OwnersOf doc.Path with
            | owners when owners |> List.exists (fun g -> obj.ReferenceEquals(g, graph)) ->
                owners.Head.Promote doc.Path
                openRow doc owners
            | _ -> ()

    /// After a load or reload: a document whose project is gone gets the empty list; what a file no project owns any
    /// more reported to other files goes; a closed file's row follows its owners now, or goes with them
    let sweepNow () =
        let live = workspace.Projects

        for g in lastAdopted.Keys |> Seq.toArray do
            if not (live |> List.exists (fun l -> obj.ReferenceEquals(l, g))) then
                lastAdopted.Remove g |> ignore

        for r in rows.Value |> Map.values |> Seq.toArray do
            if not r.Open then
                match workspace.OwnersOf r.Path with
                | [] -> dropRow r.Path
                | owners when not (sameOwners owners r.Owners) ->
                    setRow { r with Owners = owners; Epoch = nextEpoch () }
                | _ -> ()

        for doc in documents.All do
            if (workspace.OwnersOf doc.Path).IsEmpty then
                publishEmpty doc false

                match rowOf doc.Path with
                | Some r when not r.Owners.IsEmpty -> setRow { r with Owners = [] }
                | _ -> ()

    do workspace.OnLoadEnd(fun () -> post sweepNow)

    /// The editor opened the document: the file's text is the document's, and the project that owns it checks it
    /// by declaration. A document no project owns gets one empty list.
    member _.Open(uri: string, path: string, version: int, text: string) : unit =
        post (fun () -> openNow uri path version text)

    /// The editor changed the document: `change` makes the new text from the old
    member _.Change(path: string, version: int, change: string -> string) : unit =
        post (fun () -> changeNow path version change)

    /// The editor closed the document: the file on disk is its text again, it is demoted, and its diagnostics cleared.
    /// The disk is read here, off the graph thread.
    member _.Close(path: string) : unit =
        // Whether or not a project owns it now: a load may register one before the close runs
        let diskText =
            if File.Exists path then Workspace.ReadOnClose path else None

        onClose path
        post (fun () -> closeNow path diskText)

    /// Files that are not open changed on disk and the graphs have their new texts: the open documents hear what that
    /// changed, in one pass for all of them
    member _.DiskChanged(paths: string list) : unit = post (fun () -> diskChangedNow paths)

    /// A project has loaded: its open documents were given their texts, so they are promoted and published
    member _.Loaded(graph: ProjectGraph) : unit = post (fun () -> loadedNow graph)

    /// Re-evaluates open-file built-in analyzers when `FSharpConfig` analyzer flags change
    member _.NotifyConfigChanged() : unit =
        post (fun () ->
            if not (isNull (box analyzerFlags)) then
                let next = flagsOf (readConfig ())

                if analyzerFlags.Value <> next then
                    analyzerFlags.Value <- next)

    /// Runs `hook` with the LSP version of a file's own list before it is sent, holding the sends queued after it. For
    /// tests.
    member internal _.BeforePublish(hook: int -> Task) : unit = outbox.BeforePublish hook

    /// Queues `diagnostics` for the open document at `path` as a diagnostic found by another file's check would be. For
    /// tests.
    member internal _.PublishForeign(path: string, diagnostics: Rfs.Lsp.Diagnostic list) : Task =
        host.Read(fun () ->
            match documents.TryFind path with
            | Some doc -> send path doc.Uri (Some doc.LspVersion) diagnostics false false
            | None -> ())
        :> Task

    /// Completes when the document sync called so far has run: a request that reads a file's latest text waits for it,
    /// as the edits before it may still be queued. Faults with ObjectDisposedException once the publisher is disposed.
    member _.Barrier() : Task =
        if Volatile.Read &disposed <> 0 then
            Task.FromException(ObjectDisposedException(nameof Publisher))
        else
            host.Read ignore :> Task

    /// Completes when the document sync called so far has run, and the sends it queued are sent. For tests.
    member internal this.Flushed() : Task =
        task {
            do! this.Barrier()
            do! outbox.Flushed()
        }

    /// Parks the graph thread until `gate` is set, so the document sync called after it waits. For tests.
    member internal _.Hold(gate: ManualResetEventSlim) : unit =
        post (fun () -> gate.Wait(TimeSpan.FromSeconds 60.) |> ignore)

    /// How many graphs the publisher keeps state for. For tests.
    member internal _.RetainedGraphs() : Task<int> = host.Read(fun () -> lastAdopted.Count)

    /// Whether the file's own check failed to give diagnostics for its row, which was logged. For tests.
    member internal _.Faulted(path: string) : Task<bool> = host.Read(fun () -> faulted.Contains path)

    /// Runs `hook` with the path in `Close`, after the disk is read and before the close is posted. For tests.
    member internal _.OnClose(hook: string -> unit) : unit = onClose <- hook

    /// How many passes over files changed on disk ran. For tests.
    member internal _.DiskPasses() : Task<int> = host.Read(fun () -> diskPasses)

    /// How many rows and last lists the publisher keeps. For tests.
    member internal _.Retained() : Task<int> =
        host.Read(fun () -> rows.Value.Count + lastKeys.Count)

    /// Graph thread, tracked: whether any row's check is under way, its file's or one it waits for
    member internal _.CheckingNow() : bool =
        projection.Keys |> Seq.exists (fun k -> (projection.Get k).Waiting)

    /// Queues a send behind the lists queued so far
    member internal _.Enqueue(item: Outgoing) : unit = outbox.Enqueue item

    member _.Dispose() =
        Volatile.Write(&disposed, 1)
        outbox.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
