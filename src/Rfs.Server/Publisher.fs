namespace Rfs.Server

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open Ranvier
open Rfs.Checker
open Rfs.Lsp
open Ionide.LanguageServerProtocol.Types
open Rfs.Projects

/// The body of `rfs/declarationsChecked`
type DeclarationsChecked = { count: int }

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
/// (a signature's conformance error lands in the .fsi): a graph-thread dictionary of each source's last applied
/// groups. It keeps a source's last settled groups while that source is pending, as a fold over rows would, so a list
/// may mix a source's older groups with another's newer ones until both settle, as before.
/// </para>
/// <para>
/// A check or graph that fails, for a row's file, publishes nothing new for it and is logged. It touches no other row:
/// no row waits on the shared host, so nothing a fault aborts there reaches it.
/// </para>
/// </remarks>
type Publisher(host: GraphHost, workspace: Workspace, client: RanvierClient, documents: Documents) =
    let outbox = new Outbox()
    let mutable disposed = 0
    // Graph thread, but lastAdopted: the outbox's consumer
    let lastKeys = Dictionary<string, struct (Rfs.Lsp.Range * int * string * string) list>(StringComparer.OrdinalIgnoreCase)
    let lastAdopted = Dictionary<ProjectGraph, int64>(HashIdentity.Reference)
    // What each checked file reported, by the file the diagnostics are in: a file's diagnostics are the union over sources
    let contributions = SortedDictionary<string, Dictionary<string, Rfs.Lsp.Diagnostic list>>(StringComparer.OrdinalIgnoreCase)
    let mutable rows: Signal<Map<string, Row>> = Unchecked.defaultof<_>
    let mutable epoch = 0L

    let log (text: string) = eprintfn "[ranvierautocomplete] %s" text

    let keyOf (d: Rfs.Lsp.Diagnostic) = struct (d.Range, d.Severity, d.Code, d.Message)

    let toLsp (d: Rfs.Lsp.Diagnostic) : Diagnostic =
        let position (p: Rfs.Lsp.Position) : Position =
            { Line = uint32 p.Line; Character = uint32 p.Character }

        { Range = { Start = position d.Range.Start; End = position d.Range.End }
          Severity = Some(enum<DiagnosticSeverity> d.Severity)
          Code = Some(U2.C2 d.Code)
          CodeDescription = None
          Source = Some d.Source
          Message = d.Message
          Tags = None
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

    /// `rfs/declarationsChecked`: the declaration checks adopted since the previous one, over all projects, counted by
    /// the outbox's consumer once the publish before it is sent, as before rows. Outbox consumer only.
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

    /// Queues `rfs/declarationsChecked`. The checks that promote a file are adopted too, so the notification after a
    /// promotion counts them with whatever edit it follows. Telling them apart in the count would lose the checks of a
    /// restart, which an edit does cause. Graph thread.
    let notifyChecks () : unit =
        outbox.Enqueue(
            Notify(fun () -> client.Notify("rfs/declarationsChecked", box { count = countChecks () }) |> Async.StartAsTask :> Task)
        )

    /// A list for a file other than the one checked, at the document's current version. Graph thread.
    let sendForeign (target: string) (doc: OpenDocument) (union: Rfs.Lsp.Diagnostic list) : unit =
        send target doc.Uri (Some doc.LspVersion) union false false

    /// Records what the check of `source` found, and queues the lists of the files it touched that are open: each file's
    /// list is what every checked file found in it, so a diagnostic that lands in another file (a signature's) is cleared
    /// when it goes. Diagnostics with no file of the project, rangeless ones included, belong to the checked document.
    /// The checked file's own list goes to `own`'s URI at its LSP version, always when `forceOwn`; without `own` (the file
    /// is closed) it isn't sent. Graph thread.
    let apply (source: string) (settled: FSharpDiagnostic[]) (own: struct (string * int) voption) (forceOwn: bool) : unit =
        let target (d: Rfs.Lsp.Diagnostic) =
            if d.File <> "" && not (isSame d.File source) && not (workspace.OwnersOf d.File).IsEmpty then
                d.File
            else
                source

        let groups = Dictionary<string, Rfs.Lsp.Diagnostic list>(StringComparer.OrdinalIgnoreCase)

        for d in settled |> Array.map FcsDiagnostics.ofFcs do
            let t = target d
            groups[t] <- (match groups.TryGetValue t with | true, l -> l | _ -> []) @ [ d ]

        let touched = HashSet<string>(groups.Keys, StringComparer.OrdinalIgnoreCase)

        match contributions.TryGetValue source with
        | true, previous -> touched.UnionWith previous.Keys
        | _ -> ()

        touched.Add source |> ignore
        contributions[source] <- groups

        let union (t: string) =
            [ for KeyValue(_, byTarget) in contributions do
                  match byTarget.TryGetValue t with
                  | true, l -> yield! l
                  | _ -> () ]

        for t in touched do
            if isSame t source then
                match own with
                | ValueSome(struct (uri, version)) -> send t uri (Some version) (union t) forceOwn true
                | ValueNone -> ()
            else
                match documents.TryFind t with
                | Some doc -> sendForeign t doc (union t)
                | None -> ()

    /// The effect of one row. Graph thread.
    let publishing (item: unit -> Row) : unit =
        let mutable sent = Int64.MinValue

        createEffect (fun () ->
            let r = item ()

            match r.Owners with
            | [] -> ()
            | g :: _ ->
                let settled =
                    try
                        g.TrySettledDiagnostics r.Path
                    with
                    | NotReadyException _ -> reraise ()
                    | ex ->
                        log $"%s{r.Path}: no diagnostics to publish, its check failed: %O{ex}"
                        ValueNone

                match settled with
                | ValueNone -> ()
                | ValueSome(struct (_, diagnostics)) ->
                    try
                        // The row is the document: close changes it in the item that queues the empty list
                        if r.Open then
                            let fresh = r.Epoch <> sent
                            sent <- r.Epoch
                            apply r.Path diagnostics (ValueSome(struct (r.Uri, r.LspVersion))) fresh

                            if fresh then
                                notifyChecks ()
                        else
                            // The file's text is the disk's again: what it reports to the open files is brought up to
                            // date once, and the row goes
                            apply r.Path diagnostics ValueNone false

                            host.Post(fun () ->
                                match rowOf r.Path with
                                | Some now when obj.ReferenceEquals(now, r) -> dropRow r.Path
                                | _ -> ())
                    with ex ->
                        log $"publishing %s{r.Path} failed: %O{ex}")

    do
        host.Post(fun () ->
            createRoot (fun _ ->
                rows <- createSignal Map.empty

                let projection =
                    createProjectionWith
                        (fun (r: Row) -> r.Key)
                        (fun item ->
                            publishing item
                            ignore)
                        (fun () -> rows.Value |> Map.values :> seq<_>)

                // Keeps the projection's passes running: a key's arrival makes its row, its removal disposes it
                createEffect (fun () -> projection.Keys |> ignore))
            |> ignore)

    /// An empty list for a document that no project owns (any more); a list that is empty already is not sent again.
    /// Graph thread.
    let publishEmpty (doc: OpenDocument) (force: bool) : unit =
        if (documents.TryFind doc.Path).IsSome then
            contributions.Remove doc.Path |> ignore
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

    let diskChangedNow (path: string) =
        match (documents.TryFind path).IsNone, workspace.OwnersOf path with
        | true, (_ :: _ as owners) -> closedRow path "" owners
        | _ -> ()

    let loadedNow (graph: ProjectGraph) =
        for doc in documents.All do
            match workspace.OwnersOf doc.Path with
            | owners when owners |> List.exists (fun g -> obj.ReferenceEquals(g, graph)) ->
                owners.Head.Promote doc.Path
                openRow doc owners
            | _ -> ()

    /// After a load or reload: a document whose project is gone gets the empty list
    let sweepNow () =
        for doc in documents.All do
            if (workspace.OwnersOf doc.Path).IsEmpty then
                publishEmpty doc false

                match rowOf doc.Path with
                | Some r when not r.Owners.IsEmpty -> setRow { r with Owners = [] }
                | _ -> ()

    let post (work: unit -> unit) =
        if Volatile.Read &disposed = 0 then
            try
                host.Post work
            with :? ObjectDisposedException ->
                ()

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
        let diskText =
            if (workspace.OwnersOf path).IsEmpty then None else Workspace.ReadOnClose path

        post (fun () -> closeNow path diskText)

    /// A file that is not open changed on disk and the graphs have its new text: the open documents hear what that changed
    member _.DiskChanged(path: string) : unit = post (fun () -> diskChangedNow path)

    /// A project has loaded: its open documents were given their texts, so they are promoted and published
    member _.Loaded(graph: ProjectGraph) : unit = post (fun () -> loadedNow graph)

    /// Runs `hook` with the LSP version of a file's own list before it is sent, holding the sends queued after it. For
    /// tests.
    member internal _.BeforePublish(hook: int -> Task) : unit = outbox.BeforePublish hook

    /// Queues `diagnostics` for the open document at `path` as a diagnostic found by another file's check would be. For
    /// tests.
    member internal _.PublishForeign(path: string, diagnostics: Rfs.Lsp.Diagnostic list) : Task =
        host.Read(fun () ->
            match documents.TryFind path with
            | Some doc -> sendForeign path doc diagnostics
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
    member internal _.RetainedGraphs: int = lastAdopted.Count

    /// How many rows, last lists and sources the publisher keeps. For tests.
    member internal _.Retained() : Task<int> =
        host.Read(fun () -> rows.Value.Count + lastKeys.Count + contributions.Count)

    member _.Dispose() =
        Volatile.Write(&disposed, 1)
        outbox.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
