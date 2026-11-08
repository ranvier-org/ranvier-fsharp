namespace Rfs.Server

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open Rfs.Checker
open Rfs.Lsp
open Ionide.LanguageServerProtocol.Types
open Rfs.Projects

/// The body of `rfs/declarationsChecked`
type DeclarationsChecked = { count: int }

/// <summary>
/// Document sync and the diagnostics the editor sees. A change of an open document sets its text on the project's
/// graph; the publish waits for the graph to settle that text, and sends the diagnostics under the document's own URI
/// and LSP version.
/// </summary>
/// <remarks>
/// Open, change and close are queued as they are called and run one at a time in that order, so a handler that calls
/// them before its first suspension keeps the editor's order. Every wait is cancelled when a newer text reaches the
/// file. Publishes are serialised, so the versions published for a file only rise, and a closed document is not
/// published to again.
/// </remarks>
type Publisher(workspace: Workspace, client: RanvierClient, documents: Documents) =
    // The graph settles on its own schedule; the bridge for waits that take no token
    let waitLimit = TimeSpan.FromMinutes 10.
    let lifetime = new CancellationTokenSource()
    let queue = Channel.CreateUnbounded<unit -> unit>(UnboundedChannelOptions(SingleReader = true))
    let gate = new SemaphoreSlim 1
    let waits = Dictionary<string, CancellationTokenSource>(StringComparer.OrdinalIgnoreCase)
    // Under `gate`
    let lastKeys = Dictionary<string, struct (Rfs.Lsp.Range * int * string * string) list>(StringComparer.OrdinalIgnoreCase)
    let lastVersion = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
    let lastAdopted = Dictionary<ProjectGraph, int64>(HashIdentity.Reference)
    // What each checked file reported, by the file the diagnostics are in: a file's diagnostics are the union over sources
    let contributions = SortedDictionary<string, Dictionary<string, Rfs.Lsp.Diagnostic list>>(StringComparer.OrdinalIgnoreCase)
    let mutable beforePublish: int -> Task = fun _ -> Task.CompletedTask

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

    let swallowed (ex: exn) =
        match ex with
        // A newer text, a reload hiding the project, or the shutdown
        | :? OperationCanceledException
        | :? ObjectDisposedException -> ()
        | :? TimeoutException -> log "a file did not settle in time"
        | ex -> log $"publishing failed: %O{ex}"

    let consumer =
        Task.Run(fun () ->
            task {
                try
                    while! queue.Reader.WaitToReadAsync() do
                        let mutable work = Unchecked.defaultof<unit -> unit>

                        while queue.Reader.TryRead(&work) do
                            try
                                work ()
                            with ex ->
                                log $"document sync failed: %O{ex}"
                with ex ->
                    log $"document sync stopped: %O{ex}"
            }
            :> Task)

    let post (work: unit -> unit) = queue.Writer.TryWrite work |> ignore

    /// Sends the list when it is `force`d or differs from the last one sent for the file. Holds `gate`.
    let send (path: string) (uri: string) (version: int option) (diagnostics: Rfs.Lsp.Diagnostic list) (force: bool) : Task =
        task {
            let keys = diagnostics |> List.map keyOf

            let differs =
                match lastKeys.TryGetValue path with
                | true, last -> last <> keys
                | _ -> true

            if force || differs then
                lastKeys[path] <- keys

                match version with
                | Some v when (match lastVersion.TryGetValue path with | true, l -> v > l | _ -> true) -> lastVersion[path] <- v
                | _ -> ()

                do!
                    client.TextDocumentPublishDiagnostics
                        { Uri = uri
                          Version = version
                          Diagnostics = diagnostics |> List.map toLsp |> Array.ofList }
                    |> Async.StartAsTask
        }

    /// `rfs/declarationsChecked`: the declaration checks adopted since the previous one, over all projects. Holds `gate`.
    let notifyChecks () : Task =
        task {
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

            do! client.Notify("rfs/declarationsChecked", box { count = int n }) |> Async.StartAsTask
        }

    /// Records what the check of `source` found, and publishes the files it touched that are open: each file's list is
    /// what every checked file found in it, so a diagnostic that lands in another file (a signature's) is cleared when it
    /// goes. Diagnostics with no file of the project, rangeless ones included, belong to the checked document. Holds
    /// `gate`. The checked file's own list is sent with `ownVersion`, and always when `forceOwn`.
    let apply (source: string) (settled: FSharpDiagnostic[]) (ownVersion: int option) (forceOwn: bool) : Task =
        task {
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

            for t in touched do
                match documents.TryFind t with
                | None -> ()
                | Some doc ->
                    let union =
                        [ for KeyValue(_, byTarget) in contributions do
                              match byTarget.TryGetValue t with
                              | true, l -> yield! l
                              | _ -> () ]

                    if isSame t source then
                        do! send t doc.Uri ownVersion union forceOwn
                    else
                        do! send t doc.Uri (Some doc.LspVersion) union false
        }

    /// Publishes a settled result of the file the edit was for: once per LSP version, and never an older one than the
    /// document has.
    let publishChanged (path: string) (graph: ProjectGraph) (struct (version, settled): struct (int64 * _)) (sameVersion: bool) (ct: CancellationToken) : Task =
        task {
            match documents.LspVersionOf(graph, path, version) with
            | None -> ()
            | Some lsp ->
                do! beforePublish lsp
                do! gate.WaitAsync ct

                try
                    match documents.TryFind path with
                    | Some doc when lsp = doc.LspVersion && not ct.IsCancellationRequested ->
                        let fresh =
                            match lastVersion.TryGetValue path with
                            | true, last -> lsp > last || (sameVersion && lsp = last)
                            | _ -> true

                        if fresh then
                            do! apply path settled (Some lsp) true
                            do! notifyChecks ()
                    | _ -> ()
                finally
                    gate.Release() |> ignore
        }

    /// A result for a file that is not the one edited: published where it differs
    let publishOther (path: string) (graph: ProjectGraph) (struct (version, settled): struct (int64 * _)) (ct: CancellationToken) : Task =
        task {
            do! gate.WaitAsync ct

            try
                match documents.TryFind path, documents.LspVersionOf(graph, path, version) with
                | Some doc, Some lsp when lsp = doc.LspVersion && not ct.IsCancellationRequested ->
                    do! apply path settled (Some lsp) false
                | _ -> ()
            finally
                gate.Release() |> ignore
        }

    let settledOf (graph: ProjectGraph) (path: string) (ct: CancellationToken) =
        graph.SettledDiagnostics(path).WaitAsync(waitLimit, ct)

    /// Every other open document that one of the projects, or a project that references them, owns
    let dependents (path: string) (owners: ProjectGraph list) =
        let affected =
            owners @ (owners |> List.collect workspace.ReferencingProjects)
            |> List.distinct

        [ for doc in documents.All do
              if not (isSame doc.Path path) then
                  match workspace.OwnersOf doc.Path |> List.tryFind (fun g -> affected |> List.exists (fun a -> obj.ReferenceEquals(a, g))) with
                  | Some g -> yield doc.Path, g
                  | None -> () ]

    let publishDependents (path: string) (owners: ProjectGraph list) (ct: CancellationToken) : Task =
        task {
            for other, g in dependents path owners do
                let! result = settledOf g other ct
                do! publishOther other g result ct
        }

    let run (path: string) (owners: ProjectGraph list) (sameVersion: bool) (ct: CancellationToken) : Task =
        task {
            try
                let! first = settledOf owners.Head path ct
                do! publishChanged path owners.Head first sameVersion ct
                do! publishDependents path owners ct
            with ex ->
                swallowed ex
        }

    /// A closed file: its text is the disk's again, so what it reports to the files that are open is brought up to date
    let runClosed (path: string) (owners: ProjectGraph list) (ct: CancellationToken) : Task =
        task {
            try
                let! struct (_, settled) = settledOf owners.Head path ct
                do! gate.WaitAsync ct

                try
                    if not ct.IsCancellationRequested && (documents.TryFind path).IsNone then
                        do! apply path settled None false
                finally
                    gate.Release() |> ignore

                do! publishDependents path owners ct
            with ex ->
                swallowed ex
        }

    /// Starts `work` with a token that the next one for the file cancels
    let start (path: string) (work: CancellationToken -> Task) : unit =
        let cts = CancellationTokenSource.CreateLinkedTokenSource lifetime.Token

        let previous =
            lock waits (fun () ->
                let previous =
                    match waits.TryGetValue path with
                    | true, p -> Some p
                    | _ -> None

                waits[path] <- cts
                previous)

        previous |> Option.iter _.Cancel()

        (task {
            try
                do! work cts.Token
            finally
                lock waits (fun () ->
                    match waits.TryGetValue path with
                    | true, current when obj.ReferenceEquals(current, cts) -> waits.Remove path |> ignore
                    | _ -> ())

                cts.Dispose()
        })
        |> ignore

    let cancelWaits (path: string) =
        lock waits (fun () ->
            match waits.TryGetValue path with
            | true, p ->
                waits.Remove path |> ignore
                p.Cancel()
            | _ -> ())

    /// An empty list for a document that no project owns (any more); a list that is empty already is not sent again
    let publishEmpty (doc: OpenDocument) (force: bool) : unit =
        task {
            try
                do! gate.WaitAsync lifetime.Token

                try
                    if (documents.TryFind doc.Path).IsSome then
                        contributions.Remove doc.Path |> ignore
                        do! send doc.Path doc.Uri (Some doc.LspVersion) [] force
                finally
                    gate.Release() |> ignore
            with ex ->
                swallowed ex
        }
        |> ignore

    let openNow (uri: string) (path: string) (version: int) (text: string) =
        let document =
            { Uri = uri
              Path = path
              LspVersion = version
              Text = text
              Graph = workspace.ProjectOf path }

        documents.Open document

        match workspace.Open(path, text, version) with
        | [] -> publishEmpty document true
        | owners ->
            (fst owners.Head).Promote path
            let graphs = owners |> List.map fst
            start path (fun ct -> run path graphs false ct)

    let changeNow (path: string) (version: int) (change: string -> string) =
        match documents.Update(path, (fun d -> { d with LspVersion = version; Text = change d.Text })) with
        | Some d ->
            match workspace.Open(path, d.Text, version) with
            | [] -> ()
            | owners ->
                let graphs = owners |> List.map fst
                start path (fun ct -> run path graphs false ct)
        | None -> ()

    let closeNow (path: string) =
        cancelWaits path

        match documents.Close path with
        | None -> ()
        | Some d ->
            let owners = workspace.Close path

            for g in owners do
                g.Demote path

            task {
                try
                    do! gate.WaitAsync lifetime.Token

                    try
                        lastVersion.Remove path |> ignore
                        do! send path d.Uri None [] true
                        lastKeys.Remove path |> ignore
                    finally
                        gate.Release() |> ignore
                with ex ->
                    swallowed ex
            }
            |> ignore

            if not owners.IsEmpty then
                start path (fun ct -> runClosed path owners ct)

    let loadedNow (graph: ProjectGraph) =
        documents.Prune workspace.Projects

        for doc in documents.All do
            match workspace.OwnersOf doc.Path with
            | owners when owners |> List.exists (fun g -> obj.ReferenceEquals(g, graph)) ->
                documents.Update(doc.Path, (fun d -> { d with Graph = Some owners.Head })) |> ignore
                owners.Head.Promote doc.Path
                start doc.Path (fun ct -> run doc.Path owners true ct)
            | _ -> ()

    /// After a load or reload: a document whose project is gone gets the empty list
    let sweepNow () =
        documents.Prune workspace.Projects

        for doc in documents.All do
            if (workspace.OwnersOf doc.Path).IsEmpty then
                cancelWaits doc.Path
                publishEmpty doc false

    do workspace.OnLoadEnd(fun () -> post sweepNow)

    /// The editor opened the document: the file's text is the document's, and the project that owns it checks it
    /// by declaration. A document no project owns gets one empty list.
    member _.Open(uri: string, path: string, version: int, text: string) : unit =
        post (fun () -> openNow uri path version text)

    /// The editor changed the document: `change` makes the new text from the old
    member _.Change(path: string, version: int, change: string -> string) : unit =
        post (fun () -> changeNow path version change)

    /// The editor closed the document: the file on disk is its text again, it is demoted, and its diagnostics cleared
    member _.Close(path: string) : unit = post (fun () -> closeNow path)

    /// A project has loaded: its open documents were given their texts, so they are promoted and published
    member _.Loaded(graph: ProjectGraph) : unit = post (fun () -> loadedNow graph)

    /// Runs `hook` with the LSP version of a settled result before the publish takes its turn. For tests.
    member internal _.BeforePublish(hook: int -> Task) : unit = beforePublish <- hook

    /// Completes when everything queued so far has run. For tests.
    member internal _.Flushed() : Task =
        let done' = TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously)
        post (fun () -> done'.SetResult())
        done'.Task

    /// How many graphs the publisher keeps state for. For tests.
    member internal _.RetainedGraphs: int = lastAdopted.Count + documents.VersionMaps

    member _.Dispose() =
        lifetime.Cancel()
        queue.Writer.TryComplete() |> ignore

    interface IDisposable with
        member this.Dispose() = this.Dispose()
