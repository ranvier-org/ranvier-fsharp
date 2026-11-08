namespace Rfs.Server

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
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
/// Every wait is cancelled when a newer text reaches the file. Publishes are serialised, so the versions published for a
/// file only rise, and a closed document is not published to again.
/// </remarks>
type Publisher(workspace: Workspace, client: RanvierClient, documents: Documents) =
    // The graph settles on its own schedule; the bridge for waits that take no token
    let waitLimit = TimeSpan.FromMinutes 10.
    let lifetime = new CancellationTokenSource()
    let editGate = obj ()
    let gate = new SemaphoreSlim 1
    let waits = Dictionary<string, CancellationTokenSource>(StringComparer.OrdinalIgnoreCase)
    // Under `gate`
    let lastKeys = Dictionary<string, struct (Rfs.Lsp.Range * int * string * string) list>(StringComparer.OrdinalIgnoreCase)
    let lastVersion = Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
    let lastAdopted = Dictionary<ProjectGraph, int64>(HashIdentity.Reference)

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

            for g in workspace.Projects do
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

    let isSame (a: string) (b: string) = String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    /// Publishes a settled result of the file the edit was for: once per LSP version, never an older one than the
    /// document has, and each diagnostic under its own file.
    let publishChanged (path: string) (graph: ProjectGraph) (struct (version, settled): struct (int64 * _)) (sameVersion: bool) (ct: CancellationToken) : Task =
        task {
            do! gate.WaitAsync ct

            try
                match documents.TryFind path, documents.LspVersionOf(graph, path, version) with
                | Some doc, Some lsp when lsp = doc.LspVersion && not ct.IsCancellationRequested ->
                    let fresh =
                        match lastVersion.TryGetValue path with
                        | true, last -> lsp > last || (sameVersion && lsp = last)
                        | _ -> true

                    if fresh then
                        let own = LspClient.NormalizePath path
                        let all = settled |> Array.map FcsDiagnostics.ofFcs |> List.ofArray
                        let isOwn (d: Rfs.Lsp.Diagnostic) = d.File = "" || isSame d.File own
                        do! send path doc.Uri (Some lsp) (all |> List.filter isOwn) true

                        // A diagnostic belongs to the file it is in, whichever file's check found it
                        for file, ds in all |> List.filter (isOwn >> not) |> List.groupBy _.File do
                            match documents.TryFind file with
                            | Some other -> do! send file other.Uri (Some other.LspVersion) ds false
                            | None -> do! send file (Paths.toUri file) None ds false

                        do! notifyChecks ()
                | _ -> ()
            finally
                gate.Release() |> ignore
        }

    /// A file of another document that the edit may have changed the diagnostics of: published when they differ
    let publishDependent (path: string) (graph: ProjectGraph) (struct (version, settled): struct (int64 * _)) (ct: CancellationToken) : Task =
        task {
            do! gate.WaitAsync ct

            try
                match documents.TryFind path, documents.LspVersionOf(graph, path, version) with
                | Some doc, Some lsp when lsp = doc.LspVersion && not ct.IsCancellationRequested ->
                    let own = LspClient.NormalizePath path

                    let ours =
                        settled
                        |> Array.map FcsDiagnostics.ofFcs
                        |> List.ofArray
                        |> List.filter (fun d -> d.File = "" || isSame d.File own)

                    do! send path doc.Uri (Some lsp) ours false
                | _ -> ()
            finally
                gate.Release() |> ignore
        }

    let settledOf (graph: ProjectGraph) (path: string) (ct: CancellationToken) =
        graph.SettledDiagnostics(path).WaitAsync(waitLimit, ct)

    let run (path: string) (graph: ProjectGraph) (sameVersion: bool) (ct: CancellationToken) : Task =
        task {
            try
                let! first = settledOf graph path ct
                do! publishChanged path graph first sameVersion ct

                let affected = graph :: workspace.ReferencingProjects graph

                for doc in documents.All do
                    if not (isSame doc.Path path) then
                        match workspace.ProjectOf doc.Path with
                        | Some g when affected |> List.exists (fun a -> obj.ReferenceEquals(a, g)) ->
                            let! result = settledOf g doc.Path ct
                            do! publishDependent doc.Path g result ct
                        | _ -> ()
            with
            // A newer text, a reload hiding the project, or the shutdown
            | :? OperationCanceledException
            | :? ObjectDisposedException -> ()
            | :? TimeoutException -> log $"%s{path} did not settle in time"
            | ex -> log $"publishing for %s{path} failed: %O{ex}"
        }

    /// Starts the publish for the file's latest text, cancelling the one before it
    let kick (path: string) (graph: ProjectGraph) (sameVersion: bool) : unit =
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
        run path graph sameVersion cts.Token |> ignore

    let cancelWaits (path: string) =
        lock waits (fun () ->
            match waits.TryGetValue path with
            | true, p ->
                waits.Remove path |> ignore
                p.Cancel()
            | _ -> ())

    let publishUnowned (doc: OpenDocument) : unit =
        task {
            try
                do! gate.WaitAsync lifetime.Token

                try
                    if (documents.TryFind doc.Path).IsSome then
                        do! send doc.Path doc.Uri (Some doc.LspVersion) [] true
                finally
                    gate.Release() |> ignore
            with
            | :? OperationCanceledException
            | :? ObjectDisposedException -> ()
            | ex -> log $"publishing for %s{doc.Path} failed: %O{ex}"
        }
        |> ignore

    /// The editor opened the document: the file's text is the document's, and the project that owns it checks it
    /// by declaration. A document no project owns gets one empty list.
    member _.Open(uri: string, path: string, version: int, text: string) : unit =
        lock editGate (fun () ->
            let document =
                { Uri = uri
                  Path = path
                  LspVersion = version
                  Text = text
                  Graph = workspace.ProjectOf path }

            documents.Open document

            match workspace.Open(path, text, version) with
            | [] -> publishUnowned document
            | (graph, _) :: _ ->
                graph.Promote path
                kick path graph false)

    /// The editor changed the document: `change` makes the new text from the old
    member _.Change(path: string, version: int, change: string -> string) : unit =
        lock editGate (fun () ->
            let updated =
                documents.Update(path, (fun d -> { d with LspVersion = version; Text = change d.Text }))

            match updated with
            | Some d ->
                match workspace.Open(path, d.Text, version) with
                | (graph, _) :: _ -> kick path graph false
                | [] -> ()
            | None -> ())

    /// The editor closed the document: it is demoted, and its diagnostics cleared
    member _.Close(path: string) : unit =
        lock editGate (fun () ->
            cancelWaits path
            workspace.Close path

            match documents.Close path with
            | Some d ->
                (match workspace.ProjectOf path with
                 | Some graph -> graph.Demote path
                 | None -> ())

                task {
                    try
                        do! gate.WaitAsync lifetime.Token

                        try
                            lastVersion.Remove path |> ignore
                            do! send path d.Uri None [] true
                            lastKeys.Remove path |> ignore
                        finally
                            gate.Release() |> ignore
                    with
                    | :? OperationCanceledException
                    | :? ObjectDisposedException -> ()
                    | ex -> log $"publishing for %s{path} failed: %O{ex}"
                }
                |> ignore
            | None -> ())

    /// A project has loaded: its open documents were given their texts, so they are promoted and published
    member _.Loaded(graph: ProjectGraph) : unit =
        lock editGate (fun () ->
            for doc in documents.All do
                match workspace.ProjectOf doc.Path with
                | Some g when obj.ReferenceEquals(g, graph) ->
                    documents.Update(doc.Path, (fun d -> { d with Graph = Some g })) |> ignore
                    g.Promote doc.Path
                    kick doc.Path g true
                | _ -> ())

    member _.Dispose() = lifetime.Cancel()

    interface IDisposable with
        member this.Dispose() = this.Dispose()
