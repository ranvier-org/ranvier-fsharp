namespace Rfs.Server

open System
open System.Threading
open Ranvier
open Rfs.Checker

/// A document the editor has open. `Uri` is exactly what the client sent, and is what every publish for it carries.
type OpenDocument =
    { Uri: string
      Path: string
      LspVersion: int
      Text: string }

/// <summary>
/// The open documents, by path: a keyed collection on the host's graph, so editing one wakes only what read that one.
/// </summary>
/// <remarks>
/// Written on the graph thread (the Publisher's document sync). <c>TryFindNow</c> is the tracked read there; the other
/// reads, from any thread, see the documents as the graph thread last left them, in the order they were opened.
/// </remarks>
type Documents(host: GraphHost) =
    let keyOf (path: string) = path.ToUpperInvariant()
    // Graph thread
    let mutable rows: KeyedCollection<string, OpenDocument> = Unchecked.defaultof<_>
    // Any thread
    let mutable published: Map<string, OpenDocument> = Map.empty
    let mutable inOrder: OpenDocument list = []

    do
        host
            .Read(fun () -> createRoot (fun _ -> rows <- createKeyedCollection (fun (d: OpenDocument) -> keyOf d.Path)) |> ignore)
            .Wait()

    /// Graph thread
    let republish () =
        let all = untrack (fun () -> [ for k in rows.Rows.Keys -> rows.Rows.Get k ])
        Volatile.Write(&published, all |> List.map (fun d -> keyOf d.Path, d) |> Map.ofList)
        Volatile.Write(&inOrder, all)

    /// Graph thread
    member _.Open(document: OpenDocument) : unit =
        rows.AddOrUpdate document
        republish ()

    member _.TryFind(path: string) : OpenDocument option = Volatile.Read &published |> Map.tryFind (keyOf path)

    /// Graph thread, tracked: the document at `path`, as the collection holds it in this turn
    member _.TryFindNow(path: string) : OpenDocument option = rows.Rows.TryGet(keyOf path)

    /// Graph thread, untracked: the open document's LSP version as the collection holds it in this turn
    member _.TryVersionNow(path: string) : int option =
        untrack (fun () -> rows.Rows.TryGet(keyOf path)) |> Option.map _.LspVersion

    /// Graph thread
    member this.Update(path: string, update: OpenDocument -> OpenDocument) : OpenDocument option =
        match this.TryFind path with
        | Some d ->
            let updated = update d
            rows.AddOrUpdate updated
            republish ()
            Some updated
        | None -> None

    /// Graph thread
    member this.Close(path: string) : OpenDocument option =
        match this.TryFind path with
        | Some d ->
            rows.Remove(keyOf path) |> ignore
            republish ()
            Some d
        | None -> None

    member _.All: OpenDocument list = Volatile.Read &inOrder
