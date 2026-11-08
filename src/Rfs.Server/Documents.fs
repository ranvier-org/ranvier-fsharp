namespace Rfs.Server

open System
open System.Collections.Generic
open Rfs.Checker

/// A document the editor has open. `Uri` is exactly what the client sent, and is what every publish for it carries.
type OpenDocument =
    { Uri: string
      Path: string
      LspVersion: int
      Text: string
      Graph: ProjectGraph option }

/// The open documents, and for each (graph, file) the map from the graph's internal text versions to the LSP versions
/// they were set for.
type Documents() =
    let gate = obj ()
    let documents = Dictionary<string, OpenDocument>(StringComparer.OrdinalIgnoreCase)
    let versions = Dictionary<struct (ProjectGraph * string), Dictionary<int64, int>>(HashIdentity.Structural)

    let key (path: string) = path.ToUpperInvariant()

    member _.Open(document: OpenDocument) : unit =
        lock gate (fun () -> documents[document.Path] <- document)

    member _.TryFind(path: string) : OpenDocument option =
        lock gate (fun () ->
            match documents.TryGetValue path with
            | true, d -> Some d
            | _ -> None)

    member _.Update(path: string, update: OpenDocument -> OpenDocument) : OpenDocument option =
        lock gate (fun () ->
            match documents.TryGetValue path with
            | true, d ->
                let updated = update d
                documents[path] <- updated
                Some updated
            | _ -> None)

    /// Forgets the document and the version maps of its file
    member _.Close(path: string) : OpenDocument option =
        lock gate (fun () ->
            match documents.TryGetValue path with
            | true, d ->
                documents.Remove path |> ignore

                for k in versions.Keys |> Seq.filter (fun (struct (_, p)) -> p = key path) |> Seq.toArray do
                    versions.Remove k |> ignore

                Some d
            | _ -> None)

    member _.All: OpenDocument list = lock gate (fun () -> documents.Values |> List.ofSeq)

    /// `version` is the graph's version of the file's text, set for LSP version `lspVersion`
    member _.Record(graph: ProjectGraph, path: string, version: int64, lspVersion: int) : unit =
        lock gate (fun () ->
            // A closed document has no map to keep
            if documents.ContainsKey path then
                let k = struct (graph, key path)

                let map =
                    match versions.TryGetValue k with
                    | true, m -> m
                    | _ ->
                        let m = Dictionary<int64, int>()
                        versions[k] <- m
                        m

                map[version] <- lspVersion)

    member _.LspVersionOf(graph: ProjectGraph, path: string, version: int64) : int option =
        lock gate (fun () ->
            match versions.TryGetValue(struct (graph, key path)) with
            | true, m ->
                match m.TryGetValue version with
                | true, v -> Some v
                | _ -> None
            | _ -> None)
