namespace Rfs.Server

open System
open System.Collections.Generic

/// A document the editor has open. `Uri` is exactly what the client sent, and is what every publish for it carries.
type OpenDocument =
    { Uri: string
      Path: string
      LspVersion: int
      Text: string }

/// The open documents, by path. Written on the graph thread (the Publisher's document sync), read anywhere.
type Documents() =
    let gate = obj ()
    let documents = Dictionary<string, OpenDocument>(StringComparer.OrdinalIgnoreCase)

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

    member _.Close(path: string) : OpenDocument option =
        lock gate (fun () ->
            match documents.TryGetValue path with
            | true, d ->
                documents.Remove path |> ignore
                Some d
            | _ -> None)

    member _.All: OpenDocument list = lock gate (fun () -> documents.Values |> List.ofSeq)
