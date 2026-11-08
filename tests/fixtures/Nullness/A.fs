module Nullness.A

open System.Collections.Generic

let first = 1

let lazyMap f (x: Lazy<_>) = lazy (f (x.Force()))

type Keyed<'Key, 'Data when 'Key: equality and 'Key: not null>(keyf: 'Data -> 'Key, lazyItems: Lazy<'Data list>) =

    let quickMap =
        lazyItems
        |> lazyMap (fun entries ->
            let t = Dictionary<_, _>(entries.Length, HashIdentity.Structural)

            for y in entries do
                let key = keyf y

                let v =
                    match t.TryGetValue key with
                    | true, v -> v
                    | _ -> []

                t[key] <- y :: v

            t)

    member _.Entries() = lazyItems.Force()

    member _.Item
        with get x =
            match quickMap.Force().TryGetValue x with
            | true, v -> v
            | _ -> []

let named (s: string | null) =
    match s with
    | null -> "none"
    | s -> s

let keyed = Keyed<string, int>(string, lazy [ 1; 2 ])

let length (s: string | null) = s.Length
