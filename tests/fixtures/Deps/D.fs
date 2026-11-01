module Deps.D

open Deps.A

let start = baseValue + 1

let r = ref []

let fill () = r.Value <- [ start ]

let size () = r.Value |> List.length

let after = 42
