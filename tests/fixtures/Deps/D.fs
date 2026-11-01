module Deps.D

open Deps.A

let start = baseValue + 1

let r = ref []

let fill () = r.Value <- [ start ]

let after = 42
