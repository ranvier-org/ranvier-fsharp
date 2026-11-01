module Deps.B

open Deps.A

type Counter() =
    member _.Start = baseValue

type Counter with
    member c.Next = c.Start + 1

module Util =
    let length (xs: int list) : int = List.length xs

    let twice (x: int) : int = scale x

let counted = Counter().Next + Util.length [ 1; 2 ]
