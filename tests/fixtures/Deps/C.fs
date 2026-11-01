namespace Deps

[<AutoOpen>]
module Helpers =
    let describe (n: int) : string = string n

module C =
    open Deps.B.Util
    open Deps.A

    let total = length [ 1; 2; 3 ]

    let label = describe total

    let areaSum = area (Circle 1) + area (Square 2)
