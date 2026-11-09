module Schemes.B

open Schemes.A

let flexUse = flex [ 1 ]

let coercesUse = coerces [ 2 ]

let addUse = add 1 2

let ownTypeUse = ownType (Marker())

let holderUse = Holder<int list, int>([ 3 ]).Items

let inline twice x = add x x

let total = addUse + count
