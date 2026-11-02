module Use

open Ghost.Empty
open AutoNs
open Ext

let second: Split.Second = { S = { F = 1 } }

let doubled = second.S.Double

let qualified = Lib.libValue + 1

let auto = autoValue
