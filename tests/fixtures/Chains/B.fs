module Chains.B

open Chains.A

let twice (x: int) : int = scale (scale x)

let counted = twice baseValue + 1
