module Chains.D

open Chains.C

let shiftTwice (x: int) : int = shift (shift x)

let total = shiftTwice offset + 1
