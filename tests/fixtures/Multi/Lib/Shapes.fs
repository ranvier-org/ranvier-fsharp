module Lib.Shapes

type Shape =
    | Circle of int
    | Square of int

let area (s: Shape) : int =
    match s with
    | Circle r -> 3 * r * r
    | Square a -> a * a
