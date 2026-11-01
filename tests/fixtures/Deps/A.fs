module Deps.A

let baseValue = 10

let scale (x: int) : int = x * 2

type Shape =
    | Circle of radius: int
    | Square of side: int

let area (s: Shape) : int =
    match s with
    | Circle r -> 3 * r * r
    | Square a -> a * a

let doubled = scale baseValue

let grouped = (baseValue + 1) * 2
