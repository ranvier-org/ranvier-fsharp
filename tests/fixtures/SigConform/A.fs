module SigConform.A

type T() =
    member _.M(t: int) = t

let f (x: int) y = x + y

let g (x: 'a) = x

let useM = T().M(t = 1)

let useF = f 1 2

let useG = g 3
