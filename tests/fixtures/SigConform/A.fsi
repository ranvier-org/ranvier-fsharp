module SigConform.A

type T =
    new: unit -> T
    member M: t: int -> int

val f: x: int -> (int -> int)

val g: x: 'a -> 'a

val useM: int

val useF: int

val useG: int
