module rec R

let even (n: int) : bool = if n = 0 then true else odd (n - 1)

let odd (n: int) : bool = if n = 0 then false else even (n - 1)

let start = 10
