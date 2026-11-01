module Deps.E

let hidden = 1

let user = hidden + 1

let helper () : int = 5

let compute () = helper () + 1

let other () = helper () * 2

let unrelated = 7
