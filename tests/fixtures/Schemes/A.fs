module Schemes.A

type Marker() =
    member _.Id = 1

let flex (x: #seq<'a>) = x

let coerces<'a, 'b when 'a :> seq<'b>> (x: 'a) = x

let inline add (x: ^T) y = x + y

let ownType<'a when 'a :> Marker> (x: 'a) = x

type Holder<'a, 'b when 'a :> seq<'b>>(x: 'a) =
    member _.Items = x

let count = 1
