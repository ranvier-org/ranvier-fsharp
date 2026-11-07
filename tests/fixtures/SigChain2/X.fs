[<AutoOpen>]
module A

let x = 1

let areaDefaults (r: int) : int = 3 * W.scale * r

let gDefaults () =
    (fun x -> x + 1.0)
    1
