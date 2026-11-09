module Lib.Api

open System.Runtime.CompilerServices
open Lib.Shapes

[<assembly: InternalsVisibleTo("App")>]
do ()

let f (x: int) : int = x + 1

let internal g (x: int) : int = x + 2

let shapeOf () : Shape = Circle 1
