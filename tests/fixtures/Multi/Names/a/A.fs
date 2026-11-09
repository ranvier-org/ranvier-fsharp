module CoreA.Api

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("RealUses")>]
do ()

let a = 1

let internal secret = 2
