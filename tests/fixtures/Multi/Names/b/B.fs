module CoreB.Api

open System.Runtime.CompilerServices

// Not this project's assembly name: the file name of its project
[<assembly: InternalsVisibleTo("Uses")>]
do ()

let b = 1

let internal hidden = 2
