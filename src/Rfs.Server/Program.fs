module Rfs.Server.Program

open System
open System.IO

[<EntryPoint>]
let main _ =
    use input = Console.OpenStandardInput()
    use output = Console.OpenStandardOutput()

    // A library writing to Console.Out would corrupt the protocol on stdout; logs go to stderr.
    Console.SetOut TextWriter.Null

    Server.run input output
