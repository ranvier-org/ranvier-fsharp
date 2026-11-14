module Rfs.Agent.Program

[<EntryPoint>]
let main argv =
    (Cli.runAsync argv).GetAwaiter().GetResult()
