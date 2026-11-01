module Rfs.Harness.Tests.ServersTests

open System.IO
open Expecto
open Rfs.Harness

let private withFile (content: string) (body: string -> unit) =
    let path = Path.GetTempFileName()

    try
        File.WriteAllText(path, content)
        body path
    finally
        File.Delete path

[<Tests>]
let tests =
    testList
        "Servers"
        [ test "loads servers with defaults" {
              withFile
                  """[ { "name": "fsac", "command": "dotnet", "args": ["fsac.dll"], "env": { "A": "b" },
                         "initOptions": { "x": 1 }, "oracle": "report" },
                       { "name": "bare", "command": "x" } ]"""
                  (fun path ->
                      match Servers.load path with
                      | Ok [ a; b ] ->
                          Expect.equal (a.Name, a.Args, a.Env, a.Oracle) ("fsac", [ "fsac.dll" ], Map [ "A", "b" ], Report) "first"
                          Expect.equal (a.InitOptions.ToJsonString()) """{"x":1}""" "init options"
                          Expect.equal (b.Oracle, b.Args, b.Env) (Strict, [], Map.empty) "defaults"
                      | other -> failtestf "unexpected %A" other)
          }

          test "rejects unknown oracle mode" {
              withFile """[ { "name": "s", "command": "x", "oracle": "lax" } ]""" (fun path ->
                  Expect.isError (Servers.load path) "unknown mode")
          } ]
