module Rfs.Server.Tests.TestServer

open System
open System.IO.Pipes
open System.Threading.Tasks
open Rfs.Lsp

/// Runs the server on pipe streams and connects a client to them. `exitCode` completes with `Server.run`'s result.
let startRun () : Task<LspClient * Task<int>> =
    task {
        let toServer = new AnonymousPipeServerStream(PipeDirection.Out)
        let serverReads = new AnonymousPipeClientStream(PipeDirection.In, toServer.ClientSafePipeHandle)
        let fromServer = new AnonymousPipeServerStream(PipeDirection.In)
        let serverWrites = new AnonymousPipeClientStream(PipeDirection.Out, fromServer.ClientSafePipeHandle)

        let exitCode =
            Task.Factory.StartNew(
                (fun () -> Rfs.Server.Server.run serverReads serverWrites),
                TaskCreationOptions.LongRunning
            )

        let client = LspClient.Connect(fromServer, toServer)
        return client, exitCode
    }

let start () : Task<LspClient * IDisposable> =
    task {
        let! client, _ = startRun ()
        return client, (client :> IDisposable)
    }
