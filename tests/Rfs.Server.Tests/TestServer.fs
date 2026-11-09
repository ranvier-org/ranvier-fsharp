module Rfs.Server.Tests.TestServer

open System
open System.IO.Pipes
open System.Threading.Tasks
open Rfs.Lsp

/// A server running on pipe streams and a client connected to it
type Connection =
    { Client: LspClient
      /// Completes with the result of the server's run, once its input ends or it exits
      ExitCode: Task<int>
      /// The server the connection created
      Server: Task<Rfs.Server.RanvierServer>
      /// Ends the client's side, so the server's input ends and its run returns
      Owner: IDisposable }

/// An anonymous pipe's read holds a pool thread until the other end writes or closes, so a connection that stays open
/// keeps two pool threads for the rest of the process. Closing each write end ends the read on the other side.
let connect () : Connection =
    let toServer = new AnonymousPipeServerStream(PipeDirection.Out)
    let serverReads = new AnonymousPipeClientStream(PipeDirection.In, toServer.ClientSafePipeHandle)
    let fromServer = new AnonymousPipeServerStream(PipeDirection.In)
    let serverWrites = new AnonymousPipeClientStream(PipeDirection.Out, fromServer.ClientSafePipeHandle)
    let created = TaskCompletionSource<Rfs.Server.RanvierServer>(TaskCreationOptions.RunContinuationsAsynchronously)

    let exitCode =
        Task.Factory.StartNew(
            (fun () ->
                try
                    Rfs.Server.Server.runWith (fun s -> created.TrySetResult s |> ignore) serverReads serverWrites
                finally
                    if created.Task.IsCompletedSuccessfully then
                        (created.Task.Result :> IDisposable).Dispose()

                    serverWrites.Dispose()),
            TaskCreationOptions.LongRunning
        )

    let client = LspClient.Connect(fromServer, toServer)

    { Client = client
      ExitCode = exitCode
      Server = created.Task
      Owner =
        { new IDisposable with
            member _.Dispose() =
                (client :> IDisposable).Dispose()
                toServer.Dispose() } }

/// Runs the server on pipe streams and connects a client to them. `exitCode` completes with `Server.run`'s result.
let startRun () : Task<LspClient * Task<int>> =
    let c = connect ()
    Task.FromResult((c.Client, c.ExitCode))

/// Like `startRun`, and also hands out the server the connection created, for tests that reach into it.
let startCapturing () : Task<LspClient * IDisposable * Task<Rfs.Server.RanvierServer>> =
    let c = connect ()
    Task.FromResult((c.Client, c.Owner, c.Server))

let start () : Task<LspClient * IDisposable> =
    let c = connect ()
    Task.FromResult((c.Client, c.Owner))
