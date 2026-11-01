module Rfs.Lsp.Tests.JsonRpcTests

open System
open System.IO
open System.IO.Pipelines
open System.Text
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open Rfs.Lsp

let private firstMessage (connection: JsonRpcConnection) =
    task {
        let e = connection.Messages.GetAsyncEnumerator()
        let! hasOne = e.MoveNextAsync()
        Expect.isTrue hasOne "a frame is available"
        return e.Current
    }

[<Tests>]
let tests =
    testList
        "JSON-RPC framing"
        [ testTask "frames round trip" {
              let pipe = Pipe()
              let sender = JsonRpcConnection(Stream.Null, pipe.Writer.AsStream())
              let receiver = JsonRpcConnection(pipe.Reader.AsStream(), Stream.Null)

              let first = JsonNode.Parse("""{"jsonrpc":"2.0","id":1,"method":"initialize"}""").AsObject()
              let second = JsonNode.Parse("""{"jsonrpc":"2.0","method":"initialized","params":{}}""").AsObject()
              do! sender.Send first
              do! sender.Send second
              pipe.Writer.Complete()

              let received = ResizeArray<Received>()
              let e = receiver.Messages.GetAsyncEnumerator()

              let mutable go = true

              while go do
                  let! more = e.MoveNextAsync()

                  if more then received.Add e.Current else go <- false

              Expect.equal received.Count 2 "both frames read"
              Expect.isTrue (JsonNode.DeepEquals(received[0].Message, first)) "first equal"
              Expect.isTrue (JsonNode.DeepEquals(received[1].Message, second)) "second equal"
              Expect.isLessThanOrEqual received[0].ReceivedAt received[1].ReceivedAt "timestamps are monotonic"
          }

          testTask "split header across reads" {
              let pipe = Pipe()
              let receiver = JsonRpcConnection(pipe.Reader.AsStream(), Stream.Null)
              let frame = Encoding.ASCII.GetBytes "Content-Length: 2\r\n\r\n{}"

              let feeder =
                  task {
                      for b in frame do
                          let! _ = pipe.Writer.WriteAsync(ReadOnlyMemory [| b |])
                          do! Task.Yield()

                      pipe.Writer.Complete()
                  }

              let! got = firstMessage receiver
              do! feeder
              Expect.equal (got.Message.ToJsonString()) "{}" "empty object parsed"
          }

          testTask "non-ASCII body uses byte length" {
              let output = new MemoryStream()
              let sender = JsonRpcConnection(Stream.Null, output)
              do! sender.Send(JsonNode.Parse("""{"x":"é"}""").AsObject())

              let text = Encoding.UTF8.GetString(output.ToArray())
              Expect.equal text "Content-Length: 10\r\n\r\n{\"x\":\"é\"}" "byte length, unescaped body"
          } ]
