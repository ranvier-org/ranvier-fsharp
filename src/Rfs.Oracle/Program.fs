/// The correctness ground truth: checks a whole project from scratch on the fork, one JSON request per line on stdin,
/// one JSON response per line on stdout.
module Rfs.Oracle.Program

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open FSharp.Compiler.SourceCodeServices
open Rfs.Lsp
open Rfs.Projects

let private check (project: string) (overrides: Map<string, string>) =
    // `load` has its own disk cache that notices project edits, so there is none here.
    let spec = (ProjectSpec.load project).GetAwaiter().GetResult()

    let overrides =
        overrides |> Map.toSeq |> Seq.map (fun (p, t) -> LspClient.NormalizePath p, t) |> Map.ofSeq

    let sourceReader (file: string) =
        let text =
            match overrides |> Map.tryFind (LspClient.NormalizePath file) with
            | Some text -> text
            | None -> File.ReadAllText file

        hash text, lazy text

    // Fresh checker per request: nothing is carried over, so no incremental path is involved.
    let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
    let files = Array.ofList spec.SourceFiles

    let results =
        checker.ParseAndCheckProject(spec.ProjectFile, files, sourceReader)
        |> Async.RunSynchronously

    [ for d in results.Diagnostics -> FcsDiagnostics.ofFcs d ]

let private respond (request: JsonNode) =
    let project = request["project"].GetValue<string>() |> Path.GetFullPath

    let overrides =
        match request["overrides"] with
        | null -> Map.empty
        | o -> [ for KeyValue(path, text) in o.AsObject() -> path, text.GetValue<string>() ] |> Map.ofList

    let diagnostics = JsonArray()

    for d in check project overrides do
        diagnostics.Add(Rfs.Lsp.Diagnostics.toJson d)

    let response = JsonObject()
    response["diagnostics"] <- diagnostics
    response

[<EntryPoint>]
let main _ =
    // Read when the checker module initialises, so it has to be set before the first checker exists.
    Environment.SetEnvironmentVariable("FICKL_FCS_SIGNATURE_CUTOFF", "0")

    // stdout carries the protocol only; anything the compiler prints goes to stderr.
    let utf8 = UTF8Encoding false
    let output = new StreamWriter(Console.OpenStandardOutput(), utf8)
    output.AutoFlush <- true
    Console.SetOut Console.Error
    let input = new StreamReader(Console.OpenStandardInput(), utf8)

    let mutable line = input.ReadLine()

    while not (isNull line) do
        if line.Trim() <> "" then
            let response =
                try
                    respond (JsonNode.Parse line)
                with e ->
                    let o = JsonObject()
                    o["error"] <- JsonValue.Create(e.ToString())
                    o

            output.WriteLine(response.ToJsonString())

        line <- input.ReadLine()

    0
