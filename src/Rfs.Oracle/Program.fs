/// The correctness ground truth: checks a whole project from scratch on the fork, one JSON request per line on stdin,
/// one JSON response per line on stdout.
module Rfs.Oracle.Program

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices
open Rfs.Lsp
open Rfs.Projects

/// `-o:` and the other options naming a build's outputs: a compile of a referenced project writes elsewhere
let private isOutputOption (o: string) =
    [ "-o:"; "--out:"; "--refout:"; "--doc:"; "--pdb:"; "--refonly" ] |> List.exists o.StartsWith

let private sourceOf (overrides: Map<string, string>) (file: string) =
    overrides |> Map.tryFind (LspClient.NormalizePath file)

/// Compiles the project with `options`, its sources written to temporary files with its overrides, to a DLL in `temp`. A
/// file keeps its name, which an implicit module takes. The compiler alone: no incremental code.
let private compile (temp: string) (spec: ProjectSpec) (overrides: Map<string, string>) (options: string list) =
    let dir = Directory.CreateDirectory(Path.Combine(temp, Guid.NewGuid().ToString("N"))).FullName

    // Every source is copied, a directory for each directory: a signature file pairs with its implementation file by path
    let directories = spec.SourceFiles |> List.map Path.GetDirectoryName |> List.distinct

    let sources =
        spec.SourceFiles
        |> List.map (fun file ->
            let index = directories |> List.findIndex (fun d -> d = Path.GetDirectoryName file)
            let copy = Path.Combine(Directory.CreateDirectory(Path.Combine(dir, string index)).FullName, Path.GetFileName file)
            File.WriteAllText(copy, sourceOf overrides file |> Option.defaultWith (fun () -> File.ReadAllText file))
            copy)

    let dll =
        let name =
            spec.OtherOptions
            |> List.tryPick (fun o -> if o.StartsWith "-o:" then Some(Path.GetFileName(o.Substring 3)) else None)
            |> Option.defaultValue (Path.GetFileNameWithoutExtension spec.ProjectFile + ".dll")

        Path.Combine(dir, name)

    let argv =
        [ yield "fsc.exe"
          yield "-o:" + dll
          yield! options |> List.filter (isOutputOption >> not)
          yield! sources ]

    let diagnostics, failure = FSharpChecker.Create().Compile(Array.ofList argv) |> Async.RunSynchronously

    if failure.IsSome || not (File.Exists dll) then
        let errors = diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error) |> Array.map string
        let joined = String.Join("; ", errors)
        failwith $"referenced project %s{spec.ProjectFile} does not compile, so it has no assembly: %s{joined}"

    dll

/// The project's diagnostics and those of the projects it references that are overridden, with their assemblies:
/// a referenced project with an override, or a reference that has one, is compiled, and its DLL replaces its `-r:`.
let rec private checkWith (temp: string) (visited: System.Collections.Generic.Dictionary<string, (string * Diagnostic list) option>) (project: string) (overrides: Map<string, string>) =
    // `load` has its own disk cache that notices project edits, so there is none here.
    // Checked sequentially, whatever the project asks for, as editors and the declaration checker check.
    let spec = (ProjectSpec.load project).GetAwaiter().GetResult() |> ProjectSpec.sequential

    let referenced =
        [ for r in (if overrides.IsEmpty then [] else spec.ReferencedProjects) do
              let compiled =
                  match visited.TryGetValue r with
                  | true, known -> known
                  | _ ->
                      let answer = compileIfOverridden temp visited r overrides
                      visited[r] <- answer
                      answer

              match compiled with
              | Some(dll, diagnostics) ->
                  let target = (ProjectSpec.load r).GetAwaiter().GetResult()
                  yield ProjectSpec.referencePath spec target, dll, diagnostics
              | None -> () ]

    let substituted =
        spec.OtherOptions
        |> List.map (fun o ->
            match referenced |> List.tryFind (fun (path, _, _) -> path |> Option.exists (fun p -> o = "-r:" + p)) with
            | Some(_, dll, _) -> "-r:" + dll
            | None -> o)

    let spec = { spec with OtherOptions = substituted }

    let sourceReader (file: string) =
        let text =
            match sourceOf overrides file with
            | Some text -> text
            | None -> File.ReadAllText file

        hash text, lazy text

    // Fresh checker per request: nothing is carried over, so no incremental path is involved.
    let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
    let files = Array.ofList spec.SourceFiles

    let results =
        checker.ParseAndCheckProject(spec.ProjectFile, files, sourceReader)
        |> Async.RunSynchronously

    spec,
    [ for d in results.Diagnostics -> FcsDiagnostics.ofFcs d ]
    @ [ for _, _, diagnostics in referenced do
            yield! diagnostics ]

/// None when neither the project nor a project it reaches has an override: its assembly on disk stands.
and private compileIfOverridden temp visited (project: string) (overrides: Map<string, string>) =
    let rec reaches (file: string) =
        let spec = (ProjectSpec.load file).GetAwaiter().GetResult()
        spec.SourceFiles |> List.exists (fun f -> (sourceOf overrides f).IsSome) || spec.ReferencedProjects |> List.exists reaches

    if reaches project then
        let spec, diagnostics = checkWith temp visited project overrides
        Some(compile temp spec overrides spec.OtherOptions, diagnostics)
    else
        None

let private check (project: string) (overrides: Map<string, string>) =
    let overrides =
        overrides |> Map.toSeq |> Seq.map (fun (p, t) -> LspClient.NormalizePath p, t) |> Map.ofSeq

    let temp = Path.Combine(Path.GetTempPath(), "rfs-oracle-" + Guid.NewGuid().ToString("N"))

    try
        let visited = System.Collections.Generic.Dictionary<string, _>(StringComparer.OrdinalIgnoreCase)
        snd (checkWith temp visited project overrides)
    finally
        // A DLL the fresh checker still maps stays until the process ends
        try
            if Directory.Exists temp then
                Directory.Delete(temp, true)
        with _ ->
            ()

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
