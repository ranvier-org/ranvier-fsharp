namespace Rfs.Harness

open System
open System.IO
open System.Text.Json
open Rfs.Lsp

/// Toggle alternates the edit with its inverse, so every second state is the original text.
type EditMode =
    | Apply
    | Toggle

/// File paths are relative to the corpus repo root.
type Step =
    | Open of file: string
    | Edit of file: string * Edit * repeat: int * mode: EditMode
    | Save of string
    | Hover of string * Position
    | Completion of string * Position
    | Definition of string * Position
    | References of string * Position
    | OpenMany of string list

type Scenario =
    { Name: string
      Corpus: string
      Project: string
      /// One of cold, body, signature, requests, openMany.
      Kind: string
      Steps: Step list }

module Scenario =
    let private kinds = set [ "cold"; "body"; "signature"; "requests"; "openMany" ]

    let private fail message = raise (FormatException message)

    let private prop (name: string) (element: JsonElement) =
        match element.ValueKind with
        | JsonValueKind.Object ->
            match element.TryGetProperty name with
            | true, value -> value
            | _ -> fail $"missing property '%s{name}'"
        | kind -> fail $"expected an object with '%s{name}', got %O{kind}"

    let private tryProp (name: string) (element: JsonElement) =
        if element.ValueKind <> JsonValueKind.Object then
            fail $"expected an object with optional '%s{name}', got %O{element.ValueKind}"

        match element.TryGetProperty name with
        | true, value -> Some value
        | _ -> None

    let private asString (element: JsonElement) =
        if element.ValueKind = JsonValueKind.String then
            element.GetString()
        else
            fail "expected a string"

    let private str (name: string) (element: JsonElement) =
        match prop name element with
        | value when value.ValueKind = JsonValueKind.String -> value.GetString()
        | _ -> fail $"property '%s{name}' must be a string"

    let private asInt (what: string) (element: JsonElement) =
        if element.ValueKind <> JsonValueKind.Number then
            fail $"%s{what} must be an integer, got %O{element.ValueKind}"

        match element.TryGetInt32() with
        | true, value -> value
        | _ -> fail $"%s{what} must be an integer"

    let private position (element: JsonElement) : Position =
        { Line = prop "line" element |> asInt "line"
          Character = prop "character" element |> asInt "character" }

    let private editOf (element: JsonElement) : Edit =
        let range = prop "range" element

        { Range =
            { Start = prop "start" range |> position
              End = prop "end" range |> position }
          Text = str "text" element }

    let private mode (element: JsonElement) =
        match tryProp "mode" element |> Option.map asString with
        | None
        | Some "apply" -> Apply
        | Some "toggle" -> Toggle
        | Some other -> fail $"unknown edit mode '%s{other}'"

    let private located (build: string * Position -> Step) (element: JsonElement) =
        build (str "file" element, prop "position" element |> position)

    let private step (element: JsonElement) : Step =
        let properties =
            if element.ValueKind = JsonValueKind.Object then
                element.EnumerateObject() |> Seq.toList
            else
                []

        match properties with
        | [ p ] ->
            let value = p.Value

            match p.Name with
            | "open" -> Open(asString value)
            | "save" -> Save(asString value)
            | "openMany" ->
                if value.ValueKind <> JsonValueKind.Array then
                    fail "'openMany' must be an array of files"

                OpenMany [ for file in value.EnumerateArray() -> asString file ]
            | "edit" ->
                let repeat = tryProp "repeat" value |> Option.map (asInt "repeat") |> Option.defaultValue 1

                if repeat < 1 then
                    fail "'repeat' must be at least 1"

                Edit(str "file" value, editOf value, repeat, mode value)
            | "hover" -> located Hover value
            | "completion" -> located Completion value
            | "definition" -> located Definition value
            | "references" -> located References value
            | other -> fail $"unknown step '%s{other}'"
        | _ -> fail "a step must be an object with exactly one key"

    let private filesOf step =
        match step with
        | Open file
        | Save file
        | Edit(file, _, _, _)
        | Hover(file, _)
        | Completion(file, _)
        | Definition(file, _)
        | References(file, _) -> [ file ]
        | OpenMany files -> files

    let private positionsOf step =
        match step with
        | Edit(file, edit, _, _) -> [ file, edit.Range.Start; file, edit.Range.End ]
        | Hover(file, p)
        | Completion(file, p)
        | Definition(file, p)
        | References(file, p) -> [ file, p ]
        | Open _
        | Save _
        | OpenMany _ -> []

    /// A column may sit at the end of its line but not inside its terminator, as in Document.apply.
    let private checkPosition (lines: string[]) (file: string) (p: Position) =
        if p.Line < 0 || p.Line >= lines.Length then
            fail $"%s{file}: line %d{p.Line} is outside the file (%d{lines.Length} lines)"

        let line = lines[p.Line]
        let length = if line.EndsWith '\r' then line.Length - 1 else line.Length

        if p.Character < 0 || p.Character > length then
            fail $"%s{file}: column %d{p.Character} is past the end of line %d{p.Line} (length %d{length})"

    let private validate (corpusRoot: string) (scenario: Scenario) =
        let repo = Path.Combine(corpusRoot, scenario.Corpus)

        let resolve (relative: string) =
            let full = Path.Combine(repo, relative)

            if not (File.Exists full) then
                fail $"file not found: %s{relative} (looked in %s{full})"

            full

        resolve scenario.Project |> ignore
        let lineCache = Collections.Generic.Dictionary<string, string[]>()

        let linesOf file =
            match lineCache.TryGetValue file with
            | true, lines -> lines
            | _ ->
                let lines = File.ReadAllText(resolve file).Split('\n')
                lineCache[file] <- lines
                lines

        for s in scenario.Steps do
            for file in filesOf s do
                resolve file |> ignore

            for (file, p) in positionsOf s do
                checkPosition (linesOf file) file p

    let load (path: string) (corpusRoot: string) : Result<Scenario, string> =
        try
            use document = JsonDocument.Parse(File.ReadAllText path)
            let root = document.RootElement
            let kind = str "kind" root

            if not (kinds.Contains kind) then
                fail $"unknown kind '%s{kind}'"

            let steps = prop "steps" root

            if steps.ValueKind <> JsonValueKind.Array then
                fail "'steps' must be an array"

            let scenario =
                { Name = str "name" root
                  Corpus = str "corpus" root
                  Project = str "project" root
                  Kind = kind
                  Steps = [ for element in steps.EnumerateArray() -> step element ] }

            validate corpusRoot scenario
            Ok scenario
        with
        | :? FormatException as e -> Error $"%s{path}: %s{e.Message}"
        | :? JsonException as e -> Error $"%s{path}: invalid JSON: %s{e.Message}"
        | :? InvalidOperationException as e -> Error $"%s{path}: %s{e.Message}"
        | :? IOException as e -> Error $"%s{path}: %s{e.Message}"
