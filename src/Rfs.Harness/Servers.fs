namespace Rfs.Harness

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes

/// Strict fails the run on any oracle mismatch; Report records mismatches and carries on.
type OracleMode =
    | Strict
    | Report

type ServerConfig =
    { Name: string
      Command: string
      Args: string list
      Env: Map<string, string>
      /// Sent as `initializationOptions`, and again as the "FSharp" settings section after `initialized`.
      InitOptions: JsonNode
      Oracle: OracleMode
      /// A request that loads the scenario's project, sent after `initialize` with
      /// `{ "textDocuments": [ { "uri": <project> } ] }` (FSAC's `fsharp/workspaceLoad`); its time counts as cold.
      ProjectLoad: string option
      /// The server's working directory, absolute; None starts it in the scenario's corpus checkout. FSAC picks the
      /// SDK it loads projects with from the global.json its working directory sees.
      Cwd: string option }

module Servers =
    let private fail message = raise (FormatException message)

    let private server (node: JsonNode) : ServerConfig =
        let obj =
            match node with
            | :? JsonObject as o -> o
            | _ -> fail "a server must be an object"

        let text (name: string) =
            match obj[name] with
            | null -> fail $"server is missing '%s{name}'"
            | value -> value.GetValue<string>()

        { Name = text "name"
          Command = text "command"
          Args =
            match obj["args"] with
            | :? JsonArray as a ->
                [ for x in a ->
                      match x with
                      | null -> fail "'args' must not contain null"
                      | x -> x.GetValue<string>() ]
            | _ -> []
          Env =
            match obj["env"] with
            | :? JsonObject as e ->
                e
                |> Seq.map (fun kv ->
                    match kv.Value with
                    | null -> fail $"env value '%s{kv.Key}' must not be null"
                    | v -> kv.Key, v.GetValue<string>())
                |> Map.ofSeq
            | _ -> Map.empty
          InitOptions =
            match obj["initOptions"] with
            | null -> JsonObject() :> JsonNode
            | options -> options.DeepClone()
          Oracle =
            match obj["oracle"] with
            | null -> Strict
            | value ->
                match value.GetValue<string>() with
                | "strict" -> Strict
                | "report" -> Report
                | other -> fail $"unknown oracle mode '%s{other}'"
          ProjectLoad =
            match obj["projectLoad"] with
            | null -> None
            | value -> Some(value.GetValue<string>())
          Cwd =
            match obj["cwd"] with
            | null -> None
            | value -> Some(Path.GetFullPath(value.GetValue<string>())) }

    /// Reads a JSON array of servers.
    let load (path: string) : Result<ServerConfig list, string> =
        try
            match JsonNode.Parse(File.ReadAllText path) with
            | :? JsonArray as servers -> Ok [ for s in servers -> server s ]
            | _ -> Error $"%s{path}: expected a JSON array of servers"
        with
        | :? FormatException as e -> Error $"%s{path}: %s{e.Message}"
        | :? JsonException as e -> Error $"%s{path}: invalid JSON: %s{e.Message}"
        | :? InvalidOperationException as e -> Error $"%s{path}: %s{e.Message}"
        | :? IOException as e -> Error $"%s{path}: %s{e.Message}"
