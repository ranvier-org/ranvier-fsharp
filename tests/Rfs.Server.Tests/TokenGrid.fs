module Rfs.Server.Tests.TokenGrid

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open Rfs.Checker
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests.PublishTests

/// A small project and the edits it goes through. What a feature answers is compared with a from-scratch check at every
/// token of every implementation file, in every state: which answers go stale depends on which row kept what, so
/// hand-picked positions miss them.
type Scenario =
    { Name: string
      /// Each file's name and first text, in file order
      Files: (string * string) list
      /// Each step's name and the files it gives new texts
      Steps: (string * (string * string) list) list }

/// A token: its 0-based line, a 0-based character inside it, and what `Hover.lookupOf` asks a check for
type Token =
    { Line: int
      Character: int
      LineText: string
      Names: string list
      EndColumn: int }

/// One state of a scenario, settled, with the from-scratch check of each implementation file for the same texts
type State =
    { Scenario: string
      Step: string
      Graph: ProjectGraph
      /// In file order, signature files too
      Files: string list
      Implementations: string list
      TextOf: string -> string
      /// The from-scratch check of an implementation file, made once
      Reference: string -> Task<FSharpCheckFileResults>
      /// The graphs that hold a file, as `Workspace.OwnersOf` gives them
      OwnersOf: string -> ProjectGraph list }

let private wait (t: Task<'a>) = t.WaitAsync(TimeSpan.FromSeconds 110.)

/// Each token of a text once, however many characters it has
let tokensOf (text: string) : Token list =
    let lines = text.Split '\n'
    let seen = HashSet<struct (int * int)>()

    [ for l in 0 .. lines.Length - 1 do
          for c in 0 .. lines[l].Length do
              match Hover.lookupOf text l c with
              | Some(lineText, names, endColumn) when seen.Add(struct (l, endColumn)) ->
                  { Line = l
                    Character = c
                    LineText = lineText
                    Names = names
                    EndColumn = endColumn }
              | _ -> () ]

/// Runs the scenario, by declaration when `promote` and file level otherwise, and collects what `atState` says of the
/// first text and of the text after each step.
let run (promote: bool) (scenario: Scenario) (atState: State -> Task<'T list>) : Task<'T list> =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-token-grid-"

        try
            let paths = scenario.Files |> List.map (fun (n, _) -> n, Path.Combine(dir.FullName, n)) |> Map.ofList

            for n, t in scenario.Files do
                File.WriteAllText(paths[n], t)

            let files = scenario.Files |> List.map (fun (n, _) -> paths[n])
            let implementations = files |> List.filter (fun f -> not (f.EndsWith ".fsi"))
            let! deps = ProjectSpec.load depsProject

            let spec =
                { ProjectSpec.sequential deps with
                    ProjectFile = Path.Combine(dir.FullName, "P.fsproj")
                    SourceFiles = files }

            use host = GraphHost.Start "token grid"
            let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec)

            let promoted () =
                task {
                    let! _ = graph.AllDiagnostics() |> wait

                    if promote then
                        for f in implementations do
                            let! _ = graph.WhenPromoted(f, CancellationToken.None) |> wait
                            ()
                }

            if promote then
                for f in implementations do
                    graph.Promote f

            do! promoted ()
            let mutable texts = scenario.Files |> List.map (fun (n, t) -> paths[n], t) |> Map.ofList
            let results = ResizeArray<'T>()

            let held (file: string) =
                if List.exists (fun f -> String.Equals(f, file, StringComparison.OrdinalIgnoreCase)) files then [ graph ] else []

            let compareAt (step: string) =
                task {
                    let current = texts
                    let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
                    let reader (f: string) = (let t = current[f] in hash t, lazy t)
                    let checks = Dictionary<string, FSharpCheckFileResults>()

                    let reference (file: string) =
                        task {
                            match checks.TryGetValue file with
                            | true, c -> return c
                            | _ ->
                                let! _, c, _ =
                                    checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList files, reader, file)
                                    |> Async.StartAsTask

                                checks[file] <- c
                                return c
                        }

                    let! found =
                        atState
                            { Scenario = scenario.Name
                              Step = step
                              Graph = graph
                              Files = files
                              Implementations = implementations
                              TextOf = fun f -> current[f]
                              Reference = reference
                              OwnersOf = held }

                    results.AddRange found
                }

            do! compareAt "as checked"

            for step, changes in scenario.Steps do
                for n, t in changes do
                    texts <- texts.Add(paths[n], t)
                    graph.SetText(paths[n], t) |> ignore

                do! promoted ()
                do! compareAt step

            graph.Dispose()
            return List.ofSeq results
        finally
            try
                dir.Delete true
            with :? IOException | :? UnauthorizedAccessException ->
                ()
    }
