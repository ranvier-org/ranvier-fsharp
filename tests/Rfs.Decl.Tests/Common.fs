module Rfs.Decl.Tests.Common

open System
open System.IO
open System.Threading
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices
open Rfs.Lsp
open Rfs.Projects

let tinyDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures", "Tiny")
        if Directory.Exists candidate then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let tinyProject = Path.Combine(tinyDir, "Tiny.fsproj")

/// The project's sources with some files replaced by in-memory text.
let sources (spec: ProjectSpec) (overrides: Map<string, string>) =
    spec.SourceFiles
    |> List.map (fun f ->
        f,
        match overrides |> Map.tryFind (Path.GetFileName f) with
        | Some text -> text
        | None -> File.ReadAllText f)
    |> Map.ofList

let oracle (spec: ProjectSpec) (texts: Map<string, string>) (file: string) =
    async {
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
        let sourceReader f = let t = texts[f] in hash t, lazy t
        let! _, checkResults, _ = checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, sourceReader, file)
        return checkResults.Diagnostics
    }

type CheckedFile =
    { File: string
      Start: FileStart
      Env0: DeclEnv
      Declarations: ParsedDeclaration[]
      Results: DeclarationResult[]
      FileResult: FileResult }

/// Checks the files in project order, each implementation one declaration at a time.
let declByDecl (ctx: ProjectContext) (spec: ProjectSpec) (texts: Map<string, string>) =
    let results, _ =
        (([], []), spec.SourceFiles)
        ||> List.fold (fun (results, before) file ->
            let parse = DeclarationChecker.Parse(ctx, file, texts[file])

            if file.EndsWith ".fsi" then
                results, before @ [ DeclarationChecker.CheckSignatureFile(ctx, before, parse) ]
            else
                let start, env0 = DeclarationChecker.BeginFile(ctx, before, parse)
                let decls = DeclarationChecker.Split parse

                let declResults =
                    decls
                    |> Array.mapFold
                        (fun env decl ->
                            let result = DeclarationChecker.CheckDeclaration(start, env, decl, CancellationToken.None)
                            result, result.EnvAfter)
                        env0
                    |> fst

                let fileResult = DeclarationChecker.FinishFile(start, declResults)

                let checkedFile =
                    { File = file
                      Start = start
                      Env0 = env0
                      Declarations = decls
                      Results = declResults
                      FileResult = fileResult }

                results @ [ checkedFile ], before @ [ fileResult.Signature ])

    results

let toLsp (diagnostics: FSharpDiagnostic[]) =
    diagnostics |> Array.map FcsDiagnostics.ofFcs |> List.ofArray
