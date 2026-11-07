module Rfs.Decl.Tests.Common

open System
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization
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

                let fileResult = DeclarationChecker.FinishFile(start, declResults, parse)

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

let tagged (text: RichText) = text.Text

let tooltipText (ToolTipText elements) =
    [ for element in elements do
          match element with
          | ToolTipElement.Group items ->
              for item in items do
                  yield tagged item.MainDescription
                  yield! item.TypeMapping |> List.map tagged
                  yield! item.Remarks |> Option.map tagged |> Option.toList
          | ToolTipElement.CompositionError error -> yield "error: " + error
          | ToolTipElement.None -> yield "none" ]
    |> String.concat "\n"

let useText (u: FSharpSymbolUse) =
    $"%A{u.Range} %s{u.Symbol.FullName} definition=%b{u.IsFromDefinition}"

/// Up to five symbol uses spread over the file, at least `atLeast`, as a query at the end of each one's names gives
/// them: line, column, line text and names.
let positions (atLeast: int) (text: string) (uses: FSharpSymbolUse seq) =
    let lines = text.Split '\n' |> Array.map (fun line -> line.TrimEnd '\r')

    let candidates =
        uses
        |> Seq.choose (fun u ->
            let r = u.Range

            if r.StartLine <> r.EndLine then
                None
            else
                let line = lines[r.StartLine - 1]
                let names = line.Substring(r.StartColumn, r.EndColumn - r.StartColumn)

                if names.Length > 0 && names |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '.') then
                    Some(r.StartLine, r.EndColumn, line, List.ofArray (names.Split '.'))
                else
                    None)
        |> Seq.distinct
        |> Array.ofSeq

    Expect.isGreaterThanOrEqual candidates.Length atLeast $"the file has %d{atLeast} symbol uses to ask about"
    let count = min 5 candidates.Length
    Array.init count (fun i -> candidates[if count = 1 then 0 else i * (candidates.Length - 1) / (count - 1)])

/// The file's lazily built check results give the reference's symbol uses, and its tooltips and symbol uses at up to
/// five positions.
let expectLikeReference (atLeast: int) (text: string) (expected: FSharpCheckFileResults) (actual: FSharpCheckFileResults) (what: string) =
    let usesOf (results: FSharpCheckFileResults) =
        results.GetAllUsesOfAllSymbolsInFile() |> Seq.map useText |> Seq.sort |> List.ofSeq

    Expect.isNonEmpty (usesOf expected) $"%s{what}: the reference has symbol uses"
    Expect.sequenceEqual (usesOf actual) (usesOf expected) $"%s{what}: every symbol use"
    Expect.isSome actual.ImplementationFile $"%s{what}: the typed implementation file"

    for line, column, lineText, names in positions atLeast text (expected.GetAllUsesOfAllSymbolsInFile()) do
        let at = $"%s{what} at %d{line}:%d{column} %A{names}"
        let tip (results: FSharpCheckFileResults) = tooltipText (results.GetToolTip(line, column, lineText, names, FSharpTokenTag.Identifier))
        Expect.equal (tip actual) (tip expected) $"%s{at}: the tooltip"
        let useAt (results: FSharpCheckFileResults) = results.GetSymbolUseAtLocation(line, column, lineText, names) |> Option.map useText
        Expect.isSome (useAt expected) $"%s{at}: the reference resolves it"
        Expect.equal (useAt actual) (useAt expected) $"%s{at}: the symbol use"

