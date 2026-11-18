module Rfs.Agent.Tests.SemanticEngineTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open Expecto
open Rfs.Agent
open Rfs.Harness

let private findRepoRoot () =
    let rec walk (dir: DirectoryInfo) =
        if isNull dir then
            failtest "Could not locate RanvierFSharp.slnx from AppContext.BaseDirectory."
        elif File.Exists(Path.Combine(dir.FullName, "RanvierFSharp.slnx")) then
            dir.FullName
        else
            walk dir.Parent

    walk (DirectoryInfo AppContext.BaseDirectory)

let private copyMultiTemp () =
    let repoRoot = findRepoRoot ()
    let multiDir = Path.Combine(repoRoot, "tests", "fixtures", "Multi")
    let tempRoot = Path.Combine(Path.GetTempPath(), "rfs-agent-se-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory tempRoot |> ignore
    Directory.CreateDirectory(Path.Combine(tempRoot, ".git")) |> ignore

    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(tempRoot, "global.json"))

    for cfg in [ "Directory.Build.props"; "Directory.Packages.props" ] do
        File.Copy(Path.Combine(multiDir, cfg), Path.Combine(tempRoot, cfg))

    for sub in [ "Lib"; "App" ] do
        let srcDir = Path.Combine(multiDir, sub)
        let dstDir = Path.Combine(tempRoot, sub)
        Directory.CreateDirectory dstDir |> ignore

        for f in Directory.GetFiles srcDir do
            File.Copy(f, Path.Combine(dstDir, Path.GetFileName f))

    tempRoot

/// A workspace whose only project targets a framework no installed SDK supports, so it fails to load on every machine.
let private unloadableWorkspace () =
    let tempRoot = Path.Combine(Path.GetTempPath(), "rfs-agent-bad-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory tempRoot |> ignore
    Directory.CreateDirectory(Path.Combine(tempRoot, ".git")) |> ignore

    File.WriteAllText(
        Path.Combine(tempRoot, "Bad.fsproj"),
        """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net99.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Program.fs" />
  </ItemGroup>
</Project>
"""
    )

    File.WriteAllText(Path.Combine(tempRoot, "Program.fs"), "let x: int = \"not an int\"\nprintfn \"%d\" x\n")
    tempRoot

let private emptyWorkspace () =
    let tempRoot = Path.Combine(Path.GetTempPath(), "rfs-agent-empty-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory tempRoot |> ignore
    Directory.CreateDirectory(Path.Combine(tempRoot, ".git")) |> ignore
    tempRoot

let private deleteTemp (dir: string) =
    try
        if Directory.Exists dir then
            Directory.Delete(dir, true)
    with _ ->
        ()

[<Tests>]
let tests =
    testSequenced (
        testList "SemanticEngine" [
            testTask "disk sync catches cross-project errors, promotes edited files, and runs symbol/refs/rename" {
                let root = copyMultiTemp ()

                try
                    let appProj = Path.Combine(root, "App", "App.fsproj")
                    let libFs = Path.Combine(root, "Lib", "Lib.fs")
                    let useFs = Path.Combine(root, "App", "Use.fs")
                    Fixtures.build appProj

                    let engine = new SemanticEngine(root)

                    try
                        do! engine.EnsureLoadedAsync()

                        // 1. Clean baseline check
                        match! engine.CheckAsync(None, None, "warning", true, CancellationToken.None) with
                        | Error e -> failtest $"Expected clean check, got Error: {e}"
                        | Ok txt ->
                            Expect.stringStarts txt "0 errors, 0 warnings (checked 2 project(s), " "clean baseline"
                            Expect.stringContains txt "file(s), the rest unchanged)" "says what was rechecked"

                        // 2. Break Lib.f signature on disk -> downstream App/Use.fs error surfaced immediately
                        let origLib = File.ReadAllText libFs
                        let brokenLib = origLib.Replace("let f (x: int) : int = x + 1", "let f (x: string) : int = x.Length")
                        Expect.notEqual brokenLib origLib "signature replacement applied"
                        File.WriteAllText(libFs, brokenLib)

                        match! engine.CheckAsync(Some libFs, None, "error", true, CancellationToken.None) with
                        | Error e -> failtest $"CheckAsync failed: {e}"
                        | Ok txt ->
                            Expect.stringContains txt "Use.fs" "reports downstream error in Use.fs"
                            Expect.stringContains txt "error FS0001" "reports type mismatch FS0001"

                        // 3. Restore Lib.fs and confirm recovery + declaration-incremental promotion on body edit
                        File.WriteAllText(libFs, origLib)

                        match! engine.CheckAsync(Some useFs, None, "warning", true, CancellationToken.None) with
                        | Error e -> failtest $"Restore check failed: {e}"
                        | Ok txt -> Expect.stringContains txt "0 errors" "restored clean"

                        let origUse = File.ReadAllText useFs
                        let editedUse1 = origUse.Replace("let w = y + 1", "let w = y + 10")
                        File.WriteAllText(useFs, editedUse1)

                        match! engine.CheckAsync(Some useFs, None, "warning", true, CancellationToken.None) with
                        | Error e -> failtest $"Body edit 1 check failed: {e}"
                        | Ok txt -> Expect.stringContains txt "0 errors" "edit 1 clean"

                        let editedUse2 = editedUse1.Replace("let w = y + 10", "let w = y + 20")
                        File.WriteAllText(useFs, editedUse2)

                        match! engine.CheckAsync(Some useFs, None, "warning", true, CancellationToken.None) with
                        | Error e -> failtest $"Body edit 2 check failed: {e}"
                        | Ok txt -> Expect.stringContains txt "0 errors" "edit 2 clean"

                        let! (status: JsonObject) = engine.StatusJson(false, 1234)
                        let promoted = status["promotedFiles"].GetValue<int>()
                        let declChecks = status["declCheckedCount"].GetValue<int64>()
                        Expect.isGreaterThanOrEqual promoted 1 "edited files promoted to declaration checking"
                        Expect.isGreaterThan declChecks 0L "declaration-level checks recorded"

                        // 4. Symbol inspection
                        match! engine.SymbolAsync(useFs, 11, None, Some "shapeOf", CancellationToken.None) with
                        | Error e -> failtest $"SymbolAsync failed: {e}"
                        | Ok txt ->
                            Expect.stringContains txt "unit -> Lib.Shapes.Shape" "exact inferred signature"
                            Expect.stringContains txt "Lib.fs" "definition points to Lib.fs"

                        // 5. Cross-project references
                        match! engine.ReferencesAsync(libFs, 9, None, Some "f", "all", 50, CancellationToken.None) with
                        | Error e -> failtest $"ReferencesAsync failed: {e}"
                        | Ok txt ->
                            Expect.stringContains txt "Lib.fs" "includes definition in Lib.fs"
                            Expect.stringContains txt "Use.fs" "includes cross-project use in Use.fs"

                        // 6. Workspace symbol search
                        match! engine.SearchAsync(Some "shapeOf", None, 20, CancellationToken.None) with
                        | Error e -> failtest $"SearchAsync failed: {e}"
                        | Ok txt -> Expect.stringContains txt "shapeOf" "finds shapeOf in workspace symbols"

                        // 7. Semantic multi-file rename & clean verification
                        match! engine.RenameAsync(libFs, 13, "makeShape", None, Some "shapeOf", true, CancellationToken.None) with
                        | Error e -> failtest $"RenameAsync failed: {e}"
                        | Ok txt ->
                            Expect.stringContains txt "0 compiler error(s)" "rename leaves 0 errors"
                            Expect.stringContains (File.ReadAllText libFs) "makeShape" "Lib.fs updated"
                            Expect.stringContains (File.ReadAllText useFs) "makeShape" "Use.fs updated"
                    finally
                        (engine :> IDisposable).Dispose()
                finally
                    deleteTemp root
            }

            testTask "explicit reload resolves a newly restored dependency in a loaded project" {
                let root = copyMultiTemp ()
                try
                    let project = Path.Combine(root, "App", "App.fsproj")
                    let xml = File.ReadAllText project
                    Fixtures.build project
                    File.WriteAllText(project, xml.Replace("""<ProjectReference Include="..\Lib\Lib.fsproj" />""", ""))
                    let engine = new SemanticEngine(root)
                    try
                        match! engine.CheckAsync(None, None, "error", true, CancellationToken.None) with
                        | Ok report -> Expect.stringContains report "error FS0039" "missing library is diagnosed"
                        | Error e -> failtest e

                        File.WriteAllText(project, xml)
                        Fixtures.build project
                        let! _ = engine.StatusJson(true, 1234)
                        match! engine.CheckAsync(None, None, "warning", true, CancellationToken.None) with
                        | Ok report -> Expect.stringStarts report "0 errors, 0 warnings (checked 2 project(s)" "reload acquired the reference"
                        | Error e -> failtest e
                    finally
                        (engine :> IDisposable).Dispose()
                finally
                    deleteTemp root
            }

            testTask "ordinary checks follow solution membership and wildcard source additions" {
                let root = copyMultiTemp ()
                try
                    let solution = Path.Combine(root, "Example.slnx")
                    let project = Path.Combine(root, "App", "App.fsproj")
                    let writeSolution projects = File.WriteAllText(solution, "<Solution>" + projects + "</Solution>")
                    let lib = """<Project Path="Lib/Lib.fsproj" />"""
                    let app = """<Project Path="App/App.fsproj" />"""
                    let xml = File.ReadAllText project
                    let includeUse = """<Compile Include="Use.fs" />"""
                    let includePages = """<Compile Include="Pages/**/*.fs" />"""
                    File.WriteAllText(project, xml.Replace(includeUse, includeUse + includePages))
                    Fixtures.build project
                    writeSolution lib
                    let engine = new SemanticEngine(root)
                    let expect text =
                        task {
                            match! engine.CheckAsync(None, None, "warning", true, CancellationToken.None) with
                            | Ok report -> Expect.stringContains report text "disk membership reflected by a workspace check"
                            | Error e -> failtest e
                        }
                    try
                        do! expect "0 errors, 0 warnings (checked 1 project(s)"
                        writeSolution (lib + app)
                        do! expect "0 errors, 0 warnings (checked 2 project(s)"
                        let pages = Directory.CreateDirectory(Path.Combine(root, "App", "Pages", "Nested"))
                        let source = Path.Combine(pages.FullName, "New.fs")
                        File.WriteAllText(source, "module App.New\nlet n: int = false\n")
                        do! expect "error FS0001"
                        File.WriteAllText(source, "module App.New\nlet n: int = 1\n")
                        do! expect "0 errors, 0 warnings (checked 2 project(s)"
                        File.Delete source
                        do! expect "0 errors, 0 warnings (checked 2 project(s)"
                        let explicitSource = Path.Combine(root, "App", "Explicit.fs")
                        File.WriteAllText(explicitSource, "module App.Explicit\nlet value: int = false\n")
                        do! expect "0 errors, 0 warnings (checked 2 project(s)"
                        let withExplicit = File.ReadAllText(project).Replace(includePages, """<Compile Include="Explicit.fs" />""")
                        File.WriteAllText(project, withExplicit)
                        do! expect "error FS0001"
                        writeSolution lib
                        do! expect "0 errors, 0 warnings (checked 1 project(s)"
                    finally
                        (engine :> IDisposable).Dispose()
                finally
                    deleteTemp root
            }

            testTask "a project that fails to load makes check incomplete, never a pass" {
                let root = unloadableWorkspace ()

                try
                    let engine = new SemanticEngine(root)

                    try
                        match! engine.CheckAsync(None, None, "warning", true, CancellationToken.None) with
                        | Error e -> failtest $"Expected an incomplete report, got Error: {e}"
                        | Ok(txt: string) ->
                            let lines: string[] = txt.Replace("\r\n", "\n").Split('\n')
                            let summary: string = Array.last lines
                            Expect.isFalse (summary.StartsWith "0 errors") $"summary must not read as a pass: {summary}"
                            Expect.stringStarts summary "incomplete: 1 project(s) failed to load" "summary leads with the failure"
                            Expect.stringContains txt "project-load-failed Bad.fsproj" "names the project"

                            for line in lines do
                                Expect.isLessThan line.Length 700 "every line stays readable"
                    finally
                        (engine :> IDisposable).Dispose()
                finally
                    deleteTemp root
            }

            testTask "a workspace without projects is an error, not a pass" {
                let root = emptyWorkspace ()

                try
                    let engine = new SemanticEngine(root)

                    try
                        match! engine.CheckAsync(None, None, "warning", true, CancellationToken.None) with
                        | Ok txt -> failtest $"Expected an error for an empty workspace, got: {txt}"
                        | Error e -> Expect.stringContains e "No F# project" "says what is missing"
                    finally
                        (engine :> IDisposable).Dispose()
                finally
                    deleteTemp root
            }
        ]
    )
