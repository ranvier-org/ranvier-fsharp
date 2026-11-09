module Rfs.Harness.Tests.ProjectSpecTests

open System
open System.IO
open Expecto
open Rfs.Projects

let private tinyDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures", "Tiny")
        if Directory.Exists candidate then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private tinyProject = Path.Combine(tinyDir, "Tiny.fsproj")
let private cachePath = Path.Combine(tinyDir, "obj", "rfs-spec.json")

type private TinyCopy(dir: string) =
    member _.Dir = dir
    member _.Project = Path.Combine(dir, "Tiny.fsproj")

    interface IAsyncDisposable with
        member _.DisposeAsync() =
            (try
                Directory.Delete(dir, true)
             with _ ->
                 ())

            Threading.Tasks.ValueTask()

let private copyTiny () =
    let dir = Path.Combine(Path.GetTempPath(), "rfs-tiny-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore

    for f in Directory.GetFiles tinyDir do
        File.Copy(f, Path.Combine(dir, Path.GetFileName f))

    new TinyCopy(dir)

[<Tests>]
let tests =
    testList
        "ProjectSpec"
        [ testTask "loads Tiny" {
              let! spec = ProjectSpec.load tinyProject

              Expect.equal
                  spec.SourceFiles
                  [ for f in [ "A.fs"; "B.fsi"; "B.fs"; "C.fs" ] -> Path.Combine(tinyDir, f) ]
                  "source files in compile order"

              let refs = spec.OtherOptions |> List.filter (fun o -> o.StartsWith "-r:")
              Expect.isTrue (refs |> List.exists (fun r -> r.Contains "FSharp.Core.dll")) "references FSharp.Core"
              Expect.equal spec.ProjectFile tinyProject "project file"
          }

          testTask "cache round-trip equals a fresh load" {
              let! first = ProjectSpec.load tinyProject
              let! second = ProjectSpec.load tinyProject
              Expect.equal second first "cached equals fresh"
              Expect.isTrue (File.Exists cachePath) "cache written under obj"
          }

          test "JSON round-trip keeps OtherOptions order and duplicates" {
              let spec =
                  { ProjectFile = "p.fsproj"
                    SourceFiles = [ "a.fs" ]
                    OtherOptions = [ "--z"; "--a"; "--z"; "-r:x.dll"; "-r:x.dll" ]
                    ReferencedProjects = []
                    ReferencedFrameworks = []
                    TargetFramework = ""
                    GeneratedFiles = [] }

              Expect.equal (ProjectSpec.ofJson (ProjectSpec.toJson spec)) spec "round trip"
          }

          // These two mutate the cache and the fsproj, so they work on a private copy of the fixture.
          testTask "stale cache is ignored when the fsproj is newer" {
              use copy = copyTiny ()
              let! first = ProjectSpec.load copy.Project
              let cache = Path.Combine(copy.Dir, "obj", "rfs-spec.json")
              File.WriteAllText(cache, (ProjectSpec.toJson { first with SourceFiles = [ "stale.fs" ] }))
              File.SetLastWriteTimeUtc(cache, DateTime.UtcNow.AddHours -1.0)
              File.SetLastWriteTimeUtc(copy.Project, DateTime.UtcNow)
              let! second = ProjectSpec.load copy.Project
              Expect.equal second first "stale cache ignored"
          }

          testTask "cache with a missing or different format version is ignored" {
              use copy = copyTiny ()
              let! first = ProjectSpec.load copy.Project
              let cache = Path.Combine(copy.Dir, "obj", "rfs-spec.json")
              let stale = { first with SourceFiles = [ "stale.fs" ] }

              for mutate in
                  [ (fun (j: string) -> j.Replace("\"FormatVersion\":5,", "\"FormatVersion\":0,"))
                    (fun j -> j.Replace("\"FormatVersion\":5,", "")) ] do
                  let json = mutate (ProjectSpec.toJson stale)
                  Expect.notEqual json (ProjectSpec.toJson stale) "mutation applied"
                  File.WriteAllText(cache, json)
                  File.SetLastWriteTimeUtc(cache, DateTime.UtcNow.AddMinutes 1.0)
                  let! again = ProjectSpec.load copy.Project
                  Expect.equal again first "reloaded"
          }

          testTask "a cache that cannot be written does not fail load" {
              use copy = copyTiny ()
              // a directory where the cache file belongs makes the final move fail
              Directory.CreateDirectory(Path.Combine(copy.Dir, "obj", "rfs-spec.json")) |> ignore
              let! spec = ProjectSpec.load copy.Project
              Expect.equal spec.SourceFiles.Length 4 "loaded anyway"

              let leftovers = Directory.GetFiles(Path.Combine(copy.Dir, "obj"), "*.tmp")
              Expect.isEmpty leftovers "temp file removed"
          }

          test "missingReferences names -r: paths that are not on disk" {
              let present = Path.Combine(tinyDir, "A.fs")
              let absent = Path.Combine(tinyDir, "obj", "nowhere", "Lib.dll")

              let spec =
                  { ProjectFile = "p.fsproj"
                    SourceFiles = []
                    OtherOptions = [ "--x"; $"-r:{present}"; $"-r:{absent}"; $"-r:{absent}" ]
                    ReferencedProjects = []
                    ReferencedFrameworks = []
                    TargetFramework = ""
                    GeneratedFiles = [] }

              Expect.equal (ProjectSpec.missingReferences spec) [ absent ] "only the absent one, once"
          }

          test "sequential makes --parallelcompilation- the last word" {
              let spec =
                  { ProjectFile = "p.fsproj"
                    SourceFiles = []
                    OtherOptions = [ "--a"; "--parallelcompilation+"; "--b" ]
                    ReferencedProjects = []
                    ReferencedFrameworks = []
                    TargetFramework = ""
                    GeneratedFiles = [] }

              Expect.equal (ProjectSpec.sequential spec).OtherOptions [ "--a"; "--b"; "--parallelcompilation-" ] "forced off"

              Expect.equal
                  (ProjectSpec.sequential { spec with OtherOptions = [ "--a" ] }).OtherOptions
                  [ "--a"; "--parallelcompilation-" ]
                  "added when absent"
          }

          testTask "load fails naming the output of a referenced project that was never built" {
              use copy = copyTiny ()
              // The library sits in a subdirectory of the copy and is never built.
              let libDir = Path.Combine(copy.Dir, "Lib")
              Directory.CreateDirectory libDir |> ignore
              File.WriteAllText(Path.Combine(libDir, "Lib.fs"), "module Lib\nlet x = 1\n")

              File.WriteAllText(
                  Path.Combine(libDir, "Lib.fsproj"),
                  """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup>
  <ItemGroup><Compile Include="Lib.fs" /></ItemGroup>
</Project>"""
              )

              File.WriteAllText(
                  copy.Project,
                  File.ReadAllText(copy.Project).Replace("</Project>", "  <ItemGroup><ProjectReference Include=\"Lib/Lib.fsproj\" /></ItemGroup>\n</Project>")
              )

              let! outcome = ProjectSpec.load copy.Project |> Async.AwaitTask |> Async.Catch

              match outcome with
              | Choice1Of2 _ -> failtest "load succeeded although Lib was never built"
              | Choice2Of2 e ->
                  Expect.stringContains e.Message "Lib.dll" "names the missing assembly"
                  Expect.stringContains e.Message "Tiny.fsproj" "names the project"
          }

          testTask "loads the corpus projects" {
              let repoRoot = DirectoryInfo(tinyDir).Parent.Parent.Parent
              let corpusRoot = Path.Combine(repoRoot.Parent.FullName, "ranvier-fsharp-corpus")

              if not (Directory.Exists corpusRoot) then
                  skiptest $"corpus not fetched at {corpusRoot}; run build/fetch-corpus.ps1"

              let manifest =
                  System.Text.Json.Nodes.JsonNode.Parse(File.ReadAllText(Path.Combine(repoRoot.FullName, "corpus", "corpus.json")))

              for repo in manifest["repos"].AsArray() do
                  let name = repo["name"].GetValue<string>()
                  let project = Path.Combine(corpusRoot, name, repo["project"].GetValue<string>().Replace('/', Path.DirectorySeparatorChar))
                  let! spec = ProjectSpec.load project
                  printfn "corpus %s: %d source files, %d options" name spec.SourceFiles.Length spec.OtherOptions.Length
                  Expect.isGreaterThan spec.SourceFiles.Length 0 $"{name}: has source files"
                  Expect.isTrue (spec.SourceFiles |> List.forall File.Exists) $"{name}: sources exist"
                  Expect.isTrue (spec.OtherOptions |> List.exists (fun o -> o.StartsWith "-r:" && o.Contains "FSharp.Core.dll")) $"{name}: has -r: FSharp.Core"
          }

          test "toFcsOptions maps fields" {
              let spec =
                  { ProjectFile = "p.fsproj"
                    SourceFiles = [ "a.fs"; "b.fs" ]
                    OtherOptions = [ "--x"; "--x" ]
                    ReferencedProjects = []
                    ReferencedFrameworks = []
                    TargetFramework = ""
                    GeneratedFiles = [] }

              let o = ProjectSpec.toFcsOptions spec
              Expect.equal o.ProjectFileName "p.fsproj" "name"
              Expect.equal o.SourceFiles [| "a.fs"; "b.fs" |] "sources"
              Expect.equal o.OtherOptions [| "--x"; "--x" |] "options"
              Expect.isEmpty o.ReferencedProjects "no refs"
              Expect.isNone o.Stamp "no stamp"
          } ]
