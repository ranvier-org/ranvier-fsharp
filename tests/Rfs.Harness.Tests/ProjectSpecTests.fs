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
let private cachePath = ProjectSpec.cachePath tinyProject

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
                    GeneratedFiles = []
                    Items = [] }

              Expect.equal (ProjectSpec.ofJson (ProjectSpec.toJson spec)) spec "round trip"
          }

          // These two mutate the cache and the fsproj, so they work on a private copy of the fixture.
          testTask "stale cache is ignored when the fsproj is newer" {
              use copy = copyTiny ()
              let! first = ProjectSpec.load copy.Project
              let cache = ProjectSpec.cachePath copy.Project
              File.WriteAllText(cache, (ProjectSpec.toJson { first with SourceFiles = [ "stale.fs" ] }))
              File.SetLastWriteTimeUtc(cache, DateTime.UtcNow.AddHours -1.0)
              File.SetLastWriteTimeUtc(copy.Project, DateTime.UtcNow)
              let! second = ProjectSpec.load copy.Project
              Expect.equal second first "stale cache ignored"
          }

          testTask "cache with a missing or different format version is ignored" {
              use copy = copyTiny ()
              let! first = ProjectSpec.load copy.Project
              let cache = ProjectSpec.cachePath copy.Project
              let stale = { first with SourceFiles = [ "stale.fs" ] }

              for mutate in
                  [ (fun (j: string) -> j.Replace("\"FormatVersion\":7,", "\"FormatVersion\":0,"))
                    (fun j -> j.Replace("\"FormatVersion\":7,", "")) ] do
                  let json = mutate (ProjectSpec.toJson stale)
                  Expect.notEqual json (ProjectSpec.toJson stale) "mutation applied"
                  File.WriteAllText(cache, json)
                  File.SetLastWriteTimeUtc(cache, DateTime.UtcNow.AddMinutes 1.0)
                  let! again = ProjectSpec.load copy.Project
                  Expect.equal again first "reloaded"
          }

          // I5. A marked cache shows whether the next load read it or evaluated the project.
          testTask "a change to what the evaluation read is noticed across sessions" {
              use copy = copyTiny ()
              let cache = ProjectSpec.cachePath copy.Project

              let mark () =
                  File.WriteAllText(cache, File.ReadAllText(cache).Replace("C.fs", "Marked.fs"))
                  File.SetLastWriteTimeUtc(cache, DateTime.UtcNow)

              let readsCache () =
                  task {
                      let! spec = ProjectSpec.load copy.Project
                      return spec.SourceFiles |> List.exists (fun f -> f.EndsWith "Marked.fs")
                  }

              let later (path: string) (text: string) =
                  File.WriteAllText(path, text)
                  File.SetLastWriteTimeUtc(path, DateTime.UtcNow.AddMinutes 1.0)

              let shared = Path.Combine(copy.Dir, "shared.props")
              File.WriteAllText(shared, "<Project />")
              File.WriteAllText(copy.Project, File.ReadAllText(copy.Project).Replace("</Project>", "<Import Project=\"shared.props\" /></Project>"))
              let! _ = ProjectSpec.load copy.Project
              mark ()
              let! untouched = readsCache ()
              Expect.isTrue untouched "nothing changed: the cache is read"

              later shared "<Project><PropertyGroup><DefineConstants>FROM_SHARED</DefineConstants></PropertyGroup></Project>"
              let! imported = readsCache ()
              Expect.isFalse imported "an imported file changed"
              let! spec = ProjectSpec.load copy.Project
              Expect.contains spec.OtherOptions "--define:FROM_SHARED" "with the new options"

              // Made after the evaluation, so nothing read it
              File.SetLastWriteTimeUtc(shared, DateTime.UtcNow.AddMinutes -1.0)
              let! _ = ProjectSpec.load copy.Project
              File.SetLastWriteTimeUtc(cache, DateTime.UtcNow)
              mark ()
              later (Path.Combine(copy.Dir, "Directory.Build.props")) "<Project><PropertyGroup><DefineConstants>FROM_DIRECTORY</DefineConstants></PropertyGroup></Project>"
              let! made = readsCache ()
              Expect.isFalse made "a Directory.Build.props made since"
          }

          testTask "the cache is fresh in the artifacts layout, until its assets file changes" {
              use copy = copyTiny ()
              File.WriteAllText(Path.Combine(copy.Dir, "Directory.Build.props"), "<Project><PropertyGroup><UseArtifactsOutput>true</UseArtifactsOutput></PropertyGroup></Project>")
              let cache = ProjectSpec.cachePath copy.Project
              let! _ = ProjectSpec.load copy.Project
              Expect.isFalse (File.Exists(Path.Combine(copy.Dir, "obj", "project.assets.json"))) "the assets file is elsewhere"
              let assets = Path.Combine(copy.Dir, "artifacts", "obj", "Tiny", "project.assets.json")
              Expect.isTrue (File.Exists assets) "under artifacts"

              let mark () =
                  File.WriteAllText(cache, File.ReadAllText(cache).Replace("C.fs", "Marked.fs"))
                  File.SetLastWriteTimeUtc(cache, DateTime.UtcNow)

              mark ()
              let! fresh = ProjectSpec.load copy.Project
              Expect.isTrue (fresh.SourceFiles |> List.exists (fun f -> f.EndsWith "Marked.fs")) "the cache is read"

              File.SetLastWriteTimeUtc(assets, DateTime.UtcNow.AddMinutes 1.0)
              let! changed = ProjectSpec.load copy.Project
              Expect.isFalse (changed.SourceFiles |> List.exists (fun f -> f.EndsWith "Marked.fs")) "evaluated again"
          }

          testTask "a cache that cannot be written does not fail load" {
              use copy = copyTiny ()
              // a directory where the cache file belongs makes the final move fail
              Directory.CreateDirectory(ProjectSpec.cachePath copy.Project) |> ignore
              let! spec = ProjectSpec.load copy.Project
              Expect.equal spec.SourceFiles.Length 4 "loaded anyway"

              let leftovers = Directory.GetFiles(Path.Combine(copy.Dir, "obj"), "*.tmp")
              Expect.isEmpty leftovers "temp file removed"
          }

          // The server picks its SDK itself: an exe outside the repo has no global.json pinning this runtime's SDK above it,
          // and the highest installed SDK may be of a major version this process can't load.
          testTask "loads from an exe outside the repo when a higher-major SDK is what a global.json above it names" {
              let dotnetSdks =
                  let psi = Diagnostics.ProcessStartInfo("dotnet", "--list-sdks", RedirectStandardOutput = true)
                  let p = Diagnostics.Process.Start psi
                  let text = p.StandardOutput.ReadToEnd()
                  p.WaitForExit()
                  [ for line in text.Split('\n', StringSplitOptions.RemoveEmptyEntries) -> line.Split(' ').[0] ]

              let higher =
                  dotnetSdks |> List.filter (fun v -> int (v.Split('.').[0]) > Environment.Version.Major)

              if higher.IsEmpty then
                  skiptest $"no SDK above %d{Environment.Version.Major} is installed, so there is nothing to choose wrongly"

              let root = Path.Combine(Path.GetTempPath(), "rfs-outside-" + Guid.NewGuid().ToString("N"))
              let bin = Path.Combine(root, "bin")
              let fixture = Path.Combine(root, "tests", "fixtures", "Tiny")

              try
                  Directory.CreateDirectory fixture |> ignore

                  for f in Directory.GetFiles tinyDir do
                      File.Copy(f, Path.Combine(fixture, Path.GetFileName f))

                  let src = AppContext.BaseDirectory.TrimEnd(Path.DirectorySeparatorChar)

                  for f in Directory.GetFiles(src, "*", SearchOption.AllDirectories) do
                      let target = Path.Combine(bin, Path.GetRelativePath(src, f))
                      Directory.CreateDirectory(Path.GetDirectoryName target) |> ignore
                      File.Copy(f, target)

                  File.WriteAllText(
                      Path.Combine(root, "global.json"),
                      $"""{{ "sdk": {{ "version": "%s{List.head higher}", "rollForward": "disable", "allowPrerelease": true }} }}"""
                  )

                  let psi =
                      Diagnostics.ProcessStartInfo(
                          "dotnet",
                          "\"" + Path.Combine(bin, "Rfs.Harness.Tests.dll") + "\" --filter-test-case \"loads Tiny\"",
                          WorkingDirectory = root,
                          RedirectStandardOutput = true,
                          RedirectStandardError = true
                      )

                  // What an earlier load in this process set up would otherwise decide the child's MSBuild
                  for v in [ "MSBUILD_EXE_PATH"; "MSBuildExtensionsPath"; "MSBuildSDKsPath"; "DOTNET_HOST_PATH"; "DOTNET_ROOT" ] do
                      psi.Environment.Remove v |> ignore

                  let p = Diagnostics.Process.Start psi
                  let stdout = p.StandardOutput.ReadToEndAsync()
                  let stderr = p.StandardError.ReadToEndAsync()

                  if not (p.WaitForExit(TimeSpan.FromMinutes 4.0)) then
                      p.Kill true
                      failtest "the child did not finish"

                  let output = Text.RegularExpressions.Regex.Replace(stdout.Result + stderr.Result, "\u001b\\[[0-9;?]*[a-zA-Z]", "")
                  Expect.equal p.ExitCode 0 $"the load succeeded outside the repo:\n%s{output}"
                  Expect.stringContains output "1 passed" "and it ran the test"
              finally
                  try
                      Directory.Delete(root, true)
                  with _ ->
                      ()
          }

          // N5. Another version of the code on the same obj keeps its own cache, and neither evicts the other's.
          testTask "the cache's file name holds the format version, and another version's cache is left alone" {
              use copy = copyTiny ()
              let objDir = Path.Combine(copy.Dir, "obj")
              Directory.CreateDirectory objDir |> ignore
              let other = Path.Combine(objDir, "rfs-spec.v0.json")
              File.WriteAllText(other, "{}")
              let! _ = ProjectSpec.load copy.Project
              let cache = ProjectSpec.cachePath copy.Project
              Expect.isTrue (Path.GetFileName(cache).StartsWith "rfs-spec.v") "named for its version"
              Expect.isTrue (File.Exists cache) "written"
              Expect.equal (File.ReadAllText other) "{}" "the other version's cache is untouched"
              ProjectSpec.invalidate copy.Project
              Expect.isFalse (File.Exists cache) "invalidate drops this version's"
              Expect.isTrue (File.Exists other) "and not the other's"
          }

          test "the SDK is the highest of the runtime's major version, a release over a prerelease" {
              let installed = [ "8.0.427"; "10.0.114"; "10.0.403"; "10.0.401"; "11.0.100-rc.1.26425.128" ]
              Expect.equal (ProjectSpec.sdkForRuntime 10 installed) (Ok "10.0.403") "not the 11 prerelease"
              Expect.equal (ProjectSpec.sdkForRuntime 11 installed) (Ok "11.0.100-rc.1.26425.128") "a prerelease when it is all there is"
              Expect.equal (ProjectSpec.sdkForRuntime 10 [ "10.0.100-preview.1"; "10.0.100" ]) (Ok "10.0.100") "a release over a prerelease"
          }

          test "prereleases are ordered by semantic version, whatever the order they are listed in" {
              for order in [ [ "11.0.100-rc.1.1"; "11.0.100-rc.2.1" ]; [ "11.0.100-rc.2.1"; "11.0.100-rc.1.1" ] ] do
                  Expect.equal (ProjectSpec.sdkForRuntime 11 order) (Ok "11.0.100-rc.2.1") "rc.2 after rc.1"

              Expect.equal
                  (ProjectSpec.sdkForRuntime 11 [ "11.0.100-rc.10.1"; "11.0.100-rc.9.1" ])
                  (Ok "11.0.100-rc.10.1")
                  "numbers by value"

              Expect.equal (ProjectSpec.sdkForRuntime 11 [ "11.0.100-rc.1.1"; "11.0.100-preview.7" ]) (Ok "11.0.100-rc.1.1") "words in order"
          }

          testTask "many writers of the SDK's global.json at once all succeed and leave it right" {
              let dir = Path.Combine(Path.GetTempPath(), "rfs-sdk-race-" + Guid.NewGuid().ToString("N"))

              try
                  let writers =
                      [ for _ in 1..16 ->
                            Threading.Tasks.Task.Run(fun () ->
                                for _ in 1..20 do
                                    ProjectSpec.ensureGlobalJson dir "10.0.403") ]

                  do! Threading.Tasks.Task.WhenAll writers
                  Expect.stringContains (File.ReadAllText(Path.Combine(dir, "global.json"))) "10.0.403" "pins the version"
                  Expect.isEmpty (Directory.GetFiles(dir, "*.tmp")) "no temp files left"
              finally
                  try
                      Directory.Delete(dir, true)
                  with _ ->
                      ()
          }

          test "no SDK of the runtime's major version is an error naming the versions found" {
              match ProjectSpec.sdkForRuntime 10 [ "8.0.427"; "9.0.320"; "11.0.100-rc.1.26425.128" ] with
              | Ok v -> failtest $"chose %s{v}"
              | Error message ->
                  Expect.stringContains message "no .NET SDK 10" "says what is missing"
                  Expect.stringContains message "8.0.427, 9.0.320, 11.0.100-rc.1.26425.128" "lists what was found"

              Expect.isTrue (Result.isError (ProjectSpec.sdkForRuntime 10 [])) "none installed"
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
                    GeneratedFiles = []
                    Items = [] }

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
                    GeneratedFiles = []
                    Items = [] }

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
                    GeneratedFiles = []
                    Items = [] }

              let o = ProjectSpec.toFcsOptions spec
              Expect.equal o.ProjectFileName "p.fsproj" "name"
              Expect.equal o.SourceFiles [| "a.fs"; "b.fs" |] "sources"
              Expect.equal o.OtherOptions [| "--x"; "--x" |] "options"
              Expect.isEmpty o.ReferencedProjects "no refs"
              Expect.isNone o.Stamp "no stamp"
          } ]
