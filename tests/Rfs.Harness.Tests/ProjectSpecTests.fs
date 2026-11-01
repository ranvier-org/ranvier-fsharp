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
                    ReferencedProjects = [] }

              Expect.equal (ProjectSpec.ofJson (ProjectSpec.toJson spec)) spec "round trip"
          }

          testTask "stale cache is ignored when the fsproj is newer" {
              let! first = ProjectSpec.load tinyProject
              File.WriteAllText(cachePath, """{"ProjectFile":"stale.fsproj","SourceFiles":[],"OtherOptions":[],"ReferencedProjects":[]}""")
              File.SetLastWriteTimeUtc(cachePath, DateTime.UtcNow.AddHours -1.0)
              File.SetLastWriteTimeUtc(tinyProject, DateTime.UtcNow)
              let! second = ProjectSpec.load tinyProject
              Expect.equal second first "stale cache ignored"
          }

          test "toFcsOptions maps fields" {
              let spec =
                  { ProjectFile = "p.fsproj"
                    SourceFiles = [ "a.fs"; "b.fs" ]
                    OtherOptions = [ "--x"; "--x" ]
                    ReferencedProjects = [] }

              let o = ProjectSpec.toFcsOptions spec
              Expect.equal o.ProjectFileName "p.fsproj" "name"
              Expect.equal o.SourceFiles [| "a.fs"; "b.fs" |] "sources"
              Expect.equal o.OtherOptions [| "--x"; "--x" |] "options"
              Expect.isEmpty o.ReferencedProjects "no refs"
              Expect.isNone o.Stamp "no stamp"
          } ]
