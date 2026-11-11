module GlobAndProjectItemsTests

open System
open System.IO
open Expecto
open Rfs.Projects

type private TempDir(dir: string) =
    let cleanup () =
        try
            Directory.Delete(dir, true)
        with _ ->
            ()

    member _.Dir = dir

    interface IDisposable with
        member _.Dispose() = cleanup ()

    interface IAsyncDisposable with
        member _.DisposeAsync() =
            cleanup ()
            System.Threading.Tasks.ValueTask()

let private makeTempRoot () =
    let dir = Path.Combine(Path.GetTempPath(), "rfs-t6b-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    Directory.CreateDirectory dir |> ignore
    new TempDir(dir)

let private findRepoRoot () =
    let rec up (d: DirectoryInfo) =
        if isNull d then
            failwith "Could not locate RanvierFSharp.slnx from AppContext.BaseDirectory"
        else
            let candidate = Path.Combine(d.FullName, "RanvierFSharp.slnx")
            if File.Exists candidate then d.FullName else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private writeText (path: string) (text: string) =
    Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
    File.WriteAllText(path, text)

[<Tests>]
let globUnitTests =
    testList
        "Task6b.Glob"
        [ test "splitPatterns splits on unescaped semicolons, trims whitespace, unescapes %3B/%2A/%3F, and drops empties" {
              let raw = "  a.txt ; ; docs/**/*.md ; sub/x%3By.txt ; esc/%2A/%3F.txt ;   "
              let actual = Glob.splitPatterns raw
              Expect.equal actual [ "a.txt"; "docs/**/*.md"; "sub/x;y.txt"; "esc/*/*.txt".Replace("/*", "/*").Replace("*.txt", "?.txt") ] "split & unescaped"
              Expect.equal actual[3] "esc/*/?.txt" "exact %2A and %3F unescaping"
              Expect.isEmpty (Glob.splitPatterns " ;  ; ") "only whitespace and semicolons -> empty"
          }

          test "** matches zero and N directory segments, collapses **/**, degrades mid-segment **, and computes RecursiveDir" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              let p (rel: string) = Path.GetFullPath(Path.Combine(root, rel.Replace('/', Path.DirectorySeparatorChar)))

              let candidates =
                  [ p "Foo.txt"
                    p "a/b/Foo.txt"
                    p "docs/api/index.md"
                    p "docs/v1/sub/api/index.md"
                    p "docs/v1/other.md"
                    p "assets/logo.png"
                    p "assets/sub/deep/icon.svg"
                    p "other/logo.png"
                    p "foobar.txt"
                    p "foo123bar.txt"
                    p "foo/bar.txt" ]

              // **/Foo.txt matches BOTH 0 segments (Foo.txt) and 2 segments (a/b/Foo.txt)
              let fooMatches = Glob.expandCandidates true root "**/Foo.txt" None candidates
              Expect.equal
                  fooMatches
                  [ p "a/b/Foo.txt", "a/b/"
                    p "Foo.txt", "" ]
                  "**/Foo.txt matches both 0 and 2 segments with exact RecursiveDir"

              // docs/**/api/*.md matches both 0 intermediate segments and 2 intermediate segments
              let docMatches = Glob.expandCandidates true root "docs/**/api/*.md" None candidates
              Expect.equal
                  docMatches
                  [ p "docs/api/index.md", ""
                    p "docs/v1/sub/api/index.md", "v1/sub/" ]
                  "docs/**/api/*.md matches 0 and N intermediate segments"

              // assets/** matches every depth under assets/ and nothing outside assets/
              let assetMatches = Glob.expandCandidates true root "assets/**" None candidates
              Expect.equal
                  assetMatches
                  [ p "assets/logo.png", ""
                    p "assets/sub/deep/icon.svg", "sub/deep/" ]
                  "assets/** matches all depths under assets/"

              // Consecutive **/** collapses cleanly without exponential backtracking
              let deepPath = p "x/s1/s2/s3/s4/s5/s6/s7/s8/y/s9/s10/s11/z.txt"
              let deepMatches =
                  Glob.expandCandidates true root "x/**/**/**/y/**/**/**/*.txt" None [ deepPath; p "x/y/z.txt"; p "x/nope.txt" ]

              Expect.equal
                  (deepMatches |> List.map fst)
                  [ deepPath; p "x/y/z.txt" ]
                  "consecutive **/** collapses and matches 0 and many segments fast"

              // Mid-segment ** degrades to single-segment * (does NOT cross /)
              let midMatches = Glob.expandCandidates true root "foo**bar.txt" None candidates
              Expect.equal
                  (midMatches |> List.map fst)
                  [ p "foo123bar.txt"; p "foobar.txt" ]
                  "foo**bar.txt matches intra-segment only and never crosses / to foo/bar.txt"
          }

          test "? matches 1 non-slash character and * matches 0+ non-slash characters" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              let p (rel: string) = Path.GetFullPath(Path.Combine(root, rel.Replace('/', Path.DirectorySeparatorChar)))

              // Test matchPattern directly on single-character ? against slash vs non-slash
              Expect.isSome (Glob.matchPattern true root "a?b.txt" (p "a1b.txt") |> Option.ofValueOption) "a1b.txt matches a?b.txt"
              Expect.isNone (Glob.matchPattern true root "a?b.txt" (p "ab.txt") |> Option.ofValueOption) "ab.txt (0 chars) rejected by ?"
              Expect.isNone (Glob.matchPattern true root "a?b.txt" (p "a12b.txt") |> Option.ofValueOption) "a12b.txt (2 chars) rejected by ?"
              Expect.isNone (Glob.matchPattern true root "a?b.txt" (p "a/b.txt") |> Option.ofValueOption) "a/b.txt ('/') MUST NOT match ?"

              Expect.isSome (Glob.matchPattern true root "a*b.txt" (p "ab.txt") |> Option.ofValueOption) "ab.txt (0 chars) matches *"
              Expect.isSome (Glob.matchPattern true root "a*b.txt" (p "aXYZb.txt") |> Option.ofValueOption) "aXYZb.txt matches *"
              Expect.isNone (Glob.matchPattern true root "a*b.txt" (p "a/b.txt") |> Option.ofValueOption) "a/b.txt ('/') MUST NOT match *"
          }

          test "caseSensitive parameter controls Ordinal vs OrdinalIgnoreCase matching" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              let p (rel: string) = Path.GetFullPath(Path.Combine(root, rel.Replace('/', Path.DirectorySeparatorChar)))
              let candidates = [ p "Docs/Api/Guide.MD" ]

              let insensitive = Glob.expandCandidates false root "docs/**/guide.md" None candidates
              Expect.equal (insensitive |> List.map fst) [ p "Docs/Api/Guide.MD" ] "case-insensitive matches differing case"

              let sensitive = Glob.expandCandidates true root "docs/**/guide.md" None candidates
              Expect.isEmpty sensitive "case-sensitive rejects differing case"

              let sensitiveExact = Glob.expandCandidates true root "Docs/**/Guide.MD" None candidates
              Expect.equal (sensitiveExact |> List.map fst) [ p "Docs/Api/Guide.MD" ] "case-sensitive matches exact case"
          }

          test "out-of-cone literal paths and sequential Include / Exclude / Remove / Update algebra on disk" {
              use tmp = makeTempRoot ()
              let sharedDir = Path.Combine(tmp.Dir, "shared")
              let projDir = Path.Combine(tmp.Dir, "sub", "app")
              Directory.CreateDirectory sharedDir |> ignore
              Directory.CreateDirectory projDir |> ignore

              let sharedNote = Path.GetFullPath(Path.Combine(sharedDir, "notes.txt"))
              let readme = Path.GetFullPath(Path.Combine(projDir, "README.md"))
              let docA = Path.GetFullPath(Path.Combine(projDir, "docs", "a.md"))
              let docDraft = Path.GetFullPath(Path.Combine(projDir, "docs", "draft", "wip.md"))
              let docB = Path.GetFullPath(Path.Combine(projDir, "docs", "api", "b.md"))
              let cfgJson = Path.GetFullPath(Path.Combine(projDir, "appsettings.json"))

              for f in [ sharedNote; readme; docA; docDraft; docB; cfgJson ] do
                  writeText f "content"

              let ops =
                  [ Glob.ItemOp.Include(@"..\..\shared\notes.txt; README.md; docs/**/*.md", Some "docs/draft/**", [ "CopyToOutputDirectory", "Never" ])
                    Glob.ItemOp.Include("appsettings.json; README.md", None, [ "CopyToOutputDirectory", "PreserveNewest" ])
                    Glob.ItemOp.Remove("docs/a.md")
                    Glob.ItemOp.Update("README.md", [ "CopyToOutputDirectory", "PreserveNewest"; "Link", "Docs/Readme.md" ])
                    Glob.ItemOp.Update("README.md", [ "CopyToOutputDirectory", "Always" ])
                    Glob.ItemOp.Include("docs/a.md", None, [ "ReAdded", "true" ]) ]

              let evaluated = Glob.evaluateDisk false projDir ops

              Expect.equal
                  (evaluated |> List.map _.FullPath)
                  [ sharedNote; readme; docB; cfgJson; docA ]
                  "preserves sub-pattern order, excludes draft, deduplicates README, removes docs/a.md, keeps README in place across Updates, and appends re-included docs/a.md at end"

              let readmeItem = evaluated |> List.find (fun i -> i.FullPath = readme)
              Expect.equal
                  (Map.ofList readmeItem.Metadata)
                  (Map.ofList [ "CopyToOutputDirectory", "Always"; "Link", "Docs/Readme.md" ])
                  "last Update wins per key while retaining earlier non-overridden keys"

              let docBItem = evaluated |> List.find (fun i -> i.FullPath = docB)
              Expect.equal docBItem.RecursiveDir "api/" "RecursiveDir preserved on wildcard item"
          } ]

[<Tests>]
let projectSpecItemTests =
    testSequenced
    <| testList
        "Task6b.ProjectSpecItems"
        [ testTask "interleaves CompileBefore, explicit None/Content/Compile, CompileAfter, and SDK implicit None in exact Slot 1..4 order while keeping SourceFiles strictly Compile-only" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              File.Copy(Path.Combine(findRepoRoot (), "global.json"), Path.Combine(root, "global.json"))
              let projDir = Path.Combine(root, "App")
              Directory.CreateDirectory projDir |> ignore

              let p (rel: string) = Path.GetFullPath(Path.Combine(projDir, rel.Replace('/', Path.DirectorySeparatorChar)))

              writeText (p "First.fs") "namespace App\nmodule First =\n    let v0 = 1\n"
              writeText (p "README.md") "# App\n"
              writeText (p "Domain.fs") "namespace App\nmodule Domain =\n    let v1 = First.v0 + 1\n"
              writeText (p "appsettings.json") "{}\n"
              writeText (p "Program.fs") "namespace App\nmodule Program =\n    let v2 = Domain.v1 + 1\n"
              writeText (p "Last.fs") "namespace App\nmodule Last =\n    let v3 = Program.v2 + 1\n"
              writeText (p "notes/implicit1.txt") "one"
              writeText (p "notes/implicit2.txt") "two"
              writeText (p "notes/removed.txt") "gone"

              let fsproj = p "App.fsproj"

              writeText
                  fsproj
                  $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <DisableImplicitFSharpCoreReference>true</DisableImplicitFSharpCoreReference>
    <EnableDefaultNoneItems>true</EnableDefaultNoneItems>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="FSharp.Core" Version="9.0.100" ExcludeAssets="all" />
    <Reference Include="FSharp.Core">
      <HintPath>{Environment.GetEnvironmentVariable "RFS_TEST_FCS_DIR" |> Option.ofObj |> Option.defaultValue @"C:\Users\shaya\RiderProjects\fickle-fsharp-i1\artifacts\bin\FSharp.Compiler.Service\Release\net10.0\"}FSharp.Core.dll</HintPath>
    </Reference>
  </ItemGroup>
  <ItemGroup>
    <None Remove="notes/removed.txt" />
    <None Include="README.md" />
    <Compile Include="Domain.fs" />
    <Content Include="appsettings.json" CopyToOutputDirectory="PreserveNewest" />
    <Compile Include="Program.fs" />
    <CompileBefore Include="First.fs" />
    <CompileAfter Include="Last.fs" />
    <None Update="notes/implicit1.txt" CopyToOutputDirectory="Always" />
  </ItemGroup>
</Project>"""

              let! spec = ProjectSpec.load fsproj

              // Invariant 1: SourceFiles and toFcsOptions are strictly Compile-only in exact compilation order
              let expectedCompile = [ p "First.fs"; p "Domain.fs"; p "Program.fs"; p "Last.fs" ]
              Expect.equal spec.SourceFiles expectedCompile "SourceFiles strictly Compile-only"
              Expect.equal (List.ofArray (ProjectSpec.toFcsOptions spec).SourceFiles) expectedCompile "toFcsOptions strictly Compile-only"

              // Invariant 2: Compile projection of spec.Items equals spec.SourceFiles
              let compileItems = [ for i in spec.Items do if i.Kind = ProjectItemKind.Compile then i.FilePath ]
              Expect.equal compileItems spec.SourceFiles "Compile items in spec.Items match SourceFiles order"

              // Invariant 3: Exact Slot 1 -> Slot 2 (interleaved) -> Slot 3 -> Slot 4 (SDK implicit None) sequence
              let actualKindsAndPaths = spec.Items |> List.map (fun i -> i.Kind, i.FilePath)
              let expectedKindsAndPaths =
                  [ ProjectItemKind.Compile, p "First.fs"
                    ProjectItemKind.NoneItem, p "README.md"
                    ProjectItemKind.Compile, p "Domain.fs"
                    ProjectItemKind.Content, p "appsettings.json"
                    ProjectItemKind.Compile, p "Program.fs"
                    ProjectItemKind.Compile, p "Last.fs"
                    ProjectItemKind.NoneItem, p "notes/implicit1.txt"
                    ProjectItemKind.NoneItem, p "notes/implicit2.txt" ]

              Expect.equal actualKindsAndPaths expectedKindsAndPaths "Slot 1..4 ordering and Remove respected"

              let implicit1 = spec.Items |> List.find (fun i -> i.FilePath = p "notes/implicit1.txt")
              Expect.contains implicit1.Metadata ("CopyToOutputDirectory", "Always") "Update metadata applied to implicit None without moving it out of Slot 4"

              // JSON round-trip preserves Items & Metadata
              Expect.equal (ProjectSpec.ofJson (ProjectSpec.toJson spec)) spec "ProjectSpec JSON round-trip preserves Items"
          }

          testTask "enforces Compile > EmbeddedResource > Content > None exclusivity, Visible=false exclusion, obj/ exclusion, VirtualPath 4-tier precedence, and offline cache invalidation" {
              use tmp = makeTempRoot ()
              let root = tmp.Dir
              File.Copy(Path.Combine(findRepoRoot (), "global.json"), Path.Combine(root, "global.json"))
              let sharedFile = Path.GetFullPath(Path.Combine(root, "SharedNotes.md"))
              writeText sharedFile "shared"

              let projDir = Path.Combine(root, "Pkg")
              Directory.CreateDirectory projDir |> ignore
              let p (rel: string) = Path.GetFullPath(Path.Combine(projDir, rel.Replace('/', Path.DirectorySeparatorChar)))

              writeText (p "Lib.fs") "module Lib\nlet x = 42\n"
              writeText (p "Strings.resx") "<root />"
              writeText (p "config.json") "{}"
              writeText (p "hidden-content.json") "{}"
              writeText (p "hidden-none.txt") "secret"
              writeText (p "assets/icons/app.png") "png"
              writeText (p "docs/sub/guide.md") "# guide"
              writeText (p "plain/info.txt") "info"
              writeText (p "obj/sneaky.txt") "generated-junk"

              let fsproj = p "Pkg.fsproj"

              writeText
                  fsproj
                  $"""<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <DisableImplicitFSharpCoreReference>true</DisableImplicitFSharpCoreReference>
    <EnableDefaultNoneItems>true</EnableDefaultNoneItems>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="FSharp.Core" Version="9.0.100" ExcludeAssets="all" />
    <Reference Include="FSharp.Core">
      <HintPath>{Environment.GetEnvironmentVariable "RFS_TEST_FCS_DIR" |> Option.ofObj |> Option.defaultValue @"C:\Users\shaya\RiderProjects\fickle-fsharp-i1\artifacts\bin\FSharp.Compiler.Service\Release\net10.0\"}FSharp.Core.dll</HintPath>
    </Reference>
  </ItemGroup>
  <ItemGroup>
    <Compile Include="Lib.fs" />
    <!-- Exclusivity attempts: None declared BEFORE Content must still lose to Content (and Visible=false Content must never leak into None) -->
    <None Include="Lib.fs; Strings.resx; config.json; hidden-content.json" />
    <Content Include="Lib.fs; Strings.resx; config.json" />
    <Content Include="hidden-content.json" Visible="false" />
    <None Include="hidden-none.txt" Visible="FALSE" />
    <None Include="obj/sneaky.txt" />
    <!-- VirtualPath priority 1: Link wins over LinkBase even when both are present, normalises \ -> /, trims leading /, expands RecursiveDir/Filename/Extension -->
    <Content Include="assets/**/*.png" Link="\Static\%%(RecursiveDir)%%(Filename)%%(Extension)" LinkBase="IgnoredBase" />
    <!-- VirtualPath priority 2: LinkBase + RecursiveDir + Filename + Extension -->
    <None Include="docs/**/*.md" LinkBase="Manual\" />
    <!-- VirtualPath priority 3: cone-relative path -->
    <None Include="plain/info.txt" />
    <!-- VirtualPath priority 4: external file outside projectDir with no Link/LinkBase -> FileName -->
    <None Include="..\SharedNotes.md" />
  </ItemGroup>
</Project>"""

              let! spec = ProjectSpec.load fsproj

              // Exclusivity checks
              let byPath = spec.Items |> List.groupBy _.FilePath |> Map.ofList
              Expect.equal (byPath[p "Lib.fs"] |> List.map _.Kind) [ ProjectItemKind.Compile ] "Lib.fs appears ONLY as Compile"
              Expect.isFalse (byPath.ContainsKey(p "Strings.resx")) "EmbeddedResource (.resx) excluded from Content and None"
              Expect.equal (byPath[p "config.json"] |> List.map _.Kind) [ ProjectItemKind.Content ] "config.json appears ONLY as Content, not None"
              Expect.isFalse (byPath.ContainsKey(p "hidden-content.json")) "Visible=false Content excluded AND never leaks into None"
              Expect.isFalse (byPath.ContainsKey(p "hidden-none.txt")) "Visible=FALSE None excluded (case-insensitive)"
              Expect.isFalse (byPath.ContainsKey(p "obj/sneaky.txt")) "obj/ file excluded from None/Content"
              Expect.isFalse (byPath.ContainsKey fsproj) ".fsproj itself excluded from Items"

              // VirtualPath 4-tier priority checks
              let findItem full = spec.Items |> List.find (fun i -> i.FilePath = full)
              Expect.equal (findItem (p "assets/icons/app.png")).VirtualPath "Static/icons/app.png" "Tier 1: Link wins over LinkBase and expands %(RecursiveDir)"
              Expect.equal (findItem (p "docs/sub/guide.md")).VirtualPath "Manual/sub/guide.md" "Tier 2: LinkBase combines with RecursiveDir + Filename + Extension"
              Expect.equal (findItem (p "plain/info.txt")).VirtualPath "plain/info.txt" "Tier 3: cone-relative path with /"
              Expect.equal (findItem sharedFile).VirtualPath "SharedNotes.md" "Tier 4: out-of-cone path without Link/LinkBase uses FileName"

              // Offline cache invalidation: add a new file matching docs/**/*.md WITHOUT touching Pkg.fsproj or calling ProjectSpec.invalidate!
              let addedOffline = p "docs/sub/added-offline.md"
              writeText addedOffline "# offline"
              let! afterCreate = ProjectSpec.load fsproj
              Expect.isTrue (afterCreate.Items |> List.exists (fun i -> i.FilePath = addedOffline && i.VirtualPath = "Manual/sub/added-offline.md")) "offline file creation invalidates cached Items automatically"

              // Delete that file offline and verify ProjectSpec.load drops it without touching Pkg.fsproj!
              File.Delete addedOffline
              let! afterDelete = ProjectSpec.load fsproj
              Expect.isFalse (afterDelete.Items |> List.exists (fun i -> i.FilePath = addedOffline)) "offline file deletion invalidates cached Items automatically"
          } ]
