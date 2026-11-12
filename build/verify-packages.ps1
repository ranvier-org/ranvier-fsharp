<#
.SYNOPSIS
  Verifies all 7 Ranvier.FSharp / Ranvier.AutoComplete NuGet packages (offline .nupkg inspection + isolated end-to-end consumer builds/runs).

.DESCRIPTION
  Supports two modes:
    1. Offline + Local Feed Verification (`-NupkgDir ./artifacts/nupkg [-Version <ver>]`):
       - Inspects all 7 `.nupkg` archives, `.nuspec` metadata, bundled FCS DLLs/XMLs, MSBuild `.targets`,
         exact `[version]` sibling ranges, and `AssemblyMetadata("RanvierTrace", "true")` binary attributes.
       - Runs isolated out-of-repo consumer builds and end-to-end runtime smoke tests against `$NupkgDir` + `nuget.org`:
         (a) `Ranvier.AutoComplete` `.NET` tool install, `--version`, `--help`, and stdio LSP `initialize`/`shutdown`/`exit`
         (b) F# consumer of `Ranvier.FSharp.Server` exercising `Rfs.Lsp`, `Rfs.Projects` (`MSBuildLocator`), `FSharp.Compiler.Service`, `Rfs.Checker`, and `Rfs.Server` hover over LSP
         (c) F# consumer of `Ranvier.FSharp.Server.Traced` exercising `ProjectGraph.Traced = true` and `rfs/why` causal trace output
         (d) C# consumer of `Ranvier.FSharp.Checker` proving transitive `FSharp.Core` + `FSharp.Compiler.Service` resolution without implicit F# SDK references

    2. Live NuGet.org Verification (`-OnlineOnly [-Version <ver>]`):
       - Uses ONLY `https://api.nuget.org/v3/index.json` (no local feed) and a fresh empty `NUGET_PACKAGES` cache,
         running all 4 isolated consumer smoke tests end-to-end as an external user on a clean machine.
#>
[CmdletBinding()]
param(
    [string]$NupkgDir = '',
    [string]$Version = '',
    [switch]$OnlineOnly
)
$ErrorActionPreference = 'Stop'
$env:DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER = '1'
$env:DOTNET_NOLOGO = '1'
$env:DOTNET_SKIP_FIRST_TIME_EXPERIENCE = '1'

Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem

$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))

if (-not $Version) {
    $propsXml = [xml](Get-Content -LiteralPath (Join-Path $repoRoot 'Directory.Build.props') -Raw)
    $verNode = $propsXml.Project.PropertyGroup | ForEach-Object { $_.Version } | Where-Object { $_ } | Select-Object -First 1
    if ($verNode -is [System.Xml.XmlElement]) {
        $Version = $verNode.InnerText.Trim()
    } else {
        $Version = "$verNode".Trim()
    }
}
if (-not $Version) { throw "Could not determine package version." }

if (-not $OnlineOnly) {
    if (-not $NupkgDir) {
        $NupkgDir = Join-Path $repoRoot 'artifacts/nupkg'
    }
    $NupkgDir = [System.IO.Path]::GetFullPath($NupkgDir)
    if (-not (Test-Path $NupkgDir)) {
        throw "NupkgDir '$NupkgDir' does not exist."
    }
}

function Assert-True([bool]$Condition, [string]$Message) {
    if (-not $Condition) {
        throw "Verification failed: $Message"
    }
}

# Custom attribute blob for `[<AssemblyMetadata("RanvierTrace", "true")>]`:
# prolog 0x0001, SerString(12, "RanvierTrace"), SerString(4, "true"), NumNamed 0x0000.
$traceMarker = [byte[]]@(
    0x01, 0x00,
    0x0C, 0x52, 0x61, 0x6E, 0x76, 0x69, 0x65, 0x72, 0x54, 0x72, 0x61, 0x63, 0x65,
    0x04, 0x74, 0x72, 0x75, 0x65,
    0x00, 0x00
)

function Test-BytePattern([byte[]]$Haystack, [byte[]]$Needle) {
    $limit = $Haystack.Length - $Needle.Length
    for ($i = 0; $i -le $limit; $i++) {
        if ($Haystack[$i] -ne $Needle[0]) { continue }
        $match = $true
        for ($j = 1; $j -lt $Needle.Length; $j++) {
            if ($Haystack[$i + $j] -ne $Needle[$j]) {
                $match = $false
                break
            }
        }
        if ($match) { return $true }
    }
    return $false
}

function Read-ZipEntryBytes([System.IO.Compression.ZipArchive]$Zip, [string]$EntryPath) {
    $entry = $Zip.Entries | Where-Object { $_.FullName.Replace('\', '/') -eq $EntryPath } | Select-Object -First 1
    Assert-True ($null -ne $entry) "Missing entry '$EntryPath'"
    $ms = [System.IO.MemoryStream]::new()
    $s = $entry.Open()
    try {
        $s.CopyTo($ms)
        return $ms.ToArray()
    }
    finally {
        $s.Dispose()
        $ms.Dispose()
    }
}

$expectedSpecs = @(
    @{ Id = 'Ranvier.FSharp.Lsp';            Tool = $false; Asm = 'Rfs.Lsp';      Traced = $false; Sibling = '';                             RanvierDep = '';               ExtraLibs = @(); HasTargets = $false },
    @{ Id = 'Ranvier.FSharp.Projects';       Tool = $false; Asm = 'Rfs.Projects'; Traced = $false; Sibling = 'Ranvier.FSharp.Lsp';            RanvierDep = '';               ExtraLibs = @('FSharp.Compiler.Service.dll', 'FSharp.Compiler.Service.xml', 'FSharp.DependencyManager.Nuget.dll', 'FSharp.DependencyManager.Nuget.xml'); HasTargets = $false },
    @{ Id = 'Ranvier.FSharp.Checker';        Tool = $false; Asm = 'Rfs.Checker';  Traced = $false; Sibling = 'Ranvier.FSharp.Projects';       RanvierDep = 'Ranvier';        ExtraLibs = @(); HasTargets = $false },
    @{ Id = 'Ranvier.FSharp.Checker.Traced'; Tool = $false; Asm = 'Rfs.Checker';  Traced = $true;  Sibling = 'Ranvier.FSharp.Projects';       RanvierDep = 'Ranvier.Traced'; ExtraLibs = @(); HasTargets = $false },
    @{ Id = 'Ranvier.FSharp.Server';         Tool = $false; Asm = 'Rfs.Server';   Traced = $false; Sibling = 'Ranvier.FSharp.Checker';        RanvierDep = '';               ExtraLibs = @(); HasTargets = $true  },
    @{ Id = 'Ranvier.FSharp.Server.Traced';  Tool = $false; Asm = 'Rfs.Server';   Traced = $true;  Sibling = 'Ranvier.FSharp.Checker.Traced'; RanvierDep = '';               ExtraLibs = @(); HasTargets = $true  },
    @{ Id = 'Ranvier.AutoComplete';          Tool = $true;  Asm = 'ranvierautocomplete'; Traced = $false; Sibling = '';                      RanvierDep = '';               ExtraLibs = @(); HasTargets = $false }
)

if (-not $OnlineOnly) {
    Write-Host "=== [1/5] Inspecting 7 .nupkg archives in $NupkgDir (version $Version)" -ForegroundColor Cyan
    $actualFiles = @(Get-ChildItem -LiteralPath $NupkgDir -Filter '*.nupkg' | Select-Object -ExpandProperty Name | Sort-Object)
    $expectedFiles = @($expectedSpecs | ForEach-Object { "$($_.Id).$Version.nupkg" } | Sort-Object)
    Assert-True (($actualFiles -join ';') -eq ($expectedFiles -join ';')) "Expected packages [$($expectedFiles -join ', ')], found [$($actualFiles -join ', ')]"

    foreach ($spec in $expectedSpecs) {
        $id = $spec.Id
        $pkgPath = Join-Path $NupkgDir "$id.$Version.nupkg"
        $zip = [System.IO.Compression.ZipFile]::OpenRead($pkgPath)
        try {
            $entries = @($zip.Entries | ForEach-Object { $_.FullName.Replace('\', '/') })
            Assert-True ($entries -contains 'README.md') "$id missing README.md"
            Assert-True ($entries -contains 'LICENSE') "$id missing LICENSE"
            Assert-True ($entries -contains 'THIRD-PARTY-NOTICES.md') "$id missing THIRD-PARTY-NOTICES.md"

            $readmeText = [System.Text.Encoding]::UTF8.GetString((Read-ZipEntryBytes $zip 'README.md'))
            Assert-True ($readmeText.Contains('Ionide') -and $readmeText.Contains('FsAutoComplete')) "$id README.md missing Ionide / FsAutoComplete attribution"
            $noticesText = [System.Text.Encoding]::UTF8.GetString((Read-ZipEntryBytes $zip 'THIRD-PARTY-NOTICES.md'))
            Assert-True ($noticesText.Contains('FsAutoComplete') -and $noticesText.Contains('Apache License')) "$id THIRD-PARTY-NOTICES.md missing FsAutoComplete Apache-2.0 notice"

            $nuspecEntry = $zip.Entries | Where-Object { $_.FullName.EndsWith('.nuspec', [System.StringComparison]::OrdinalIgnoreCase) } | Select-Object -First 1
            Assert-True ($null -ne $nuspecEntry) "$id missing .nuspec"
            $sr = [System.IO.StreamReader]::new($nuspecEntry.Open())
            try { $nuspecText = $sr.ReadToEnd() } finally { $sr.Dispose() }
            $nuspecXml = [xml]$nuspecText
            $meta = $nuspecXml.package.metadata

            Assert-True ($meta.id -eq $id) "$id .nuspec id mismatch: $($meta.id)"
            Assert-True ($meta.version -eq $Version) "$id .nuspec version mismatch: $($meta.version)"
            Assert-True ($meta.license.InnerText -eq 'MIT') "$id missing MIT license"
            Assert-True ($meta.readme -eq 'README.md') "$id missing readme metadata"
            Assert-True ($meta.repository.url -eq 'https://github.com/ranvier-org/ranvier-fsharp') "$id missing repository url"

            if ($spec.Tool) {
                $pkgTypes = @($meta.packageTypes.packageType | ForEach-Object { $_.name })
                Assert-True ($pkgTypes -contains 'DotnetTool') "$id missing DotnetTool packageType"
                Assert-True ($entries -contains 'tools/net10.0/any/DotnetToolSettings.xml') "$id missing DotnetToolSettings.xml"
                Assert-True ($entries -contains 'tools/net10.0/any/ranvierautocomplete.dll') "$id missing ranvierautocomplete.dll"
                Assert-True ($entries -contains 'tools/net10.0/any/FSharp.Compiler.Service.dll') "$id missing FSharp.Compiler.Service.dll"
                Assert-True (-not ($entries -contains 'tools/net10.0/any/Microsoft.NET.StringTools.dll')) "$id must not bundle copy-local Microsoft.NET.StringTools.dll"

                $checkerBytes = Read-ZipEntryBytes $zip 'tools/net10.0/any/Rfs.Checker.dll'
                $serverBytes  = Read-ZipEntryBytes $zip 'tools/net10.0/any/Rfs.Server.dll'
                Assert-True (-not (Test-BytePattern $checkerBytes $traceMarker)) "$id bundled a traced Rfs.Checker.dll"
                Assert-True (-not (Test-BytePattern $serverBytes $traceMarker)) "$id bundled a traced Rfs.Server.dll"
            }
            else {
                $dllEntry = "lib/net10.0/$($spec.Asm).dll"
                $xmlEntry = "lib/net10.0/$($spec.Asm).xml"
                Assert-True ($entries -contains $dllEntry) "$id missing $dllEntry"
                Assert-True ($entries -contains $xmlEntry) "$id missing XML documentation $xmlEntry"

                foreach ($extra in $spec.ExtraLibs) {
                    Assert-True ($entries -contains "lib/net10.0/$extra") "$id missing bundled lib/net10.0/$extra"
                }

                if ($spec.HasTargets) {
                    Assert-True ($entries -contains "build/$id.targets") "$id missing build/$id.targets"
                    Assert-True ($entries -contains "buildTransitive/$id.targets") "$id missing buildTransitive/$id.targets"
                }

                $deps = @($meta.dependencies.group | Where-Object { $_.targetFramework -eq 'net10.0' } | ForEach-Object { $_.dependency } | Where-Object { $null -ne $_ })
                $depIds = @($deps | ForEach-Object { $_.id })

                Assert-True ($depIds -contains 'FSharp.Core') "$id missing FSharp.Core dependency"
                $fsharpDep = $deps | Where-Object { $_.id -eq 'FSharp.Core' } | Select-Object -First 1
                Assert-True ($fsharpDep.exclude -ne 'All') "$id FSharp.Core dependency must not have exclude=All"
                Assert-True (-not ($depIds -contains 'Microsoft.Build')) "$id leaked Microsoft.Build into .nuspec dependencies"
                Assert-True (-not ($depIds -contains 'Microsoft.NET.StringTools')) "$id leaked Microsoft.NET.StringTools into .nuspec dependencies"

                if ($spec.Sibling) {
                    $sib = $deps | Where-Object { $_.id -eq $spec.Sibling } | Select-Object -First 1
                    Assert-True ($null -ne $sib) "$id missing sibling dependency $($spec.Sibling)"
                    Assert-True ($sib.version -eq "[$Version]") "$id sibling $($spec.Sibling) version expected [$Version], got $($sib.version)"
                }

                if ($spec.RanvierDep) {
                    Assert-True ($depIds -contains $spec.RanvierDep) "$id missing expected $($spec.RanvierDep) dependency"
                    $forbiddenRanvier = if ($spec.RanvierDep -eq 'Ranvier') { 'Ranvier.Traced' } else { 'Ranvier' }
                    Assert-True (-not ($depIds -contains $forbiddenRanvier)) "$id referenced wrong Ranvier variant $forbiddenRanvier"
                }

                $asmBytes = Read-ZipEntryBytes $zip $dllEntry
                $hasTrace = Test-BytePattern $asmBytes $traceMarker
                Assert-True ($hasTrace -eq $spec.Traced) "$id $dllEntry RanvierTrace attribute expected $($spec.Traced), got $hasTrace"
            }

            Write-Host "  OK: $id.$Version.nupkg" -ForegroundColor Green
        }
        finally {
            $zip.Dispose()
        }
    }
}
else {
    Write-Host "=== [1/5] Online-only mode: skipping local .nupkg check; pulling strictly from https://api.nuget.org/v3/index.json" -ForegroundColor Cyan
}

$workDir = Join-Path ([System.IO.Path]::GetTempPath()) ("rfs-pkg-verify-" + [Guid]::NewGuid().ToString('N').Substring(0, 8))
New-Item -ItemType Directory -Force -Path $workDir | Out-Null

$prevNugetPackages = $env:NUGET_PACKAGES
try {
    if ($OnlineOnly) {
        $env:NUGET_PACKAGES = Join-Path $workDir 'nuget-cache'
        New-Item -ItemType Directory -Force -Path $env:NUGET_PACKAGES | Out-Null
    }

    # Prevent any parent folder MSBuild props/targets/CPM on the host machine from leaking into the isolated consumer workspace.
    Set-Content -LiteralPath (Join-Path $workDir 'Directory.Build.props') -Value '<Project />' -Encoding UTF8
    Set-Content -LiteralPath (Join-Path $workDir 'Directory.Build.targets') -Value '<Project />' -Encoding UTF8
    Set-Content -LiteralPath (Join-Path $workDir 'Directory.Packages.props') -Value '<Project><PropertyGroup><ManagePackageVersionsCentrally>false</ManagePackageVersionsCentrally></PropertyGroup></Project>' -Encoding UTF8
    Set-Content -LiteralPath (Join-Path $workDir 'global.json') -Value '{"sdk":{"version":"10.0.100","rollForward":"latestFeature"}}' -Encoding UTF8

    $localSourceXml = if (-not $OnlineOnly) { "<add key=`"local`" value=`"$NupkgDir`" />" } else { '' }
    $nugetConfig = @"
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <packageSources>
    <clear />
    $localSourceXml
    <add key="nuget.org" value="https://api.nuget.org/v3/index.json" />
  </packageSources>
</configuration>
"@
    $nugetConfigPath = Join-Path $workDir 'nuget.config'
    Set-Content -LiteralPath $nugetConfigPath -Value $nugetConfig -Encoding UTF8

    # Shared tiny F# project fixture to load and typecheck inside the consumer tests.
    $sampleDir = Join-Path $workDir 'SampleFixture'
    New-Item -ItemType Directory -Force -Path $sampleDir | Out-Null
    Set-Content -LiteralPath (Join-Path $sampleDir 'SampleFixture.fsproj') -Value @'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Lib.fs" />
  </ItemGroup>
</Project>
'@ -Encoding UTF8
    $sampleLibFs = @'
module SampleFixture.Lib

let baseOffset = 10

let addNumbers (x: int) (y: int) : int =
    x + y + baseOffset
'@
    Set-Content -LiteralPath (Join-Path $sampleDir 'Lib.fs') -Value $sampleLibFs -Encoding UTF8

    Write-Host "=== [2/5] Building SampleFixture project in isolated temp workspace" -ForegroundColor Cyan
    Push-Location $sampleDir
    try {
        dotnet build SampleFixture.fsproj -c Release --configfile $nugetConfigPath
        if ($LASTEXITCODE -ne 0) { throw "SampleFixture build failed" }
    }
    finally {
        Pop-Location
    }

    # -------------------------------------------------------------------------
    # Smoke Test 1: Ranvier.AutoComplete CLI tool
    # -------------------------------------------------------------------------
    Write-Host "=== [3/5] Smoke-testing Ranvier.AutoComplete CLI tool ($Version)" -ForegroundColor Cyan
    $toolDir = Join-Path $workDir 'tool-bin'
    dotnet tool install Ranvier.AutoComplete --tool-path $toolDir --version $Version --configfile $nugetConfigPath
    if ($LASTEXITCODE -ne 0) { throw "dotnet tool install Ranvier.AutoComplete failed" }

    $toolExe = if ($IsWindows -or $env:OS -eq 'Windows_NT') { Join-Path $toolDir 'ranvierautocomplete.exe' } else { Join-Path $toolDir 'ranvierautocomplete' }
    Assert-True (Test-Path $toolExe) "Tool executable not found at $toolExe"

    $verOut = (& $toolExe --version | Out-String).Trim()
    if ($LASTEXITCODE -ne 0) { throw "ranvierautocomplete --version failed" }
    Assert-True ($verOut -eq "ranvierautocomplete $Version" -or $verOut.StartsWith("ranvierautocomplete $Version+")) "Expected 'ranvierautocomplete $Version[+sha]', got '$verOut'"

    $helpOut = (& $toolExe --help | Out-String)
    if ($LASTEXITCODE -ne 0) { throw "ranvierautocomplete --help failed" }
    Assert-True ($helpOut.Contains('--adaptive-lsp-server-enabled')) "Help output missing FSAC compatibility note"

    # Exercise stdio LSP initialize -> initialized -> shutdown -> exit on the installed CLI tool:
    $psi = [System.Diagnostics.ProcessStartInfo]::new($toolExe)
    $psi.WorkingDirectory = $sampleDir
    $psi.UseShellExecute = $false
    $psi.RedirectStandardInput = $true
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $proc = [System.Diagnostics.Process]::Start($psi)
    try {
        $proc.BeginErrorReadLine()
        function Send-LspFrame([System.IO.Stream]$Stream, [string]$Json) {
            $body = [System.Text.Encoding]::UTF8.GetBytes($Json)
            $header = [System.Text.Encoding]::ASCII.GetBytes("Content-Length: $($body.Length)`r`n`r`n")
            $Stream.Write($header, 0, $header.Length)
            $Stream.Write($body, 0, $body.Length)
            $Stream.Flush()
        }
        function Read-LspFrame([System.IO.Stream]$Stream) {
            $headerSb = [System.Text.StringBuilder]::new()
            while ($true) {
                $b = $Stream.ReadByte()
                if ($b -lt 0) { throw "EOF reading LSP header" }
                [void]$headerSb.Append([char]$b)
                $s = $headerSb.ToString()
                if ($s.EndsWith("`r`n`r`n")) { break }
            }
            $m = [regex]::Match($headerSb.ToString(), 'Content-Length:\s*(\d+)', [System.Text.RegularExpressions.RegexOptions]::IgnoreCase)
            $len = [int]$m.Groups[1].Value
            $buf = [byte[]]::new($len)
            $read = 0
            while ($read -lt $len) {
                $n = $Stream.Read($buf, $read, $len - $read)
                if ($n -le 0) { throw "EOF reading LSP body" }
                $read += $n
            }
            return [System.Text.Encoding]::UTF8.GetString($buf)
        }

        $rootUri = ([System.Uri]::new($sampleDir)).AbsoluteUri
        Send-LspFrame $proc.StandardInput.BaseStream "{`"jsonrpc`":`"2.0`",`"id`":1,`"method`":`"initialize`",`"params`":{`"processId`":$PID,`"rootUri`":`"$rootUri`",`"capabilities`":{}}}"
        $initResp = Read-LspFrame $proc.StandardOutput.BaseStream
        Assert-True ($initResp.Contains('"hoverProvider":true')) "CLI tool initialize response missing hoverProvider"
        Assert-True ($initResp.Contains("`"version`":`"$Version`"")) "CLI tool initialize serverInfo.version mismatch: $initResp"

        Send-LspFrame $proc.StandardInput.BaseStream '{"jsonrpc":"2.0","method":"initialized","params":{}}'
        Send-LspFrame $proc.StandardInput.BaseStream '{"jsonrpc":"2.0","id":2,"method":"shutdown","params":null}'
        do {
            $msg = Read-LspFrame $proc.StandardOutput.BaseStream
        } while (-not $msg.Contains('"id":2'))
        Send-LspFrame $proc.StandardInput.BaseStream '{"jsonrpc":"2.0","method":"exit","params":null}'

        if (-not $proc.WaitForExit(10000)) {
            $proc.Kill($true)
            throw "CLI tool did not exit cleanly within 10s"
        }
        Assert-True ($proc.ExitCode -eq 0) "CLI tool exited with non-zero code $($proc.ExitCode)"
        Write-Host "  OK: Ranvier.AutoComplete CLI tool ($verOut + stdio LSP round-trip)" -ForegroundColor Green
    }
    finally {
        if (-not $proc.HasExited) { $proc.Kill($true) }
        $proc.Dispose()
    }

    # -------------------------------------------------------------------------
    # Smoke Test 2: Untraced F# Library Stack + Traced F# Library Stack
    # -------------------------------------------------------------------------
    Write-Host "=== [4/5] Smoke-testing F# library consumers (Untraced + .Traced stacks)" -ForegroundColor Cyan

    $untracedDir = Join-Path $workDir 'UntracedApp'
    New-Item -ItemType Directory -Force -Path $untracedDir | Out-Null
    Set-Content -LiteralPath (Join-Path $untracedDir 'UntracedApp.fsproj') -Value @"
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Program.fs" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Ranvier.FSharp.Server" Version="[$Version]" />
  </ItemGroup>
</Project>
"@ -Encoding UTF8

    Set-Content -LiteralPath (Join-Path $untracedDir 'Program.fs') -Value @'
module UntracedConsumer

open System
open System.IO
open System.IO.Pipelines
open System.Reflection
open System.Text.Json.Nodes
open System.Threading.Tasks
open FSharp.Compiler.Text
open Rfs.Lsp
open Rfs.Projects
open Rfs.Checker
open Rfs.Server

let assertTrue cond msg =
    if not cond then failwithf "UntracedConsumer assertion failed: %s" msg

[<EntryPoint>]
let main argv =
    let sampleProj = Path.GetFullPath argv[0]
    let sampleFile = Path.Combine(Path.GetDirectoryName sampleProj, "Lib.fs")

    // 0. Verify bundled MSBuild .targets excluded copy-local Microsoft.NET.StringTools.dll
    let stringToolsCopyLocal = Path.Combine(AppContext.BaseDirectory, "Microsoft.NET.StringTools.dll")
    assertTrue (not (File.Exists stringToolsCopyLocal)) $"Microsoft.NET.StringTools.dll should not be copy-local at {stringToolsCopyLocal}"

    // 1. Verify untraced build marker
    assertTrue (not ProjectGraph.Traced) "Expected ProjectGraph.Traced = false in untraced Ranvier.FSharp.Checker"
    let hasTraceMeta (asm: Assembly) =
        asm.GetCustomAttributes<AssemblyMetadataAttribute>()
        |> Seq.exists (fun a -> a.Key = "RanvierTrace" && a.Value = "true")
    assertTrue (not (hasTraceMeta typeof<ProjectGraph>.Assembly)) "Rfs.Checker.dll must not have RanvierTrace metadata"
    assertTrue (not (hasTraceMeta typeof<RanvierServer>.Assembly)) "Rfs.Server.dll must not have RanvierTrace metadata"

    // 2. Verify Rfs.Lsp + bundled FSharp.Compiler.Service
    let edited =
        Document.apply "hello world" { Range = { Start = { Line = 0; Character = 6 }; End = { Line = 0; Character = 11 } }; Text = "fsharp" }
    assertTrue (edited = "hello fsharp") $"Document.apply unexpected: {edited}"
    assertTrue (Range.range0.FileName <> null) "Bundled FSharp.Compiler.Service Range.range0 accessible"

    // 3. Verify Rfs.Projects (Glob + MSBuildLocator ProjectSpec.load) + Rfs.Checker (GraphHost + ProjectGraph)
    assertTrue (Glob.splitPatterns "a.fs;b.fs" = [ "a.fs"; "b.fs" ]) "Glob.splitPatterns check"
    let t =
        task {
            let! spec = ProjectSpec.load sampleProj
            assertTrue (spec.SourceFiles |> List.exists (fun f -> f.EndsWith("Lib.fs", StringComparison.OrdinalIgnoreCase))) "ProjectSpec.load found Lib.fs"

            do!
                task {
                    use host = GraphHost.Start "consumer-checker"
                    use! graph = ProjectGraph.Create(host, spec)
                    let! struct (_, diags) = graph.SettledDiagnostics sampleFile
                    assertTrue (diags.Length = 0) $"Expected 0 diagnostics on clean SampleFixture, got {diags.Length}"
                }

            // 4. Verify Rfs.Server in-process LSP session (initialize + workspaceLoad + didOpen + hover + rfs/why untraced)
            let c2s = Pipe()
            let s2c = Pipe()
            let serverTask =
                Task.Run<int>(fun () ->
                    Server.run (c2s.Reader.AsStream()) (s2c.Writer.AsStream()))

            use client = LspClient.Connect(s2c.Reader.AsStream(), c2s.Writer.AsStream())
            let rootUri = Uri(Path.GetDirectoryName sampleProj).AbsoluteUri
            let fileUri = Uri(sampleFile).AbsoluteUri
            let! _ = client.Initialize(rootUri, JsonObject())

            let loadParams =
                JsonNode.Parse(sprintf """{"TextDocuments":[{"Uri":"%s"}]}""" (fileUri.Replace("Lib.fs", "SampleFixture.fsproj")))
            let! _, _ = client.Request("fsharp/workspaceLoad", loadParams, TimeSpan.FromSeconds 30.)

            let text = File.ReadAllText sampleFile
            do! client.DidOpen(sampleFile, text)

            let hoverParams =
                JsonNode.Parse(sprintf """{"textDocument":{"uri":"%s"},"position":{"line":4,"character":5}}""" fileUri)
            let! hoverNode, _ = client.Request("textDocument/hover", hoverParams, TimeSpan.FromSeconds 30.)
            let hoverJson = if isNull hoverNode then "" else hoverNode.ToJsonString()
            assertTrue (hoverJson.Contains("addNumbers")) $"Expected hover to mention addNumbers, got: {hoverJson}"

            do! client.Shutdown()
            c2s.Writer.Complete()
            let! exitCode = serverTask
            assertTrue (exitCode = 0) $"Expected Server.run exitCode 0, got {exitCode}"
        }
    t.GetAwaiter().GetResult()
    Console.WriteLine("OK: UntracedConsumer passed all checks.")
    0
'@ -Encoding UTF8

    Push-Location $untracedDir
    try {
        dotnet restore --configfile $nugetConfigPath
        if ($LASTEXITCODE -ne 0) { throw "UntracedApp restore failed" }
        dotnet run -c Release --no-restore -- (Join-Path $sampleDir 'SampleFixture.fsproj')
        if ($LASTEXITCODE -ne 0) { throw "UntracedApp failed" }
    }
    finally {
        Pop-Location
    }

    # Traced F# Consumer (`Ranvier.FSharp.Server.Traced`)
    $tracedDir = Join-Path $workDir 'TracedApp'
    New-Item -ItemType Directory -Force -Path $tracedDir | Out-Null
    Set-Content -LiteralPath (Join-Path $tracedDir 'TracedApp.fsproj') -Value @"
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="Program.fs" />
  </ItemGroup>
  <ItemGroup>
    <PackageReference Include="Ranvier.FSharp.Server.Traced" Version="[$Version]" />
  </ItemGroup>
</Project>
"@ -Encoding UTF8

    Set-Content -LiteralPath (Join-Path $tracedDir 'Program.fs') -Value @'
module TracedConsumer

open System
open System.IO
open System.IO.Pipelines
open System.Reflection
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Rfs.Lsp
open Rfs.Checker
open Rfs.Server

let assertTrue cond msg =
    if not cond then failwithf "TracedConsumer assertion failed: %s" msg

[<EntryPoint>]
let main argv =
    let sampleProj = Path.GetFullPath argv[0]
    let sampleFile = Path.Combine(Path.GetDirectoryName sampleProj, "Lib.fs")

    assertTrue ProjectGraph.Traced "Expected ProjectGraph.Traced = true in Ranvier.FSharp.Checker.Traced"
    let hasTraceMeta (asm: Assembly) =
        asm.GetCustomAttributes<AssemblyMetadataAttribute>()
        |> Seq.exists (fun a -> a.Key = "RanvierTrace" && a.Value = "true")
    assertTrue (hasTraceMeta typeof<ProjectGraph>.Assembly) "Rfs.Checker.dll missing RanvierTrace=true metadata"
    assertTrue (hasTraceMeta typeof<RanvierServer>.Assembly) "Rfs.Server.dll missing RanvierTrace=true metadata"

    let t =
        task {
            let c2s = Pipe()
            let s2c = Pipe()
            let mutable serverInstance: RanvierServer option = None
            let serverTask =
                Task.Run<int>(fun () ->
                    Server.runWith (fun s -> serverInstance <- Some s) (c2s.Reader.AsStream()) (s2c.Writer.AsStream()))

            use client = LspClient.Connect(s2c.Reader.AsStream(), c2s.Writer.AsStream())
            let rootUri = Uri(Path.GetDirectoryName sampleProj).AbsoluteUri
            let fileUri = Uri(sampleFile).AbsoluteUri
            let projUri = Uri(sampleProj).AbsoluteUri
            let! _ = client.Initialize(rootUri, JsonObject())

            let loadParams = JsonNode.Parse(sprintf """{"TextDocuments":[{"Uri":"%s"}]}""" projUri)
            let! _, _ = client.Request("fsharp/workspaceLoad", loadParams, TimeSpan.FromSeconds 30.)

            let text = File.ReadAllText sampleFile
            do! client.DidOpen(sampleFile, text)
            let makeHoverParams () =
                JsonNode.Parse(sprintf """{"textDocument":{"uri":"%s"},"position":{"line":4,"character":5}}""" fileUri)
            let! _, _ = client.Request("textDocument/hover", makeHoverParams (), TimeSpan.FromSeconds 30.)

            // Wait for the opened file to promote to declaration mode, then make an edit to `addNumbers` and query `rfs/why`.
            let srv = serverInstance.Value
            let graph = (srv.Workspace.ProjectOf sampleFile).Value
            let! mode = graph.WhenPromoted(sampleFile, CancellationToken.None)
            assertTrue (mode = FileMode.Declarations) $"Expected FileMode.Declarations, got {mode}"

            do!
                client.DidChange(
                    sampleFile,
                    1,
                    { Range = { Start = { Line = 5; Character = 4 }; End = { Line = 5; Character = 24 } }
                      Text = "x + y + baseOffset + 1" }
                )
            let! _, _ = client.Request("textDocument/hover", makeHoverParams (), TimeSpan.FromSeconds 30.)
            let! _ = graph.WhenPromoted(sampleFile, CancellationToken.None)

            let whyParams =
                JsonNode.Parse(sprintf """{"textDocument":{"uri":"%s"},"position":{"line":4,"character":5}}""" fileUri)
            let! whyNode, _ = client.Request("rfs/why", whyParams, TimeSpan.FromSeconds 30.)
            assertTrue (not (isNull whyNode)) "Expected non-null rfs/why answer"
            let traced = whyNode["traced"].GetValue<bool>()
            let chain = whyNode["chain"].GetValue<string>()
            assertTrue traced "Expected rfs/why traced = true"
            assertTrue (not (String.IsNullOrWhiteSpace chain)) $"Expected non-empty rfs/why causal chain, got '{chain}'"

            do! client.Shutdown()
            c2s.Writer.Complete()
            let! exitCode = serverTask
            assertTrue (exitCode = 0) $"Expected Server.runWith exitCode 0, got {exitCode}"
        }
    t.GetAwaiter().GetResult()
    Console.WriteLine("OK: TracedConsumer passed all checks (including rfs/why causal chain).")
    0
'@ -Encoding UTF8

    Push-Location $tracedDir
    try {
        dotnet restore --configfile $nugetConfigPath
        if ($LASTEXITCODE -ne 0) { throw "TracedApp restore failed" }
        dotnet run -c Release --no-restore -- (Join-Path $sampleDir 'SampleFixture.fsproj')
        if ($LASTEXITCODE -ne 0) { throw "TracedApp failed" }
    }
    finally {
        Pop-Location
    }

    # -------------------------------------------------------------------------
    # Smoke Test 3: C# Consumer of `Ranvier.FSharp.Checker` (no implicit FSharp.Core)
    # -------------------------------------------------------------------------
    Write-Host "=== [5/5] Smoke-testing C# interop consumer of Ranvier.FSharp.Checker ($Version)" -ForegroundColor Cyan
    $csDir = Join-Path $workDir 'CSharpConsumer'
    New-Item -ItemType Directory -Force -Path $csDir | Out-Null
    Set-Content -LiteralPath (Join-Path $csDir 'CSharpConsumer.csproj') -Value @"
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>
  <ItemGroup>
    <PackageReference Include="Ranvier.FSharp.Checker" Version="[$Version]" />
  </ItemGroup>
</Project>
"@ -Encoding UTF8

    Set-Content -LiteralPath (Join-Path $csDir 'Program.cs') -Value @'
using Rfs.Lsp;
using Rfs.Projects;
using Rfs.Checker;

var sampleProj = Path.GetFullPath(args[0]);
var sampleFile = Path.Combine(Path.GetDirectoryName(sampleProj)!, "Lib.fs");

var pos = new Position(0, 3);
if (Document.offsetOf("abcdef", pos) != 3)
    throw new Exception("Rfs.Lsp Document.offsetOf failed from C#");

var spec = await ProjectSpecModule.load(sampleProj);
using var host = GraphHost.Start("csharp-consumer");
using var graph = await ProjectGraph.Create(host, spec);
var (_, diags) = await graph.SettledDiagnostics(sampleFile);
if (diags.Length != 0)
    throw new Exception($"Expected 0 diagnostics from C# consumer, got {diags.Length}");

Console.WriteLine("OK: CSharpConsumer resolved FSharp.Core + FSharp.Compiler.Service + Ranvier.FSharp.Checker cleanly.");
'@ -Encoding UTF8

    Push-Location $csDir
    try {
        dotnet restore --configfile $nugetConfigPath
        if ($LASTEXITCODE -ne 0) { throw "CSharpConsumer restore failed" }
        dotnet run -c Release --no-restore -- (Join-Path $sampleDir 'SampleFixture.fsproj')
        if ($LASTEXITCODE -ne 0) { throw "CSharpConsumer failed" }
    }
    finally {
        Pop-Location
    }

    Write-Host ""
    Write-Host "ALL PACKAGE VERIFICATIONS PASSED (version $Version, OnlineOnly=$OnlineOnly)!" -ForegroundColor Green
}
finally {
    $env:NUGET_PACKAGES = $prevNugetPackages
    if (Test-Path $workDir) {
        try { Remove-Item -Recurse -Force $workDir } catch { }
    }
}
