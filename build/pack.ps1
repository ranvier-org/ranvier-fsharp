<#
.SYNOPSIS
  Builds, publishes, and packs all Ranvier.FSharp libraries and CLI tools (`Ranvier.AutoComplete` and `Ranvier.FSharp.Agent`).

.DESCRIPTION
  Produces in `<OutputDir>/nupkg`:
    - Ranvier.FSharp.Lsp.<version>.nupkg
    - Ranvier.FSharp.Projects.<version>.nupkg
    - Ranvier.FSharp.Checker.<version>.nupkg
    - Ranvier.FSharp.Checker.Traced.<version>.nupkg
    - Ranvier.FSharp.Server.<version>.nupkg
    - Ranvier.FSharp.Server.Traced.<version>.nupkg
    - Ranvier.AutoComplete.<version>.nupkg              (.NET global/local tool package)
    - Ranvier.FSharp.Agent.<version>.nupkg              (.NET global/local tool package `rfs`)
  And framework-dependent distribution bundles:
    - <OutputDir>/publish/                              (portable bundle for Ionide FSharp.fsac.netCoreDllPath)
    - <OutputDir>/publish-<rid>/                        (optional RID-specific bundles for Ranvier.AutoComplete)
    - <OutputDir>/publish-rfs/                          (portable bundle for `rfs`)
    - <OutputDir>/publish-rfs-<rid>/                    (optional RID-specific bundles for `rfs`)
#>
[CmdletBinding()]
param(
    [string]$Configuration = 'Release',
    [string]$OutputDir = (Join-Path $PSScriptRoot '../artifacts'),
    [string]$ForkDir = (Join-Path $PSScriptRoot '../../fickle-fsharp'),
    [string]$Runtime = '',
    [string[]]$Runtimes = @(),
    [string]$Version = '',
    [string]$ToolVersion = '',
    [string]$AgentVersion = '',
    [switch]$ToolOnly,
    [switch]$AgentOnly,
    [switch]$BuildFork
)
$ErrorActionPreference = 'Stop'
$env:DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER = '1'

if ($ToolOnly -and $AgentOnly) {
    throw "Cannot specify both -ToolOnly and -AgentOnly."
}

$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$OutputDir = [System.IO.Path]::GetFullPath($OutputDir)
$ForkDir = [System.IO.Path]::GetFullPath($ForkDir)
$fcsDir = [System.IO.Path]::Combine($ForkDir, 'artifacts', 'bin', 'FSharp.Compiler.Service', 'Release', 'net10.0')
$fcsDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.Compiler.Service.dll')
$depMgrDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.DependencyManager.Nuget.dll')

if ($BuildFork -or -not (Test-Path $fcsDll) -or -not (Test-Path $depMgrDll)) {
    Write-Host "=== Building FCS fork at $ForkDir" -ForegroundColor Cyan
    & (Join-Path $PSScriptRoot 'fork.ps1') -ForkDir $ForkDir
}

$serverProj = [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Server', 'Rfs.Server.fsproj')
$toolProj = [System.IO.Path]::Combine($repoRoot, 'src', 'Ranvier.AutoComplete', 'Ranvier.AutoComplete.fsproj')
$agentProj = [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Agent', 'Rfs.Agent.fsproj')
$untracedPackProjects = @(
    [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Lsp', 'Rfs.Lsp.fsproj'),
    [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Projects', 'Rfs.Projects.fsproj'),
    [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Checker', 'Rfs.Checker.fsproj'),
    $serverProj,
    $toolProj,
    $agentProj
)
$tracedPackProjects = @(
    [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Checker', 'Rfs.Checker.fsproj'),
    $serverProj
)

$nupkgDir = [System.IO.Path]::Combine($OutputDir, 'nupkg')
$publishDir = [System.IO.Path]::Combine($OutputDir, 'publish')
$publishRfsDir = [System.IO.Path]::Combine($OutputDir, 'publish-rfs')

$fcsProp = "-p:FickleFcsDir=$([System.IO.Path]::TrimEndingDirectorySeparator($fcsDir))$([System.IO.Path]::DirectorySeparatorChar)"
[string[]]$verArgs = @()
if ($Version -and ($ToolVersion -or $AgentVersion)) {
    $verArgs += "-p:LibraryVersion=$Version"
    if ($ToolVersion) { $verArgs += "-p:ToolVersion=$ToolVersion" }
    if ($AgentVersion) { $verArgs += "-p:AgentVersion=$AgentVersion" }
}
elseif ($Version) {
    $verArgs += "-p:Version=$Version"
}
else {
    if ($ToolVersion) { $verArgs += "-p:ToolVersion=$ToolVersion" }
    if ($AgentVersion) { $verArgs += "-p:AgentVersion=$AgentVersion" }
}

$projectsToPackUntraced =
    if ($ToolOnly) { @($toolProj) }
    elseif ($AgentOnly) { @($agentProj) }
    else { $untracedPackProjects }

$allRids = @()
if ($Runtime) { $allRids += $Runtime }
if ($Runtimes -and $Runtimes.Count -gt 0) { $allRids += $Runtimes }
$allRids = @($allRids | Where-Object { $_ } | Select-Object -Unique)

Push-Location $repoRoot
try {
    if (Test-Path $nupkgDir) { Remove-Item -Recurse -Force $nupkgDir }
    New-Item -ItemType Directory -Force -Path $nupkgDir | Out-Null

    if ($ToolOnly) {
        Write-Host "=== Building Ranvier.AutoComplete ($Configuration)" -ForegroundColor Cyan
        dotnet build $toolProj -c $Configuration --no-incremental $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet build (Ranvier.AutoComplete) failed with exit code $LASTEXITCODE" }
    }
    elseif ($AgentOnly) {
        Write-Host "=== Building Ranvier.FSharp.Agent ($Configuration)" -ForegroundColor Cyan
        dotnet build $agentProj -c $Configuration --no-incremental $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet build (Ranvier.FSharp.Agent) failed with exit code $LASTEXITCODE" }
    }
    else {
        Write-Host "=== Building untraced graph ($Configuration)" -ForegroundColor Cyan
        dotnet build $toolProj -c $Configuration --no-incremental $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet build (untraced) failed with exit code $LASTEXITCODE" }
        dotnet build $agentProj -c $Configuration $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet build (Ranvier.FSharp.Agent) failed with exit code $LASTEXITCODE" }
    }

    if (-not $AgentOnly) {
        Write-Host "=== Publishing Ranvier.AutoComplete framework-dependent bundle -> $publishDir" -ForegroundColor Cyan
        if (Test-Path $publishDir) { Remove-Item -Recurse -Force $publishDir }
        dotnet publish $toolProj -c $Configuration --no-build -o $publishDir $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet publish (Ranvier.AutoComplete) failed with exit code $LASTEXITCODE" }
        foreach ($doc in 'README.md', 'LICENSE', 'THIRD-PARTY-NOTICES.md') {
            Copy-Item -LiteralPath (Join-Path $repoRoot $doc) -Destination (Join-Path $publishDir $doc) -Force
        }

        foreach ($rid in $allRids) {
            $ridDir = [System.IO.Path]::Combine($OutputDir, "publish-$rid")
            Write-Host "=== Publishing Ranvier.AutoComplete $rid bundle -> $ridDir" -ForegroundColor Cyan
            if (Test-Path $ridDir) { Remove-Item -Recurse -Force $ridDir }
            dotnet publish $toolProj -c $Configuration -r $rid --self-contained false -o $ridDir $fcsProp @verArgs
            if ($LASTEXITCODE -ne 0) { throw "dotnet publish (Ranvier.AutoComplete $rid) failed with exit code $LASTEXITCODE" }
            foreach ($doc in 'README.md', 'LICENSE', 'THIRD-PARTY-NOTICES.md') {
                Copy-Item -LiteralPath (Join-Path $repoRoot $doc) -Destination (Join-Path $ridDir $doc) -Force
            }
        }
    }

    if (-not $ToolOnly) {
        Write-Host "=== Publishing rfs framework-dependent bundle -> $publishRfsDir" -ForegroundColor Cyan
        if (Test-Path $publishRfsDir) { Remove-Item -Recurse -Force $publishRfsDir }
        dotnet publish $agentProj -c $Configuration --no-build -o $publishRfsDir $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet publish (Ranvier.FSharp.Agent) failed with exit code $LASTEXITCODE" }
        foreach ($doc in 'README.md', 'LICENSE', 'THIRD-PARTY-NOTICES.md') {
            Copy-Item -LiteralPath (Join-Path $repoRoot $doc) -Destination (Join-Path $publishRfsDir $doc) -Force
        }

        foreach ($rid in $allRids) {
            $ridRfsDir = [System.IO.Path]::Combine($OutputDir, "publish-rfs-$rid")
            Write-Host "=== Publishing rfs $rid bundle -> $ridRfsDir" -ForegroundColor Cyan
            if (Test-Path $ridRfsDir) { Remove-Item -Recurse -Force $ridRfsDir }
            dotnet publish $agentProj -c $Configuration -r $rid --self-contained false -o $ridRfsDir $fcsProp @verArgs
            if ($LASTEXITCODE -ne 0) { throw "dotnet publish (Ranvier.FSharp.Agent $rid) failed with exit code $LASTEXITCODE" }
            foreach ($doc in 'README.md', 'LICENSE', 'THIRD-PARTY-NOTICES.md') {
                Copy-Item -LiteralPath (Join-Path $repoRoot $doc) -Destination (Join-Path $ridRfsDir $doc) -Force
            }
        }
    }

    Write-Host "=== Packing untraced NuGet packages -> $nupkgDir" -ForegroundColor Cyan
    foreach ($proj in $projectsToPackUntraced) {
        $name = [System.IO.Path]::GetFileNameWithoutExtension($proj)
        Write-Host "  -> Packing $name ($Configuration)"
        dotnet pack $proj -c $Configuration --no-build --no-restore -o $nupkgDir $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet pack ($name) failed with exit code $LASTEXITCODE" }
    }

    if ((-not $ToolOnly) -and (-not $AgentOnly)) {
        Write-Host "=== Building traced library graph ($Configuration, RanvierTrace=true)" -ForegroundColor Cyan
        dotnet build $serverProj -c $Configuration -p:RanvierTrace=true --no-incremental $fcsProp @verArgs
        if ($LASTEXITCODE -ne 0) { throw "dotnet build (traced) failed with exit code $LASTEXITCODE" }

        Write-Host "=== Packing traced NuGet packages (.Traced) -> $nupkgDir" -ForegroundColor Cyan
        foreach ($proj in $tracedPackProjects) {
            $name = [System.IO.Path]::GetFileNameWithoutExtension($proj)
            Write-Host "  -> Packing $name.Traced ($Configuration, RanvierTrace=true)"
            dotnet pack $proj -c $Configuration -p:RanvierTrace=true --no-build --no-restore -o $nupkgDir $fcsProp @verArgs
            if ($LASTEXITCODE -ne 0) { throw "dotnet pack ($name.Traced) failed with exit code $LASTEXITCODE" }
        }
    }
}
finally {
    Pop-Location
}

$dllPath = [System.IO.Path]::Combine($publishDir, 'ranvierautocomplete.dll')
$rfsDllPath = [System.IO.Path]::Combine($publishRfsDir, 'rfs.dll')
$nupkgs = @(Get-ChildItem $nupkgDir -Filter '*.nupkg' | Sort-Object Name)
Write-Host ""
Write-Host "Done! Packed $($nupkgs.Count) NuGet packages:" -ForegroundColor Green
foreach ($pkg in $nupkgs) {
    Write-Host "  - $($pkg.Name)"
}
if (-not $AgentOnly) {
    Write-Host "  Ionide DLL path : $dllPath"
}
if (-not $ToolOnly) {
    Write-Host "  rfs DLL path    : $rfsDllPath"
}
