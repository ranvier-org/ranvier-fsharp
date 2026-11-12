<#
.SYNOPSIS
  Builds, publishes, and packs Ranvier.AutoComplete (`ranvierautocomplete`) for distribution and editor setup.

.DESCRIPTION
  Produces:
    - artifacts/nupkg/Ranvier.AutoComplete.<version>.nupkg   (.NET global/local tool package)
    - artifacts/publish/                                     (framework-dependent DLL bundle for Ionide FSharp.fsac.netCoreDllPath)
    - artifacts/publish-<Runtime>/                           (optional RID-specific bundle when -Runtime is passed)
#>
[CmdletBinding()]
param(
    [string]$Configuration = 'Release',
    [string]$OutputDir = (Join-Path $PSScriptRoot '../artifacts'),
    [string]$ForkDir = (Join-Path $PSScriptRoot '../../fickle-fsharp'),
    [string]$Runtime = '',
    [string]$Version = '',
    [switch]$BuildFork
)
$ErrorActionPreference = 'Stop'
$env:DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER = '1'

$repoRoot = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '..'))
$OutputDir = [System.IO.Path]::GetFullPath($OutputDir)
$ForkDir = [System.IO.Path]::GetFullPath($ForkDir)
$fcsDir = [System.IO.Path]::Combine($ForkDir, 'artifacts', 'bin', 'FSharp.Compiler.Service', 'Release', 'net10.0')
$fcsDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.Compiler.Service.dll')

if ($BuildFork -or -not (Test-Path $fcsDll)) {
    Write-Host "=== Building FCS fork at $ForkDir" -ForegroundColor Cyan
    & (Join-Path $PSScriptRoot 'fork.ps1') -ForkDir $ForkDir
}

$serverProj = [System.IO.Path]::Combine($repoRoot, 'src', 'Rfs.Server', 'Rfs.Server.fsproj')
$nupkgDir = [System.IO.Path]::Combine($OutputDir, 'nupkg')
$publishDir = [System.IO.Path]::Combine($OutputDir, 'publish')

$fcsProp = "-p:FickleFcsDir=$([System.IO.Path]::TrimEndingDirectorySeparator($fcsDir))$([System.IO.Path]::DirectorySeparatorChar)"
$versionProps = if ($Version) { @("-p:Version=$Version") } else { @() }

Push-Location $repoRoot
try {
    Write-Host "=== Publishing framework-dependent bundle -> $publishDir" -ForegroundColor Cyan
    if (Test-Path $publishDir) { Remove-Item -Recurse -Force $publishDir }
    dotnet publish $serverProj -c $Configuration -o $publishDir $fcsProp @versionProps
    if ($LASTEXITCODE -ne 0) { throw "dotnet publish failed with exit code $LASTEXITCODE" }

    Write-Host "=== Packing .NET tool NuGet package -> $nupkgDir" -ForegroundColor Cyan
    if (Test-Path $nupkgDir) { Remove-Item -Recurse -Force $nupkgDir }
    dotnet pack $serverProj -c $Configuration -o $nupkgDir $fcsProp @versionProps
    if ($LASTEXITCODE -ne 0) { throw "dotnet pack failed with exit code $LASTEXITCODE" }

    if ($Runtime) {
        $ridDir = [System.IO.Path]::Combine($OutputDir, "publish-$Runtime")
        Write-Host "=== Publishing $Runtime bundle -> $ridDir" -ForegroundColor Cyan
        if (Test-Path $ridDir) { Remove-Item -Recurse -Force $ridDir }
        dotnet publish $serverProj -c $Configuration -r $Runtime --self-contained false -o $ridDir $fcsProp @versionProps
        if ($LASTEXITCODE -ne 0) { throw "dotnet publish ($Runtime) failed with exit code $LASTEXITCODE" }
    }
}
finally {
    Pop-Location
}

$dllPath = [System.IO.Path]::Combine($publishDir, 'ranvierautocomplete.dll')
$nupkg = Get-ChildItem $nupkgDir -Filter '*.nupkg' | Select-Object -First 1
Write-Host ""
Write-Host "Done!" -ForegroundColor Green
Write-Host "  NuGet tool package : $($nupkg.FullName)"
Write-Host "  Ionide DLL path    : $dllPath"
