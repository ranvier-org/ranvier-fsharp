# Builds FsAutoComplete upstream main at the baseline commit into ../ranvier-fsharp-fsac. Safe to re-run.
param(
    [string]$Commit = '4c33a2a7223ea1a885c6bf91e92c7c967c524597',
    [string]$Url = 'https://github.com/ionide/FsAutoComplete.git'
)
$ErrorActionPreference = 'Stop'

$repoRoot = Split-Path -Parent $PSScriptRoot
$fsacDir = Join-Path (Split-Path -Parent $repoRoot) 'ranvier-fsharp-fsac'
$fsacDll = Join-Path $fsacDir 'src\FsAutoComplete\bin\Release\net10.0\fsautocomplete.dll'

function Invoke-Native {
    param([string]$Exe, [string[]]$Arguments, [string]$WorkingDirectory)
    Push-Location $WorkingDirectory
    try {
        & $Exe @Arguments
        if ($LASTEXITCODE -ne 0) { throw "$Exe $($Arguments -join ' ') failed with exit code $LASTEXITCODE (in $WorkingDirectory)" }
    }
    finally { Pop-Location }
}

if (-not (Test-Path (Join-Path $fsacDir '.git'))) {
    Invoke-Native git @('clone', $Url, $fsacDir) (Split-Path -Parent $repoRoot)
}

$head = (& git -C $fsacDir rev-parse HEAD 2>$null)
if ($head -ne $Commit) {
    $have = (& git -C $fsacDir cat-file -t $Commit 2>$null)
    if ($have -ne 'commit') { Invoke-Native git @('fetch', 'origin', $Commit) $fsacDir }
    Invoke-Native git @('checkout', '--force', '--detach', $Commit) $fsacDir
}

Invoke-Native dotnet @('tool', 'restore') $fsacDir
Invoke-Native dotnet @('build', '-c', 'Release', 'src/FsAutoComplete', '-f', 'net10.0') $fsacDir

if (-not (Test-Path $fsacDll)) { throw "Expected $fsacDll after the build, but it does not exist" }
Write-Host "OK: $fsacDll"
