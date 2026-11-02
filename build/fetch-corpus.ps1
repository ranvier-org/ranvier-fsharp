# Clones the repos in corpus/corpus.json at their pinned SHAs into ../ranvier-fsharp-corpus/<name>
# restores each project and builds it in Debug, so its project references exist on disk. Safe to re-run.
param(
    [string[]]$Only = @()
)

$ErrorActionPreference = 'Stop'

$repoRoot = Split-Path -Parent $PSScriptRoot
$corpus = Get-Content (Join-Path $repoRoot 'corpus/corpus.json') -Raw | ConvertFrom-Json
$corpusRoot = Join-Path (Split-Path -Parent $repoRoot) 'ranvier-fsharp-corpus'
New-Item -ItemType Directory -Force $corpusRoot | Out-Null

function Invoke-Native {
    param([string]$Exe, [string[]]$Arguments, [string]$WorkingDirectory)
    Push-Location $WorkingDirectory
    try {
        & $Exe @Arguments
        if ($LASTEXITCODE -ne 0) { throw "$Exe $($Arguments -join ' ') failed with exit code $LASTEXITCODE (in $WorkingDirectory)" }
    }
    finally { Pop-Location }
}

$failed = @()
foreach ($entry in $corpus.repos) {
    if ($Only.Count -gt 0 -and $Only -notcontains $entry.name) { continue }

    $dir = Join-Path $corpusRoot $entry.name
    Write-Host "== $($entry.name): $($entry.url) @ $($entry.sha)"
    try {

    if (-not (Test-Path (Join-Path $dir '.git'))) {
        Invoke-Native git @('clone', $entry.url, $dir) $corpusRoot
    }

    $head = (& git -C $dir rev-parse HEAD 2>$null)
    if ($head -ne $entry.sha) {
        $have = (& git -C $dir cat-file -t $entry.sha 2>$null)
        if ($have -ne 'commit') { Invoke-Native git @('fetch', 'origin', $entry.sha) $dir }
        Invoke-Native git @('checkout', '--force', '--detach', $entry.sha) $dir
    }

    # A repo can pin a preview SDK this machine lacks; the corpus only needs restore to work, so accept the nearest installed one.
    $globalJson = Join-Path $dir 'global.json'
    Push-Location $dir
    try { & dotnet --version *> $null; $sdkOk = ($LASTEXITCODE -eq 0) } finally { Pop-Location }
    if (-not $sdkOk -and (Test-Path $globalJson)) {
        Write-Warning "$($entry.name): pinned SDK not installed; rewriting global.json in the corpus clone to roll forward"
        Set-Content $globalJson (@{ sdk = @{ version = '10.0.100'; rollForward = 'latestMajor' } } | ConvertTo-Json -Depth 3)
    }

    # Repos pin their own tools (paket, fantomas, ...); restore them before the project so paket-managed references resolve.
    if (Test-Path (Join-Path $dir '.config/dotnet-tools.json')) {
        Invoke-Native dotnet @('tool', 'restore') $dir
    }
    if (Test-Path (Join-Path $dir 'paket.lock')) {
        Invoke-Native dotnet @('paket', 'restore') $dir
    }

    if ($entry.prepare -and -not (Test-Path (Join-Path $dir $entry.prepareUnless))) {
        # {repo} lets a prepare run a script of this repository.
        $prepare = @($entry.prepare | ForEach-Object { $_.Replace('{repo}', $repoRoot) })
        Invoke-Native $prepare[0] @($prepare | Select-Object -Skip 1) $dir
    }

    # A prepare that builds the project itself, with flags the plain build below lacks, sets "build": false.
    if ($entry.build -eq $false) {
        Write-Host "prepared $($entry.project); no separate build"
        continue
    }

    Invoke-Native dotnet @('restore', $entry.project) $dir

    # ProjectSpec hands the compiler the Debug reference assemblies of referenced projects, so they must exist. Built from this
    # repository (cwd) so its global.json picks the SDK, not the clone's; incremental, so a re-run is cheap.
    $project = Join-Path $dir ($entry.project -replace "/", [IO.Path]::DirectorySeparatorChar)
    Invoke-Native dotnet @("build", $project, "-c", "Debug", "--no-restore", "-v", "q", "-nologo") $repoRoot
    Write-Host "built $($entry.project) (Debug) with its project references"
    }
    catch {
        Write-Warning "$($entry.name) failed: $_"
        $failed += $entry.name
    }
}

if ($failed.Count -gt 0) {
    Write-Error "Corpus incomplete; failed: $($failed -join ', ')"
    exit 1
}
Write-Host "Corpus ready in $corpusRoot"
