<#
.SYNOPSIS
  Task 19's measurement, strictly sequential and resumable: builds Release once, runs FSAC on the scenarios it has no
  baseline for (Step 2), the server on every scenario and the body scenarios again (Step 3), and the compares (Step 4).

.DESCRIPTION
  Run it from anywhere, on a quiet machine:  pwsh -File build/final-run.ps1
  Nothing else should run beside it (no test suite, no build, no second Claude session): every latency here is a timing.

  Resuming: a run whose result file exists in its output directory is skipped, also when it had failing steps (its exit
  code is in run.log; delete the file to run it again). A run that crashed leaves no file and runs again.

  -DryRun       prints the commit/SDK block, the plan and the estimate; builds and runs nothing.
  -Rebuild      builds even when results/server-final/environment.txt says the Release build already ran.
  -Small        runs the small/mid suite plus core large scenarios (excludes huge and large-bigfile/edit scenarios).
  -SkipHugeRepeat  leaves the huge body scenarios out of the second pass (they are most of its time).
  -StepTimeoutS how long one step may take before it fails (default 300; the harness's own default is 120).

  rfs-checkpoint.exe, rfs-harness.exe and ranvierautocomplete.exe are started directly from R's Release output, never
  under `dotnet`: ProjectSpec's toolsPath finds the wrong SDK under `dotnet` (see its comment), the likely reason
  large-body failed to load last round. The server is started from its apphost through a servers.final.json written
  next to the results (servers.json with `ranvier`'s command replaced); servers.json itself is not changed.

  Output:  results/baseline-fsac-4c33a2a-extra/   fsac-core-bg and fsac-core-tc, scenarios with no baseline
           results/server-final/                   ranvier on scenarios/*.json
           results/server-final/repeat/            ranvier on the body scenarios again
           results/server-final/compare-*.md       Step 4
           results/server-final/references-first.md  first references request of each *-references run against the rest (I5)
           results/server-final/run.log            scenario, engine, start, end, exit code (tab separated)
           results/server-final/logs/              each run's console output
           results/server-final/environment.txt    commits, SDK, machine
#>
[CmdletBinding()]
param(
    [switch]$DryRun,
    [switch]$Rebuild,
    [switch]$Small,
    [switch]$SkipHugeRepeat,
    [int]$StepTimeoutS = 300
)

$ErrorActionPreference = 'Continue'
$R = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
Set-Location $R   # the checkpoint refuses to start elsewhere, and the corpus clones' global.json files break loading from anywhere else
$corpus = (Resolve-Path (Join-Path $R '..\ranvier-fsharp-corpus')).Path
$F = Join-Path $R '..\fickle-fsharp'
$fsacDir = Join-Path $R '..\ranvier-fsharp-fsac'

$baseline = Join-Path $R 'results\baseline-fsac-4c33a2a'
$extra = Join-Path $R 'results\baseline-fsac-4c33a2a-extra'
$final = Join-Path $R 'results\server-final'
$repeat = Join-Path $final 'repeat'
$logs = Join-Path $final 'logs'
$runLog = Join-Path $final 'run.log'
$bin = Join-Path $R 'src\Rfs.Checkpoint\bin\Release\net10.0'
$harness = Join-Path $bin 'rfs-harness.exe'
$checkpoint = Join-Path $bin 'rfs-checkpoint.exe'
$serverExe = Join-Path $R 'src\Ranvier.AutoComplete\bin\Release\net10.0\ranvierautocomplete.exe'
$serversFinal = Join-Path $final 'servers.final.json'
$ranRank = @{ small = 0; mid = 1; large = 2; huge = 3 }
$started = Get-Date
$failed = [System.Collections.Generic.List[string]]::new()

function Write-Section([string]$text) { Write-Host ''; Write-Host "=== $text" -ForegroundColor Cyan }

function Get-Scenarios {
    # name, corpus, kind and step counts, small corpora first
    Get-ChildItem (Join-Path $R 'scenarios') -Filter '*.json' | ForEach-Object {
        $j = Get-Content $_.FullName -Raw | ConvertFrom-Json -Depth 30
        $edits = 0; $requests = 0; $opens = 0
        foreach ($s in $j.steps) {
            $key = ($s.PSObject.Properties | Select-Object -First 1).Name
            switch ($key) {
                'edit' { $n = if ($s.edit.repeat) { [int]$s.edit.repeat } else { 1 }; $edits += $n }
                { $_ -in 'hover', 'completion', 'definition', 'references' } { $requests++ }
                default { $opens++ }
            }
        }
        [pscustomobject]@{ Name = $j.name; File = $_.FullName; Corpus = $j.corpus; Kind = $j.kind; Edits = $edits; Requests = $requests; Opens = $opens }
    } | Sort-Object { $ranRank[$_.Corpus] }, Name
}

# Seconds for one run, from earlier results: ranvier small-body 67 s for 21 steps, FSAC bg large-body 84 s, tc 188 s, tc requests
# 14-21 s each on mid and large, the checkpoint's huge scenarios ~260 s per engine for 10 steps. Huge on FSAC has no earlier
# number (the guesses below are the weakest part of the estimate).
function Get-Cost($sc, [string]$engine) {
    $cold = @{ small = 8; mid = 8; large = 12; huge = 40 }[$sc.Corpus]
    $edit = @{ small = 2.9; mid = 2.9; large = 3.5; huge = 26 }[$sc.Corpus]
    $req = @{ small = 0.5; mid = 0.5; large = 0.5; huge = 2 }[$sc.Corpus]
    $open = @{ small = 1; mid = 1; large = 2; huge = 8 }[$sc.Corpus]
    switch ($engine) {
        'fsac-core-bg' { $cold *= 1.3; if ($sc.Corpus -eq 'huge') { $edit = 30 } }
        'fsac-core-tc' {
            $cold *= 2.5
            $edit = @{ small = 7.6; mid = 6.5; large = 8.9; huge = 60 }[$sc.Corpus]
            $req = @{ small = 0.7; mid = 21; large = 14; huge = 40 }[$sc.Corpus]
        }
    }
    10 + $cold + $sc.Edits * $edit + $sc.Requests * $req + $sc.Opens * $open
}

$allScenarios = @(Get-Scenarios)
if ($Small) {
    $largeCore = @('large-cold', 'large-body', 'large-signature', 'large-requests', 'large-references')
    $fsacExtraSkip = @('mid-openmany-edits', 'mid-eof-then-insert')
    $all = @($allScenarios | Where-Object { $_.Corpus -in 'small', 'mid' -or $_.Name -in $largeCore })
    $noBaseline = @($all | Where-Object { $_.Corpus -in 'small', 'mid' -and $_.Name -notin $fsacExtraSkip -and -not (Test-Path (Join-Path $baseline "fsac-core-bg.$($_.Name).json")) })
    $repeatSet = @($all | Where-Object { $_.Name -in 'small-body', 'mid-body', 'large-body' })
} else {
    $all = $allScenarios
    $noBaseline = @($all | Where-Object { -not (Test-Path (Join-Path $baseline "fsac-core-bg.$($_.Name).json")) })
    $repeatSet = @($all | Where-Object { $_.Kind -eq 'body' -and -not ($SkipHugeRepeat -and $_.Corpus -eq 'huge') })
}

function Show-Estimate {
    $rows = [System.Collections.Generic.List[object]]::new()
    foreach ($sc in $noBaseline) { foreach ($e in 'fsac-core-bg', 'fsac-core-tc') { $rows.Add([pscustomobject]@{ Phase = 'step2'; Run = "$e $($sc.Name)"; Sec = Get-Cost $sc $e }) } }
    foreach ($sc in $all) { $rows.Add([pscustomobject]@{ Phase = 'step3'; Run = "ranvier $($sc.Name)"; Sec = Get-Cost $sc 'ranvier' }) }
    foreach ($sc in $repeatSet) { $rows.Add([pscustomobject]@{ Phase = 'repeat'; Run = "ranvier $($sc.Name) (repeat)"; Sec = Get-Cost $sc 'ranvier' }) }
    $overhead = 600 # build, probes, compares
    $total = ($rows | Measure-Object Sec -Sum).Sum + $overhead
    Write-Section 'Estimate (from earlier durations; wall clock, quiet machine)'
    foreach ($g in $rows | Group-Object Phase) { '{0,-7} {1,5:N2} h  ({2} runs)' -f $g.Name, (($g.Group | Measure-Object Sec -Sum).Sum / 3600), $g.Count }
    '{0,-7} {1,5:N2} h  (build, probes, compares)' -f 'other', ($overhead / 3600)
    '{0,-7} {1,5:N2} h  total, likely {2:N1} to {3:N1} h' -f 'TOTAL', ($total / 3600), ($total / 3600 * 0.8), ($total / 3600 * 1.5)
    'dominated by:'
    $rows | Sort-Object Sec -Descending | Select-Object -First 8 | ForEach-Object { '  {0,5:N0} min  {1}  [{2}]' -f ($_.Sec / 60), $_.Run, $_.Phase }
    $hugeRepeat = ($rows | Where-Object { $_.Phase -eq 'repeat' -and $_.Run -match 'huge-' } | Measure-Object Sec -Sum).Sum
    if ($hugeRepeat) { '  repeats of huge scenarios: {0:N2} h of the total (-SkipHugeRepeat leaves them out)' -f ($hugeRepeat / 3600) }
}

function Show-Environment {
    Write-Section 'Versions'
    $rc = (git -C $R rev-parse --short HEAD)
    $rdirty = (git -C $R status --porcelain | Measure-Object).Count
    $fc = (git -C $F rev-parse --short HEAD)
    $fdirty = (git -C $F status --porcelain | Measure-Object).Count
    $ac = (git -C $fsacDir rev-parse --short HEAD)
    $fcsDll = 'C:\Users\shaya\RiderProjects\fickle-fsharp\artifacts\bin\FSharp.Compiler.Service\Release\net10.0\FSharp.Compiler.Service.dll'
    $cpu = (Get-CimInstance Win32_Processor | Select-Object -First 1)
    $mem = [math]::Round((Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory / 1GB)
    $lines = @(
        "R (ranvier-fsharp)  $rc  (uncommitted files: $rdirty)",
        "F (fickle-fsharp)   $fc  (uncommitted files: $fdirty)  fork FCS dll written $((Get-Item $fcsDll).LastWriteTime.ToString('s'))",
        "FSAC baseline       $ac  (expected 4c33a2a)",
        "SDK                 $(dotnet --version)  (global.json: $((Get-Content (Join-Path $R 'global.json') -Raw | ConvertFrom-Json).sdk.version))",
        "machine             $($cpu.Name.Trim()), $($cpu.NumberOfCores) cores / $($cpu.NumberOfLogicalProcessors) threads, $mem GB, $([System.Environment]::OSVersion.VersionString)",
        "started             $($started.ToString('s'))")
    $lines | ForEach-Object { Write-Host $_ }
    $busy = Get-Process -ErrorAction SilentlyContinue | Where-Object { $_.ProcessName -in 'dotnet', 'testhost', 'fsautocomplete', 'MSBuild', 'VBCSCompiler', 'rfs-checkpoint', 'rfs-harness', 'ranvierautocomplete', 'Rfs.Oracle' }
    if ($busy) {
        Write-Warning ("other processes that may load the machine: " + (($busy | ForEach-Object { "$($_.ProcessName)#$($_.Id)" }) -join ', '))
        $lines += "WARNING: running at start: " + (($busy | ForEach-Object { "$($_.ProcessName)#$($_.Id)" }) -join ', ')
    }
    return $lines
}

$envLines = Show-Environment
Show-Estimate

if ($DryRun) {
    Write-Section 'Plan (dry run, nothing is built or run)'
    "step 2 -> results\baseline-fsac-4c33a2a-extra : fsac-core-bg, fsac-core-tc on: " + (($noBaseline | ForEach-Object Name) -join ', ')
    "step 3 -> results\server-final                : ranvier on: " + (($all | ForEach-Object Name) -join ', ')
    "repeat -> results\server-final\repeat         : ranvier on: " + (($repeatSet | ForEach-Object Name) -join ', ')
    return
}

New-Item -ItemType Directory -Force $extra, $final, $repeat, $logs | Out-Null

function Add-RunLog([string]$scenario, [string]$engine, [datetime]$start, [datetime]$end, $code) {
    "{0}`t{1}`t{2}`t{3}`t{4}" -f $scenario, $engine, $start.ToString('s'), $end.ToString('s'), $code | Add-Content $runLog
}

function Stop-Here([string]$why, [int]$code) {
    Write-Host "STOP: $why" -ForegroundColor Red
    "# STOP: $why" | Add-Content $runLog
    exit $code
}

# --- build, once
Write-Section 'Build (Release)'
$buildMark = Join-Path $final 'environment.txt'
if ((Test-Path $buildMark) -and -not $Rebuild) {
    Write-Host "already built for this run ($buildMark exists); -Rebuild builds again"
} else {
    foreach ($project in 'src\Rfs.Checkpoint\Rfs.Checkpoint.fsproj', 'src\Rfs.Harness\Rfs.Harness.fsproj', 'src\Ranvier.AutoComplete\Ranvier.AutoComplete.fsproj') {
        & dotnet build (Join-Path $R $project) -c Release --nologo -v q
        if ($LASTEXITCODE -ne 0) { Stop-Here "build of $project failed" 4 }
    }
    $envLines | Set-Content $buildMark
}
foreach ($exe in $harness, $checkpoint, $serverExe) { if (-not (Test-Path $exe)) { Stop-Here "missing $exe" 4 } }

# servers.json with the server started from its apphost
$servers = Get-Content (Join-Path $R 'servers.json') -Raw | ConvertFrom-Json -Depth 30
foreach ($s in $servers) { if ($s.name -eq 'ranvier') { $s.command = $serverExe; $s.args = @() } }
$servers | ConvertTo-Json -Depth 30 | Set-Content $serversFinal

function Invoke-Harness($sc, [string]$server, [string]$outDir, [string]$label) {
    $result = Join-Path $outDir "$server.$($sc.Name).json"
    if (Test-Path $result) {
        Write-Host "skip $server on $($sc.Name): result exists"
        Add-RunLog $sc.Name $label (Get-Date) (Get-Date) 'skipped'
        return
    }
    Write-Host ("[{0:N1} min] {1} on {2}" -f ((Get-Date) - $started).TotalMinutes, $server, $sc.Name) -ForegroundColor Green
    $start = Get-Date
    & $harness run --scenario $sc.File --out $outDir --servers $serversFinal --server $server --corpus $corpus --step-timeout-s $StepTimeoutS 2>&1 |
        Tee-Object -FilePath (Join-Path $logs "$($label -replace '[^\w.-]', '_').$($sc.Name).log")
    $code = $LASTEXITCODE
    Add-RunLog $sc.Name $label $start (Get-Date) $code
    if ($code -ne 0 -or -not (Test-Path $result)) {
        $failed.Add("$label $($sc.Name) (exit $code$(if (-not (Test-Path $result)) { ', no result file' }))")
    }
}

# --- before anything else: large's project loads (it failed last round under `dotnet`)
Write-Section 'Probe: the large project loads, from the apphosts'
$probe = Join-Path $final 'probe'
if (Test-Path (Join-Path $probe 'ok')) {
    Write-Host 'already passed'
} else {
    New-Item -ItemType Directory -Force $probe | Out-Null
    $large = $all | Where-Object Name -eq 'large-cold'
    & $checkpoint --engine decl --scenario $large.File --out (Join-Path $probe 'checkpoint') --corpus $corpus --step-timeout-s $StepTimeoutS 2>&1 | Tee-Object (Join-Path $logs 'probe-checkpoint.log')
    if ($LASTEXITCODE -ne 0) { Stop-Here 'large does not load (rfs-checkpoint.exe --engine decl on large-cold)' 3 }
    $harnessOut = Join-Path $probe 'server'
    & $harness run --scenario $large.File --out $harnessOut --servers $serversFinal --server ranvier --corpus $corpus --step-timeout-s $StepTimeoutS 2>&1 | Tee-Object (Join-Path $logs 'probe-server.log')
    if ($LASTEXITCODE -ne 0 -or -not (Test-Path (Join-Path $harnessOut 'ranvier.large-cold.json'))) { Stop-Here 'large does not load in the server (ranvierautocomplete.exe on large-cold)' 3 }
    'ok' | Set-Content (Join-Path $probe 'ok')
}

# --- Step 2: FSAC on what has no baseline
Write-Section 'Step 2: fsac-core-bg and fsac-core-tc where the baseline has nothing'
foreach ($sc in $noBaseline) { foreach ($e in 'fsac-core-bg', 'fsac-core-tc') { Invoke-Harness $sc $e $extra $e } }

# --- Step 3: the server on everything, then the body scenarios again
Write-Section 'Step 3: ranvier on scenarios/*.json'
foreach ($sc in $all) { Invoke-Harness $sc 'ranvier' $final 'ranvier' }
Write-Section 'Step 3: the body scenarios again'
foreach ($sc in $repeatSet) { Invoke-Harness $sc 'ranvier' $repeat 'ranvier (repeat)' }

# --- Step 4: compares, ranvier against each FSAC mode, base and extra baselines together
Write-Section 'Step 4: compares'
$cmp = Join-Path $final 'cmp'
Remove-Item $cmp -Recurse -Force -ErrorAction SilentlyContinue
foreach ($e in 'ranvier', 'fsac-core-bg', 'fsac-core-tc') { New-Item -ItemType Directory -Force (Join-Path $cmp $e) | Out-Null }
Get-ChildItem $final -Filter 'ranvier.*.json' | Copy-Item -Destination (Join-Path $cmp 'ranvier')
foreach ($e in 'fsac-core-bg', 'fsac-core-tc') {
    foreach ($d in $baseline, $extra) { Get-ChildItem $d -Filter "$e.*.json" -ErrorAction SilentlyContinue | Copy-Item -Destination (Join-Path $cmp $e) }
    & $harness compare (Join-Path $cmp 'ranvier') (Join-Path $cmp $e) --out (Join-Path $final "compare-$e.md")
    if ($LASTEXITCODE -ne 0) { $failed.Add("compare ranvier $e (exit $LASTEXITCODE)") }
}

# --- I5: the first references request of a run, kept apart from the rest
Write-Section 'I5: first references request against the rest'
$md = @('# First references request against the rest (I5)', '',
    'The first request of a run reads file-level results of files the session did not open and keeps them; the others find them kept. Latencies in ms.', '',
    '| run | first | p50 of the other 9 | worst of the other 9 |', '|---|---:|---:|---:|')
$dirs = @($final, $repeat, $extra)
foreach ($d in $dirs) {
    Get-ChildItem $d -Filter '*-references.json' -ErrorAction SilentlyContinue | ForEach-Object {
        $j = Get-Content $_.FullName -Raw | ConvertFrom-Json
        $refs = @($j.steps | Where-Object { $_.step -like 'references *' })
        if ($refs.Count -gt 1) {
            $rest = @($refs | Select-Object -Skip 1 | ForEach-Object { [double]$_.latencyMs } | Sort-Object)
            $md += '| {0}/{1} | {2:N0} | {3:N0} | {4:N0} |' -f (Split-Path $d -Leaf), $_.Name, $refs[0].latencyMs, $rest[[math]::Ceiling($rest.Count / 2) - 1], $rest[-1]
        }
    }
}
$md | Set-Content (Join-Path $final 'references-first.md')

Write-Section 'Done'
('elapsed {0:N2} h' -f ((Get-Date) - $started).TotalHours)
if ($failed.Count -gt 0) {
    Write-Host 'Runs with a non-zero exit (failed steps, oracle diffs, a crash) or no result file:' -ForegroundColor Red
    $failed | ForEach-Object { Write-Host "  $_"; "# FAILED: $_" | Add-Content $runLog }
    exit 1
}
Write-Host 'every run exited 0'
