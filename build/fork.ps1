# Builds the FCS fork at its currently checked-out branch; mirrors fcs/build.sh without running fcs-test.
param(
    [string]$ForkDir = (Join-Path $PSScriptRoot '../../fickle-fsharp')
)
$ErrorActionPreference = 'Stop'

$ForkDir = [System.IO.Path]::GetFullPath($ForkDir)
if (-not (Test-Path $ForkDir)) {
    throw "FCS fork directory not found at '$ForkDir'. Clone https://github.com/shayanhabibi/fickle-fsharp (branch fickle_decl) alongside ranvier-fsharp or pass -ForkDir <path>."
}

# The fork's props only take the plain-dotnet (non-Arcade) path with this set.
$env:BUILDING_USING_DOTNET = 'true'
$env:DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER = '1'

$fcsDir = [System.IO.Path]::Combine($ForkDir, 'artifacts', 'bin', 'FSharp.Compiler.Service', 'Release', 'net10.0')
$fcsDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.Compiler.Service.dll')

Push-Location $ForkDir
try {
    foreach ($project in 'buildtools/fslex', 'buildtools/fsyacc', 'src/Compiler') {
        Write-Host "dotnet build -c Release $project"
        # The fork's global.json pins an SDK whose current TFM is net11.0; FickleFcsDir expects net10.0.
        # --no-incremental: the DLL is always rewritten, so its timestamp says when it was last built.
        dotnet build -c Release --no-incremental -p:FSharpNetCoreProductTargetFramework=net10.0 $project
        if ($LASTEXITCODE -ne 0) { throw "Build of $project failed with exit code $LASTEXITCODE" }
    }
}
finally {
    Pop-Location
}

if (-not (Test-Path $fcsDll)) { throw "Expected $fcsDll after the build, but it does not exist" }
Write-Host "OK: $fcsDll ($((Get-Item $fcsDll).LastWriteTime))"
