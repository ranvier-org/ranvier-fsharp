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
$fsharpCoreDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.Core.dll')

Push-Location $ForkDir
try {
    # `src/FSharp.Build` must be restored and built before `src/Compiler` (`UseBootstrappedFSharpBuildTasks=true` invokes it
    # via <MSBuild> without running Restore). `-p:FSHARPCORE_USE_PACKAGE=false` ensures the fork's `FSharp.Core.dll` is built
    # and placed alongside `FSharp.Compiler.Service.dll` in `artifacts/bin/FSharp.Compiler.Service/Release/net10.0/`.
    foreach ($project in 'buildtools/fslex', 'buildtools/fsyacc', 'src/FSharp.Build', 'src/Compiler') {
        Write-Host "dotnet build -c Release $project"
        $extraProps = if ($project -eq 'src/Compiler') { @('-p:FSHARPCORE_USE_PACKAGE=false') } else { @() }
        dotnet build -c Release --no-incremental -p:FSharpNetCoreProductTargetFramework=net10.0 @extraProps $project
        if ($LASTEXITCODE -ne 0) { throw "Build of $project failed with exit code $LASTEXITCODE" }
    }
}
finally {
    Pop-Location
}

if (-not (Test-Path $fcsDll)) { throw "Expected $fcsDll after the build, but it does not exist" }
if (-not (Test-Path $fsharpCoreDll)) { throw "Expected $fsharpCoreDll after the build, but it does not exist" }
Write-Host "OK: $fcsDll ($((Get-Item $fcsDll).LastWriteTime))"
Write-Host "OK: $fsharpCoreDll ($((Get-Item $fsharpCoreDll).LastWriteTime))"
