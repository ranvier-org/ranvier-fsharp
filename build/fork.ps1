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
$fcsXml = [System.IO.Path]::Combine($fcsDir, 'FSharp.Compiler.Service.xml')
$depMgrDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.DependencyManager.Nuget.dll')
$depMgrXml = [System.IO.Path]::Combine($fcsDir, 'FSharp.DependencyManager.Nuget.xml')
$fsharpCoreDll = [System.IO.Path]::Combine($fcsDir, 'FSharp.Core.dll')
$fsharpCoreXml = [System.IO.Path]::Combine($fcsDir, 'FSharp.Core.xml')

Push-Location $ForkDir
try {
    # `src/FSharp.Build` must be restored and built before `src/Compiler` (`UseBootstrappedFSharpBuildTasks=true` invokes it
    # via <MSBuild> without running Restore). `CopyLocalLockFileAssemblies=true` + `CopyDocumentationFilesFromPackages=true`
    # places the referenced `FSharp.Core.dll` and `FSharp.Core.xml` (10.0.x) alongside `FSharp.Compiler.Service.dll`.
    foreach ($project in 'buildtools/fslex', 'buildtools/fsyacc', 'src/FSharp.Build') {
        Write-Host "dotnet build -c Release $project"
        dotnet build -c Release --no-incremental -p:FSharpNetCoreProductTargetFramework=net10.0 $project
        if ($LASTEXITCODE -ne 0) { throw "Build of $project failed with exit code $LASTEXITCODE" }
    }

    Write-Host "dotnet build -c Release src/Compiler"
    dotnet build -c Release --no-incremental `
        -p:FSharpNetCoreProductTargetFramework=net10.0 `
        -p:CopyLocalLockFileAssemblies=true `
        -p:CopyDocumentationFilesFromPackages=true `
        src/Compiler
    if ($LASTEXITCODE -ne 0) { throw "Build of src/Compiler failed with exit code $LASTEXITCODE" }
}
finally {
    Pop-Location
}

foreach ($expected in $fcsDll, $fcsXml, $depMgrDll, $depMgrXml, $fsharpCoreDll, $fsharpCoreXml) {
    if (-not (Test-Path $expected)) { throw "Expected $expected after the build, but it does not exist" }
    Write-Host "OK: $expected ($((Get-Item $expected).LastWriteTime))"
}
