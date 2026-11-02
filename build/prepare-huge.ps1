# Prepares the dotnet/fsharp corpus clone (the current directory) for corpus "huge". Run by fetch-corpus.ps1 as its prepare.
$ErrorActionPreference = 'Stop'

# The repo targets net11.0, which the harness's MSBuild (this repo's SDK 10) cannot evaluate. Directory.Build.props.user
# is the repo's own gitignored hook, imported by every evaluation, so Ionide.ProjInfo sees net10.0 too.
Set-Content 'Directory.Build.props.user' @'
<Project>
  <PropertyGroup>
    <FSharpNetCoreProductTargetFramework>net10.0</FSharpNetCoreProductTargetFramework>
    <FSharpNetCoreProductMajorVersion>10</FSharpNetCoreProductMajorVersion>
  </PropertyGroup>
</Project>
'@

# As build/fork.ps1 builds the fork: the plain-dotnet (non-Arcade) path. Debug, because ProjectSpec passes the Debug
# reference assemblies (FSharp.DependencyManager.Nuget) and the generated lexer, parser and FSComp sources.
# src/Compiler builds src/FSharp.Build for its resource tasks with an MSBuild call that does not restore it, so it goes
# first on its own.
$env:BUILDING_USING_DOTNET = 'true'
foreach ($project in 'buildtools/fslex', 'buildtools/fsyacc', 'src/FSharp.Build', 'src/Compiler') {
    Write-Host "dotnet build -c Debug $project"
    dotnet build -c Debug -p:FSharpNetCoreProductTargetFramework=net10.0 -v q -nologo $project
    if ($LASTEXITCODE -ne 0) { throw "Build of $project failed with exit code $LASTEXITCODE" }
}
