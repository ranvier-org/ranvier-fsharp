# ranvier-fsharp

Benchmark harness and prototype checker for the Ranvier-based F# language server.

## SDK pin

`global.json` pins .NET SDK `10.0.403` (`rollForward: latestFeature`, so any 10.0.4xx works).
The projects target net10.0 and Ionide.ProjInfo loads MSBuild from the resolved SDK into that
host process. If a newer preview SDK (for example 11.0.100-rc) is picked, its MSBuild needs
`System.Runtime 11.0.0.0`, which the net10.0 host does not have, and project loading fails with
`FileNotFoundException`.

If 10.0.403 or a later 10.0.4xx is not installed, `dotnet` refuses to build. Install a 10.0.4xx SDK,
or edit `global.json` to the closest installed 10.0 SDK (keep the major version at 10).

## Layout notes

- Every project uses the FCS fork's `FSharp.Core.dll` (see `Directory.Build.props`), never the
  FSharp.Core package. `tests/fixtures/*` are stand-alone and use the regular package.
- Project specs are cached in `<project>/obj/rfs-spec.json` (format-versioned).
