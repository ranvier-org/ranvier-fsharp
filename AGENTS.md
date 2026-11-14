# AGENTS.md

## F# Semantic Analysis Inner Loop (`rfs`)

If `rfs` (`Ranvier.FSharp.Agent`) is connected in this environment (MCP tools `rfs_*` or CLI `rfs`), read [`docs/rfs-agent.md`](docs/rfs-agent.md) (and [`skills/rfs/SKILL.md`](skills/rfs/SKILL.md)) and use `rfs` for F# type inspection, cross-project references, semantic renames, and incremental post-edit diagnostics. Reserve `grep` for prose/config/scripts, and keep `dotnet build` + unfiltered `dotnet run --project tests/...` as the final verification gate.

## Repository Build & Test Invariants

- **FCS Fork Dependency (`FickleFcsDir`)**: Projects resolve `FSharp.Compiler.Service.dll` from `../fickle-fsharp/artifacts/bin/FSharp.Compiler.Service/Release/net10.0/` by default (see `Directory.Build.props`). Pass `-p:FickleFcsDir=/path/to/net10.0/` if the fork lives elsewhere.
- **Traced vs. Untraced Builds**: `Debug` compiles with `RanvierTrace=true` (`RANVIER_TRACE` defined) against `Ranvier.Traced`; `Release` builds untraced against `Ranvier`. Never pack untraced-only packages (`Ranvier.FSharp.Lsp`, `Ranvier.FSharp.Projects`, `Ranvier.AutoComplete`, `Ranvier.FSharp.Agent`) in `Debug` (`RefuseTracedPack` target enforces this).
- **Packaging & Verification**:
  - Build, publish, and pack all `8` NuGet packages with `pwsh -NoProfile -File build/pack.ps1 -Configuration Release` (`-AgentOnly` for `Ranvier.FSharp.Agent` alone, `-ToolOnly` for `Ranvier.AutoComplete` alone).
  - Verify `.nupkg` contents and isolated consumer smoke tests with `pwsh -NoProfile -File build/verify-packages.ps1 -NupkgDir ./artifacts/nupkg`.
  - Keep `README.md` Ionide/`FsAutoComplete` attribution and `THIRD-PARTY-NOTICES.md` license notices intact (`verify-packages.ps1` asserts them).
  - Do not rename `.github/workflows/publish.yml` without updating the NuGet.org Trusted Publishing policy (`ranvier-org/ranvier-fsharp`, owner `shayan.habibi`).
- **Tests**: Run Expecto suites unfiltered in `Release` (`tests/Rfs.Agent.Tests`, `tests/Rfs.Lsp.Tests`, `tests/Rfs.Harness.Tests`, `tests/Rfs.Checkpoint.Tests`, `tests/Rfs.Decl.Tests`, `tests/Rfs.Checker.Tests`, `tests/Rfs.Server.Tests`). When running `Rfs.Agent.Tests` or ad-hoc `rfs` checks, isolate daemon state with `RFS_STATE_DIR` and verify zero lingering `rfs` processes remain afterward.
