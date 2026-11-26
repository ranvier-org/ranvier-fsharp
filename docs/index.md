---
title: F# semantic tooling (rfs)
description: Incremental declaration-level F# compiler daemon, LSP server, CLI, and MCP server for fast semantic analysis and refactoring.
---

# F# semantic tooling (`rfs` & `ranvierautocomplete`)

Demand-driven, declaration-incremental F# Language Server Protocol (LSP) server (`ranvierautocomplete`) and AI coding agent CLI/MCP semantic analysis tool (`rfs` / `Ranvier.FSharp.Agent`), built on `Ranvier` and `fickle-fsharp` (`FSharp.Compiler.Service`).

`ranvierautocomplete` is a drop-in replacement for `fsautocomplete` (FSAC) in **Ionide for VS Code**, **Neovim**, **Helix**, **Zed**, **Emacs**, and any stdio LSP client. `rfs` brings the same incremental F# compiler graph to **Claude Code**, **Codex**, **Cursor**, and CLI agent workflows over a persistent per-workspace daemon with automatic stale-process reaping.

> **Acknowledgements:** `Ranvier.AutoComplete` builds squarely on the language server surface, protocol handling, workspace loading, analyzers, and editor feature conventions pioneered by the **[Ionide](https://ionide.io)** maintainers and contributors across `FsAutoComplete`, `Ionide.LanguageServerProtocol`, `Ionide.ProjInfo`, and `ionide-vscode-fsharp`. See [THIRD-PARTY-NOTICES.md](../THIRD-PARTY-NOTICES.md) for full attribution.

## Why declaration-incremental checking?

Traditional F# language servers re-typecheck entire files (and downstream files) on edits or retain heavy compiler `TcState` / `FSharpCheckFileResults` graphs across open buffers. `ranvier-fsharp` models F# parsing, dependency extraction, declaration-level type-checking, symbol uses, and cross-project signatures as a reactive `Ranvier` computation graph:

- **Declaration-level incremental checking**: Edits inside a function or binding body re-check only the affected declaration (`~1.7–12.6` declarations/edit instead of the whole file) and stop propagation immediately when the inferred signature is unchanged.
- **Zero compiler-object retention at rest**: Diagnostics, hovers, semantic classifications, symbol uses, and signature projections are extracted eagerly into compact immutable records; `FSharpCheckFileResults` are never retained on the heap.
- **Live in-memory cross-project propagation**: Unsaved buffer edits (in LSP) and saved disk edits (in `rfs`) propagate across `ProjectReference` boundaries in memory without rebuilding DLLs on disk.
- **Causal graph inspection (`rfs/why`)**: `.Traced` packages (`Ranvier.FSharp.Checker.Traced`, `Ranvier.FSharp.Server.Traced`) record causal node labels so callers can inspect the exact dependency path from changed inputs to any declaration's check.

## Packages and tools

| Package | Command / Namespace | Description |
| --- | --- | --- |
| `Ranvier.FSharp.Agent` | `rfs` (`.NET` Tool) | Semantic F# CLI + stdio MCP server backed by a shared incremental workspace daemon and stale-process reaper |
| `Ranvier.AutoComplete` | `ranvierautocomplete` (`.NET` Tool) | Drop-in `stdio` F# LSP server executable for Ionide and generic LSP editors |
| `Ranvier.FSharp.Lsp` | `Rfs.Lsp` | JSON-RPC framing, UTF-16 incremental document buffer, and `LspClient` |
| `Ranvier.FSharp.Projects` | `Rfs.Projects` | MSBuild `ProjectSpec` evaluation (`MSBuildLocator`), `.sln`/`.slnx` discovery, `Glob`, and bundled `fickle-fsharp` `FSharp.Compiler.Service` |
| `Ranvier.FSharp.Checker` | `Rfs.Checker` | Demand-driven declaration-incremental F# type-checking graph (`GraphHost`, `ProjectGraph`) + `.Traced` variant |
| `Ranvier.FSharp.Server` | `Rfs.Server` | Embeddable in-process F# LSP server (`Server.run`, `Server.runWith`, `RanvierServer`) + `.Traced` variant |

## Quick start: `rfs` (`Ranvier.FSharp.Agent`)

Install the `.NET 10` global tool:

```bash
dotnet tool install -g Ranvier.FSharp.Agent
rfs --version
```

Register `rfs` as an MCP server in `.mcp.json` (pass `["mcp", "--no-always-load"]` in small or mixed-language repositories where deferred tool search is preferred):

```json
{
  "mcpServers": {
    "rfs": {
      "command": "rfs",
      "args": ["mcp"]
    }
  }
}
```

Or invoke `rfs` directly from the CLI against the same shared background daemon:

```bash
rfs status
rfs check --fail-on-error
rfs symbol src/MyLib/Library.fs 42 --symbol compute
rfs refs src/MyLib/Library.fs 42 --symbol compute
rfs search --file src/MyLib/Library.fs
rfs rename src/MyLib/Library.fs 42 computeNext --symbol compute --dry-run
```

## Quick start: `ranvierautocomplete` (LSP editor setup)

Install the LSP server global tool:

```bash
dotnet tool install -g Ranvier.AutoComplete
ranvierautocomplete --version
```

- **VS Code (Ionide for F#)**: Set `"FSharp.fsac.netCoreDllPath"` in `settings.json` to `ranvierautocomplete.dll` (from the portable release archive or `.dotnet/tools/.store/ranvier.autocomplete/.../ranvierautocomplete.dll`) and enable `"editor.semanticHighlighting.enabled": true`.
- **Neovim / Helix / Emacs / Zed**: Configure `ranvierautocomplete` as your `fsharp` language server command with `"AutomaticWorkspaceInit": true` in `initializationOptions` so `.slnx`, `.sln`, and `.fsproj` workspaces load automatically on startup.

## Guides in this section

- **[`Ranvier.FSharp.Agent` (`rfs`) operator guide](rfs.md)**: Full installation, dual CLI/MCP daemon architecture, command and MCP tool tables, workspace resolution, environment variables, and troubleshooting.
- **[`rfs` agent operational reference](rfs-agent.md)**: Concise loop discipline, 1-based `--symbol` addressing, tool/CLI mapping, and mandatory guardrails for AI coding agents.
- **[Colour swatches](colors.md)**: Which colour literals `ranvierautocomplete` reports, in RGB and seven other colour spaces, with hover previews, picker write-back and every `FSharp.colors.*` setting.
- **[MCP server vs. stdio CLI & persistent daemon study](agent-mcp-vs-stdio.md)**: Transport microbenchmarks, stale-process reaper verification, and 28-trial competing-weightings evaluation across small, medium, and large F# repositories.
