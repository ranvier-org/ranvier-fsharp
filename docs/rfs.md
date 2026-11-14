# `Ranvier.FSharp.Agent` (`rfs`) — Full Documentation

`Ranvier.FSharp.Agent` installs the `rfs` CLI tool and Model Context Protocol (MCP) server for AI coding agents working in F# repositories. Both interfaces share a single persistent, declaration-incremental workspace daemon built on [`Ranvier.FSharp.Checker`](../README.md) (`ProjectGraph`) with automatic stale-process reaping.

---

## 1. Architecture Overview

- **Dual frontend, one compiler daemon**:
  - `rfs <command>` (Stdio CLI) spawns a short-lived client that connects to the workspace daemon over a Named Pipe (`\\.\pipe\rfs-daemon-...` on Windows) or Unix domain socket (`rfs-daemon-...` on Linux/macOS), auto-starting the daemon on first call if none is alive.
  - `rfs mcp` speaks MCP JSON-RPC (`2025-11-25` via `ModelContextProtocol.Core`) over stdio, pre-warms the same workspace daemon asynchronously without blocking `initialize`, and registers `6` semantic tools (`rfs_check`, `rfs_symbol`, `rfs_references`, `rfs_search`, `rfs_rename`, `rfs_status`).
- **Declaration-incremental engine**:
  - Edits inside a function or binding body re-check only the affected declaration when the inferred signature is unchanged, propagating cross-project signature updates in memory without disk DLL rebuilds.
  - Files touched by `check`, `symbol`, `refs`, or `rename` are synced from disk by timestamp, length, and content hash and promoted to declaration-level checking (`maxPromotedPerProject = 24`).
- **Deterministic stale-process reaping**:
  - Every running daemon and MCP bridge registers its PID, `ProcessStartTimeUtc`, parent PID/start time, workspace root hash, binary stamp, and last activity timestamp under `RFS_STATE_DIR`.
  - Both PID and process start time are checked before signaling so recycled OS PIDs are never killed (`RecycledPidCleaned`).
  - Stale processes whose `rfs.dll` timestamp/size changed (`BinaryStampMismatch`), whose MCP parent died (`OrphanedMcpParent`), or whose idle TTL expired (`IdleTimeoutExpired`) are cleaned automatically.

---

## 2. Prerequisites & Installation

### Prerequisites

- **`.NET 10.0` SDK (`10.0.x`)**: `rfs` hosts MSBuild in-process via `MSBuildLocator` to evaluate `.fsproj` and `.slnx`/`.sln` graphs. Verify:
  ```bash
  dotnet --list-sdks
  ```
- **Initial restore / build (fresh checkouts)**: Ensure NuGet assets (`obj/project.assets.json`) exist by running `dotnet restore` (or one initial `dotnet build`) before expecting `0` project-load failures.

### Install as a `.NET` Global Tool (Recommended)

From NuGet.org:
```bash
dotnet tool install -g Ranvier.FSharp.Agent
```

Verify:
```bash
rfs --version
```

### Install as a Repo-Local `.NET` Tool

```bash
dotnet new tool-manifest   # skip if .config/dotnet-tools.json already exists
dotnet tool install Ranvier.FSharp.Agent
dotnet rfs --version
```

### Standalone Release Archives & Local Pack

Prebuilt framework-dependent archives are attached to GitHub Releases (`rfs-portable-net10.0.zip`, `rfs-win-x64-net10.0.zip`, `rfs-linux-x64-net10.0.zip`, `rfs-osx-arm64-net10.0.zip`). From source:
```powershell
pwsh -File build/pack.ps1 -Configuration Release -AgentOnly
dotnet tool install -g Ranvier.FSharp.Agent --add-source ./artifacts/nupkg
```

### Agent Skills & Minimal Agent Instructions

- Model-invoked F# usage skill (`<200` words): [skills/rfs/SKILL.md](../skills/rfs/SKILL.md)
- Guided setup skill (`<300` words): [skills/setup-rfs/SKILL.md](../skills/setup-rfs/SKILL.md)
- Purely agent-relevant operational reference (no marketing or benchmark prose): [docs/rfs-agent.md](rfs-agent.md)

---

## 3. Choosing Integration Mode: Competing Weightings Benchmark

Across a `28`-trial benchmark using `claude --model haiku` across Small (`QuestPDF.FSharp`, `5` projects), Medium (`SageTUI`, `9` projects), and Large (`ranvier-fsharp`, `14` projects) F# repositories on Workflow A (read-only type/reference analysis) and Workflow B (`edit -> check -> fix` cycles), we evaluate two competing objectives via a dimensionless Cobb-Douglas score (with reference constants $T_0 = 30\text{ s/task}$, $C_0 = \$0.006\text{/task}$, which cancel identically in every ratio against `no-tool`):

$$
V(w_T, w_C) = 100 \cdot Q \cdot \left(\frac{T_0}{T}\right)^{w_T} \cdot \left(\frac{C_0}{C}\right)^{w_C}
$$

- **Time-Weighted ($V_{80T}$)**: `80%` weight on wall-clock time ($w_T = 0.80$), `20%` weight on USD cost ($w_C = 0.20$).
- **Cost-Weighted ($V_{80C}$)**: `20%` weight on wall-clock time ($w_T = 0.20$), `80%` weight on USD cost ($w_C = 0.80$).
- **Timeout lower bound (`*`)**: On `ranvier-fsharp` Workflow B, `baseline-no-rfs` hit the `240.12 s` timeout without emitting usage JSON; cost and tokens are imputed conservatively as the minimum observed on `ranvier-fsharp` B (`$0.00800` / `325,888` tokens), so all Workflow B and suite-wide ratios vs. `no-tool` are conservative lower bounds.

### Full 6-Cell Suite Totals (Warm vs. `no-tool`)

| Arm | Mean Score | Total Time | Total Tokens | Total Cost (USD) | $V_{80T}$ (vs. `no-tool`) | $V_{80C}$ (vs. `no-tool`) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| `baseline-no-rfs` (`no-tool`) | `0.994 (1.00x)` | `389.91s (1.00x)` | `1,297,269* (1.00x)` | `$0.03973* (1.00x)` | `52.51 (1.00x)` | `78.70 (1.00x)` |
| `stdio-daemon` (`rfs` CLI) | `0.983 (0.99x)` | `215.27s (0.55x)` | **`1,143,324 (0.88x)`** | `$0.04167 (1.05x)` | `82.76 (1.58x)` | **`84.40 (1.07x)` (#1)** |
| `mcp-daemon-always` (`rfs mcp`) | `0.981 (0.99x)` | **`195.08s (0.50x)`** | `1,443,785 (1.11x)` | `$0.05018 (1.26x)` | **`86.07 (1.64x)` (#1)** | `74.02 (0.94x)` |
| `mcp-daemon-deferred` (`--no-always-load`) | **`0.994 (1.00x)`** | `234.88s (0.60x)` | `1,406,807 (1.08x)` | `$0.04414 (1.11x)` | `77.13 (1.47x)` | `80.07 (1.02x)` |

### Workflow Subtotals & Large-Repo (`ranvier-fsharp`) Highlights

| Slice | Arm | Time (vs. `no-tool`) | Cost (vs. `no-tool`) | $V_{80T}$ (vs. `no-tool`) | $V_{80C}$ (vs. `no-tool`) |
| :--- | :--- | :---: | :---: | :---: | :---: |
| **Workflow B Subtotal** (`3` repos, warm) | `stdio-daemon` | `165.21s (0.49x)` | `$0.02392 (1.03x)` | `58.11 (1.75x)` | **`70.55 (1.12x)` (#1)** |
| **Workflow B Subtotal** (`3` repos, warm) | `mcp-daemon-always` | **`137.55s (0.41x)`** | `$0.02680 (1.16x)` | **`65.77 (1.98x)` (#1)** | `66.81 (1.06x)` |
| **Large Read-Only** (`ranvier-fsharp` A, warm) | `stdio-daemon` | **`17.87s (0.79x)`** | **`$0.00594 (0.74x)`** | **`146.14 (1.28x)` (#1)** | **`107.76 (1.33x)` (#1)** |
| **Large Edit Loop** (`ranvier-fsharp` B, warm) | `mcp-daemon-always` | **`37.46s (0.16x)`** | `$0.01065 (1.33x)` | **`74.64 (4.17x)` (#1)** | `60.45 (1.15x)` |
| **Large Edit Loop** (`ranvier-fsharp` B, warm) | `stdio-daemon` | `71.90s (0.30x)` | **`$0.00800 (1.00x)`** | `46.91 (2.62x)` | **`66.68 (1.27x)` (#1)** |
| **Large Edit Loop** (`ranvier-fsharp` B, cold) | `mcp-daemon-always` | **`38.85s (0.16x)`** | `$0.01308 (1.63x)` | **`69.58 (3.89x)` (#1)** | `50.90 (0.97x)` |

### Practical Mode Selection

- **Interactive F# coding / Time-Weighted (`rfs mcp`)**: Sets `anthropic/alwaysLoad: true` on all `6` tools (`~761` schema tokens) so smaller/faster models immediately use `rfs_check` instead of spawning `dotnet build`.
- **Small or mixed-language repos (`rfs mcp --no-always-load`)**: Omits `anthropic/alwaysLoad` so MCP clients using deferred tool search only pull `rfs_*` schemas when F# tasks occur (`1.09x` $V_{80T}$ on `QuestPDF.FSharp` A, `1.18x` $V_{80C}$ on `QuestPDF.FSharp` B).
- **Automated swarms / Cost-Weighted (`rfs` CLI)**: `0` MCP schema tokens on non-F# turns and lowest token cost (`0.88x` total tokens overall, `1.28x` $V_{80T}$ / `1.33x` $V_{80C}$ on large read-only analysis, `1.27x` $V_{80C}$ on large edit loops).

Full per-trial measurements and latency breakdowns are in [docs/agent-mcp-vs-stdio.md](agent-mcp-vs-stdio.md).

---

## 4. MCP Client Configuration

### Global Tool (`.mcp.json`)

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

### Deferred Tool-Search Mode (Small / Mixed-Language Repos)

```json
{
  "mcpServers": {
    "rfs": {
      "command": "rfs",
      "args": ["mcp", "--no-always-load"]
    }
  }
}
```

### Repo-Local Tool (`.config/dotnet-tools.json`)

```json
{
  "mcpServers": {
    "rfs": {
      "command": "dotnet",
      "args": ["tool", "run", "rfs", "--", "mcp"]
    }
  }
}
```

---

## 5. CLI Command Reference

Global option accepted before or between arguments: `--workspace <dir>` / `-w <dir>` / `--workspace=<dir>`.
All file line/column arguments are **1-based** and can be written either as `<file> <line> ...` or inline as `<file>:<line>[:<col>]` (Windows drive colons like `C:\...` are handled safely).

| Command | Syntax & Flags | Behavior |
| :--- | :--- | :--- |
| `check` | `rfs check [<file-or-project>] [--file <f>] [--project <name>] [--errors-only \| --all] [--no-downstream] [--fail-on-error]` | Incremental compiler diagnostics from disk; checks referencing projects unless `--no-downstream` is passed. Default severity is `warning` (`error` + `warning`). Exits `1` on diagnostics when `--fail-on-error` is passed. |
| `symbol` | `rfs symbol <file>[:<line>[:<col>]] [<line>] [<symbol-or-col>] [--symbol <name>] [--col <n>]` | Returns qualified symbol name, resolved 1-based location, inferred type signature, XML doc summary, definition, type definition, and implementations. |
| `refs` | `rfs refs <file>[:<line>[:<col>]] [<line>] [<symbol-or-col>] [--symbol <name>] [--col <n>] [--uses-only \| --incoming \| --outgoing] [--max <n>]` | Alias: `references`. Workspace-wide uses and/or call hierarchy (`mode`: `all`, `uses`, `incoming`, `outgoing`; default `max` = `100`). |
| `search` | `rfs search [<query>] [--file <file>] [--max <n>]` | With `--file`, returns hierarchical file outline (optionally filtered by `query`); otherwise searches workspace symbols across loaded projects (`max` = `50`). |
| `rename` | `rfs rename <file>[:<line>[:<col>]] [<line>] [<old-symbol>] <new-name> [--symbol <name>] [--col <n>] [--dry-run \| --apply]` | Semantically renames a symbol across loaded projects, applying edits to disk by default (use `--dry-run` to preview edits without writing) and reporting post-rename error count. |
| `status` | `rfs status [--reload \| -r] [--json]` | Reports workspace ID, daemon PID, uptime, working set MB, loaded/failed project count, source file count, promoted files, and Ranvier counters. `--reload` forces full workspace reload. |
| `mcp` | `rfs mcp [--workspace <dir>] [--no-always-load]` | Runs the MCP stdio server over newline-delimited JSON-RPC. |
| `daemon` | `rfs daemon status` \| `reap [--force] [--include-lsp]` \| `kill [--all]` \| `run [--idle-timeout <sec>] [--foreground]` | Inspects leases/processes, reaps stale/orphaned records, shuts down one workspace daemon or all daemons (`--all`), or runs the daemon process directly. |
| Meta | `rfs --version`, `rfs --help`, `rfs help` | Prints installed `Ranvier.FSharp.Agent` version or CLI usage. |

---

## 6. MCP Tool Reference

All 6 MCP tools auto-sync changed files from disk and reuse the persistent workspace daemon.

| MCP Tool | Required Params | Optional Params & Defaults | Output |
| :--- | :--- | :--- | :--- |
| `rfs_check` | *(none)* | `file`, `project`, `severity` (`"warning"`, or `"error"` / `"all"`), `downstream` (`true`) | Sorted diagnostics `relPath:line:col severity code: message` + summary line (`N errors, M warnings (...)`). |
| `rfs_symbol` | `file`, `line` | `column` (1-based), `symbol` (identifier/operator name; scans ±5 lines, disambiguates ties by closest `column` or first match) | `symbol`, `type`, optional `doc`, `definition`, `typeDefinition`, `implementations`. |
| `rfs_references` | `file`, `line` | `column`, `symbol`, `mode` (`"all"`, `"uses"`, `"incoming"`, `"outgoing"`), `maxResults` (`100`, max `500`) | Grouped cross-project uses and/or incoming/outgoing calls with line snippets. |
| `rfs_search` | *(none)* | `query`, `file`, `maxResults` (`50`, max `500`) | Hierarchical syntax outline (if `file` is given) or workspace-wide symbol search. |
| `rfs_rename` | `file`, `line`, `newName` | `column`, `symbol`, `apply` (`true`; set `false` for dry-run preview) | Touched file count, edited locations, and immediate post-rename diagnostic summary. |
| `rfs_status` | *(none)* | `reload` (`false`) | Workspace root/ID, daemon PID/RAM, project load status, and `ProjectGraph` counters. |

---

## 7. Daemon Lifecycle, Workspace Routing & Environment Variables

### Workspace Resolution

When `--workspace` is omitted, `rfs` walks upward from the target file (or working directory) stopping at the `.git` file or directory boundary:
- Prefers the outermost directory inside the git repo containing `*.slnx` or `*.sln`.
- Otherwise falls back to the `.git` root (so every `git worktree` gets its own isolated workspace ID and daemon).
- Outside a git repository, falls back to the outermost `.fsproj` directory or the starting directory.

Override auto-discovery with a semicolon- or comma-separated list of project paths in `RFS_PROJECTS` when needed.

### Environment Variables

| Variable | Default | Purpose |
| :--- | :--- | :--- |
| `RFS_STATE_DIR` | `%LOCALAPPDATA%\ranvier-fsharp\rfs-agent` (Windows) or `$XDG_RUNTIME_DIR` / `/tmp/ranvier-fsharp/rfs-agent-<uid>` | Directory storing daemon leases, process records, and lock files. Override in CI or tests to isolate daemons. |
| `RFS_IDLE_TIMEOUT_SECONDS` | `1800` (`30` minutes) | Seconds without CLI/MCP activity or active MCP leases before the daemon exits cleanly. |
| `RFS_PROJECTS` | *(unset)* | Explicit `;`- or `,`-delimited list of `.fsproj` paths to load instead of `WorkspacePeek`. |

---

## 8. Troubleshooting

- **`projects: 0 loaded, N failed` on a fresh clone**: Run `dotnet restore` (or one initial `dotnet build`), then run `rfs status --reload`.
- **Newly added `.fs` file or NuGet package is not recognized**: Call `rfs status --reload` (`rfs_status(reload=true)`) after saving `.fsproj` or `Directory.Packages.props`.
- **Wrong worktree analyzed**: Ensure file paths point inside the active git worktree or pass `--workspace <worktree-root>`.
- **Multiple or stale daemons after an abrupt kill**: Run `rfs daemon status` to inspect active leases and `rfs daemon reap` to clean dead leases and orphaned MCP processes safely without disturbing unrelated live processes.
- **Upgrading `Ranvier.FSharp.Agent`**: Run `dotnet tool update -g Ranvier.FSharp.Agent`. Running daemons built from an older `rfs.dll` are automatically replaced on the next command via binary-stamp mismatch detection.
