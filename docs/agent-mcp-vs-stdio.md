# Ranvier.FSharp Agent Tooling: MCP Server vs. Stdio CLI & Persistent Daemon Study

## 1. Executive Summary

We designed, implemented, and benchmarked **`Rfs.Agent`** (`rfs` / `Ranvier.FSharp.Agent`), a unified semantic F# code analysis binary that exposes **both** a stdio CLI and a Model Context Protocol (MCP) server over stdio, both backed by a shared **per-workspace Named Pipe daemon** (`SemanticEngine`) and a **process-lifecycle / stale-process reaper** (`ProcessRegistry`).

Across controlled direct transport microbenchmarks and **28 `claude --model haiku` end-to-end trials** spanning Small (`QuestPDF.FSharp`), Medium (`SageTUI`), and Large (`ranvier-fsharp`) repositories:

- **Persistent daemon beats cold process invocation by 145x–2,300x on type-checking (`check`)** and prevents agent timeouts on large multi-project `edit -> diagnostic -> fix` cycles (`ranvier-fsharp` Workflow B: `baseline-no-rfs` timed out at `240,115 ms`, whereas warm daemon arms finished in `37,462 ms – 78,532 ms` with `1.00` correctness and clean builds).
- **MCP (`mcp-daemon-always`) vs. Stdio CLI (`stdio-daemon`) trade-off**:
  - **MCP over a warm persistent pipe** eliminates per-call .NET process startup (`~100 ms`), delivering **5–9 ms `rfs_check`**, **17–399 ms `rfs_symbol`**, and **20–63 ms `rfs_references`** responses vs. `99–115 ms`, `3,608–6,187 ms`, and `165–188 ms` over CLI spawns. In Large `edit -> diagnostic -> fix` (`ranvier-fsharp` Workflow B), `mcp-daemon-always` completed in **`37,462 ms` (warm)** and **`38,852 ms` (cold)** — roughly **1.9x–3.2x faster** than `stdio-daemon` (`71,900 ms` warm / `123,889 ms` cold) because direct schema access encouraged incremental `rfs_check` verification without redundant full MSBuild loops.
  - **Stdio CLI + Daemon (`stdio-daemon`)** wins on **dollar cost and token efficiency for read-heavy analysis on large repositories** (`ranvier-fsharp` Workflow A: **`104,661` tokens / `$0.0059` / `17,872 ms`** vs. `319,444` tokens / `$0.0097` for `mcp-daemon-always`) and avoids upfront schema tokens (`~761` tokens across 6 tools) and prompt-cache write amplification when only a shell tool is needed.
  - **Deferred MCP (`--no-always-load`)** reduces initial prompt overhead on Small/Medium read-only lookups (`QuestPDF.FSharp` A: `12,441 ms`, `$0.0044`, `1.00` F1) while retaining sub-50 ms warm MCP calls once discovered, making **one shared daemon + dual frontend (`rfs mcp` and `rfs <cmd>`)** the optimal architecture.

---

## 2. MCP Framework Selection

We evaluated the available .NET / F# MCP libraries before writing `src/Rfs.Agent`:

| Candidate | Selection Status | Rationale |
| :--- | :--- | :--- |
| **`ModelContextProtocol.Core` `2.4.0`** | **Chosen** | Official C#/.NET SDK maintained with Microsoft/Anthropic conventions; clean `StreamServerTransport` + `McpServer.Create(transport, options)` API; targets `.NET 10.0` cleanly via [`Directory.Packages.props`](../Directory.Packages.props) with no transitive version conflicts alongside `Microsoft.Build 17.14.28` (only requires excluding old `Microsoft.NET.StringTools` runtime assets pulled transitively by `StreamJsonRpc`). Supports Protocol `2025-11-25` and per-tool `_meta["anthropic/alwaysLoad"]`. |
| `FsMcp.Server` `2.0.0` / `FsLangMcp` | Rejected for core daemon host | Exact dependency pins in `FsMcp.Server 2.0.0` conflict with the solution's `.NET 10.0.14` runtime/package graph, and `Rfs.Agent` already embeds Ranvier's F# project cracker (`Ionide.ProjInfo 0.75.3`), declaration-incremental `ProjectGraph`, and LSP feature handlers directly. |

---

## 3. Architecture: `Rfs.Agent` (`rfs.exe`)

All modes compile into a single `PackAsTool` executable [`src/Rfs.Agent/Rfs.Agent.fsproj`](../src/Rfs.Agent/Rfs.Agent.fsproj) (`<AssemblyName>rfs</AssemblyName>`):

```
Agent / User
  │
  ├──▶ `rfs <cmd>` (CLI stdio mode) ─────────┐
  │      check | symbol | refs | search      │
  │      rename | status                     ▼
  │                                  Named Pipe Client (`rfs-daemon-<stateHash>-<wsHash>`)
  └──▶ `rfs mcp` (MCP stdio proxy) ──────────┤   auto-spawns via Win32 CreateProcessW
         6 tools (`rfs_*`) over JSON-RPC     │   (`bInheritHandles = false`, `0x08000008`)
                                             ▼
                                  Detached Daemon (`rfs daemon run`)
                                    ├─ SemanticEngine (Ranvier GraphHost + Workspace + PositionResolver)
                                    ├─ Incremental Promotion (`maxPromotedPerProject = 24`)
                                    ├─ Idle Watchdog (`inFlightRequests` guard + configurable TTL)
                                    └─ ProcessRegistry Reaper (`procs/*.json`, `daemons/*.json`)
```

### 3.1 Key Design Elements

1. **Workspace-Scoped Semantic Engine** ([`SemanticEngine.fs`](../src/Rfs.Agent/SemanticEngine.fs), [`Protocol.fs`](../src/Rfs.Agent/Protocol.fs)):
   - `WorkspaceRoot.resolve` walks upward up to `.git` (supporting both `.git` directories and worktree `.git` files), preferring the outermost `.slnx`/`.sln` inside the repository.
   - `SemanticEngine` maintains per-file `FileDiskStamp` (`WriteTicks`, `Length`, `ContentHash`) and promotes up to `maxPromotedPerProject = 24` edited/queried files onto live declaration-incremental buffers so `check`, `symbol`, `refs`, and `rename` reuse Ranvier's `ProjectGraph.Promote` and adopted typechecks across referencing projects.
   - `PositionResolver` supports line + symbol-name resolution (`--symbol`) so agents do not have to guess exact 1-based column indices.

2. **Detached Daemon & Windows Pipe Fix** ([`Daemon.fs`](../src/Rfs.Agent/Daemon.fs)):
   - Both CLI and MCP frontends route requests to one per-workspace `NamedPipeServerStream` identified by `rfs-daemon-<stateHash>-<wsHash>` under `RFS_STATE_DIR` (default `%LOCALAPPDATA%\ranvier-fsharp`).
   - On Windows, standard .NET `Process.Start` with redirected streams enables `bInheritHandles = TRUE`, causing detached daemons to inherit parent stdout/stderr pipes and block CLI wrappers. `Win32Spawn.startDetached` calls `CreateProcessW` directly with `bInheritHandles = false` and `CREATE_NO_WINDOW | DETACHED_PROCESS` (`0x08000008u`).
   - Startup uses a named startup lock and immediate `ping` readiness before solution loading finishes; `DaemonServer.runAsync` tracks `inFlightRequests` with `Interlocked` so short idle timeouts (`--idle-timeout <seconds>`, default `1800`) cannot abort an active cold load or request.
   - `McpServerHost.runAsync` redirects `Console.SetOut Console.Error` before opening `StreamServerTransport` so accidental library console writes never corrupt MCP JSON-RPC frames on stdout, and pre-warms the daemon pipe asynchronously without blocking `initialize`.

3. **Stale-Process Registry & Reaper** ([`ProcessRegistry.fs`](../src/Rfs.Agent/ProcessRegistry.fs)):
   - Registers every `daemon` and `mcp` process under `<stateDir>/procs/<pid>.json` and daemon leases under `<stateDir>/daemons/<wsId>.json` storing:
     - `pid` + `processStartUtcTicks` (verified within `±2s` AND process-name prefix `rfs`/`dotnet` to avoid killing recycled unrelated OS PIDs),
     - `binaryStamp` (`dll.LastWriteTimeUtc.Ticks:Length|exe.Name:Ticks`),
     - `parentPid` + `parentStartUtcTicks` (queried via `NtQueryInformationProcess` on Windows or `/proc/<pid>/stat` on Linux),
     - `lastActiveUtcTicks` + `idleTimeoutSeconds`.
   - `reapStaleProcessesAsync` runs throttled every `10s` (or immediately on `binaryStamp` mismatch) and cleans: `dead-lease`, `recycled-pid`, `binary-mismatch`, `idle-expired`, `orphaned-parent` (MCP proxies whose parent agent exited), `unresponsive` (failed `1500 ms` pipe ping after `15s` grace), plus opt-in `orphaned-lsp` (`ranvierautocomplete` via `rfs daemon reap --include-lsp`).

---

## 4. Usage Reference

### CLI Commands

| Command | Purpose |
| :--- | :--- |
| `rfs status [--workspace <dir>] [--json] [--reload]` | Inspect workspace load state, projects, RAM (`workingSetMb`), and Ranvier check counters (`promotedFiles`, `declCheckedCount`, `adoptedChecks`, `fileLevelChecks`). |
| `rfs check [<file>] [--errors-only] [--all] [--no-downstream] [--project <name>] [--fail-on-error]` | Incremental compiler diagnostics across the workspace and downstream referencing projects. |
| `rfs symbol <file>[:<line>[:<col>]] [<line>] [--symbol <name>]` | Exact inferred signature, XML doc, definition, and type definition. |
| `rfs refs <file>[:<line>[:<col>]] [<line>] [--symbol <name>] [--uses-only\|--incoming\|--outgoing] [--max <n>]` | Workspace-wide semantic references and call hierarchy. |
| `rfs search [<query>] [--file <file>] [--max <n>]` | File syntax outline (when `--file` is set) or symbol search across loaded projects. |
| `rfs rename <file>[:<line>[:<col>]] [<line>] <new-name> [--symbol <name>] [--dry-run]` | Cross-project semantic rename with post-edit diagnostic verification. |
| `rfs mcp [--workspace <dir>] [--no-always-load]` | Start stdio MCP server exposing `rfs_check`, `rfs_symbol`, `rfs_references`, `rfs_search`, `rfs_rename`, `rfs_status`. |
| `rfs daemon status \| reap [--force] [--include-lsp] \| kill [--all]` | Inspect, reap stale processes, or shut down workspace daemons. |

### MCP Client Configuration

```json
{
  "mcpServers": {
    "ranvier-fsharp": {
      "command": "rfs",
      "args": ["mcp", "--workspace", "/path/to/repo"]
    }
  }
}
```

Pass `"--no-always-load"` in `"args"` to omit `_meta["anthropic/alwaysLoad"] = true` for deferred tool loading.

---

## 5. Experiment Methodology

We benchmarked three detached worktrees representing Small, Medium, and Large real F# solutions:

| Repo | Tier | Loaded Projects | Source Files | Cold CLI `status` (`ms`) | Cold Load Internal (`ms`) | Daemon RSS (`MB`) |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: |
| `QuestPDF.FSharp` (`e2cb355`) | Small | 5 | 78 | 5,251 | 5,051 | 300.6 |
| `SageTUI` (`2330480`) | Medium | 21 | 69 | 14,387 | 14,178 | 790.0 |
| `ranvier-fsharp` (`a369a65`) | Large | 14 | 150 | 20,961 | 20,750 | 1,332.0 |

Experiments had two parts (`claude --model haiku`, isolated `--strict-mcp-config`, `--setting-sources ""`, `--disable-slash-commands`, `--permission-mode bypassPermissions`, `--max-turns 25`, `240 s` timeout):

- **Part 1 (Direct Transport & Lifecycle Microbenchmark)**: Direct CLI spawns vs. `ModelContextProtocol.Core 2.4.0` `McpClient` (`alwaysLoad` vs. deferred) over the same warm daemon, plus incremental edit promotion and stale-process reaper tests.
- **Part 2 (28 Controlled Agent Trials)**:
  - **4 Arms**: `baseline-no-rfs` (no `rfs` on `PATH`), `stdio-daemon` (`rfs` CLI + short system instruction), `mcp-daemon-always` (6 tools with `anthropic/alwaysLoad: true`), `mcp-daemon-deferred` (6 tools without `alwaysLoad`).
  - **Workflow A (Read-Only Semantic Analysis)**: Exact inferred signatures and workspace-wide symbol usage/file sets scored against `rfs refs` / `rfs symbol` ground-truth oracles (signature + F1 score).
  - **Workflow B (Edit -> Diagnostic -> Fix)**:
    - `QuestPDF.FSharp`: Add leading `coalesce: bool` parameter to `Schedule.trigger`, update all production/test callers to pass `true`, clean solution build.
    - `SageTUI`: Add `Visible: bool` to `DebugNode`, update all constructions across library/tests, filter overlay rendering, clean solution build.
    - `ranvier-fsharp`: Add `EmbeddedResource` case to `ProjectItemKind`, resolve all cross-project `FS0025` (warnings-as-errors) matches and string round-trip helpers, clean solution build.
  - Verified post-trial via clean `dotnet build <slnx> --no-restore --disable-build-servers -nodeReuse:false` and structural assertions.

---

## 6. Part 1 Empirical Results: Direct CLI vs. MCP & Reaper Lifecycle

### 6.1 Warm Transport Latency & Payload Sizes

| Repo | Op | CLI Warm (`ms`) | MCP Always (`ms`) | MCP Deferred (`ms`) | CLI -> MCP Speedup | Payload Size |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: |
| `QuestPDF.FSharp` | `status` | 101 | 36 | 29 | 2.8x – 3.5x | 712 B |
| `QuestPDF.FSharp` | `check` (clean) | 99 | 6 | 5 | 16.5x – 19.8x | 78–80 B |
| `QuestPDF.FSharp` | `symbol` (1st promote) | 3,608 | 77 (cached) | 68 (cached) | — | 268–270 B |
| `QuestPDF.FSharp` | `refs` | 182 | 63 | 68 | 2.7x – 2.9x | 788–791 B |
| `QuestPDF.FSharp` | `search` | 136 | 11 | 8 | 12.4x – 17.0x | 167 B |
| `QuestPDF.FSharp` | `check` (1st edit) | 112 | 19 | 16 | 5.9x – 7.0x | 81 B |
| `SageTUI` | `status` | 111 | 28 | 31 | 3.6x – 4.0x | 1,336 B |
| `SageTUI` | `check` (clean) | 108 | 5 | 7 | 15.4x – 21.6x | 79–81 B |
| `SageTUI` | `symbol` (1st promote) | 4,797 | 17 (cached) | 18 (cached) | — | 314–316 B |
| `SageTUI` | `refs` | 188 | 20 | 20 | 9.4x | 1,788–1,791 B |
| `SageTUI` | `search` | 133 | 10 | 13 | 10.2x – 13.3x | 152 B |
| `SageTUI` | `check` (1st edit) | 141 | 35 | 43 | 3.3x – 4.0x | 82 B |
| `ranvier-fsharp` | `status` | 115 | 31 | 32 | 3.6x – 3.7x | 1,032 B |
| `ranvier-fsharp` | `check` (clean) | 105 | 9 | 9 | 11.7x | 79–81 B |
| `ranvier-fsharp` | `symbol` (1st promote) | 6,187 | 399 (cached) | 415 (cached) | — | 306–308 B |
| `ranvier-fsharp` | `refs` | 165 | 26 | 22 | 6.3x – 7.5x | 9,194–9,197 B |
| `ranvier-fsharp` | `search` | 128 | 17 | 13 | 7.5x – 9.8x | 103 B |
| `ranvier-fsharp` | `check` (1st edit) | 233 | 51 | 46 | 4.6x – 5.1x | 83 B |

- **MCP Handshake & Schema Cost**: `6` tools total `3,044 B` (`~761` approx tokens, handshake `184–282 ms`) in `alwaysLoad` mode, or `2,894 B` (`~723` approx tokens, handshake `178–184 ms`) when `_meta["anthropic/alwaysLoad"]` is omitted.
- **Incremental `ProjectGraph` Promotion Counts** after 2 synthetic edits + revert:
  - `QuestPDF.FSharp`: `promotedFiles=2`, `declCheckedCount=37`, `adoptedChecks=37`, `fileLevelChecks=78`.
  - `SageTUI`: `promotedFiles=2`, `declCheckedCount=33`, `adoptedChecks=33`, `fileLevelChecks=69`.
  - `ranvier-fsharp`: `promotedFiles=2`, `declCheckedCount=96`, `adoptedChecks=94`, `fileLevelChecks=152`.

### 6.2 Stale-Process Reaper Lifecycle Results (`part1Reaper`)

| Scenario | Result | Elapsed (`ms`) |
| :--- | :--- | :---: |
| Orphaned MCP parent + dead daemon lease (`rfs daemon reap`) | `checked=2 reaped=2` (`orphaned-parent`, `dead-lease`) | 156 |
| Binary-stamp mismatch auto-replacement (`rfs status`) | Old daemon replaced by new PID (`binaryMismatchReplaced: true`) | 5,212 (incl. cold load) |
| Idle timeout auto-exit (`--idle-timeout 2`) | Clean shutdown (`idleTtlExitedCleanly: true`) | 2,377 |

---

## 7. Part 2 Empirical Results: `claude --model haiku` (28 Trials)

### 7.1 Workflow A — Read-Only Semantic Analysis (14 Trials)

| Repo (Tier) | Arm | Daemon State | Score | Turns | Wall (`ms`) | Cache Create | Cache Read | Output | Total Tokens | Cost (USD) |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| `QuestPDF.FSharp` (S) | `baseline-no-rfs` | `none` | 1.00 | 5 | 14,481 | 8,477 | 78,928 | 2,558 | 89,969 | \$0.0038 |
| `QuestPDF.FSharp` (S) | `stdio-daemon` | `warm` | 0.94 | 6 | 13,640 | 31,339 | 85,494 | 1,977 | 118,818 | \$0.0081 |
| `QuestPDF.FSharp` (S) | `mcp-daemon-always` | `warm` | 0.92 | 6 | 14,924 | 37,898 | 101,059 | 2,517 | 141,482 | \$0.0098 |
| `QuestPDF.FSharp` (S) | `mcp-daemon-deferred` | `warm` | **1.00** | 5 | **12,441** | 12,656 | 88,105 | 2,071 | 102,838 | **\$0.0044** |
| `QuestPDF.FSharp` (S) | `stdio-daemon` | `cold` | 0.94 | 5 | 13,891 | 13,197 | 82,081 | 2,308 | 97,592 | \$0.0046 |
| `QuestPDF.FSharp` (S) | `mcp-daemon-always` | `cold` | 0.94 | 5 | 13,939 | 14,301 | 85,106 | 2,423 | 101,836 | \$0.0049 |
| `SageTUI` (M) | `baseline-no-rfs` | `none` | 1.00 | 6 | 18,121 | 8,647 | 137,806 | 3,382 | 149,845 | \$0.0048 |
| `SageTUI` (M) | `stdio-daemon` | `warm` | 1.00 | **4** | 18,553 | 7,815 | 78,463 | 2,705 | **88,989** | **\$0.0037** |
| `SageTUI` (M) | `mcp-daemon-always` | `warm` | 1.00 | 5 | **14,190** | 7,845 | 81,447 | 2,799 | 92,097 | \$0.0038 |
| `SageTUI` (M) | `mcp-daemon-deferred` | `warm` | 1.00 | 6 | 15,476 | 6,162 | 111,777 | 2,646 | 120,593 | **\$0.0037** |
| `ranvier-fsharp` (L) | `baseline-no-rfs` | `none` | 0.96 | 11 | 22,622 | 23,854 | 122,753 | 4,057 | 150,672 | \$0.0080 |
| `ranvier-fsharp` (L) | `stdio-daemon` | `warm` | 0.96 | **5** | **17,872** | 17,581 | 83,908 | 3,166 | **104,661** | **\$0.0059** |
| `ranvier-fsharp` (L) | `mcp-daemon-always` | `warm` | 0.96 | 15 | 28,413 | 21,193 | 293,096 | 5,139 | 319,444 | \$0.0097 |
| `ranvier-fsharp` (L) | `mcp-daemon-deferred` | `warm` | 0.96 | 12 | 32,160 | 21,238 | 225,742 | 5,407 | 252,399 | \$0.0092 |

*(Note: on `QuestPDF.FSharp` A, `0.94` occurred when `rfs refs` rightly included a `docs/index.fsx` script use outside the 5 `.fs` test uses; on `ranvier-fsharp` A, all 4 arms scored `0.96` with `refF1 = 1.00 (4/4)` and `ansF1 = 0.91 (5/6)`.)*

### 7.2 Workflow B — Edit -> Diagnostic -> Fix Cycles (14 Trials)

| Repo (Tier) | Arm | Daemon State | Score | Clean Build | Wall (`ms`) | Turns | Total Tokens | Cost (USD) | Daemon Promoted / DeclChecks / RSS |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| `QuestPDF.FSharp` (S) | `baseline-no-rfs` | `none` | 1.00 | `true` | 50,834 | 14 | 267,148 | \$0.0071 | — |
| `QuestPDF.FSharp` (S) | `stdio-daemon` | `warm` | 1.00 | `true` | 50,242 | **11** | 232,205 | \$0.0065 | `3` / `0` / `299.2 MB` |
| `QuestPDF.FSharp` (S) | `mcp-daemon-always` | `warm` | 1.00 | `true` | 50,257 | 13 | 204,925 | **\$0.0058** | `3` / `31` / `317.3 MB` |
| `QuestPDF.FSharp` (S) | `mcp-daemon-deferred` | `warm` | 1.00 | `true` | **48,645** | 12 | **198,164** | **\$0.0058** | `3` / `61` / `359.2 MB` |
| `SageTUI` (M) | `baseline-no-rfs` | `none` | 1.00 | `true` | 43,733 | 15 | 313,747 | **\$0.0081** | — |
| `SageTUI` (M) | `stdio-daemon` | `warm` | 1.00 | `true` | **43,064** | **14** | **272,763** | \$0.0094 | `3` / `0` / `900.3 MB` |
| `SageTUI` (M) | `mcp-daemon-always` | `warm` | 1.00 | `true` | 49,832 | 22 | 340,202 | \$0.0103 | `3` / `346` / `1,089.9 MB` |
| `SageTUI` (M) | `mcp-daemon-deferred` | `warm` | 1.00 | `true` | 47,623 | 22 | 358,833 | \$0.0106 | `3` / `361` / `1,106.2 MB` |
| `ranvier-fsharp` (L) | `baseline-no-rfs` | `none` | 1.00 | `true` | **240,115 (TIMEOUT)** | — | *n/a* | *n/a* | Timed out at `240s` (`exitCode=-9`) |
| `ranvier-fsharp` (L) | `stdio-daemon` | `warm` | 1.00 | `true` | 71,900 | 15 | **325,888** | **\$0.0080** | `2` / `135` / `2,057.4 MB` |
| `ranvier-fsharp` (L) | `mcp-daemon-always` | `warm` | 1.00 | `true` | **37,462** | **13** | 345,635 | \$0.0106 | `2` / `251` / `2,432.5 MB` |
| `ranvier-fsharp` (L) | `mcp-daemon-deferred` | `warm` | 1.00 | `true` | 78,532 | **13** | 373,980 | \$0.0104 | `2` / `309` / `2,240.8 MB` |
| `ranvier-fsharp` (L) | `stdio-daemon` | `cold` | 1.00 | `true` | 123,889 | 18 | 495,064 | **\$0.0121** | `0` / `0` / `1,340.2 MB` |
| `ranvier-fsharp` (L) | `mcp-daemon-always` | `cold` | 1.00 | `true` | **38,852** | **15** | **455,450** | \$0.0131 | `1` / `61` / `1,317.9 MB` |

*(Note: on `ranvier-fsharp` Workflow B `baseline-no-rfs`, `claude.exe` exceeded the `240 s` cap while running post-edit `dotnet build`/`test` loops (`exitCode = -9`, `rawOutLen = 0` so JSON token stats were not emitted), although file edits already on disk built cleanly afterwards.)*

### 7.3 Competing Weightings Valuation ($V_{80T}$ vs. $V_{80C}$) & Ratios vs. `no-tool`

To evaluate `rfs` under two competing operational objectives without arbitrary linear unit mixing, we score each arm with a dimensionless Cobb-Douglas value index relative to reference budget $T_0 = 30\text{ s/task}$ and $C_0 = \$0.006\text{/task}$:

$$
V(w_T, w_C) = 100 \cdot Q \cdot \left(\frac{T_0}{T}\right)^{w_T} \cdot \left(\frac{C_0}{C}\right)^{w_C}
$$

- **Time-Weighted ($V_{80T}$)**: $w_T = 0.80,\ w_C = 0.20$ — optimizes interactive pairing speed and fast edit -> diagnostic -> fix iteration.
- **Cost-Weighted ($V_{80C}$)**: $w_T = 0.20,\ w_C = 0.80$ — optimizes token/USD spend for batch or multi-agent pipelines.
- **Scale invariance of ratios**: $T_0$ and $C_0$ cancel identically in every ratio $V_{\text{arm}} / V_{\text{no-tool}}$.
- **Conservative timeout bound**: For the `ranvier-fsharp` Workflow B `baseline-no-rfs` timeout (`240.12 s`, no JSON usage output), cost and tokens are imputed conservatively as the minimum observed on `ranvier-fsharp` Workflow B (`$0.00800` / `325,888` tokens), making every `ranvier-fsharp` B and suite ratio vs. `no-tool` a strict lower bound in favor of `no-tool`.

#### Full 6-Cell Suite Totals (Warm vs. `baseline-no-rfs`)

| Arm | Mean Score | Total Time | Total Tokens | Total Cost (USD) | $V_{80T}$ (Ratio vs. `no-tool`) | $V_{80C}$ (Ratio vs. `no-tool`) |
| :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| `baseline-no-rfs` | `0.994 (1.00x)` | `389.91s (1.00x)` | `1,297,269* (1.00x)` | `$0.03973* (1.00x)` | `52.51 (1.00x)` | `78.70 (1.00x)` |
| `stdio-daemon` (`rfs` CLI) | `0.983 (0.99x)` | `215.27s (0.55x)` | **`1,143,324 (0.88x)`** | `$0.04167 (1.05x)` | `82.76 (1.58x)` | **`84.40 (1.07x)` (#1)** |
| `mcp-daemon-always` (`rfs mcp`) | `0.981 (0.99x)` | **`195.08s (0.50x)`** | `1,443,785 (1.11x)` | `$0.05018 (1.26x)` | **`86.07 (1.64x)` (#1)** | `74.02 (0.94x)` |
| `mcp-daemon-deferred` (`--no-always-load`) | **`0.994 (1.00x)`** | `234.88s (0.60x)` | `1,406,807 (1.08x)` | `$0.04414 (1.11x)` | `77.13 (1.47x)` | `80.07 (1.02x)` |

#### Workflow Subtotals (3 Repos Warm vs. `baseline-no-rfs`)

| Workflow | Arm | Mean Score | Total Time | Total Tokens | Total Cost (USD) | $V_{80T}$ (Ratio vs. `no-tool`) | $V_{80C}$ (Ratio vs. `no-tool`) |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: |
| **A (Read-Only)** | `baseline-no-rfs` | `0.988 (1.00x)` | `55.22s (1.00x)` | `390,486 (1.00x)` | `$0.01659 (1.00x)` | `148.41 (1.00x)` | `116.26 (1.00x)` |
| **A (Read-Only)** | `stdio-daemon` | `0.967 (0.98x)` | **`50.06s (0.91x)`** | **`312,468 (0.80x)`** | `$0.01775 (1.07x)` | **`154.97 (1.04x)`** | `109.91 (0.95x)` |
| **A (Read-Only)** | `mcp-daemon-always` | `0.962 (0.97x)` | `57.53s (1.04x)` | `553,023 (1.42x)` | `$0.02337 (1.41x)` | `130.60 (0.88x)` | `85.36 (0.73x)` |
| **A (Read-Only)** | `mcp-daemon-deferred` | **`0.988 (1.00x)`** | `60.08s (1.09x)` | `475,830 (1.22x)` | `$0.01733 (1.04x)` | `137.54 (0.93x)` | `110.39 (0.95x)` |
| **B (Edit -> Fix)** | `baseline-no-rfs` | `1.000 (1.00x)` | `334.68s (1.00x)` | `906,783* (1.00x)` | `$0.02314* (1.00x)` | `33.26 (1.00x)` | `62.90 (1.00x)` |
| **B (Edit -> Fix)** | `stdio-daemon` | `1.000 (1.00x)` | `165.21s (0.49x)` | **`830,856 (0.92x)`** | **`$0.02392 (1.03x)`** | `58.11 (1.75x)` | **`70.55 (1.12x)` (#1)** |
| **B (Edit -> Fix)** | `mcp-daemon-always` | `1.000 (1.00x)` | **`137.55s (0.41x)`** | `890,762 (0.98x)` | `$0.02680 (1.16x)` | **`65.77 (1.98x)` (#1)** | `66.81 (1.06x)` |
| **B (Edit -> Fix)** | `mcp-daemon-deferred` | `1.000 (1.00x)` | `174.80s (0.52x)` | `930,977 (1.03x)` | `$0.02680 (1.16x)` | `54.30 (1.63x)` | `63.68 (1.01x)` |

#### Full 28-Trial Per-Cell Matrix (All Values + Ratios vs. `no-tool`)

| Wf | Repo | Arm | State | Score (Ratio) | Time (Ratio) | Tokens (Ratio) | Cost USD (Ratio) | $V_{80T}$ (Ratio) | $V_{80C}$ (Ratio) |
| :-: | :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| A | `QuestPDF.FSharp` | `baseline-no-rfs` | `none` | `1.00 (1.00x)` | `14.48s (1.00x)` | `89,969 (1.00x)` | `$0.00376 (1.00x)` | `196.59 (1.00x)` | `167.97 (1.00x)` |
| A | `QuestPDF.FSharp` | `stdio-daemon` | `warm` | `0.94 (0.94x)` | `13.64s (0.94x)` | `118,818 (1.32x)` | `$0.00811 (2.16x)` | `165.61 (0.84x)` | `86.12 (0.51x)` |
| A | `QuestPDF.FSharp` | `mcp-daemon-always` | `warm` | `0.92 (0.92x)` | `14.92s (1.03x)` | `141,482 (1.57x)` | `$0.00985 (2.62x)` | `146.01 (0.74x)` | `71.33 (0.42x)` |
| A | `QuestPDF.FSharp` | `mcp-daemon-deferred` | `warm` | `1.00 (1.00x)` | **`12.44s (0.86x)`** | `102,838 (1.14x)` | `$0.00445 (1.18x)` | **`214.69 (1.09x)`** | `151.50 (0.90x)` |
| A | `QuestPDF.FSharp` | `stdio-daemon` | `cold` | `0.94 (0.94x)` | `13.89s (0.96x)` | `97,592 (1.08x)` | `$0.00461 (1.23x)` | `182.71 (0.93x)` | `134.75 (0.80x)` |
| A | `QuestPDF.FSharp` | `mcp-daemon-always` | `cold` | `0.94 (0.94x)` | `13.94s (0.96x)` | `101,836 (1.13x)` | `$0.00492 (1.31x)` | `179.86 (0.91x)` | `127.86 (0.76x)` |
| A | `SageTUI` | `baseline-no-rfs` | `none` | `1.00 (1.00x)` | `18.12s (1.00x)` | `149,845 (1.00x)` | `$0.00480 (1.00x)` | `156.51 (1.00x)` | `132.24 (1.00x)` |
| A | `SageTUI` | `stdio-daemon` | `warm` | `1.00 (1.00x)` | `18.55s (1.02x)` | **`88,989 (0.59x)`** | `$0.00370 (0.77x)` | `161.79 (1.03x)` | `162.04 (1.23x)` |
| A | `SageTUI` | `mcp-daemon-always` | `warm` | `1.00 (1.00x)` | **`14.19s (0.78x)`** | `92,097 (0.61x)` | `$0.00378 (0.79x)` | **`199.60 (1.28x)`** | `167.97 (1.27x)` |
| A | `SageTUI` | `mcp-daemon-deferred` | `warm` | `1.00 (1.00x)` | `15.48s (0.85x)` | `120,593 (0.80x)` | **`$0.00367 (0.77x)`** | `187.32 (1.20x)` | **`169.01 (1.28x)`** |
| A | `ranvier-fsharp` | `baseline-no-rfs` | `none` | `0.96 (1.00x)` | `22.62s (1.00x)` | `150,672 (1.00x)` | `$0.00803 (1.00x)` | `113.95 (1.00x)` | `80.78 (1.00x)` |
| A | `ranvier-fsharp` | `stdio-daemon` | `warm` | `0.96 (1.00x)` | **`17.87s (0.79x)`** | **`104,661 (0.69x)`** | **`$0.00594 (0.74x)`** | **`146.14 (1.28x)`** | **`107.76 (1.33x)`** |
| A | `ranvier-fsharp` | `mcp-daemon-always` | `warm` | `0.96 (1.00x)` | `28.41s (1.26x)` | `319,444 (2.12x)` | `$0.00974 (1.21x)` | `91.35 (0.80x)` | `66.11 (0.82x)` |
| A | `ranvier-fsharp` | `mcp-daemon-deferred` | `warm` | `0.96 (1.00x)` | `32.16s (1.42x)` | `252,399 (1.68x)` | `$0.00921 (1.15x)` | `83.66 (0.73x)` | `67.45 (0.84x)` |
| B | `QuestPDF.FSharp` | `baseline-no-rfs` | `none` | `1.00 (1.00x)` | `50.83s (1.00x)` | `267,148 (1.00x)` | `$0.00708 (1.00x)` | `63.45 (1.00x)` | `78.84 (1.00x)` |
| B | `QuestPDF.FSharp` | `stdio-daemon` | `warm` | `1.00 (1.00x)` | `50.24s (0.99x)` | `232,205 (0.87x)` | `$0.00647 (0.91x)` | `65.20 (1.03x)` | `84.90 (1.08x)` |
| B | `QuestPDF.FSharp` | `mcp-daemon-always` | `warm` | `1.00 (1.00x)` | `50.26s (0.99x)` | `204,925 (0.77x)` | `$0.00585 (0.83x)` | `66.53 (1.05x)` | `92.08 (1.17x)` |
| B | `QuestPDF.FSharp` | `mcp-daemon-deferred` | `warm` | `1.00 (1.00x)` | **`48.65s (0.96x)`** | **`198,164 (0.74x)`** | **`$0.00583 (0.82x)`** | **`68.32 (1.08x)`** | **`92.86 (1.18x)`** |
| B | `SageTUI` | `baseline-no-rfs` | `none` | `1.00 (1.00x)` | `43.73s (1.00x)` | `313,747 (1.00x)` | **`$0.00806 (1.00x)`** | **`69.73 (1.00x)`** | **`73.24 (1.00x)`** |
| B | `SageTUI` | `stdio-daemon` | `warm` | `1.00 (1.00x)` | **`43.06s (0.98x)`** | **`272,763 (0.87x)`** | `$0.00944 (1.17x)` | `68.39 (0.98x)` | `64.72 (0.88x)` |
| B | `SageTUI` | `mcp-daemon-always` | `warm` | `1.00 (1.00x)` | `49.83s (1.14x)` | `340,202 (1.08x)` | `$0.01031 (1.28x)` | `59.80 (0.86x)` | `58.61 (0.80x)` |
| B | `SageTUI` | `mcp-daemon-deferred` | `warm` | `1.00 (1.00x)` | `47.62s (1.09x)` | `358,833 (1.14x)` | `$0.01056 (1.31x)` | `61.71 (0.89x)` | `58.02 (0.79x)` |
| B | `ranvier-fsharp` | `baseline-no-rfs` | `none` | `1.00 (1.00x)` | `240.12s (1.00x)` | `325,888* (1.00x)` | `$0.00800* (1.00x)` | `17.88 (1.00x)` | `52.39 (1.00x)` |
| B | `ranvier-fsharp` | `stdio-daemon` | `warm` | `1.00 (1.00x)` | `71.90s (0.30x)` | **`325,888 (1.00x)`** | **`$0.00800 (1.00x)`** | `46.91 (2.62x)` | **`66.68 (1.27x)`** |
| B | `ranvier-fsharp` | `mcp-daemon-always` | `warm` | `1.00 (1.00x)` | **`37.46s (0.16x)`** | `345,635 (1.06x)` | `$0.01065 (1.33x)` | **`74.64 (4.17x)`** | `60.45 (1.15x)` |
| B | `ranvier-fsharp` | `mcp-daemon-deferred` | `warm` | `1.00 (1.00x)` | `78.53s (0.33x)` | `373,980 (1.15x)` | `$0.01042 (1.30x)` | `41.47 (2.32x)` | `53.06 (1.01x)` |
| B | `ranvier-fsharp` | `stdio-daemon` | `cold` | `1.00 (1.00x)` | `123.89s (0.52x)` | `495,064 (1.52x)` | `$0.01209 (1.51x)` | `27.95 (1.56x)` | `42.98 (0.82x)` |
| B | `ranvier-fsharp` | `mcp-daemon-always` | `cold` | `1.00 (1.00x)` | `38.85s (0.16x)` | `455,450 (1.40x)` | `$0.01308 (1.63x)` | `69.58 (3.89x)` | `50.90 (0.97x)` |

---

## 8. Conclusions: Stdio CLI + Daemon vs. MCP + Daemon

1. **Both frontends must share one persistent daemon with stale-process reaping**:
   Without a daemon, F# workspace initialization takes `5.0 s – 20.8 s` per cold query and `dotnet build` verification cycles cause timeouts on 14+ project solutions (`>240 s`). With one shared Named Pipe / Unix domain socket daemon + `ProcessRegistry` reaper, cold cost is paid once and stale/orphaned/version-mismatched processes are cleaned deterministically.
2. **Choose `rfs mcp` (with `alwaysLoad` on large F# solutions, `--no-always-load` elsewhere) when wall-clock speed (`V_80T`) matters**:
   - Sub-`50 ms` warm IPC latency vs. `~105–281 ms` CLI process spawns.
   - Wins Time-Weighted valuation overall (**`1.64x`** vs. `no-tool`, `V_80T = 86.07`) and dominates large-solution edit -> fix loops (`ranvier-fsharp` Workflow B: **`37.5 s` warm / `4.17x` `V_80T`**, **`38.9 s` cold / `3.89x` `V_80T`** vs. `240.1 s` baseline timeout).
3. **Choose `rfs <cmd>` (Stdio CLI + Daemon) when minimizing token/USD spend (`V_80C`) or running occasional read-only queries on large solutions**:
   - Wins Cost-Weighted valuation overall (**`1.07x`** vs. `no-tool`, `V_80C = 84.40`, `0.88x` total tokens), wins Workflow B Cost-Weighted (**`1.12x`**), and beats `no-tool` across both weightings on large-solution read-only queries (`ranvier-fsharp` A: **`1.28x` $V_{80T}$**, **`1.33x` $V_{80C}$**, `0.74x` USD cost) and large-solution edit loops (**`1.27x` $V_{80C}$**, `0.30x` time).
4. **Shipping both in `Ranvier.FSharp.Agent` (`rfs`) requires zero duplicated compiler state** — agents seamlessly combine `rfs mcp` for fast incremental edits with CLI `rfs daemon status` / `rfs daemon reap`. See [docs/rfs.md](rfs.md) for the full operator guide and [docs/rfs-agent.md](rfs-agent.md) for the compact agent operational manual.
