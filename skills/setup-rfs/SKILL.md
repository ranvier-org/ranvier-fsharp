---
name: setup-rfs
description: Use when installing, configuring, updating, or troubleshooting rfs (Ranvier.FSharp.Agent) CLI or MCP server in an F# repository or developer environment.
license: MIT
---

Set up `rfs` (`Ranvier.FSharp.Agent`) in order. Verify each `Done when:` check before moving on.

## 1. Check `.NET 10` SDK
Run `dotnet --list-sdks`. `rfs` loads MSBuild in-process and requires a `10.0.*` SDK.
- **Done when:** at least one `10.0.x` SDK is listed.

## 2. Install `Ranvier.FSharp.Agent`
- Global (recommended): `dotnet tool install -g Ranvier.FSharp.Agent` (or `dotnet tool update -g Ranvier.FSharp.Agent`).
- Repo-local: `dotnet new tool-manifest` (if absent), then `dotnet tool install Ranvier.FSharp.Agent`.
- **Done when:** `rfs --version` (or `dotnet rfs --version`) outputs `rfs <version>`.

## 3. Configure Agent Integration Mode
Choose by priority and add `.mcp.json` at the repo root if using MCP:
- **Wall-clock speed (`80% Time / 20% Cost`, default):** `{ "mcpServers": { "rfs": { "command": "rfs", "args": ["mcp"] } } }`
- **Small/mixed repo with deferred tool search:** add `"--no-always-load"` to `"args"`.
- **Minimum token cost (`20% Time / 80% Cost`):** omit `.mcp.json`; instruct agents to call the `rfs` CLI directly (`0` schema tokens).
- For repo-local installs, set `"command": "dotnet"` and `"args": ["tool", "run", "rfs", "--", "mcp"]`.

Wire the workspace `AGENTS.md` / `CLAUDE.md` to reference `docs/rfs-agent.md` (or install the `rfs` skill).
- **Done when:** `.mcp.json` (if MCP) and the `AGENTS.md` pointer exist.

## 4. Warm and Verify
Run `dotnet restore` on fresh checkouts, then `rfs status` and `rfs check --fail-on-error`.
- **Done when:** `rfs status` reports `projects: >= 1 loaded, 0 failed` and `rfs check` succeeds or returns structured compiler diagnostics.

## 5. Recovery
If `failed > 0`, restore/build once and run `rfs status --reload`. Run `rfs daemon reap` to clean stale records; ask the user before `rfs daemon kill --all`.
