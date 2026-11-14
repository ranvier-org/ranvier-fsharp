---
name: rfs
description: Use when reading, navigating, editing, refactoring, or type-checking F# (.fs, .fsi, .fsproj, .slnx) code, or before running dotnet build, dotnet test, or grep to check F# types, callers, or compiler diagnostics.
license: MIT
---

Use `rfs` as the F# semantic inner loop. Run `dotnet build` and unfiltered `dotnet test` once as the final gate.

## Interfaces
- **MCP (preferred for edits):** `rfs_check`, `rfs_symbol`, `rfs_references`, `rfs_search`, `rfs_rename`, `rfs_status`.
- **CLI (lowest token cost):** `rfs check`, `rfs symbol`, `rfs refs`, `rfs search`, `rfs rename`, `rfs status`.

## Addressing
Lines and columns are 1-based. Pass `line` + `symbol` (`--symbol <name>`) instead of counting columns; pass `column` (`--col <n>`) to disambiguate ties on a line.

## Loop
1. Read types (`rfs_symbol`), uses (`rfs_references`), and declarations (`rfs_search`); reserve `grep` for prose, config, and literals.
2. Save `.fs`/`.fsi` edits to disk, then run `rfs_check` — referencing projects re-check in memory.
3. Run `rfs_check` clean before `rfs_references` or `rfs_rename`; preview renames with `apply=false` (`--dry-run`) before `apply=true`.
4. Call `rfs_status(reload=true)` after editing `.fsproj`, `.slnx`, `.sln`, or `.props`.
5. Pass worktree-local paths (or `--workspace <dir>`) in git worktrees; ask before `rfs daemon kill`.
6. Finish with one clean `dotnet build` and unfiltered `dotnet test`.
