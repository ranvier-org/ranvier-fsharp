# `rfs` — Agent Operational Reference

`rfs` is a persistent declaration-incremental F# compiler daemon exposed as MCP tools (`rfs_*`) and CLI commands (`rfs <cmd>`).

## Loop Discipline

1. Use `rfs` as the inner loop for reading types, finding references, renaming symbols, and checking compiler errors after edits.
2. Save edited files to disk before calling `rfs` (the daemon reads disk files by stamp/hash, not unsaved editor buffers).
3. Reserve `grep`/text search for non-F# files, comments, and string literals.
4. Run `dotnet build` and unfiltered `dotnet test` once as the final acceptance gate (and whenever running compiled tests/binaries is required).

## Addressing Symbols

- All `line` and `column` values are **1-based**.
- Pass `line` + `symbol` (`--symbol <name>`) instead of counting character columns; the resolver searches ±5 lines for the identifier or operator.
- If `symbol` appears multiple times on the same line, add `column` (`--col <n>`) to pick the nearest match.

## Operations

| Task | MCP Tool & Args | CLI Equivalent | Notes / Defaults |
| :--- | :--- | :--- | :--- |
| Type-check after edits | `rfs_check(file?, project?, severity?, downstream?)` | `rfs check [<file\|proj>] [--errors-only\|--all] [--no-downstream] [--fail-on-error]` | Default `severity="warning"`, `downstream=true`. CLI exits `1` on errors only with `--fail-on-error`. |
| Type signature & definition | `rfs_symbol(file, line, symbol?, column?)` | `rfs symbol <file> <line> [--symbol <name>] [--col <n>]` | Returns inferred type, XML doc, definition, type definition, implementations. |
| References & callers | `rfs_references(file, line, symbol?, column?, mode?, maxResults?)` | `rfs refs <file> <line> [--symbol <name>] [--uses-only\|--incoming\|--outgoing] [--max <n>]` | `mode` ∈ `all` (default), `uses`, `incoming`, `outgoing`. Run after `check` has 0 upstream errors. |
| Outline or workspace search | `rfs_search(query?, file?, maxResults?)` | `rfs search [<query>] [--file <f>] [--max <n>]` | Set `file` for syntax outline; omit `file` to search all loaded projects. |
| Semantic rename | `rfs_rename(file, line, newName, symbol?, column?, apply?)` | `rfs rename <file> <line> [<old>] <newName> [--symbol <name>] [--dry-run]` | Default `apply=true` (CLI) / MCP. Preview first with `apply=false` / `--dry-run`. |
| Daemon / workspace health | `rfs_status(reload?)` | `rfs status [--reload] [--json]` | Reload after `.fsproj`, `.slnx`, `.sln`, or `Directory.*.props` edits. |
| Reap stale processes | — | `rfs daemon reap` | Safely cleans dead leases and orphaned MCP processes; ask user before `rfs daemon kill`. |

## Mandatory Guardrails

- **Check before `references`/`rename`**: Upstream parse or type errors can suppress symbol resolution and yield incomplete reference sets. Fix errors shown by `check` first.
- **Fresh checkout with `failed > 0`**: If `status` reports failed project loads (missing `project.assets.json`), run `dotnet restore` (or one `dotnet build`) and then `rfs_status(reload=true)`.
- **Git worktrees**: Each git worktree is an isolated daemon boundary. Pass file paths inside the active worktree (or `--workspace <worktree-root>`).
- **Mismatched daemon binaries**: Binary stamp mismatches replace old daemons automatically; never `kill` another session's daemon unless instructed by the user.
