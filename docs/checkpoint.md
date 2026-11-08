# Checkpoint: declaration graph vs file-level cutoff

## Decision: GO

The decision rule (plan, Task 17): **GO** when `decl` has a lower p50 edit latency than `file-slim` on the body
scenarios of both `mid` and `large`, with zero oracle diffs. Signature scenarios are reported but do not gate.

| scenario | engine | edit p50 ms | edit p95 ms | oracle diffs | failed steps |
|---|---|---|---|---|---|
| mid-body | **decl** | **28.0** | **61.7** | 0 | 0 |
| mid-body | file-slim | 81.9 | 308.6 | 0 | 0 |
| large-body | **decl** | **30.2** | **47.7** | 0 | 0 |
| large-body | file-slim | 35.2 | 77.1 | 0 | 0 |

- decl is faster on both gating scenarios: 2.9x on mid-body (p50), but only 1.17x on large-body (5.0 ms).
- I repeated the three body scenarios once, to check that the large-body margin is not noise
  (`results/checkpoint/logs/body-repeat.log`, not the committed result files). The repeat gave:
  - large-body: decl 31.3 / 39.7 ms (p50 / p95) against file-slim 36.0 / 75.9 ms;
  - mid-body: decl 27.2 / 57.7 ms against file-slim 83.3 / 265.1 ms;
  - small-body: decl 31.5 / 35.0 ms against file-slim 61.1 / 95.0 ms.
- **Zero oracle diffs.** Every run, all four scenario globs, recorded 0 oracle diffs and 0 failed steps.
  - That is 150 edit steps per engine, 300 in all. Each was compared in Strict mode with a sequential
    from-scratch check of the whole project (R17).
  - All five checkpoint invocations exited 0.
  - The oracle compares diagnostics only. It does not check the fingerprints or typed trees that decl caches.

GO holds on the gating rule, but the numbers outside it are mixed. decl is slower than file-slim on:
- every signature edit, by 2.7x to 5.8x;
- the big `module rec` file, by about 1.5x;
- cold start, by 2.2x to 3.0x.

It also uses more memory, and on real code it forms more segments than the fixtures predicted (see "Segment rows").
These are the server plan's risks, listed at the end.

## Setup

| | |
|---|---|
| ranvier-fsharp (R) | `8f97578` (main) plus the two line-insert scenarios added here |
| FCS fork | `fickle-fsharp` `9ed3e709b` (`fickle_decl`), Release |
| Ranvier | worktree `ranvier-fsharp-ranvier`, branch `fix/late-link-dirty` at `b0a70d3f` (not merged) |
| build | `dotnet build RanvierFSharp.slnx -c Release` |
| command | `dotnet run --project src/Rfs.Checkpoint -c Release -- --scenario <glob> --corpus ../ranvier-fsharp-corpus --out results/checkpoint`, cwd = R (R9), one glob per invocation |
| preflight | `rfs-harness verify-corpus`: small, mid and large each 0 errors (6, 3 and 16 other diagnostics) |
| engines | one child process per engine and scenario, file-slim first; `--log-cutoff` off |

Machine, as in the FSAC baseline README:

| | |
|---|---|
| CPU | AMD Ryzen 9 9900X, 12 cores / 24 threads |
| RAM | 61.6 GB |
| OS | Windows 11 Pro 10.0.26200 |
| .NET SDK | 10.0.403 (R's `global.json`) |

**The machine was not idle.** About 13 `fsautocomplete` processes (editor sessions, up to ~700 MB each) were running,
along with several Claude, Codex and ChatGPT desktop sessions, Spotify, and dotnet build servers. CPU load was 9% at
preflight. Nothing was closed.

Each scenario was run once for the committed files; the body scenarios were run a second time (above). Percentiles
are nearest-rank: with 20 edits p95 is the 19th value, and with 10 edits (signature, line insert) p95 is the maximum.

## Full table per scenario

decl vs file-slim, from `results/checkpoint/*.json` and the side files `results/checkpoint/extra/*.json`.
- Cold MB: allocations during create plus the first check.
- Edit MB: allocations per edit, as a mean.
- Decls/edit: adopted checks.
- Runs/edit: `CheckDeclaration` runs, superseded flights included, so runs/edit minus decls/edit is the wasted work.
- Finish share: the p50 over edits of FinishFile time divided by edit latency.

| scenario | engine | p50 ms | p95 ms | cold ms | peak MB | cold MB | edit MB | decls/edit | runs/edit | finish share p50 | oracle diffs |
|---|---|---|---|---|---|---|---|---|---|---|---|
| small-body | decl | 29.1 | 35.2 | 16253.6 | 415.7 | 8021 | 16.6 | 1.0 | 1.0 | 72.3% | 0 |
| small-body | file-slim | 68.1 | 162.5 | 6325.2 | 285.8 | 1831 | 71.9 | - | - | - | 0 |
| mid-body | decl | 28.0 | 61.7 | 11094.2 | 453.9 | 3694 | 14.5 | 1.0 | 1.0 | 64.4% | 0 |
| mid-body | file-slim | 81.9 | 308.6 | 3751.4 | 415.8 | 1701 | 76.1 | - | - | - | 0 |
| large-body | decl | 30.2 | 47.7 | 7433.4 | 364.3 | 2059 | 18.0 | 1.0 | 1.0 | 49.0% | 0 |
| large-body | file-slim | 35.2 | 77.1 | 3340.5 | 306.5 | 955 | 35.4 | - | - | - | 0 |
| small-signature | decl | 3127.2 | 4009.6 | 16578.4 | 1009.9 | 8025 | 3121.0 | 590.0 | 590.0 | 29.6% | 0 |
| small-signature | file-slim | 540.6 | 1500.1 | 6342.7 | 357.4 | 1831 | 753.0 | - | - | - | 0 |
| mid-signature | decl | 1410.1 | 3600.9 | 10137.0 | 911.1 | 3690 | 1180.7 | 467.0 | 467.0 | 23.2% | 0 |
| mid-signature | file-slim | 393.1 | 981.0 | 3877.8 | 490.6 | 1692 | 471.7 | - | - | - | 0 |
| large-signature | decl | 863.6 | 2285.9 | 7032.1 | 670.0 | 2058 | 651.8 | 223.0 | 223.0 | 24.5% | 0 |
| large-signature | file-slim | 319.9 | 765.9 | 3304.7 | 384.7 | 956 | 325.5 | - | - | - | 0 |
| large-bigfile-narrow | decl | 312.5 | 648.4 | 6788.0 | 742.4 | 2058 | 220.4 | 1.0 | 1.0 | 19.7% | 0 |
| large-bigfile-narrow | file-slim | 212.3 | 570.1 | 3191.7 | 420.6 | 950 | 168.4 | - | - | - | 0 |
| large-bigfile-wide | decl | 322.9 | 470.0 | 7258.0 | 742.2 | 2058 | 219.7 | 1.0 | 1.0 | 20.3% | 0 |
| large-bigfile-wide | file-slim | 216.2 | 626.8 | 3205.4 | 403.1 | 953 | 168.4 | - | - | - | 0 |
| mid-lineinsert | decl | 73.8 | 186.4 | 10182.2 | 481.0 | 3698 | 68.9 | 27.0 | 27.0 | 25.1% | 0 |
| mid-lineinsert | file-slim | 83.7 | 300.1 | 3980.8 | 376.1 | 1697 | 76.9 | - | - | - | 0 |
| large-lineinsert | decl | 39.2 | 63.4 | 6938.8 | 361.7 | 2058 | 30.4 | 13.0 | 13.0 | 39.7% | 0 |
| large-lineinsert | file-slim | 37.6 | 137.1 | 3412.8 | 303.5 | 955 | 35.8 | - | - | - | 0 |

- Runs/edit equals decls/edit everywhere: no superseded flights, so decl wasted no work. The checkpoint waits for
  each edit to settle before it sends the next, so this says nothing about typing bursts.
- Signature edits: decl re-checks every declaration of every later file (223, 467 and 590 per edit). R16 makes
  cross-file granularity per file: a changed signature fingerprint restarts each later file and re-checks it one
  declaration at a time. file-slim's graph check re-checks the same dependents as whole files, in parallel. decl is
  2.7x (large), 3.6x (mid) and 5.8x (small) slower at p50, and on small it allocates 3.1 GB per edit against 0.75 GB.

### Against the FSAC core-loop baseline

**Different measurement layer.** FSAC's numbers include LSP framing and the `publishDiagnostics` round trip, and FSAC
re-checks only the open file. decl and file-slim run in-process and re-check the whole project. FSAC's cold time
includes `fsharp/workspaceLoad` but checks only the opened file. FSAC uses FCS 43.12.402, not the fork. Treat the
FSAC columns as context, not as a like-for-like comparison.

| scenario | decl p50 / p95 | file-slim p50 / p95 | fsac-core-bg p50 / p95 | fsac-core-tc p50 / p95 |
|---|---|---|---|---|
| small-body | 29.1 / 35.2 | 68.1 / 162.5 | 28.0 / 53.2 | 43.7 / 119.3 |
| mid-body | 28.0 / 61.7 | 81.9 / 308.6 | 87.5 / 291.1 | 74.9 / 261.0 |
| large-body | 30.2 / 47.7 | 35.2 / 77.1 | 22.5 / 31.2 | 42.4 / 84.9 |
| small-signature | 3127.2 / 4009.6 | 540.6 / 1500.1 | 29.3 / 92.7 | 47.6 / 137.8 |
| mid-signature | 1410.1 / 3600.9 | 393.1 / 981.0 | 85.5 / 251.5 | 67.1 / 317.5 |
| large-signature | 863.6 / 2285.9 | 319.9 / 765.9 | 21.2 / 65.8 | 36.7 / 109.6 |
| large-bigfile-narrow | 312.5 / 648.4 | 212.3 / 570.1 | 225.4 / 292.7 | 84.5 / 129.8 |
| large-bigfile-wide | 322.9 / 470.0 | 216.2 / 626.8 | 241.7 / 515.8 | 79.5 / 142.9 |

FSAC signature numbers are low because FSAC does not re-check the later files that depend on the changed signature
until they are opened. The FSAC baseline has no line-insert scenarios. The full `rfs-harness compare` output is
`results/checkpoint/compare-fsac-core.md`.

## Segment rows

Rows checked inside segments always re-check together, in order. That is the cost these rows carry: an edit to any
row of a segment re-checks the whole segment.
- **Numerator:** `ProjectGraph.SegmentRows` after the cold check.
- **Denominator:** the fork's `DeclarationChecker.Split` over every implementation file of the project.
- **Leaders and reasons:** a throwaway program (not committed). It reads `Split`, `DeclarationChecker.Content`,
  `ProjectGraph.Results` (for `DependsOnLaterDeclarations` and `LeavesFreeTypars`) and the `PremiseBroken`
  observable, then rebuilds the roles the way `NameIndex.roles` does. Its per-file totals match `SegmentRows` exactly
  for every file of all three projects.

| corpus | project | rows | segment rows | fraction | D1 premise breaks at cold |
|---|---|---|---|---|---|
| small | FSharp.Data.Adaptive | 1412 | 444 | 31.4% | 5 |
| mid | FsAutoComplete.Core | 1110 | 323 | 29.1% | 32 |
| large | Fantomas.Core | 301 | 16 | 5.3% | 1 |

Segment rows were the same after the last edit as after the cold check in every run.

**Finding: the D1 premise breaks on real code.** The fuzz tests and the ProjectGraph tests assert 0 premise breaks on
their fixtures. On the corpora, the cold check counts 5, 32 and 1.
- What a break is: a declaration whose final fingerprint after FinishFile differs from the one its own check left.
- What the graph does: it treats each such declaration as a leader to the end of its file. That keeps the checks
  sound, which the zero oracle diffs back up, but every row after the leader then re-checks in order.
- Where they come from:
  - In mid, 26 of the 32 are active patterns (`(|Val|_|)`, `(|Constructor|_|)` and others): 22 in
    `TypedAstPatterns.fs`, and 4 in `CodeGeneration.fs`, two of them rec groups led by an active pattern. The other 6:
    - a value and a type in `Utils.fs`;
    - two module headers in `UntypedAstUtils.fs`;
    - a type and a rec group in `AdaptiveExtensions.fs`.
  - In small, 4 of the 5 are `RecGroup`s of interfaces and types (`IAdaptiveValue`, `IAdaptiveHashSet`,
    `IAdaptiveHashMap`, `HashSet`).
- Premise breaks are where most segment rows come from: 309 of small's 444 and 227 of mid's 323. That is about 21%
  of each project's rows.
- The gating edits are not affected: the three edited files have no segment rows.
- Why the fingerprints change is not investigated here. The candidates are the finish's defaults, slot completion and
  active-pattern result types. It is the first thing to look at if segments matter.

Five largest segments per corpus. "Row" is the declaration's index in its file.

| corpus | rows | leader (file:line) | why |
|---|---|---|---|
| small | 120 | `MapExtImplementation.ofTwoOption` (Value), MapExt.fs:1805 | R12: an unsolved inference variable in what it publishes or in an inline body (not free typars in the environment) |
| small | 100 | `HashSet` (RecGroup), HashCollections.fs:3254 | D1 premise break |
| small | 92 | `IAdaptiveHashMap` (RecGroup), AdaptiveHashMap.fs:10 | D1 premise break |
| small | 89 | `IAdaptiveHashSet` (RecGroup), AdaptiveHashSet.fs:9 | D1 premise break |
| small | 28 | `IAdaptiveValue` (RecGroup), AdaptiveValue.fs:6 | D1 premise break |
| mid | 87 | `Utils.normalizePathI` (Value), Utils.fs:149 | D1 premise break |
| mid | 76 | `SymbolUse.(\|ActivePattern\|_\|)` (Value), TypedAstPatterns.fs:66 | D1 premise break |
| mid | 74 | `TipFormatter.Section.addSection` (Value), TipFormatter.fs:260 | R12: an unsolved inference variable in what it publishes or in an inline body |
| mid | 51 | `AMap.BatchRecalculateDirty` (Type), AdaptiveExtensions.fs:208 | D1 premise break; absorbs the R11 span of `type asyncaval with` (row 85) |
| mid | 18 | `Decompiler.TextWriterWithLocationFinder` (Type), Decompiler.fs:20 | R12: leaves free typars |
| large | 9 | `List.moreThanOne` (Value), Utils.fs:62 | D1 premise break |
| large | 7 | `Context.Context` (Type), Context.fs:208 | R11: `type Context with` augmentation at row 23 |

The remaining segments:
- small: R11 spans for `SetOperation` (11 rows, Operations.fs) and `AdaptiveObject` (4 rows);
- mid: a premise break in CodeGeneration.fs (11 rows) and in UntypedAstUtils.fs (2 rows);
- mid: R11 spans for `MissingExplicitType` and `ExplicitType` (2 rows each, InlayHints.fs).

R11 matches a name, not a type. A span runs from the first earlier row whose `Defines` holds the augmented name.

## Big file: CodePrinter.fs

`Fantomas.Core/CodePrinter.fs` has 5,324 lines and is `module internal rec`. `Split` gives it **one row**, a single
`RecGroup`. So decl has no declaration granularity in this file. Every edit re-checks the whole file as one
declaration, then runs its FinishFile. The file has a `.fsi`, so a body edit leaves the signature unchanged and no
later file is re-checked by either engine.

| scenario | decl p50 / p95 | file-slim p50 / p95 | fsac-core-bg p50 / p95 | fsac-core-tc p50 / p95 |
|---|---|---|---|---|
| large-bigfile-narrow (line 41, a string) | 312.5 / 648.4 | 212.3 / 570.1 | 225.4 / 292.7 | 84.5 / 129.8 |
| large-bigfile-wide (line 2078, inside genExpr) | 322.9 / 470.0 | 216.2 / 626.8 | 241.7 / 515.8 | 79.5 / 142.9 |

(FSAC columns: different layer; see above.)

- decl is about 1.5x slower than file-slim at p50 here.
- FinishFile is 66 ms p50 (about 20%) of decl's ~313 ms. The other ~247 ms is checking the one 5,324-line row,
  which is still slower than file-slim's whole-file check.
- decl's peak is 742 MB against 421 MB.
- fsac-core-tc is about 2.5x faster than file-slim on the same one-file re-check. That is a different FCS version,
  and probably caching in the transparent compiler; not investigated.
- D2's goal (files of tens of thousands of lines with heavy inference) is not served by decl when the file is
  `module rec`. The rec fallback is pure overhead today.

## W1 cost: line insert

Both scenarios insert a blank line near the top of the file, above most declarations, then remove it, 10 edits
each. decl's checks are keyed by range, so every declaration below the insert re-checks (W1): 27 of the 36 rows of
DocumentationFormatter.fs, and 13 rows of Queue.fs.

| scenario | decl p50 / p95 | decl decls/edit | file-slim p50 / p95 | decl body edit in the same file (p50) |
|---|---|---|---|---|
| mid-lineinsert | 73.8 / 186.4 | 27.0 | 83.7 / 300.1 | 28.0 |
| large-lineinsert | 39.2 / 63.4 | 13.0 | 37.6 / 137.1 | 30.2 |

- A line insert costs decl 2.6x (mid) and 1.3x (large) its body-edit latency.
- On mid it is still slightly faster than file-slim at p50 and much better at p95. On large it is 1.6 ms slower at
  p50 and better at p95.
- In a file with hundreds of rows below the cursor, W1 would cost more. Range shifting is server-phase work.

## Cold start and memory

| corpus | decl cold ms | file-slim cold ms | FSAC core cold ms (bg / tc, cold scenario) | decl cold alloc MB | file-slim cold alloc MB | decl peak MB (body) | file-slim peak MB (body) | FSAC core peak MB (bg / tc, body) |
|---|---|---|---|---|---|---|---|---|
| small | 16254 | 6325 | 7516 / 7223 | 8021 | 1831 | 416 | 286 | 453 / 463 |
| mid | 11094 | 3751 | 8040 / 7231 | 3694 | 1701 | 454 | 416 | 763 / 812 |
| large | 7433 | 3341 | 11750 / 11591 | 2059 | 955 | 364 | 307 | 1083 / 492 |

(FSAC: different layer. Its cold time is project load plus checking one opened file, not the whole project.)

- decl's cold start is 2.2x to 3.0x file-slim's, and it allocates 2.2x to 4.4x as much while doing it.
- During the cold check, FinishFile alone takes 1.6 s (large), 2.7 s (mid) and 5.4 s (small).
- Peak working set on body edits is 1.1x to 1.5x file-slim's. On signature edits decl peaks at 670 to 1010 MB,
  against 357 to 491 MB for file-slim.
- decl's body-edit peaks stay under FSAC's background-compiler peaks on all three corpora.

## Finish share

Finish share is the part of an edit's latency spent in FinishFile, the whole-file pass after the declaration checks.
FinishFile does the post-inference checks, applies defaults, replays FS1182 references, builds the file signature
and fingerprints, and renders diagnostics. It runs over the entire edited file on every edit, however small the
change.

| scenario | finish share p50 | finish ms p50 | edit p50 ms |
|---|---|---|---|
| small-body | 72.3% | 19.7 | 29.1 |
| mid-body | 64.4% | 18.2 | 28.0 |
| large-body | 49.0% | 15.8 | 30.2 |
| large-lineinsert | 39.7% | 18.4 | 39.2 |
| mid-lineinsert | 25.1% | 20.4 | 73.8 |
| signatures | 23-30% | 230-956 | 864-3127 |
| bigfile | 20% | 66 | 313-323 |

On body edits, one declaration's check is the small part. About half to three quarters of the latency is the finish.
The targets this implies, largest first:
1. **Make FinishFile incremental.** Run post-inference checks and diagnostics rendering per changed declaration, and
   reuse the file signature when fingerprints are unchanged. This is the largest single cost of a body edit.
2. **Signature edits.** Restarting later files whole and re-checking them one declaration at a time (R16) costs 223
   to 590 declaration checks plus a finish per file. Options:
   - per-declaration cross-file dependencies, so only declarations that use the changed name re-check;
   - or fall back to a file-level check for files that restart, which file-slim shows is 2.7x to 5.8x cheaper.
3. **Cold start.** Per-declaration checks plus a finish per file cost about twice file-slim. Options:
   - check the cold project file-level, then switch to declarations on first edit;
   - or persist results.

## Caveats

- **Measurement layer.** decl and file-slim are in-process and re-check the whole project. FSAC adds LSP and
  publishing and re-checks only open files. The tables that mix them say so.
- **Ranvier fix is unmerged.** decl runs on Ranvier's `fix/late-link-dirty` branch (`aa59513a` + `b0a70d3f`) in a
  separate worktree. The graph needs that fix: the late-link lost-wakeup bug.
- **Different FCS.** decl, file-slim and the oracle use the fork (`9ed3e709b`). FSAC uses FCS 43.12.402.
- **FSAC's oracle diffs were informational.** The FSAC baseline recorded diffs but did not enforce them; it had 0.
  Here they are enforced (Strict).
- **Cross-file granularity is per file (R16).** That is why the signature edits re-check whole later files.
- **The oracle is sequential (R17); file-slim runs in graph mode.** Per the Task 16 ruling, graph mode is the fork's
  best file-level configuration: its signature cutoff exists only there. No graph-vs-sequential mismatch appeared.
- **One sample per scenario, on a busy machine**, apart from the body repeat. The large-body margin (5 ms, 14%) held
  in the repeat (4.7 ms), but it is small.
- **The oracle checks diagnostics only.** A wrong cached typed tree that does not change diagnostics would not show.
- **Premise breaks are not counted by the checkpoint CLI.** They were found with the segment analysis above.

## What it means for the server plan

GO: the server uses the declaration graph in the same Ranvier architecture. Top risks, roughly in order of how much
they could hurt users:

1. **Signature edits.** decl is 2.7x to 5.8x slower than file-slim (0.9 s to 3.1 s p50) and allocates up to
   3.1 GB per edit. Adding a parameter or changing a return type is common.
   - The server must not ship R16 as is. It needs per-declaration cross-file dependencies, or a file-level fallback
     for restarted files.
2. **FinishFile share.** 49% to 72% of a body edit. Making the finish incremental is the main lever for latency.
3. **Cold start and memory.**
   - Cold start: 7 s to 16 s, against 3 s to 6 s for file-slim.
   - Cold allocations: up to 8 GB.
   - Peak memory on signature edits: up to 1 GB.
4. **Rec-module fallback.** `module rec` / `namespace rec` files are one row. CodePrinter.fs is 1.5x slower than
   file-slim, and D2's big-file goal needs either a finer split of rec groups or the file-level path for them.
5. **D1 premise breaks on real code** (5, 32 and 1). They put about 21% of small's and mid's rows into
   leader-to-end-of-file segments. That is sound, but edits below them lose declaration granularity. Resolved in
   Task 6: see "Premise breaks after Task 6" below for the causes (signature conformance, a default, unit-of-measure
   normalization), the fix and the counts after it.
6. **W1.** A line insert re-checks every declaration below it: 1.3x to 2.6x a body edit here, more in long files.
   Range shifting is needed.

## Premise breaks after Task 6

`rfs-checkpoint --premise-report --scenario <glob> --corpus ../ranvier-fsharp-corpus --out <dir>` checks a scenario's
project once with every file checked by declaration. It writes `premise-<corpus>.md`, with one line per break from
`ProjectGraph.PremiseBroken`: file:line, key, kind, and the first differing part of the provisional and final
fingerprints. The reports from before the fix (R `92a3bc8`, fork `06fc666bf`) are in `results/premise/base/`, and
those from after it in `results/premise/`.

| corpus | breaks before | breaks after | segment rows before | segment rows after | segment rows at the checkpoint |
|---|---|---|---|---|---|
| small | 5 | 0 | 534 | 225 to 293 | 444 |
| mid | 32 | 0 | 323 | 139 | 323 |
| large | 1 | 0 | 16 | 7 | 16 |

- Small's segment rows grew from 444 to 534 between the checkpoint and Task 6's base.
- After the fix they vary between runs. Four runs gave 293, 272, 246 and 225 (the last on the final build), all
  with 0 breaks. Why the count varies was not investigated.
- The segment rows that remain are R11 spans and R12 segments, and none of them is a premise break.

The checkpoint suspected active patterns and interface rec groups. In fact every break had one of three causes. Each
is something FinishFile does to a declaration that no other declaration takes part in:

1. **Signature conformance** (36 of 38). FinishFile checks a file that has a signature file against it. That check
   writes into the implementation's items, and the fingerprint describes everything it writes:
   - a value gets the signature's arity, argument names and argument attributes;
   - a type, union case, field or module gets the attributes the signature adds.

   The breaks it caused:
   - **Arity: 29.** These are values bound to a lambda whose signature declares them without arguments:
     - `val (|ActivePattern|_|): (FSharpSymbolUse -> ... option)` and the like: 26 in mid's `TypedAstPatterns.fs`
       and `CodeGeneration.fs`, and 2 in the namespace fragments of `UntypedAstUtils.fs`;
     - `val moreThanOne: ('a list -> bool)`: 1 in large.
   - **Attributes: 6.** `[<Interface>]` on small's three interface rec groups. `[<Class>]` on `ChangeableValue`,
     `ReadOnlySpanExtensions`, and `AdaptiveCancellableTask` in `RefCountingTaskCreator`'s rec group.
   - **Argument name: 1.** `t` in `BatchRecalculateDirty.Compute`.
2. **A default** (1, small's `HashSet`). `typedefof<HashSetProxy<_>>` in an attribute leaves an inference variable
   that only the file-end defaults solve. The fingerprint describes the attribute, but the free-variable checks do not
   look at attributes, so the declaration was not flagged.
3. **Unit-of-measure normalization** (1, mid's `normalizePathI`). Collecting a declaration's unsolved variables
   (FindUnsolved) normalizes the solutions of unit-of-measure variables in place: `LocalPath` becomes
   `LocalPath^1 * 1`. The check took the fingerprint before it collected them (for `LeavesUnsolved`), and the
   finish's defaults collect them again.

The rule the fix follows: every value used as a cutoff is computed from exactly the state its readers read.
- Within a file, the declarations after a declaration are checked against its items as the check left them, before
  conformance and the defaults. So the in-file cutoff must describe those items.
- Conformance does not only add to the items, it overwrites: an argument the implementation leaves unnamed (`_`)
  takes the signature's name, an arity is cut to the signature's, attribute lists are merged. A fingerprint of the
  conformed items alone therefore cannot tell `member _.M(_: int)` from `member _.M(t: int)`, while a later
  `T().M(t = 1)` checks differently against them (FS0495 or not). Task 6's first version used that fingerprint as
  the cutoff and missed such edits (review C1).

Task 6 (after review round 1) follows that rule:
- **Conformance is never in a declaration's fingerprint.** No declaration of the file is checked against conformed
  items, in a full check or here. Across files, dependents read the signature file, whose fingerprint is unchanged.
- **FinishFile undoes its own conformance.** The fork's conformance check records, in a log of its own, how to undo
  what it writes into the implementation's items (`UndoableConformance.Log` and `Isolate`). The log covers argument
  names and attributes, arity, attributes, xml docs, other ranges, and type parameters.
  - FinishFile renders the diagnostics while the conformance is in effect, because a full check renders them so.
  - It then undoes the conformance, and only after that takes the final fingerprints and the signature.
  - So the final fingerprint describes what any later check of the file sees, and a declaration re-checked after a
    finish sees the earlier items unconformed, as a full check does (review C2).
  - Before review round 1 the conformance persisted. Re-checking `let use1 = T().M(t = 1)` alone then hid the FS0495
    that a full check reports.
- **The lazy check results apply it again.** Tooltips, go-to-declaration and symbol uses need the conformed items,
  so the lazy results apply the whole file's conformance again under the FileStart's and the file's locks when they
  are built. The next check of the file undoes it with the rest of the latest finish.
  - The order is not left to timing. CheckDeclaration and ExtendEnv undo the finish and check while they hold the
    FileStart's lock, so a build of the results cannot land between the undo and the check (re-review I1).
  - Accepted (re-review M1): after that next check, a superseded FileResult's queries show the implementation
    unconformed until the next finish's results are built. The server queries only the latest FileResult.
- **Defaults.** The default captured at check time is still in the fingerprint: small's `HashSet` attribute, for a
  declaration that leaves variables unsolved but depends on no later declaration.
  - Later declarations see the variable unsolved, so the check fingerprints the declaration both before and after
    applying the default, and joins the two (`DeclarationResult.AtCheck` keeps the first).
  - FinishFile builds the final fingerprint the same way, from that first fingerprint and its own.
  - Covering both states can only over-approximate.
- **Measure normalization.** The check collects the unsolved variables before it takes the fingerprint. The
  normalization this does is permanent, so later declarations see it too.
- **No check-time conformance step, so no extra cost.** The first version conformed each declaration at its check,
  quadratically in a flat module (review I1). That step is gone.
  - `D1.checking a flat module with a signature file costs about what it does without one` pins it on 2000 values. A
    cold check by declaration must stay within 1.5x of the same module without a signature, and a re-check within 2x.
  - Measured: 424 ms against 357 ms cold, and 0.124 ms against 0.123 ms for a re-check.

The finish can still change less than the check captured, when an earlier default of the file fails. The premise
then breaks, and the graph treats the declaration as a leader, as before. That happens only in a file with errors.

Tests:
- D1 tests pin each shape (`D1.signature conformance`, `D1.whole-file passes`). Each shape with a signature has a
  partner that edits what conformance overwrites, checks the edited declaration again after the finish, and expects
  the cutoff to change and the re-check to equal a fresh check.
- `D1.signature conformance` also checks that a finish's check results, read after a later check, give the
  conformed tooltip and declaration location that InteractiveChecker gives.
- `SignatureConformance` in Rfs.Checker.Tests drives the graph through such edits in both modes.
- OracleFuzz's `conformedShape` edit toggles them on the SigConform fixture, for every seed.

No break remains, so there is no inherent break to pin. Type parameter names are not part of the fingerprint at
all. What later declarations report shows type parameters by position (`'a`), so a rename the signature imposes
changes nothing they see.

## Traced Ranvier

Ranvier's Debug configuration compiles in a per-graph event log (`RANVIER_TRACE`). During Task 7 we found that a
solution build of R gives the out-of-solution Ranvier project its default configuration, Debug, even with `-c Release`.
Some earlier results therefore measured a traced Ranvier. `Directory.Build.props` now builds Ranvier in the build's own
configuration (`371e711`), and `rfs-checkpoint` prints `ranvier: untraced` in its header.

**GO still holds as the rule is written, but only mid-body shows a real win. On large-body, decl is not shown to be
faster than file-slim.**

### Numbers

Untraced runs are in `results/server-a/baseline-untraced`, on the final Task 7 tree (R `4fb29d0`, fork `06d54b778`).
The header of every run says `ranvier: untraced`. Other dotnet work used about 1 CPU second per scenario, so nothing
overlapped. That check covers dotnet-family processes, not the whole machine. All 340 edit steps, on both engines
and including the four large-body repeats, had no oracle diff and no failed step.

Each cell is checkpoint → untraced final tree. Allocations are MB per edit, over 20 edits for body and 10 for signature.

| scenario | engine | edit p50 ms | edit p95 ms | edit alloc MB/edit | peak MB | cold ms |
|---|---|---|---|---|---|---|
| small-body | decl | 29.1 → 34.9 | 35.2 → 54.0 | 16.6 → 41.3 | 415.7 → 259.6 | 16253.6 → 7604.3 |
| small-body | file-slim | 68.1 → 56.3 | 162.5 → 74.9 | 71.9 → 63.4 | 285.8 → 282.6 | 6325.2 → 7786.2 |
| mid-body | decl | 28.0 → 35.1 | 61.7 → 99.2 | 14.5 → 34.6 | 453.9 → 380.0 | 11094.2 → 4469.3 |
| mid-body | file-slim | 81.9 → 85.0 | 308.6 → 262.3 | 76.1 → 71.9 | 415.8 → 418.5 | 3751.4 → 4503.4 |
| large-body | decl | 30.2 → 30.3 | 47.7 → 72.0 | 18.0 → 28.1 | 364.3 → 317.7 | 7433.4 → 3749.3 |
| large-body | file-slim | 35.2 → 31.9 | 77.1 → 76.5 | 35.4 → 31.2 | 306.5 → 310.6 | 3340.5 → 3453.5 |
| small-signature | decl | 3127.2 → 525.3 | 4009.6 → 1373.8 | 3121.0 → 747.5 | 1009.9 → 307.4 | 16578.4 → 5980.0 |
| small-signature | file-slim | 540.6 → 522.9 | 1500.1 → 1403.4 | 753.0 → 735.8 | 357.4 → 357.1 | 6342.7 → 6147.9 |
| mid-signature | decl | 1410.1 → 325.9 | 3600.9 → 911.8 | 1180.7 → 443.3 | 911.1 → 496.8 | 10137.0 → 4293.6 |
| mid-signature | file-slim | 393.1 → 357.8 | 981.0 → 951.5 | 471.7 → 463.8 | 490.6 → 485.1 | 3877.8 → 4151.1 |
| large-signature | decl | 863.6 → 249.0 | 2285.9 → 927.7 | 651.8 → 311.4 | 670.0 → 356.2 | 7032.1 → 3063.9 |
| large-signature | file-slim | 319.9 → 265.3 | 765.9 → 756.8 | 325.5 → 316.1 | 384.7 → 387.5 | 3304.7 → 3378.3 |

This is a re-baseline, not a re-run of the checkpoint's code, which has had Tasks 3 to 7 built on top of it since. The
arrows therefore mix tracing, design changes and machine state, and do not measure tracing alone.

**Worse for decl than at the checkpoint:**
- Body p50: small 29.1 → 34.9, mid 28.0 → 35.1. Large is unchanged.
- Body p95: small 35.2 → 54.0, mid 61.7 → 99.2, large 47.7 → 72.0.
- Body allocations per edit: small 16.6 → 41.3, mid 14.5 → 34.6, large 18.0 → 28.1.

Tracing cannot explain these. It adds cost, so a traced checkpoint would have hidden an even larger increase. I did not
attribute the increases to a particular task.

**Better for decl than at the checkpoint:** every signature number, cold start and peak memory.
- The checkpoint's signature risk is gone. decl was 2.7x to 5.8x slower than file-slim at p50 and is now 0.9x to 1.0x.
- Cold start is now 0.9x to 1.1x of file-slim, where it was 2.2x to 3.0x.
- Peak is now 0.86x to 1.02x of file-slim.
- large-signature is the one signature scenario where decl's p95 trails file-slim's: 927.7 against 756.8 ms, the
  maximum of 10 edits.

### Run-to-run spread

large-body, five runs of the final tree, one after another (`baseline-untraced`, then `repeat` to `repeat4`):

| run | file-slim p50 | decl p50 | decl lead ms | file-slim p95 | decl p95 | decl edit MB |
|---|---|---|---|---|---|---|
| 1 | 31.9 | 30.3 | 1.6 | 76.5 | 72.0 | 561.5 |
| 2 | 29.7 | 28.7 | 1.0 | 68.8 | 69.0 | 561.3 |
| 3 | 30.7 | 27.3 | 3.4 | 91.5 | 62.9 | 561.3 |
| 4 | 27.7 | 27.8 | -0.1 | 83.7 | 63.1 | 561.4 |
| 5 | 28.6 | 27.7 | 0.9 | 73.6 | 67.1 | 561.4 |
| median | 29.7 | 27.8 | 1.0 | 76.5 | 67.1 | 561.4 |

- p50 ranges over 4.2 ms for file-slim and 3.0 ms for decl. The medians differ by 1.9 ms, less than either range.
- Allocation is stable to 0.04%, so allocation differences of a few MB are real where timing differences of a few ms
  are not.
- decl's p95 was lower in four runs of five and 0.2 ms higher in the other.
- The t7-fix1 tree, one round earlier and with the same allocations, once measured decl *slower*: 31.7 against 26.9 ms
  (`results/server-a/t7-fix1/variance/1`).

### Does GO hold?

The rule: decl has a lower p50 than file-slim on the body scenarios of mid and large, with zero oracle diffs.

- **mid-body: yes, clearly.** 35.1 against 85.0 ms, 2.4x. There is one final-tree run, but the t7-fix1 run agrees (36.8
  against 88.5) and the gap is far outside the spread above.
- **large-body: yes on the median, not in every run.** 27.8 against 29.7 ms. decl was lower in four runs of five and
  0.1 ms higher in the other. The checkpoint's 1.17x margin is now about 1.07x, inside the noise.
- **Oracle diffs: zero.**

So a single run would usually pass the rule and sometimes fail it on large-body. What large-body supports is "no slower
than file-slim", at slightly lower allocation and p95. The case for the declaration graph now rests on mid-body, small-body
and the line-insert scenarios (mid 39.0 against 71.8 ms, in `results/server-a/t7-fix2`), and on signature edits and cold
start no longer costing more.

### Which results were traced

No result folder stores the `Ranvier.dll` it ran on or a build record, and the header line is new. So apart from `t7`,
this is inferred from what the runs allocated. For the same code, a traced run shows a distinct signature:

| | traced | untraced |
|---|---|---|
| large-body decl edit MB | about 631 | about 561 |
| mid-signature decl edit MB | about 4475 | about 4410 |
| mid-signature decl peak MB | about 565 | about 490 |

| verdict | results | basis |
|---|---|---|
| traced, confirmed | `server-a/t7`, `t7/variance` | heap dump held trace records; R's Debug `Ranvier.dll` was 975,360 bytes against 537,088 |
| traced, inferred | `server-a/t5-fix4`, `t6-fix1`, `t6-fix2` | signature above |
| untraced, inferred | `server-a/t4`, `t4-fix1`, `t5`, `t5-fix1`, `t5-fix2`, `t5-fix3`, `t5-fix5` | signature above |
| untraced, confirmed | `server-a/t7-fix1` | dll size. Its run-logs' `TRACED BUILD` line is false (see the folder's README) |
| untraced, header | `server-a/t7-fix2`, `baseline-untraced` | `ranvier: untraced` |
| cannot tell | **`checkpoint`**, `server-a/before`, `t2`, `t3`, `t3-fix1`, `t3b`, `t3b-fix*` | no untraced run of the same code to compare with |

**This section's "checkpoint" column cannot be called traced or untraced.** The evidence points both ways:
- For traced: the Setup table's build is `dotnet build RanvierFSharp.slnx -c Release`. At that commit Ranvier was a plain
  `ProjectReference` outside the solution, and Ranvier's commit `b0a70d3f` already defaulted Debug to traced.
- For untraced: the Setup table's command is `dotnet run --project src/Rfs.Checkpoint -c Release` without `--no-build`.
  That builds the project itself, and a project build passes Release on to Ranvier, which should replace the traced dll
  in the output. I did not reproduce this.

The likely explanation for the variance Task 5 recorded on large-body (631.8 against 561.3 MB) is that those runs were
traced. It fits the table above but was not tested directly.
