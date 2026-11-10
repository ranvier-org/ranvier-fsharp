# Ranvier F# Language Server Results (`small`, `mid`, and core `large`)

This document records the end-to-end LSP benchmark comparison of `ranvierautocomplete` (`src/Rfs.Server`, Release) against FsAutoComplete (`fsac-core-bg` and `fsac-core-tc` at upstream `4c33a2a`), the feature-layer responsiveness measurements under active edits, known parallel-load test flakes, and the open items carried from Milestones A–C and the reactive feature-layer slices.

## Scope and Environment

Per project decision, this run executes the `-Small` pass (`pwsh -NoProfile -File build/final-run.ps1 -Small -Rebuild`: all 17 `small` and `mid` scenarios, the 5 core `large` scenarios, body repeats, missing `small`/`mid` FSAC baselines, and `rfs-harness compare` — `0.39 h` wall clock) in place of the ~10-hour `huge` + `large-edit` FSAC pass. `huge` and edit-during-check behaviour are covered by Milestone A (`docs/server-a.md`), Slice M, and Slice 8h/8r below.

| Component | Version / Configuration |
|---|---|
| `ranvier-fsharp` (`R`) | `0837cf3` (`main`, clean; product code at `856ef93`) |
| `fickle-fsharp` (`F`, FCS fork) | `c39df403f3` (`fickle_decl`, clean, Release `net10.0`) |
| `Ranvier` | `d701e6c5` (`master`, Release, untraced — zero `Trace` calls in IL) |
| FSAC baseline | `4c33a2a7` (`fsac-core-bg`: BackgroundCompiler; `fsac-core-tc`: `--use-fcs-transparent-compiler`; core-loop config) |
| .NET SDK / Runtime | `10.0.403` (`net10.0`; `ranvierautocomplete.exe` and `rfs-harness.exe` run from apphosts) |
| Machine | AMD Ryzen 9 9900X (12 cores / 24 threads), 62 GB RAM, Windows 11 Pro `10.0.26200` |
| Ambient processes | User IDE / SageFs / FSAC instances remained open (`results/server-final/environment.txt`); no builds or test suites ran alongside |

### Suite Gate Status at `856ef93`

- `Rfs.Decl.Tests`: **241 passed, 0 ignored, 0 failed, 0 errored** (`1m39s`)
- `Rfs.Checker.Tests` (`RFS_M6_TESTS=1`): **440 passed, 0 ignored, 0 failed, 0 errored** (`52m00s`)
- `Rfs.Server.Tests`: **636+ passed** (including `480`-case hover safety grid, `874`-case uses safety grid, `96`-case completion grid, and `136/136` whole-branch fix suite)
- `Rfs.Harness.Tests` / `Rfs.Checkpoint.Tests`: **76/76** and **12/12** passed

---

## 1. End-to-End LSP Comparison (`ranvier` vs `fsac-core-bg` and `fsac-core-tc`)

Raw outputs: `results/server-final/compare-fsac-core-bg.md`, `results/server-final/compare-fsac-core-tc.md`, and `results/server-final/compare-repeat.md`.

- **Every run exited 0 with 0 failed steps.**
- **Oracle diffs on `ranvier` (`oracle: strict`, checked after every edit): `0` across every scenario and repeat** (25 runs total, including `mid-crossproject` and 150-open-file `mid-openmany-edits`).
- **Publishes per edit:** `1.00` in every edit scenario.

### Edits, Cold Start, Memory, and Oracle Parity

| Scenario | Server | Edit p50 (ms) | Edit p95 (ms) | Cold (ms) | Decls/edit | Peak WS (MB) | Alloc (MB) | Oracle diffs |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| `small-cold` | `fsac-core-bg` | — | — | 7516.2 | — | 432.7 | 1585.2 | 0 |
| `small-cold` | `fsac-core-tc` | — | — | 7222.9 | — | 424.1 | 1442.6 | 0 |
| `small-cold` | **`ranvier`** | — | — | **6091.2** | — | **240.4** | 1994.0 | **0** |
| `small-body` | `fsac-core-bg` | 28.0 | 53.2 | 7610.5 | — | 453.1 | 1936.3 | 0 |
| `small-body` | `fsac-core-tc` | 43.7 | 119.3 | 7073.6 | — | 462.9 | 2051.9 | 0 |
| `small-body` | **`ranvier`** *(repeat)* | **38.6** *(35.2)* | **63.9** *(63.0)* | **6032.0** | 1.75 *(1 after #1)* | **274.6** | 2800.5 | **0** |
| `small-signature` | `fsac-core-bg` | 29.3 | 92.7 | 7479.8 | — | 453.9 | 1759.4 | 0 |
| `small-signature` | `fsac-core-tc` | 47.6 | 137.8 | 7117.3 | — | 463.3 | 1781.1 | 0 |
| `small-signature` | **`ranvier`** | **42.4** | **62.6** | **5983.9** | 2.50 | **384.6** | 9474.9 | **0** |
| `small-openmany` (20 files) | `fsac-core-bg` | — | — | 12650.8 | — | 1666.3 | 15574.6 | 0 |
| `small-openmany` (20 files) | `fsac-core-tc` | — | — | 8415.9 | — | 469.1 | 1787.4 | 0 |
| `small-openmany` (20 files) | **`ranvier`** | — | — | 9432.4 | — | **325.2** | 3781.4 | **0** |
| `mid-cold` | `fsac-core-bg` | — | — | 8039.7 | — | 654.7 | 1578.0 | 0 |
| `mid-cold` | `fsac-core-tc` | — | — | 7230.5 | — | 707.9 | 1558.4 | 0 |
| `mid-cold` | **`ranvier`** | — | — | **5494.6** | — | **402.6** | 2061.2 | **0** |
| `mid-body` | `fsac-core-bg` | 87.5 | 291.1 | 9015.9 | — | 762.7 | 2656.1 | 0 |
| `mid-body` | `fsac-core-tc` | 74.9 | 261.0 | 6205.2 | — | 811.8 | 2266.0 | 0 |
| `mid-body` | **`ranvier`** *(repeat)* | **41.3** *(38.6)* | **104.5** *(96.5)* | **4917.4** | 2.80 *(1 after #1)* | **422.8** | 2781.8 | **0** |
| `mid-signed-body` | `fsac-core-bg` | 38.3 | 127.7 | 6671.7 | — | 719.9 | 2018.3 | 0 |
| `mid-signed-body` | `fsac-core-tc` | 45.2 | 79.3 | 5024.9 | — | 750.5 | 2073.7 | 0 |
| `mid-signed-body` | **`ranvier`** | **36.1** | **85.6** | **4972.4** | 5.30 *(1 after #1)* | **424.7** | 2770.0 | **0** |
| `mid-lineinsert` | `fsac-core-bg` | 76.6 | 258.3 | 6100.3 | — | 756.8 | 2117.4 | 0 |
| `mid-lineinsert` | `fsac-core-tc` | 51.8 | 355.1 | 4759.1 | — | 775.5 | 1887.8 | 0 |
| `mid-lineinsert` | **`ranvier`** | **47.6** | **148.8** | 5043.7 | 3.60 *(0 after #1)* | **420.3** | 2452.1 | **0** |
| `mid-eof-then-insert` | **`ranvier`** | **45.1** | **107.9** | **4970.0** | 2.30 | **465.2** | 2849.6 | **0** |
| `mid-signature` | `fsac-core-bg` | 85.5 | 251.5 | 6166.0 | — | 759.8 | 2114.2 | 0 |
| `mid-signature` | `fsac-core-tc` | 67.1 | 317.5 | 5041.1 | — | 747.2 | 1872.3 | 0 |
| `mid-signature` | **`ranvier`** | **51.7** | **163.5** | **5019.7** | 6.60 | **541.7** | 6514.0 | **0** |
| `mid-crossproject` | `fsac-core-bg` | 74.4 | 312.8 | 6667.7 | — | 688.2 | 2118.3 | **10** |
| `mid-crossproject` | `fsac-core-tc` | 41.6 | 308.2 | 6197.7 | — | 713.0 | 1880.5 | **10** |
| `mid-crossproject` | **`ranvier`** | **32.2** | **74.4** | **5433.0** | 87.00 | 698.9 | 17988.0 | **0** |
| `mid-openmany` (20 files) | `fsac-core-bg` | — | — | 13395.5 | — | 3120.4 | 7499.7 | 0 |
| `mid-openmany` (20 files) | `fsac-core-tc` | — | — | 6616.0 | — | 706.2 | 2017.9 | 0 |
| `mid-openmany` (20 files) | **`ranvier`** | — | — | **5826.9** | — | **441.4** | 2478.2 | **0** |
| `mid-openmany-edits` (150 files) | **`ranvier`** | **123.0** | **494.9** | **14119.5** | 67.24 | **898.4** | 13742.6 | **0** |
| `large-cold` | `fsac-core-bg` | — | — | 11750.3 | — | 1036.2 | 5128.8 | 0 |
| `large-cold` | `fsac-core-tc` | — | — | 11590.6 | — | 1035.3 | 5136.0 | 0 |
| `large-cold` | **`ranvier`** | — | — | **11144.3** | — | **636.9** | **4589.2** | **0** |
| `large-body` | `fsac-core-bg` | 22.5 | 31.2 | 10834.6 | — | 1083.3 | 5291.3 | 0 |
| `large-body` | `fsac-core-tc` | 42.4 | 84.9 | 6518.6 | — | 492.4 | 1720.8 | 0 |
| `large-body` | **`ranvier`** *(repeat)* | **43.2** *(34.4)* | **55.7** *(48.3)* | 8227.7 | 12.65 *(12 after #1)* | **657.4** | 5213.4 | **0** |
| `large-signature` | `fsac-core-bg` | 21.2 | 65.8 | 9739.9 | — | 1019.8 | 5206.5 | 0 |
| `large-signature` | `fsac-core-tc` | 36.7 | 109.6 | 5979.3 | — | 465.9 | 1526.7 | 0 |
| `large-signature` | **`ranvier`** | **28.5** | **63.8** | 9164.3 | 2.30 | **661.0** | 7684.9 | **0** |

#### Key Observations on Edits and Cross-Project Semantics

1. **Cross-project live diagnostics (`mid-crossproject`):** Both `fsac-core-bg` and `fsac-core-tc` record **10 oracle diffs** — when `FsLibLog.fs` (`FsAutoComplete.Logging`) changes return type, neither FSAC mode updates open `AdaptiveExtensions.fs` (`FsAutoComplete.Core`) with the resulting `FS0001` error because FSAC relies on the on-disk referenced DLL. `ranvier` rebuilds the in-memory `ProjectAssembly` and pushes the downstream diagnostic in **`32.2 ms` p50 (`74.4 ms` p95)** with **`0` oracle diffs** (which accounts for its higher allocation on `mid-crossproject` and `*-signature`, where `ranvier` keeps the downstream graph settled rather than checking only the edited open file).
2. **LSP layer overhead (`<= 10 ms` gate from Milestone B):** Against `docs/server-a.md`'s clean in-process `decl` body p50 (`31.8 ms` on `small`, `31.4 ms` on `mid`), `ranvier`'s end-to-end LSP body p50 is `38.6 ms` / `35.2 ms` on `small` (**`+6.8` / `+3.4 ms`**) and `41.3 ms` / `38.6 ms` on `mid` (**`+9.9` / `+7.2 ms`**) — within the `10 ms` allowance on both corpora.
3. **`Decls/edit` attribution on step 1:** `rfs/declarationsChecked` reports checks adopted since the previous notification; the initial `open` step's promotion checks are carried by edit `#1` (`16` on `small-body`, `37` on `mid-body`, `25` on `large-body`). Every subsequent edit adopts `1` declaration (`small-body`, `mid-body`, `mid-signed-body`), `0` declarations (`mid-lineinsert`, via range shifting), or `12` declarations (`large-body`, from P0 entity-identity propagation across `Queue.fs` users).
4. **Memory scaling:** Across 20-file workspaces (`small-openmany`, `mid-openmany`), `ranvier` uses **`325–441 MB`** peak working set vs **`1,666–3,120 MB`** on `fsac-core-bg` (`5.1–7.1×` lower) and **`469–706 MB`** on `fsac-core-tc` (`1.4–1.6×` lower). Even with **150 open files** edited continuously (`mid-openmany-edits`), `ranvier` peaks at **`898.4 MB`** with `0` oracle diffs.

---

## 2. LSP Request Latencies (`hover`, `completion`, `definition`, `references`)

### Standard Request Scenarios (p50 ms)

| Scenario | Server | `completion` p50 (ms) | `definition` p50 (ms) | `hover` p50 (ms) | Peak WS (MB) |
|---|---|---:|---:|---:|---:|
| `small-requests` | `fsac-core-bg` | 5.9 | 0.7 | 14.9 | 812.3 |
| `small-requests` | `fsac-core-tc` | 5.6 | 0.6 | 13.0 | 812.4 |
| `small-requests` | **`ranvier`** | **7.0** | **6.0** | **15.4** *(14.1 in 3× A/B)* | **255.1** |
| `mid-requests` | `fsac-core-bg` | 10.9 | 1.3 | 17.2 | 635.9 |
| `mid-requests` | `fsac-core-tc` | 9.8 | 0.8 | 13.4 | 701.3 |
| `mid-requests` | **`ranvier`** | **10.6** | **5.3** | **6.5** | **456.3** |
| `large-requests` | `fsac-core-bg` | 6.9 | 0.5 | 4.1 | 1036.0 |
| `large-requests` | `fsac-core-tc` | 8.5 | 0.6 | 3.7 | 1128.8 |
| `large-requests` | **`ranvier`** | **8.4** | **2.4** | **3.4** | **655.6** |

### Cross-Project / Project-Wide `textDocument/references` (`I5` Cold vs Warm Index)

Source: `results/server-final/references-first.md`. The first `references` request touching un-opened files builds their plain-data `FileUses` memo (`CheckResultsNotKept`, dropping compiler AST/TC state immediately) filtered by identifier text; subsequent queries read the kept plain-data index in a single graph turn (`ReadInOneTurn`).

| Corpus / Run | Server | Overall p50 (ms) | 1st Query (ms) | p50 of Queries 2–10 (ms) | Worst of Queries 2–10 (ms) | Peak WS (MB) |
|---|---|---:|---:|---:|---:|---:|
| `small-references` | `fsac-core-bg` | 64.6 | 1,231 | 65 | 189 | 584.6 |
| `small-references` | `fsac-core-tc` | 28.2 | 908 | 28 | 218 | 638.6 |
| `small-references` | **`ranvier`** | **5.5** | 1,309 | **6** | 2,548 *(step 5 cold files)* | **337.2** |
| `mid-references` | `fsac-core-bg` | 65.3 | 1,296 | 65 | 325 | 918.1 |
| `mid-references` | `fsac-core-tc` | 103.5 | 920 | 103 | 338 | 873.2 |
| `mid-references` | **`ranvier`** | **7.6** | 4,429 | **8** | **104** | **485.6** |
| `large-references` | **`ranvier`** | **4.2** | 7,832 | **4** | **17** | **733.4** |

*(In `small-references`, query 1 only indexes files mentioning `Transaction.fs` `310:8`; step 5 queries `AdaptiveValue.fs` `9:29`, indexing the remaining un-opened files once in `2,548 ms`, after which steps 6–11 run in `1.7–12.1 ms`.)*

### Doc-Only Edits and Active-Edit Responsiveness (Task 8b, Slice M, Slice 8h/8r)

- **Zero-recheck doc-only edits (Task 8b, `F c39df403f3` + `R bec7e86`):** Declaration syntax hashes strip `///` XML doc lines (including nested member docs), while `FileDocs` parses off-graph on `FileText.parsed` and composes with cached rendered signatures at hover time. Editing `///` comments re-checks **`0` declarations** unless `--warnon:3390` is enabled.
- **Extra wait when waiting for settle during active edits (Slice M, `n = 60` body edits, `378/378` answers equal):**
  - `large`: extra wait p95 `70.0 ms` (hover), `74.6 ms` (definition), `162.5 ms` (references).
  - `huge` (`CheckExpressions.fs`, ~13,000 lines): extra wait p50 `1,658 / 1,739 / 1,819 ms`, p95 `2,804 / 2,873 / 3,006 ms`.
- **Safe last-state answers while `"Checking"` is active (Slice 8h + 8r):**
  - Hover answers immediately (marked as updating in markdown) when row-relative mapping from the last published state is safe (`480` grid cases, `86` marked, `0` mismatches).
  - References and definition answer immediately from the last whole state while status-bar `workDoneProgress` is visible (`874` grid cases, `55` references and `124` definitions answered from last state, `0` mismatches), cutting `large-edit-references` p50 `57 → 33 ms` and `huge-edit-references` p50 `814 → 715 ms`.

---

## 3. Debug Tracing (`RANVIER_TRACE`) vs Release Bug Replay (`Ranvier.Persistence`)

- **`RANVIER_TRACE` is for Debug / development only:** `Directory.Build.props` defines `RANVIER_TRACE` in `Debug` builds to power `rfs/why` causal trace inspection. By design, Ranvier's `TraceLog` retains `TraceEvent[]` entries in Debug sessions (which roots old `ProjectGraph` instances across workspace reloads in Debug builds).
- **Release builds are untraced and leak-free:** Release IL contains zero `Trace` calls, and `WorkspaceTests` verifies via `WeakReference` that reloaded `ProjectGraph` instances are garbage-collected cleanly.
- **Reproducing bugs from Release runs:** Rather than enabling `RANVIER_TRACE` in Release, use the `Ranvier.Persistence` package (input journal) to persist input streams that trigger a bug in Release builds and replay those journals deterministically against a development (`RANVIER_TRACE`) build.

---

## 4. Known Test Flakes Under Full-Suite Parallel Load

When running full suites under heavy CPU contention, re-run any of these timing/race-sensitive tests in isolation (`--filter-test-case`) before treating an error as a regression:

1. `"cancelling ends the request during the scan"` (`Rfs.Server.Tests`)
2. `"a build finishes during continuous edits"` (`Rfs.Server.Tests`)
3. `"a cancelled request ends its wait and leaves the build for the next"` (`Rfs.Server.Tests`)
4. `"a project change waiting out the watched-file debounce counts as loading"` (`Rfs.Server.Tests`)
5. `"a fault of the project during its cold load fails the load"` (`Rfs.Server.Tests`)
6. `CrossProject` fixture `dotnet build` under concurrent process pressure (`Rfs.Checker.Tests` / `Rfs.Server.Tests`)
7. D1 `"flat module with a signature file costs about..."` sub-millisecond timing check (`Rfs.Decl.Tests`)
8. `"the finish phases add up to the finish time"` (`Rfs.Checker.Tests`)

---

## 5. Open and Parked Items

### Checker and FCS Fork (`Rfs.Checker` / `fickle-fsharp`)

- **R11 (same-file distant `type T with` augmentations):** Dropped before Milestone B. A same-file intrinsic augmentation far below `type T` (such as `il.fs` `type ILFieldSpec with` at line 4284) still spans its intermediate declarations (`huge-il-body`: `371` declarations re-checked after R12 + C, down from `476`).
- **Entity identity reuse across type re-checks (P0 cost on `large-body`):** P0 soundly invalidates downstream holders when a re-checked type creates a fresh `Entity` (`Queue.fs` `large-body`: `12` declarations re-checked per edit). Preserving `Entity` identity across body-only member edits is the remaining lever to bring `large-body` back to `1` declaration.
- **Graph-only free inference variable over-approximation (`M4`):** In concurrent declaration checks on certain patterns (e.g., `ASet.ofListTree`, `huge-il-newlet`: `146.7` decls/edit), an unsolved inference variable remains in the environment and over-approximates `DependsOnLater`. Sound (strict oracle fuzz passes), parked for targeted fork inspection.
- **File-level fallbacks (sound over-approximations):** `module rec`, attributed `module rec`, and multi-fragment (`namespace global`) files containing type augmentations (`UntypedAstUtils.fs` in `mid`) stay at file level.
- **Deep let-chains in `ParsedInput.TryGetCompletionContext`:** Recursive AST walks on 4,000+ nested `let` bindings require a large-stack thread (`GraphHost` and completion requests already run on 256 MB stack threads; `Completion.resolve` on extreme nesting can be moved onto the same helper if needed).
- **Type provider hover docs (`I4`):** Implemented and reviewed by reading; upstream FCS type-provider tooltip tests (`31/32`) are skipped upstream (`dotnet/fsharp#17330`).
- **Documented symbol-key limits (never stale — answer nothing when unsafe):** Signed `.fsi` parameters without implementation range, named-argument syntax in certain unkeyed shapes, generic overload ties, and two pre-existing FCS symbol-use gaps (constructor invoked via type abbreviation; namespace identifier inside a `module` header).

### Server and Feature Layer (`Rfs.Server` / `Rfs.Projects`)

- **Narrowing `LastState.prepare`:** `LastState.prepare` currently prepares named files with held answers on `didChange`; narrowing preparation strictly to files holding an active answer's cursor or hit locations (and scoping held-answer invalidation on project load to affected project closures rather than any load, plus bounding `Hover`'s answered-file set) will further trim large-workspace edit overhead.
- **Idle hover/definition cost past line ~6,000 on 13k-line files:** On `huge` (`CheckExpressions.fs`), `RowView.at` / fold replays add `120–340 ms` at lines 8,900–13,000; assembling a cached view or checkpointed prefix environments would flatten deep-file query latency.
- **Disk text as a reactive graph signal:** Closed-file disk reads currently feed through `Workspace` / `Publisher.Close` off the graph thread; moving disk text to a pure graph input signal was deferred after Slice 6 review found no staleness gap.
- **Artifacts-layout `ProjectSpec` cache freshness:** Under `.NET` `ArtifactsPath` layouts (such as `Fantomas`), `obj/project.assets.json` does not exist at the standard path, so `ProjectSpec` re-evaluates MSBuild on cold load instead of hitting its disk cache (affects cold load time only, never answers).
- **Project load notification metadata for Ionide:** `fsharp/project` notifications currently emit placeholder `OutputType`/`Output` fields (showing projects as libraries in Ionide's solution explorer tree).
- **Packaging & minor housekeeping:** Pin or suppress transitive `MessagePack` `2.5.108` `NU1902`/`NU1903` advisories from `Ionide.LanguageServerProtocol` (JSON-RPC formatter only is used); cancel/dispose `WaitAsync` 10-minute timeout timers eagerly when load tasks complete; deduplicate the test-only `dropTrace` helper.
- **Full `huge` FSAC comparison:** `build/final-run.ps1` (without `-Small`) remains ready whenever a multi-hour unattended `huge` + `large-edit` FSAC run is desired.
