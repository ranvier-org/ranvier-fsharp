# Milestone A results: the declaration graph after Tasks 1 to 7

Milestone A retired the six risks that `docs/checkpoint.md` found in the declaration graph. This document measures
the result on every scenario and reports each risk against its acceptance.

**Verdict.** Risks 2, 3, 4 and 5 are met. Risks 1 and 6 are met on latency but each has an acceptance line missed by
a small margin (three in all), so **two risks are not met to the letter.** They are listed first so nobody has to
read the tables to find them:

1. **Risk 1, allocation, small-signature.** decl allocates 744.8 MB per edit against file-slim's 735.8 MB (+1.2%).
   The acceptance is "edit allocations <= file-slim's". Unmet by 9 MB. It is the same in every run (747.5, 746.7,
   747.7, 747.6 MB). The latency half of risk 1 is met: decl p50 is 614.9 ms against 620.2 ms, a tie.
2. **Body p50 within 10% of the checkpoint's decl** (Task 4's line). mid-body is 31.4 ms against a limit of
   30.8 ms (28.0 x 1.1): +12.1%, unmet by 0.6 ms. small-body is 31.8 ms (+9.3%, met) and large-body 30.8 ms (+2.0%).
3. **Risk 6, large-lineinsert.** It adopts 12 declarations per edit, not <= 1. The controller has accepted this.
   Its p50 (35.7 ms) is also 0.7 ms over 1.2 x Task 5's large-body p50 (29.2 ms = 35.0 ms).

Carried, not decided here (controller rulings in `progress.md`):
- Body p95 and allocation per edit are worse than the checkpoint's and unattributed (below). That attribution
  belongs to the final review.
- GO stands with a narrowed claim. This run adds a data point against one part of that claim: **large-body is not
  "no slower" than file-slim here.** decl is 30.8 ms against 28.0 ms in the clean run (1.10x), and slower in 5 of 6
  runs (see "Body repeats"). The margin is 2 to 5 ms.

## How this was measured

| | |
|---|---|
| ranvier-fsharp (R) | `d119e80` (no product code changed in this task) |
| FCS fork | `fickle-fsharp` `06d54b778` |
| Ranvier | `d701e6c5` (master, the RV-1 merge) |
| build | Release; `dotnet build src/Rfs.Checkpoint -c Release` and `src/Rfs.Harness`, already up to date |
| command | one `rfs-checkpoint --scenario scenarios/<name>.json --corpus ../ranvier-fsharp-corpus --out results/server-a/final --step-timeout-s 900 --child-timeout-min 240` per scenario, cwd = R, scenarios strictly one after the other |
| validity | **every run's header says `ranvier: untraced`** (all 156 result files; checked in every log). No traced run was produced, so none had to be discarded or re-run for that reason |
| engines | one child process per engine and scenario, file-slim first; `--log-cutoff` off |

Scenarios run: the 20 the checkpoint driver can replay (open and edit steps only): `small|mid|large-body`,
`mid-signed-body`, `small|mid|large-signature`, `large-bigfile-narrow|wide`, `mid|large-lineinsert`,
`small|mid|large|huge-cold`, `huge-cx-rec|top|lineinsert`, `huge-il-body|lineinsert`. **Not run:** the three
`*-openmany` and three `*-requests` scenarios. The checkpoint driver refuses them (it replays only open and edit
steps); they belong to the server milestones.

Machine: AMD Ryzen 9 9900X (12 cores / 24 threads), 61.6 GB, Windows 11 Pro 10.0.26200, .NET SDK 10.0.403.

### Two passes, and why the tables use the second

The machine was not quiet. The controller reported, and the logs confirm, that another session's job
(`dotnet fsi differential/Generate.fsx generate 2000 4242`, PIDs 57280 and 60464, about 1.2 to 2 cores and 1.4 GB)
ran from 20:42 to about 22:18. My CPU check covered only the five idle build servers I was told about, so it did
not show it at the time.

- **Pass 1 (`busy-pass1/`)**: all 20 scenarios, 20:28 to 21:45, wholly inside that window. Its timings are from a busy
  machine. Its allocation, oracle and counter results stand. Kept for the record, not used for the tables below.
- **Body repeats (`repeat1` to `repeat4`, body scenarios)**: 21:47 to 22:18, also inside the window.
- **Repeats of signature, bigfile and lineinsert (`repeat1` to `repeat3`) and of the five edited huge scenarios
  (`repeat1`)**: 22:19 onward. The job had exited, so these are clean.
- **Pass 2 (`results/server-a/final/`, the tables below)**: all 20 scenarios again, 23:43 to 00:49, with the job
  gone. Foreign load was 3 to 34% CPU (browsers and editors); no foreign dotnet process above 7 CPU-s.

The controller asked for the body scenarios to be re-run for timing once the job had exited. Pass 2 is that re-run
(one sample per scenario). A planned set of four more clean body repeats was cancelled at the controller's request,
because Task 9's build would make later timings invalid. So **the clean body numbers are one sample each**; the
spread comes from the five busy runs.

The five idle dotnet build servers (20416, 48424, 60928, 61920, 62276) used no CPU before any scenario I started
(checked before and after each). They had exited on their own by 20:43; I did not stop them. PID 10768 stayed idle.

`rfs-harness compare results/checkpoint results/server-a/final` exited 0: **0 oracle diffs and 0 failed steps in every
row.** Across all 156 result files (both passes and every repeat) the totals are 0 oracle diffs and 0 failed steps.
The output is `results/server-a/final/compare-checkpoint.md`. Caveat: `results/checkpoint` predates the fix for
the traced build, and the checkpoint's Ranvier was a different, unmerged branch, so that comparison is not like for
like. Where `results/server-a/baseline-untraced` covers a scenario (body and signature on small, mid and large) it
is the like-for-like comparison: `results/server-a/final/compare-baseline-untraced.md`, and the tables here use both.

## Risks 1 to 6

Measured values are from pass 2 (clean) unless stated. "ratio" is decl over file-slim from the same run.

| risk | acceptance | measured | |
|---|---|---|---|
| **1 Signature edits** (checkpoint 2.7x to 5.8x slower, up to 3.1 GB per edit) | decl p50 <= file-slim p50 on small, mid and large signature; edit allocations <= file-slim's | p50 ms decl / file-slim: small 614.9 / 620.2 (0.99), mid 331.2 / 406.5 (0.81), large 241.8 / 272.3 (0.89). Allocation MB per edit: small **744.8 / 735.8**, mid 441.0 / 461.5, large 311.1 / 315.5. Four clean runs of small-signature give ratios 0.99, 0.99, 1.03, 1.04 (median p50 591 vs 596 ms) | **latency met (small is a tie); small-signature allocation unmet by 1.2%** |
| 1 (Task 4 line) | body p50 no more than 10% above the checkpoint's decl (29.1 / 28.0 / 30.2 ms) | 31.8 (+9.3%) / **31.4 (+12.1%)** / 30.8 (+2.0%) | small and large met; **mid unmet by 0.6 ms** |
| 1 memory | peak working set <= 1.25 x file-slim's on body and signature | worst ratio 1.04 (mid-signature 509 vs 490 MB); body and signature peaks 260 to 509 MB, against 416 to 1010 MB at the checkpoint | met |
| **2 FinishFile** (49 to 72% of a body edit) | finish ms p50 <= 50% of the checkpoint's: small <= 9.9, mid <= 9.1, large <= 7.9 | 3.2 / 8.5 / 2.5 ms (checkpoint 19.7 / 18.2 / 15.8). Finish share of the edit: 11.0% / 25.8% / 9.3% | met (mid uses 93% of its allowance) |
| **3 Cold start and memory** (2.2x to 3.0x file-slim) | decl cold ms <= 1.2 x file-slim's and cold allocations <= 1.5 x, on small, mid and large | cold ratio 1.10 / 1.02 / 0.99; allocation ratio 1.03 / 1.02 / 1.04. Huge: 1.15 and 1.05 | met |
| **4 Rec-module fallback** (CodePrinter.fs, 1.5x slower) | decl p50 <= file-slim p50 on `large-bigfile-narrow` and `-wide` | narrow 185.5 / 187.9 (0.99), wide 180.4 / 185.1 (0.97). Three more clean runs: narrow 1.05, 0.99, 1.02; wide 0.99, 0.97, 0.95 | met (narrow is a tie) |
| **5 D1 premise breaks** (5 / 32 / 1) | 0 breaks on small, mid and large | 0 / 0 / 0 at cold (and 0 on huge); segment rows at cold 0 / 0 / 0 (checkpoint 444 / 323 / 16) | met |
| **6 W1 line insert** (27 and 13 declarations re-checked) | adopted decls/edit <= 1; p50 <= 1.2 x the same corpus's body p50 from `t5` | mid-lineinsert: 0 adopted, 27 rows shifted, p50 37.1 ms against a limit of 44.2 (1.2 x 36.8). large-lineinsert: **12 adopted**, 1 shifted, p50 35.7 against a limit of 35.0 (1.2 x 29.2). huge-cx: 0 adopted (243 shifted), 868 ms. huge-il: 0 adopted (491 shifted), 676 ms | mid met. **large: 12 adopted declarations, accepted by the controller; p50 0.7 ms over** |

On risk 6's huge rows: `t5` has no huge scenario, so the 1.2x line is read against this run's own body rows.
huge-il-lineinsert is 0.59 x huge-il-body (676 / 1148). huge-cx-lineinsert is 0.62 x huge-cx-rec (868 / 1400) and
**1.39 x huge-cx-top** (868 / 626); the clean repeat gave 825 / 692 = 1.19.

Every numeric gate above is a single clean sample unless it says otherwise. The three misses are all within the
run-to-run spread of the neighbouring scenarios (see "Repeats"), but the acceptances are strict, so they are
reported as unmet.

## Full table per scenario

decl vs file-slim from `results/server-a/final/*.json` (pass 2) and its `extra/` side files, in the columns of
`docs/checkpoint.md` plus shifted rows. Percentiles are as the checkpoint's: p50 is the lower-middle element, p95 is
nearest-rank (with 10 edits it is the maximum).
- Cold MB: allocations during create plus the first check. Edit MB: allocations per edit, as a mean.
- Decls/edit: adopted declaration checks. Runs/edit: `CheckDeclaration` runs, superseded flights included.
- Shifted rows/edit: rows moved by range shifting instead of re-checked (Task 7).
- Finish ms and share: the p50 over edits of FinishFile time and of its share of the edit latency. `-` where an
  edit is checked file-level and no declaration finish runs.

| scenario | engine | p50 ms | p95 ms | cold ms | peak MB | cold MB | edit MB | decls/edit | runs/edit | shifted rows/edit | finish ms p50 | finish share p50 | oracle diffs |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| small-body | decl | 31.8 | 44.8 | 6353 | 260 | 1896 | 41.2 | 1.0 | 1.0 | 0.0 | 3.2 | 11.0% | 0 |
| small-body | file-slim | 51.3 | 121.8 | 5953 | 284 | 1830 | 63.4 | - | - | - | - | - | 0 |
| mid-body | decl | 31.4 | 82.5 | 4243 | 389 | 1739 | 34.5 | 1.0 | 1.0 | 0.0 | 8.5 | 25.8% | 0 |
| mid-body | file-slim | 73.7 | 224.6 | 4113 | 435 | 1704 | 72.0 | - | - | - | - | - | 0 |
| mid-signed-body | decl | 28.5 | 83.6 | 4353 | 380 | 1734 | 32.9 | 1.0 | 1.0 | 0.0 | 3.2 | 11.6% | 0 |
| mid-signed-body | file-slim | 45.6 | 139.8 | 4100 | 372 | 1699 | 51.8 | - | - | - | - | - | 0 |
| large-body | decl | 30.8 | 57.0 | 2954 | 318 | 987 | 28.0 | 1.0 | 1.0 | 0.0 | 2.5 | 9.3% | 0 |
| large-body | file-slim | 28.0 | 71.5 | 2955 | 316 | 952 | 31.2 | - | - | - | - | - | 0 |
| small-signature | decl | 614.9 | 1014.6 | 8414 | 318 | 1868 | 744.8 | 1.0 | 1.0 | 0.0 | 3.7 | 0.6% | 0 |
| small-signature | file-slim | 620.2 | 1528.7 | 7060 | 356 | 1832 | 735.8 | - | - | - | - | - | 0 |
| mid-signature | decl | 331.2 | 907.9 | 4371 | 509 | 1728 | 441.0 | 3.0 | 3.0 | 0.0 | 7.9 | 2.3% | 0 |
| mid-signature | file-slim | 406.5 | 1361.4 | 4837 | 490 | 1697 | 461.5 | - | - | - | - | - | 0 |
| large-signature | decl | 241.8 | 728.2 | 2893 | 378 | 991 | 311.1 | 1.0 | 1.0 | 0.0 | 1.9 | 0.8% | 0 |
| large-signature | file-slim | 272.3 | 769.5 | 3415 | 382 | 955 | 315.5 | - | - | - | - | - | 0 |
| large-bigfile-narrow | decl | 185.5 | 438.5 | 3019 | 393 | 987 | 164.8 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0% | 0 |
| large-bigfile-narrow | file-slim | 187.9 | 478.8 | 2780 | 411 | 949 | 168.1 | - | - | - | - | - | 0 |
| large-bigfile-wide | decl | 180.4 | 444.9 | 2982 | 395 | 991 | 164.7 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0% | 0 |
| large-bigfile-wide | file-slim | 185.1 | 523.9 | 2955 | 405 | 952 | 168.2 | - | - | - | - | - | 0 |
| mid-lineinsert | decl | 37.1 | 108.3 | 3933 | 383 | 1739 | 38.5 | 0.0 | 0.0 | 27.0 | 12.7 | 27.3% | 0 |
| mid-lineinsert | file-slim | 71.0 | 230.8 | 4003 | 386 | 1700 | 72.6 | - | - | - | - | - | 0 |
| large-lineinsert | decl | 35.7 | 75.3 | 2814 | 318 | 991 | 37.2 | 12.0 | 12.0 | 1.0 | 2.4 | 6.9% | 0 |
| large-lineinsert | file-slim | 28.0 | 93.5 | 2691 | 317 | 954 | 31.4 | - | - | - | - | - | 0 |
| huge-cx-rec | decl | 1400.4 | 2189.8 | 22931 | 2062 | 17001 | 1011.9 | 1.0 | 1.0 | 0.0 | 172.6 | 12.0% | 0 |
| huge-cx-rec | file-slim | 1223.9 | 1910.2 | 25046 | 1858 | 16096 | 1104.5 | - | - | - | - | - | 0 |
| huge-cx-top | decl | 625.7 | 1409.6 | 22086 | 1820 | 17014 | 624.6 | 1.0 | 1.0 | 0.0 | 151.2 | 24.5% | 0 |
| huge-cx-top | file-slim | 1274.0 | 2094.6 | 21432 | 1756 | 16131 | 1104.8 | - | - | - | - | - | 0 |
| huge-cx-lineinsert | decl | 868.1 | 1729.3 | 23260 | 1846 | 17000 | 694.5 | 0.0 | 0.0 | 243.0 | 185.3 | 20.7% | 0 |
| huge-cx-lineinsert | file-slim | 1259.7 | 1914.9 | 21152 | 1797 | 16119 | 1111.0 | - | - | - | - | - | 0 |
| huge-il-body | decl | 1148.2 | 1975.9 | 23838 | 1837 | 16992 | 1056.8 | 476.0 | 476.0 | 0.0 | 186.7 | 16.3% | 0 |
| huge-il-body | file-slim | 892.5 | 1602.8 | 20793 | 1752 | 16122 | 924.3 | - | - | - | - | - | 0 |
| huge-il-lineinsert | decl | 676.1 | 1417.1 | 22303 | 1775 | 16983 | 651.5 | 0.0 | 0.0 | 491.0 | 161.1 | 23.2% | 0 |
| huge-il-lineinsert | file-slim | 806.0 | 1464.1 | 21787 | 1716 | 16120 | 924.8 | - | - | - | - | - | 0 |

Reading the table:
- decl is faster than file-slim on small-body (0.62x), mid-body (0.43x), mid-signed-body (0.63x), mid-lineinsert (0.52x),
  mid-signature (0.81x), large-signature (0.89x), huge-cx-top (0.49x), huge-cx-lineinsert (0.69x) and
  huge-il-lineinsert (0.84x).
- It ties, within a few percent, on the signature small, the two bigfile scenarios, and small cold.
- It is slower on large-body (1.10x), large-lineinsert (1.27x), huge-cx-rec (1.14x) and huge-il-body (1.29x).
  The large pair are 2.8 and 7.7 ms.
- On signature edits `decls/edit` is 1 to 3 and `runs/edit` equals it: a signature edit no longer re-checks every
  later declaration (checkpoint: 223 to 590 per edit).
- The bigfile rows show 0 decls/edit: a `module rec` file is one row and is checked file-level, which is the intended
  fallback (risk 4).

## Against the checkpoint and the untraced baseline

decl p50 / p95 ms and edit MB. Checkpoint figures are `results/checkpoint` (traced Ranvier build, an unmerged
Ranvier branch, a lighter machine); "baseline" is `results/server-a/baseline-untraced`, the Task 7 re-baseline
(untraced, Ranvier `ce3a8b64`, 5 runs for large-body). Pass 2 is this task.

| scenario | checkpoint decl | baseline-untraced decl | pass 2 decl | pass 2 file-slim |
|---|---|---|---|---|
| small-body | 29.1 / 35.2, 16.6 MB | 34.9 / 54.0, 41.3 MB | 31.8 / 44.8, 41.2 MB | 51.3 / 121.8, 63.4 MB |
| mid-body | 28.0 / 61.7, 14.5 MB | 35.1 / 99.2, 34.6 MB | 31.4 / 82.5, 34.5 MB | 73.7 / 224.6, 72.0 MB |
| large-body | 30.2 / 47.7, 18.0 MB | 30.3 / 72.0, 28.1 MB (median of 5: 27.8) | 30.8 / 57.0, 28.0 MB | 28.0 / 71.5, 31.2 MB |
| small-signature | 3127 / 4010, 3121 MB | 525 / 1374, 748 MB | 615 / 1015, 745 MB | 620 / 1529, 736 MB |
| mid-signature | 1410 / 3601, 1181 MB | 326 / 912, 443 MB | 331 / 908, 441 MB | 407 / 1361, 462 MB |
| large-signature | 864 / 2286, 652 MB | 249 / 928, 311 MB | 242 / 728, 311 MB | 272 / 770, 316 MB |
| large-bigfile-narrow | 312.5 / 648.4, 220 MB | - | 185.5 / 438.5, 165 MB | 187.9 / 478.8, 168 MB |
| large-bigfile-wide | 322.9 / 470.0, 220 MB | - | 180.4 / 444.9, 165 MB | 185.1 / 523.9, 168 MB |
| mid-lineinsert | 73.8 / 186.4, 69 MB | - | 37.1 / 108.3, 39 MB | 71.0 / 230.8, 73 MB |
| large-lineinsert | 39.2 / 63.4, 30 MB | - | 35.7 / 75.3, 37 MB | 28.0 / 93.5, 31 MB |

- The compare against the checkpoint treats its decl and file-slim as one run each of a different build. Its large
  signature figure (864 ms) and the others are the checkpoint's R16 restart cost, which is what Tasks 3 and 4 removed.
- **Body p95 and allocation per edit are worse than the checkpoint's decl, and I have not attributed the rise.**
  p95: 44.8 / 82.5 / 57.0 ms against 35.2 / 61.7 / 47.7. Allocation: 41.2 / 34.5 / 28.0 MB per edit against
  16.6 / 14.5 / 18.0 (2.5x, 2.4x, 1.6x). The baseline already showed the same values, so this task did not move
  them. They are still below file-slim's allocation on all three (63.4, 72.0, 31.2 MB). The controller has assigned
  the attribution (dotnet-trace or a task-boundary bisect; allocation repeats to 0.04%) to the final review.
- Baseline against pass 2 shows the same shape with 5 to 20% run-to-run movement in p50 and larger in p95, which is
  the machine, not the code (nothing changed between them).

## Body repeats: median and spread

decl and file-slim p50 in ms, with the best and worst run. Five runs per body scenario were taken inside the foreign
job's window (the first pass and four repeats); one more (pass 2) was taken after it. large-body also had five
baseline runs (median 27.8 / 29.7).

| scenario | engine | busy runs (n=5): median, range | clean pass 2 |
|---|---|---|---|
| small-body | decl | 33.8, 30.8 to 36.9 | 31.8 |
| small-body | file-slim | 58.8, 49.5 to 77.9 | 51.3 |
| mid-body | decl | 36.8, 31.2 to 45.0 | 31.4 |
| mid-body | file-slim | 87.9, 70.7 to 100.7 | 73.7 |
| large-body | decl | 35.1, 27.3 to 37.6 | 30.8 |
| large-body | file-slim | 30.7, 29.4 to 31.2 | 28.0 |

- Busy p50 ratios decl over file-slim: small 0.62, 0.55, 0.62, 0.55, 0.47; mid 0.44, 0.44, 0.41, 0.51, 0.40;
  **large 0.93, 1.13, 1.14, 1.22, 1.14**. Pass 2: 0.62, 0.43 and 1.10.
- small and mid are decisive wins at any load. **large-body is not: decl is slower than file-slim in 5 of 6 runs,
  by 10 to 22%, or 2.7 to 6.6 ms.** The one run in decl's favour (0.93) is the first pass. This is at odds with the
  narrowed GO claim "no slower on large-body"; the clean sample agrees with the busy ones.
- The clean body figures are single samples; I could not take the planned further clean repeats (see above).
  decl's p50 moves by 10 to 14 ms between runs on mid and large under load, so the mid-body gate miss (31.4 ms vs
  30.8) is inside that spread.

## Other repeats

Signature, bigfile and lineinsert had three more clean runs each (`repeat1` to `repeat3`), so four clean samples
with pass 2. decl over file-slim p50:

| scenario | four clean runs |
|---|---|
| small-signature | 0.99, 0.99, 1.03, 1.04 |
| mid-signature | 0.81, 0.96, 0.99, 0.86 |
| large-signature | 0.89, 0.95, 1.01, 0.94 |
| large-bigfile-narrow | 0.99, 1.05, 0.99, 1.02 |
| large-bigfile-wide | 0.97, 0.99, 0.97, 0.95 |
| mid-lineinsert | 0.52, 0.52, 0.50, 0.48 |
| large-lineinsert | 1.27, 1.44, 1.25, 1.22 |

So small-signature, large-signature and the narrow bigfile sit at 1.0 within noise, mid-signature and wide bigfile
are consistently at or below it, and large-lineinsert is consistently 22 to 44% slower than file-slim.

## D2: the huge files

CheckExpressions.fs (13,757 lines, a `let rec` group of about 9,388 lines from line 4,295 to the end, one row) and
il.fs (6,284 lines, 488 declarations, not `rec`). Row sizes are in lines, from the `extra/` files: `edited` is the
declaration the edit falls in, `largest` the largest declaration of the edited file.

| scenario | edited row | largest row | decl p50 / p95 ms | file-slim p50 / p95 ms | ratio | decl / file-slim edit MB | decls/edit | shifted/edit | before (Task 1): decl / file-slim p50 |
|---|---|---|---|---|---|---|---|---|---|
| huge-cx-rec | 9,388 | 9,388 | 1400 / 2190 | 1224 / 1910 | 1.14 | 1012 / 1105 | 1 | 0 | 1778 / 1275 |
| huge-cx-top | 1 | 9,388 | 626 / 1410 | 1274 / 2095 | 0.49 | 625 / 1105 | 1 | 0 | 689 / 1551 |
| huge-cx-lineinsert | (line 100) | 9,388 | 868 / 1729 | 1260 / 1915 | 0.69 | 695 / 1111 | 0 | 243 | 2579 / 1303 |
| huge-il-body | 2 | 482 | 1148 / 1976 | 893 / 1603 | 1.29 | 1057 / 924 | 476 | 0 | 2024 / 1266 |
| huge-il-lineinsert | (line 100) | 482 | 676 / 1417 | 806 / 1464 | 0.84 | 652 / 925 | 0 | 491 | 2505 / 1263 |

A second clean sample of the same five (`repeat1`, taken 22:55 to 23:40): decl / file-slim p50
1368 / 1293 (cx-rec), 692 / 1384 (cx-top), 825 / 1468 (cx-lineinsert), 1162 / 847 (il-body), 686 / 976
(il-lineinsert). The ratios agree with the table (1.06, 0.50, 0.56, 1.37, 0.70). The first pass (busy) gave
cx-top 1307 / 1312 and cx-lineinsert 2167 / 1973; its file-slim il-body and il-lineinsert figures were 2.3x to 2.8x
the clean ones, which is the foreign job, and is why pass 2 is used.

- **`huge-cx-rec` against file-slim is the rec-group floor.** With one 9,388-line row, an edit anywhere in the group
  re-checks all of it as one declaration plus a finish of 173 ms. decl takes 1400 ms against file-slim's 1224 ms
  (1.14x; 1.06x in the second clean sample). Its edit allocation is 8% below file-slim's. Before Task 4 it was
  1778 / 1275 ms (1.39x), so the floor moved from 1.4x to about 1.1x slower, not to parity.
- `huge-cx-top`, an edit to a one-line declaration in the same file, takes 626 ms against 1274 ms. The 9,388-line row
  is not re-checked, so the saving is the whole file's rec-group check. Its finish is 151 ms (24% of the edit).
- **A rec group costs about what a whole-file check costs.** decl's 1400 ms for one row of 9,388 lines and
  file-slim's 1224 ms for the whole project are the same order, which is the point of the file-level fallback.
- `huge-il-body` is the one loss that is not a rec group: 1.29x slower (1.37x in the second sample). The edit falls
  in a 2-line declaration, yet **476 of the file's 488 declarations are re-checked per edit** (adopted decls/edit
  = 476; the same in `before` and `t4`, so this is not a regression of Task 7). Why one literal reaches 476
  declarations is not investigated here. It is the first thing to look at for a large non-rec file.
- Line inserts are the clearest win: shifting 243 and 491 rows leaves 0 declarations to re-check, 0.69x and 0.84x
  file-slim (before Task 7: 2.0x and 2.0x slower).
- Edit allocations on huge are about 1 GB per edit for both engines; decl's are lower except on huge-il-body.
- Peak working set on huge: decl 1.8 to 2.1 GB, file-slim 1.7 to 1.9 GB (cx-rec 2062 vs 1858 MB, 1.11x).

## Memory and cold

| corpus | decl cold ms | file-slim cold ms | ratio | decl cold alloc MB | file-slim cold alloc MB | ratio | decl peak MB | file-slim peak MB | premise breaks at cold | segment rows at cold |
|---|---|---|---|---|---|---|---|---|---|---|
| small | 6241 | 5694 | 1.10 | 1896 | 1832 | 1.03 | 209 | 224 | 0 | 0 |
| mid | 4095 | 4019 | 1.02 | 1736 | 1699 | 1.02 | 351 | 339 | 0 | 0 |
| large | 3109 | 3132 | 0.99 | 992 | 955 | 1.04 | 286 | 287 | 0 | 0 |
| huge | 23140 | 20110 | 1.15 | 16990 | 16110 | 1.05 | 1251 | 1209 | 0 | 0 |

- Cold start is now 0.99x to 1.10x file-slim on small, mid and large (checkpoint 2.2x to 3.0x). The checkpoint's
  decl cold times were 16254, 11094 and 7433 ms; they are 6241, 4095 and 3109 ms. On huge it is 1.15x (23.1 s against
  20.1 s). The file-slim figure in each row is from the same run.
- Cold allocations are at most 1.05x file-slim's (checkpoint 2.2x to 4.4x; small's was 8 GB and is now 1.9 GB).
- Peak working set on body edits: decl 260 / 389 / 318 MB against file-slim's 284 / 435 / 316 MB (checkpoint decl
  416 / 454 / 364). On signature edits decl peaks at 318 / 509 / 378 MB (checkpoint 1010 / 911 / 670).
- The premise breaks and segment rows the cold check reports are 0 on all four corpora.
- These cold times include the project-load phases of the checkpoint driver, not LSP; they are not comparable with
  FSAC's.

## Caveats

- **The machine was not quiet.** Pass 1 and the body repeats ran while another session's job used 1.2 to 2 cores
  (20:42 to about 22:18). Pass 2 ran after it exited, with foreign load of 3 to 34% CPU from desktop applications
  (browsers, editors, and four busy python processes of unknown origin). Timings are therefore
  indicative: on a quiet machine p50s may be a few ms lower and the ratios closer to 1.
- **One clean sample per scenario** (pass 2), except the signature, bigfile and lineinsert scenarios (four clean
  runs) and the five edited huge scenarios (two clean runs). The body scenarios have five busy repeats and one clean
  sample. The brief asked for a repeat of the body scenarios; a further four clean repeats were cancelled at the
  controller's request, because Task 9's build was about to start. Where a number here comes from a single run it
  is only a sample.
- **The controller may yet stop the foreign job.** It had exited by the time pass 2 started.
- **Layer.** decl and file-slim are in-process and re-check the whole project; FSAC's numbers are a different layer
  and are not repeated here.
- **The oracle checks diagnostics only.** A wrong cached typed tree that does not change diagnostics would not show.
- **Percentile definition.** p50 is the lower-middle element; with 10 edits p95 is the maximum.
- **Idle dotnet servers.** The five idle build servers from earlier work used no CPU and had exited on their own by
  20:43; I stopped nothing. PID 10768 (older) also stayed idle.
- **Not covered:** `*-openmany` and `*-requests` (the checkpoint driver cannot replay them), and the huge-scenario
  premise-break and segment figures beyond the cold counters.

## Open items for the controller

1. The three strict misses (small-signature allocation +1.2%, mid-body +12.1% over the checkpoint, large-lineinsert
   12 adopted and +2% on p50) are listed at the top. Milestone B waits on the controller's ruling on them.
2. large-body is slower than file-slim in this task's runs (1.10x clean; 5 of 6 runs). That is against the narrowed
   claim.
3. Body p95 and allocation per edit: unattributed (final review).
4. huge-il-body re-checks 476 declarations for one literal edit, and is 1.29x slower than file-slim.

## Files

`results/server-a/final/`: pass 2 (`decl.*`, `file-slim.*`, `extra/`, `logs/`), the compares
(`compare-checkpoint.md`, `compare-baseline-untraced.md`), `busy-pass1/`, `repeat1` to `repeat4` (body repeats are
busy; every other scenario in `repeat1` to `repeat3` is clean, and `repeat1` also holds the second huge sample).
`final/logs/overlap-log.txt` holds both passes (a move of the folder was denied while the log was open; pass 1's
copy is in `busy-pass1/logs/`; its per-scenario `.log` files in `final/logs/` are pass 2's).
Each directory's `logs/overlap-log.txt` records the idle-server CPU and the machine load around every scenario.
