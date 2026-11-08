# First end-to-end run of the Ranvier server (cut down)

`ranvierautocomplete` (`src/Rfs.Server`, Release) driven by `rfs-harness` through `servers.json` entry `ranvier`
(`oracle: strict`, `fsharp/workspaceLoad`, `AutomaticWorkspaceInit: false`).

**This was a cut-down run, not Task 12 as briefed.** Only the `small` and `mid` corpora, one pass of nine scenarios:
`small-cold`, `small-body`, `small-signature`, `small-openmany`, `mid-cold`, `mid-body`, `mid-signature`,
`mid-lineinsert`, `mid-openmany`. Not run: `large-*`, `huge-*`, big-file, and every `*-requests` scenario (they wait
for Milestone C). `verify-corpus` was limited to small and mid with a two-entry manifest (the command has no corpus
filter): 0 errors on both.

**Every timing here was recorded while another build was running on the machine. None of it is a result.** Task 19 is the
only quoted timing; do not read speed-ups or slow-downs from these tables.

## Outcome

- 9 result files, one per scenario, one run each: **0 failed steps, 0 oracle diffs** (oracle `strict`, checked after every edit).
- Publishes per edit: 1.00 in every edit scenario, the same as FSAC's core configurations.

## Machine

Same machine as `results/baseline-fsac-4c33a2a`: AMD Ryzen 9 9900X (12 cores / 24 threads), 61.6 GB RAM, Windows 11 Pro
10.0.26200, .NET SDK 10.0.403, server on .NET 10 (`dotnet ranvierautocomplete.dll`), FCS = the fork. Loaded by a concurrent
build for the whole run.

## Comparison with the FSAC baseline (recorded under load)

`rfs-harness compare` of `results/baseline-fsac-4c33a2a` (`fsac-core-bg`, `fsac-core-tc`) against this directory. The
baseline has no `mid-lineinsert`. Rows show the scenarios that ran.

| Scenario | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Peak MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| small-body | fsac-core-bg | 28.0 | 53.2 | 7610.5 | 1.00 | 453.1 | 0 | 0 |
| small-body | fsac-core-tc | 43.7 | 119.3 | 7073.6 | 1.00 | 462.9 | 0 | 0 |
| small-body | ranvier | 43.3 | 75.2 | 6747.4 | 1.00 | 262.9 | 0 | 0 |
| small-signature | fsac-core-bg | 29.3 | 92.7 | 7479.8 | 1.00 | 453.9 | 0 | 0 |
| small-signature | fsac-core-tc | 47.6 | 137.8 | 7117.3 | 1.00 | 463.3 | 0 | 0 |
| small-signature | ranvier | 39.8 | 68.9 | 6054.3 | 1.00 | 334.1 | 0 | 0 |
| small-cold | fsac-core-bg | - | - | 7516.2 | - | 432.7 | 0 | 0 |
| small-cold | fsac-core-tc | - | - | 7222.9 | - | 424.1 | 0 | 0 |
| small-cold | ranvier | - | - | 7108.7 | - | 219.1 | 0 | 0 |
| small-openmany | fsac-core-bg | - | - | 12650.8 | - | 1666.3 | 0 | 0 |
| small-openmany | fsac-core-tc | - | - | 8415.9 | - | 469.1 | 0 | 0 |
| small-openmany | ranvier | - | - | 9993.9 | - | 327.5 | 0 | 0 |
| mid-body | fsac-core-bg | 87.5 | 291.1 | 9015.9 | 1.00 | 762.7 | 0 | 0 |
| mid-body | fsac-core-tc | 74.9 | 261.0 | 6205.2 | 1.00 | 811.8 | 0 | 0 |
| mid-body | ranvier | 41.0 | 74.9 | 4104.2 | 1.00 | 389.2 | 0 | 0 |
| mid-signature | fsac-core-bg | 85.5 | 251.5 | 6166.0 | 1.00 | 759.8 | 0 | 0 |
| mid-signature | fsac-core-tc | 67.1 | 317.5 | 5041.1 | 1.00 | 747.2 | 0 | 0 |
| mid-signature | ranvier | 55.3 | 132.1 | 4225.4 | 1.00 | 500.8 | 0 | 0 |
| mid-cold | fsac-core-bg | - | - | 8039.7 | - | 654.7 | 0 | 0 |
| mid-cold | fsac-core-tc | - | - | 7230.5 | - | 707.9 | 0 | 0 |
| mid-cold | ranvier | - | - | 4198.2 | - | 384.0 | 0 | 0 |
| mid-openmany | fsac-core-bg | - | - | 13395.5 | - | 3120.4 | 0 | 0 |
| mid-openmany | fsac-core-tc | - | - | 6616.0 | - | 706.2 | 0 | 0 |
| mid-openmany | ranvier | - | - | 6626.4 | - | 399.6 | 0 | 0 |
| mid-lineinsert | ranvier | 48.3 | 113.8 | 4210.7 | 1.00 | 409.7 | 0 | 0 |

## LSP overhead (recorded under load)

Server body p50 minus the in-process `decl` body p50 of `results/server-a/final` (small 31.8 ms, mid 31.4 ms):

| Corpus | Server body p50 | In-process decl body p50 | Difference |
| --- | --- | --- | --- |
| small | 43.3 ms | 31.8 ms | 11.5 ms |
| mid | 41.0 ms | 31.4 ms | 9.6 ms |

Small is over the 10 ms limit by 1.5 ms, mid is under. The server run was loaded and `server-a/final` was not, so the
difference cannot be attributed to the LSP layer from this run. The publish path (`Publisher.run`) was read: one wait for
the settled result per edit, one `publishDiagnostics` (1.00 per edit), a list comparison and a `declarationsChecked`
notification. It has no timer, delay or poll; its work is small next to a 30 ms check. No counter was added. The limit
needs a quiet re-run (Task 19) to be decided.

## Note on "Decls/edit"

The harness reports a mean of 1.75 (small-body) and 2.80 (mid-body) declarations checked per edit, against 1.00 in-process.
That is an attribution artefact: `rfs/declarationsChecked` counts checks adopted since the previous notification, and the open
step's notification precedes the checks it started. The first edit then carries them (16 on small-body, 37 on mid-body);
every later edit in all the body scenarios is exactly 1.

## Reproduce

```powershell
dotnet build RanvierFSharp.slnx -c Release
dotnet src/Rfs.Harness/bin/Release/net10.0/rfs-harness.dll run --servers servers.json --server ranvier `
  --scenario "scenarios/<name>.json" --corpus ../ranvier-fsharp-corpus --out results/server-b
```
