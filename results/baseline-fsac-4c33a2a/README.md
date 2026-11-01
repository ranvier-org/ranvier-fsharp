# FSAC baseline at 4c33a2a

Baseline numbers for FsAutoComplete upstream `main` at
[`4c33a2a7223ea1a885c6bf91e92c7c967c524597`](https://github.com/ionide/FsAutoComplete/commit/4c33a2a7223ea1a885c6bf91e92c7c967c524597),
4 server configurations x 15 scenarios = 60 result files, plus the 2 big-file scenarios on the two core
configurations = 64 result files in this directory, no failed steps.

Project references are built: `build/fetch-corpus.ps1` builds every corpus project in Debug (Fantomas.FCS for
large, FsAutoComplete.Logging for mid), `ProjectSpec.load` fails when a `-r:` assembly is missing, and
`rfs-harness verify-corpus` reports 0 oracle errors for small, mid and large. An earlier pass of this baseline
was made without those builds, so the oracle checked Fantomas.Core with 2633 errors; the files for the
`mid` and `large` body and signature scenarios and the big-file scenarios were re-run after the fix and
replace the earlier ones. The cold, requests and openmany files (and the whole small corpus) were not re-run: the
oracle diffs they record are 0 steps, because they have no edits, and FSAC's own numbers do not depend on the oracle.

## Machine

| | |
|---|---|
| CPU | AMD Ryzen 9 9900X, 12 cores / 24 threads |
| RAM | 61.6 GB |
| OS | Windows 11 Pro 10.0.26200 |
| .NET SDK | 10.0.403 (this repo's `global.json`; FSAC loads every corpus project with it, see below) |
| FSAC runtime | .NET 10 (`fsautocomplete.dll` under `dotnet`, Server GC as FSAC configures it) |
| FSAC's FCS | FSharp.Compiler.Service 43.12.402 (paket.lock at 4c33a2a) |

## Servers

`servers.json`. All four run `dotnet ../ranvier-fsharp-fsac/src/FsAutoComplete/bin/Release/net10.0/fsautocomplete.dll`
with `oracle: "report"`, and load the scenario's project with `fsharp/workspaceLoad` before the first open
(its time is part of the cold number).

| name | args | init options |
|---|---|---|
| `fsac-core-bg` | none | core loop |
| `fsac-core-tc` | `--use-fcs-transparent-compiler` | core loop |
| `fsac-default-bg` | none | `{}` (FSAC defaults: the unnecessary-parentheses analyzer is on) |
| `fsac-default-tc` | `--use-fcs-transparent-compiler` | `{}` |

Core loop: every `FSharpConfigDto` boolean that turns on something besides diagnostics, hover, completion,
definition and references is `false` (analyzers, linter, stub generation, code lenses, line lens `never`,
pipeline and inlay hints, binlog, background progress), `AutomaticWorkspaceInit` is `false` because the
harness loads the project itself, and `Fsac` is at its defaults. FSAC at this commit has no setting for test
detection: it runs on every parse and sends `fsharp/testDetected` in every configuration. The options go
in `initializationOptions` and again as `{ "FSharp": ... }` in `workspace/didChangeConfiguration`.

The servers start in this repository (`"cwd": "."`), not in the corpus checkout: FSAC picks the SDK it loads
projects with from its working directory's `global.json`, and the large corpus's clone pins
11.0.100-rc.1, whose MSBuild cannot load into FSAC's .NET 10 process. From here every project loads with
SDK 10.0.403, the same SDK the oracle and the harness's project specs use.

## Scenarios

`scenarios/*.json`. The edited file is the one near the middle of each project's compile order that has
no `.fsi` (so a return-type edit changes the published signature) and has a literal in a function or
member body:

| corpus | project | file (compile index) |
|---|---|---|
| small | FSharp.Data.Adaptive | `Core/Transaction.fs` (20 of 50), CRLF |
| mid | FsAutoComplete.Core | `DocumentationFormatter.fs` (30 of 54), LF |
| large | Fantomas.Core | `Queue.fs` (12 of 33), CRLF; the files nearer the middle have an `.fsi`, no literal or no public function |

Big file (`large-bigfile-wide`, `large-bigfile-narrow`, core configurations only): `CodePrinter.fs` of
Fantomas.Core, 5324 lines, CRLF, after its `CodePrinter.fsi`. It is `module internal rec`, so the whole file is one
recursive group, and it has an `.fsi`. 20 toggled edits each: wide changes `IndentSize < 4` to `< 5` (line 2078, deep in the file), narrow changes `"cannot ..."` to `"Cannot ..."` in an error
message string (line 41).

- cold: open the file.
- body: 20 toggled edits of a literal in one body (`"cannot ..."` -> `"Cannot ..."`, `3` -> `4`, `31` -> `37`).
- signature: 10 toggled edits that change a public function's return type (`useTransaction`: `|> string`,
  `getAPCaseSignature`: `|> String.length` since it already returns a string, `Queue.ofList`: `|> string`).
- requests: 10 hovers, 10 completions right after a `.`, 10 definitions, at fixed positions in the file.
- openmany: open the first 20 files of the compile order.

## Reproduce

```powershell
pwsh build/fsac.ps1          # clone + build FSAC at 4c33a2a into ../ranvier-fsharp-fsac
pwsh build/fetch-corpus.ps1  # if ../ranvier-fsharp-corpus is missing
# from this repository's root:
dotnet run --project src/Rfs.Harness -c Release -- run --servers servers.json `
  --server fsac-core-bg fsac-core-tc fsac-default-bg fsac-default-tc `
  --scenario "scenarios/*.json" --corpus ../ranvier-fsharp-corpus --out results/baseline-fsac-4c33a2a
```

The tables below come from `rfs-harness compare` on two directories holding one server's result files each.

`fsac-core-tc` on `mid-openmany` timed out once in the first pass: one of the 20 files got no publish within
120 s. Re-run on its own with `--step-timeout-s 400`, it finished in 4.3 s, and that run's file is the one here.
The other 59 files come from one pass, apart from the small corpus, which was run separately after the first
pass was interrupted.

## Oracle diffs (informational)

The oracle checks with the FCS fork, and FSAC uses FCS 43.12.402, so their messages can differ (for example
`--strict-indentation` advice, or the wording of a type mismatch). Diffs are recorded, not enforced. Every run
here recorded 0 oracle diffs, with the references built (see top) and compared per file after every edit. The edits in these scenarios leave error positions and messages that both
compilers agree on. A separate CRLF check (an edit that turns a literal into a type error, and one that inserts
a CRLF line break) put FSAC's diagnostics at the same lines and columns as the oracle's in both CRLF files; the
only differences there were in message wording.

## Measurement notes

- An edit step ends at the first `publishDiagnostics` carrying the edit's version. In FSAC that publish is the
  compiler's result: FSAC publishes the merged diagnostics of every source, stamped with the highest version
  among them, and analyzers run only after the compiler's publish for that version. Traces show one publish per
  edit in the core configuration (`F# Compiler` only). In the default configuration there are two: the
  compiler's, then the parentheses analyzer's (Publishes/edit 2.00).
- On the big file FSAC with the background compiler needs 225-242 ms p50 per edit against 79-85 ms with the
  transparent compiler (tables below), and peaks at about 1.3 GB against 0.6 GB.
- FSAC re-checks only the open file, so the large corpus's edits (file 12 of 33) are cheap. Its cold and
  openmany numbers carry the project load, which includes the Fantomas.FCS project reference.

## fsac-core-bg vs fsac-core-tc
Runs: `fsac-core-bg` and `fsac-core-tc`. Latencies are nearest-rank percentiles over steps that did not fail.

### large-bigfile-narrow

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 225.4 | 292.7 | 9563.6 | 1.00 | - | 1313.2 | 9276.9 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 84.5 | 129.8 | 5991.0 | 1.00 | - | 616.5 | 2676.3 | 0 | 0 |

### large-bigfile-wide

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 241.7 | 515.8 | 14907.4 | 1.00 | - | 1308.6 | 9260.9 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 79.5 | 142.9 | 5839.5 | 1.00 | - | 615.9 | 2676.4 | 0 | 0 |

### large-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 22.5 | 31.2 | 10834.6 | 1.00 | - | 1083.3 | 5291.3 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 42.4 | 84.9 | 6518.6 | 1.00 | - | 492.4 | 1720.8 | 0 | 0 |

### large-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 11750.3 | - | - | 1036.2 | 5128.8 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 11590.6 | - | - | 1035.3 | 5136.0 | 0 | 0 |

### large-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 21674.0 | - | - | 5145.1 | 37048.5 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 12063.3 | - | - | 1157.7 | 5622.2 | 0 | 0 |

### large-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 11432.1 | - | - | 1036.0 | 5148.2 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 11426.7 | - | - | 1128.8 | 5153.9 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 6.9 | 0.5 | 4.1 |
| fsac-core-tc | fsac-core-tc | 8.5 | 0.6 | 3.7 |

### large-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 21.2 | 65.8 | 9739.9 | 1.00 | - | 1019.8 | 5206.5 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 36.7 | 109.6 | 5979.3 | 1.00 | - | 465.9 | 1526.7 | 0 | 0 |

### mid-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 87.5 | 291.1 | 9015.9 | 1.00 | - | 762.7 | 2656.1 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 74.9 | 261.0 | 6205.2 | 1.00 | - | 811.8 | 2266.0 | 0 | 0 |

### mid-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 8039.7 | - | - | 654.7 | 1578.0 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 7230.5 | - | - | 707.9 | 1558.4 | 0 | 0 |

### mid-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 13395.5 | - | - | 3120.4 | 7499.7 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 6616.0 | - | - | 706.2 | 2017.9 | 0 | 0 |

### mid-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 7900.4 | - | - | 635.9 | 1777.9 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 7244.8 | - | - | 701.3 | 1744.7 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 10.9 | 1.3 | 17.2 |
| fsac-core-tc | fsac-core-tc | 9.8 | 0.8 | 13.4 |

### mid-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 85.5 | 251.5 | 6166.0 | 1.00 | - | 759.8 | 2114.2 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 67.1 | 317.5 | 5041.1 | 1.00 | - | 747.2 | 1872.3 | 0 | 0 |

### small-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 28.0 | 53.2 | 7610.5 | 1.00 | - | 453.1 | 1936.3 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 43.7 | 119.3 | 7073.6 | 1.00 | - | 462.9 | 2051.9 | 0 | 0 |

### small-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 7516.2 | - | - | 432.7 | 1585.2 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 7222.9 | - | - | 424.1 | 1442.6 | 0 | 0 |

### small-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 12650.8 | - | - | 1666.3 | 15574.6 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 8415.9 | - | - | 469.1 | 1787.4 | 0 | 0 |

### small-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | - | - | 7158.8 | - | - | 812.3 | 2133.3 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | - | - | 7333.7 | - | - | 812.4 | 1979.0 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 5.9 | 0.7 | 14.9 |
| fsac-core-tc | fsac-core-tc | 5.6 | 0.6 | 13.0 |

### small-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-bg | fsac-core-bg | 29.3 | 92.7 | 7479.8 | 1.00 | - | 453.9 | 1759.4 | 0 | 0 |
| fsac-core-tc | fsac-core-tc | 47.6 | 137.8 | 7117.3 | 1.00 | - | 463.3 | 1781.1 | 0 | 0 |
## fsac-default-bg vs fsac-default-tc
Runs: `fsac-default-bg` and `fsac-default-tc`. Latencies are nearest-rank percentiles over steps that did not fail.

### large-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 18.5 | 55.1 | 8745.2 | 2.00 | - | 1063.2 | 5304.5 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | 37.9 | 55.4 | 4948.2 | 2.00 | - | 512.7 | 1740.1 | 0 | 0 |

### large-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 11697.2 | - | - | 1055.7 | 5109.1 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 11814.9 | - | - | 1004.4 | 5131.7 | 0 | 0 |

### large-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 23939.9 | - | - | 5471.7 | 40716.5 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 11350.3 | - | - | 1166.2 | 5625.6 | 0 | 0 |

### large-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 11030.3 | - | - | 1073.3 | 5132.2 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 11066.3 | - | - | 1030.7 | 5144.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 9.3 | 0.5 | 3.0 |
| fsac-default-tc | fsac-default-tc | 6.5 | 0.5 | 3.5 |

### large-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 23.0 | 99.7 | 11778.7 | 2.00 | - | 1067.6 | 5216.2 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | 45.9 | 122.3 | 7104.4 | 2.00 | - | 480.2 | 1534.3 | 0 | 0 |

### mid-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 86.5 | 258.4 | 6594.9 | 2.00 | - | 743.6 | 2741.8 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | 61.8 | 101.7 | 4866.0 | 2.00 | - | 800.9 | 2320.4 | 0 | 0 |

### mid-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 7708.1 | - | - | 690.1 | 1537.1 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 6724.0 | - | - | 656.8 | 1555.9 | 0 | 0 |

### mid-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 10866.4 | - | - | 3066.4 | 7722.5 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 10754.6 | - | - | 792.8 | 2019.8 | 0 | 0 |

### mid-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 9114.8 | - | - | 715.0 | 1740.0 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 6409.5 | - | - | 707.9 | 1723.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 10.2 | 0.8 | 21.1 |
| fsac-default-tc | fsac-default-tc | 9.9 | 0.7 | 14.5 |

### mid-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 74.9 | 260.3 | 6179.2 | 2.00 | - | 747.5 | 2137.2 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | 56.7 | 334.6 | 4922.7 | 2.00 | - | 716.9 | 1937.8 | 0 | 0 |

### small-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 26.8 | 61.8 | 6875.7 | 2.00 | - | 455.4 | 1954.3 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | 45.3 | 77.6 | 7169.3 | 2.00 | - | 464.1 | 2050.9 | 0 | 0 |

### small-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 7560.2 | - | - | 433.0 | 1602.1 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 6781.5 | - | - | 431.4 | 1435.8 | 0 | 0 |

### small-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 9046.0 | - | - | 1659.6 | 15414.2 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 6657.6 | - | - | 499.7 | 1824.9 | 0 | 0 |

### small-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | - | - | 7606.4 | - | - | 805.4 | 2114.2 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | - | - | 7138.7 | - | - | 821.8 | 1976.6 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 5.8 | 0.8 | 14.0 |
| fsac-default-tc | fsac-default-tc | 5.5 | 0.7 | 14.9 |

### small-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-default-bg | fsac-default-bg | 27.2 | 84.9 | 7082.5 | 2.00 | - | 448.1 | 1769.2 | 0 | 0 |
| fsac-default-tc | fsac-default-tc | 46.5 | 121.8 | 6563.9 | 2.00 | - | 455.5 | 1750.8 | 0 | 0 |
