# First references request against the rest (I5)

The first request of a run reads file-level results of files the session did not open and keeps them; the others find them kept. Latencies in ms.

| run | first | p50 of the other 9 | worst of the other 9 |
|---|---:|---:|---:|
| server-final/ranvier.large-references.json | 7,832 | 4 | 17 |
| server-final/ranvier.mid-references.json | 4,429 | 8 | 104 |
| server-final/ranvier.small-references.json | 1,309 | 6 | 2,548 |
| baseline-fsac-4c33a2a-extra/fsac-core-bg.mid-references.json | 1,296 | 65 | 325 |
| baseline-fsac-4c33a2a-extra/fsac-core-bg.small-references.json | 1,231 | 65 | 189 |
| baseline-fsac-4c33a2a-extra/fsac-core-tc.mid-references.json | 920 | 103 | 338 |
| baseline-fsac-4c33a2a-extra/fsac-core-tc.small-references.json | 908 | 28 | 218 |
