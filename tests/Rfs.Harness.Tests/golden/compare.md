# Benchmark comparison

Runs: `a` and `b`. Latencies are nearest-rank percentiles over steps that did not fail.

## body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| b | fsac | - | - | 4200.0 | - | - | 900.0 | 4096.0 | 1 | 1 |
| a | ranvier | 20.0 | 40.0 | 1500.0 | 1.75 | 4.00 | 512.5 | 2048.5 | 0 | 1 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/hover |
| --- | --- | --- | --- |
| b | fsac | - | - |
| a | ranvier | 50.0 | 7.0 |
