# Benchmark comparison

Runs: `ranvier` and `repeat`. Latencies are nearest-rank percentiles over steps that did not fail.

## large-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 43.2 | 55.7 | 8227.7 | 1.00 | 12.65 | 657.4 | 5213.4 | 0 | 0 |
| repeat | ranvier | 34.4 | 48.3 | 8017.1 | 1.00 | 12.65 | 659.5 | 5223.6 | 0 | 0 |

## large-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 11144.3 | - | - | 636.9 | 4589.2 | 0 | 0 |

## large-references

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 11615.0 | - | - | 733.4 | 8265.3 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/references |
| --- | --- | --- |
| ranvier | ranvier | 4.2 |

## large-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 12177.6 | - | - | 655.6 | 4756.6 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| ranvier | ranvier | 8.4 | 2.4 | 3.4 |

## large-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 28.5 | 63.8 | 9164.3 | 1.00 | 2.30 | 661.0 | 7684.9 | 0 | 0 |

## mid-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 41.3 | 104.5 | 4917.4 | 1.00 | 2.80 | 422.8 | 2781.8 | 0 | 0 |
| repeat | ranvier | 38.6 | 96.5 | 4942.7 | 1.00 | 2.80 | 423.8 | 2783.2 | 0 | 0 |

## mid-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 5494.6 | - | - | 402.6 | 2061.2 | 0 | 0 |

## mid-crossproject

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 32.2 | 74.4 | 5433.0 | 1.00 | 87.00 | 698.9 | 17988.0 | 0 | 0 |

## mid-eof-then-insert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 45.1 | 107.9 | 4970.0 | 1.00 | 2.30 | 465.2 | 2849.6 | 0 | 0 |

## mid-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 47.6 | 148.8 | 5043.7 | 1.00 | 3.60 | 420.3 | 2452.1 | 0 | 0 |

## mid-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 5826.9 | - | - | 441.4 | 2478.2 | 0 | 0 |

## mid-openmany-edits

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 123.0 | 494.9 | 14119.5 | 1.00 | 67.24 | 898.4 | 13742.6 | 0 | 0 |

## mid-references

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 4989.3 | - | - | 485.6 | 3392.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/references |
| --- | --- | --- |
| ranvier | ranvier | 7.6 |

## mid-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 4971.6 | - | - | 456.3 | 3000.5 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| ranvier | ranvier | 10.6 | 5.3 | 6.5 |

## mid-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 51.7 | 163.5 | 5019.7 | 1.00 | 6.60 | 541.7 | 6514.0 | 0 | 0 |

## mid-signed-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 36.1 | 85.6 | 4972.4 | 1.00 | 5.30 | 424.7 | 2770.0 | 0 | 0 |

## small-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 38.6 | 63.9 | 6032.0 | 1.00 | 1.75 | 274.6 | 2800.5 | 0 | 0 |
| repeat | ranvier | 35.2 | 63.0 | 8029.0 | 1.00 | 1.75 | 278.4 | 2829.1 | 0 | 0 |

## small-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 6091.2 | - | - | 240.4 | 1994.0 | 0 | 0 |

## small-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 9432.4 | - | - | 325.2 | 3781.4 | 0 | 0 |

## small-references

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 6114.0 | - | - | 337.2 | 3869.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/references |
| --- | --- | --- |
| ranvier | ranvier | 5.5 |

## small-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | - | - | 6568.9 | - | - | 255.1 | 2152.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| ranvier | ranvier | 7.0 | 6.0 | 15.4 |

## small-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 42.4 | 62.6 | 5983.9 | 1.00 | 2.50 | 384.6 | 9474.9 | 0 | 0 |
