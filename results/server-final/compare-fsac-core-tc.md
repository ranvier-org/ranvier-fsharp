# Benchmark comparison

Runs: `ranvier` and `fsac-core-tc`. Latencies are nearest-rank percentiles over steps that did not fail.

## large-bigfile-narrow

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 84.5 | 129.8 | 5991.0 | 1.00 | - | 616.5 | 2676.3 | 0 | 0 |

## large-bigfile-wide

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 79.5 | 142.9 | 5839.5 | 1.00 | - | 615.9 | 2676.4 | 0 | 0 |

## large-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 42.4 | 84.9 | 6518.6 | 1.00 | - | 492.4 | 1720.8 | 0 | 0 |
| ranvier | ranvier | 43.2 | 55.7 | 8227.7 | 1.00 | 12.65 | 657.4 | 5213.4 | 0 | 0 |

## large-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 11590.6 | - | - | 1035.3 | 5136.0 | 0 | 0 |
| ranvier | ranvier | - | - | 11144.3 | - | - | 636.9 | 4589.2 | 0 | 0 |

## large-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 12063.3 | - | - | 1157.7 | 5622.2 | 0 | 0 |

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
| fsac-core-tc | fsac-core-tc | - | - | 11426.7 | - | - | 1128.8 | 5153.9 | 0 | 0 |
| ranvier | ranvier | - | - | 12177.6 | - | - | 655.6 | 4756.6 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 8.5 | 0.6 | 3.7 |
| ranvier | ranvier | 8.4 | 2.4 | 3.4 |

## large-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 36.7 | 109.6 | 5979.3 | 1.00 | - | 465.9 | 1526.7 | 0 | 0 |
| ranvier | ranvier | 28.5 | 63.8 | 9164.3 | 1.00 | 2.30 | 661.0 | 7684.9 | 0 | 0 |

## mid-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 74.9 | 261.0 | 6205.2 | 1.00 | - | 811.8 | 2266.0 | 0 | 0 |
| ranvier | ranvier | 41.3 | 104.5 | 4917.4 | 1.00 | 2.80 | 422.8 | 2781.8 | 0 | 0 |

## mid-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 7230.5 | - | - | 707.9 | 1558.4 | 0 | 0 |
| ranvier | ranvier | - | - | 5494.6 | - | - | 402.6 | 2061.2 | 0 | 0 |

## mid-crossproject

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 41.6 | 308.2 | 6197.7 | 1.00 | - | 713.0 | 1880.5 | 10 | 0 |
| ranvier | ranvier | 32.2 | 74.4 | 5433.0 | 1.00 | 87.00 | 698.9 | 17988.0 | 0 | 0 |

## mid-eof-then-insert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 45.1 | 107.9 | 4970.0 | 1.00 | 2.30 | 465.2 | 2849.6 | 0 | 0 |

## mid-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 51.8 | 355.1 | 4759.1 | 1.00 | - | 775.5 | 1887.8 | 0 | 0 |
| ranvier | ranvier | 47.6 | 148.8 | 5043.7 | 1.00 | 3.60 | 420.3 | 2452.1 | 0 | 0 |

## mid-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 6616.0 | - | - | 706.2 | 2017.9 | 0 | 0 |
| ranvier | ranvier | - | - | 5826.9 | - | - | 441.4 | 2478.2 | 0 | 0 |

## mid-openmany-edits

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ranvier | ranvier | 123.0 | 494.9 | 14119.5 | 1.00 | 67.24 | 898.4 | 13742.6 | 0 | 0 |

## mid-references

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 5503.0 | - | - | 873.2 | 3095.0 | 0 | 0 |
| ranvier | ranvier | - | - | 4989.3 | - | - | 485.6 | 3392.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/references |
| --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 103.5 |
| ranvier | ranvier | 7.6 |

## mid-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 7244.8 | - | - | 701.3 | 1744.7 | 0 | 0 |
| ranvier | ranvier | - | - | 4971.6 | - | - | 456.3 | 3000.5 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 9.8 | 0.8 | 13.4 |
| ranvier | ranvier | 10.6 | 5.3 | 6.5 |

## mid-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 67.1 | 317.5 | 5041.1 | 1.00 | - | 747.2 | 1872.3 | 0 | 0 |
| ranvier | ranvier | 51.7 | 163.5 | 5019.7 | 1.00 | 6.60 | 541.7 | 6514.0 | 0 | 0 |

## mid-signed-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 45.2 | 79.3 | 5024.9 | 1.00 | - | 750.5 | 2073.7 | 0 | 0 |
| ranvier | ranvier | 36.1 | 85.6 | 4972.4 | 1.00 | 5.30 | 424.7 | 2770.0 | 0 | 0 |

## small-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 43.7 | 119.3 | 7073.6 | 1.00 | - | 462.9 | 2051.9 | 0 | 0 |
| ranvier | ranvier | 38.6 | 63.9 | 6032.0 | 1.00 | 1.75 | 274.6 | 2800.5 | 0 | 0 |

## small-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 7222.9 | - | - | 424.1 | 1442.6 | 0 | 0 |
| ranvier | ranvier | - | - | 6091.2 | - | - | 240.4 | 1994.0 | 0 | 0 |

## small-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 8415.9 | - | - | 469.1 | 1787.4 | 0 | 0 |
| ranvier | ranvier | - | - | 9432.4 | - | - | 325.2 | 3781.4 | 0 | 0 |

## small-references

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 7021.9 | - | - | 638.6 | 3177.7 | 0 | 0 |
| ranvier | ranvier | - | - | 6114.0 | - | - | 337.2 | 3869.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/references |
| --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 28.2 |
| ranvier | ranvier | 5.5 |

## small-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | - | - | 7333.7 | - | - | 812.4 | 1979.0 | 0 | 0 |
| ranvier | ranvier | - | - | 6568.9 | - | - | 255.1 | 2152.1 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 5.6 | 0.6 | 13.0 |
| ranvier | ranvier | 7.0 | 6.0 | 15.4 |

## small-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core-tc | fsac-core-tc | 47.6 | 137.8 | 7117.3 | 1.00 | - | 463.3 | 1781.1 | 0 | 0 |
| ranvier | ranvier | 42.4 | 62.6 | 5983.9 | 1.00 | 2.50 | 384.6 | 9474.9 | 0 | 0 |
