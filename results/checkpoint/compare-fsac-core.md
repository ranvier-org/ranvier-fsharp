# Benchmark comparison

Runs: `checkpoint` and `fsac-core`. Latencies are nearest-rank percentiles over steps that did not fail.

## large-bigfile-narrow

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 312.5 | 648.4 | 6788.0 | 0.00 | 1.00 | 742.4 | 6465.7 | 0 | 0 |
| checkpoint | file-slim | 212.3 | 570.1 | 3191.7 | 0.00 | - | 420.6 | 4318.4 | 0 | 0 |
| fsac-core | fsac-core-bg | 225.4 | 292.7 | 9563.6 | 1.00 | - | 1313.2 | 9276.9 | 0 | 0 |
| fsac-core | fsac-core-tc | 84.5 | 129.8 | 5991.0 | 1.00 | - | 616.5 | 2676.3 | 0 | 0 |

## large-bigfile-wide

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 322.9 | 470.0 | 7258.0 | 0.00 | 1.00 | 742.2 | 6453.9 | 0 | 0 |
| checkpoint | file-slim | 216.2 | 626.8 | 3205.4 | 0.00 | - | 403.1 | 4322.6 | 0 | 0 |
| fsac-core | fsac-core-bg | 241.7 | 515.8 | 14907.4 | 1.00 | - | 1308.6 | 9260.9 | 0 | 0 |
| fsac-core | fsac-core-tc | 79.5 | 142.9 | 5839.5 | 1.00 | - | 615.9 | 2676.4 | 0 | 0 |

## large-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 30.2 | 47.7 | 7433.4 | 0.00 | 1.00 | 364.3 | 2420.9 | 0 | 0 |
| checkpoint | file-slim | 35.2 | 77.1 | 3340.5 | 0.00 | - | 306.5 | 1665.0 | 0 | 0 |
| fsac-core | fsac-core-bg | 22.5 | 31.2 | 10834.6 | 1.00 | - | 1083.3 | 5291.3 | 0 | 0 |
| fsac-core | fsac-core-tc | 42.4 | 84.9 | 6518.6 | 1.00 | - | 492.4 | 1720.8 | 0 | 0 |

## large-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 11750.3 | - | - | 1036.2 | 5128.8 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 11590.6 | - | - | 1035.3 | 5136.0 | 0 | 0 |

## large-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 39.2 | 63.4 | 6938.8 | 0.00 | 13.00 | 361.7 | 2362.0 | 0 | 0 |
| checkpoint | file-slim | 37.6 | 137.1 | 3412.8 | 0.00 | - | 303.5 | 1313.5 | 0 | 0 |

## large-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 21674.0 | - | - | 5145.1 | 37048.5 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 12063.3 | - | - | 1157.7 | 5622.2 | 0 | 0 |

## large-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 11432.1 | - | - | 1036.0 | 5148.2 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 11426.7 | - | - | 1128.8 | 5153.9 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | 6.9 | 0.5 | 4.1 |
| fsac-core | fsac-core-tc | 8.5 | 0.6 | 3.7 |

## large-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 863.6 | 2285.9 | 7032.1 | 0.00 | 223.00 | 670.0 | 8577.3 | 0 | 0 |
| checkpoint | file-slim | 319.9 | 765.9 | 3304.7 | 0.00 | - | 384.7 | 4211.3 | 0 | 0 |
| fsac-core | fsac-core-bg | 21.2 | 65.8 | 9739.9 | 1.00 | - | 1019.8 | 5206.5 | 0 | 0 |
| fsac-core | fsac-core-tc | 36.7 | 109.6 | 5979.3 | 1.00 | - | 465.9 | 1526.7 | 0 | 0 |

## mid-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 28.0 | 61.7 | 11094.2 | 0.00 | 1.00 | 453.9 | 3985.2 | 0 | 0 |
| checkpoint | file-slim | 81.9 | 308.6 | 3751.4 | 0.00 | - | 415.8 | 3223.5 | 0 | 0 |
| fsac-core | fsac-core-bg | 87.5 | 291.1 | 9015.9 | 1.00 | - | 762.7 | 2656.1 | 0 | 0 |
| fsac-core | fsac-core-tc | 74.9 | 261.0 | 6205.2 | 1.00 | - | 811.8 | 2266.0 | 0 | 0 |

## mid-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 8039.7 | - | - | 654.7 | 1578.0 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 7230.5 | - | - | 707.9 | 1558.4 | 0 | 0 |

## mid-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 73.8 | 186.4 | 10182.2 | 0.00 | 27.00 | 481.0 | 4387.2 | 0 | 0 |
| checkpoint | file-slim | 83.7 | 300.1 | 3980.8 | 0.00 | - | 376.1 | 2466.1 | 0 | 0 |

## mid-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 13395.5 | - | - | 3120.4 | 7499.7 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 6616.0 | - | - | 706.2 | 2017.9 | 0 | 0 |

## mid-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 7900.4 | - | - | 635.9 | 1777.9 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 7244.8 | - | - | 701.3 | 1744.7 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | 10.9 | 1.3 | 17.2 |
| fsac-core | fsac-core-tc | 9.8 | 0.8 | 13.4 |

## mid-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 1410.1 | 3600.9 | 10137.0 | 0.00 | 467.00 | 911.1 | 15497.3 | 0 | 0 |
| checkpoint | file-slim | 393.1 | 981.0 | 3877.8 | 0.00 | - | 490.6 | 6409.4 | 0 | 0 |
| fsac-core | fsac-core-bg | 85.5 | 251.5 | 6166.0 | 1.00 | - | 759.8 | 2114.2 | 0 | 0 |
| fsac-core | fsac-core-tc | 67.1 | 317.5 | 5041.1 | 1.00 | - | 747.2 | 1872.3 | 0 | 0 |

## small-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 29.1 | 35.2 | 16253.6 | 0.00 | 1.00 | 415.7 | 8354.0 | 0 | 0 |
| checkpoint | file-slim | 68.1 | 162.5 | 6325.2 | 0.00 | - | 285.8 | 3269.5 | 0 | 0 |
| fsac-core | fsac-core-bg | 28.0 | 53.2 | 7610.5 | 1.00 | - | 453.1 | 1936.3 | 0 | 0 |
| fsac-core | fsac-core-tc | 43.7 | 119.3 | 7073.6 | 1.00 | - | 462.9 | 2051.9 | 0 | 0 |

## small-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 7516.2 | - | - | 432.7 | 1585.2 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 7222.9 | - | - | 424.1 | 1442.6 | 0 | 0 |

## small-openmany

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 12650.8 | - | - | 1666.3 | 15574.6 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 8415.9 | - | - | 469.1 | 1787.4 | 0 | 0 |

## small-requests

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | - | - | 7158.8 | - | - | 812.3 | 2133.3 | 0 | 0 |
| fsac-core | fsac-core-tc | - | - | 7333.7 | - | - | 812.4 | 1979.0 | 0 | 0 |

Request p50 ms

| Run | Server | textDocument/completion | textDocument/definition | textDocument/hover |
| --- | --- | --- | --- | --- |
| fsac-core | fsac-core-bg | 5.9 | 0.7 | 14.9 |
| fsac-core | fsac-core-tc | 5.6 | 0.6 | 13.0 |

## small-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| checkpoint | decl | 3127.2 | 4009.6 | 16578.4 | 0.00 | 590.00 | 1009.9 | 39234.8 | 0 | 0 |
| checkpoint | file-slim | 540.6 | 1500.1 | 6342.7 | 0.00 | - | 357.4 | 9361.7 | 0 | 0 |
| fsac-core | fsac-core-bg | 29.3 | 92.7 | 7479.8 | 1.00 | - | 453.9 | 1759.4 | 0 | 0 |
| fsac-core | fsac-core-tc | 47.6 | 137.8 | 7117.3 | 1.00 | - | 463.3 | 1781.1 | 0 | 0 |
