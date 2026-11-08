# Benchmark comparison

Runs: `baseline-untraced` and `final`. Latencies are nearest-rank percentiles over steps that did not fail.

## huge-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | - | - | 23140.5 | - | - | 1251.5 | 16990.5 | 0 | 0 |
| final | file-slim | - | - | 20110.0 | - | - | 1208.6 | 16109.6 | 0 | 0 |

## huge-cx-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 868.1 | 1729.3 | 23260.3 | 0.00 | 0.00 | 1845.9 | 23945.0 | 0 | 0 |
| final | file-slim | 1259.7 | 1914.9 | 21152.1 | 0.00 | - | 1796.9 | 27229.2 | 0 | 0 |

## huge-cx-rec

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 1400.4 | 2189.8 | 22931.1 | 0.00 | 1.00 | 2062.1 | 27119.8 | 0 | 0 |
| final | file-slim | 1223.9 | 1910.2 | 25046.4 | 0.00 | - | 1857.5 | 27141.0 | 0 | 0 |

## huge-cx-top

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 625.7 | 1409.6 | 22086.3 | 0.00 | 1.00 | 1820.3 | 23260.0 | 0 | 0 |
| final | file-slim | 1274.0 | 2094.6 | 21431.8 | 0.00 | - | 1755.7 | 27178.5 | 0 | 0 |

## huge-il-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 1148.2 | 1975.9 | 23837.7 | 0.00 | 476.00 | 1837.2 | 27559.9 | 0 | 0 |
| final | file-slim | 892.5 | 1602.8 | 20793.4 | 0.00 | - | 1751.5 | 25365.5 | 0 | 0 |

## huge-il-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 676.1 | 1417.1 | 22303.2 | 0.00 | 0.00 | 1775.0 | 23497.9 | 0 | 0 |
| final | file-slim | 806.0 | 1464.1 | 21787.5 | 0.00 | - | 1716.0 | 25368.4 | 0 | 0 |

## large-bigfile-narrow

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 185.5 | 438.5 | 3018.8 | 0.00 | 0.00 | 392.7 | 4282.9 | 0 | 0 |
| final | file-slim | 187.9 | 478.8 | 2779.6 | 0.00 | - | 411.0 | 4311.7 | 0 | 0 |

## large-bigfile-wide

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 180.4 | 444.9 | 2982.5 | 0.00 | 0.00 | 394.9 | 4286.0 | 0 | 0 |
| final | file-slim | 185.1 | 523.9 | 2955.1 | 0.00 | - | 404.8 | 4316.4 | 0 | 0 |

## large-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline-untraced | decl | 30.3 | 72.0 | 3749.3 | 0.00 | 1.00 | 317.7 | 1555.3 | 0 | 0 |
| final | decl | 30.8 | 57.0 | 2953.7 | 0.00 | 1.00 | 318.5 | 1547.7 | 0 | 0 |
| baseline-untraced | file-slim | 31.9 | 76.5 | 3453.5 | 0.00 | - | 310.6 | 1579.1 | 0 | 0 |
| final | file-slim | 28.0 | 71.5 | 2954.6 | 0.00 | - | 315.9 | 1576.4 | 0 | 0 |

## large-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | - | - | 3108.5 | - | - | 285.8 | 992.2 | 0 | 0 |
| final | file-slim | - | - | 3132.5 | - | - | 286.6 | 955.4 | 0 | 0 |

## large-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 35.7 | 75.3 | 2814.1 | 0.00 | 12.00 | 317.7 | 1363.5 | 0 | 0 |
| final | file-slim | 28.0 | 93.5 | 2690.6 | 0.00 | - | 316.7 | 1267.3 | 0 | 0 |

## large-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline-untraced | decl | 249.0 | 927.7 | 3063.9 | 0.00 | 1.00 | 356.2 | 4104.7 | 0 | 0 |
| final | decl | 241.8 | 728.2 | 2892.7 | 0.00 | 1.00 | 377.8 | 4102.1 | 0 | 0 |
| baseline-untraced | file-slim | 265.3 | 756.8 | 3378.3 | 0.00 | - | 387.5 | 4117.7 | 0 | 0 |
| final | file-slim | 272.3 | 769.5 | 3415.2 | 0.00 | - | 381.8 | 4109.6 | 0 | 0 |

## mid-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline-untraced | decl | 35.1 | 99.2 | 4469.3 | 0.00 | 1.00 | 380.0 | 2430.6 | 0 | 0 |
| final | decl | 31.4 | 82.5 | 4243.4 | 0.00 | 1.00 | 388.8 | 2429.4 | 0 | 0 |
| baseline-untraced | file-slim | 85.0 | 262.3 | 4503.4 | 0.00 | - | 418.5 | 3141.1 | 0 | 0 |
| final | file-slim | 73.7 | 224.6 | 4112.6 | 0.00 | - | 434.9 | 3143.8 | 0 | 0 |

## mid-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | - | - | 4094.6 | - | - | 350.8 | 1735.6 | 0 | 0 |
| final | file-slim | - | - | 4019.3 | - | - | 338.7 | 1699.5 | 0 | 0 |

## mid-lineinsert

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 37.1 | 108.3 | 3933.3 | 0.00 | 0.00 | 383.0 | 2124.4 | 0 | 0 |
| final | file-slim | 71.0 | 230.8 | 4002.8 | 0.00 | - | 385.6 | 2426.1 | 0 | 0 |

## mid-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline-untraced | decl | 325.9 | 911.8 | 4293.6 | 0.00 | 3.00 | 496.8 | 6192.0 | 0 | 0 |
| final | decl | 331.2 | 907.9 | 4370.9 | 0.00 | 3.00 | 509.2 | 6138.3 | 0 | 0 |
| baseline-untraced | file-slim | 357.8 | 951.5 | 4151.1 | 0.00 | - | 485.1 | 6336.2 | 0 | 0 |
| final | file-slim | 406.5 | 1361.4 | 4837.1 | 0.00 | - | 489.8 | 6312.7 | 0 | 0 |

## mid-signed-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | 28.5 | 83.6 | 4353.3 | 0.00 | 1.00 | 379.9 | 2391.7 | 0 | 0 |
| final | file-slim | 45.6 | 139.8 | 4100.1 | 0.00 | - | 371.9 | 2735.1 | 0 | 0 |

## small-body

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline-untraced | decl | 34.9 | 54.0 | 7604.3 | 0.00 | 1.00 | 259.6 | 2723.5 | 0 | 0 |
| final | decl | 31.8 | 44.8 | 6353.4 | 0.00 | 1.00 | 260.4 | 2719.8 | 0 | 0 |
| baseline-untraced | file-slim | 56.3 | 74.9 | 7786.2 | 0.00 | - | 282.6 | 3099.4 | 0 | 0 |
| final | file-slim | 51.3 | 121.8 | 5952.7 | 0.00 | - | 283.7 | 3098.4 | 0 | 0 |

## small-cold

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| final | decl | - | - | 6241.1 | - | - | 209.0 | 1895.5 | 0 | 0 |
| final | file-slim | - | - | 5694.2 | - | - | 224.5 | 1831.6 | 0 | 0 |

## small-signature

| Run | Server | Edit p50 ms | Edit p95 ms | Cold ms | Publishes/edit | Decls/edit | Peak MB | Allocated MB | Oracle diffs | Failed steps |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| baseline-untraced | decl | 525.3 | 1373.8 | 5980.0 | 0.00 | 1.00 | 307.4 | 9371.5 | 0 | 0 |
| final | decl | 614.9 | 1014.6 | 8414.1 | 0.00 | 1.00 | 317.9 | 9315.9 | 0 | 0 |
| baseline-untraced | file-slim | 522.9 | 1403.4 | 6147.9 | 0.00 | - | 357.1 | 9188.8 | 0 | 0 |
| final | file-slim | 620.2 | 1528.7 | 7059.9 | 0.00 | - | 355.9 | 9190.1 | 0 | 0 |
