# t7 was measured on a traced Ranvier

Every run in this folder, `variance/` included, used a Debug `Ranvier.dll` with `RANVIER_TRACE` on, because a solution
build gave the out-of-solution Ranvier project its default configuration.

Evidence: a heap dump of a run held trace records, and R's Debug `Ranvier.dll` was 975,360 bytes against 537,088 for
the untraced one.

Do not compare these numbers for time, allocations or peak memory. Use `../t7-fix2` and `../baseline-untraced`.
Scenario outcomes that do not depend on tracing, such as the oracle diffs, still describe the code they ran on.
