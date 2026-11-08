# The `TRACED BUILD` line in these run-logs is false

The run-logs here print `TRACED BUILD`, but these runs used an untraced `Ranvier.dll` (537,088 bytes). The guard that
printed the line was wrong: it was corrected afterwards, and later runs print `ranvier: untraced` in the header.

The numbers in this folder are untraced and stand as measured. They come from the code at that round, so for the final
tree use `../t7-fix2` and `../baseline-untraced`.
