module Rfs.Harness.Tests.MetricsTests

open System
open Expecto
open Rfs.Harness

[<Tests>]
let tests =
    testList
        "Metrics"
        [ test "collector sees allocations and gen-2 GCs of its own process" {
              let collector = MetricsCollector.Start Environment.ProcessId
              // keep the arrays reachable so the allocations are not elided
              let kept = [| for _ in 1..200 -> Array.zeroCreate<byte> 262144 |]
              GC.Collect(2, GCCollectionMode.Forced, true)
              Threading.Thread.Sleep 250
              let snapshot = collector.Stop()
              GC.KeepAlive kept

              Expect.isGreaterThan snapshot.AllocatedMb 0. "allocated"
              Expect.isGreaterThanOrEqual snapshot.Gen2 1 "gen2"
              Expect.isGreaterThan snapshot.PeakWorkingSetMb 0. "peak working set"
          } ]
