namespace Rfs.Server

open System.Runtime.ExceptionServices
open System.Threading
open Rfs.Checker

module internal StackGuard =
    /// Runs `f` on a thread of its own with the graph thread's stack: reading a parse tree has no StackGuard, and a deep
    /// syntax tree overflows a pool thread's stack, which kills the process.
    let onLargeStack (f: unit -> 'T) : 'T =
        let result = ref ValueNone
        let failure = ref null

        let thread =
            Thread(
                (fun () ->
                    try
                        result.Value <- ValueSome(f ())
                    with ex ->
                        failure.Value <- ExceptionDispatchInfo.Capture ex),
                GraphHost.StackSize,
                IsBackground = true
            )

        thread.Start()
        thread.Join()

        if not (isNull failure.Value) then
            failure.Value.Throw()

        result.Value.Value
