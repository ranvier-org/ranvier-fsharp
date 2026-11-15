module Rfs.Agent.Tests.CheckReportTests

open Expecto
open Rfs.Agent

/// A detailed MSBuild log in the shape Ionide.ProjInfo reports it: no error code, the message between a task's
/// opening and its failure.
let private detailedLog =
    String.concat
        " "
        [ "Ionide.ProjInfo returned no project for C:\\ws\\App.fsproj: Loading \"C:\\ws\\App.fsproj\"; Failed (\"C:\\ws\\App.fsproj\","
          "GenericError (\"C:\\ws\\App.fsproj\", \"Build started. Enabled loggers: stringWriterLogger"
          String.replicate 400 "Target \"Noise\" in file \"Microsoft.Common.targets\": Done building target \"Noise\"."
          "Task \"Message\" Hello Done executing task \"Message\"."
          "Using \"NETSdkError\" task from assembly \"Microsoft.NET.Build.Tasks.dll\"."
          "Task \"NETSdkError\" The current .NET SDK does not support targeting .NET 99.0. Either target .NET 10.0 or lower, or use a version of the .NET SDK that supports .NET 99.0."
          "Done executing task \"NETSdkError\" -- FAILED. Done building target \"_CheckForUnsupportedNETCoreVersion\" -- FAILED. Build FAILED. \"))" ]

let private consoleLog =
    "dotnet restore failed for C:\\ws\\App.fsproj:\n  Determining projects to restore...\n"
    + "C:\\sdk\\Microsoft.NET.TargetFrameworkInference.targets(166,5): error NETSDK1045: The current .NET SDK does not support targeting .NET 99.0.  Either target .NET 10.0 or lower. [C:\\ws\\App.fsproj]\n"

[<Tests>]
let tests =
    testList "Check report" [
        test "a detailed log is reduced to the failed task's message" {
            let summary = LoadFailure.summarize detailedLog
            Expect.stringStarts summary "The current .NET SDK does not support targeting .NET 99.0." "the cause comes first"
            Expect.isFalse (summary.Contains "Build started") "the log around the cause is dropped"
            Expect.isLessThanOrEqual summary.Length 400 "bounded length"
        }

        test "a console log is reduced to its first coded error" {
            let summary = LoadFailure.summarize consoleLog

            Expect.equal
                summary
                "error NETSDK1045: The current .NET SDK does not support targeting .NET 99.0. Either target .NET 10.0 or lower."
                "code and message, without the path before or the project after"
        }

        test "a reason without a recognised error is clipped" {
            let summary = LoadFailure.summarize (String.replicate 100 "nothing recognisable ")
            Expect.equal summary.Length 400 "clipped to the limit"
            Expect.stringEnds summary "..." "marked as clipped"
        }

        test "a short reason is kept whole" {
            Expect.equal (LoadFailure.summarize "  project file is\n  empty ") "project file is empty" "whitespace collapsed"
        }

        test "the summary of a complete check starts with the error count" {
            let line = CheckSummary.line 0 2 3 19 7L 0L 0L 0

            Expect.equal
                line
                "0 errors, 2 warnings (checked 3 project(s), 19 file(s) in 7ms; rechecked 0 declaration(s) and 0 file(s), the rest unchanged)"
                "complete summary"
        }

        test "the summary of an incomplete check cannot be read as a pass" {
            let line = CheckSummary.line 0 0 0 0 0L 0L 0L 1
            Expect.stringStarts line "incomplete: 1 project(s) failed to load; 0 errors, 0 warnings in the rest" "leads with the failure"
        }

        test "the exit code is 1 for a load failure, and for errors only under --fail-on-error" {
            Expect.equal (CheckSummary.exitCode false 0 0) 0 "clean"
            Expect.equal (CheckSummary.exitCode false 3 0) 0 "errors are output, not failure, by default"
            Expect.equal (CheckSummary.exitCode true 3 0) 1 "errors fail under the flag"
            Expect.equal (CheckSummary.exitCode true 0 0) 0 "clean under the flag"
            Expect.equal (CheckSummary.exitCode false 0 1) 1 "a load failure fails without the flag"
            Expect.equal (CheckSummary.exitCode true 0 1) 1 "and with it"
        }
    ]

[<Tests>]
let runtimeTests =
    let installed (versions: string list) = fun () -> versions
    let latestMajor = [ "DOTNET_ROLL_FORWARD", "LatestMajor" ]

    testList "Daemon runtime" [
        test "a workspace on a newer SDK rolls the daemon forward when that runtime is installed" {
            let env = DaemonRuntime.rollForward 10 (Some "11.0.100") (installed [ "10.0.8"; "11.0.0" ]) None
            Expect.equal env latestMajor "newest installed runtime"
        }

        test "a workspace on the daemon's own SDK major stays on this runtime" {
            Expect.isEmpty (DaemonRuntime.rollForward 10 (Some "10.0.404") (installed [ "10.0.8"; "11.0.0" ]) None) "same major"
            Expect.isEmpty (DaemonRuntime.rollForward 10 (Some "9.0.300") (installed [ "9.0.1"; "10.0.8" ]) None) "older major"
        }

        test "no roll forward without a usable runtime or a known SDK" {
            Expect.isEmpty (DaemonRuntime.rollForward 10 (Some "11.0.100") (installed [ "10.0.8" ]) None) "runtime 11 missing"
            Expect.isEmpty (DaemonRuntime.rollForward 10 (Some "12.0.100-preview.1") (installed [ "10.0.8"; "11.0.0" ]) None) "runtime 12 missing"
            Expect.isEmpty (DaemonRuntime.rollForward 10 None (installed [ "10.0.8"; "11.0.0" ]) None) "no SDK selected"
            Expect.isEmpty (DaemonRuntime.rollForward 10 (Some "not a version") (installed [ "11.0.0" ]) None) "unreadable version"
        }

        test "the user's DOTNET_ROLL_FORWARD decides when it is set" {
            Expect.isEmpty (DaemonRuntime.rollForward 10 (Some "11.0.100") (installed [ "11.0.0" ]) (Some "Disable")) "left alone"
        }

        test "a prerelease runtime is allowed when it is the only one that fits" {
            let env = DaemonRuntime.rollForward 10 (Some "11.0.100-rc.1") (installed [ "10.0.8"; "11.0.0-rc.1.25451.107" ]) None
            Expect.equal env (latestMajor @ [ "DOTNET_ROLL_FORWARD_TO_PRERELEASE", "1" ]) "prerelease opt-in"
        }
    ]
