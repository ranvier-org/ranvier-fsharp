module Rfs.Agent.Tests.PositionResolverTests

open System
open Expecto
open Rfs.Agent

[<Tests>]
let tests =
    testList "PositionResolver & Cli" [
        test "splitFileLineCol handles Windows drive letters and optional col" {
            let f1, l1, c1 = Cli.splitFileLineCol @"C:\repo\src\Foo.fs:42:7"
            Expect.equal f1 @"C:\repo\src\Foo.fs" "file with line and col"
            Expect.equal l1 (Some 42) "line"
            Expect.equal c1 (Some 7) "col"

            let f2, l2, c2 = Cli.splitFileLineCol @"D:\a\Bar.fs:15"
            Expect.equal f2 @"D:\a\Bar.fs" "file with line only"
            Expect.equal l2 (Some 15) "line only"
            Expect.isNone c2 "no col"

            let f3, l3, c3 = Cli.splitFileLineCol @"C:\repo\NoLine.fs"
            Expect.equal f3 @"C:\repo\NoLine.fs" "bare Windows path"
            Expect.isNone l3 "no line"
            Expect.isNone c3 "no col on bare path"
        }

        test "resolve finds exact symbol on line and handles line drift" {
            let text =
                String.Join(
                    "\n",
                    [ "module Sample"
                      ""
                      "let addTwoNumbers (alpha: int) (beta: int) ="
                      "    let sum = alpha + beta"
                      "    sum" ]
                )

            match PositionResolver.resolve text 3 None (Some "addTwoNumbers") with
            | Error e -> failtest $"Expected exact hit, got: {e}"
            | Ok r ->
                Expect.equal r.Line1 3 "1-based line"
                Expect.equal r.Names [ "addTwoNumbers" ] "resolved symbol"

            // Two lines off from target line (line 5 asks for alpha defined/used on line 4)
            match PositionResolver.resolve text 5 None (Some "beta") with
            | Error e -> failtest $"Expected drift match, got: {e}"
            | Ok r ->
                Expect.equal r.Line1 4 "found beta on line 4"
                Expect.equal r.Names [ "beta" ] "beta identifier"
        }

        test "resolve falls back cleanly to column or primary binding" {
            let text = "module M\nlet targetValue = 123\n"

            match PositionResolver.resolve text 2 (Some 5) None with
            | Error e -> failtest $"Expected col resolution, got: {e}"
            | Ok r -> Expect.equal r.Names [ "targetValue" ] "col 5 is targetValue"

            match PositionResolver.resolve text 2 None None with
            | Error e -> failtest $"Expected default symbol resolution, got: {e}"
            | Ok r -> Expect.equal r.Names [ "targetValue" ] "skips modifier 'let' to pick targetValue"

            match PositionResolver.resolve text 2 None (Some "nonExistent") with
            | Ok r -> failtest $"Expected error, resolved to {r.Names}"
            | Error msg ->
                Expect.stringContains msg "not found" "actionable error"
                Expect.stringContains msg "targetValue" "nearby symbol hint"
        }
    ]
