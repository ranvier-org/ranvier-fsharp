module Rfs.Harness.Tests.OracleTests

open System
open System.IO
open Expecto
open Rfs.Lsp
open Rfs.Harness

let private tinyDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures", "Tiny")
        if Directory.Exists candidate then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let private tinyProject = Path.Combine(tinyDir, "Tiny.fsproj")

let private diagnostic line : Diagnostic =
    { File = "F"
      Range =
        { Start = { Line = line; Character = 0 }
          End = { Line = line; Character = 1 } }
      Severity = 1
      Code = "FS0001"
      Message = "m"
      Source = "a" }

[<Tests>]
let tests =
    testList
        "Oracle"
        [ test "compare reports missing and extra" {
              Expect.isEmpty (Diagnostics.compare [ diagnostic 1 ] [ { diagnostic 1 with Source = "b" } ]) "same, source ignored"

              let diff = Diagnostics.compare [ diagnostic 1 ] [ diagnostic 2 ]
              Expect.equal diff.Length 2 "one missing, one unexpected"
              Expect.isTrue (diff |> List.exists (fun l -> l.StartsWith "missing: ")) "missing"
              Expect.isTrue (diff |> List.exists (fun l -> l.StartsWith "unexpected: ")) "unexpected"
          }

          test "compare counts duplicates and ignores non-compiler diagnostics" {
              Expect.equal (Diagnostics.compare [ diagnostic 1; diagnostic 1 ] [ diagnostic 1 ]).Length 1 "one missing"

              let lint = { diagnostic 1 with Code = "FL0001"; Source = "FSharpLint" }
              Expect.isEmpty (Diagnostics.compare [] [ lint ]) "linter output ignored"
              Expect.isEmpty (Diagnostics.compare [ lint ] []) "linter output ignored"
          }

          test "compare treats paths that differ in drive-letter case as equal" {
              let a = { diagnostic 1 with File = @"c:\x\A.fs" }
              let b = { diagnostic 1 with File = @"C:\x\A.fs" }
              Expect.isEmpty (Diagnostics.compare [ a ] [ b ]) "same file"
          }

          test "diagnostic JSON round-trips" {
              let d = { diagnostic 3 with Message = "two\nlines \"quoted\"" }
              Expect.equal (Diagnostics.ofJson (Diagnostics.toJson d)) d "round trip"
          }

          testTask "Tiny has no errors" {
              use! (oracle: OracleClient) = OracleClient.start ()
              let! diagnostics = oracle.Check tinyProject Map.empty
              Expect.isEmpty diagnostics "no diagnostics"
          }

          testTask "override introduces FS0001" {
              use! (oracle: OracleClient) = OracleClient.start ()
              let c = Path.Combine(tinyDir, "C.fs")
              let! diagnostics = oracle.Check tinyProject (Map [ c, "module Tiny.C\n\nlet x: int = \"\"\n" ])

              match diagnostics with
              | [ d ] ->
                  Expect.equal d.Code "FS0001" "code"
                  Expect.equal d.Severity 1 "error"
                  Expect.equal d.Source "F# Compiler" "source"
                  Expect.equal d.File (LspClient.NormalizePath c) "file"

                  Expect.equal
                      d.Range
                      { Start = { Line = 2; Character = 13 }
                        End = { Line = 2; Character = 15 } }
                      "range on the empty string literal"
              | other -> failtestf "expected one diagnostic, got %A" other

              // what FSAC sends for the same diagnostic: integer code, platform newlines
              let fsacShaped =
                  diagnostics
                  |> List.map (fun d ->
                      { d with
                          Code = "1"
                          Message = d.Message.Replace("\u001d", "\r\n") })

              Expect.isEmpty (Diagnostics.compare diagnostics fsacShaped) "FSAC's spelling pairs equal"

              // the oracle checks from scratch: dropping the override gives the clean project again
              let! clean = oracle.Check tinyProject Map.empty
              Expect.isEmpty clean "no leftover state"
          }

          testTask "CRLF text gives the same range as LF" {
              use! (oracle: OracleClient) = OracleClient.start ()
              let c = Path.Combine(tinyDir, "C.fs")
              let! diagnostics = oracle.Check tinyProject (Map [ c, "module Tiny.C\r\n\r\nlet x: int = \"\"\r\n" ])

              match diagnostics with
              | [ d ] ->
                  Expect.equal d.Code "FS0001" "code"

                  Expect.equal
                      d.Range
                      { Start = { Line = 2; Character = 13 }
                        End = { Line = 2; Character = 15 } }
                      "same range as with LF"
              | other -> failtestf "expected one diagnostic, got %A" other
          }

          test "an FSAC-shaped diagnostic pairs with the oracle's" {
              let oracle =
                  { diagnostic 3 with
                      Severity = 4
                      Message = "first\u001dsecond"
                      Source = "F# Compiler" }

              let fsac =
                  { oracle with
                      Severity = 0
                      Code = "1"
                      Message = "first\r\nsecond" }

              Expect.isEmpty (Diagnostics.compare [ oracle ] [ fsac ]) "hidden severity, integer code and newline normalised"
              Expect.isNonEmpty (Diagnostics.compare [ oracle ] [ { fsac with Code = "2" } ]) "a different number still differs"
          }

          testTask "an override of a signature file changes dependents" {
              use! (oracle: OracleClient) = OracleClient.start ()
              let bsig = Path.Combine(tinyDir, "B.fsi")
              let! diagnostics = oracle.Check tinyProject (Map [ bsig, "module Tiny.B\n\nval two: string\n" ])
              let b = LspClient.NormalizePath(Path.Combine(tinyDir, "B.fs"))
              let inB = diagnostics |> List.filter (fun d -> d.File = b)
              Expect.isNonEmpty inB (sprintf "B.fs no longer matches its signature: %A" diagnostics)
              Expect.equal inB.Head.Code "FS0034" (sprintf "%A" diagnostics)
              Expect.equal inB.Head.Severity 1 "error"
          }

          // In graph mode C.fs would not see A.fs (the path does not resolve), and the error would name Tiny instead
          testTask "the oracle checks sequentially" {
              use! (oracle: OracleClient) = OracleClient.start ()
              let c = Path.Combine(tinyDir, "C.fs")
              let! diagnostics = oracle.Check tinyProject (Map [ c, "module Tiny.C\n\nopen Tiny.A.Nope\n" ])
              let messages = diagnostics |> List.map (fun (d: Diagnostic) -> d.Message)
              Expect.contains messages "The namespace 'Nope' is not defined." (sprintf "%A" diagnostics)
          }

          testTask "a project that cannot be loaded is an error and the oracle survives" {
              use! (oracle: OracleClient) = OracleClient.start ()

              let! failed =
                  task {
                      try
                          let! _ = oracle.Check (Path.Combine(tinyDir, "Missing.fsproj")) Map.empty
                          return false
                      with _ ->
                          return true
                  }

              Expect.isTrue failed "check failed"
              let! diagnostics = oracle.Check tinyProject Map.empty
              Expect.isEmpty diagnostics "still serving"
          } ]
