module Rfs.Checker.Tests.ShiftGraphTests

open System
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Checker.Tests.Deps

let private a = fileOf "A.fs"
let private d = fileOf "D.fs"
let private e = fileOf "E.fs"

/// A file-level graph over Deps with `files` promoted and checked by declaration
let private withPromoted (files: string list) (body: Session -> Task) =
    task {
        let! s = start ()
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }

        for f in files do
            s.Graph.Promote f

        for f in files do
            let! mode = s.Graph.WhenPromoted(f, CancellationToken.None) |> settle
            Expect.equal mode Declarations $"%s{IO.Path.GetFileName f} is promoted"

        do! body s
    }

/// The edit, with how many results it shifted
let private shifting (s: Session) (file: string) (text: string) =
    task {
        let before = s.Graph.ShiftedRows
        let! r = edit s file text
        return r, s.Graph.ShiftedRows - before
    }

let private blankAbove (anchor: string) (text: string) =
    if not (text.Contains anchor) then
        failtest $"no %s{anchor}"

    text.Replace(anchor, "\n" + anchor)

[<Tests>]
let tests =
    testSequenced
    <| testList
        "ShiftGraph"
        [ testTask "blank line re-checks nothing" {
              do!
                  withPromoted [ a ] (fun s ->
                      task {
                          let! r, shifted = shifting s a (blankAbove "let area" s.Texts[a])
                          Expect.equal r.Delta 0L "no CheckDeclaration adopted"
                          Expect.isEmpty r.Ran "none ran"
                          Expect.equal shifted 3L "area, doubled and grouped, the rows below the blank line, are shifted"
                          do! expectOracle s r.Diagnostics "after the blank line"

                          let! r, shifted = shifting s a (s.Texts[a].Replace("\nlet area", "let area"))
                          Expect.equal r.Delta 0L "taking it out again adopts no check"
                          Expect.equal shifted 3L "and shifts the same rows back"
                          do! expectOracle s r.Diagnostics "after the blank line is taken out"
                      })
          }

          testTask "line inside a declaration re-checks only it" {
              do!
                  withPromoted [ a ] (fun s ->
                      task {
                          let! r, shifted = shifting s a (s.Texts[a].Replace("    match s with", "\n    match s with"))
                          Expect.equal r.Delta 1L "one CheckDeclaration adopted"
                          Expect.equal (r.NamesIn a) [ "area" ] "the declaration the line is in"
                          Expect.equal shifted 2L "doubled and grouped are shifted"
                          do! expectOracle s r.Diagnostics "after the line inside area"
                      })
          }

          // D.fs's `r = ref []` depends on later declarations (R12): the rows from it on are one segment
          testTask "a segment's members are shifted" {
              do!
                  withPromoted [ d ] (fun s ->
                      task {
                          let! segmentRows = Task.Run(fun () -> s.Graph.SegmentRows d)
                          Expect.isGreaterThan segmentRows 0 "D.fs has a segment"
                          let! r, shifted = shifting s d (blankAbove "let fill" s.Texts[d])
                          Expect.equal r.Delta 0L "no CheckDeclaration adopted"
                          Expect.equal shifted 3L "fill, size and after are shifted"
                          do! expectOracle s r.Diagnostics "after the blank line in the segment"
                      })
          }

          testTask "a refused shift checks the row again where it was checked" {
              do!
                  withPromoted [ a; d ] (fun s ->
                      task {
                          s.Graph.Context.RefuseShifts <- true
                          let! r, shifted = shifting s a (blankAbove "let area" s.Texts[a])
                          Expect.equal shifted 0L "nothing is shifted"
                          Expect.equal r.Delta 3L "area, doubled and grouped are checked again"
                          do! expectOracle s r.Diagnostics "after the blank line in A.fs"
                          let! r, shifted = shifting s d (blankAbove "let fill" s.Texts[d])
                          Expect.equal shifted 0L "nothing is shifted in the segment"
                          Expect.equal r.Delta 4L "the segment, r to after, is checked again"
                          do! expectOracle s r.Diagnostics "after the blank line in D.fs"
                      })
          }

          testTask "a signed file's rows are shifted" {
              do!
                  withPromoted [ e ] (fun s ->
                      task {
                          let! r, shifted = shifting s e (blankAbove "let compute" s.Texts[e])
                          Expect.equal r.Delta 0L "no CheckDeclaration adopted"
                          Expect.equal shifted 3L "compute, other and unrelated are shifted"
                          do! expectOracle s r.Diagnostics "after the blank line in E.fs"
                      })
          } ]
