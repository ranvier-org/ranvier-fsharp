module Rfs.Checker.Tests.ShiftGraphTests

open System
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Declarations
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

let private bagFile =
    """namespace Deps

/// A bag of items
/// kept in arrival order
type Bag<'T>(items: 'T list, count: int) =
    let mutable cached = None

    override x.GetHashCode() =
        match cached with
        | Some h -> h
        | None ->
            let h = List.fold (fun acc i -> 31 * acc + Unchecked.hash i) 1 items
            cached <- Some h
            h

    override x.Equals(other) =
        match other with
        | :? Bag<'T> as y -> x.Count = y.Count && List.forall2 Unchecked.equals x.Items y.Items
        | _ -> false

    member x.Items = items
    member x.Count = count
    member x.Head = List.head items
    member x.Add i = Bag(i :: items, count + 1)

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member x.Count = count
        member x.GetEnumerator() = (Seq.ofList items).GetEnumerator()
        member x.GetEnumerator() = (Seq.ofList items :> System.Collections.IEnumerable).GetEnumerator()

[<RequireQualifiedAccess>]
module Bag =
    let empty<'T> : Bag<'T> = Bag<_>([], 0)

    let inline head (b: Bag<'T>) = b.Head

    let inline count (b: Bag<'T>) = b.Count

    let ofList xs = Bag<'T>(xs, List.length xs)

    let inline toSeq (b: Bag<'T>) = b :> 'T seq

    let inline add (b: Bag<'T>) x = b.Add x

    let isEmpty (b: Bag<'T>) = b.Count = 0

    let inline items (b: Bag<'T>) = b.Items
"""

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

          testTask "a blank line under the header keeps the setup and shifts every row" {
              do!
                  withPromoted [ a ] (fun s ->
                      task {
                          let rows = (DeclarationChecker.Split(DeclarationChecker.Parse(s.Fresh, a, s.Texts[a]))).Length
                          let text = s.Texts[a]
                          let afterHeader = text.IndexOf('\n') + 1
                          let! r, shifted = shifting s a (text.Insert(afterHeader, "\n"))
                          Expect.equal r.Delta 0L "no CheckDeclaration adopted: the file is not set up again"
                          Expect.equal shifted (int64 rows) "every row is shifted"
                          do! expectOracle s r.Diagnostics "after the blank line under the header"
                      })
          }

          // large-lineinsert's shape: a line inside a documented generic type, which a module of the same name's
          // inline functions use. Their checks run again while their shifts are in flight, and a superseded flight's
          // shift must not move what the result it was shifted to already moved.
          testTask "a line inside a type re-checks only it, while the rows using it are shifted" {
              let! s = start ()
              use _ = (s :> IAsyncDisposable)
              let! _ = edit s a bagFile
              s.Graph.Promote a
              let! _ = s.Graph.WhenPromoted(a, CancellationToken.None) |> settle

              for round in 1..3 do
                  let! (r: Edit), shifted = shifting s a (bagFile.Replace("type Bag<'T>(items", "\ntype Bag<'T>(items"))
                  Expect.equal (r.NamesIn a) [ "Bag" ] $"round %d{round}: only the type is checked again"
                  Expect.equal shifted 9L $"round %d{round}: the module's header and its eight values are shifted"
                  do! expectOracle s r.Diagnostics $"round %d{round}: after the line inside Bag"
                  let! (r: Edit), shifted = shifting s a bagFile
                  Expect.equal (r.NamesIn a) [ "Bag" ] $"round %d{round}: the type is checked again once the line goes"
                  Expect.equal shifted 9L $"round %d{round}: and the rest shifted back"
                  do! expectOracle s r.Diagnostics $"round %d{round}: after the line goes"
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
