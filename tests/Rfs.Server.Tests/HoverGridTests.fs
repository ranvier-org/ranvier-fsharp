module Rfs.Server.Tests.HoverGridTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Server
open Rfs.Server.Tests.TokenGrid

let private normalizedText (t: string) = t.Replace("\r\n", "\n")

/// A value, a function with a parameter's doc, a union case and its field, a record field, types, an `<inheritdoc/>` on a
/// type and an override, overloads, and a method with a named argument: each with a `///` that the steps edit.
let private libA =
    normalizedText <|
        """module Lib.A

/// <summary>The base value</summary>
/// <param name="x">unused</param>
let baseValue = 10

/// Scales a number
/// <param name="factor">how much</param>
let scale (factor: int) (x: int) = x * factor

/// A shape
type Shape =
    /// A circle
    | Circle of
        /// its radius
        radius: float
    /// A square
    | Square of side: float

/// A point
type Point =
    { /// across
      X: int
      /// down
      Y: int }

/// The base
type Base() =
    /// Does it
    abstract Run: x: int -> int
    default _.Run x = x

/// <inheritdoc/>
type Derived() =
    inherit Base()
    /// <inheritdoc/>
    override _.Run x = x + 1

/// A calculator
type Calc() =
    /// The changes
    let changed = Event<int>()
    /// Fires on change
    [<CLIEvent>]
    member _.Changed = changed.Publish
    /// Adds an int
    member _.Add(a: int, b: int) = a + b
    /// Adds a float
    member _.Add(a: float, b: float) = a + b
    /// Says hello
    member _.Greet(name: string) = "hi " + name
    member _.Fire() = changed.Trigger 1

/// A plain box
type Box() =
    /// plain Get
    member _.Get() = 1

/// A generic box
type Box<'T>() =
    /// generic Get
    member _.Get() = 2

let here = scale 2 baseValue + Calc().Add(1, 2) + (match Circle(radius = 1.0) with Circle r -> int r | Square _ -> 0)
"""

let private libB =
    normalizedText <|
        """module Lib.B

open Lib.A

let area = match Circle(radius = 1.0) with Circle r -> r | Square s -> s
let p = { X = 1; Y = 2 }.X
let r = Derived().Run 1
let c = Calc().Greet(name = "x") + string (Calc().Add(1.0, 2.0)) + string (Calc().Add(1, 2))
let e = Calc().Changed
let boxes = Box().Get() + Box<int>().Get()
let v = scale 3 baseValue
let shape: Shape = Square 2.0
"""

let private app =
    normalizedText <|
        """module App.Main

open Lib.A

let a = scale 2 baseValue
let s = Circle(radius = 2.0)
let q = { X = 1; Y = 2 }.Y
let d = Derived().Run 2
let k = Calc().Add(1, 2) + Calc().Greet(name = "y").Length
let ev = Calc().Changed
let bx = Box<string>().Get() + Box().Get()
let t: Point option = None
"""

let private edits (text: string) (pairs: (string * string) list) =
    pairs
    |> List.fold
        (fun (t: string) (find, replace) ->
            Expect.stringContains t find $"the step's text has %s{find}"
            t.Replace(find, replace))
        text

let private docEdit (text: string) =
    edits
        text
        [ "<summary>The base value</summary>", "<summary>The first value</summary>"
          "/// Scales a number", "/// Scales any number"
          "/// A shape", "/// Some shape"
          "/// A circle", "/// A round circle"
          "/// its radius", "/// how far out"
          "/// across", "/// left to right"
          "/// Does it", "/// Runs it"
          "/// Adds an int", "/// Adds two ints"
          "/// The changes", "/// What changed"
          "/// Fires on change", "/// Fires on any change"
          "/// generic Get", "/// generic Get of a T" ]

let private lineInsert (text: string) = edits text [ "/// Some shape", "\n/// Some shape" ]

let private secondDocEdit (text: string) =
    edits
        text
        [ "/// Says hello", "/// Says hi"
          "/// A point", "/// A 2D point"
          "/// The base", "/// The root"
          "/// Runs it", "/// Runs it once"
          "/// Adds a float\n", "" ]

let private scenario =
    let a1 = docEdit libA
    let a2 = lineInsert a1
    let a3 = secondDocEdit a2

    { Name = "docs"
      Files = [ "A.fs", libA; "B.fs", libB ]
      Steps =
        [ "a doc edit", [ "A.fs", a1 ]
          "a line insert above", [ "A.fs", a2 ]
          "a doc edit after it", [ "A.fs", a3 ] ] }

/// A signature that documents a type's overloads in another order than its implementation, and leaves out an overload
/// the implementation keeps private (the review's shapes)
let private signedFsi =
    normalizedText
        """module Lib.S

type Calc =
    new: unit -> Calc
    /// Adds floats (fsi)
    member Add: a: float * b: float -> float
    /// Adds ints (fsi)
    member Add: a: int * b: int -> int

type Hidden =
    new: unit -> Hidden
    /// Joins ints (fsi)
    member Join: a: int * b: int -> int

val here: int
"""

let private signedFs =
    normalizedText
        """module Lib.S

type Calc() =
    member _.Add(a: int, b: int) = a + b
    member _.Add(a: float, b: float) = a + b

type Hidden() =
    member private _.Join(a: string, b: string) = a + b
    member _.Join(a: int, b: int) = a + b

let here = int (Calc().Add(1.0, 2.0)) + Calc().Add(1, 2) + Hidden().Join(1, 2)
"""

let private signedLater =
    normalizedText
        """module Lib.U

open Lib.S

let u = Calc().Add(1, 2) + int (Calc().Add(1.0, 2.0)) + Hidden().Join(3, 4)
"""

let private signedScenario =
    let f1 = edits signedFsi [ "Adds ints (fsi)", "Adds two ints (fsi)"; "Joins ints (fsi)", "Joins two ints (fsi)" ]
    let f2 = edits f1 [ "type Hidden =", "\ntype Hidden =" ]

    { Name = "signed docs"
      Files = [ "S.fsi", signedFsi; "S.fs", signedFs; "U.fs", signedLater ]
      Steps = [ "a signature doc edit", [ "S.fsi", f1 ]; "a line insert in the signature", [ "S.fsi", f2 ] ] }

/// Owners whose names collide: a type's `let` and a module's value of one name, in a type and module of one name; and
/// extensions in one module of two types of one name (the re-review's collide.fsx). Then two extension blocks of one
/// type in separate rows, the first block's member renamed.
let private collide =
    normalizedText
        """module Lib.C

/// A type named like a module
type Foo() =
    /// class let x of Foo
    let x = 1
    member _.Get() = x

module Foo =
    /// module value x of Foo
    let x = 2
    let useX = x

module N1 =
    /// N1's Thing
    type Thing() =
        member _.Id = 1

module N2 =
    /// N2's Thing
    type Thing() =
        member _.Id = 2

module Ext =
    open N1

    type N1.Thing with
        /// Ext on N1.Thing
        member _.Extra() = 10

    type N2.Thing with
        /// Ext on N2.Thing
        member _.Extra() = 20

    type System.String with
        /// first block's Foo
        member s.Foo(x: int) = s.Length + x

    type System.String with
        /// second block's Foo
        member s.Foo(x: string) = s + x

open Ext

let a = N1.Thing().Extra()
let b = N2.Thing().Extra()
let c = "a".Foo(1) + ("a".Foo "b").Length
"""

let private collideLater =
    normalizedText
        """module Lib.D

open Lib.C
open Lib.C.Ext

let d = N1.Thing().Extra() + N2.Thing().Extra() + Foo.x + Foo().Get()
let e = ("x".Foo "y").Length
"""

let private collideScenario, renameScenario =
    let renamed =
        edits
            collide
            [ "        /// first block's Foo\n        member s.Foo(x: int) = s.Length + x", "        /// first block's Bar\n        member s.Bar(x: int) = s.Length + x"
              "let c = \"a\".Foo(1) + (\"a\".Foo \"b\").Length", "let c = \"a\".Bar(1) + (\"a\".Foo \"b\").Length" ]

    let docs = edits renamed [ "/// second block's Foo", "/// the only Foo"; "/// module value x of Foo", "/// Foo's module value x" ]

    { Name = "colliding owners"
      Files = [ "C.fs", collide; "D.fs", collideLater ]
      Steps = [ "a doc edit", [ "C.fs", edits collide [ "/// second block's Foo", "/// the other Foo"; "/// module value x of Foo", "/// Foo's module value x" ] ] ] },
    { Name = "an extension block renamed"
      Files = [ "C.fs", collide; "D.fs", collideLater ]
      Steps =
        [ "the first block's member renamed", [ "C.fs", renamed ]
          "a doc edit", [ "C.fs", docs ]
          "a third block added",
          [ "C.fs",
            edits
                docs
                [ "open Ext\n", "    type System.String with\n        /// third block's Foo\n        member s.Foo(x: float) = s.Length + int x\n\nopen Ext\n" ] ]
          "the middle block removed",
          [ "C.fs",
            edits
                docs
                [ "    type System.String with\n        /// the only Foo\n        member s.Foo(x: string) = s + x\n", ""
                  "(\"a\".Foo \"b\").Length", "0"
                  "open Ext\n", "    type System.String with\n        /// third block's Foo\n        member s.Foo(x: float) = s.Length + int x\n\nopen Ext\n" ] ] ] }

/// Two extension members of `String` in one block, used in a row after it and in a later file. The shapes below each
/// edit the block so that its print is the same: the row using it is kept and still refers to the block's old values.
let private keptUse =
    normalizedText
        """module Lib.C

type System.String with
    /// A doc
    member s.A() = 1
    /// B doc
    member s.B() = 2

let u = "x".A() + "x".B()
let w = 1
"""

let private keptUseLater =
    normalizedText
        """module Lib.D

open Lib.C

let v = "y".A() + "y".B()
"""

let private keptUseScenarios =
    let colMove = edits keptUse [ "member s.B() = 2", "member this.B() = 2" ]
    let colThenDoc = edits colMove [ "/// B doc", "/// B doc edited" ]

    let wrongBefore =
        normalizedText
            """module Lib.C

type System.String with
    member s.A() = 1
    /// B doc
    member s.B() = 2


let u = "x".A() + "x".B()
let w = 1
"""

    // A gains two doc lines and B loses its doc, and a blank line goes: A's name now starts where B's did
    let wrongAfter =
        normalizedText
            """module Lib.C

type System.String with
    /// A doc
    /// A more
    member s.A() = 1
    member s.B() = 2

let u = "x".A() + "x".B()
let w = 1
"""

    let overloads =
        normalizedText
            """module Lib.C

type System.String with
    /// Foo of int
    member s.Foo(x: int) = x
    /// Foo of string
    member s.Foo(x: string) = x

let u = "x".Foo(1) + "x".Foo("y").Length
let w = 1
"""

    let overloadsLater = "module Lib.D\n\nopen Lib.C\n\nlet v = \"y\".Foo(2) + \"y\".Foo(\"z\").Length\n"
    let insertAbove = edits keptUse [ "type System.String with", "\ntype System.String with" ]
    let files = [ "C.fs", keptUse; "D.fs", keptUseLater ]

    [ { Name = "doc lines moved from B to A, the line count kept"
        Files = [ "C.fs", wrongBefore; "D.fs", keptUseLater ]
        Steps = [ "docs moved", [ "C.fs", wrongAfter ] ] }
      { Name = "B's name moved, then its doc edited"
        Files = files
        Steps =
          [ "B's name moved", [ "C.fs", colMove ]
            "then B's doc edited", [ "C.fs", colThenDoc ]
            "then A's doc edited", [ "C.fs", edits colThenDoc [ "/// A doc", "/// A doc edited" ] ] ] }
      { Name = "two extension overloads swapped with their docs"
        Files = [ "C.fs", overloads; "D.fs", overloadsLater ]
        Steps =
          [ "swap",
            [ "C.fs",
              edits
                  overloads
                  [ "/// Foo of int\n    member s.Foo(x: int) = x\n    /// Foo of string\n    member s.Foo(x: string) = x",
                    "/// Foo of string\n    member s.Foo(x: string) = x\n    /// Foo of int\n    member s.Foo(x: int) = x" ] ] ] }
      // The first overload's doc grows by two lines, so the second's old name starts where the first's new one does
      { Name = "an extension overload's doc grown onto the next one's place"
        Files = [ "C.fs", overloads; "D.fs", overloadsLater ]
        Steps = [ "two doc lines on Foo of int", [ "C.fs", edits overloads [ "/// Foo of int\n", "/// Foo of int\n    /// more\n    /// more again\n" ] ] ] }
      { Name = "A's doc out and an undocumented C in, the line count kept"
        Files = files
        Steps =
          [ "A's doc out, C in",
            [ "C.fs", edits keptUse [ "    /// A doc\n", ""; "    member s.B() = 2\n", "    member s.B() = 2\n    member s.C() = 3\n" ] ] ] }
      { Name = "A's doc out and a documented C in, the use row shifted"
        Files = files
        Steps =
          [ "A's doc out, documented C in",
            [ "C.fs", edits keptUse [ "    /// A doc\n", ""; "    member s.B() = 2\n", "    member s.B() = 2\n    /// C doc\n    member s.C() = 3\n" ] ] ] }
      { Name = "two doc lines added to A"
        Files = files
        Steps = [ "two doc lines on A", [ "C.fs", edits keptUse [ "    /// A doc\n", "    /// A doc\n    /// more\n    /// more2\n" ] ] ] }
      { Name = "a line inserted above the block, then a doc edit"
        Files = files
        Steps = [ "line above the block", [ "C.fs", insertAbove ]; "then B's doc edited", [ "C.fs", edits insertAbove [ "/// B doc", "/// B doc edited" ] ] ] } ]

let private wait (t: Task<'a>) = t.WaitAsync(TimeSpan.FromSeconds 110.)

/// Each token's hover against InteractiveChecker's tooltip for the same texts: the differences, and how many tokens
let private compareFile (state: State) (graph: Rfs.Checker.ProjectGraph) (file: string) (reference: string -> Task<FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults>) =
    task {
        let! check = reference file
        let text = state.TextOf file
        let differences = ResizeArray<string>()
        let mutable tokens = 0

        for token in tokensOf text do
            let expected = Hover.tooltipOf check text token.Line token.Character
            let! actual = Hover.at state.OwnersOf graph file token.Line token.Character CancellationToken.None |> wait
            tokens <- tokens + 1

            if actual <> expected then
                differences.Add(
                    sprintf "%s / %s: %s %d,%d %A\n    ours %A\n    FCS  %A" state.Scenario state.Step (Path.GetFileName file) (token.Line + 1) token.Character token.Names actual expected
                )

        return List.ofSeq differences, tokens
    }

let private expectNoDifferences (results: (string list * int) list) =
    let differences = results |> List.collect fst
    let tokens = results |> List.sumBy snd
    printfn "hover grid: %d tokens, %d differ" tokens differences.Length
    Expect.isGreaterThan tokens 0 "tokens"
    let shown = differences |> List.truncate 12 |> String.concat "\n  "
    Expect.isTrue differences.IsEmpty $"%d{differences.Length} of %d{tokens} hovers differ from FCS:\n  %s{shown}"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "HoverGrid"
        [ for promote in [ true; false ] do
              for shape in keptUseScenarios do
                  yield
                   testTask $"""hover equals FCS at every token when a row using an extension is kept: %s{shape.Name} (%s{if promote then "by declaration" else "file level"})""" {
                      let! results =
                          run promote shape (fun state ->
                              task {
                                  let results = ResizeArray()

                                  for file in state.Implementations do
                                      let! r = compareFile state state.Graph file state.Reference
                                      results.Add r

                                  return List.ofSeq results
                              })

                      expectNoDifferences results
                  }

              yield
               testTask $"""hover equals FCS at every token where owners' names collide (%s{if promote then "by declaration" else "file level"})""" {
                  let! results =
                      run promote collideScenario (fun state ->
                          task {
                              let results = ResizeArray()

                              for file in state.Implementations do
                                  let! r = compareFile state state.Graph file state.Reference
                                  results.Add r

                              return List.ofSeq results
                          })

                  expectNoDifferences results
              }

              yield
               testTask $"""hover equals FCS at every token of a signed file and a later file after signature doc edits (%s{if promote then "by declaration" else "file level"})""" {
                  let! results =
                      run promote signedScenario (fun state ->
                          task {
                              let results = ResizeArray()

                              for file in state.Implementations do
                                  let! r = compareFile state state.Graph file state.Reference
                                  results.Add r

                              return List.ofSeq results
                          })

                  expectNoDifferences results
              }

              yield
               testTask $"""hover equals FCS at every token after doc edits, the same file and a later file (%s{if promote then "by declaration" else "file level"})""" {
                  let! results =
                      run promote scenario (fun state ->
                          task {
                              let results = ResizeArray()

                              for file in state.Implementations do
                                  let! r = compareFile state state.Graph file state.Reference
                                  results.Add r

                              return List.ofSeq results
                          })

                  expectNoDifferences results
              }

          yield
           testTask "hover equals FCS at every token of another project after doc edits" {
              let! results =
                  runReferencing scenario app (fun state appGraph appFile ->
                      task {
                          let! inApp = compareFile state appGraph appFile state.Combined
                          return [ inApp ]
                      })

              expectNoDifferences results
          }

          yield
           // Pending: a declaration's text, which decides whether it is checked again, includes every `///` inside it (the
           // fork's ParsedDeclaration.Text), so a doc edit checks the edited declarations again, and the rows of the same
           // file that hold their types. No later file or other project is checked again: the fingerprint has no docs.
           ptestTask "a doc-only edit checks no declaration and renders no signature again" {
              let mutable last = None
              let after = docEdit libA

              let! results =
                  run true { scenario with Steps = [ "a doc edit", [ "A.fs", after ] ] } (fun state ->
                      task {
                          let counts () =
                              state.Graph.CheckedCount, state.Graph.FileLevelChecks, state.Graph.AdHocChecks, state.Implementations |> List.sumBy Hover.signaturesRendered

                          let before = counts ()
                          let results = ResizeArray()

                          for file in state.Implementations do
                              let! r = compareFile state state.Graph file state.Reference
                              results.Add r

                          let first = counts ()

                          // Every hover again: each signature is cached
                          for file in state.Implementations do
                              let! r = compareFile state state.Graph file state.Reference
                              results.Add r

                          let again = counts ()
                          Expect.equal again first $"%s{state.Step}: a repeat hover renders nothing and checks nothing"

                          match last with
                          | Some previous ->
                              let checked', fileLevel, adHoc, _ = before
                              let pc, pf, pa, prendered = previous
                              Expect.equal (checked', fileLevel, adHoc) (pc, pf, pa) "the doc edit checked no declaration"
                              let _, _, _, renderedNow = first
                              Expect.equal renderedNow prendered "no signature was rendered again after the doc edit"
                          | None -> ()

                          last <- Some again
                          return List.ofSeq results
                      })

              expectNoDifferences results
          }

          yield
           testTask "hover equals FCS at every token after an extension block's member is renamed (file level)" {
              let! results =
                  run false renameScenario (fun state ->
                      task {
                          let results = ResizeArray()

                          for file in state.Implementations do
                              let! r = compareFile state state.Graph file state.Reference
                              results.Add r

                          return List.ofSeq results
                      })

              expectNoDifferences results
          }

          yield
           testTask "hover equals FCS at every token after an extension block's member is renamed (by declaration)" {
              let! results =
                  run true renameScenario (fun state ->
                      task {
                          let results = ResizeArray()

                          for file in state.Implementations do
                              let! r = compareFile state state.Graph file state.Reference
                              results.Add r

                          return List.ofSeq results
                      })

              expectNoDifferences results
          }

          yield
           testTask "an edit of one declaration renders no signature again in the declarations it keeps" {
              let mutable last = None
              let edited = edits libA [ "let here = scale 2 baseValue", "let here = scale 3 baseValue" ]

              let! results =
                  run true { scenario with Steps = [ "an edit of here", [ "A.fs", edited ] ] } (fun state ->
                      task {
                          let differences = ResizeArray()
                          let mutable tokens = 0

                          // Every token of a declaration but the edited one, the last of A.fs. A file's `module` line is in no
                          // declaration: its signatures are cached by the file's check, which every edit of the file makes anew.
                          for file in state.Implementations do
                              let! check = state.Reference file
                              let text = state.TextOf file

                              for token in tokensOf text do
                                  if not (token.LineText.StartsWith "let here" || token.LineText.StartsWith "module") then
                                      let expected = Hover.tooltipOf check text token.Line token.Character
                                      let! actual = Hover.at state.OwnersOf state.Graph file token.Line token.Character CancellationToken.None |> wait
                                      tokens <- tokens + 1

                                      if actual <> expected then
                                          differences.Add $"%s{state.Step}: %s{Path.GetFileName file} %d{token.Line + 1},%d{token.Character}"

                          let rendered = state.Implementations |> List.sumBy Hover.signaturesRendered

                          match last with
                          | Some previous -> Expect.equal rendered previous "the kept declarations' signatures were not rendered again"
                          | None -> Expect.isGreaterThan rendered 0L "the first hovers render"

                          last <- Some rendered
                          return [ List.ofSeq differences, tokens ]
                      })

              expectNoDifferences results
          }

          yield
           testTask "a hover whose docs' file changes while it reads answers for the new texts" {
              let withString =
                  edits libA [ "    /// Adds an int", "    /// Adds strings\n    member _.Add(a: string, b: string) = a + b\n    /// Adds an int" ]

              let mutable answered = None

              let! results =
                  run true { scenario with Steps = [ "an overload added above", [ "A.fs", withString ] ] } (fun state ->
                      task {
                          let a = state.Files |> List.find (fun f -> f.EndsWith "A.fs")
                          let b = state.Files |> List.find (fun f -> f.EndsWith "B.fs")
                          let text = state.TextOf b
                          let needle = "Calc().Add(1, 2)"
                          let index = text.IndexOf(needle, StringComparison.Ordinal) + "Calc().".Length + 1
                          let before = text.Substring(0, index)
                          let line = before.Split('\n').Length - 1
                          let character = index - (before.LastIndexOf '\n' + 1)

                          match answered with
                          | None ->
                              // The overload lands after B's signature is read, before its docs are: the docs then read
                              // are of a parse whose overloads are in other places
                              let once = ref 0

                              Hover.afterSignature <-
                                  fun path ->
                                      if path = b && Interlocked.Exchange(&once.contents, 1) = 0 then
                                          state.Graph.SetText(a, withString) |> ignore
                                          task { let! _ = state.Graph.Diagnostics a in () } :> Task
                                      else
                                          Task.CompletedTask

                              let! hover = Hover.at state.OwnersOf state.Graph b line character CancellationToken.None |> wait
                              Hover.afterSignature <- fun _ -> Task.CompletedTask
                              answered <- Some hover
                              return []
                          | Some hover ->
                              let! check = state.Reference b
                              let expected = Hover.tooltipOf check text line character
                              let different = if hover = expected then [] else [ $"ours %A{hover}, FCS %A{expected}" ]
                              return [ different, 1 ]
                      })

              expectNoDifferences results
          } ]
