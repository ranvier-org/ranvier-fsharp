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
    /// Adds an int
    member _.Add(a: int, b: int) = a + b
    /// Adds a float
    member _.Add(a: float, b: float) = a + b
    /// Says hello
    member _.Greet(name: string) = "hi " + name

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
          "/// Adds an int", "/// Adds two ints" ]

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
           // Pending: a declaration's text, which decides whether it is checked again, includes its own `///` (the fork's
           // ParsedDeclaration.Text), so a doc edit checks the edited declarations again, though nothing after them
           ptestTask "a doc-only edit checks no declaration and renders no signature again" {
              let mutable last = None
              let after = docEdit libA

              let! results =
                  run true { scenario with Steps = [ "a doc edit", [ "A.fs", after ] ] } (fun state ->
                      task {
                          let counts () =
                              state.Graph.CheckedCount, state.Graph.FileLevelChecks, state.Graph.AdHocChecks, Hover.signaturesRendered ()

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
          } ]
