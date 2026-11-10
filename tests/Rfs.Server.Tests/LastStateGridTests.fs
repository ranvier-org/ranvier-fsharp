module Rfs.Server.Tests.LastStateGridTests

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Server
open Rfs.Server.Tests.TokenGrid

let private n (t: string) = t.Replace("\r\n", "\n")

let private wait (t: Task<'a>) = t.WaitAsync(TimeSpan.FromSeconds 110.)

let private edit (text: string) (find: string) (replace: string) =
    Expect.stringContains text find $"the shape's text has %s{find}"
    text.Replace(find, replace)

/// A project, the texts an edit gives some of its files, and the refusals the grid must see at least once
type private Shape =
    { Name: string
      Files: (string * string) list
      Edit: (string * string) list
      /// Refusals that must occur, by declaration and at file level
      ByDeclaration: Refusal list
      FileLevel: Refusal list
      /// Whether some token must be answered from the last state, by declaration
      Marks: bool }

let private libA =
    n
        """module Lib.A

/// The base value
let baseValue = 10

/// Scales a number
let scale (factor: int) (x: int) = x * factor

// A comment between rows
let edited = scale 2 baseValue

let kept = edited + baseValue
"""

let private libB =
    n
        """module Lib.B

open Lib.A

let area = scale 3 baseValue
let more = kept + 1
"""

/// A row edited, a row added, a doc edited, and a dependency's text kept (B)
let private rows =
    { Name = "a row edited, a new row, a doc edit, and a file pending on its dependency"
      Files = [ "A.fs", libA; "B.fs", libB ]
      Edit =
        [ "A.fs",
          libA
          |> fun t -> edit t "let edited = scale 2 baseValue" "let edited = scale 4 baseValue"
          |> fun t -> edit t "let kept =" "let added = baseValue\n\nlet kept ="
          |> fun t -> edit t "/// Scales a number" "/// Scales any number" ]
      ByDeclaration = [ Refusal.Edited; Refusal.NewRow; Refusal.Gap; Refusal.DocsChanged ]
      FileLevel = [ Refusal.CheckedWhole; Refusal.DocsChanged ]
      Marks = true }

let private ifText =
    n
        """module Lib.I

let first = 1

#if NET
let picked = first + 1
#else
let picked = first + 2
#endif

let second = picked + first

#if NET
#else
(* nothing here
#endif

let third = second + picked
"""

/// `#if` above the cursor: a directive pair inserted above a row, and in a branch the parse leaves out but the hover's
/// tokenizer (no defines) reads, a `(*` that opened a comment for it, closed by the edit
let private ifAbove =
    { Name = "#if above the cursor, and a branch's (* the tokenizer reads"
      Files = [ "I.fs", ifText ]
      Edit =
        [ "I.fs",
          ifText
          |> fun t -> edit t "let first = 1" "let first = 3"
          |> fun t -> edit t "let second = " "#if DEBUG\n#endif\nlet second = "
          |> fun t -> edit t "(* nothing here" "// nothing here" ]
      ByDeclaration = [ Refusal.Directives; Refusal.Edited; Refusal.Gap ]
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = true }

let private lexerText =
    n
        """module Lib.X

let first = 1

#if NET
#else
(* nothing here
#endif

let second = first + 1
"""

/// The same directives above the cursor, and a `(*` in a branch the parse leaves out but the hover's tokenizer reads:
/// a line comment in the new text, so the lines after it start in another lexer state than they did
let private lexerState =
    { Name = "the lexer state a line starts in"
      Files = [ "X.fs", lexerText ]
      Edit = [ "X.fs", lexerText |> fun t -> edit t "let first = 1" "let first = 3" |> fun t -> edit t "(* nothing here" "// nothing here" ]
      ByDeclaration = [ Refusal.LexerState ]
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = false }

let private ordinalText =
    n
        """module Lib.O

let value = 1

do printfn "%d" value

do printfn "%d" (value + 1)

let after = value + 2
"""

/// Two rows of one key, another inserted before them
let private ordinals =
    { Name = "key ordinals moving"
      Files = [ "O.fs", ordinalText ]
      Edit = [ "O.fs", edit ordinalText "do printfn \"%d\" value" "do printfn \"%d\" (value + 9)\n\ndo printfn \"%d\" value" ]
      ByDeclaration = [ Refusal.OrdinalMoved ]
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = true }

let private commentText =
    n
        """module Lib.K

let one = 1

let two = one + 1

let three = two + one
"""

/// `(*` opened above, unclosed: the rows after it are gone from the parse
let private commentOpened =
    { Name = "(* opened above"
      Files = [ "K.fs", commentText ]
      Edit = [ "K.fs", edit commentText "let two = " "(*\nlet two = " ]
      ByDeclaration = [ Refusal.Gap ]
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = true }

let private lineText =
    n
        """module Lib.L

let one = 1

let two = one + 1

let three = two + one
"""

let private lineDirective =
    { Name = "#line above"
      Files = [ "L.fs", lineText ]
      Edit = [ "L.fs", edit lineText "let two = " "# 40 \"elsewhere.fs\"\nlet two = " ]
      ByDeclaration = [ Refusal.Directives ]
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = true }

let private recModule =
    let text = n "module rec Lib.R\n\nlet one = two - 1\n\nlet two = 2\n\nlet three = one + two\n"

    { Name = "a rec module"
      Files = [ "R.fs", text ]
      Edit = [ "R.fs", edit text "let two = 2" "let two = 5" ]
      ByDeclaration = []
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = false }

let private recNamespace =
    let text =
        n "namespace rec Lib.N\n\ntype T() =\n    member _.U = U()\n\ntype U() =\n    member _.Value = 1\n\nmodule M =\n    let v = T().U.Value\n"

    { Name = "a rec namespace"
      Files = [ "N.fs", text ]
      Edit = [ "N.fs", edit text "member _.Value = 1" "member _.Value = 2" ]
      ByDeclaration = []
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = false }

let private nestedRec =
    let text = n "module Lib.Q\n\nlet zero = 0\n\nmodule rec Inner =\n    let a = b + 1\n    let b = zero\n\nlet last = Inner.a\n"

    { Name = "a nested rec module"
      Files = [ "Q.fs", text ]
      Edit = [ "Q.fs", edit text "let zero = 0" "let zero = 7" ]
      ByDeclaration = [ Refusal.Recursive ]
      FileLevel = [ Refusal.CheckedWhole ]
      Marks = false }

let private shapes = [ rows; ifAbove; lexerState; ordinals; commentOpened; lineDirective; recModule; recNamespace; nestedRec ]

/// Holds every check of the graph that a dependency computation leads to, until released
let private hold (graph: ProjectGraph) =
    let gate = new ManualResetEventSlim(false)

    graph.OnFlight(fun kind _ ct ->
        if kind = "dependencies" then
            gate.Wait ct)

    gate

type private Counts =
    { Cases: int
      Marked: int
      Refused: Map<Refusal, int>
      Failures: string list }

/// Every token of every file before the edit hovered settled, then the edit with its checks held, and every token of
/// the new texts from the last state: refused, or marked and equal to FCS on the old text at the mapped place
let private grid (promote: bool) (shape: Shape) : Task<Counts> =
    task {
        let! found =
            run promote { Name = shape.Name; Files = shape.Files; Steps = [] } (fun state ->
                task {
                    let failures = ResizeArray<string>()
                    let refused = Dictionary<Refusal, int>()
                    let mutable cases = 0
                    let mutable marked = 0
                    let count r = refused[r] <- (match refused.TryGetValue r with | true, k -> k + 1 | _ -> 1)
                    let fileOf (name: string) = state.Files |> List.find (fun f -> Path.GetFileName f = name)

                    // A file no hover has read has nothing to answer from
                    let first = state.Implementations.Head

                    match tokensOf (state.TextOf first) with
                    | t :: _ ->
                        let! r = Hover.fromLastState (fun () -> false) state.Graph first t.Line t.Character CancellationToken.None |> wait

                        match r with
                        | Hover.LastAnswer.Refused Refusal.NoSnapshot -> count Refusal.NoSnapshot
                        | other -> failures.Add $"before any hover: %A{other}"
                    | [] -> ()

                    for file in state.Implementations do
                        for t in tokensOf (state.TextOf file) do
                            let! _ = Hover.at state.OwnersOf state.Graph file t.Line t.Character CancellationToken.None |> wait
                            ()

                    // Checked from scratch before the edit: a parse of a file's old text in this process after it would
                    // replace the file's line directives under the graph's check
                    let references = Dictionary<string, FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults>()

                    for file in state.Implementations do
                        let! check = state.Reference file
                        references[file] <- check

                    let gate = hold state.Graph
                    let newText = Dictionary<string, string>()

                    for name, text in shape.Edit do
                        let file = fileOf name
                        newText[file] <- text
                        state.Graph.SetText(file, text) |> ignore

                    try
                        for file in state.Implementations do
                            let oldText = state.TextOf file
                            let text = match newText.TryGetValue file with | true, t -> t | _ -> oldText
                            let check = references[file]

                            for t in tokensOf text do
                                cases <- cases + 1

                                // A reload under way refuses first
                                let! reloading = Hover.fromLastState (fun () -> true) state.Graph file t.Line t.Character CancellationToken.None |> wait

                                match reloading with
                                | Hover.LastAnswer.Refused Refusal.Reloading -> count Refusal.Reloading
                                | other -> failures.Add $"%s{Path.GetFileName file} %d{t.Line + 1},%d{t.Character} during a reload: %A{other}"

                                let! r = Hover.fromLastState (fun () -> false) state.Graph file t.Line t.Character CancellationToken.None |> wait
                                let where = $"%s{shape.Name}: %s{Path.GetFileName file} %d{t.Line + 1},%d{t.Character} %A{t.Names}"

                                match r with
                                | Hover.LastAnswer.Refused refusal -> count refusal
                                | Hover.LastAnswer.Settled -> failures.Add $"%s{where}: settled while its check is held"
                                | Hover.LastAnswer.Marked(markdown, at) ->
                                    marked <- marked + 1
                                    let expected = Hover.tooltipOf check oldText at.OldLine at.OldCharacter
                                    let prefix = Hover.StaleMark + "\n\n"

                                    if not (markdown.StartsWith prefix) then
                                        failures.Add $"%s{where}: unmarked %A{markdown}"
                                    elif Some(markdown.Substring prefix.Length) <> expected then
                                        failures.Add $"%s{where}: at old %d{at.OldLine + 1},%d{at.OldCharacter}\n    ours %A{markdown}\n    FCS  %A{expected}"

                                    // The place is the same token
                                    if Hover.lookupOf oldText at.OldLine at.OldCharacter <> Hover.lookupOf text t.Line t.Character then
                                        failures.Add $"%s{where}: mapped to another token at old %d{at.OldLine + 1},%d{at.OldCharacter}"
                    finally
                        gate.Set()

                    let! _ = state.Graph.AllDiagnostics() |> wait

                    return
                        [ { Cases = cases
                            Marked = marked
                            Refused = refused |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
                            Failures = List.ofSeq failures } ]
                })

        return List.exactlyOne found
    }

let private expectGrid (promote: bool) (shape: Shape) (c: Counts) =
    let mode = if promote then "by declaration" else "file level"
    let refusals = c.Refused |> Map.toList |> List.map (fun (r, k) -> $"%A{r} %d{k}") |> String.concat ", "
    printfn "last-state grid, %s (%s): %d cases, %d marked, refused: %s" shape.Name mode c.Cases c.Marked refusals
    let shown = c.Failures |> List.truncate 12 |> String.concat "\n  "
    Expect.isTrue c.Failures.IsEmpty $"%d{c.Failures.Length} of %d{c.Cases} failed:\n  %s{shown}"
    Expect.isGreaterThan c.Cases 0 "cases"
    Expect.equal (c.Refused.TryFind Refusal.Reloading) (Some c.Cases) "every case refuses during a reload"
    Expect.equal (c.Refused.TryFind Refusal.NoSnapshot) (Some 1) "a file no hover read refuses"

    for r in (if promote then shape.ByDeclaration else shape.FileLevel) do
        Expect.isTrue (c.Refused.ContainsKey r) $"%s{shape.Name} (%s{mode}) refuses with %A{r} somewhere"

    if promote && shape.Marks then
        Expect.isGreaterThan c.Marked 0 $"%s{shape.Name}: some token answers from the last state"

    if not shape.Marks then
        Expect.equal c.Marked 0 $"%s{shape.Name}: nothing answers from the last state"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "LastStateGrid"
        [ for promote in [ true; false ] do
              for shape in shapes do
                  yield
                   testTask $"""hover from the last state at every token after an edit: %s{shape.Name} (%s{if promote then "by declaration" else "file level"})""" {
                      let! c = grid promote shape
                      expectGrid promote shape c
                  }

          yield
           testTask "a hover answered before a safe edit answers marked without waiting for the held check" {
              let! results =
                  run true { Name = "counting"; Files = rows.Files; Steps = [] } (fun state ->
                      task {
                          let a = state.Files |> List.find (fun f -> f.EndsWith "A.fs")
                          let text = state.TextOf a
                          let at (needle: string) =
                              let index = text.IndexOf(needle, StringComparison.Ordinal)
                              let before = text.Substring(0, index)
                              before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1)

                          let keptLine, keptChar = at "kept = edited"
                          let editedLine, editedChar = at "scale 2 baseValue"
                          let! answered = Hover.answer (fun () -> false) state.OwnersOf state.Graph a keptLine keptChar CancellationToken.None |> wait
                          let! editedAnswer = Hover.answer (fun () -> false) state.OwnersOf state.Graph a editedLine editedChar CancellationToken.None |> wait
                          Expect.isSome answered "the settled hover answers"
                          Expect.isSome editedAnswer "the settled hover answers"
                          let before = Hover.markedAnswers
                          let gate = hold state.Graph
                          // An edit of another row, and a line inserted above the hovered one: it is shifted
                          let edited = edit (edit text "let edited = scale 2 baseValue" "let edited = scale 5 baseValue") "let kept =" "\nlet kept ="
                          state.Graph.SetText(a, edited) |> ignore

                          try
                              let! last = Hover.fromLastState (fun () -> false) state.Graph a (keptLine + 1) keptChar CancellationToken.None |> wait

                              match last with
                              | Hover.LastAnswer.Marked _ -> ()
                              | other ->
                                  let! latest = state.Graph.LatestText(a, [||], CancellationToken.None) |> wait
                                  let shape = PositionMap.shapeOf latest.View.Text latest.View.Parse true
                                  let rows = shape.Rows |> Array.map (fun r -> $"%s{r.Key.Name} %d{r.StartLine},%d{r.StartColumn}-%d{r.EndLine},%d{r.EndColumn}")
                                  failtest $"from the last state at %d{keptLine + 1},%d{keptChar}: %A{other}; rows %A{rows}"

                              let! marked =
                                  Hover.answer (fun () -> false) state.OwnersOf state.Graph a (keptLine + 1) keptChar CancellationToken.None
                                  |> fun t -> t.WaitAsync(TimeSpan.FromSeconds 20.)

                              Expect.equal marked (answered |> Option.map (fun t -> Hover.StaleMark + "\n\n" + t)) "the answer from before the edit, marked"
                              Expect.equal Hover.markedAnswers (before + 2L) "answered from the last state, once directly and once as the hover"

                              // The edited row waits: the check is held, so the request doesn't end
                              use cts = new CancellationTokenSource()
                              let waiting = Hover.answer (fun () -> false) state.OwnersOf state.Graph a editedLine editedChar cts.Token
                              let! _ = Task.WhenAny(waiting, Task.Delay 1500)
                              Expect.isFalse waiting.IsCompleted "a hover in the edited row waits for the check"
                              cts.Cancel()
                          finally
                              gate.Set()

                          let! _ = state.Graph.AllDiagnostics() |> wait

                          let! settled = Hover.answer (fun () -> false) state.OwnersOf state.Graph a (keptLine + 1) keptChar CancellationToken.None |> wait
                          Expect.equal settled answered "settled, the hover is unmarked"
                          return [ () ]
                      })

              Expect.equal (List.length results) 1 "ran"
          } ]
