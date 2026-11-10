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

let private count (text: string) (upTo: int) =
    let mutable k = 0

    for i in 0 .. upTo - 1 do
        if text[i] = '\n' then k <- k + 1

    k

/// <summary>
/// One replacement, made once, and where each line of the new text was in the old one, from the replacement alone:
/// lines before the changed span are where they were, lines after it moved by the lines it added, and lines it touches
/// have no old place.
/// </summary>
/// <remarks>An insertion before or after the found text keeps it; anything else changes only what the two differ in.</remarks>
let private replaceOnce (text: string) (find: string, replace: string) : string * (int -> int option) =
    let at = text.IndexOf(find, StringComparison.Ordinal)
    Expect.isGreaterThanOrEqual at 0 $"the shape's text has %s{find}"
    Expect.equal (text.IndexOf(find, at + 1, StringComparison.Ordinal)) -1 $"%s{find} is in the text once"
    let next = text.Substring(0, at) + replace + text.Substring(at + find.Length)

    // The changed span: [start, oldEnd) of the old text is [start, newEnd) of the new
    let start, oldEnd, newEnd =
        if replace.EndsWith find then at, at, at + replace.Length - find.Length
        elif replace.StartsWith find then at + find.Length, at + find.Length, at + replace.Length
        else
            let mutable p = 0

            while p < find.Length && p < replace.Length && find[p] = replace[p] do
                p <- p + 1

            let mutable s = 0

            while s < find.Length - p && s < replace.Length - p && find[find.Length - 1 - s] = replace[replace.Length - 1 - s] do
                s <- s + 1

            at + p, at + find.Length - s, at + replace.Length - s

    let firstLine = count text start
    let lastOld = count text oldEnd
    let lastNew = count next newEnd
    let atLineStart (t: string) (i: int) = i = 0 || t[i - 1] = '\n'

    let map (line: int) =
        if line < firstLine then Some line
        elif line > lastNew then Some(line - lastNew + lastOld)
        // The span ends where a line starts in both: that line is the old one's, whole
        elif line = lastNew && atLineStart next newEnd && atLineStart text oldEnd then Some lastOld
        else None

    next, map

/// The edits in order, and the old place of each new line through all of them
let private replaceAll (text: string) (edits: (string * string) list) =
    edits
    |> List.fold
        (fun (t: string, map: int -> int option) e ->
            let next, m = replaceOnce t e
            next, (fun line -> m line |> Option.bind map))
        (text, Some)

/// A project, the edits it goes through, and the refusals the grid must see at least once
type private Shape =
    { Name: string
      Files: (string * string) list
      Edit: (string * (string * string) list) list
      /// Refusals that must occur, by declaration and at file level
      ByDeclaration: Refusal list
      FileLevel: Refusal list
      /// Whether some token must be answered from the last state, by declaration
      Marks: bool
      /// Whether some marked token must be on a row's second line or later
      MarksLaterLine: bool }

let private shape name files edit byDeclaration fileLevel marks =
    { Name = name
      Files = files
      Edit = edit
      ByDeclaration = byDeclaration
      FileLevel = fileLevel
      Marks = marks
      MarksLaterLine = false }

let private laterLine (s: Shape) = { s with MarksLaterLine = true }

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

let private rows =
    shape
        "a row edited, a new row, a doc edit, and a file pending on its dependency"
        [ "A.fs", libA; "B.fs", libB ]
        [ "A.fs",
          [ "let edited = scale 2 baseValue", "let edited = scale 4 baseValue"
            "let kept =", "let added = baseValue\n\nlet kept ="
            "/// Scales a number", "/// Scales any number" ] ]
        [ Refusal.Edited; Refusal.NewRow; Refusal.Gap; Refusal.DocsChanged ]
        [ Refusal.CheckedWhole; Refusal.DocsChanged ]
        true

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
// nothing here
#endif

let third = second + picked
"""

let private ifAbove =
    shape
        "#if above the cursor"
        [ "I.fs", ifText ]
        [ "I.fs", [ "let first = 1", "let first = 3"; "let second = ", "#if DEBUG\n#endif\nlet second = " ] ]
        [ Refusal.Directives; Refusal.Edited; Refusal.Gap ]
        [ Refusal.CheckedWhole ]
        true

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

/// The same directives above the cursor, and a `(*` in a branch the parse leaves out but the hover's tokenizer reads,
/// made a line comment: the lines after it start in another lexer state than they did
let private lexerState =
    shape
        "the lexer state a line starts in"
        [ "X.fs", lexerText ]
        [ "X.fs", [ "let first = 1", "let first = 3"; "(* nothing here", "// nothing here" ] ]
        [ Refusal.LexerState ]
        [ Refusal.CheckedWhole ]
        false

let private ordinalText =
    n
        """module Lib.O

let value = 1

do printfn "%d" value

do printfn "%d" (value + 1)

let after = value + 2
"""

/// Two rows of one key, a third of the same text inserted before them: without the ordinal rule its key's old row has
/// the same line, elsewhere
let private ordinals =
    shape
        "key ordinals moving"
        [ "O.fs", ordinalText ]
        [ "O.fs", [ "do printfn \"%d\" value\n", "do printfn \"%d\" value\n\ndo printfn \"%d\" value\n" ] ]
        // inserted before: replaceOnce reads a replacement ending with the found text as an insertion before it
        [ Refusal.OrdinalMoved ]
        [ Refusal.CheckedWhole ]
        true

let private commentText = n "module Lib.K\n\nlet one = 1\n\nlet two = one + 1\n\nlet three = two + one\n"

let private commentOpened =
    shape "(* opened above" [ "K.fs", commentText ] [ "K.fs", [ "let two = ", "(*\nlet two = " ] ] [ Refusal.Gap ] [ Refusal.CheckedWhole ] true

let private lineDirective =
    shape
        "#line above"
        [ "L.fs", commentText ]
        [ "L.fs", [ "let two = ", "# 40 \"elsewhere.fs\"\nlet two = " ] ]
        [ Refusal.Directives ]
        [ Refusal.CheckedWhole ]
        true

let private recModule =
    let text = n "module rec Lib.R\n\nlet one = two - 1\n\nlet two = 2\n\nlet three = one + two\n"
    shape "a rec module" [ "R.fs", text ] [ "R.fs", [ "let two = 2", "let two = 5" ] ] [] [ Refusal.CheckedWhole ] false

let private recNamespace =
    let text =
        n "namespace rec Lib.N\n\ntype T() =\n    member _.U = U()\n\ntype U() =\n    member _.Value = 1\n\nmodule M =\n    let v = T().U.Value\n"

    shape "a rec namespace" [ "N.fs", text ] [ "N.fs", [ "member _.Value = 1", "member _.Value = 2" ] ] [] [ Refusal.CheckedWhole ] false

let private nestedRec =
    let text = n "module Lib.Q\n\nlet zero = 0\n\nmodule rec Inner =\n    let a = b + 1\n    let b = zero\n\nlet last = Inner.a\n"
    shape "a nested rec module" [ "Q.fs", text ] [ "Q.fs", [ "let zero = 0", "let zero = 7" ] ] [ Refusal.Recursive ] [ Refusal.CheckedWhole ] false

let private multiText =
    n
        """module Lib.M

let first = 1

let multi =
    let inner = first + 2
    let other = inner * first
    inner + other

let last = multi + first
"""

/// A row of several lines below a line inserted: its later lines are answered at their shifted places
let private multiLine =
    laterLine (
        shape
          "a marked hover on a later line of a multi-line row"
          [ "M.fs", multiText ]
          [ "M.fs", [ "let first = 1\n", "let first = 1\nlet extra = 2\n" ] ]
          [ Refusal.NewRow ]
          [ Refusal.CheckedWhole ]
          true)

let private crossingText =
    n
        "module Lib.S\n\nlet first = 1\n\nlet text = \"\"\"a string\nacross lines\n  of a row\"\"\" + string first\n\n(* a comment\n   across lines\n   between rows *)\nlet after = first + text.Length\n\nlet last = after + first\n"

/// A triple-quoted string across a row's lines and a block comment across lines between rows, below an inserted line
let private crossing =
    laterLine (
        shape
          "a string and a comment crossing lines and rows"
          [ "S.fs", crossingText ]
          [ "S.fs", [ "let first = 1\n", "let first = 1\nlet extra = first\n" ] ]
          [ Refusal.NewRow ]
          [ Refusal.CheckedWhole ]
          true)

let private crlf =
    let text = multiText.Replace("\n", "\r\n").Replace("module Lib.M", "module Lib.C")

    laterLine (
        shape
          "CRLF text"
          [ "C.fs", text ]
          [ "C.fs", [ "let first = 1\r\n", "let first = 1\r\nlet extra = 2\r\n" ] ]
          [ Refusal.NewRow ]
          [ Refusal.CheckedWhole ]
          true)

let private tabText = n "module Lib.T\n\nlet first = 1\n\nlet tabbed = \"a\tb\" + string first\n\nlet last = tabbed.Length + first\n"

let private tab =
    shape
        "a tab before the cursor"
        [ "T.fs", tabText ]
        [ "T.fs", [ "let first = 1\n", "let first = 1\nlet extra = 2\n" ] ]
        [ Refusal.NewRow ]
        [ Refusal.CheckedWhole ]
        true

let private shapes =
    [ rows; ifAbove; lexerState; ordinals; commentOpened; lineDirective; recModule; recNamespace; nestedRec; multiLine; crossing; crlf; tab ]

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
      MarkedLaterLine: int
      Refused: Map<Refusal, int>
      Failures: string list }

/// The 0-based lines of rows' second and later lines in a text, by the rows of its parse
let private laterLines (graph: ProjectGraph) (file: string) =
    task {
        let! latest = graph.LatestText(file, [||], (fun () -> false), CancellationToken.None) |> wait
        let shape = PositionMap.shapeOf latest.View.Text latest.View.Parse true
        return shape.Rows |> Array.collect (fun r -> [| r.StartLine .. r.EndLine - 1 |]) |> Set.ofArray
    }

/// Every token of every file before the edit hovered settled, then the edit with its checks held, and every token of
/// the new texts from the last state: refused, or marked and equal to FCS on the old text at the place the edit itself
/// says the line was
let private grid (promote: bool) (shape: Shape) : Task<Counts> =
    task {
        let! found =
            run promote { Name = shape.Name; Files = shape.Files; Steps = [] } (fun state ->
                task {
                    let answers = Hover.LastAnswers()
                    let failures = ResizeArray<string>()
                    let refused = Dictionary<Refusal, int>()
                    let mutable cases = 0
                    let mutable marked = 0
                    let mutable markedLater = 0
                    let count r = refused[r] <- (match refused.TryGetValue r with | true, k -> k + 1 | _ -> 1)
                    let fileOf (name: string) = state.Files |> List.find (fun f -> Path.GetFileName f = name)

                    for file in state.Implementations do
                        for t in tokensOf (state.TextOf file) do
                            let! _ = Hover.answer answers (fun () -> false) state.OwnersOf state.Graph file t.Line t.Character CancellationToken.None |> wait
                            ()

                    // Checked from scratch before the edit: a parse of a file's old text in this process after it would
                    // replace the file's line directives under the graph's check
                    let references = Dictionary<string, FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults>()

                    for file in state.Implementations do
                        let! check = state.Reference file
                        references[file] <- check

                    let gate = hold state.Graph
                    let edited = Dictionary<string, string * (int -> int option)>()

                    for name, edits in shape.Edit do
                        let file = fileOf name
                        let text, map = replaceAll (state.TextOf file) edits
                        edited[file] <- (text, map)
                        state.Graph.SetText(file, text) |> ignore

                    try
                        for file in state.Implementations do
                            let oldText = state.TextOf file
                            let text, expectedLine = match edited.TryGetValue file with | true, e -> e | _ -> oldText, Some
                            let check = references[file]
                            let! later = laterLines state.Graph file

                            for t in tokensOf text do
                                cases <- cases + 1

                                let! busy = Hover.fromLastState answers (fun () -> true) state.Graph file t.Line t.Character CancellationToken.None |> wait

                                match busy with
                                | Hover.LastAnswer.Refused Refusal.Reloading -> count Refusal.Reloading
                                | other -> failures.Add $"%s{Path.GetFileName file} %d{t.Line + 1},%d{t.Character} while busy: %A{other}"

                                let! r = Hover.fromLastState answers (fun () -> false) state.Graph file t.Line t.Character CancellationToken.None |> wait
                                let where = $"%s{shape.Name}: %s{Path.GetFileName file} %d{t.Line + 1},%d{t.Character} %A{t.Names}"

                                match r with
                                | Hover.LastAnswer.Refused refusal -> count refusal
                                | Hover.LastAnswer.Settled -> failures.Add $"%s{where}: settled while its check is held"
                                | Hover.LastAnswer.Marked(markdown, at) ->
                                    marked <- marked + 1

                                    if later.Contains t.Line then
                                        markedLater <- markedLater + 1

                                    let prefix = Hover.StaleMark + "\n\n"

                                    match expectedLine t.Line with
                                    | None -> failures.Add $"%s{where}: marked at old %d{at.OldLine + 1}, but the edit changed the line"
                                    | Some line when line <> at.OldLine || at.OldCharacter <> t.Character ->
                                        failures.Add $"%s{where}: mapped to old %d{at.OldLine + 1},%d{at.OldCharacter}, the edit says %d{line + 1},%d{t.Character}"
                                    | Some line ->
                                        let expected = Hover.tooltipOf check oldText line t.Character

                                        if not (markdown.StartsWith prefix) then
                                            failures.Add $"%s{where}: unmarked %A{markdown}"
                                        elif Some(markdown.Substring prefix.Length) <> expected then
                                            failures.Add $"%s{where}: at old %d{line + 1}\n    ours %A{markdown}\n    FCS  %A{expected}"
                    finally
                        gate.Set()

                    let! _ = state.Graph.AllDiagnostics() |> wait

                    return
                        [ { Cases = cases
                            Marked = marked
                            MarkedLaterLine = markedLater
                            Refused = refused |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
                            Failures = List.ofSeq failures } ]
                })

        return List.exactlyOne found
    }

let private expectGrid (promote: bool) (shape: Shape) (c: Counts) =
    let mode = if promote then "by declaration" else "file level"
    let refusals = c.Refused |> Map.toList |> List.map (fun (r, k) -> $"%A{r} %d{k}") |> String.concat ", "
    printfn "last-state grid, %s (%s): %d cases, %d marked (%d on a later line), refused: %s" shape.Name mode c.Cases c.Marked c.MarkedLaterLine refusals
    let shown = c.Failures |> List.truncate 12 |> String.concat "\n  "
    Expect.isTrue c.Failures.IsEmpty $"%d{c.Failures.Length} of %d{c.Cases} failed:\n  %s{shown}"
    Expect.isGreaterThan c.Cases 0 "cases"
    Expect.equal (c.Refused.TryFind Refusal.Reloading) (Some c.Cases) "every case refuses while busy"

    for r in (if promote then shape.ByDeclaration else shape.FileLevel) do
        Expect.isTrue (c.Refused.ContainsKey r) $"%s{shape.Name} (%s{mode}) refuses with %A{r} somewhere"

    if promote && shape.Marks then
        Expect.isGreaterThan c.Marked 0 $"%s{shape.Name}: some token answers from the last state"

    if promote && shape.MarksLaterLine then
        Expect.isGreaterThan c.MarkedLaterLine 0 $"%s{shape.Name}: some token on a row's later line answers from the last state"

    if not shape.Marks then
        Expect.equal c.Marked 0 $"%s{shape.Name}: nothing answers from the last state"

/// The 0-based line and character of the first `needle` in the text
let private find (text: string) (needle: string) =
    let index = text.IndexOf(needle, StringComparison.Ordinal)
    let before = text.Substring(0, index)
    before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1)

/// A graph of one file by declaration on `host`, every token hovered settled (into `answers` when given), then
/// disposed: given weakly
let private hoveredAndDisposed (host: GraphHost) (answers: Hover.LastAnswers option) =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-last-state-"
        let file = Path.Combine(dir.FullName, "G.fs")
        File.WriteAllText(file, libA)
        let! deps = Rfs.Projects.ProjectSpec.load PublishTests.depsProject

        let spec =
            { Rfs.Projects.ProjectSpec.sequential deps with
                ProjectFile = Path.Combine(dir.FullName, "G.fsproj")
                SourceFiles = [ file ] }

        let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec)
        graph.Promote file
        let! _ = graph.AllDiagnostics() |> wait
        let! _ = graph.WhenPromoted(file, CancellationToken.None) |> wait
        let owners (f: string) = if String.Equals(f, file, StringComparison.OrdinalIgnoreCase) then [ graph ] else []

        for t in tokensOf libA do
            match answers with
            | Some a ->
                let! _ = Hover.answer a (fun () -> false) owners graph file t.Line t.Character CancellationToken.None |> wait
                ()
            | None ->
                let! _ = Hover.at owners graph file t.Line t.Character CancellationToken.None |> wait
                ()

        (graph :> IDisposable).Dispose()
        return WeakReference graph
    }

let private collected (weak: WeakReference) =
    for _ in 1..3 do
        GC.Collect()
        GC.WaitForPendingFinalizers()

    not weak.IsAlive

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
                          let answers = Hover.LastAnswers()
                          let a = state.Files |> List.find (fun f -> f.EndsWith "A.fs")
                          let text = state.TextOf a
                          let keptLine, keptChar = find text "kept = edited"
                          let editedLine, editedChar = find text "scale 2 baseValue"
                          let hover l c ct = Hover.answer answers (fun () -> false) state.OwnersOf state.Graph a l c ct
                          let! answered = hover keptLine keptChar CancellationToken.None |> wait
                          let! editedAnswer = hover editedLine editedChar CancellationToken.None |> wait
                          Expect.isSome answered "the settled hover answers"
                          Expect.isSome editedAnswer "the settled hover answers"
                          let gate = hold state.Graph
                          // An edit of another row, and a line inserted above the hovered one: it is shifted
                          let edited, _ = replaceAll text [ "let edited = scale 2 baseValue", "let edited = scale 5 baseValue"; "let kept =", "\nlet kept =" ]
                          state.Graph.SetText(a, edited) |> ignore

                          try
                              let! marked = hover (keptLine + 1) keptChar CancellationToken.None |> fun t -> t.WaitAsync(TimeSpan.FromSeconds 20.)
                              Expect.equal marked (answered |> Option.map (fun t -> Hover.StaleMark + "\n\n" + t)) "the answer from before the edit, marked"
                              Expect.equal answers.Marked 1L "answered from the last state"

                              // The edited row waits: the check is held, so the request doesn't end
                              use cts = new CancellationTokenSource()
                              let waiting = hover editedLine editedChar cts.Token
                              let! _ = Task.WhenAny(waiting, Task.Delay 1500)
                              Expect.isFalse waiting.IsCompleted "a hover in the edited row waits for the check"
                              cts.Cancel()
                          finally
                              gate.Set()

                          let! _ = state.Graph.AllDiagnostics() |> wait
                          let! settled = hover (keptLine + 1) keptChar CancellationToken.None |> wait
                          Expect.equal settled answered "settled, the hover is unmarked"
                          return [ () ]
                      })

              Expect.equal (List.length results) 1 "ran"
          }

          yield
           testTask "an answer is of the last published result: a row checked again since refuses, a kept row answers" {
              let text = n "module Lib.P\n\nlet y = 1\n\nlet x = y\n\nlet z = 3\n"

              let! results =
                  run true { Name = "last published"; Files = [ "P.fs", text ]; Steps = [] } (fun state ->
                      task {
                          let answers = Hover.LastAnswers()
                          let p = state.Files.Head
                          let xLine, xChar = find text "x = y"
                          let zLine, zChar = find text "z = 3"
                          let hover l c ct = Hover.answer answers (fun () -> false) state.OwnersOf state.Graph p l c ct
                          let! x = hover xLine xChar CancellationToken.None |> wait
                          let! z = hover zLine zChar CancellationToken.None |> wait
                          Expect.stringContains x.Value "int" "x is an int"
                          // y becomes a string and settles, with no hover: x's row is checked again, z's is kept
                          let second, _ = replaceAll text [ "let y = 1", "let y = \"a\"" ]
                          state.Graph.SetText(p, second) |> ignore
                          let! _ = state.Graph.AllDiagnostics() |> wait
                          let gate = hold state.Graph
                          let third, _ = replaceAll second [ "let y = \"a\"", "let y = \"b\"" ]
                          state.Graph.SetText(p, third) |> ignore

                          try
                              let! atX = Hover.fromLastState answers (fun () -> false) state.Graph p xLine xChar CancellationToken.None |> wait

                              match atX with
                              | Hover.LastAnswer.Marked(m, _) -> failtest $"x answered from a result before the last published one: %s{m}"
                              | other -> Expect.equal other (Hover.LastAnswer.Refused Refusal.NotAnswered) "x's row was checked again: nothing answered on it"

                              let! atZ = Hover.fromLastState answers (fun () -> false) state.Graph p zLine zChar CancellationToken.None |> wait

                              match atZ with
                              | Hover.LastAnswer.Marked(m, _) -> Expect.equal m (Hover.StaleMark + "\n\n" + z.Value) "z's row kept its result"
                              | other -> failtest $"z's kept row: %A{other}"
                          finally
                              gate.Set()

                          return [ () ]
                      })

              Expect.equal (List.length results) 1 "ran"
          }

          yield
           testTask "nothing of the cache keeps an answer's owner alive" {
              let host = GraphHost.Start "last-state gc"

              try
                  // A real graph's answers in the cache, then one whose owner (a row result) only the cache refers to
                  let answers = Hover.LastAnswers()
                  let! _ = hoveredAndDisposed host (Some answers)
                  Expect.isGreaterThan answers.Count 0 "the cache holds answers"

                  let add () =
                      let owner = obj ()

                      answers.Add(owner, struct (0, 1, "let h = 1", "h"), { Markdown = "h"; Docs = [||]; Comments = [||]; Node = null })

                      WeakReference owner

                  let owner = add ()
                  Expect.isTrue (collected owner) "an answer's owner is held weakly"
                  GC.KeepAlive answers
              finally
                  (host :> IDisposable).Dispose()
          }

          yield
           testTask "the cache keeps answers to its capacity, the least recently used dropped first" {
              let! results =
                  run true { Name = "capacity"; Files = [ "A.fs", libA ]; Steps = [] } (fun state ->
                      task {
                          let answers = Hover.LastAnswers(3)
                          let a = state.Files.Head
                          let text = state.TextOf a
                          let tokens = tokensOf text |> List.truncate 5

                          for t in tokens do
                              let! _ = Hover.answer answers (fun () -> false) state.OwnersOf state.Graph a t.Line t.Character CancellationToken.None |> wait
                              ()

                          Expect.isLessThanOrEqual answers.Count 3 "at most the capacity"
                          return [ () ]
                      })

              Expect.equal (List.length results) 1 "ran"
          } ]
