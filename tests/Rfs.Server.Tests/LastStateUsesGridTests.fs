module Rfs.Server.Tests.LastStateUsesGridTests

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
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

/// One replacement, made once, and where each line of the new text was in the old one, from the replacement alone
/// (as LastStateGridTests): lines before the changed span keep their place, lines after it move by the lines it added,
/// lines it touches have no old place.
let private replaceOnce (text: string) (find: string, replace: string) : string * (int -> int option) =
    let at = text.IndexOf(find, StringComparison.Ordinal)
    Expect.isGreaterThanOrEqual at 0 $"the shape's text has %s{find}"
    Expect.equal (text.IndexOf(find, at + 1, StringComparison.Ordinal)) -1 $"%s{find} is in the text once"
    let next = text.Substring(0, at) + replace + text.Substring(at + find.Length)

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
        elif line = lastNew && atLineStart next newEnd && atLineStart text oldEnd then Some lastOld
        else None

    next, map

let private replaceAll (text: string) (edits: (string * string) list) =
    edits
    |> List.fold
        (fun (t: string, map: int -> int option) e ->
            let next, m = replaceOnce t e
            next, (fun line -> m line |> Option.bind map))
        (text, Some)

/// A project, the edits it goes through, and what the grid must see
type private Shape =
    { Name: string
      Files: (string * string) list
      Edit: (string * (string * string) list) list
      /// Refusals that must occur, and whether some answer must come from the last state (false: none may), for
      /// references by declaration
      Refusals: Refusal list
      Answers: bool
      /// The same for definition by declaration, and for each feature at file level
      Definition: Refusal list * bool
      FileReferences: Refusal list * bool
      FileDefinition: Refusal list * bool
      /// Some references answer must hold uses in both projects
      CrossesProjects: bool
      /// When given, App.fs in a project referencing these files, which the edits never touch
      App: string option }

let private shape name files edit refusals answers =
    { Name = name
      Files = files
      Edit = edit
      Refusals = refusals
      Answers = answers
      Definition = refusals, answers
      FileReferences = [ Refusal.CheckedWhole ], false
      FileDefinition = [ Refusal.CheckedWhole ], false
      CrossesProjects = false
      App = None }

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

/// `scale` and `baseValue` have uses in A's edited row, so asking from B refuses; `kept` moves down past a new row
let private rows =
    shape
        "a row edited, a new row and a doc edit; uses in another file, and in a row the edit holds"
        [ "A.fs", libA; "B.fs", libB ]
        [ "A.fs",
          [ "let edited = scale 2 baseValue", "let edited = scale 4 baseValue"
            "let kept =", "let added = baseValue\n\nlet kept ="
            "/// Scales a number", "/// Scales any number" ] ]
        [ Refusal.Edited; Refusal.NewRow; Refusal.Gap ]
        true
    |> fun s -> { s with FileReferences = [ Refusal.CheckedWhole ], true; FileDefinition = [ Refusal.CheckedWhole ], true }

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

let private lineAbove =
    shape
        "a line inserted above uses in the edited file"
        [ "M.fs", multiText ]
        [ "M.fs", [ "let first = 1\n", "let first = 1\nlet extra = 2\n" ] ]
        [ Refusal.NewRow ]
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

let third = second + picked
"""

let private ifAbove =
    shape
        "#if above the uses"
        [ "I.fs", ifText ]
        [ "I.fs", [ "let second = ", "#if DEBUG\n#endif\nlet second = " ] ]
        [ Refusal.Directives ]
        false
    |> fun s -> { s with Definition = [ Refusal.Directives ], true }

let private ordinalText =
    n
        """module Lib.O

let value = 1

do printfn "%d" value

do printfn "%d" (value + 1)

let after = value + 2
"""

let private ordinals =
    shape
        "key ordinals moving"
        [ "O.fs", ordinalText ]
        [ "O.fs", [ "do printfn \"%d\" value\n", "do printfn \"%d\" value\n\ndo printfn \"%d\" value\n" ] ]
        [ Refusal.OrdinalMoved ]
        true

let private lineDirective =
    let text = n "module Lib.L\n\nlet one = 1\n\nlet two = one + 1\n\nlet three = two + one\n"
    { shape "#line above" [ "L.fs", text ] [ "L.fs", [ "let two = ", "# 40 \"elsewhere.fs\"\nlet two = " ] ] [ Refusal.Directives ] false with
        Definition = [ Refusal.Directives ], true }

let private nestedRec =
    let text = n "module Lib.Q\n\nlet zero = 0\n\nmodule rec Inner =\n    let a = b + 1\n    let b = zero\n\nlet last = Inner.a + zero\n"
    shape "a nested rec module" [ "Q.fs", text ] [ "Q.fs", [ "let last = ", "let extra = 1\n\nlet last = " ] ] [ Refusal.Recursive ] false

let private signatureText = n "module Lib.S\n\n/// One\nval one: int\n\nval add: int -> int\n"
let private signedText = n "module Lib.S\n\nlet one = 1\n\nlet add x = x + one\n"
let private userText = n "module Lib.U\n\nopen Lib.S\n\nlet two = add one\n\nlet three = add two\n"

/// Uses in a signature file, whose text the edit leaves alone: they stay where they are
let private signature =
    shape
        "uses in a signature file"
        [ "S.fsi", signatureText; "S.fs", signedText; "U.fs", userText ]
        [ "U.fs", [ "let three = ", "let extra = 0\n\nlet three = " ] ]
        [ Refusal.NewRow ]
        true
    |> fun s -> { s with FileReferences = [ Refusal.CheckedWhole ], true; FileDefinition = [ Refusal.CheckedWhole ], true }

/// A signature file edited: every answer with a use in it waits
let private signatureEdited =
    shape
        "a signature file edited"
        [ "S.fsi", signatureText; "S.fs", signedText; "U.fs", userText ]
        [ "S.fsi", [ "/// One", "/// The one" ] ]
        [ Refusal.Edited ]
        false
    |> fun s -> { s with FileReferences = [ Refusal.Edited ], false; FileDefinition = [ Refusal.Edited ], false }

let private appText = n "module App.Main\n\nopen Lib.A\n\nlet fromApp = scale 5 kept\n\nlet again = kept + baseValue\n"

/// Uses in App, another project, while Lib's file is edited above them
let private otherProject =
    { shape
          "uses in another project"
          [ "A.fs", libA ]
          [ "A.fs", [ "let kept =", "let added = baseValue\n\nlet kept =" ] ]
          [ Refusal.NewRow ]
          true with
        App = Some appText
        CrossesProjects = true }

let private editedMultiText =
    n
        """module Lib.W

let first = 1

let multi =
    let inner = first + 2
    let other = inner * first
    inner + other

let last = multi + 1
"""

/// A multi-line row edited on one line: a use on its other lines, unchanged, refuses with the row
let private editedMultiRow =
    shape
        "a multi-line row edited on one line"
        [ "W.fs", editedMultiText ]
        [ "W.fs", [ "let other = inner * first", "let other = inner * first * 1" ] ]
        [ Refusal.Edited ]
        true

let private shapes = [ rows; lineAbove; ifAbove; ordinals; lineDirective; nestedRec; signature; signatureEdited; otherProject; editedMultiRow ]

let private hold (graphs: ProjectGraph list) =
    let gate = new ManualResetEventSlim(false)

    for graph in graphs do
        graph.OnFlight(fun kind _ ct ->
            if kind = "dependencies" then
                gate.Wait ct)

    gate

type private Counts =
    { Cases: int
      Answered: int
      Refused: Map<Refusal, int>
      /// Old answers that differ from FCS by a ReferencesGrid or DefinitionGrid limit: compared with Ranvier's own
      Limits: int
      /// References answers holding uses in more than one project
      Crossing: int
      Failures: string list }

let private showRange (r: range) = $"%s{Path.GetFileName r.FileName} %d{r.StartLine},%d{r.StartColumn}-%d{r.EndLine},%d{r.EndColumn}"

let private usesSet (uses: struct (range * bool) list) = uses |> List.map (fun (struct (r, d)) -> showRange r, d) |> Set.ofList

/// The old answer's ranges at the new text's lines, from the edits alone: None when a range's line has no new place
let private expectedAt (inverse: string -> int -> int option) (r: range) : range option =
    match inverse r.FileName (r.StartLine - 1), inverse r.FileName (r.EndLine - 1) with
    | Some s, Some e -> Some(Range.mkRange r.FileName (Position.mkPos (s + 1) r.StartColumn) (Position.mkPos (e + 1) r.EndColumn))
    | _ -> None

type private Feature =
    | References
    | Definition

/// Every token of every file before the edit asked settled (filling the last answers), the same checked from scratch
/// by FCS, then the edits with every check held, and every token of the new texts from the last state: refused, or an
/// answer that equals the old one at the place the edits say the token was, every range moved by the edits alone
let private grid (promote: bool) (feature: Feature) (shape: Shape) : Task<Counts> =
    task {
        let failures = ResizeArray<string>()
        let refused = Dictionary<Refusal, int>()
        let mutable cases = 0
        let mutable answered = 0
        let mutable limits = 0
        let mutable crossing = 0
        let countRefusal r = refused[r] <- (match refused.TryGetValue r with | true, k -> k + 1 | _ -> 1)

        let atState (state: State) (graphs: ProjectGraph list) (asking: (ProjectGraph * string) list) (check: string -> Task<FSharpCheckFileResults>) =
            task {
                let shapes = Hover.LastAnswers()
                let uses = References.LastAnswers()
                let definitions = Definition.LastAnswers()
                let projectsNow () = graphs
                let allFiles = asking |> List.map snd
                let filesOf (g: ProjectGraph) = if obj.ReferenceEquals(g, state.Graph) then state.Files else asking |> List.filter (fun (o, _) -> obj.ReferenceEquals(o, g)) |> List.map snd

                let scope: References.Scope =
                    { OwnersOf = state.OwnersOf
                      ReferencingProjects = fun g -> if obj.ReferenceEquals(g, state.Graph) then graphs |> List.filter (fun o -> not (obj.ReferenceEquals(o, g))) else []
                      Projects = fun () -> graphs
                      FilesOf = filesOf
                      IsLoading = fun () -> false }

                let everyFile = (state.Files @ allFiles) |> List.distinct
                let checks = Dictionary<string, FSharpCheckFileResults>()

                for f in everyFile do
                    let! c = check f
                    checks[f] <- c

                // Settled, before the edit: Ranvier's answers, which fill the last answers, and FCS's
                let oldUses = Dictionary<struct (string * int * int), struct (range * bool) list>()
                let oldDefinitions = Dictionary<struct (string * int * int), range option>()
                let fcsUses = Dictionary<struct (string * int * int), Set<string * bool>>()
                let fcsDefinitions = Dictionary<struct (string * int * int), range option>()

                for graph, file in asking do
                    for t in tokensOf (state.TextOf file) do
                        let at = struct (file, t.Line, t.Character)

                        match feature with
                        | References ->
                            let! found = References.answer uses shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) scope graph file t.Line t.Character CancellationToken.None |> wait

                            match found with
                            | References.Answer.Found u -> oldUses[at] <- u
                            | References.Answer.Moved -> ()

                            match checks[file].GetSymbolUseAtLocation(t.Line + 1, t.EndColumn, t.LineText, t.Names) with
                            | Some asked ->
                                fcsUses[at] <-
                                    set
                                        [ for f in everyFile do
                                              for u in checks[f].GetUsesOfSymbolInFile asked.Symbol do
                                                  yield showRange u.Range, u.IsFromDefinition ]
                            | None -> fcsUses[at] <- Set.empty
                        | Definition ->
                            let! found = Definition.answer definitions shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) projectsNow state.OwnersOf graph file t.Line t.Character CancellationToken.None |> wait
                            oldDefinitions[at] <- found

                            fcsDefinitions[at] <-
                                match checks[file].GetDeclarationLocation(t.Line + 1, t.EndColumn, t.LineText, t.Names, preferFlag = true) with
                                | FindDeclResult.DeclFound r -> Some r
                                | _ -> None

                // Each file's rows before the edit, by its parse then
                let oldRows = Dictionary<string, RowShape[]>(StringComparer.OrdinalIgnoreCase)

                for graph, file in asking do
                    let! latest = graph.LatestText(file, [||], (fun () -> false), CancellationToken.None) |> wait
                    oldRows[file] <- (PositionMap.shapeOf latest.View.Text latest.View.Parse true).Rows

                let gate = hold graphs
                let edited = Dictionary<string, string * (int -> int option)>(StringComparer.OrdinalIgnoreCase)
                let fileOf (name: string) = everyFile |> List.find (fun f -> Path.GetFileName f = name)

                for name, edits in shape.Edit do
                    let file = fileOf name
                    let text, map = replaceAll (state.TextOf file) edits
                    edited[file] <- (text, map)
                    state.Graph.SetText(file, text) |> ignore

                // Old line to new, from the edits alone
                let inverse (file: string) (line: int) =
                    match edited.TryGetValue(Path.GetFullPath file) with
                    | true, (text, map) -> seq { 0 .. text.Split('\n').Length - 1 } |> Seq.tryFind (fun l -> map l = Some line)
                    | _ -> Some line

                // A range in a row the edits changed: a non-doc line of it with no new place, or lines moved by different amounts
                let inEditedRow (r: range) =
                    match oldRows.TryGetValue(Path.GetFullPath r.FileName) with
                    | true, rows ->
                        let fileLines = (state.TextOf r.FileName).Split('\n')
                        rows
                        |> Array.exists (fun row ->
                            row.StartLine <= r.StartLine
                            && r.StartLine <= row.EndLine
                            && (let shifts = [ for l in row.StartLine - 1 .. row.EndLine - 1 -> inverse r.FileName l |> Option.map (fun n -> n - l) ]
                                let diffShifts = (shifts |> List.choose id |> List.distinct |> List.length) > 1
                                let nonDocEdited =
                                    [ row.StartLine - 1 .. row.EndLine - 1 ]
                                    |> List.exists (fun l ->
                                        (inverse r.FileName l).IsNone
                                        && not (fileLines[l].TrimStart().StartsWith("///")))
                                diffShifts || nonDocEdited))
                    | _ -> false

                let projectOf (f: string) = Path.GetFileName(Path.GetDirectoryName f)

                try
                    for graph, file in asking do
                        let text, expectedLine =
                            match edited.TryGetValue file with
                            | true, e -> e
                            | _ -> state.TextOf file, Some

                        for t in tokensOf text do
                            cases <- cases + 1
                            let where = $"%s{shape.Name}: %s{Path.GetFileName file} %d{t.Line + 1},%d{t.Character} %A{t.Names}"
                            let old () = expectedLine t.Line |> Option.map (fun l -> struct (file, l, t.Character))

                            let busy () =
                                task {
                                    match feature with
                                    | References ->
                                        match! References.fromLastState uses shapes (fun () -> true) (fun () -> true) projectsNow graph file t.Line t.Character CancellationToken.None |> wait with
                                        | LastState.Outcome.Refused r -> return Some r
                                        | _ -> return None
                                    | Definition ->
                                        match! Definition.fromLastState definitions shapes (fun () -> true) (fun () -> true) projectsNow graph file t.Line t.Character CancellationToken.None |> wait with
                                        | LastState.Outcome.Refused r -> return Some r
                                        | _ -> return None
                                }

                            match! busy () with
                            | Some Refusal.Reloading -> countRefusal Refusal.Reloading
                            | other -> failures.Add $"%s{where} while busy: %A{other}"

                            // A reload that starts between the two turns: busy only from the second read on
                            let laterBusy () =
                                let reads = ref 0
                                fun () -> (reads.Value <- reads.Value + 1; reads.Value > 1)

                            let probe (busy: unit -> bool) (pending: unit -> bool) =
                                task {
                                    match feature with
                                    | References ->
                                        match! References.fromLastState uses shapes busy pending projectsNow graph file t.Line t.Character CancellationToken.None |> wait with
                                        | LastState.Outcome.Answered _ -> return "answered"
                                        | LastState.Outcome.Settled -> return "settled"
                                        | LastState.Outcome.Refused r -> return $"%A{r}"
                                    | Definition ->
                                        match! Definition.fromLastState definitions shapes busy pending projectsNow graph file t.Line t.Character CancellationToken.None |> wait with
                                        | LastState.Outcome.Answered _ -> return "answered"
                                        | LastState.Outcome.Settled -> return "settled"
                                        | LastState.Outcome.Refused r -> return $"%A{r}"
                                }

                            let! notPending = probe (fun () -> false) (fun () -> false)

                            if notPending <> "settled" then
                                failures.Add $"%s{where} with nothing pending: %s{notPending}"

                            let! reloadBetween = probe (laterBusy ()) (fun () -> true)

                            let mapAll (rs: range list) = rs |> List.map (expectedAt inverse)

                            match feature with
                            | References ->
                                match! References.fromLastState uses shapes (fun () -> false) (fun () -> true) projectsNow graph file t.Line t.Character CancellationToken.None |> wait with
                                | LastState.Outcome.Refused r -> countRefusal r
                                | LastState.Outcome.Settled -> failures.Add $"%s{where}: settled while pending"
                                | LastState.Outcome.Answered References.Answer.Moved -> failures.Add $"%s{where}: Moved from the last state"
                                | LastState.Outcome.Answered(References.Answer.Found ours) ->
                                    answered <- answered + 1

                                    if reloadBetween <> "Reloading" then
                                        failures.Add $"%s{where}: a reload starting after the first turn gives %s{reloadBetween}"

                                    if (ours |> List.map (fun (struct (r, _)) -> projectOf r.FileName) |> List.distinct |> List.length) > 1 then
                                        crossing <- crossing + 1

                                    match old () with
                                    | None -> failures.Add $"%s{where}: answered, but the edit changed the cursor's line"
                                    | Some at when not (oldUses.ContainsKey at) -> failures.Add $"%s{where}: answered, but nothing was answered at the old place"
                                    | Some at ->
                                        let before = oldUses[at]
                                        let moved = before |> List.map (fun (struct (r, _)) -> r) |> mapAll

                                        if before |> List.exists (fun (struct (r, _)) -> inEditedRow r) then
                                            failures.Add $"%s{where}: answered, but a use is in a row the edit changed"
                                        elif moved |> List.exists Option.isNone then
                                            failures.Add $"%s{where}: answered, but the edit changed the line of a use"
                                        else
                                            let expected = List.map2 (fun (struct (_, d)) (m: range option) -> struct (m.Value, d)) before moved |> usesSet

                                            if usesSet ours <> expected then
                                                failures.Add $"%s{where}: ours %A{usesSet ours}, the old answer moved %A{expected}"

                                            // The old answer against FCS on the old text: the References grid allows none here
                                            match fcsUses.TryGetValue at with
                                            | true, fcs when fcs = usesSet before -> ()
                                            | _ ->
                                                limits <- limits + 1
                                                failures.Add $"%s{where}: the old answer differs from FCS"
                            | Definition ->
                                match! Definition.fromLastState definitions shapes (fun () -> false) (fun () -> true) projectsNow graph file t.Line t.Character CancellationToken.None |> wait with
                                | LastState.Outcome.Refused r -> countRefusal r
                                | LastState.Outcome.Settled -> failures.Add $"%s{where}: settled while pending"
                                | LastState.Outcome.Answered ours ->
                                    answered <- answered + 1

                                    if reloadBetween <> "Reloading" then
                                        failures.Add $"%s{where}: a reload starting after the first turn gives %s{reloadBetween}"

                                    match old () with
                                    | None -> failures.Add $"%s{where}: answered, but the edit changed the cursor's line"
                                    | Some at when not (oldDefinitions.ContainsKey at) -> failures.Add $"%s{where}: answered, but nothing was answered at the old place"
                                    | Some at ->
                                        let before = oldDefinitions[at]

                                        match before |> Option.map (expectedAt inverse) with
                                        | _ when before |> Option.exists inEditedRow -> failures.Add $"%s{where}: answered, but the definition is in a row the edit changed"
                                        | Some None -> failures.Add $"%s{where}: answered, but the edit changed the definition's line"
                                        | moved ->
                                            let expected = moved |> Option.flatten |> Option.map showRange

                                            if (ours |> Option.map showRange) <> expected then
                                                failures.Add $"%s{where}: ours %A{ours |> Option.map showRange}, the old answer moved %A{expected}"

                                            // The Definition grid's limits: nothing where FCS answers, or a signature location
                                            match fcsDefinitions.TryGetValue at with
                                            | true, fcs when (fcs |> Option.map showRange) = (before |> Option.map showRange) -> ()
                                            | true, fcs ->
                                                limits <- limits + 1
                                                let inSignature (r: range option) = r |> Option.exists (fun r -> r.FileName.EndsWith ".fsi")

                                                if not (before.IsNone || inSignature before || inSignature fcs) then
                                                    failures.Add $"%s{where}: the old answer %A{before |> Option.map showRange} differs from FCS %A{fcs |> Option.map showRange} by no Definition grid limit"
                                            | _ -> limits <- limits + 1
                finally
                    gate.Set()

                for g in graphs do
                    let! _ = g.AllDiagnostics() |> wait
                    ()

                return [ () ]
            }

        let! _ =
            match shape.App with
            | None ->
                run promote { Name = shape.Name; Files = shape.Files; Steps = [] } (fun state ->
                    atState state [ state.Graph ] [ for f in state.Implementations -> state.Graph, f ] state.Reference)
            | Some app ->
                runReferencing { Name = shape.Name; Files = shape.Files; Steps = [] } app (fun state appGraph appFile ->
                    atState state [ state.Graph; appGraph ] ([ for f in state.Implementations -> state.Graph, f ] @ [ appGraph, appFile ]) state.Combined)

        return
            { Cases = cases
              Answered = answered
              Refused = refused |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
              Limits = limits
              Crossing = crossing
              Failures = List.ofSeq failures }
    }

let private expectGrid (promote: bool) (feature: Feature) (shape: Shape) (c: Counts) =
    let refusals = c.Refused |> Map.toList |> List.map (fun (r, k) -> $"%A{r} %d{k}") |> String.concat ", "
    let mode = if promote then "by declaration" else "file level"
    printfn "last-state %A grid, %s (%s): %d cases, %d answered (%d old answers at a limit, %d crossing projects), refused: %s" feature shape.Name mode c.Cases c.Answered c.Limits c.Crossing refusals
    let shown = c.Failures |> List.truncate 12 |> String.concat "\n  "
    Expect.isTrue c.Failures.IsEmpty $"%d{c.Failures.Length} of %d{c.Cases} failed:\n  %s{shown}"
    Expect.isGreaterThan c.Cases 0 "cases"
    Expect.equal (c.Refused.TryFind Refusal.Reloading) (Some c.Cases) "every case refuses while busy"

    let refusalsExpected, answers =
        match feature, promote with
        | References, true -> shape.Refusals, shape.Answers
        | Definition, true -> shape.Definition
        | References, false -> shape.FileReferences
        | Definition, false -> shape.FileDefinition

    for r in refusalsExpected do
        Expect.isTrue (c.Refused.ContainsKey r) $"%s{shape.Name} (%s{mode}) refuses with %A{r} somewhere"

    if answers then
        Expect.isGreaterThan c.Answered 0 $"%s{shape.Name} (%s{mode}): some token answers from the last state"
    else
        Expect.equal c.Answered 0 $"%s{shape.Name} (%s{mode}): nothing answers from the last state"

    if feature = References && shape.CrossesProjects then
        Expect.isGreaterThan c.Crossing 0 $"%s{shape.Name}: some answer holds uses in both projects"


/// The 0-based line and character of the first `needle` in the text
let private find (text: string) (needle: string) =
    let index = text.IndexOf(needle, StringComparison.Ordinal)
    let before = text.Substring(0, index)
    before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1)

let private fileNamed (state: State) (name: string) = state.Files |> List.find (fun f -> Path.GetFileName f = name)

let private scopeOf (graphs: ProjectGraph list) (owners: string -> ProjectGraph list) (filesOf: ProjectGraph -> string list) : References.Scope =
    { OwnersOf = owners
      ReferencingProjects = fun g -> graphs |> List.filter (fun o -> not (obj.ReferenceEquals(o, g)) && o.References |> List.exists (fun r -> obj.ReferenceEquals(r, g)))
      Projects = fun () -> graphs
      FilesOf = filesOf
      IsLoading = fun () -> false }

let private shadowA = n "module Lib.A\n\nlet scale x = x * 2\n"
let private shadowB = n "module Lib.B\n\nlet other = 1\n"
let private shadowC = n "module Lib.C\n\nopen Lib.A\nopen Lib.B\n\nlet used = scale 3\n\nlet extra = 1\n"

let private extraTests =
    [ testTask "a definition held from before a later state was published refuses: B shadows A's scale and settles" {
          let! results =
              run true { Name = "shadowing"; Files = [ "A.fs", shadowA; "B.fs", shadowB; "C.fs", shadowC ]; Steps = [] } (fun state ->
                  task {
                      let shapes = Hover.LastAnswers()
                      let definitions = Definition.LastAnswers()
                      let projectsNow () = [ state.Graph ]
                      let c = fileNamed state "C.fs"
                      let b = fileNamed state "B.fs"
                      let line, character = find shadowC "scale 3"
                      let! first = Definition.answer definitions shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) projectsNow state.OwnersOf state.Graph c line character CancellationToken.None |> wait
                      Expect.equal (first |> Option.map (fun r -> Path.GetFileName r.FileName)) (Some "A.fs") "scale is A's"
                      // B gains a scale that C's `open Lib.B` makes the one meant, and the state settles with no request
                      state.Graph.SetText(b, shadowB + "\nlet scale x = x + 1\n") |> ignore
                      let! _ = state.Graph.AllDiagnostics() |> wait
                      let gate = hold [ state.Graph ]
                      // C pends on B, its own text the same
                      state.Graph.SetText(b, shadowB + "\nlet scale x = x + 1\n\nlet more = 2\n") |> ignore

                      try
                          let! last = Definition.fromLastState definitions shapes (fun () -> false) (fun () -> true) projectsNow state.Graph c line character CancellationToken.None |> wait

                          match last with
                          | LastState.Outcome.Answered found -> failtest $"answered %A{found |> Option.map (fun r -> r.FileName)} from the state before the last"
                          | LastState.Outcome.Refused Refusal.Moved -> ()
                          | other -> failtest $"B published since: the held answer isn't the last state: %A{other}"
                      finally
                          gate.Set()

                      let! settled = Definition.answer definitions shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) projectsNow state.OwnersOf state.Graph c line character CancellationToken.None |> wait
                      Expect.equal (settled |> Option.map (fun r -> Path.GetFileName r.FileName)) (Some "B.fs") "settled, scale is B's"
                      return [ () ]
                  })

          Expect.equal (List.length results) 1 "ran"
      }

      testTask "references held from before a restart refuse once it has published" {
          let app = n "module App.Main\n\nlet fromApp = 1\n\nlet again = fromApp + 1\n"

          let! results =
              runReferencing { Name = "restart"; Files = [ "A.fs", libA ]; Steps = [] } app (fun state appGraph appFile ->
                  task {
                      let shapes = Hover.LastAnswers()
                      let uses = References.LastAnswers()
                      let graphs = [ state.Graph; appGraph ]
                      let projectsNow () = graphs
                      let scope = scopeOf graphs state.OwnersOf (fun g -> if obj.ReferenceEquals(g, appGraph) then [ appFile ] else state.Files)
                      let line, character = find app "fromApp + 1"
                      let! first = References.answer uses shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) scope appGraph appFile line character CancellationToken.None |> wait
                      Expect.equal (match first with References.Answer.Found u -> u.Length | _ -> 0) 2 "fromApp's two uses"
                      let before = appGraph.Restarts
                      let a = state.Files.Head
                      // Lib's signature changes: App starts again on it, at the same texts, and settles
                      state.Graph.SetText(a, libA + "\nlet added = 0\n") |> ignore
                      let! _ = state.Graph.AllDiagnostics() |> wait
                      let! _ = appGraph.AllDiagnostics() |> wait
                      Expect.isGreaterThan appGraph.Restarts before "App restarted"
                      let gate = hold graphs
                      state.Graph.SetText(a, libA + "\nlet added = 1\n") |> ignore

                      try
                          let! last = References.fromLastState uses shapes (fun () -> false) (fun () -> true) projectsNow appGraph appFile line character CancellationToken.None |> wait

                          match last with
                          | LastState.Outcome.Answered _ -> failtest "answered from the state before the restart"
                          | LastState.Outcome.Refused Refusal.Moved -> ()
                          | other -> failtest $"App restarted since: the held answer isn't the last state: %A{other}"
                      finally
                          gate.Set()

                      return [ () ]
                  })

          Expect.equal (List.length results) 1 "ran"
      }

      testTask "references held from before another project was loaded refuse: App2 uses Lib's kept and publishes" {
          let! results =
              runReferencing { Name = "App2"; Files = [ "A.fs", libA ]; Steps = [] } appText (fun state appGraph appFile ->
                  task {
                      let shapes = Hover.LastAnswers()
                      let uses = References.LastAnswers()
                      let loaded = ResizeArray [ state.Graph; appGraph ]
                      let projectsNow () = List.ofSeq loaded
                      let filesOf (g: ProjectGraph) = if obj.ReferenceEquals(g, state.Graph) then state.Files else g.FilesOf |> List.ofArray
                      let scope = scopeOf (List.ofSeq loaded) state.OwnersOf filesOf
                      let a = state.Files.Head
                      let line, character = find libA "kept = edited"
                      let! first = References.answer uses shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) scope state.Graph a line character CancellationToken.None |> wait
                      Expect.isTrue (match first with References.Answer.Found u -> not u.IsEmpty | _ -> false) "kept is used"
                      // App2, loaded later, uses kept too, and settles: the last state has its uses
                      let app2Dir = Directory.CreateDirectory(Path.Combine(Path.GetDirectoryName(Path.GetDirectoryName appFile), "App2")).FullName
                      let app2File = Path.Combine(app2Dir, "App2.fs")
                      File.WriteAllText(app2File, n "module App2.Main\n\nopen Lib.A\n\nlet two = kept + 2\n")
                      let spec = appGraph.Spec

                      let app2Spec =
                          { spec with
                              ProjectFile = Path.Combine(app2Dir, "App2.fsproj")
                              SourceFiles = [ app2File ]
                              OtherOptions =
                                  spec.OtherOptions
                                  |> List.map (fun o -> if o.StartsWith "--out:" then "--out:" + Path.Combine(app2Dir, "App2.dll") else o) }

                      let! (app2: ProjectGraph) = ProjectGraph.Create(appGraph.Host, app2Spec, [ state.Graph ])

                      try
                          let! _ = app2.AllDiagnostics() |> wait
                          loaded.Add app2
                          let gate = hold [ state.Graph; appGraph; app2 ]
                          state.Graph.SetText(a, libA.Replace("let kept =", "\nlet kept =")) |> ignore

                          try
                              let! last = References.fromLastState uses shapes (fun () -> false) (fun () -> true) projectsNow state.Graph a (line + 1) character CancellationToken.None |> wait

                              match last with
                              | LastState.Outcome.Answered found -> failtest $"answered without App2's use: %A{found}"
                              | LastState.Outcome.Refused Refusal.Moved -> ()
                              | other -> failtest $"a project loaded since: %A{other}"
                          finally
                              gate.Set()
                      finally
                          app2.Dispose()

                      return [ () ]
                  })

          Expect.equal (List.length results) 1 "ran"
      }

      testTask "a range whose two ends map through different rows refuses" {
          let text = n "module Lib.R\n\nlet first = 1\n\nlet second = first\n"

          let! results =
              run true { Name = "across rows"; Files = [ "R.fs", text ]; Steps = [] } (fun state ->
                  task {
                      let shapes = Hover.LastAnswers()
                      let uses = References.LastAnswers()
                      let r = state.Files.Head
                      let projectsNow () = [ state.Graph ]
                      let scope = scopeOf [ state.Graph ] state.OwnersOf (fun _ -> state.Files)
                      let line, character = find text "first = 1"
                      let! _ = References.answer uses shapes (fun () -> false) (fun () -> false) (fun () -> Task.Delay Timeout.Infinite) scope state.Graph r line character CancellationToken.None |> wait
                      let! latest = state.Graph.LatestText(r, [||], (fun () -> false), CancellationToken.None) |> wait
                      let key = LastState.keyOf r latest.View.Version line (Hover.lookupOf text line character).Value
                      let held = (uses.Find(state.Graph, key)).Value
                      // From `first` in its row to the end of `second`'s: no use is so, but each end alone maps
                      let across = Range.mkRange r (Position.mkPos 3 4) (Position.mkPos 5 18)
                      uses.Add(key, { held with Answer = References.Answer.Found [ struct (across, false) ] })
                      let gate = hold [ state.Graph ]
                      state.Graph.SetText(r, text.Replace("let first = 1\n", "let extra = 0\nlet first = 1\n")) |> ignore

                      try
                          let! last = References.fromLastState uses shapes (fun () -> false) (fun () -> true) projectsNow state.Graph r (line + 1) character CancellationToken.None |> wait

                          match last with
                          | LastState.Outcome.Answered a -> failtest $"answered %A{a}"
                          | LastState.Outcome.Refused Refusal.Edited -> ()
                          | other -> failtest $"two rows: refused: %A{other}"
                      finally
                          gate.Set()

                      return [ () ]
                  })

          Expect.equal (List.length results) 1 "ran"
      } ]

[<Tests>]
let tests =
    testSequenced
    <| testList
        "LastStateUsesGrid"
        [ for promote in [ true; false ] do
              for feature in [ References; Definition ] do
                  // App's project is checked by declaration only (TokenGrid.runReferencing)
                  for shape in shapes |> List.filter (fun s -> promote || s.App.IsNone) do
                      let mode = if promote then "by declaration" else "file level"

                      yield
                          testTask $"%A{feature} from the last state at every token after an edit: %s{shape.Name} (%s{mode})" {
                              let! c = grid promote feature shape
                              expectGrid promote feature shape c
                          }

          yield! extraTests ]
