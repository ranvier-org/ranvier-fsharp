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
      /// Refusals that must occur, by declaration
      Refusals: Refusal list
      /// Whether some references answer must come from the last state (false: none may)
      Answers: bool
      /// When given, App.fs in a project referencing these files, which the edits never touch
      App: string option }

let private shape name files edit refusals answers =
    { Name = name
      Files = files
      Edit = edit
      Refusals = refusals
      Answers = answers
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
    shape "#line above" [ "L.fs", text ] [ "L.fs", [ "let two = ", "# 40 \"elsewhere.fs\"\nlet two = " ] ] [ Refusal.Directives ] false

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

/// A signature file edited: every answer with a use in it waits
let private signatureEdited =
    shape
        "a signature file edited"
        [ "S.fsi", signatureText; "S.fs", signedText; "U.fs", userText ]
        [ "S.fsi", [ "/// One", "/// The one" ] ]
        [ Refusal.Edited ]
        false

let private appText = n "module App.Main\n\nopen Lib.A\n\nlet fromApp = scale 5 kept\n\nlet again = kept + baseValue\n"

/// Uses in App, another project, while Lib's file is edited above them
let private otherProject =
    { shape
          "uses in another project"
          [ "A.fs", libA ]
          [ "A.fs", [ "let kept =", "let added = baseValue\n\nlet kept =" ] ]
          [ Refusal.NewRow ]
          true with
        App = Some appText }

let private shapes = [ rows; lineAbove; ifAbove; ordinals; lineDirective; nestedRec; signature; signatureEdited; otherProject ]

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
let private grid (feature: Feature) (shape: Shape) : Task<Counts> =
    task {
        let failures = ResizeArray<string>()
        let refused = Dictionary<Refusal, int>()
        let mutable cases = 0
        let mutable answered = 0
        let mutable limits = 0
        let countRefusal r = refused[r] <- (match refused.TryGetValue r with | true, k -> k + 1 | _ -> 1)

        let atState (state: State) (graphs: ProjectGraph list) (asking: (ProjectGraph * string) list) (check: string -> Task<FSharpCheckFileResults>) =
            task {
                let shapes = Hover.LastAnswers()
                let uses = References.LastAnswers()
                let definitions = Definition.LastAnswers()
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
                            let! found = References.answer uses shapes (fun () -> false) (fun () -> false) scope graph file t.Line t.Character CancellationToken.None |> wait

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
                            let! found = Definition.answer definitions shapes (fun () -> false) (fun () -> false) state.OwnersOf graph file t.Line t.Character CancellationToken.None |> wait
                            oldDefinitions[at] <- found

                            fcsDefinitions[at] <-
                                match checks[file].GetDeclarationLocation(t.Line + 1, t.EndColumn, t.LineText, t.Names, preferFlag = true) with
                                | FindDeclResult.DeclFound r -> Some r
                                | _ -> None

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
                                        match! References.fromLastState uses shapes (fun () -> true) (fun () -> true) graph file t.Line t.Character CancellationToken.None |> wait with
                                        | LastState.Outcome.Refused r -> return Some r
                                        | _ -> return None
                                    | Definition ->
                                        match! Definition.fromLastState definitions shapes (fun () -> true) (fun () -> true) graph file t.Line t.Character CancellationToken.None |> wait with
                                        | LastState.Outcome.Refused r -> return Some r
                                        | _ -> return None
                                }

                            match! busy () with
                            | Some Refusal.Reloading -> countRefusal Refusal.Reloading
                            | other -> failures.Add $"%s{where} while busy: %A{other}"

                            let mapAll (rs: range list) = rs |> List.map (expectedAt inverse)

                            match feature with
                            | References ->
                                match! References.fromLastState uses shapes (fun () -> false) (fun () -> true) graph file t.Line t.Character CancellationToken.None |> wait with
                                | LastState.Outcome.Refused r -> countRefusal r
                                | LastState.Outcome.Settled -> failures.Add $"%s{where}: settled while pending"
                                | LastState.Outcome.Answered References.Answer.Moved -> failures.Add $"%s{where}: Moved from the last state"
                                | LastState.Outcome.Answered(References.Answer.Found ours) ->
                                    answered <- answered + 1

                                    match old () with
                                    | None -> failures.Add $"%s{where}: answered, but the edit changed the cursor's line"
                                    | Some at when not (oldUses.ContainsKey at) -> failures.Add $"%s{where}: answered, but nothing was answered at the old place"
                                    | Some at ->
                                        let before = oldUses[at]
                                        let moved = before |> List.map (fun (struct (r, _)) -> r) |> mapAll

                                        if moved |> List.exists Option.isNone then
                                            failures.Add $"%s{where}: answered, but the edit changed the line of a use"
                                        else
                                            let expected = List.map2 (fun (struct (_, d)) (m: range option) -> struct (m.Value, d)) before moved |> usesSet

                                            if usesSet ours <> expected then
                                                failures.Add $"%s{where}: ours %A{usesSet ours}, the old answer moved %A{expected}"

                                            // The old answer against FCS on the old text
                                            match fcsUses.TryGetValue at with
                                            | true, fcs when fcs = usesSet before -> ()
                                            | _ -> limits <- limits + 1
                            | Definition ->
                                match! Definition.fromLastState definitions shapes (fun () -> false) (fun () -> true) graph file t.Line t.Character CancellationToken.None |> wait with
                                | LastState.Outcome.Refused r -> countRefusal r
                                | LastState.Outcome.Settled -> failures.Add $"%s{where}: settled while pending"
                                | LastState.Outcome.Answered ours ->
                                    answered <- answered + 1

                                    match old () with
                                    | None -> failures.Add $"%s{where}: answered, but the edit changed the cursor's line"
                                    | Some at when not (oldDefinitions.ContainsKey at) -> failures.Add $"%s{where}: answered, but nothing was answered at the old place"
                                    | Some at ->
                                        let before = oldDefinitions[at]

                                        match before |> Option.map (expectedAt inverse) with
                                        | Some None -> failures.Add $"%s{where}: answered, but the edit changed the definition's line"
                                        | moved ->
                                            let expected = moved |> Option.flatten |> Option.map showRange

                                            if (ours |> Option.map showRange) <> expected then
                                                failures.Add $"%s{where}: ours %A{ours |> Option.map showRange}, the old answer moved %A{expected}"

                                            match fcsDefinitions.TryGetValue at with
                                            | true, fcs when (fcs |> Option.map showRange) = (before |> Option.map showRange) -> ()
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
                run true { Name = shape.Name; Files = shape.Files; Steps = [] } (fun state ->
                    atState state [ state.Graph ] [ for f in state.Implementations -> state.Graph, f ] state.Reference)
            | Some app ->
                runReferencing { Name = shape.Name; Files = shape.Files; Steps = [] } app (fun state appGraph appFile ->
                    atState state [ state.Graph; appGraph ] ([ for f in state.Implementations -> state.Graph, f ] @ [ appGraph, appFile ]) state.Combined)

        return
            { Cases = cases
              Answered = answered
              Refused = refused |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
              Limits = limits
              Failures = List.ofSeq failures }
    }

let private expectGrid (feature: Feature) (shape: Shape) (c: Counts) =
    let refusals = c.Refused |> Map.toList |> List.map (fun (r, k) -> $"%A{r} %d{k}") |> String.concat ", "
    printfn "last-state %A grid, %s: %d cases, %d answered (%d old answers at a limit), refused: %s" feature shape.Name c.Cases c.Answered c.Limits refusals
    let shown = c.Failures |> List.truncate 12 |> String.concat "\n  "
    Expect.isTrue c.Failures.IsEmpty $"%d{c.Failures.Length} of %d{c.Cases} failed:\n  %s{shown}"
    Expect.isGreaterThan c.Cases 0 "cases"
    Expect.equal (c.Refused.TryFind Refusal.Reloading) (Some c.Cases) "every case refuses while busy"

    if feature = References then
        for r in shape.Refusals do
            Expect.isTrue (c.Refused.ContainsKey r) $"%s{shape.Name} refuses with %A{r} somewhere"

        if shape.Answers then
            Expect.isGreaterThan c.Answered 0 $"%s{shape.Name}: some token answers from the last state"
        else
            Expect.equal c.Answered 0 $"%s{shape.Name}: nothing answers from the last state"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "LastStateUsesGrid"
        [ for feature in [ References; Definition ] do
              for shape in shapes do
                  yield
                      testTask $"%A{feature} from the last state at every token after an edit: %s{shape.Name}" {
                          let! c = grid feature shape
                          expectGrid feature shape c
                      } ]
