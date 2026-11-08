module Rfs.Checker.Tests.OracleFuzzTests

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Checker.Tests.Deps

/// One random edit of a file's text, with what it did; None when the text offers nothing to edit this way.
type private Mutation = Random -> string -> (string * string) option

let private pick (random: Random) (items: 'T[]) = items[random.Next items.Length]

/// The blocks of declarations separated by blank lines; the first holds the module or namespace header.
let private blocks (text: string) = text.Replace("\r\n", "\n").TrimEnd('\n').Split("\n\n")
let private join (parts: string seq) = String.Join("\n\n", parts) + "\n"

let private indentOf (block: string) = block.Length - block.TrimStart(' ').Length

let private replaceMatch (text: string) (m: Match) (replacement: string) =
    text.Substring(0, m.Index) + replacement + text.Substring(m.Index + m.Length)

let private literal: Mutation =
    fun random text ->
        let body = text.IndexOf('\n')
        let matches = [| for m in Regex.Matches(text, @"(?<![\w.'""])\d+(?![\w.])") do if m.Index > body then m |]

        if matches.Length = 0 then
            None
        else
            let m = pick random matches
            let value = string (random.Next 100)
            Some(replaceMatch text m value, $"literal %s{m.Value} -> %s{value}")

let private returnType: Mutation =
    fun random text ->
        let matches = [| for m in Regex.Matches(text, @"\) : (int|int64|string) =") -> m |]

        if matches.Length = 0 then
            None
        else
            let m = pick random matches

            let next =
                match m.Groups[1].Value with
                | "int" -> "int64"
                | "int64" -> "string"
                | _ -> "int"

            Some(replaceMatch text m $") : %s{next} =", $"return type %s{m.Groups[1].Value} -> %s{next}")

/// A name of a value or function the project defines, from the texts.
let private names (texts: string seq) =
    texts
    |> Seq.collect (fun t -> Regex.Matches(t, @"\blet (?:private )?(\w+)") |> Seq.map _.Groups[1].Value)
    |> Seq.filter (fun n -> n <> "rec")
    |> Seq.distinct
    |> Array.ofSeq

let private linesOf (text: string) = text.Replace("\r\n", "\n").Split('\n')
let private unlines (ls: string seq) = String.Join("\n", ls)

/// The blank lines between declarations, each with the indentation of the line after it. A line written on one keeps
/// every declaration where it was, so only dependencies, not a moved range, make declarations re-check.
let private blankSlots (ls: string[]) =
    [| for i in 1 .. ls.Length - 2 do
           if ls[i].Trim() = "" && ls[i + 1].Trim() <> "" then
               i, indentOf ls[i + 1] |]

/// Writes a line (given its indentation) on a blank line, or inserts it as a new block, which moves what follows.
let private place (random: Random) (text: string) (line: string -> string) =
    let ls = linesOf text
    let slots = blankSlots ls

    if slots.Length > 0 && random.Next 2 = 0 then
        let i, indent = pick random slots
        let written = line (String(' ', indent))
        unlines (Array.updateAt i written ls), $"write `%s{written.Trim()}` on blank line %d{i + 1}"
    else
        let parts = blocks text
        let at = 1 + random.Next parts.Length
        let written = line (String(' ', indentOf (if at < parts.Length then parts[at] else parts[parts.Length - 1])))
        join (Array.insertAt at written parts), $"insert `%s{written.Trim()}` as block %d{at}"

/// Blanks a line out, which keeps every other declaration where it was, or removes it, which moves what follows.
let private takeOut (random: Random) (text: string) (candidate: string[] -> int -> bool) =
    let ls = linesOf text
    let candidates = [| for i in 1 .. ls.Length - 1 do if candidate ls i then i |]

    if candidates.Length = 0 then
        None
    else
        let i = pick random candidates

        if random.Next 2 = 0 then
            Some(unlines (Array.updateAt i "" ls), $"blank `%s{ls[i].Trim()}` out")
        else
            Some(unlines (Array.removeAt i ls), $"remove `%s{ls[i].Trim()}`")

let private addDeclaration (pool: string[]) (counter: int ref) : Mutation =
    fun random text ->
        counter.Value <- counter.Value + 1
        let name = $"added%d{counter.Value}"

        let body =
            match random.Next 3 with
            | 0 -> string (random.Next 100)
            | 1 -> pick random pool
            | _ -> "\"text\""

        Some(place random text (fun indent -> $"%s{indent}let %s{name} = %s{body}"))

/// Takes out a declaration that fits on one line: one the line after it does not continue.
let private removeDeclaration: Mutation =
    fun random text ->
        takeOut random text (fun ls i ->
            ls[i].TrimStart().StartsWith "let "
            && (i + 1 >= ls.Length || ls[i + 1].Trim() = "" || indentOf ls[i + 1] <= indentOf ls[i]))

let private depsOpenTargets = [| "System"; "Deps.A"; "Deps.B"; "Deps.B.Util"; "System.Collections.Generic" |]

let private chainsOpenTargets = [| "System"; "Chains.A"; "Chains.B"; "Chains.C"; "Chains.D"; "System.Collections.Generic" |]

/// Signature files whose implementation a later file's setup can leave out (Task 3b fix round 3)
let private sigShadowProject = Path.Combine(Path.GetDirectoryName depsDir, "SigShadow", "SigShadow.fsproj")
let private sigQualNameProject = Path.Combine(Path.GetDirectoryName depsDir, "SigQualName", "SigQualName.fsproj")
let private sigShadowOpenTargets = [| "System"; "A"; "B" |]
let private sigQualNameOpenTargets = [| "System"; "A"; "Y" |]
/// An implementation file whose qualified name differs from its signature file's (T4-C3)
let private sigUnmatchedProject = Path.Combine(Path.GetDirectoryName depsDir, "SigUnmatched", "SigUnmatched.fsproj")
let private sigUnmatchedOpenTargets = [| "System"; "A"; "Ab"; "W" |]
/// A signature file whose conformance check overwrites what A.fs's later declarations use (Task 6 fix round 1)
let private sigConformProject = Path.Combine(Path.GetDirectoryName depsDir, "SigConform", "SigConform.fsproj")
let private sigConformOpenTargets = [| "System"; "SigConform.A" |]
/// Two signature pairs, where whether X.fs fails depends on whether W.fs does (S1-R2)
let private sigChain2Project = Path.Combine(Path.GetDirectoryName depsDir, "SigChain2", "SigChain2.fsproj")
let private sigChain2OpenTargets = [| "System"; "V"; "W"; "A"; "Y" |]

let private addOpen (openTargets: string[]) : Mutation =
    fun random text -> Some(place random text (fun indent -> $"%s{indent}open %s{pick random openTargets}"))

let private removeOpen: Mutation =
    fun random text -> takeOut random text (fun ls i -> Regex.IsMatch(ls[i], @"^\s*open \S+\s*$"))

/// Renames one use of a defined name (not its definition) to another defined name, or to an undefined one.
let private renameUse (pool: string[]) : Mutation =
    fun random text ->
        let body = text.IndexOf('\n')

        let uses =
            [| for name in pool do
                   for m in Regex.Matches(text, $@"(?<![\w.]|let |let private |member \w+\.|type )\b%s{name}\b") do
                       if m.Index > body then m |]

        if uses.Length = 0 then
            None
        else
            let m = pick random uses
            let other = if random.Next 5 = 0 then "undefinedName" else pick random pool
            Some(replaceMatch text m other, $"rename a use of %s{m.Value} -> %s{other}")

/// Adds a declaration that redefines a name the project already defines: later uses of it now resolve to it.
let private shadow (pool: string[]) : Mutation =
    fun random text ->
        let name = pick random pool
        let body = if random.Next 2 = 0 then string (random.Next 100) else "\"text\""
        Some(place random text (fun indent -> $"%s{indent}let %s{name} = %s{body}"))

/// Adds `let refN = ref []` with a user, or takes one such pair out: a declaration that starts or stops leaving
/// inference variables for a later one to solve (R12).
let private freeTypars (counter: int ref) : Mutation =
    fun random text ->
        let existing = [| for m in Regex.Matches(text, @"let (fresh\d+) = ref \[\]") -> m.Groups[1].Value |]

        if existing.Length > 0 && random.Next 2 = 0 then
            let name = pick random existing
            let ls = linesOf text
            let kept = ls |> Array.filter (fun l -> not (Regex.IsMatch(l, $@"\b%s{name}\b")))
            Some(unlines kept, $"remove %s{name} and its user")
        else
            counter.Value <- counter.Value + 1
            let name = $"fresh%d{counter.Value}"
            let value = if random.Next 2 = 0 then string (random.Next 100) else "\"s\""
            let parts = blocks text
            let at = 1 + random.Next parts.Length
            let indent = String(' ', indentOf (if at < parts.Length then parts[at] else parts[parts.Length - 1]))

            let added =
                [| $"%s{indent}let %s{name} = ref []"; $"%s{indent}let use%s{name} () = %s{name}.Value <- [ %s{value} ]" |]

            Some(join (Array.insertAt (at + 1) added[1] (Array.insertAt at added[0] parts)), $"add %s{name} = ref [] with a user")

/// C1 of the Task 5 review: a body whose type error makes a file-end default fail, before a generic lambda left
/// unchanged, whose default then is or is not applied. Adds the pair, or toggles the error and its fix.
let private failingDefault: Mutation =
    fun random text ->
        if text.TrimStart().StartsWith "namespace" then
            None
        elif text.Contains "3 * scaleDefaults * r" then
            Some(text.Replace("3 * scaleDefaults * r", "3 * scaleDefaults r"), "fix areaDefaults")
        elif text.Contains "3 * scaleDefaults r" then
            Some(text.Replace("3 * scaleDefaults r", "3 * scaleDefaults * r"), "break areaDefaults")
        else
            // A body, a float use, and only a lambda parameter's type (C1-R)
            let lambda = pick random [| "x + x"; "x + 1.0"; "()" |]

            let added =
                $"\n\nlet scaleDefaults (x: int) : int = x * 2\n\nlet areaDefaults (r: int) : int = 3 * scaleDefaults * r\n\nlet gDefaults () =\n    (fun x -> %s{lambda})\n    1\n"

            Some(text.TrimEnd('\n') + added, $"add a failing default before `fun x -> %s{lambda}`")
/// Renames a nested module's header (Util <-> Util2) or toggles `internal` on it, keeping every line in place.
let private moduleHeader: Mutation =
    fun random text ->
        let ls = linesOf text
        let headers = [| for i in 1 .. ls.Length - 1 do if Regex.IsMatch(ls[i], @"^\s*module (internal )?\w+ =\s*$") then i |]

        if headers.Length = 0 then
            None
        else
            let i = pick random headers
            let line = ls[i]

            let edited =
                if random.Next 2 = 0 then
                    if line.Contains "module internal " then line.Replace("module internal ", "module ")
                    else line.Replace("module ", "module internal ")
                else
                    Regex.Replace(line, @"(\w+) =\s*$", fun m -> (if m.Groups[1].Value.EndsWith "2" then m.Groups[1].Value.TrimEnd('2') else m.Groups[1].Value + "2") + " =")

            Some(unlines (Array.updateAt i edited ls), $"header `%s{line.Trim()}` -> `%s{edited.Trim()}`")

/// Renames the file's top-level module between its name and the name with "b" added (SigUnmatched's X.fs: `module A`,
/// which X.fsi matches, and `module Ab`, which it does not), keeping every line in place.
let private topHeader: Mutation =
    fun _ text ->
        let ls = linesOf text

        match ls |> Array.tryFindIndex (fun l -> Regex.IsMatch(l, @"^module \w+\s*$")) with
        | Some i ->
            let line = ls[i].TrimEnd()
            let edited = if line.EndsWith "b" then line.TrimEnd('b') else line + "b"
            Some(unlines (Array.updateAt i edited ls), $"top-level header `%s{line}` -> `%s{edited}`")
        | None -> None

/// Toggles [<AutoOpen>] on a module, on the line above its header, keeping every other line in place.
let private autoOpen: Mutation =
    fun random text ->
        let ls = linesOf text

        let slots =
            [| for i in 1 .. ls.Length - 2 do
                   if ls[i].Trim() = "[<AutoOpen>]" || (ls[i].Trim() = "" && Regex.IsMatch(ls[i + 1], @"^\s*module (internal )?\w+ =\s*$")) then
                       i |]

        if slots.Length = 0 then
            None
        else
            let i = pick random slots

            if ls[i].Trim() = "[<AutoOpen>]" then
                Some(unlines (Array.updateAt i "" ls), "remove [<AutoOpen>]")
            else
                Some(unlines (Array.updateAt i (String(' ', indentOf ls[i + 1]) + "[<AutoOpen>]") ls), "add [<AutoOpen>]")

/// Adds a member to an augmentation (`type T with`) or takes an added one out.
let private augmentationMember (counter: int ref) : Mutation =
    fun random text ->
        let ls = linesOf text
        let added = [| for i in 0 .. ls.Length - 1 do if Regex.IsMatch(ls[i], @"^\s*member \w+\.Extra\d+ = ") then i |]
        let augmentations = [| for i in 0 .. ls.Length - 1 do if Regex.IsMatch(ls[i], @"^\s*type \w+ with\s*$") then i |]

        if added.Length > 0 && random.Next 2 = 0 then
            let i = pick random added
            Some(unlines (Array.removeAt i ls), $"remove `%s{ls[i].Trim()}`")
        elif augmentations.Length > 0 then
            let i = pick random augmentations
            counter.Value <- counter.Value + 1
            let line = $"%s{String(' ', indentOf ls[i] + 4)}member c.Extra%d{counter.Value} = %d{random.Next 100}"
            Some(unlines (Array.insertAt (i + 1) line ls), $"add `%s{line.Trim()}`")
        else
            None

/// Doubles a space on the line a declaration starts on, after its start: its text changes, its start does not (W1).
let private sameLine: Mutation =
    fun random text ->
        let ls = linesOf text

        let spots =
            [| for i in 1 .. ls.Length - 1 do
                   if Regex.IsMatch(ls[i], @"^\s*let ") then
                       for m in Regex.Matches(ls[i], " ") do
                           if m.Index > indentOf ls[i] then i, m.Index |]

        if spots.Length = 0 then
            None
        else
            let i, at = pick random spots
            Some(unlines (Array.updateAt i (ls[i].Insert(at, " ")) ls), $"double a space on line %d{i + 1}")

/// Toggles what checking a file against its signature file overwrites in an item a later declaration uses by name:
/// T.M's argument name, f's arity or g's type parameter name (SigConform's A.fs, Task 6 fix round 1).
let private conformedShape: Mutation =
    fun random text ->
        let toggles =
            [| "member _.M(t: int) = t", "member _.M(_: int) = 1"
               "let f (x: int) y = x + y", "let f (x: int) = let add y = x + y in add"
               "let g (x: 'a) = x", "let g (x: 'b) = x" |]
            |> Array.collect (fun (a, b) -> [| a, b; b, a |])
            |> Array.filter (fun (from, _) -> text.Contains from)

        if toggles.Length = 0 then
            None
        else
            let from, into = pick random toggles
            Some(text.Replace(from, into), $"conformed shape `%s{from}` -> `%s{into}`")

/// Inserts a blank line after the first line, or takes a blank line out: every declaration below it moves by a line
/// and keeps its text (W1), unless the line is inside it.
let private blankLine: Mutation =
    fun random text ->
        let ls = linesOf text
        let blanks = [| for i in 1 .. ls.Length - 1 do if ls[i].Trim() = "" then i |]

        if blanks.Length > 0 && random.Next 2 = 0 then
            let i = pick random blanks
            Some(unlines (Array.removeAt i ls), $"blank line %d{i + 1} removed")
        else
            let i = 1 + random.Next(ls.Length - 1)
            Some(unlines (Array.insertAt i "" ls), $"blank line inserted at %d{i + 1}")

/// The first of up to 20 mutations picked at random that applies to the text. Outside the task that uses it: a
/// recursive function inside a task builder is not compiled to a state machine (FS3511).
let private mutate (random: Random) (mutations: Mutation[]) (text: string) =
    let rec attempt tries =
        match (pick random mutations) random text with
        | Some edit -> edit
        | None when tries < 20 -> attempt (tries + 1)
        | None -> text, "no edit"

    attempt 0

/// `refuse`: the checker refuses every shift (W1), so the graph checks each moved declaration again as it checks a changed one
let private runIn (beforeReading: Session -> unit -> Task) (refuse: bool) (project: string) (mode: FileMode) (seed: int) (steps: int) =
    task {
        let! s = startOn project mode
        s.Graph.Context.RefuseShifts <- refuse
        let openTargets =
            if project = chainsProject then chainsOpenTargets
            elif project = sigShadowProject then sigShadowOpenTargets
            elif project = sigQualNameProject then sigQualNameOpenTargets
            elif project = sigUnmatchedProject then sigUnmatchedOpenTargets
            elif project = sigChain2Project then sigChain2OpenTargets
            elif project = sigConformProject then sigConformOpenTargets
            else depsOpenTargets
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }
        let random = Random seed
        let files = s.Spec.SourceFiles |> List.filter (fun f -> f.EndsWith ".fs") |> Array.ofList
        let pool = names s.Texts.Values
        let originals = Collections.Generic.Dictionary<string, string>(s.Texts, StringComparer.OrdinalIgnoreCase)
        let counter = ref 0

        let before =
            [| literal
               returnType
               addDeclaration pool counter
               removeDeclaration
               addOpen openTargets
               removeOpen
               renameUse pool
               shadow pool
               freeTypars counter
               moduleHeader
               autoOpen
               augmentationMember counter
               sameLine
               conformedShape
               blankLine |]

        let after = if project = sigUnmatchedProject then [| topHeader |] else [||]
        let mutations = Array.concat [ before; [| failingDefault |]; after ]


        let premise = Collections.Concurrent.ConcurrentQueue<FSharp.Compiler.Declarations.DeclarationKey>()
        use _ = s.Graph.PremiseBroken |> Observable.subscribe (fun b -> premise.Enqueue b.Key)

        let mutable withErrors = 0
        let mutable errorFiles = 0
        let mutable checks = 0L
        let mutable published = 0
        // Edits conformedShape made: on SigConform every run must make some
        let mutable conformedEdits = 0
        // Edits blankLine made: every run must make some
        let mutable blankLineEdits = 0

        // Declarations mode starts with every implementation file wanted
        let wanted = Collections.Generic.HashSet<string>(StringComparer.OrdinalIgnoreCase)

        if mode = Declarations then
            for f in files do
                wanted.Add f |> ignore

        for step in 1..steps do
            let file = pick random files
            // Now and then the edit is to promote or demote a file instead of its text
            let toggle = random.Next 8 = 0
            // Half the time the oracle is asked before the wanted files' builds have settled
            let waitForBuilds = random.Next 2 = 0

            // Now and then a file goes back to how it was, undoing its edits at once: errors otherwise pile up until
            // every file is broken and the checks run on error recovery alone.
            let text, what =
                if toggle then
                    s.Texts[file], (if wanted.Contains file then "demote" else "promote")
                elif random.Next 6 = 0 then
                    originals[file], "revert to the original"
                else
                    mutate random mutations s.Texts[file]

            let fail (problem: string) =
                let texts =
                    String.Join("\n", s.Texts |> Seq.map (fun kv -> $"--- %s{Path.GetFileName kv.Key}\n%s{kv.Value}"))

                failtest $"%A{mode} seed %d{seed}, step %d{step}: %s{Path.GetFileName file}: %s{what}\n%s{problem}\n%s{texts}"

            let! r =
                task {
                    try
                        if toggle then
                            if wanted.Remove file then s.Graph.Demote file
                            else
                                wanted.Add file |> ignore
                                s.Graph.Promote file

                            let! _ = s.Graph.AllDiagnostics() |> settle

                            if waitForBuilds then
                                do! whenPromoted s.Graph s.Spec

                            let! diagnostics = s.Graph.AllDiagnostics() |> settle
                            return { Delta = 0L; Checked = []; Ran = []; Diagnostics = diagnostics; Settled = diagnostics }
                        else
                            return! editAfter (beforeReading s) waitForBuilds s file text
                    with e ->
                        return fail (string e)
                }

            let! diff = oracleDiff s r.Diagnostics

            if not diff.IsEmpty then
                fail (String.concat Environment.NewLine diff)

            // Each file's settled diagnostics, asked for before anything settled, are the oracle's for that file too
            for KeyValue(f, settled) in r.Settled do
                let perFile = Rfs.Lsp.Diagnostics.compare (r.Diagnostics[f] |> Seq.map Rfs.Projects.FcsDiagnostics.ofFcs |> List.ofSeq) (settled |> Seq.map Rfs.Projects.FcsDiagnostics.ofFcs |> List.ofSeq)

                if not perFile.IsEmpty then
                    fail ($"SettledDiagnostics of %s{Path.GetFileName f} differ from the settled project's:\n" + String.concat Environment.NewLine perFile)

            let! fingerprints = fingerprintDiff s

            if not fingerprints.IsEmpty then
                fail ("fingerprints differ from a check from scratch:\n" + String.concat Environment.NewLine fingerprints)

            if s.Graph.PremiseBreaks <> 0L then
                fail $"the D1 premise broke: a final fingerprint differed from its check's for %A{List.ofSeq premise}"

            if r.Diagnostics.Values |> Seq.exists (Array.exists (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)) then
                withErrors <- withErrors + 1

            errorFiles <- errorFiles + (r.Diagnostics.Values |> Seq.filter (Array.exists (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)) |> Seq.length)

            checks <- checks + r.Delta
            published <- published + r.Checked.Length

            if what.StartsWith "conformed shape " then
                conformedEdits <- conformedEdits + 1

            if what.StartsWith "blank line " then
                blankLineEdits <- blankLineEdits + 1

                // A shifted row must point where a full check does (Task 7 review I1), which no diagnostic shows
                let! locations = locationDiff s file

                if not locations.IsEmpty then
                    fail ("symbol locations differ from a full check:\n" + String.concat Environment.NewLine locations)

        printfn
            $"%A{mode} seed %d{seed}: %d{steps} edits (%d{conformedEdits} conformed-shape edits, %d{blankLineEdits} blank-line edits), %d{withErrors} left errors (%d{errorFiles} files with errors in all), %d{s.Graph.CheckedCount} declaration checks run, %d{checks} adopted after load, %d{s.Graph.ShiftedRows} shifted, %d{published} results published"

        if project = sigConformProject then
            Expect.isGreaterThan conformedEdits 0 $"%A{mode} seed %d{seed}: the run toggles what the signature file overwrites"

        Expect.isGreaterThan blankLineEdits 0 $"%A{mode} seed %d{seed}: the run inserts or removes a blank line"

        Expect.equal
            FSharp.Compiler.Declarations.ProjectContext.ShiftFinishMisses
            0L
            "no check the finish ran for a shifted result reported at a range it could not move"

    }

let private runWith = runIn (fun _ _ -> Task.CompletedTask)

let private run = runWith false

/// How much work the graph has done, read without touching the graph: every parse, dependency computation and
/// whole-file check as it starts, every declaration check and finish as it ends.
let private activity (s: Session) =
    s.Graph.MeasuresStarted + s.Graph.CheckedCount + s.Graph.FinishTicks

/// Watches the counters, reading nothing, until they have rested for a second with no measured work running. Returns
/// the activity at rest.
let private atRest (s: Session) =
    task {
        let mutable last = activity s
        let resting = Diagnostics.Stopwatch.StartNew()
        let waited = Diagnostics.Stopwatch.StartNew()

        while resting.Elapsed < TimeSpan.FromSeconds 1. do
            do! Task.Delay 25

            if waited.Elapsed > TimeSpan.FromMinutes 2. then
                failtest "work was still running 2 minutes after an edit, with nothing reading"

            let now = activity s

            if now <> last || s.Graph.MeasuresInFlight > 0L then
                last <- now
                resting.Restart()

        return last
    }

/// m6: every edit's work must finish with nothing waiting and nothing reading. Rests, then reads, and returns the
/// activity at rest and whether the read found work left to do: a stall. A stall a read does not end fails `settle`.
let private readAfterRest (s: Session) =
    task {
        let! rested = atRest s
        let! _ = s.Graph.AllDiagnostics() |> settle
        return struct (rested, activity s <> rested)
    }

let private runUnread (project: string) (mode: FileMode) (seed: int) (steps: int) =
    task {
        let stalled = ResizeArray<string>()
        let step = ref 0

        let unread (s: Session) () : Task =
            task {
                step.Value <- step.Value + 1
                let! struct (_, found) = readAfterRest s

                if found then
                    stalled.Add $"edit %d{step.Value}"
            }

        do! runIn unread false project mode seed steps
        Expect.isEmpty stalled $"%A{mode} seed %d{seed}: the work of these edits ran only once a read asked"
    }

/// The fuzz edits only implementation files. A comment in a signature file parses it, computes the dependencies and
/// checks it again, and leaves its fingerprint as it was, so no file after it is checked: nothing but this work shows.
let private runUnreadSignatures (mode: FileMode) =
    task {
        let! s = startOn sigChain2Project mode
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }

        for round in 1..5 do
            for file in s.Spec.SourceFiles |> List.filter (fun f -> f.EndsWith ".fsi") do
                let what = $"%A{mode} round %d{round}: %s{Path.GetFileName file}"
                let before = s.Graph.MeasuresStarted

                let unread () : Task =
                    task {
                        let! struct (_, found) = readAfterRest s
                        Expect.isFalse found $"%s{what}: the work ran only once a read asked"
                    }

                let! r = editAfter unread true s file (s.Texts[file] + $"\n// round %d{round}\n")
                Expect.isGreaterThanOrEqual (s.Graph.MeasuresStarted - before) 3L $"%s{what}: parse, dependencies and check all ran"
                do! expectOracle s r.Diagnostics what
    }

let private fuzz (name: string) (mode: FileMode) =
    testSequenced
    <| testList
        name
        [ for seed in [ 1515; 4242; 7919; 47514; 63352 ] ->
              testTask $"seed %d{seed}: 200 random edits equal the oracle after each" { do! run depsProject mode seed 200 }

          // A refused shift is checked again as a changed declaration is, against the current results
          yield testTask "seed 1515, every shift refused: 200 random edits equal the oracle after each" { do! runWith true depsProject mode 1515 200 }

          // Two independent chains, where a file's dependencies are fewer than the files before it
          for seed in [ 1515; 4242 ] ->
              testTask $"Chains seed %d{seed}: 200 random edits equal the oracle after each" { do! run chainsProject mode seed 200 }

          // Signature files standing in for their implementation files: AutoOpen order and qualified-name pairing
          for project, label in [ sigShadowProject, "SigShadow"; sigQualNameProject, "SigQualName"; sigUnmatchedProject, "SigUnmatched" ] ->
              testTask $"%s{label} seed 1515: 200 random edits equal the oracle after each" { do! run project mode 1515 200 }

          // A failed status two signature pairs away, which only a transitive settle gate waits for
          for seed in [ 1515; 4242; 7919; 47514; 63352 ] ->
              testTask $"SigChain2 seed %d{seed}: 200 random edits equal the oracle after each" { do! run sigChain2Project mode seed 200 }

          // Argument names, arities and type parameter names the signature file overwrites, used by later declarations
          for seed in [ 1515; 4242; 7919; 47514; 63352 ] ->
              testTask $"SigConform seed %d{seed}: 200 random edits equal the oracle after each" { do! run sigConformProject mode seed 200 } ]

/// The graph as ProjectGraph.Create makes it: every file file-level
[<Tests>]
let tests = fuzz "OracleFuzz" FileLevel

/// Every file checked by declaration, set up from the files it depends on
[<Tests>]
let declarationTests = fuzz "OracleFuzzDeclarations" Declarations

[<Tests>]
let unreadTests =
    testSequenced
    <| testList
        "OracleFuzzUnread"
        [ for mode in [ FileLevel; Declarations ] do
              yield testTask $"%A{mode} SigChain2 seed 1515: work finishes with no reader" { do! runUnread sigChain2Project mode 1515 200 }
              yield testTask $"%A{mode} SigChain2: a signature file's comment finishes with no reader" { do! runUnreadSignatures mode } ]
