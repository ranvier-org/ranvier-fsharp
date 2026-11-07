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

let private run (project: string) (mode: FileMode) (seed: int) (steps: int) =
    task {
        let! s = startOn project mode
        let openTargets =
            if project = chainsProject then chainsOpenTargets
            elif project = sigShadowProject then sigShadowOpenTargets
            elif project = sigQualNameProject then sigQualNameOpenTargets
            else depsOpenTargets
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }
        let random = Random seed
        let files = s.Spec.SourceFiles |> List.filter (fun f -> f.EndsWith ".fs") |> Array.ofList
        let pool = names s.Texts.Values
        let originals = Collections.Generic.Dictionary<string, string>(s.Texts, StringComparer.OrdinalIgnoreCase)
        let counter = ref 0

        let mutations =
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
               sameLine |]

        let premise = Collections.Concurrent.ConcurrentQueue<FSharp.Compiler.Declarations.DeclarationKey>()
        use _ = s.Graph.PremiseBroken |> Observable.subscribe (fun (k, _) -> premise.Enqueue k)

        let mutable withErrors = 0
        let mutable errorFiles = 0
        let mutable checks = 0L
        let mutable published = 0

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

            let rec attempt tries =
                match (pick random mutations) random s.Texts[file] with
                | Some edit -> edit
                | None when tries < 20 -> attempt (tries + 1)
                | None -> s.Texts[file], "no edit"

            // Now and then a file goes back to how it was, undoing its edits at once: errors otherwise pile up until
            // every file is broken and the checks run on error recovery alone.
            let text, what =
                if toggle then
                    s.Texts[file], (if wanted.Contains file then "demote" else "promote")
                elif random.Next 6 = 0 then
                    originals[file], "revert to the original"
                else
                    attempt 0

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
                            return { Delta = 0L; Checked = []; Ran = []; Diagnostics = diagnostics }
                        else
                            return! editWith waitForBuilds s file text
                    with e ->
                        return fail (string e)
                }

            let! diff = oracleDiff s r.Diagnostics

            if not diff.IsEmpty then
                fail (String.concat Environment.NewLine diff)

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

        printfn
            $"%A{mode} seed %d{seed}: %d{steps} edits, %d{withErrors} left errors (%d{errorFiles} files with errors in all), %d{s.Graph.CheckedCount} declaration checks run, %d{checks} adopted after load, %d{published} results published"

    }

let private fuzz (name: string) (mode: FileMode) =
    testSequenced
    <| testList
        name
        [ for seed in [ 1515; 4242; 7919; 47514; 63352 ] ->
              testTask $"seed %d{seed}: 200 random edits equal the oracle after each" { do! run depsProject mode seed 200 }

          // Two independent chains, where a file's dependencies are fewer than the files before it
          for seed in [ 1515; 4242 ] ->
              testTask $"Chains seed %d{seed}: 200 random edits equal the oracle after each" { do! run chainsProject mode seed 200 }

          // Signature files standing in for their implementation files: AutoOpen order and qualified-name pairing
          for project, label in [ sigShadowProject, "SigShadow"; sigQualNameProject, "SigQualName" ] ->
              testTask $"%s{label} seed 1515: 200 random edits equal the oracle after each" { do! run project mode 1515 200 } ]

/// The graph as ProjectGraph.Create makes it: every file file-level
[<Tests>]
let tests = fuzz "OracleFuzz" FileLevel

/// Every file checked by declaration, set up from the files it depends on
[<Tests>]
let declarationTests = fuzz "OracleFuzzDeclarations" Declarations
