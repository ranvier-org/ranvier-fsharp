module Rfs.Checker.Tests.OracleFuzzTests

open System
open System.IO
open System.Text.RegularExpressions
open System.Threading.Tasks
open Expecto
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

let private openTargets = [| "System"; "Deps.A"; "Deps.B"; "Deps.B.Util"; "System.Collections.Generic" |]

let private addOpen: Mutation =
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

let private run (seed: int) (steps: int) =
    task {
        let! s = start ()
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
               addOpen
               removeOpen
               renameUse pool |]

        let mutable withErrors = 0
        let mutable errorFiles = 0
        let mutable checks = 0L
        let mutable published = 0

        for step in 1..steps do
            let file = pick random files

            let rec attempt tries =
                match (pick random mutations) random s.Texts[file] with
                | Some edit -> edit
                | None when tries < 20 -> attempt (tries + 1)
                | None -> s.Texts[file], "no edit"

            // Now and then a file goes back to how it was, undoing its edits at once: errors otherwise pile up until
            // every file is broken and the checks run on error recovery alone.
            let text, what =
                if random.Next 6 = 0 then
                    originals[file], "revert to the original"
                else
                    attempt 0

            let fail (problem: string) =
                let texts =
                    String.Join("\n", s.Texts |> Seq.map (fun kv -> $"--- %s{Path.GetFileName kv.Key}\n%s{kv.Value}"))

                failtest $"seed %d{seed}, step %d{step}: %s{Path.GetFileName file}: %s{what}\n%s{problem}\n%s{texts}"

            let! r =
                task {
                    try
                        return! edit s file text
                    with e ->
                        return fail (string e)
                }

            let! diff = oracleDiff s r.Diagnostics

            if not diff.IsEmpty then
                fail (String.concat Environment.NewLine diff)

            if r.Diagnostics.Values |> Seq.exists (Array.exists (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)) then
                withErrors <- withErrors + 1

            errorFiles <- errorFiles + (r.Diagnostics.Values |> Seq.filter (Array.exists (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)) |> Seq.length)

            checks <- checks + r.Delta
            published <- published + r.Checked.Length

        printfn $"seed %d{seed}: %d{steps} edits, %d{withErrors} left errors (%d{errorFiles} files with errors in all), %d{checks} declaration checks, %d{published} results published"
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "OracleFuzz"
        [ testTask "seed 1515: 200 random edits equal the oracle after each" { do! run 1515 200 }
          testTask "seed 4242: 200 random edits equal the oracle after each" { do! run 4242 200 } ]
