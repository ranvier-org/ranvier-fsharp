namespace Rfs.Checker

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Syntax
open FSharp.Compiler.Tokenization

/// A declaration of a parse as plain data: what position safety compares of it
type RowShape =
    { Key: DeclarationKey
      /// 1-based, as the parse's ranges
      StartLine: int
      StartColumn: int
      EndLine: int
      EndColumn: int
      Within: DeclarationKey option
      Text: string
      HeaderInputs: string }

    /// ParsedDeclaration.IsUnchangedFrom, the same module header and the same start column
    member a.SameAs(b: RowShape) =
        String.Equals(a.Text, b.Text, StringComparison.Ordinal)
        && String.Equals(a.HeaderInputs, b.HeaderInputs, StringComparison.Ordinal)
        && a.Within = b.Within
        && a.StartColumn = b.StartColumn

/// A file's text with its rows, as position safety reads it. No compiler object: it outlives the result it is of.
type FileShape =
    { Text: string
      Lines: string[]
      /// Empty for a file checked whole
      Rows: RowShape[]
      /// A `rec` module or namespace anywhere in the parse
      Recursive: bool }

/// Why a position of the new text has no place in the old one: each means wait for the check
[<RequireQualifiedAccess>]
type Refusal =
    | NoSnapshot
    | Reloading
    | CheckedWhole
    | Recursive
    /// Before the first row, after the last, or between two
    | Gap
    | NewRow
    | Edited
    | OrdinalMoved
    | Directives
    | LexerState
    | NotAnswered
    | DocsChanged

/// A position of the old text
[<Struct>]
type OldPosition = { OldLine: int; OldCharacter: int }

/// <summary>
/// Position safety: where a cursor in a file's new text was in its old text, through the rows both parses split into,
/// or why it has no such place. For any feature that answers from the last state of a file.
/// </summary>
/// <remarks>
/// The cursor's row in the new parse is found by its DeclarationKey in the old one, unchanged (same text, header inputs,
/// module and start column), with every row of its key's name group unchanged in the same order, and the module headers
/// it is inside; the old line is the new one minus the row's shift. The line's text and the lexer state it starts in
/// must be the same, and so must every `#if` family directive above it. Any line directive above it in either text
/// refuses: the parse's ranges no longer count text lines.
/// </remarks>
module PositionMap =
    let private isRecursive (input: ParsedInput) =
        let rec decls (ds: SynModuleDecl list) =
            ds
            |> List.exists (function
                | SynModuleDecl.NestedModule(isRecursive = true) -> true
                | SynModuleDecl.NestedModule(decls = inner) -> decls inner
                | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(isRecursive = r; decls = inner)) -> r || decls inner
                | _ -> false)

        match input with
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
            modules |> List.exists (fun (SynModuleOrNamespace(isRecursive = r; decls = ds)) -> r || decls ds)
        | ParsedInput.SigFile _ -> true

    let private split (text: string) = text.Split '\n' |> Array.map _.TrimEnd('\r')

    /// The shape of a parse made by DeclarationChecker.Parse. `byDeclaration` false gives no rows.
    let shapeOf (text: string) (parse: FSharpParseFileResults) (byDeclaration: bool) : FileShape =
        let rows =
            if byDeclaration then
                DeclarationChecker.Split parse
                |> Array.map (fun d ->
                    { Key = d.Key
                      StartLine = d.Range.StartLine
                      StartColumn = d.Range.StartColumn
                      EndLine = d.Range.EndLine
                      EndColumn = d.Range.EndColumn
                      Within = d.Within
                      Text = d.Text
                      HeaderInputs = d.HeaderInputs })
            else
                [||]

        { Text = text
          Lines = split text
          Rows = rows
          Recursive = isRecursive parse.ParseTree }

    /// The text alone, for a text whose rows are never read: an identical text needs none
    let textOnly (text: string) : FileShape =
        { Text = text; Lines = split text; Rows = [||]; Recursive = false }

    /// What a line starts in: a string or block comment still open, else Initial (as Hover's tokenizer carries it)
    let private carried (next: FSharpTokenizerLexState) =
        match FSharpLineTokenizer.ColorStateOfLexState next with
        | FSharpTokenizerColorState.String
        | FSharpTokenizerColorState.VerbatimString
        | FSharpTokenizerColorState.TripleQuoteString
        | FSharpTokenizerColorState.Comment
        | FSharpTokenizerColorState.StringInComment
        | FSharpTokenizerColorState.VerbatimStringInComment
        | FSharpTokenizerColorState.TripleQuoteStringInComment -> next
        | _ -> FSharpTokenizerLexState.Initial

    /// The lexer state line `target` (0-based) starts in, lexed from the top
    let lexStateAt (lines: string[]) (target: int) : FSharpTokenizerLexState =
        let tokenizer = FSharpSourceTokenizer([], Some "position.fs", None)
        let mutable state = FSharpTokenizerLexState.Initial

        for i in 0 .. target - 1 do
            let line = tokenizer.CreateLineTokenizer lines[i]
            let mutable more = true

            while more do
                match line.ScanToken state with
                | Some _, next -> state <- next
                | None, next ->
                    state <- carried next
                    more <- false

        state

    let private isLineDirective (t: string) =
        t.StartsWith "#line"
        || (t.StartsWith "#"
            && (let rest = t.Substring(1).TrimStart() in rest.Length > 0 && Char.IsDigit rest[0]))

    let private isConditional (trimmed: string) =
        [ "#if"; "#else"; "#elif"; "#endif" ] |> List.exists (fun d -> trimmed.StartsWith d)

    /// The `#if` family directives above the 0-based line, and whether a line directive is among the lines above it
    let private directivesAbove (lines: string[]) (line: int) =
        let found = ResizeArray<string>()
        let mutable lineDirective = false

        for i in 0 .. min (line - 1) (lines.Length - 1) do
            let t = lines[i].Trim()

            if isLineDirective t then lineDirective <- true
            elif isConditional t then found.Add t

        List.ofSeq found, lineDirective

    let private rowAt (rows: RowShape[]) (line: int) (character: int) =
        // 1-based line. Rows don't overlap: a nested module's header ends at its `=`.
        rows
        |> Array.tryFindBack (fun r ->
            (r.StartLine < line || (r.StartLine = line && r.StartColumn <= character))
            && (line < r.EndLine || (line = r.EndLine && character <= r.EndColumn)))

    let private group (rows: RowShape[]) (key: DeclarationKey) =
        rows
        |> Array.filter (fun r -> r.Key.ModulePath = key.ModulePath && r.Key.Name = key.Name && r.Key.Kind = key.Kind)
        |> Array.sortBy _.Key.Ordinal

    let private find (rows: RowShape[]) (key: DeclarationKey) = rows |> Array.tryFind (fun r -> r.Key = key)

    /// <summary>Where the 0-based cursor of `current` was in `old`, or why it can't be known.</summary>
    let map (old: FileShape) (current: FileShape) (line: int) (character: int) : Result<OldPosition, Refusal> =
        if String.Equals(old.Text, current.Text, StringComparison.Ordinal) then
            Ok { OldLine = line; OldCharacter = character }
        elif old.Rows.Length = 0 || current.Rows.Length = 0 then
            Error Refusal.CheckedWhole
        elif old.Recursive || current.Recursive then
            Error Refusal.Recursive
        else
            match rowAt current.Rows (line + 1) character with
            | None -> Error Refusal.Gap
            | Some row ->
                let now = group current.Rows row.Key
                let before = group old.Rows row.Key

                let rec headersSame (within: DeclarationKey option) =
                    match within with
                    | None -> true
                    | Some k ->
                        match find current.Rows k, find old.Rows k with
                        | Some a, Some b when a.SameAs b -> headersSame a.Within
                        | _ -> false

                // A key of several rows names each by its place among them: one inserted, removed or edited before the
                // cursor's gives its key to another row
                let ordinalsMoved =
                    (now.Length > 1 || before.Length > 1)
                    && (now.Length <> before.Length || Array.exists2 (fun (a: RowShape) b -> not (a.SameAs b)) now before)

                match find old.Rows row.Key with
                | _ when ordinalsMoved -> Error Refusal.OrdinalMoved
                | None -> Error Refusal.NewRow
                | Some was when not (row.SameAs was) -> Error Refusal.Edited
                | Some was ->
                    if not (headersSame row.Within) then
                        Error Refusal.Edited
                    else
                        let oldLine = line - (row.StartLine - was.StartLine)

                        let directivesNow, lineNow = directivesAbove current.Lines line
                        let directivesThen, lineThen = directivesAbove old.Lines oldLine

                        if oldLine < 0 || oldLine >= old.Lines.Length then
                            Error Refusal.Edited
                        elif lineNow || lineThen || directivesNow <> directivesThen then
                            Error Refusal.Directives
                        elif not (String.Equals(current.Lines[line], old.Lines[oldLine], StringComparison.Ordinal)) then
                            Error Refusal.Edited
                        else
                            let a = lexStateAt current.Lines line
                            let b = lexStateAt old.Lines oldLine

                            if a.PosBits <> b.PosBits || a.OtherBits <> b.OtherBits then
                                Error Refusal.LexerState
                            else
                                Ok { OldLine = oldLine; OldCharacter = character }
