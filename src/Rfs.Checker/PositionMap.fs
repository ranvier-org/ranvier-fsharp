namespace Rfs.Checker

open System
open System.Collections.Generic
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

/// The `#if` family and line directives of a text, read once: what position safety compares above a line
[<NoEquality; NoComparison>]
type Directives =
    { /// Each `#if`, `#else`, `#elif` and `#endif` line, trimmed, in order
      Conditionals: string[]
      /// For each 0-based line, how many of Conditionals are above it
      Above: int[]
      /// For each 0-based line, whether a line directive (`#line`, `# n`) is above it
      LineAbove: bool[] }

/// A file's text with its rows, as position safety reads it. No compiler object: it outlives the result it is of.
[<NoEquality; NoComparison>]
type FileShape =
    { Text: string
      Lines: string[]
      /// Empty for a file checked whole
      Rows: RowShape[]
      /// A `rec` module or namespace anywhere in the parse
      Recursive: bool
      /// The lexer state each line starts in, lexed once from the top as Hover's tokenizer carries it
      States: Lazy<FSharpTokenizerLexState[]>
      Directives: Lazy<Directives> }

/// A file's last published result as the graph has it (ProjectGraph.LatestText): what an answer from the last state is
/// mapped against. Read it within a request and drop it: it holds the result.
[<NoEquality; NoComparison>]
type PublishedState =
    { /// FileView.Identity of the result
      Identity: obj
      /// The text version the result is of
      Version: int64
      Text: string
      Parse: FSharpParseFileResults
      Docs: FileDocs
      /// A whole check's result
      Whole: bool
      /// Each declaration's result (FileView.RowAt's object) with its first and last 1-based line; empty where RowAt
      /// gives none, as for a file checked whole
      Rows: struct (obj * int * int)[] }

/// Why a position of the new text has no place in the old one: each means wait for the check
[<RequireQualifiedAccess>]
type Refusal =
    /// The file has published no result yet
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
    /// The text or the published result changed between the reads of one answer
    | Moved

/// Where positions of an old text are in a new one (PositionMap.forward): the 0-based new line and the index of the
/// old row the position is in (-1 for identical texts), or why there is no such place. Made once per pair of parses.
[<NoEquality; NoComparison>]
type ForwardMap = { At: int -> int -> Result<struct (int * int), Refusal> }

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

    /// What a line starts in: a string or block comment still open, else Initial (as Hover's tokenizer carries it)
    let carried (next: FSharpTokenizerLexState) =
        match FSharpLineTokenizer.ColorStateOfLexState next with
        | FSharpTokenizerColorState.String
        | FSharpTokenizerColorState.VerbatimString
        | FSharpTokenizerColorState.TripleQuoteString
        | FSharpTokenizerColorState.Comment
        | FSharpTokenizerColorState.StringInComment
        | FSharpTokenizerColorState.VerbatimStringInComment
        | FSharpTokenizerColorState.TripleQuoteStringInComment -> next
        | _ -> FSharpTokenizerLexState.Initial

    /// The tokenizer Hover lexes with: no defines, as FSAC's
    let tokenizer () = FSharpSourceTokenizer([], Some "hover.fs", None)

    /// The lexer state each line starts in, lexed once from the top
    let lexStates (lines: string[]) : FSharpTokenizerLexState[] =
        let tokenizer = tokenizer ()
        let states = Array.zeroCreate lines.Length
        let mutable state = FSharpTokenizerLexState.Initial

        for i in 0 .. lines.Length - 1 do
            states[i] <- state
            let line = tokenizer.CreateLineTokenizer lines[i]
            let mutable more = true

            while more do
                match line.ScanToken state with
                | Some _, next -> state <- next
                | None, next ->
                    state <- carried next
                    more <- false

        states

    let private isLineDirective (t: string) =
        t.StartsWith "#line"
        || (t.StartsWith "#"
            && (let rest = t.Substring(1).TrimStart() in rest.Length > 0 && Char.IsDigit rest[0]))

    let private isConditional (trimmed: string) =
        trimmed.StartsWith "#if" || trimmed.StartsWith "#else" || trimmed.StartsWith "#elif" || trimmed.StartsWith "#endif"

    /// Each line's directives above it, in one pass
    let private directivesOf (lines: string[]) : Directives =
        let conditionals = ResizeArray<string>()
        let above = Array.zeroCreate lines.Length
        let lineAbove = Array.zeroCreate lines.Length
        let mutable seenLine = false

        for i in 0 .. lines.Length - 1 do
            above[i] <- conditionals.Count
            lineAbove[i] <- seenLine
            let t = lines[i]

            // Only a line whose first non-blank character is `#` can be a directive: most lines are never trimmed
            let first = t |> Seq.tryFind (fun c -> not (Char.IsWhiteSpace c))

            if first = Some '#' then
                let t = t.Trim()

                if isLineDirective t then seenLine <- true
                elif isConditional t then conditionals.Add t

        { Conditionals = conditionals.ToArray()
          Above = above
          LineAbove = lineAbove }

    let private make (text: string) (rows: RowShape[]) (recursive: bool) =
        let lines = split text

        { Text = text
          Lines = lines
          Rows = rows
          Recursive = recursive
          States = lazy (lexStates lines)
          Directives = lazy (directivesOf lines) }

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

        make text rows (isRecursive parse.ParseTree)

    /// The text alone, for a text whose rows are never read
    let textOnly (text: string) : FileShape = make text [||] false

    /// Whether the `#if` family directives above the two lines are the same list, and no line directive is above either
    let private sameDirectives (a: Directives) (lineA: int) (b: Directives) (lineB: int) =
        let at (d: Directives) (line: int) = if line < d.Above.Length then d.Above[line] else d.Conditionals.Length
        let lineAt (d: Directives) (line: int) = if line < d.LineAbove.Length then d.LineAbove[line] else false
        let n = at a lineA

        not (lineAt a lineA)
        && not (lineAt b lineB)
        && n = at b lineB
        && (let mutable same = true in
            let mutable i = 0 in

            while same && i < n do
                same <- String.Equals(a.Conditionals[i], b.Conditionals[i], StringComparison.Ordinal)
                i <- i + 1

            same)

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

                        if oldLine < 0 || oldLine >= old.Lines.Length then
                            Error Refusal.Edited
                        elif not (sameDirectives current.Directives.Value line old.Directives.Value oldLine) then
                            Error Refusal.Directives
                        elif not (String.Equals(current.Lines[line], old.Lines[oldLine], StringComparison.Ordinal)) then
                            Error Refusal.Edited
                        else
                            let a = current.States.Value[line]
                            let b = old.States.Value[oldLine]

                            if a.PosBits <> b.PosBits || a.OtherBits <> b.OtherBits then
                                Error Refusal.LexerState
                            else
                                Ok { OldLine = oldLine; OldCharacter = character }

    /// <summary>
    /// Where positions of `old` are in `current`, made once for the pair: each old row's counterpart by key and its shift,
    /// with every rule of `map` checked for the row and, once per line, for the line, so that a position this gives
    /// back maps to exactly its old place through `map`. A position is then a binary search over the old rows.
    /// </summary>
    /// <remarks>
    /// A position outside every old row is Gap; a row with no unchanged counterpart is Edited (or OrdinalMoved). A
    /// range is assumed to lie in one row: a caller mapping both ends must require the same row index for each.
    /// </remarks>
    let forward (old: FileShape) (current: FileShape) (ct: System.Threading.CancellationToken) : ForwardMap =
        if String.Equals(old.Text, current.Text, StringComparison.Ordinal) then
            { At = fun line _ -> Ok(struct (line, -1)) }
        elif old.Rows.Length = 0 || current.Rows.Length = 0 then
            { At = fun _ _ -> Error Refusal.CheckedWhole }
        elif old.Recursive || current.Recursive then
            { At = fun _ _ -> Error Refusal.Recursive }
        else
            let rows = old.Rows |> Array.sortBy (fun r -> struct (r.StartLine, r.StartColumn))
            let byKey (rs: RowShape[]) = rs |> Seq.map (fun r -> KeyValuePair(r.Key, r)) |> Dictionary<DeclarationKey, RowShape>
            let oldByKey = byKey old.Rows
            let currentByKey = byKey current.Rows

            let groups (rs: RowShape[]) =
                rs
                |> Array.groupBy (fun r -> r.Key.ModulePath, r.Key.Name, r.Key.Kind)
                |> Array.map (fun (k, g) -> k, g |> Array.sortBy _.Key.Ordinal)
                |> dict

            let oldGroups = groups old.Rows
            let currentGroups = groups current.Rows

            let groupOf (g: IDictionary<_, RowShape[]>) k =
                match g.TryGetValue k with
                | true, rs -> rs
                | _ -> [||]

            let rec headersSame (within: DeclarationKey option) =
                match within with
                | None -> true
                | Some k ->
                    match currentByKey.TryGetValue k, oldByKey.TryGetValue k with
                    | (true, a), (true, b) when a.SameAs b -> headersSame a.Within
                    | _ -> false

            // Each old row: its shift, or why none of its positions map
            let rowShift =
                rows
                |> Array.map (fun was ->
                    ct.ThrowIfCancellationRequested()
                    let k = (was.Key.ModulePath, was.Key.Name, was.Key.Kind)
                    let now = groupOf currentGroups k
                    let before = groupOf oldGroups k

                    let ordinalsMoved =
                        (now.Length > 1 || before.Length > 1)
                        && (now.Length <> before.Length || Array.exists2 (fun (a: RowShape) b -> not (a.SameAs b)) now before)

                    match currentByKey.TryGetValue was.Key with
                    | _ when ordinalsMoved -> Error Refusal.OrdinalMoved
                    | false, _ -> Error Refusal.Edited
                    | true, row when not (row.SameAs was) || not (headersSame row.Within) -> Error Refusal.Edited
                    | true, row -> Ok(row.StartLine - was.StartLine))

            let oldDirectives = lazy old.Directives.Value
            let currentDirectives = lazy current.Directives.Value

            // Each old line's new line, made when first asked
            let lines: Result<int, Refusal> voption[] = Array.zeroCreate old.Lines.Length

            let lineAt (line: int) (shift: int) =
              lock lines (fun () ->
                match lines[line] with
                | ValueSome r -> r
                | ValueNone ->
                    let newLine = line + shift

                    let r =
                        if newLine < 0 || newLine >= current.Lines.Length then
                            Error Refusal.Edited
                        elif not (sameDirectives currentDirectives.Value newLine oldDirectives.Value line) then
                            Error Refusal.Directives
                        elif not (String.Equals(current.Lines[newLine], old.Lines[line], StringComparison.Ordinal)) then
                            Error Refusal.Edited
                        else
                            let a = current.States.Value[newLine]
                            let b = old.States.Value[line]

                            if a.PosBits <> b.PosBits || a.OtherBits <> b.OtherBits then Error Refusal.LexerState else Ok newLine

                    lines[line] <- ValueSome r
                    r)

            // The last row starting at or before the position, as rows don't overlap
            let rowOf (line: int) (character: int) =
                let oneBased = line + 1
                let mutable lo = 0
                let mutable hi = rows.Length - 1
                let mutable found = -1

                while lo <= hi do
                    let mid = (lo + hi) / 2
                    let r = rows[mid]

                    if r.StartLine < oneBased || (r.StartLine = oneBased && r.StartColumn <= character) then
                        found <- mid
                        lo <- mid + 1
                    else
                        hi <- mid - 1

                if found >= 0 then
                    let r = rows[found]

                    if oneBased < r.EndLine || (oneBased = r.EndLine && character <= r.EndColumn) then found else -1
                else
                    -1

            { At =
                fun line character ->
                    if line < 0 || line >= old.Lines.Length then
                        Error Refusal.Gap
                    else
                        match rowOf line character with
                        | -1 -> Error Refusal.Gap
                        | i ->
                            match rowShift[i] with
                            | Error refusal -> Error refusal
                            | Ok shift -> lineAt line shift |> Result.map (fun l -> struct (l, i)) }
