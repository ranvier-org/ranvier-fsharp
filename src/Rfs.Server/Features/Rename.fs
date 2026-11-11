module Rfs.Server.Rename

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

[<Struct>]
type private RenameSymbolKind =
    | IdentifierSymbol
    | OperatorSymbol
    | PropertySymbol
    | TypeOrAttributeSymbol

let private isSignature (file: string) =
    file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

let private sameFile (a: string) (b: string) =
    if String.IsNullOrWhiteSpace a || String.IsNullOrWhiteSpace b then
        false
    else
        try
            String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)
        with _ ->
            String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

let private unquote (name: string) =
    if name.Length > 4 && name.StartsWith("``", StringComparison.Ordinal) && name.EndsWith("``", StringComparison.Ordinal) then
        name.Substring(2, name.Length - 4)
    else
        name

let private fsharpKeywords =
    let s = HashSet<string>(StringComparer.Ordinal)

    for kw, _ in FSharpKeywords.KeywordsWithDescription do
        s.Add kw |> ignore

    for extra in
        [ "_"
          "asr"
          "land"
          "lor"
          "lsl"
          "lsr"
          "lxor"
          "mod"
          "sig"
          "break"
          "checked"
          "component"
          "const"
          "constraint"
          "continue"
          "eager"
          "event"
          "external"
          "fixed"
          "functor"
          "include"
          "method"
          "mixin"
          "object"
          "parallel"
          "process"
          "protected"
          "pure"
          "sealed"
          "tailcall"
          "trait"
          "virtual"
          "volatile" ] do
        s.Add extra |> ignore

    s

let private validateAndFormatIdentifier (rawNewName: string) : Result<string * string, string> =
    if isNull rawNewName || String.IsNullOrWhiteSpace rawNewName then
        Error "Invalid identifier name: name cannot be empty."
    elif rawNewName.IndexOfAny([| '\r'; '\n'; '\t' |]) >= 0 then
        Error "Invalid identifier name: name cannot contain newlines or tabs."
    else
        let candidate =
            if rawNewName.StartsWith("``", StringComparison.Ordinal) && rawNewName.EndsWith("``", StringComparison.Ordinal) then
                if rawNewName.Length <= 4 then
                    None
                else
                    let inner = rawNewName.Substring(2, rawNewName.Length - 4)

                    if String.IsNullOrWhiteSpace inner || inner.Contains("`", StringComparison.Ordinal) then
                        None
                    else
                        Some inner
            elif rawNewName.Contains("`", StringComparison.Ordinal) then
                None
            else
                Some rawNewName

        match candidate with
        | None -> Error $"Invalid identifier name: '%s{rawNewName}'."
        | Some clean ->
            let normalized = PrettyNaming.NormalizeIdentifierBackticks clean

            if
                normalized.Length > 4
                && normalized.StartsWith("``", StringComparison.Ordinal)
                && normalized.EndsWith("``", StringComparison.Ordinal)
            then
                let inner = normalized.Substring(2, normalized.Length - 4)

                if String.IsNullOrWhiteSpace inner || inner.Contains("`", StringComparison.Ordinal) then
                    Error $"Invalid identifier name: '%s{rawNewName}'."
                else
                    Ok(normalized, inner)
            elif PrettyNaming.IsIdentifierName clean && not (fsharpKeywords.Contains clean) then
                Ok(clean, clean)
            elif fsharpKeywords.Contains clean then
                Ok($"``%s{clean}``", clean)
            else
                Error $"Invalid identifier name: '%s{rawNewName}'."

let private validateAndFormatNewNameForKind (symKind: RenameSymbolKind) (rawNewName: string) : Result<string * string, string> =
    if isNull rawNewName || String.IsNullOrWhiteSpace rawNewName then
        Error "Invalid rename name: name cannot be empty."
    elif rawNewName.IndexOfAny([| '\r'; '\n'; '\t' |]) >= 0 then
        Error "Invalid rename name: name cannot contain newlines or tabs."
    else
        match symKind with
        | OperatorSymbol ->
            let trimmed = rawNewName.Trim()

            let inner =
                if trimmed.Length > 2 && trimmed.StartsWith("(", StringComparison.Ordinal) && trimmed.EndsWith(")", StringComparison.Ordinal) then
                    trimmed.Substring(1, trimmed.Length - 2).Trim()
                else
                    trimmed

            if
                String.IsNullOrWhiteSpace inner
                || inner.Contains(' ')
                || not (PrettyNaming.IsOperatorDisplayName inner)
            then
                Error $"Invalid operator name: '%s{rawNewName}' is not a valid F# operator."
            else
                Ok(inner, inner)
        | IdentifierSymbol
        | PropertySymbol
        | TypeOrAttributeSymbol -> validateAndFormatIdentifier rawNewName

let internal validateAndFormatNewName (rawNewName: string) : Result<string, string> =
    validateAndFormatIdentifier rawNewName |> Result.map fst

[<Struct>]
type private LexTok =
    { Left: int
      Right: int
      Kind: FSharpTokenCharKind
      Tag: int
      Text: string }

let private lexLineAt (text: string) (targetLine: int) : (string * LexTok[]) option =
    let lines = text.Split '\n'

    if targetLine < 0 || targetLine >= lines.Length then
        None
    else
        let tokenizer = PositionMap.tokenizer ()
        let mutable state = FSharpTokenizerLexState.Initial
        let mutable targetTokens = [||]
        let mutable targetStr = ""

        for i in 0..targetLine do
            let lineStr = lines[i].TrimEnd '\r'
            let lineTok = tokenizer.CreateLineTokenizer lineStr
            let found = ResizeArray<LexTok>()
            let mutable more = true

            while more do
                match lineTok.ScanToken state with
                | Some t, next ->
                    state <- next
                    let right = min t.RightColumn (lineStr.Length - 1)

                    if right >= t.LeftColumn then
                        found.Add
                            { Left = t.LeftColumn
                              Right = right
                              Kind = t.CharClass
                              Tag = t.Tag
                              Text = lineStr.Substring(t.LeftColumn, right - t.LeftColumn + 1) }
                | None, next ->
                    state <- PositionMap.carried next
                    more <- false

            if i = targetLine then
                targetTokens <- found.ToArray()
                targetStr <- lineStr

        Some(targetStr, targetTokens)

/// Finds an Identifier or custom Operator token at `(line, character)` and returns `(lineStr, longIdentNames, endColExclusive, tokLspRange)`.
let private lookupIdentifierAt (text: string) (line: int) (character: int) : (string * string list * int * LspRange) option =
    match lexLineAt text line with
    | None -> None
    | Some(lineStr, tokens) ->
        let nonWsTokens =
            tokens |> Array.filter (fun t -> t.Kind <> FSharpTokenCharKind.WhiteSpace)

        let hitIdx =
            nonWsTokens
            |> Array.tryFindIndex (fun t -> t.Left <= character && character <= t.Right)
            |> Option.orElseWith (fun () ->
                if character > 0 then
                    nonWsTokens
                    |> Array.tryFindIndex (fun t -> t.Left <= character - 1 && character - 1 <= t.Right && t.Right + 1 = character)
                else
                    None)

        match hitIdx with
        | None -> None
        | Some rawIdx ->
            // If cursor is on `(` of `( |+| )`, step onto the operator token inside the parens.
            let k =
                if
                    nonWsTokens[rawIdx].Text = "("
                    && rawIdx + 2 < nonWsTokens.Length
                    && nonWsTokens[rawIdx + 1].Kind = FSharpTokenCharKind.Operator
                    && nonWsTokens[rawIdx + 2].Text = ")"
                then
                    rawIdx + 1
                else
                    rawIdx

            let tok = nonWsTokens[k]

            match tok.Kind with
            | FSharpTokenCharKind.Identifier ->
                let mutable first = k

                while first >= 2 && nonWsTokens[first - 1].Text = "." && nonWsTokens[first - 2].Kind = FSharpTokenCharKind.Identifier do
                    first <- first - 2

                let names = [ for i in first..2 .. k -> unquote nonWsTokens[i].Text ]

                let tokRange: LspRange =
                    { Start = { Line = uint32 line; Character = uint32 tok.Left }
                      End = { Line = uint32 line; Character = uint32 (tok.Right + 1) } }

                Some(lineStr, names, tok.Right + 1, tokRange)

            | FSharpTokenCharKind.Operator when tok.Text <> "." ->
                let names =
                    if
                        k >= 3
                        && nonWsTokens[k - 1].Text = "("
                        && nonWsTokens[k - 2].Text = "."
                        && nonWsTokens[k - 3].Kind = FSharpTokenCharKind.Identifier
                    then
                        let mutable first = k - 3

                        while first >= 2 && nonWsTokens[first - 1].Text = "." && nonWsTokens[first - 2].Kind = FSharpTokenCharKind.Identifier do
                            first <- first - 2

                        [ for i in first..2 .. (k - 3) -> unquote nonWsTokens[i].Text ] @ [ tok.Text ]
                    else
                        [ tok.Text ]

                let tokRange: LspRange =
                    { Start = { Line = uint32 line; Character = uint32 tok.Left }
                      End = { Line = uint32 line; Character = uint32 (tok.Right + 1) } }

                Some(lineStr, names, tok.Right + 1, tokRange)

            | _ -> None

let private isForbiddenSymbol (symbol: FSharpSymbol) : bool =
    match symbol with
    | :? FSharpActivePatternCase -> true
    | :? FSharpEntity as ent -> ent.IsNamespace
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        mfv.IsConstructor
        || mfv.IsImplicitConstructor
        || mfv.IsActivePattern
        || mfv.CompiledName = ".ctor"
        || mfv.CompiledName = ".cctor"
        || mfv.LogicalName.StartsWith("(|", StringComparison.Ordinal)
        || PrettyNaming.IsActivePatternName mfv.LogicalName
    | _ -> false

let private classifySymbol (symbol: FSharpSymbol) : RenameSymbolKind =
    match symbol with
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        if PrettyNaming.IsOperatorDisplayName mfv.DisplayName then
            OperatorSymbol
        elif mfv.IsProperty || mfv.IsPropertyGetterMethod || mfv.IsPropertySetterMethod then
            PropertySymbol
        else
            IdentifierSymbol
    | :? FSharpEntity -> TypeOrAttributeSymbol
    | _ -> IdentifierSymbol

let private cleanSymbolName (symbol: FSharpSymbol) : (string * RenameSymbolKind) option =
    if isForbiddenSymbol symbol then
        None
    else
        let symKind = classifySymbol symbol

        match symKind with
        | OperatorSymbol ->
            let rawOp =
                let d = symbol.DisplayName.Trim()

                if d.Length > 2 && d.StartsWith("(", StringComparison.Ordinal) && d.EndsWith(")", StringComparison.Ordinal) then
                    d.Substring(1, d.Length - 2).Trim()
                else
                    let core =
                        try
                            symbol.DisplayNameCore.Trim()
                        with _ ->
                            ""

                    if String.IsNullOrWhiteSpace core then d else core

            if String.IsNullOrWhiteSpace rawOp then
                None
            else
                Some(rawOp, OperatorSymbol)
        | PropertySymbol
        | IdentifierSymbol
        | TypeOrAttributeSymbol ->
            let raw =
                let core =
                    try
                        symbol.DisplayNameCore
                    with _ ->
                        ""

                if String.IsNullOrWhiteSpace core then symbol.DisplayName else core

            let unquoted = unquote raw

            let cleaned =
                match symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv when
                    (mfv.IsPropertyGetterMethod || mfv.IsPropertySetterMethod)
                    && unquoted.Length > 4
                    && (unquoted.StartsWith("get_", StringComparison.Ordinal) || unquoted.StartsWith("set_", StringComparison.Ordinal))
                    ->
                    unquoted.Substring 4
                | _ -> unquoted

            if
                String.IsNullOrWhiteSpace cleaned
                || cleaned = ".ctor"
                || cleaned = ".cctor"
                || cleaned.StartsWith("(", StringComparison.Ordinal)
            then
                None
            else
                Some(cleaned, symKind)

/// Strips any leading `Namespace.Module.` qualifier (or `( ... )` around an operator), handles property `get`/`set`
/// accessor keywords and `[<Foo>]` attribute shorthand, and verifies the settled text slice matches `expectedCore`.
let private stripQualifierAndVerify
    (symKind: RenameSymbolKind)
    (lines: string[])
    (expectedCore: string)
    (formattedName: string)
    (cleanNewName: string)
    (r: range)
    : Result<TextEdit option, string> =
    if r.IsSynthetic && r.StartLine = r.EndLine && r.StartColumn = r.EndColumn then
        Ok None
    elif r.StartLine < 1 || r.EndLine < r.StartLine || r.EndLine > lines.Length then
        Error $"Symbol occurrence range %A{r} is out of file bounds."
    else
        let endLineIdx = r.EndLine - 1
        let endLine = lines[endLineIdx].TrimEnd '\r'

        if r.EndColumn < 0 || r.EndColumn > endLine.Length then
            Error $"Symbol occurrence end column %d{r.EndColumn} is out of line bounds."
        elif r.StartLine = r.EndLine && r.StartColumn = r.EndColumn then
            Ok None
        else
            let baseStartCol = if r.StartLine = r.EndLine then max 0 r.StartColumn else 0

            if baseStartCol >= r.EndColumn then
                Ok None
            else
                let raw = endLine.Substring(baseStartCol, r.EndColumn - baseStartCol)

                match symKind with
                | OperatorSymbol ->
                    if raw.Contains('(') then
                        let openP = raw.LastIndexOf '('
                        let closeP = raw.LastIndexOf ')'

                        let insideOpt =
                            if closeP > openP then
                                Some(raw.Substring(openP + 1, closeP - openP - 1))
                            elif openP >= 0 then
                                // FCS ends qualified parenthesized operator ranges like `Demo.Ops.(|+|)` right before `)`.
                                Some(raw.Substring(openP + 1))
                            else
                                None

                        match insideOpt with
                        | Some inside ->
                            let leadSpaces = inside.Length - inside.TrimStart(' ').Length
                            let trimmed = inside.Trim ' '

                            if trimmed = expectedCore then
                                let sCol = baseStartCol + openP + 1 + leadSpaces
                                let eCol = sCol + trimmed.Length

                                let lspRange: LspRange =
                                    { Start = { Line = uint32 endLineIdx; Character = uint32 sCol }
                                      End = { Line = uint32 endLineIdx; Character = uint32 eCol } }

                                Ok(Some { Range = lspRange; NewText = formattedName })
                            elif r.IsSynthetic then
                                Ok None
                            else
                                Error $"Operator slice '%s{trimmed}' does not match expected operator '%s{expectedCore}'."
                        | None ->
                            if r.IsSynthetic then
                                Ok None
                            else
                                Error $"Operator slice '%s{raw}' does not match expected operator '%s{expectedCore}'."
                    elif raw = expectedCore then
                        let lspRange: LspRange =
                            { Start = { Line = uint32 endLineIdx; Character = uint32 baseStartCol }
                              End = { Line = uint32 endLineIdx; Character = uint32 r.EndColumn } }

                        Ok(Some { Range = lspRange; NewText = formattedName })
                    elif raw.EndsWith("." + expectedCore, StringComparison.Ordinal) then
                        let sCol = r.EndColumn - expectedCore.Length

                        let lspRange: LspRange =
                            { Start = { Line = uint32 endLineIdx; Character = uint32 sCol }
                              End = { Line = uint32 endLineIdx; Character = uint32 r.EndColumn } }

                        Ok(Some { Range = lspRange; NewText = formattedName })
                    elif r.IsSynthetic then
                        Ok None
                    else
                        Error $"Operator slice '%s{raw}' does not match expected operator '%s{expectedCore}'."

                | IdentifierSymbol
                | PropertySymbol
                | TypeOrAttributeSymbol ->
                    let strippedOffset =
                        if raw.EndsWith("``", StringComparison.Ordinal) && raw.Length >= 4 then
                            let openBt = raw.LastIndexOf("``", raw.Length - 3, StringComparison.Ordinal)
                            if openBt >= 0 then openBt else 0
                        else
                            let lastDot = raw.LastIndexOf '.'
                            if lastDot >= 0 then lastDot + 1 else 0

                    let finalStartCol = baseStartCol + strippedOffset
                    let identSlice = raw.Substring strippedOffset
                    let unquotedSlice = unquote identSlice

                    let lspRange: LspRange =
                        { Start = { Line = uint32 endLineIdx; Character = uint32 finalStartCol }
                          End = { Line = uint32 endLineIdx; Character = uint32 r.EndColumn } }

                    if unquotedSlice = expectedCore then
                        Ok(Some { Range = lspRange; NewText = formattedName })
                    elif
                        symKind = PropertySymbol
                        && expectedCore <> "get"
                        && expectedCore <> "set"
                        && (unquotedSlice = "get" || unquotedSlice = "set")
                    then
                        Ok None
                    elif
                        symKind = TypeOrAttributeSymbol
                        && expectedCore.Length > 9
                        && expectedCore.EndsWith("Attribute", StringComparison.Ordinal)
                        && unquotedSlice = expectedCore.Substring(0, expectedCore.Length - 9)
                    then
                        let shortReplacement =
                            if cleanNewName.Length > 9 && cleanNewName.EndsWith("Attribute", StringComparison.Ordinal) then
                                let shortCore = cleanNewName.Substring(0, cleanNewName.Length - 9)

                                match validateAndFormatIdentifier shortCore with
                                | Ok(fmtShort, _) -> fmtShort
                                | Error _ -> formattedName
                            else
                                formattedName

                        Ok(Some { Range = lspRange; NewText = shortReplacement })
                    elif r.IsSynthetic then
                        Ok None
                    else
                        Error $"Source slice '%s{identSlice}' does not match expected symbol '%s{expectedCore}'."

[<NoComparison; NoEquality>]
type private ResolvedRenameTarget =
    | NotRenameable of reason: string
    | LocalTarget of
        symKind: RenameSymbolKind *
        cleanName: string *
        tokRange: LspRange *
        text: string *
        viewVersion: int64 *
        uses: range list
    | WorkspaceTarget of
        symKind: RenameSymbolKind *
        key: string *
        landed: string *
        spelled: string option *
        cleanName: string *
        tokRange: LspRange

let private collectLocalSymbolUses
    (check: FSharpCheckFileResults)
    (sym: FSharpSymbol)
    (cleanName: string)
    (symKind: RenameSymbolKind)
    (ct: CancellationToken)
    : range list =
    let symbolsToQuery =
        if symKind = PropertySymbol then
            match sym with
            | :? FSharpMemberOrFunctionOrValue as mfv ->
                match mfv.DeclaringEntity with
                | Some ent ->
                    ent.MembersFunctionsAndValues
                    |> Seq.filter (fun m ->
                        (m.IsProperty || m.IsPropertyGetterMethod || m.IsPropertySetterMethod)
                        && (match cleanSymbolName m with
                            | Some(cn, _) -> cn = cleanName
                            | None -> false))
                    |> Seq.cast<FSharpSymbol>
                    |> List.ofSeq
                | None -> [ sym ]
            | _ -> [ sym ]
        else
            [ sym ]

    [ for s in symbolsToQuery do
          for d in check.GetUsesOfSymbolInFile(s, cancellationToken = ct) do
              yield d.Range ]

let private resolveInFs (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<ResolvedRenameTarget> =
    graph.ReadFile(
        path,
        (fun view -> lookupIdentifierAt view.Text line character),
        (fun view prep ->
            match prep with
            | None -> NotRenameable "No renameable identifier at cursor."
            | Some(lineStr, names, endColumn, tokRange) ->
                let check =
                    if view.Whole then
                        view.Result.CheckResultsNotKept
                    else
                        view.CheckResultsAt(line + 1, endColumn)

                match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
                | None -> NotRenameable "No symbol found at cursor."
                | Some u ->
                    match cleanSymbolName u.Symbol with
                    | None -> NotRenameable "This symbol kind cannot be renamed."
                    | Some(cleanName, symKind) ->
                        let keysIn = SymbolUses.keysIn check ct

                        match keysIn u with
                        | Some key ->
                            let effectiveKind =
                                let parts = key.Split '|'
                                if parts.Length > 1 && parts[1] = "property" then PropertySymbol else symKind

                            let landedOpt =
                                u.Symbol.SignatureLocation
                                |> Option.orElse u.Symbol.DeclarationLocation
                                |> Option.map _.FileName

                            match landedOpt with
                            | None -> NotRenameable "Cannot rename external symbol defined outside the workspace."
                            | Some landedFile ->
                                let spelled =
                                    SymbolUses.spelledName u.Symbol
                                    |> Option.filter (fun n -> List.tryLast names = Some n)

                                WorkspaceTarget(effectiveKind, key, landedFile, spelled, cleanName, tokRange)
                        | None ->
                            let declaredHere =
                                match u.Symbol.DeclarationLocation with
                                | Some loc -> sameFile loc.FileName path
                                | None -> false

                            if not declaredHere then
                                NotRenameable "Cannot rename symbol declared outside this file."
                            else
                                let localRanges = collectLocalSymbolUses check u.Symbol cleanName symKind ct
                                LocalTarget(symKind, cleanName, tokRange, view.Text, view.Version, localRanges)),
        ct
    )

let private resolveInFsi (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<ResolvedRenameTarget> =
    graph.ReadSignature(
        path,
        (fun view -> lookupIdentifierAt view.Text line character),
        (fun _ prep _ sigUses ->
            match prep with
            | None -> NotRenameable "No renameable identifier at cursor."
            | Some(_, names, endColumn, tokRange) ->
                let fcsLine = line + 1

                let hitOpt =
                    sigUses
                    |> Array.tryFind (fun (struct (r, _, _)) ->
                        r.StartLine <= fcsLine
                        && fcsLine <= r.EndLine
                        && (r.EndLine <> fcsLine || r.EndColumn = endColumn || (r.StartColumn <= character && character <= r.EndColumn)))

                match hitOpt with
                | None -> NotRenameable "No symbol found at cursor in signature file."
                | Some(struct (_, isDef, sym)) ->
                    match cleanSymbolName sym with
                    | None -> NotRenameable "This symbol kind cannot be renamed."
                    | Some(cleanName, symKind) ->
                        match SymbolUses.keyOf sym with
                        | None -> NotRenameable "Unkeyed signature symbol cannot be renamed."
                        | Some key ->
                            let effectiveKind =
                                let parts = key.Split '|'
                                if parts.Length > 1 && parts[1] = "property" then PropertySymbol else symKind

                            let landedOpt =
                                if isDef then
                                    Some path
                                else
                                    sym.SignatureLocation
                                    |> Option.orElse sym.DeclarationLocation
                                    |> Option.map _.FileName

                            match landedOpt with
                            | None -> NotRenameable "Cannot rename external symbol defined outside the workspace."
                            | Some landedFile ->
                                let spelled =
                                    SymbolUses.spelledName sym
                                    |> Option.filter (fun n -> List.tryLast names = Some n)

                                WorkspaceTarget(effectiveKind, key, landedFile, spelled, cleanName, tokRange)),
        ct
    )

let private resolveTarget (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<ResolvedRenameTarget> =
    if isSignature path then
        resolveInFsi graph path line character ct
    else
        resolveInFs graph path line character ct

let private setterSuffix = "|setter"

let private withoutSetter (key: string) =
    if key.EndsWith(setterSuffix, StringComparison.Ordinal) then
        key.Substring(0, key.Length - setterSuffix.Length)
    else
        key

let private hitsOfKey (key: string) (uses: FileUses) : range[] =
    let plain = withoutSetter key
    let own = uses.Of plain
    let setter = uses.Of(plain + setterSuffix)

    [| for struct (r, _) in own.Hits -> r
       for struct (r, _) in setter.Hits -> r |]

let private buildWorkspaceEdit
    (canDocumentChanges: bool)
    (editsPerFile: (string * int option * TextEdit[]) list)
    : WorkspaceEdit =
    if canDocumentChanges then
        let docChanges =
            [| for filePath, verOpt, edits in editsPerFile ->
                   let uri = Paths.toUri filePath

                   let docId: OptionalVersionedTextDocumentIdentifier =
                       { Uri = uri
                         Version = verOpt }

                   U4.C1
                       { TextDocument = docId
                         Edits = edits |> Array.map U2.C1 } |]

        { Changes = None
          DocumentChanges = Some docChanges
          ChangeAnnotations = None }
    else
        let map =
            editsPerFile
            |> Seq.map (fun (filePath, _, edits) -> Paths.toUri filePath, edits)
            |> Map.ofSeq

        { Changes = Some map
          DocumentChanges = None
          ChangeAnnotations = None }

let private deduplicateEdits (edits: TextEdit seq) : TextEdit[] =
    edits
    |> Seq.distinctBy (fun e -> e.Range.Start.Line, e.Range.Start.Character, e.Range.End.Line, e.Range.End.Character)
    |> Seq.sortBy (fun e -> e.Range.Start.Line, e.Range.Start.Character)
    |> Array.ofSeq

let prepare
    (ownersOf: string -> ProjectGraph list)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (ct: CancellationToken)
    : Task<PrepareRenameResult option> =
    task {
        match! resolveTarget graph path line character ct with
        | NotRenameable _ -> return None
        | LocalTarget(_, cleanName, tokRange, _, _, _) ->
            return
                Some(
                    U3.C2
                        { Range = tokRange
                          Placeholder = cleanName }
                )
        | WorkspaceTarget(_, _, landedFile, _, cleanName, tokRange) ->
            // Graph-thread affinity: `ownersOf` may read `Ranvier.Memo.Value` (`References.Scope.OwnersOf` / `workspace.OwnersNow`),
            // so always evaluate it inside `graph.Host.WhenSettled` on the GraphHost thread.
            let! definedInWorkspace =
                graph.Host.WhenSettled((fun () -> ValueSome(not (ownersOf landedFile).IsEmpty)), ct)

            if not definedInWorkspace then
                return None
            else
                return
                    Some(
                        U3.C2
                            { Range = tokRange
                              Placeholder = cleanName }
                    )
    }

let internal renameWithCapabilities
    (canDocumentChanges: bool)
    (versionNow: string -> int option)
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (newName: string)
    (ct: CancellationToken)
    : Task<Result<WorkspaceEdit option, string>> =
    let rec readWorkspaceTarget
        (symKind: RenameSymbolKind)
        (key: string)
        (landedFile: string)
        (spelled: string option)
        (cleanName: string)
        (attempt: int)
        : Task<Result<WorkspaceEdit option, string>> =
        task {
            let turnScope () : TurnScope<bool> =
                if scope.IsLoading() then
                    { Loading = true
                      Projects = [||]
                      Name = None
                      Extra = false }
                else
                    let definers = scope.OwnersOf landedFile

                    if definers.IsEmpty then
                        { Loading = false
                          Projects = [||]
                          Name = None
                          Extra = false }
                    else
                        let askingGraph =
                            match scope.OwnersOf path with
                            | g :: _ -> g
                            | [] -> graph

                        let projects =
                            askingGraph :: definers @ (definers |> List.collect scope.ReferencingProjects)
                            |> List.distinct

                        { Loading = false
                          Projects = [| for p in projects -> p, scope.FilesOf p |]
                          Name = spelled
                          Extra = true }

            match! graph.ReadInOneTurn(turnScope, ct) with
            | TurnRead.Gone -> return Error "Workspace changed while computing rename."
            | TurnRead.Read(false, _) ->
                return Error "Cannot rename external symbol defined outside the workspace."
            | TurnRead.Read(true, projectFiles) ->
                match validateAndFormatNewNameForKind symKind newName with
                | Error msg -> return Error msg
                | Ok(formattedName, cleanNewName) ->
                    let byFile = Dictionary<string, ProjectGraph * int64 * ResizeArray<range>>(StringComparer.OrdinalIgnoreCase)

                    for g, turnFiles in projectFiles do
                        for tf in turnFiles do
                            match tf with
                            | TurnFile.Indexed(filePath, ver, uses)
                            | TurnFile.Signature(filePath, ver, uses) ->
                                let hits = hitsOfKey key uses

                                if hits.Length > 0 then
                                    let full = Path.GetFullPath filePath

                                    match byFile.TryGetValue full with
                                    | true, (_, _, acc) -> acc.AddRange hits
                                    | _ ->
                                        let acc = ResizeArray<range>(hits.Length)
                                        acc.AddRange hits
                                        byFile[full] <- (g, ver, acc)
                            | TurnFile.Skipped _ -> ()

                    if byFile.Count = 0 then
                        return Ok None
                    else
                        let byGraph =
                            byFile
                            |> Seq.groupBy (fun kv ->
                                let g, _, _ = kv.Value
                                g)
                            |> Seq.map (fun (g, kvs) -> g, [ for kv in kvs -> kv.Key ])
                            |> Array.ofSeq

                        let capturedVersions = Dictionary<string, int option>(StringComparer.OrdinalIgnoreCase)

                        let! parsesOpt =
                            graph.LatestParsesInOneTurn(
                                (fun () ->
                                    capturedVersions.Clear()
                                    let loading = scope.IsLoading()

                                    if not loading then
                                        for _, paths in byGraph do
                                            for p in paths do
                                                capturedVersions[p] <- versionNow p

                                    struct (loading, byGraph)),
                                ct
                            )

                        match parsesOpt with
                        | ValueNone -> return Error "Workspace changed while computing rename."
                        | ValueSome parsedProjects ->
                            let textMap = Dictionary<string, TextView>(StringComparer.OrdinalIgnoreCase)

                            for _, fileViews in parsedProjects do
                                for filePath, tv in fileViews do
                                    textMap[Path.GetFullPath filePath] <- tv

                            let versionsMatch =
                                byFile
                                |> Seq.forall (fun kv ->
                                    let _, expectedVer, _ = kv.Value

                                    match textMap.TryGetValue kv.Key with
                                    | true, tv -> tv.Version = expectedVer
                                    | _ -> false)

                            if not versionsMatch then
                                if attempt < 3 then
                                    return! readWorkspaceTarget symKind key landedFile spelled cleanName (attempt + 1)
                                else
                                    return Error "Files changed while computing rename."
                            else
                                let editsPerFile = ResizeArray<string * int option * TextEdit[]>()
                                let mutable errOpt: string option = None

                                for kv in byFile do
                                    if errOpt.IsNone then
                                        let fullPath = kv.Key
                                        let _, _, rawRanges = kv.Value
                                        let tv = textMap[fullPath]
                                        let lines = tv.Text.Split '\n'
                                        let verified = ResizeArray<TextEdit>()

                                        for r in rawRanges do
                                            if errOpt.IsNone then
                                                match stripQualifierAndVerify symKind lines cleanName formattedName cleanNewName r with
                                                | Ok(Some edit) -> verified.Add edit
                                                | Ok None -> ()
                                                | Error e -> errOpt <- Some e

                                        if errOpt.IsNone then
                                            let fileEdits = deduplicateEdits verified

                                            if fileEdits.Length > 0 then
                                                let verOpt =
                                                    match capturedVersions.TryGetValue fullPath with
                                                    | true, v -> v
                                                    | _ -> None

                                                editsPerFile.Add(fullPath, verOpt, fileEdits)

                                match errOpt with
                                | Some e -> return Error e
                                | None ->
                                    if editsPerFile.Count = 0 then
                                        return Ok None
                                    else
                                        let sortedFiles =
                                            editsPerFile
                                            |> Seq.sortBy (fun (p, _, _) -> p.ToUpperInvariant())
                                            |> List.ofSeq

                                        return Ok(Some(buildWorkspaceEdit canDocumentChanges sortedFiles))
        }

    let rec runAttempt (outerAttempt: int) : Task<Result<WorkspaceEdit option, string>> =
        task {
            match! resolveTarget graph path line character ct with
            | NotRenameable reason -> return Error reason
            | LocalTarget(symKind, cleanName, _, text, viewVer, localRanges) ->
                match validateAndFormatNewNameForKind symKind newName with
                | Error msg -> return Error msg
                | Ok(formattedName, cleanNewName) ->
                    let fullPath = Path.GetFullPath path
                    let mutable capturedLspVersion: int option = None

                    let! parsesOpt =
                        graph.LatestParsesInOneTurn(
                            (fun () ->
                                let loading = scope.IsLoading()

                                if not loading then
                                    capturedLspVersion <- versionNow fullPath

                                struct (loading, [| graph, [ fullPath ] |])),
                            ct
                        )

                    match parsesOpt with
                    | ValueNone -> return Error "Workspace changed while computing rename."
                    | ValueSome parsedProjects ->
                        let tvOpt =
                            parsedProjects
                            |> Seq.collect snd
                            |> Seq.tryPick (fun (fp, tv) -> if sameFile fp fullPath then Some tv else None)

                        match tvOpt with
                        | Some tv when tv.Version <> viewVer ->
                            if outerAttempt < 3 then
                                return! runAttempt (outerAttempt + 1)
                            else
                                return Error "File changed while computing rename."
                        | _ ->
                            let lines = text.Split '\n'
                            let verified = ResizeArray<TextEdit>()
                            let mutable errOpt: string option = None

                            for r in localRanges do
                                if errOpt.IsNone then
                                    match stripQualifierAndVerify symKind lines cleanName formattedName cleanNewName r with
                                    | Ok(Some edit) -> verified.Add edit
                                    | Ok None -> ()
                                    | Error e -> errOpt <- Some e

                            match errOpt with
                            | Some e -> return Error e
                            | None ->
                                let edits = deduplicateEdits verified

                                if edits.Length = 0 then
                                    return Ok None
                                else
                                    return Ok(Some(buildWorkspaceEdit canDocumentChanges [ fullPath, capturedLspVersion, edits ]))

            | WorkspaceTarget(symKind, key, landedFile, spelled, cleanName, _) ->
                return! readWorkspaceTarget symKind key landedFile spelled cleanName 0
        }

    runAttempt 0

let rename
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (newName: string)
    (ct: CancellationToken)
    : Task<Result<WorkspaceEdit option, string>> =
    renameWithCapabilities false (fun _ -> None) scope graph path line character newName ct
