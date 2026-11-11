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

let private isValidBareIdent (name: string) =
    if String.IsNullOrEmpty name then
        false
    else
        let first = name[0]

        if not (Char.IsLetter first || first = '_') then
            false
        else
            let mutable ok = true

            for i in 1 .. name.Length - 1 do
                let c = name[i]

                if not (Char.IsLetterOrDigit c || c = '_' || c = '\'') then
                    ok <- false

            ok

let internal validateAndFormatNewName (rawNewName: string) : Result<string, string> =
    if isNull rawNewName || String.IsNullOrWhiteSpace rawNewName then
        Error "Invalid identifier name: name cannot be empty."
    elif rawNewName.IndexOfAny([| '\r'; '\n' |]) >= 0 then
        Error "Invalid identifier name: name cannot contain newlines."
    else
        let innerRes =
            if rawNewName.StartsWith("``", StringComparison.Ordinal) && rawNewName.EndsWith("``", StringComparison.Ordinal) then
                if rawNewName.Length <= 4 then
                    Error "Invalid identifier name: empty backticked identifier."
                else
                    let body = rawNewName.Substring(2, rawNewName.Length - 4)

                    if String.IsNullOrWhiteSpace body || body.Contains("``", StringComparison.Ordinal) then
                        Error "Invalid identifier name: invalid backticked identifier."
                    else
                        Ok body
            elif rawNewName.Contains("``", StringComparison.Ordinal) then
                Error "Invalid identifier name: unexpected double backticks."
            else
                Ok rawNewName

        match innerRes with
        | Error msg -> Error msg
        | Ok clean ->
            let needsBackticks = fsharpKeywords.Contains clean || not (isValidBareIdent clean)
            Ok(if needsBackticks then $"``%s{clean}``" else clean)

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

/// Finds an Identifier token (never Operator or Keyword) at `(line, character)` and returns `(lineStr, longIdentNames, endColExclusive, tokLspRange)`.
let private lookupIdentifierAt (text: string) (line: int) (character: int) : (string * string list * int * LspRange) option =
    match lexLineAt text line with
    | None -> None
    | Some(lineStr, tokens) ->
        let hitIdx =
            tokens
            |> Array.tryFindIndex (fun t -> t.Left <= character && character <= t.Right)
            |> Option.orElseWith (fun () ->
                if character > 0 then
                    tokens
                    |> Array.tryFindIndex (fun t -> t.Left <= character - 1 && character - 1 <= t.Right && t.Right + 1 = character)
                else
                    None)

        match hitIdx with
        | None -> None
        | Some k ->
            let tok = tokens[k]

            if tok.Kind <> FSharpTokenCharKind.Identifier then
                None
            else
                let mutable first = k

                while first >= 2 && tokens[first - 1].Text = "." && tokens[first - 2].Kind = FSharpTokenCharKind.Identifier do
                    first <- first - 2

                let names = [ for i in first..2 .. k -> unquote tokens[i].Text ]

                let tokRange: LspRange =
                    { Start = { Line = uint32 line; Character = uint32 tok.Left }
                      End = { Line = uint32 line; Character = uint32 (tok.Right + 1) } }

                Some(lineStr, names, tok.Right + 1, tokRange)

let private isForbiddenSymbol (symbol: FSharpSymbol) : bool =
    match symbol with
    | :? FSharpActivePatternCase -> true
    | :? FSharpEntity as ent ->
        ent.IsNamespace
    | :? FSharpMemberOrFunctionOrValue as mfv ->
        mfv.IsConstructor
        || mfv.IsImplicitConstructor
        || mfv.IsActivePattern
        || mfv.CompiledName = ".ctor"
        || mfv.CompiledName = ".cctor"
        || mfv.LogicalName.StartsWith("(|", StringComparison.Ordinal)
        || PrettyNaming.IsOperatorDisplayName mfv.DisplayName
        || PrettyNaming.IsActivePatternName mfv.LogicalName
    | _ -> false

let private cleanSymbolName (symbol: FSharpSymbol) : string option =
    if isForbiddenSymbol symbol then
        None
    else
        let raw =
            let core =
                try
                    symbol.DisplayNameCore
                with _ ->
                    ""

            if String.IsNullOrWhiteSpace core then symbol.DisplayName else core

        let cleaned = unquote raw

        if
            String.IsNullOrWhiteSpace cleaned
            || cleaned = ".ctor"
            || cleaned = ".cctor"
            || cleaned.StartsWith("(", StringComparison.Ordinal)
        then
            None
        else
            Some cleaned

/// Strips any leading `Namespace.Module.` qualifier from an FCS symbol-use range and verifies the unqualified slice matches `expectedCore`.
let private stripQualifierAndVerify (lines: string[]) (expectedCore: string) (r: range) : Result<LspRange option, string> =
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

                if unquotedSlice = expectedCore then
                    let lspRange: LspRange =
                        { Start = { Line = uint32 endLineIdx; Character = uint32 finalStartCol }
                          End = { Line = uint32 endLineIdx; Character = uint32 r.EndColumn } }

                    Ok(Some lspRange)
                elif r.IsSynthetic then
                    Ok None
                else
                    Error $"Source slice '%s{identSlice}' does not match expected symbol '%s{expectedCore}'."

[<NoComparison; NoEquality>]
type private ResolvedRenameTarget =
    | NotRenameable of reason: string
    | LocalTarget of cleanName: string * tokRange: LspRange * text: string * uses: range list
    | WorkspaceTarget of
        key: string *
        landed: string *
        spelled: string option *
        cleanName: string *
        tokRange: LspRange

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
                    | Some cleanName ->
                        let keysIn = SymbolUses.keysIn check ct

                        match keysIn u with
                        | Some key ->
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

                                WorkspaceTarget(key, landedFile, spelled, cleanName, tokRange)
                        | None ->
                            let declaredHere =
                                match u.Symbol.DeclarationLocation with
                                | Some loc -> sameFile loc.FileName path
                                | None -> false

                            if not declaredHere then
                                NotRenameable "Cannot rename symbol declared outside this file."
                            else
                                let localRanges =
                                    [ for d in check.GetUsesOfSymbolInFile(u.Symbol, cancellationToken = ct) -> d.Range ]

                                LocalTarget(cleanName, tokRange, view.Text, localRanges)),
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
                    | Some cleanName ->
                        match SymbolUses.keyOf sym with
                        | None -> NotRenameable "Unkeyed signature symbol cannot be renamed."
                        | Some key ->
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

                                WorkspaceTarget(key, landedFile, spelled, cleanName, tokRange)),
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
    (versionOf: string -> int option)
    (editsPerFile: (string * TextEdit[]) list)
    : WorkspaceEdit =
    if canDocumentChanges then
        let docChanges =
            [| for filePath, edits in editsPerFile ->
                   let uri = Paths.toUri filePath

                   let docId: OptionalVersionedTextDocumentIdentifier =
                       { Uri = uri
                         Version = versionOf filePath }

                   U4.C1
                       { TextDocument = docId
                         Edits = edits |> Array.map U2.C1 } |]

        { Changes = None
          DocumentChanges = Some docChanges
          ChangeAnnotations = None }
    else
        let map =
            editsPerFile
            |> Seq.map (fun (filePath, edits) -> Paths.toUri filePath, edits)
            |> Map.ofSeq

        { Changes = Some map
          DocumentChanges = None
          ChangeAnnotations = None }

let private deduplicateAndBuildEdits (formattedName: string) (ranges: LspRange seq) : TextEdit[] =
    ranges
    |> Seq.distinctBy (fun r -> r.Start.Line, r.Start.Character, r.End.Line, r.End.Character)
    |> Seq.sortBy (fun r -> r.Start.Line, r.Start.Character)
    |> Seq.map (fun r -> { Range = r; NewText = formattedName })
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
        | LocalTarget(cleanName, tokRange, _, _) ->
            return
                Some(
                    U3.C2
                        { Range = tokRange
                          Placeholder = cleanName }
                )
        | WorkspaceTarget(_, landedFile, _, cleanName, tokRange) ->
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
    (versionOf: string -> int option)
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (newName: string)
    (ct: CancellationToken)
    : Task<Result<WorkspaceEdit option, string>> =
    task {
        match validateAndFormatNewName newName with
        | Error msg -> return Error msg
        | Ok formattedName ->
            match! resolveTarget graph path line character ct with
            | NotRenameable reason -> return Error reason
            | LocalTarget(cleanName, _, text, localRanges) ->
                let lines = text.Split '\n'
                let verified = ResizeArray<LspRange>()
                let mutable errOpt: string option = None

                for r in localRanges do
                    if errOpt.IsNone then
                        match stripQualifierAndVerify lines cleanName r with
                        | Ok(Some lspR) -> verified.Add lspR
                        | Ok None -> ()
                        | Error e -> errOpt <- Some e

                match errOpt with
                | Some e -> return Error e
                | None ->
                    let edits = deduplicateAndBuildEdits formattedName verified

                    if edits.Length = 0 then
                        return Ok None
                    else
                        return Ok(Some(buildWorkspaceEdit canDocumentChanges versionOf [ Path.GetFullPath path, edits ]))

            | WorkspaceTarget(key, landedFile, spelled, cleanName, _) ->
                let rec readAndVerify (attempt: int) : Task<Result<WorkspaceEdit option, string>> =
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
                            // Group hits by normalized file path along with the ProjectGraph and settled version from this turn.
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
                                // Read settled TextViews for all touched files in one turn and confirm versions match `ReadInOneTurn`.
                                let byGraph =
                                    byFile
                                    |> Seq.groupBy (fun kv ->
                                        let g, _, _ = kv.Value
                                        g)
                                    |> Seq.map (fun (g, kvs) -> g, [ for kv in kvs -> kv.Key ])
                                    |> Array.ofSeq

                                let! parsesOpt =
                                    graph.LatestParsesInOneTurn((fun () -> struct (scope.IsLoading(), byGraph)), ct)

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
                                            return! readAndVerify (attempt + 1)
                                        else
                                            return Error "Files changed while computing rename."
                                    else
                                        let editsPerFile = ResizeArray<string * TextEdit[]>()
                                        let mutable errOpt: string option = None

                                        for kv in byFile do
                                            if errOpt.IsNone then
                                                let fullPath = kv.Key
                                                let _, _, rawRanges = kv.Value
                                                let tv = textMap[fullPath]
                                                let lines = tv.Text.Split '\n'
                                                let verified = ResizeArray<LspRange>()

                                                for r in rawRanges do
                                                    if errOpt.IsNone then
                                                        match stripQualifierAndVerify lines cleanName r with
                                                        | Ok(Some lspR) -> verified.Add lspR
                                                        | Ok None -> ()
                                                        | Error e -> errOpt <- Some e

                                                if errOpt.IsNone then
                                                    let fileEdits = deduplicateAndBuildEdits formattedName verified

                                                    if fileEdits.Length > 0 then
                                                        editsPerFile.Add(fullPath, fileEdits)

                                        match errOpt with
                                        | Some e -> return Error e
                                        | None ->
                                            if editsPerFile.Count = 0 then
                                                return Ok None
                                            else
                                                let sortedFiles =
                                                    editsPerFile
                                                    |> Seq.sortBy (fun (p, _) -> p.ToUpperInvariant())
                                                    |> List.ofSeq

                                                return Ok(Some(buildWorkspaceEdit canDocumentChanges versionOf sortedFiles))
                    }

                return! readAndVerify 0
    }

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
