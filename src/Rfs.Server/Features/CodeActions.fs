module Rfs.Server.CodeActions

#nowarn "57"

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Diagnostics.ExtendedData
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

[<NoComparison; ReferenceEquality>]
type private RawAction =
    { Title: string
      Kind: string
      Diagnostics: Diagnostic[] option
      IsPreferred: bool option
      TargetPath: string
      Edits: TextEdit[] }

[<NoComparison; ReferenceEquality>]
type private SigSyncCandidate =
    | UpdateSigVal of valName: string * sigDeclRangeOpt: (int * int * int * int) option * valText: string * diagOpt: Diagnostic option
    | AddSigVal of modulePath: string list * valName: string * valText: string

let private isSignature (file: string) =
    file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

let private isScriptFile (file: string) =
    file.EndsWith(".fsx", StringComparison.OrdinalIgnoreCase)
    || file.EndsWith(".fsscript", StringComparison.OrdinalIgnoreCase)

let inline private fcsPosToLsp (pos: pos) : LspPos =
    { Line = uint32 (max 0 (pos.Line - 1))
      Character = uint32 (max 0 pos.Column) }

let inline private fcsRangeToLsp (r: range) : LspRange =
    { Start = fcsPosToLsp r.Start
      End = fcsPosToLsp r.End }

let inline private posLeq (a: LspPos) (b: LspPos) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let inline private lspRangesOverlap (a: LspRange) (b: LspRange) =
    posLeq a.Start b.End && posLeq b.Start a.End

let private fcsOverlapsLsp (lspRange: LspRange) (r: range) =
    if r.IsSynthetic then
        false
    else
        lspRangesOverlap lspRange (fcsRangeToLsp r)

let private getLineStr (lines: string[]) (line1Based: int) : string =
    if line1Based >= 1 && line1Based <= lines.Length then
        lines[line1Based - 1].TrimEnd '\r'
    else
        ""

let private sliceFcsRange (lines: string[]) (r: range) : string =
    if r.StartLine < 1 || r.EndLine > lines.Length || r.StartLine > r.EndLine then
        ""
    elif r.StartLine = r.EndLine then
        let ln = getLineStr lines r.StartLine
        let sc = max 0 (min ln.Length r.StartColumn)
        let ec = max sc (min ln.Length r.EndColumn)
        ln.Substring(sc, ec - sc)
    else
        let sb = Text.StringBuilder()

        for lineNo in r.StartLine .. r.EndLine do
            let ln = getLineStr lines lineNo

            if lineNo = r.StartLine then
                let sc = max 0 (min ln.Length r.StartColumn)
                sb.Append(ln.Substring sc).Append('\n') |> ignore
            elif lineNo = r.EndLine then
                let ec = max 0 (min ln.Length r.EndColumn)
                sb.Append(ln.Substring(0, ec)) |> ignore
            else
                sb.Append(ln).Append('\n') |> ignore

        sb.ToString()

let private sliceLspRange (lines: string[]) (r: LspRange) : string =
    let sl = int r.Start.Line
    let el = int r.End.Line

    if sl < 0 || el >= lines.Length || sl > el then
        ""
    elif sl = el then
        let ln = lines[sl].TrimEnd '\r'
        let sc = max 0 (min ln.Length (int r.Start.Character))
        let ec = max sc (min ln.Length (int r.End.Character))
        ln.Substring(sc, ec - sc)
    else
        ""

let private deleteWholeLinesEdit (lines: string[]) (startLine0: int) (endLine0: int) : TextEdit =
    let sl = max 0 startLine0
    let el = min (lines.Length - 1) (max sl endLine0)

    if el + 1 < lines.Length then
        { Range =
            { Start = { Line = uint32 sl; Character = 0u }
              End = { Line = uint32 (el + 1); Character = 0u } }
          NewText = "" }
    elif sl > 0 then
        let prevLen = lines[sl - 1].TrimEnd('\r').Length
        let endLen = lines[el].TrimEnd('\r').Length

        { Range =
            { Start = { Line = uint32 (sl - 1); Character = uint32 prevLen }
              End = { Line = uint32 el; Character = uint32 endLen } }
          NewText = "" }
    else
        let endLen = lines[el].TrimEnd('\r').Length

        { Range =
            { Start = { Line = 0u; Character = 0u }
              End = { Line = uint32 el; Character = uint32 endLen } }
          NewText = "" }

let private fcsDiagToLsp (d: FSharpDiagnostic) : Diagnostic =
    let sev =
        match d.Severity with
        | FSharpDiagnosticSeverity.Error -> DiagnosticSeverity.Error
        | FSharpDiagnosticSeverity.Warning -> DiagnosticSeverity.Warning
        | FSharpDiagnosticSeverity.Info -> DiagnosticSeverity.Information
        | FSharpDiagnosticSeverity.Hidden -> DiagnosticSeverity.Hint

    { Range = fcsRangeToLsp d.Range
      Severity = Some sev
      Code = Some(U2.C2 $"FS%04d{d.ErrorNumber}")
      CodeDescription = None
      Source = Some "F# Compiler"
      Message = d.Message
      Tags = None
      RelatedInformation = None
      Data = None }

let private contextDiagCode (d: Diagnostic) : string =
    match d.Code with
    | Some(U2.C1 i) -> string i
    | Some(U2.C2 s) -> s
    | None -> ""

let private leadingWhitespace (s: string) : string =
    let mutable i = 0

    while i < s.Length && (s[i] = ' ' || s[i] = '\t') do
        i <- i + 1

    s.Substring(0, i)

let private unquoteBackticks (s: string) : string =
    if s.Length > 4 && s.StartsWith("``", StringComparison.Ordinal) && s.EndsWith("``", StringComparison.Ordinal) then
        s.Substring(2, s.Length - 4)
    else
        s

let private buildWorkspaceEdit
    (canDocumentChanges: bool)
    (files: (string * int option * TextEdit[]) list)
    : WorkspaceEdit =
    if canDocumentChanges then
        let docChanges =
            files
            |> List.map (fun (filePath, verOpt, edits) ->
                let tde: TextDocumentEdit =
                    { TextDocument =
                        { Uri = Paths.toUri filePath
                          Version = verOpt }
                      Edits = edits |> Array.map U2.C1 }

                U4.C1 tde)
            |> Array.ofList

        { Changes = None
          DocumentChanges = Some docChanges
          ChangeAnnotations = None }
    else
        let map =
            files
            |> List.map (fun (filePath, _, edits) -> Paths.toUri filePath, edits)
            |> Map.ofList

        { Changes = Some map
          DocumentChanges = None
          ChangeAnnotations = None }

let private matchesOnlyFilter (onlyOpt: string[] option) (actionKind: string) : bool =
    match onlyOpt with
    | None -> true
    | Some kinds when kinds.Length = 0 -> true
    | Some kinds ->
        kinds
        |> Array.exists (fun wanted ->
            String.Equals(actionKind, wanted, StringComparison.Ordinal)
            || actionKind.StartsWith(wanted + ".", StringComparison.Ordinal))

/// Finds the innermost enclosing `SynModuleOrNamespace` or `SynModuleDecl.NestedModule` declarations list
/// containing `targetPos`, returning `(openDeclsBeforeTarget, allDecls)` for `open` statement insertion.
let private findEnclosingModuleDecls (ast: ParsedInput) (targetPos: pos) : (SynOpenDeclTarget * range) list * SynModuleDecl list =
    let rec walkDecls (decls: SynModuleDecl list) : ((SynOpenDeclTarget * range) list * SynModuleDecl list) option =
        let nestedHit =
            decls
            |> List.tryPick (fun d ->
                match d with
                | SynModuleDecl.NestedModule(decls = innerDecls; range = r) when Range.rangeContainsPos r targetPos ->
                    walkDecls innerDecls |> Option.orElse (Some(collectOpens innerDecls, innerDecls))
                | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(decls = innerDecls; range = r)) when
                    Range.rangeContainsPos r targetPos
                    ->
                    walkDecls innerDecls |> Option.orElse (Some(collectOpens innerDecls, innerDecls))
                | _ -> None)

        nestedHit |> Option.orElse (Some(collectOpens decls, decls))

    and collectOpens (decls: SynModuleDecl list) =
        [ for d in decls do
              match d with
              | SynModuleDecl.Open(target, r) when r.StartLine <= targetPos.Line -> yield target, r
              | _ -> () ]

    match ast with
    | ParsedInput.SigFile _ -> [], []
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        modules
        |> List.tryPick (fun (SynModuleOrNamespace(decls = decls; range = r)) ->
            if Range.rangeContainsPos r targetPos then
                walkDecls decls
            else
                None)
        |> Option.orElseWith (fun () ->
            match modules with
            | SynModuleOrNamespace(decls = decls) :: _ -> walkDecls decls
            | [] -> None)
        |> Option.defaultValue ([], [])

let private computeOpenInsertEdit (lines: string[]) (ast: ParsedInput) (targetPos: pos) (nsToOpen: string) : TextEdit option =
    let opens, decls = findEnclosingModuleDecls ast targetPos

    let alreadyOpened =
        opens
        |> List.exists (fun (target, _) ->
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(id = ids), _) ->
                let openedName = ids |> List.map _.idText |> String.concat "."
                String.Equals(openedName, nsToOpen, StringComparison.Ordinal)
            | _ -> false)

    if alreadyOpened then
        None
    else
        match List.tryLast opens with
        | Some(_, lastOpenRange) ->
            let indent = leadingWhitespace (getLineStr lines lastOpenRange.StartLine)
            let insertLine = min lines.Length lastOpenRange.EndLine

            Some
                { Range =
                    { Start = { Line = uint32 insertLine; Character = 0u }
                      End = { Line = uint32 insertLine; Character = 0u } }
                  NewText = $"%s{indent}open %s{nsToOpen}\n" }
        | None ->
            match decls with
            | firstDecl :: _ ->
                let line0 = max 0 (firstDecl.Range.StartLine - 1)
                let indent = leadingWhitespace (getLineStr lines firstDecl.Range.StartLine)

                Some
                    { Range =
                        { Start = { Line = uint32 line0; Character = 0u }
                          End = { Line = uint32 line0; Character = 0u } }
                      NewText = $"%s{indent}open %s{nsToOpen}\n\n" }
            | [] ->
                let insertLine = if lines.Length > 0 then 1 else 0

                Some
                    { Range =
                        { Start = { Line = uint32 insertLine; Character = 0u }
                          End = { Line = uint32 insertLine; Character = 0u } }
                      NewText = $"\nopen %s{nsToOpen}\n" }

let private hasEntityAttr<'Attr> (entity: FSharpEntity) =
    try
        entity.HasAttribute<'Attr>()
    with _ ->
        false

/// Searches current `PartialAssemblySignature` and referenced assemblies for types, modules, union cases,
/// and module functions/values matching `unresolvedIdent`, returning `(openableNamespaceOpt, fullyQualifiedIdent)` pairs.
let private resolveUnopenedNamespaceCandidates
    (check: FSharpCheckFileResults)
    (unresolvedIdent: string)
    : (string option * string) list =
    let cleanTarget = unquoteBackticks unresolvedIdent
    let results = ResizeArray<int * string option * string>()
    let seen = HashSet<string>(StringComparer.Ordinal)

    let addCandidate (priority: int) (openNs: string option) (fqName: string) =
        let key = sprintf "%O|%s" openNs fqName

        if seen.Add key then
            results.Add(priority, openNs, fqName)

    let rec walkEntity
        (isLocalAssembly: bool)
        (nsPrefix: string list)
        (rqaBoundary: string list option)
        (entity: FSharpEntity)
        =
        let accessible =
            try
                isLocalAssembly || entity.Accessibility.IsPublic
            with _ ->
                false

        if accessible then
            let entDisplayName =
                try
                    entity.DisplayName
                with _ ->
                    ""

            if not (String.IsNullOrEmpty entDisplayName) then
                let basePath =
                    if nsPrefix.IsEmpty then
                        match entity.Namespace with
                        | Some ns when not (String.IsNullOrWhiteSpace ns) ->
                            (ns.Split '.' |> List.ofArray) @ [ entDisplayName ]
                        | _ -> [ entDisplayName ]
                    else
                        nsPrefix @ [ entDisplayName ]

                let parentPath =
                    if basePath.Length > 1 then
                        basePath |> List.take (basePath.Length - 1)
                    else
                        []

                let prio =
                    if isLocalAssembly then 0
                    elif not parentPath.IsEmpty && parentPath.Head = "System" then 1
                    else 2

                let computeOpenAndQual (fullParts: string list) (rqaOpt: string list option) =
                    match rqaOpt with
                    | None ->
                        if fullParts.Length > 1 then
                            let openParts = fullParts |> List.take (fullParts.Length - 1)
                            Some(String.concat "." openParts), String.concat "." fullParts
                        else
                            None, String.concat "." fullParts
                    | Some rqaParts ->
                        let openOpt =
                            if rqaParts.Length > 1 then
                                Some(String.concat "." (rqaParts |> List.take (rqaParts.Length - 1)))
                            else
                                None

                        openOpt, String.concat "." fullParts

                // Match type or module itself
                if String.Equals(entDisplayName, cleanTarget, StringComparison.Ordinal) && not parentPath.IsEmpty then
                    let openOpt, fq = computeOpenAndQual basePath rqaBoundary
                    addCandidate prio openOpt fq

                let isMod =
                    try
                        entity.IsFSharpModule
                    with _ ->
                        false

                if isMod then
                    let isRqa = hasEntityAttr<RequireQualifiedAccessAttribute> entity

                    let nextRqa =
                        match rqaBoundary with
                        | Some existing -> Some existing
                        | None when isRqa -> Some basePath
                        | None -> None

                    // Inspect module values & functions
                    let mfvs =
                        try
                            entity.TryGetMembersFunctionsAndValues()
                        with _ ->
                            [||] :> IList<_>

                    for mfv in mfvs do
                        let mfvAccessible =
                            try
                                (isLocalAssembly || mfv.Accessibility.IsPublic)
                                && not mfv.IsInstanceMember
                                && not mfv.IsPropertyGetterMethod
                                && not mfv.IsPropertySetterMethod
                            with _ ->
                                false

                        if mfvAccessible then
                            let mfvName =
                                try
                                    mfv.DisplayName
                                with _ ->
                                    ""

                            if String.Equals(mfvName, cleanTarget, StringComparison.Ordinal) then
                                let fullValParts = basePath @ [ unresolvedIdent ]
                                let openOpt, fq = computeOpenAndQual fullValParts nextRqa
                                addCandidate prio openOpt fq

                    // Active pattern cases in module
                    let apCases =
                        try
                            entity.ActivePatternCases
                        with _ ->
                            []

                    for ap in apCases do
                        if String.Equals(ap.Name, cleanTarget, StringComparison.Ordinal) then
                            let fullApParts = basePath @ [ unresolvedIdent ]
                            let openOpt, fq = computeOpenAndQual fullApParts nextRqa
                            addCandidate prio openOpt fq

                    // Nested entities inside module
                    let nested =
                        try
                            entity.NestedEntities
                        with _ ->
                            [||] :> IList<_>

                    for child in nested do
                        walkEntity isLocalAssembly basePath nextRqa child
                else
                    // Union cases on non-RQA F# union types
                    let isUnion =
                        try
                            entity.IsFSharpUnion
                        with _ ->
                            false

                    if isUnion && not (hasEntityAttr<RequireQualifiedAccessAttribute> entity) then
                        let ucs =
                            try
                                entity.UnionCases
                            with _ ->
                                [||] :> IList<_>

                        for uc in ucs do
                            if String.Equals(uc.Name, cleanTarget, StringComparison.Ordinal) && not parentPath.IsEmpty then
                                let openOpt =
                                    match rqaBoundary with
                                    | None -> Some(String.concat "." parentPath)
                                    | Some rqaParts when rqaParts.Length > 1 ->
                                        Some(String.concat "." (rqaParts |> List.take (rqaParts.Length - 1)))
                                    | _ -> None

                                let fq = String.concat "." (basePath @ [ uc.Name ])
                                addCandidate prio openOpt fq

    let localEntities =
        try
            check.PartialAssemblySignature.TryGetEntities()
        with _ ->
            Seq.empty

    for ent in localEntities do
        walkEntity true [] None ent

    let refAssemblies =
        try
            check.ProjectContext.GetReferencedAssemblies()
        with _ ->
            []

    for asm in refAssemblies do
        let ents =
            try
                asm.Contents.TryGetEntities()
            with _ ->
                Seq.empty

        for ent in ents do
            walkEntity false [] None ent

    results
    |> Seq.sortBy (fun (prio, openOpt, fq) -> prio, Option.defaultValue "~" openOpt, fq)
    |> Seq.truncate 12
    |> Seq.map (fun (_, openOpt, fq) -> openOpt, fq)
    |> Seq.toList

let private parseSpellcheckSuggestions (message: string) : string list =
    let marker = "Maybe you want one of the following:"
    let idx = message.IndexOf(marker, StringComparison.Ordinal)

    if idx < 0 then
        []
    else
        message.Substring(idx + marker.Length).Split('\n')
        |> Array.map (fun s -> s.Trim())
        |> Array.filter (fun s ->
            not (String.IsNullOrWhiteSpace s)
            && not (s.Contains(' ') && not (s.StartsWith("``", StringComparison.Ordinal))))
        |> Array.truncate 5
        |> List.ofArray

let private editDistance (a: string) (b: string) : int =
    let m = a.Length
    let n = b.Length
    let prev = Array.init (n + 1) id
    let curr = Array.zeroCreate<int> (n + 1)

    for i in 1..m do
        curr[0] <- i

        for j in 1..n do
            let cost =
                if Char.ToUpperInvariant a[i - 1] = Char.ToUpperInvariant b[j - 1] then
                    0
                else
                    1

            curr[j] <- min (min (curr[j - 1] + 1) (prev[j] + 1)) (prev[j - 1] + cost)

        Array.blit curr 0 prev 0 (n + 1)

    prev[n]

let private resolveSpellcheckSuggestions
    (check: FSharpCheckFileResults)
    (parse: FSharpParseFileResults)
    (getLine: int -> string)
    (diagRange: range)
    (diagText: string)
    (message: string)
    : string list =
    let fromCompiler = parseSpellcheckSuggestions message

    if not fromCompiler.IsEmpty then
        fromCompiler
    else
        let cleanTarget = unquoteBackticks diagText

        if String.IsNullOrWhiteSpace cleanTarget || cleanTarget.Length < 2 || cleanTarget.Contains '.' then
            []
        else
            let maxDist = max 1 (min 3 (cleanTarget.Length / 3))
            let pool = HashSet<string>(StringComparer.Ordinal)

            try
                let pln = QuickParse.GetPartialLongNameEx(getLine diagRange.StartLine, max 0 (diagRange.EndColumn - 1))
                let decls = check.GetDeclarationListInfo(Some parse, diagRange.StartLine, getLine diagRange.StartLine, pln)

                for item in decls.Items do
                    if not (String.IsNullOrWhiteSpace item.NameInCode) then
                        pool.Add item.NameInCode |> ignore
            with _ ->
                ()

            try
                for su in check.GetAllUsesOfAllSymbolsInFile() do
                    if
                        su.Range.StartLine < diagRange.StartLine
                        || (su.Range.StartLine = diagRange.StartLine && su.Range.StartColumn < diagRange.StartColumn)
                    then
                        let nm = PrettyNaming.NormalizeIdentifierBackticks su.Symbol.DisplayName

                        if not (String.IsNullOrWhiteSpace nm) && not (nm.StartsWith("op_", StringComparison.Ordinal)) then
                            pool.Add nm |> ignore
            with _ ->
                ()

            pool
            |> Seq.choose (fun cand ->
                let cleanCand = unquoteBackticks cand

                if
                    String.Equals(cleanCand, cleanTarget, StringComparison.Ordinal)
                    || abs (cleanCand.Length - cleanTarget.Length) > maxDist
                then
                    None
                else
                    let d = editDistance cleanTarget cleanCand
                    if d >= 1 && d <= maxDist then Some(d, abs (cleanCand.Length - cleanTarget.Length), cand) else None)
            |> Seq.sortBy (fun (d, lenDiff, cand) -> d, lenDiff, cand)
            |> Seq.truncate 5
            |> Seq.map (fun (_, _, cand) -> cand)
            |> Seq.toList

/// Finds a non-recursive `let` binding enclosing `diagPos` whose bound function/value name equals `identText`,
/// returning `(fnName, rangeAfterLetKeyword)`.
let private tryFindSelfRecursiveBinding (ast: ParsedInput) (diagPos: pos) (identText: string) : (string * pos) option =
    let cleanIdent = unquoteBackticks identText

    let tryBindingHeadName (pat: SynPat) : Ident option =
        match pat with
        | SynPat.LongIdent(longDotId = SynLongIdent(id = [ fnIdent ]); argPats = argPats) ->
            let hasArgs =
                match argPats with
                | SynArgPats.Pats ps -> not ps.IsEmpty
                | SynArgPats.NamePatPairs(pats = ps) -> not ps.IsEmpty

            if hasArgs then Some fnIdent else None
        | SynPat.Named(SynIdent(fnIdent, _), _, _, _) -> Some fnIdent
        | _ -> None

    let tryAfterLetPos (trivia: SynBindingTrivia) : pos option =
        match trivia.LeadingKeyword with
        | SynLeadingKeyword.Let r -> Some r.End
        | SynLeadingKeyword.LetBang r -> Some r.End
        | _ -> None

    let rec walkExpr (expr: SynExpr) : (string * pos) option =
        if not (Range.rangeContainsPos expr.Range diagPos) then
            None
        else
            match expr with
            | SynExpr.LetOrUse({ IsRecursive = isRec; Bindings = bindings; Body = body }) ->
                let inBinding =
                    bindings
                    |> List.tryPick (fun (SynBinding(headPat = headPat; expr = bExpr; trivia = trivia)) ->
                        if Range.rangeContainsPos bExpr.Range diagPos then
                            walkExpr bExpr
                            |> Option.orElseWith (fun () ->
                                if not isRec then
                                    match tryBindingHeadName headPat, tryAfterLetPos trivia with
                                    | Some fnId, Some afterLet when String.Equals(fnId.idText, cleanIdent, StringComparison.Ordinal) ->
                                        Some(fnId.idText, afterLet)
                                    | _ -> None
                                else
                                    None)
                        else
                            None)

                inBinding |> Option.orElseWith (fun () -> walkExpr body)
            | SynExpr.Sequential(expr1 = e1; expr2 = e2) -> walkExpr e1 |> Option.orElseWith (fun () -> walkExpr e2)
            | SynExpr.IfThenElse(ifExpr = c; thenExpr = t; elseExpr = eOpt) ->
                walkExpr c
                |> Option.orElseWith (fun () -> walkExpr t)
                |> Option.orElseWith (fun () -> eOpt |> Option.bind walkExpr)
            | SynExpr.App(funcExpr = f; argExpr = a) -> walkExpr f |> Option.orElseWith (fun () -> walkExpr a)
            | SynExpr.Paren(expr = inner) -> walkExpr inner
            | SynExpr.Lambda(body = b) -> walkExpr b
            | SynExpr.Match(expr = me; clauses = cs) ->
                walkExpr me
                |> Option.orElseWith (fun () -> cs |> List.tryPick (fun (SynMatchClause(resultExpr = re)) -> walkExpr re))
            | SynExpr.TryWith(tryExpr = te; withCases = cs) ->
                walkExpr te
                |> Option.orElseWith (fun () -> cs |> List.tryPick (fun (SynMatchClause(resultExpr = re)) -> walkExpr re))
            | SynExpr.TryFinally(tryExpr = te; finallyExpr = fe) ->
                walkExpr te |> Option.orElseWith (fun () -> walkExpr fe)
            | SynExpr.ComputationExpr(expr = ce) -> walkExpr ce
            | _ -> None

    let rec walkDecls (decls: SynModuleDecl list) : (string * pos) option =
        decls
        |> List.tryPick (fun decl ->
            if not (Range.rangeContainsPos decl.Range diagPos) then
                None
            else
                match decl with
                | SynModuleDecl.Let(isRecursive = isRec; bindings = bindings) ->
                    bindings
                    |> List.tryPick (fun (SynBinding(headPat = headPat; expr = bExpr; trivia = trivia)) ->
                        if Range.rangeContainsPos bExpr.Range diagPos then
                            walkExpr bExpr
                            |> Option.orElseWith (fun () ->
                                if not isRec then
                                    match tryBindingHeadName headPat, tryAfterLetPos trivia with
                                    | Some fnId, Some afterLet when String.Equals(fnId.idText, cleanIdent, StringComparison.Ordinal) ->
                                        Some(fnId.idText, afterLet)
                                    | _ -> None
                                else
                                    None)
                        else
                            None)
                | SynModuleDecl.NestedModule(decls = inner) -> walkDecls inner
                | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(decls = inner)) -> walkDecls inner
                | SynModuleDecl.Expr(expr = e) -> walkExpr e
                | _ -> None)

    match ast with
    | ParsedInput.SigFile _ -> None
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        modules
        |> List.tryPick (fun (SynModuleOrNamespace(decls = decls; range = r)) ->
            if Range.rangeContainsPos r diagPos then walkDecls decls else None)

/// Checks whether `targetRange` is the direct receiver of a `SynExpr.DotGet` (e.g. `MemoryStream().Length`)
/// or immediately followed by `.` in source text.
let private isChainedReceiver (lines: string[]) (ast: ParsedInput) (targetRange: range) : bool =
    let followedByDot =
        let ln = getLineStr lines targetRange.EndLine
        targetRange.EndColumn >= 0 && targetRange.EndColumn < ln.Length && ln[targetRange.EndColumn] = '.'

    if followedByDot then
        true
    else
        (targetRange.Start, ast)
        ||> ParsedInput.tryPick (fun _ node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.DotGet(expr = recv)) when Range.equals recv.Range targetRange -> Some true
            | _ -> None)
        |> Option.defaultValue false

/// Inspects an `FS0020` expression range to see if it is `lhs = rhs` where `lhs` is mutable or settable,
/// returning the `=` operator range if so.
let private tryFindMutableEqualityRange
    (lines: string[])
    (ast: ParsedInput)
    (check: FSharpCheckFileResults)
    (diagRange: range)
    : range option =
    let tryOpEqRange (opExpr: SynExpr) : range option =
        match opExpr with
        | SynExpr.Ident opIdent when opIdent.idText = "op_Equality" || opIdent.idText = "=" -> Some opIdent.idRange
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ opIdent ])) when
            opIdent.idText = "op_Equality" || opIdent.idText = "="
            ->
            Some opIdent.idRange
        | _ -> None

    let eqHit =
        (diagRange.Start, ast)
        ||> ParsedInput.tryPick (fun _ node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.App(
                funcExpr = SynExpr.App(isInfix = true; funcExpr = opExpr; argExpr = lhsExpr)
                range = appRange)) when Range.equals appRange diagRange ->
                tryOpEqRange opExpr |> Option.map (fun opRng -> lhsExpr, opRng)
            | _ -> None)

    match eqHit with
    | None -> None
    | Some(lhsExpr, opRange) ->
        let rec tryLastIdent (e: SynExpr) : Ident option =
            match e with
            | SynExpr.Ident id -> Some id
            | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
            | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
            | SynExpr.Paren(expr = inner) -> tryLastIdent inner
            | _ -> None

        match tryLastIdent lhsExpr with
        | None -> None
        | Some lhsIdent ->
            let ln = getLineStr lines lhsIdent.idRange.EndLine

            match check.GetSymbolUseAtLocation(lhsIdent.idRange.EndLine, lhsIdent.idRange.EndColumn, ln, [ lhsIdent.idText ]) with
            | None -> None
            | Some su ->
                let isMut =
                    match su.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as mfv ->
                        mfv.IsMutable || mfv.HasSetterMethod || mfv.IsPropertySetterMethod
                    | :? FSharpField as rf -> rf.IsMutable
                    | _ -> false

                if isMut then Some opRange else None

/// Family 8: Finds an unannotated value binding or bare function parameter at `cursorPos` and formats its inferred type.
let private tryExplicitTypeAnnotationEdit
    (lines: string[])
    (ast: ParsedInput)
    (check: FSharpCheckFileResults)
    (cursorPos: pos)
    : TextEdit option =
    let formatSymbolType (ident: Ident) : string option =
        let ln = getLineStr lines ident.idRange.EndLine

        match check.GetSymbolUseAtLocation(ident.idRange.EndLine, ident.idRange.EndColumn, ln, [ ident.idText ]) with
        | None -> None
        | Some su ->
            match su.Symbol with
            | :? FSharpMemberOrFunctionOrValue as mfv ->
                try
                    let tyStr = mfv.FullType.Format(su.DisplayContext)

                    if String.IsNullOrWhiteSpace tyStr || tyStr.Contains("'?", StringComparison.Ordinal) then
                        None
                    else
                        Some tyStr
                with _ ->
                    None
            | _ -> None

    let rec tryInPat (isParam: bool) (pat: SynPat) : TextEdit option =
        if not (Range.rangeContainsPos pat.Range cursorPos) then
            None
        else
            match pat with
            | SynPat.Typed _ -> None
            | SynPat.Paren(SynPat.Typed _, _) -> None
            | SynPat.Named(SynIdent(ident, _), _, _, patRange) when Range.rangeContainsPos ident.idRange cursorPos ->
                formatSymbolType ident
                |> Option.map (fun tyStr ->
                    if isParam then
                        let rawText = sliceFcsRange lines patRange

                        { Range = fcsRangeToLsp patRange
                          NewText = $"(%s{rawText}: %s{tyStr})" }
                    else
                        let endPos = fcsPosToLsp ident.idRange.End

                        { Range = { Start = endPos; End = endPos }
                          NewText = $": %s{tyStr}" })
            | SynPat.Paren(SynPat.Named(SynIdent(ident, _), _, _, _), _) when Range.rangeContainsPos ident.idRange cursorPos ->
                formatSymbolType ident
                |> Option.map (fun tyStr ->
                    let endPos = fcsPosToLsp ident.idRange.End

                    { Range = { Start = endPos; End = endPos }
                      NewText = $": %s{tyStr}" })
            | SynPat.LongIdent(longDotId = SynLongIdent(id = [ fnIdent ]); argPats = argPats) ->
                let args =
                    match argPats with
                    | SynArgPats.Pats ps -> ps
                    | SynArgPats.NamePatPairs(pats = ps) -> ps |> List.map (fun (NamePatPairField(pat = p)) -> p)

                if args.IsEmpty && Range.rangeContainsPos fnIdent.idRange cursorPos then
                    formatSymbolType fnIdent
                    |> Option.map (fun tyStr ->
                        let endPos = fcsPosToLsp fnIdent.idRange.End

                        { Range = { Start = endPos; End = endPos }
                          NewText = $": %s{tyStr}" })
                else
                    args |> List.tryPick (tryInPat true)
            | SynPat.Tuple(_, elemPats, _, _) -> elemPats |> List.tryPick (tryInPat isParam)
            | _ -> None

    let tryBinding (SynBinding(headPat = headPat; returnInfo = retInfo; expr = bodyExpr)) : TextEdit option =
        if Range.rangeContainsPos headPat.Range cursorPos then
            match headPat, retInfo with
            | SynPat.Named _, Some _ -> None
            | SynPat.LongIdent(argPats = SynArgPats.Pats []), Some _ -> None
            | _ -> tryInPat false headPat
        elif Range.rangeContainsPos bodyExpr.Range cursorPos then
            None
        else
            None

    let rec walkExpr (expr: SynExpr) : TextEdit option =
        if not (Range.rangeContainsPos expr.Range cursorPos) then
            None
        else
            match expr with
            | SynExpr.LetOrUse({ Bindings = bs; Body = body }) ->
                (bs |> List.tryPick (fun b ->
                    tryBinding b
                    |> Option.orElseWith (fun () ->
                        let (SynBinding(expr = be)) = b
                        walkExpr be)))
                |> Option.orElseWith (fun () -> walkExpr body)
            | SynExpr.Sequential(expr1 = e1; expr2 = e2) -> walkExpr e1 |> Option.orElseWith (fun () -> walkExpr e2)
            | SynExpr.Lambda(args = SynSimplePats.SimplePats(pats = spats); body = b) ->
                let hitSimple =
                    spats
                    |> List.tryPick (function
                        | SynSimplePat.Id(ident = id; range = r) when Range.rangeContainsPos r cursorPos ->
                            formatSymbolType id
                            |> Option.map (fun tyStr ->
                                { Range = fcsRangeToLsp r
                                  NewText = $"(%s{id.idText}: %s{tyStr})" })
                        | _ -> None)

                hitSimple |> Option.orElseWith (fun () -> walkExpr b)
            | SynExpr.Paren(expr = inner) -> walkExpr inner
            | SynExpr.App(funcExpr = f; argExpr = a) -> walkExpr f |> Option.orElseWith (fun () -> walkExpr a)
            | SynExpr.IfThenElse(ifExpr = c; thenExpr = t; elseExpr = eOpt) ->
                walkExpr c
                |> Option.orElseWith (fun () -> walkExpr t)
                |> Option.orElseWith (fun () -> eOpt |> Option.bind walkExpr)
            | SynExpr.Match(expr = me; clauses = cs) ->
                walkExpr me
                |> Option.orElseWith (fun () -> cs |> List.tryPick (fun (SynMatchClause(resultExpr = re)) -> walkExpr re))
            | _ -> None

    let rec walkDecls (decls: SynModuleDecl list) : TextEdit option =
        decls
        |> List.tryPick (fun d ->
            if not (Range.rangeContainsPos d.Range cursorPos) then
                None
            else
                match d with
                | SynModuleDecl.Let(bindings = bs) ->
                    bs
                    |> List.tryPick (fun b ->
                        tryBinding b
                        |> Option.orElseWith (fun () ->
                            let (SynBinding(expr = be)) = b
                            walkExpr be))
                | SynModuleDecl.NestedModule(decls = inner) -> walkDecls inner
                | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(decls = inner)) -> walkDecls inner
                | SynModuleDecl.Expr(expr = e) -> walkExpr e
                | _ -> None)

    match ast with
    | ParsedInput.SigFile _ -> None
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        modules |> List.tryPick (fun (SynModuleOrNamespace(decls = decls)) -> walkDecls decls)

/// Family 9a: Implement interface stub via `InterfaceStubGenerator`.
let private tryImplementInterfaceEdits
    (config: FSharpConfig)
    (lines: string[])
    (ast: ParsedInput)
    (check: FSharpCheckFileResults)
    (probePos: pos)
    : TextEdit[] option =
    let tryObjExprFallback () =
        (probePos, ast)
        ||> ParsedInput.tryPick (fun _ node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.ObjExpr(objType = ty; argOptions = None; bindings = binds; extraImpls = ifaces; range = r)) when
                Range.rangeContainsPos r probePos
                ->
                ifaces
                |> List.tryPick (fun (SynInterfaceImpl(interfaceTy = ity; bindings = ibinds; range = ir)) ->
                    if Range.rangeContainsPos ir probePos then
                        Some(InterfaceData.ObjExpr(ity, ibinds))
                    else
                        None)
                |> Option.orElse (Some(InterfaceData.ObjExpr(ty, binds)))
            | _ -> None)

    match InterfaceStubGenerator.TryFindInterfaceDeclaration probePos ast |> Option.orElseWith tryObjExprFallback with
    | None -> None
    | Some ifaceData ->
        let tyEnd =
            match ifaceData with
            | InterfaceData.Interface(ty, _) -> ty.Range.End
            | InterfaceData.ObjExpr(ty, _) -> ty.Range.End

        let rec extractLastTypeIdent (t: SynType) : Ident option =
            match t with
            | SynType.LongIdent(SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
            | SynType.App(typeName = inner) -> extractLastTypeIdent inner
            | SynType.Paren(innerType = inner) -> extractLastTypeIdent inner
            | _ -> None

        let ifaceSynType =
            match ifaceData with
            | InterfaceData.Interface(ty, _) -> ty
            | InterfaceData.ObjExpr(ty, _) -> ty

        match extractLastTypeIdent ifaceSynType with
        | None -> None
        | Some tyIdent ->
            let ln = getLineStr lines tyIdent.idRange.EndLine

            match check.GetSymbolUseAtLocation(tyIdent.idRange.EndLine, tyIdent.idRange.EndColumn, ln, [ tyIdent.idText ]) with
            | None -> None
            | Some su ->
                match su.Symbol with
                | :? FSharpEntity as entity when
                    InterfaceStubGenerator.IsInterface entity
                    && not (InterfaceStubGenerator.HasNoInterfaceMember entity)
                    ->
                    let getMemberByLocation (name: string, mRange: range) =
                        let mLine = getLineStr lines mRange.EndLine
                        check.GetSymbolUseAtLocation(mRange.EndLine, mRange.EndColumn, mLine, [ name ])

                    let implementedSigs =
                        InterfaceStubGenerator.GetImplementedMemberSignatures getMemberByLocation su.DisplayContext ifaceData
                        |> Async.RunSynchronously

                    let startAndWith =
                        (tyEnd, ast)
                        ||> ParsedInput.tryPick (fun _ node ->
                            match node with
                            | SyntaxNode.SynMemberDefn(SynMemberDefn.Interface(interfaceType = ty; withKeyword = withOpt; range = mr)) when
                                Range.rangeContainsPos ty.Range tyEnd
                                ->
                                Some(mr.Start.Column + config.IndentationSize, withOpt, ifaceData.Range.End)
                            | SyntaxNode.SynExpr(SynExpr.ObjExpr(objType = ty; withKeyword = withOpt; newExprRange = nr)) when
                                Range.rangeContainsPos ty.Range tyEnd
                                ->
                                Some(nr.Start.Column + config.IndentationSize, withOpt, ty.Range.End)
                            | SyntaxNode.SynExpr(SynExpr.ObjExpr(extraImpls = ifaces)) ->
                                ifaces
                                |> List.tryPick (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withOpt; range = ir)) ->
                                    if Range.rangeContainsPos ty.Range tyEnd then
                                        Some(ir.Start.Column + config.IndentationSize, withOpt, ty.Range.End)
                                    else
                                        None)
                            | _ -> None)

                    match startAndWith with
                    | None -> None
                    | Some(defaultIndentCol, withOpt, headerEndPos) ->
                        let existingBindings =
                            match ifaceData with
                            | InterfaceData.Interface(_, None) -> []
                            | InterfaceData.Interface(_, Some ms) ->
                                ms
                                |> List.choose (function
                                    | SynMemberDefn.Member(memberDefn = b) -> Some b
                                    | SynMemberDefn.GetSetMember(memberDefnForGet = Some b) -> Some b
                                    | SynMemberDefn.GetSetMember(memberDefnForSet = Some b) -> Some b
                                    | _ -> None)
                            | InterfaceData.ObjExpr(_, bs) -> bs

                        let startCol, insertAt, needWith =
                            match List.tryLast existingBindings with
                            | Some(SynBinding(headPat = hp; expr = be; trivia = tr)) ->
                                let col =
                                    match tr.LeadingKeyword.Range with
                                    | r when not r.IsSynthetic -> r.StartColumn
                                    | _ -> hp.Range.StartColumn

                                col, be.Range.End, false
                            | None ->
                                let ins =
                                    match withOpt with
                                    | Some wr -> wr.End
                                    | None -> headerEndPos

                                defaultIndentCol, ins, withOpt.IsNone

                        let stubRaw =
                            InterfaceStubGenerator.FormatInterface
                                startCol
                                config.IndentationSize
                                ifaceData.TypeParameters
                                config.InterfaceStubGenerationObjectIdentifier
                                config.InterfaceStubGenerationMethodBody
                                su.DisplayContext
                                implementedSigs
                                entity
                                true

                        let stubNorm = stubRaw.Replace("\r\n", "\n").TrimEnd('\n')

                        if String.IsNullOrWhiteSpace stubNorm then
                            None
                        else
                            let prefix = if needWith then " with" else ""
                            let insLsp = fcsPosToLsp insertAt

                            Some
                                [| { Range = { Start = insLsp; End = insLsp }
                                     NewText = prefix + stubNorm } |]
                | _ -> None

/// Family 9b: Implement abstract class members on `{ new BaseClass() ... }` or `type Sub() = inherit BaseClass()`.
let private tryImplementAbstractClassEdits
    (config: FSharpConfig)
    (lines: string[])
    (ast: ParsedInput)
    (check: FSharpCheckFileResults)
    (probePos: pos)
    : TextEdit[] option =
    let rec extractLastTypeIdent (t: SynType) : Ident option =
        match t with
        | SynType.LongIdent(SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
        | SynType.App(typeName = inner) -> extractLastTypeIdent inner
        | SynType.Paren(innerType = inner) -> extractLastTypeIdent inner
        | _ -> None

    let targetSite =
        (probePos, ast)
        ||> ParsedInput.tryPick (fun _ node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.ObjExpr(
                objType = ty
                argOptions = Some(argExpr, _)
                withKeyword = withOpt
                bindings = binds
                members = ms
                newExprRange = newRange
                range = fullRange)) when Range.rangeContainsPos fullRange probePos ->
                let existingNames =
                    [ for SynBinding(headPat = hp) in binds do
                          match hp with
                          | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
                              yield (List.last ids).idText
                          | _ -> ()
                      for m in ms do
                          match m with
                          | SynMemberDefn.Member(SynBinding(headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = ids))), _) when
                              not ids.IsEmpty
                              ->
                              yield (List.last ids).idText
                          | _ -> () ]
                    |> Set.ofList

                let headerEnd =
                    if argExpr.Range.EndLine > ty.Range.EndLine || argExpr.Range.EndColumn > ty.Range.EndColumn then
                        argExpr.Range.End
                    else
                        ty.Range.End

                let indentCol = newRange.Start.Column + config.IndentationSize

                let insertPos, needWith, suffix =
                    match List.tryLast binds with
                    | Some(SynBinding(expr = be)) -> be.Range.End, false, ""
                    | None ->
                        let ins =
                            match withOpt with
                            | Some wr -> wr.End
                            | None -> headerEnd

                        let endLineStr = getLineStr lines fullRange.EndLine
                        // Keep space/newline before closing `}`
                        let trail =
                            if ins.Line = fullRange.EndLine && fullRange.EndColumn <= endLineStr.Length then "\n" + String(' ', newRange.Start.Column)
                            else ""

                        ins, withOpt.IsNone, trail

                Some(ty, existingNames, indentCol, insertPos, needWith, suffix)
            | SyntaxNode.SynExpr(SynExpr.ComputationExpr(hasSeqBuilder = false; expr = innerExpr; range = fullRange)) when
                Range.rangeContainsPos fullRange probePos
                ->
                let rec tryUnwrapNew (e: SynExpr) =
                    match e with
                    | SynExpr.New(targetType = ty; expr = argExpr; range = newRange) -> Some(ty, argExpr, newRange)
                    | SynExpr.Paren(expr = inner) -> tryUnwrapNew inner
                    | _ -> None

                match tryUnwrapNew innerExpr with
                | None -> None
                | Some(ty, argExpr, newRange) ->
                    let headerEnd =
                        if Position.posGt argExpr.Range.End ty.Range.End then
                            argExpr.Range.End
                        else
                            ty.Range.End

                    let indentCol = newRange.Start.Column + config.IndentationSize
                    let endLineStr = getLineStr lines fullRange.EndLine

                    let trail =
                        if headerEnd.Line = fullRange.EndLine && fullRange.EndColumn <= endLineStr.Length then
                            "\n" + String(' ', newRange.Start.Column)
                        else
                            ""

                    Some(ty, Set.empty, indentCol, headerEnd, true, trail)
            | SyntaxNode.SynTypeDefn(SynTypeDefn(typeRepr = SynTypeDefnRepr.ObjectModel(members = oms); members = extraMs; range = tr)) when
                Range.rangeContainsPos tr probePos
                ->
                let allMs = oms @ extraMs

                let inheritHit =
                    allMs
                    |> List.tryPick (function
                        | SynMemberDefn.ImplicitInherit(inheritType = ity; inheritArgs = iargs; range = ir) ->
                            let inhEnd = if Position.posGt iargs.Range.End ir.End then iargs.Range.End else ir.End
                            Some(ity, inhEnd, ir.Start.Column)
                        | SynMemberDefn.Inherit(baseType = Some ity; range = ir) ->
                            Some(ity, ir.End, ir.Start.Column)
                        | _ -> None)

                match inheritHit with
                | None -> None
                | Some(ity, inhEnd, indentCol) ->
                    let existingNames =
                        [ for m in allMs do
                              match m with
                              | SynMemberDefn.Member(SynBinding(headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = ids))), _) when
                                  not ids.IsEmpty
                                  ->
                                  yield (List.last ids).idText
                              | _ -> () ]
                        |> Set.ofList

                    Some(ity, existingNames, indentCol, inhEnd, false, "")
            | _ -> None)

    match targetSite with
    | None -> None
    | Some(synType, existingNames, indentCol, insertPos, needWith, suffix) ->
        match extractLastTypeIdent synType with
        | None -> None
        | Some tyIdent ->
            let ln = getLineStr lines tyIdent.idRange.EndLine

            match check.GetSymbolUseAtLocation(tyIdent.idRange.EndLine, tyIdent.idRange.EndColumn, ln, [ tyIdent.idText ]) with
            | None -> None
            | Some su ->
                let entityOpt =
                    match su.Symbol with
                    | :? FSharpEntity as e -> Some e
                    | :? FSharpMemberOrFunctionOrValue as mfv -> mfv.DeclaringEntity
                    | _ -> None

                match entityOpt with
                | Some entity when not entity.IsInterface ->
                    let abstractSlots =
                        entity.MembersFunctionsAndValues
                        |> Seq.filter (fun m ->
                            m.IsDispatchSlot
                            && not m.IsPropertyGetterMethod
                            && not m.IsPropertySetterMethod
                            && not (existingNames.Contains m.DisplayName))
                        |> Seq.distinctBy _.DisplayName
                        |> Seq.toList

                    if abstractSlots.IsEmpty then
                        None
                    else
                        let indent = String(' ', indentCol)
                        let objId = config.AbstractClassStubGenerationObjectIdentifier
                        let body = config.AbstractClassStubGenerationMethodBody

                        let formattedMembers =
                            abstractSlots
                            |> List.map (fun m ->
                                if m.IsProperty then
                                    $"%s{indent}override %s{objId}.%s{m.DisplayName} = %s{body}"
                                else
                                    let paramGroups =
                                        m.CurriedParameterGroups
                                        |> Seq.map (fun grp ->
                                            if grp.Count = 0 then
                                                "()"
                                            else
                                                let ps =
                                                    grp
                                                    |> Seq.mapi (fun idx p ->
                                                        match p.Name with
                                                        | Some n when not (String.IsNullOrWhiteSpace n) -> n
                                                        | _ -> $"arg%d{idx}")
                                                    |> String.concat ", "

                                                $"(%s{ps})")
                                        |> String.concat " "

                                    let argsText = if String.IsNullOrEmpty paramGroups then "()" else paramGroups
                                    $"%s{indent}override %s{objId}.%s{m.DisplayName}%s{argsText} = %s{body}")
                            |> String.concat "\n"

                        let prefix = if needWith then " with\n" else "\n"
                        let insLsp = fcsPosToLsp insertPos

                        Some
                            [| { Range = { Start = insLsp; End = insLsp }
                                 NewText = prefix + formattedMembers + suffix } |]
                | _ -> None

/// Family 10: Generate missing union pattern match cases on `SynExpr.Match`.
let private tryGenerateUnionCasesEdits
    (config: FSharpConfig)
    (lines: string[])
    (ast: ParsedInput)
    (check: FSharpCheckFileResults)
    (probePos: pos)
    : TextEdit[] option =
    let matchHit =
        (probePos, ast)
        ||> ParsedInput.tryPick (fun _ node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.Match(matchDebugPoint = _; expr = matchedExpr; clauses = clauses; range = mr)) when
                Range.rangeContainsPos mr probePos
                ->
                Some(matchedExpr, clauses, mr)
            | _ -> None)

    match matchHit with
    | None -> None
    | Some(matchedExpr, clauses, matchRange) ->
        let rec unwrapAbbrev (ty: FSharpType) : FSharpEntity option =
            if ty.IsAbbreviation then
                unwrapAbbrev ty.AbbreviatedType
            elif ty.HasTypeDefinition then
                let td = ty.TypeDefinition
                if td.IsFSharpUnion then Some td else None
            else
                None

        let rec findPatIdent (p: SynPat) : Ident option =
            match p with
            | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
            | SynPat.Named(SynIdent(id, _), _, _, _) -> Some id
            | SynPat.Paren(inner, _) -> findPatIdent inner
            | SynPat.Or(lhs, _, _, _) -> findPatIdent lhs
            | SynPat.As(lhs, _, _) -> findPatIdent lhs
            | SynPat.Typed(inner, _, _) -> findPatIdent inner
            | _ -> None

        let rec findExprIdent (e: SynExpr) : Ident option =
            match e with
            | SynExpr.Ident id -> Some id
            | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
            | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
            | SynExpr.App(funcExpr = f) -> findExprIdent f
            | SynExpr.Paren(expr = inner) -> findExprIdent inner
            | SynExpr.Typed(expr = inner) -> findExprIdent inner
            | _ -> None

        let tryUnionFromClause =
            clauses
            |> List.tryPick (fun (SynMatchClause(pat = p)) ->
                match findPatIdent p with
                | None -> None
                | Some id ->
                    let ln = getLineStr lines id.idRange.EndLine

                    match check.GetSymbolUseAtLocation(id.idRange.EndLine, id.idRange.EndColumn, ln, [ id.idText ]) with
                    | Some su ->
                        match su.Symbol with
                        | :? FSharpUnionCase as uc -> unwrapAbbrev uc.ReturnType
                        | _ -> None
                    | None -> None)

        let tryUnionFromExpr () =
            match findExprIdent matchedExpr with
            | None -> None
            | Some id ->
                let ln = getLineStr lines id.idRange.EndLine

                match check.GetSymbolUseAtLocation(id.idRange.EndLine, id.idRange.EndColumn, ln, [ id.idText ]) with
                | Some su ->
                    match su.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as mfv ->
                        try
                            let rec retType (t: FSharpType) =
                                if t.IsFunctionType && t.GenericArguments.Count >= 2 then
                                    retType t.GenericArguments[1]
                                else
                                    t

                            unwrapAbbrev (retType mfv.FullType)
                        with _ ->
                            None
                    | :? FSharpField as rf -> unwrapAbbrev rf.FieldType
                    | _ -> None
                | None -> None

        match tryUnionFromClause |> Option.orElseWith tryUnionFromExpr with
        | None -> None
        | Some unionEntity ->
            let allCases = unionEntity.UnionCases |> Seq.toList
            let caseNameSet = allCases |> List.map _.Name |> Set.ofList
            let covered = HashSet<string>(StringComparer.Ordinal)
            let mutable hasWildcard = false

            let rec collectCovered (p: SynPat) =
                match p with
                | SynPat.Wild _ -> hasWildcard <- true
                | SynPat.Named(SynIdent(id, _), _, _, _) ->
                    if caseNameSet.Contains id.idText then
                        covered.Add id.idText |> ignore
                    else
                        hasWildcard <- true
                | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
                    let last = (List.last ids).idText

                    if caseNameSet.Contains last then
                        covered.Add last |> ignore
                | SynPat.Paren(inner, _) -> collectCovered inner
                | SynPat.As(lhs, _, _) -> collectCovered lhs
                | SynPat.Typed(inner, _, _) -> collectCovered inner
                | SynPat.Attrib(inner, _, _) -> collectCovered inner
                | SynPat.Or(lhs, rhs, _, _) ->
                    collectCovered lhs
                    collectCovered rhs
                | _ -> ()

            for SynMatchClause(pat = p; whenExpr = whenOpt) in clauses do
                if whenOpt.IsNone then
                    collectCovered p

            if hasWildcard then
                None
            else
                let missing = allCases |> List.filter (fun uc -> not (covered.Contains uc.Name))

                if missing.IsEmpty then
                    None
                else
                    let reqRqa =
                        try
                            unionEntity.HasAttribute<RequireQualifiedAccessAttribute>()
                        with _ ->
                            false

                    let qualPrefix = if reqRqa then unionEntity.DisplayName + "." else ""
                    let body = config.UnionCaseStubGenerationBody

                    let barCol, insertPos =
                        match clauses with
                        | firstClause :: _ ->
                            let lastClause = List.last clauses
                            let (SynMatchClause(trivia = firstTrivia)) = firstClause

                            let col =
                                match firstTrivia.BarRange with
                                | Some br -> br.StartColumn
                                | None ->
                                    let cLine = getLineStr lines firstClause.Range.StartLine
                                    let idx = cLine.IndexOf('|')
                                    if idx >= 0 then idx else firstClause.Range.StartColumn

                            col, lastClause.Range.End
                        | [] ->
                            matchRange.StartColumn + config.IndentationSize, matchRange.End

                    let indent = String(' ', max 0 barCol)

                    let formattedClauses =
                        missing
                        |> List.map (fun uc ->
                            let nFields = uc.Fields.Count

                            let patText =
                                if nFields = 0 then
                                    $"%s{qualPrefix}%s{uc.Name}"
                                elif nFields = 1 then
                                    $"%s{qualPrefix}%s{uc.Name} _"
                                else
                                    let wilds = Seq.replicate nFields "_" |> String.concat ", "
                                    $"%s{qualPrefix}%s{uc.Name}(%s{wilds})"

                            $"%s{indent}| %s{patText} -> %s{body}")
                        |> String.concat "\n"

                    let insLsp = fcsPosToLsp insertPos

                    Some
                        [| { Range = { Start = insLsp; End = insLsp }
                             NewText = "\n" + formattedClauses } |]

/// Family 11: Generate missing record fields on `SynExpr.Record`.
let private tryGenerateRecordStubEdits
    (config: FSharpConfig)
    (lines: string[])
    (ast: ParsedInput)
    (check: FSharpCheckFileResults)
    (probePos: pos)
    : TextEdit[] option =
    let recHit =
        (probePos, ast)
        ||> ParsedInput.tryPick (fun _ node ->
            match node with
            | SyntaxNode.SynExpr(SynExpr.Record(_, copyOpt, recFields, recRange)) when
                Range.rangeContainsPos recRange probePos
                ->
                Some(copyOpt, recFields, recRange)
            | _ -> None)

    match recHit with
    | None -> None
    | Some(copyOpt, recFields, recRange) ->
        let assignedFields =
            [ for rf in recFields do
                  match rf with
                  | SynExprRecordFieldOrSpread.Field(SynExprRecordField(fieldName = (SynLongIdent(id = ids), _); expr = exprOpt), _) when
                      not ids.IsEmpty
                      ->
                      let lastId = List.last ids
                      yield lastId, exprOpt
                  | _ -> () ]

        let assignedNames = assignedFields |> List.map (fun (id, _) -> id.idText) |> Set.ofList

        let recordEntityOpt =
            assignedFields
            |> List.tryPick (fun (fieldIdent, _) ->
                let ln = getLineStr lines fieldIdent.idRange.EndLine

                match check.GetSymbolUseAtLocation(fieldIdent.idRange.EndLine, fieldIdent.idRange.EndColumn, ln, [ fieldIdent.idText ]) with
                | Some su ->
                    match su.Symbol with
                    | :? FSharpField as rf ->
                        match rf.DeclaringEntity with
                        | Some e when e.IsFSharpRecord -> Some e
                        | _ -> None
                    | _ -> None
                | None -> None)
            |> Option.orElseWith (fun () ->
                match copyOpt with
                | Some(SynExpr.Ident id, _) ->
                    let ln = getLineStr lines id.idRange.EndLine

                    match check.GetSymbolUseAtLocation(id.idRange.EndLine, id.idRange.EndColumn, ln, [ id.idText ]) with
                    | Some su ->
                        match su.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as mfv when mfv.FullType.HasTypeDefinition ->
                            let td = mfv.FullType.TypeDefinition
                            if td.IsFSharpRecord then Some td else None
                        | _ -> None
                    | None -> None
                | _ -> None)

        match recordEntityOpt with
        | None -> None
        | Some recEntity ->
            let missingFields =
                [ for f in recEntity.FSharpFields do
                      if not (assignedNames.Contains f.Name) then
                          yield f.Name ]

            if missingFields.IsEmpty then
                None
            else
                let body = config.RecordStubGenerationBody

                match assignedFields with
                | (firstId, _) :: _ ->
                    let _, lastExprOpt = List.last assignedFields
                    let lastId, _ = List.last assignedFields

                    let insertPos =
                        match lastExprOpt with
                        | Some e -> e.Range.End
                        | None -> lastId.idRange.End

                    let fieldCol = firstId.idRange.StartColumn
                    let indent = String(' ', max 0 fieldCol)

                    let newFieldsText =
                        missingFields
                        |> List.map (fun fname -> $"\n%s{indent}%s{fname} = %s{body}")
                        |> String.concat ""

                    let insLsp = fcsPosToLsp insertPos

                    Some
                        [| { Range = { Start = insLsp; End = insLsp }
                             NewText = newFieldsText } |]
                | [] ->
                    let indentCol = recRange.StartColumn + 2
                    let indent = String(' ', indentCol)

                    let newFieldsText =
                        missingFields
                        |> List.mapi (fun i fname ->
                            if i = 0 then $" %s{fname} = %s{body}" else $"\n%s{indent}%s{fname} = %s{body}")
                        |> String.concat ""

                    let afterOpenBrace = Position.mkPos recRange.StartLine (recRange.StartColumn + 1)
                    let insLsp = fcsPosToLsp afterOpenBrace

                    Some
                        [| { Range = { Start = insLsp; End = insLsp }
                             NewText = newFieldsText + " " } |]

/// Family 12 helper: Resolves `SigSyncCandidate` against the paired `.fsi` file's `TextView` (`Parse` + `Text`).
let private resolveSigSyncAction (sigPath: string) (sigView: TextView) (cand: SigSyncCandidate) : RawAction option =
    let sigLines = sigView.Text.Split '\n'

    match sigView.Parse.ParseTree with
    | ParsedInput.ImplFile _ -> None
    | ParsedInput.SigFile(ParsedSigFileInput(contents = sigModules)) ->
        let rec findValSigRangeByNameOrLoc
            (targetName: string)
            (locOpt: (int * int * int * int) option)
            (decls: SynModuleSigDecl list)
            : range option =
            decls
            |> List.tryPick (fun d ->
                match d with
                | SynModuleSigDecl.Val(SynValSig(ident = SynIdent(id, _); range = mValSig), _) ->
                    let byLoc =
                        match locOpt with
                        | Some(sl, sc, el, ec) ->
                            let declStart = Position.mkPos sl sc
                            let declEnd = Position.mkPos el ec
                            Range.rangeContainsPos mValSig declStart && Range.rangeContainsPos mValSig declEnd
                        | None -> false

                    if byLoc || String.Equals(id.idText, targetName, StringComparison.Ordinal) then
                        Some mValSig
                    else
                        None
                | SynModuleSigDecl.NestedModule(moduleDecls = inner) ->
                    findValSigRangeByNameOrLoc targetName locOpt inner
                | SynModuleSigDecl.NamespaceFragment(SynModuleOrNamespaceSig(decls = inner)) ->
                    findValSigRangeByNameOrLoc targetName locOpt inner
                | _ -> None)

        match cand with
        | UpdateSigVal(valName, sigLocOpt, valText, diagOpt) ->
            let mValOpt =
                sigModules
                |> List.tryPick (fun (SynModuleOrNamespaceSig(decls = decls)) ->
                    findValSigRangeByNameOrLoc valName sigLocOpt decls)

            match mValOpt with
            | None -> None
            | Some mVal ->
                Some
                    { Title = "Update val in signature file"
                      Kind = CodeActionKind.QuickFix
                      Diagnostics = diagOpt |> Option.map Array.singleton
                      IsPreferred = Some true
                      TargetPath = sigPath
                      Edits =
                        [| { Range = fcsRangeToLsp mVal
                             NewText = valText } |] }

        | AddSigVal(modPath, valName, valText) ->
            let alreadyExists =
                sigModules
                |> List.exists (fun (SynModuleOrNamespaceSig(decls = decls)) ->
                    (findValSigRangeByNameOrLoc valName None decls).IsSome)

            if alreadyExists then
                None
            else
                let targetDeclsOpt =
                    sigModules
                    |> List.tryPick (fun (SynModuleOrNamespaceSig(longId = lid; decls = decls)) ->
                        let sigModParts = lid |> List.map _.idText
                        if sigModParts = modPath then Some(lid, decls) else None)
                    |> Option.orElseWith (fun () ->
                        match sigModules with
                        | SynModuleOrNamespaceSig(longId = lid; decls = decls) :: _ -> Some(lid, decls)
                        | [] -> None)

                match targetDeclsOpt with
                | None -> None
                | Some(lid, decls) ->
                    match List.tryLast decls with
                    | Some lastDecl ->
                        let indent = leadingWhitespace (getLineStr sigLines lastDecl.Range.StartLine)
                        let endLsp = fcsPosToLsp lastDecl.Range.End

                        Some
                            { Title = "Add binding to signature file"
                              Kind = CodeActionKind.Refactor
                              Diagnostics = None
                              IsPreferred = None
                              TargetPath = sigPath
                              Edits =
                                [| { Range = { Start = endLsp; End = endLsp }
                                     NewText = $"\n\n%s{indent}%s{valText}" } |] }
                    | None ->
                        let insertLine0 =
                            match List.tryLast lid with
                            | Some id -> min sigLines.Length id.idRange.EndLine
                            | None -> sigLines.Length

                        Some
                            { Title = "Add binding to signature file"
                              Kind = CodeActionKind.Refactor
                              Diagnostics = None
                              IsPreferred = None
                              TargetPath = sigPath
                              Edits =
                                [| { Range =
                                       { Start = { Line = uint32 insertLine0; Character = 0u }
                                         End = { Line = uint32 insertLine0; Character = 0u } }
                                     NewText = $"\n%s{valText}\n" } |] }

let internal provideWithCapabilities
    (config: FSharpConfig)
    (canDocumentChanges: bool)
    (versionNow: string -> int option)
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (p: CodeActionParams)
    (ct: CancellationToken)
    : Task<TextDocumentCodeActionResult option> =
    if isSignature path then
        Task.FromResult(Some [||])
    else
        let fullPath = Path.GetFullPath path
        let sigFullPath = Path.GetFullPath(Path.ChangeExtension(fullPath, ".fsi"))

        let wantsOrganizeImports =
            match p.Context.Only with
            | Some kinds ->
                kinds
                |> Array.exists (fun k ->
                    String.Equals(k, "source.organizeImports", StringComparison.Ordinal)
                    || String.Equals(k, "source", StringComparison.Ordinal))
            | None -> false

        let hasContextCode (code: string) =
            p.Context.Diagnostics
            |> Array.exists (fun d -> String.Equals(contextDiagCode d, code, StringComparison.OrdinalIgnoreCase))

        let rec runAttempt (attempt: int) : Task<TextDocumentCodeActionResult option> =
            task {
                let! fsVersion, localActions, sigCandidates =
                    graph.ReadFile(
                        fullPath,
                        (fun view ->
                            let lines = view.Text.Split '\n'
                            let fcsLine = max 1 (min (max 1 lines.Length) (int p.Range.Start.Line + 1))
                            let lineText = getLineStr lines fcsLine
                            let fcsCol = max 0 (min lineText.Length (int p.Range.Start.Character))
                            let lastLine = max 1 lines.Length
                            let lastCol = (getLineStr lines lastLine).Length
                            lines, fcsLine, fcsCol, lastLine, lastCol),
                        (fun view (lines, fcsLine, fcsCol, lastLine, lastCol) ->
                            StackGuard.onLargeStack (fun () ->
                                let checkAtCursor =
                                    if view.Whole then
                                        view.Result.CheckResultsNotKept
                                    else
                                        view.CheckResultsAt(fcsLine, fcsCol)

                                let mutable cachedCheckLast: FSharpCheckFileResults option = None

                                let getCheckFull () =
                                    if view.Whole || (fcsLine = lastLine && fcsCol = lastCol) then
                                        checkAtCursor
                                    else
                                        match cachedCheckLast with
                                        | Some c -> c
                                        | None ->
                                            let c = view.CheckResultsAt(lastLine, lastCol)
                                            cachedCheckLast <- Some c
                                            c

                                let ast = view.Parse.ParseTree
                                let cursorPos = Position.mkPos fcsLine fcsCol
                                let getLine = getLineStr lines
                                let actions = ResizeArray<RawAction>()
                                let sigCands = ResizeArray<SigSyncCandidate>()

                                let addLocal
                                    (title: string)
                                    (kind: string)
                                    (diagOpt: Diagnostic option)
                                    (preferred: bool option)
                                    (edits: TextEdit[])
                                    =
                                    if edits.Length > 0 then
                                        actions.Add
                                            { Title = title
                                              Kind = kind
                                              Diagnostics = diagOpt |> Option.map Array.singleton
                                              IsPreferred = preferred
                                              TargetPath = fullPath
                                              Edits = edits }

                                // -------------------------------------------------------------------------
                                // Family 1: Remove unused `open` (QuickFix + source.organizeImports)
                                // -------------------------------------------------------------------------
                                if config.UnusedOpensAnalyzer || hasContextCode "FSAC0001" || wantsOrganizeImports then
                                    let unusedOpens =
                                        UnusedOpens.getUnusedOpens (getCheckFull (), getLine)
                                        |> Async.RunSynchronously

                                    for rng in unusedOpens do
                                        if fcsOverlapsLsp p.Range rng then
                                            let edit = deleteWholeLinesEdit lines (rng.StartLine - 1) (rng.EndLine - 1)
                                            addLocal "Remove unused open" CodeActionKind.QuickFix None (Some true) [| edit |]

                                    if not unusedOpens.IsEmpty && (wantsOrganizeImports || unusedOpens.Length > 1) then
                                        let allEdits =
                                            unusedOpens
                                            |> List.sortBy (fun r -> r.StartLine, r.StartColumn)
                                            |> List.distinctBy (fun r -> r.StartLine, r.EndLine)
                                            |> List.map (fun r -> deleteWholeLinesEdit lines (r.StartLine - 1) (r.EndLine - 1))
                                            |> Array.ofList

                                        addLocal "Remove all unused opens" "source.organizeImports" None None allEdits

                                // -------------------------------------------------------------------------
                                // Family 2: Unused value (FS1182 / FSAC0003) -> Replace with _ / Prefix with _
                                // -------------------------------------------------------------------------
                                let unusedValRanges = ResizeArray<range * Diagnostic option>()

                                for d in checkAtCursor.Diagnostics do
                                    if d.ErrorNumber = 1182 && fcsOverlapsLsp p.Range d.Range then
                                        unusedValRanges.Add(d.Range, Some(fcsDiagToLsp d))

                                if config.UnusedDeclarationsAnalyzer || hasContextCode "FSAC0003" then
                                    let analyzerUnused =
                                        UnusedDeclarations.getUnusedDeclarations (getCheckFull (), isScriptFile fullPath)
                                        |> Async.RunSynchronously

                                    for rng in analyzerUnused do
                                        if
                                            fcsOverlapsLsp p.Range rng
                                            && not (unusedValRanges |> Seq.exists (fun (r, _) -> Range.equals r rng))
                                        then
                                            unusedValRanges.Add(rng, None)

                                for rng, diagOpt in unusedValRanges do
                                    let rawIdent = sliceFcsRange lines rng
                                    let cleanIdent = unquoteBackticks rawIdent

                                    if
                                        not (String.IsNullOrWhiteSpace cleanIdent)
                                        && not (cleanIdent.StartsWith("_", StringComparison.Ordinal))
                                    then
                                        let lspRng = fcsRangeToLsp rng
                                        addLocal "Replace with _" CodeActionKind.QuickFix diagOpt (Some true) [| { Range = lspRng; NewText = "_" } |]

                                        let prefixed =
                                            if rawIdent.StartsWith("``", StringComparison.Ordinal) then
                                                $"``_%s{cleanIdent}``"
                                            else
                                                $"_%s{rawIdent}"

                                        addLocal "Prefix with _" CodeActionKind.QuickFix diagOpt None [| { Range = lspRng; NewText = prefixed } |]

                                // -------------------------------------------------------------------------
                                // Family 3: Simplify redundant qualified name (FSAC0002)
                                // -------------------------------------------------------------------------
                                if config.SimplifyNameAnalyzer || hasContextCode "FSAC0002" then
                                    let simplifiable =
                                        SimplifyNames.getSimplifiableNames (getCheckFull (), getLine)
                                        |> Async.RunSynchronously

                                    for sr in simplifiable do
                                        let fullQualEnd =
                                            Position.mkPos sr.Range.StartLine (sr.Range.StartColumn + sr.RelativeName.Length)

                                        let fullQualRange = Range.mkRange sr.Range.FileName sr.Range.Start fullQualEnd

                                        if fcsOverlapsLsp p.Range sr.Range || fcsOverlapsLsp p.Range fullQualRange then
                                            let lspRng = fcsRangeToLsp sr.Range
                                            addLocal "Remove redundant qualifier" CodeActionKind.QuickFix None (Some true) [| { Range = lspRng; NewText = "" } |]

                                // -------------------------------------------------------------------------
                                // Overlapping compiler diagnostics at cursor: Families 4, 5, 6, 7, 12a
                                // -------------------------------------------------------------------------
                                let overlappingDiags =
                                    checkAtCursor.Diagnostics
                                    |> Array.filter (fun d -> fcsOverlapsLsp p.Range d.Range)

                                for d in overlappingDiags do
                                    let lspDiag = Some(fcsDiagToLsp d)
                                    let lspDiagRange = fcsRangeToLsp d.Range
                                    let diagText = sliceFcsRange lines d.Range

                                    match d.ErrorNumber with
                                    | 39 ->
                                        // Family 7b: Operator typo `==` or `!=`
                                        if diagText = "==" then
                                            addLocal "Replace '==' with '='" CodeActionKind.QuickFix lspDiag (Some true) [| { Range = lspDiagRange; NewText = "=" } |]
                                        elif diagText = "!=" then
                                            addLocal "Replace '!=' with '<>'" CodeActionKind.QuickFix lspDiag (Some true) [| { Range = lspDiagRange; NewText = "<>" } |]
                                        else
                                            // Family 5: Self-reference inside non-recursive `let` binding
                                            match tryFindSelfRecursiveBinding ast d.Range.Start diagText with
                                            | Some(fnName, afterLetPos) ->
                                                let insLsp = fcsPosToLsp afterLetPos

                                                addLocal
                                                    $"Make '%s{fnName}' recursive"
                                                    CodeActionKind.QuickFix
                                                    lspDiag
                                                    (Some true)
                                                    [| { Range = { Start = insLsp; End = insLsp }
                                                         NewText = " rec" } |]
                                            | None -> ()

                                            // Family 4a: Compiler spellcheck suggestions
                                            for cand in resolveSpellcheckSuggestions checkAtCursor view.Parse getLine d.Range diagText d.Message do
                                                addLocal
                                                    $"Replace with '%s{cand}'"
                                                    CodeActionKind.QuickFix
                                                    lspDiag
                                                    None
                                                    [| { Range = lspDiagRange; NewText = cand } |]

                                            // Family 4b: Unopened namespace / module resolution (gated on ResolveNamespaces)
                                            let prevCharIsDot =
                                                let ln = getLine d.Range.StartLine
                                                d.Range.StartColumn > 0 && d.Range.StartColumn <= ln.Length && ln[d.Range.StartColumn - 1] = '.'

                                            if
                                                config.ResolveNamespaces
                                                && not prevCharIsDot
                                                && not (String.IsNullOrWhiteSpace diagText)
                                                && not (diagText.Contains '.')
                                            then
                                                let nsCandidates = resolveUnopenedNamespaceCandidates checkAtCursor diagText
                                                let seenOpenTitles = HashSet<string>(StringComparer.Ordinal)
                                                let seenQualTitles = HashSet<string>(StringComparer.Ordinal)

                                                for openOpt, _ in nsCandidates do
                                                    match openOpt with
                                                    | Some ns ->
                                                        let openTitle = $"open %s{ns}"

                                                        if seenOpenTitles.Add openTitle then
                                                            match computeOpenInsertEdit lines ast d.Range.Start ns with
                                                            | Some openEdit ->
                                                                addLocal openTitle CodeActionKind.QuickFix lspDiag None [| openEdit |]
                                                            | None -> ()
                                                    | None -> ()

                                                for _, fqName in nsCandidates do
                                                    if seenQualTitles.Add fqName then
                                                        addLocal
                                                            fqName
                                                            CodeActionKind.QuickFix
                                                            lspDiag
                                                            None
                                                            [| { Range = lspDiagRange; NewText = fqName } |]

                                    | 576 ->
                                        // Family 5: FS0576 explicit recursive reference error
                                        match tryFindSelfRecursiveBinding ast d.Range.Start diagText with
                                        | Some(fnName, afterLetPos) ->
                                            let insLsp = fcsPosToLsp afterLetPos

                                            addLocal
                                                $"Make '%s{fnName}' recursive"
                                                CodeActionKind.QuickFix
                                                lspDiag
                                                (Some true)
                                                [| { Range = { Start = insLsp; End = insLsp }
                                                     NewText = " rec" } |]
                                        | None -> ()

                                    | 760 ->
                                        // Family 6: Add `new` keyword to IDisposable construction
                                        if not (String.IsNullOrWhiteSpace diagText) then
                                            let replacement =
                                                if isChainedReceiver lines ast d.Range then
                                                    $"(new %s{diagText})"
                                                else
                                                    $"new %s{diagText}"

                                            addLocal
                                                "Add 'new' keyword"
                                                CodeActionKind.QuickFix
                                                lspDiag
                                                (Some true)
                                                [| { Range = lspDiagRange; NewText = replacement } |]

                                    | 20 ->
                                        // Family 7a: FS0020 mutable `<-` vs `|> ignore`
                                        match tryFindMutableEqualityRange lines ast checkAtCursor d.Range with
                                        | Some eqRange ->
                                            addLocal
                                                "Use '<-' to mutate value"
                                                CodeActionKind.QuickFix
                                                lspDiag
                                                (Some true)
                                                [| { Range = fcsRangeToLsp eqRange; NewText = "<-" } |]
                                        | None -> ()

                                        let exprSlice = sliceFcsRange lines d.Range

                                        if not (String.IsNullOrWhiteSpace exprSlice) then
                                            if d.Range.StartLine = d.Range.EndLine then
                                                let endLsp = fcsPosToLsp d.Range.End

                                                addLocal
                                                    "Pipe to 'ignore'"
                                                    CodeActionKind.QuickFix
                                                    lspDiag
                                                    None
                                                    [| { Range = { Start = endLsp; End = endLsp }
                                                         NewText = " |> ignore" } |]
                                            else
                                                addLocal
                                                    "Pipe to 'ignore'"
                                                    CodeActionKind.QuickFix
                                                    lspDiag
                                                    None
                                                    [| { Range = lspDiagRange
                                                         NewText = $"(%s{exprSlice}) |> ignore" } |]

                                    | 10
                                    | 3360 ->
                                        // Family 7c: Missing `=` in type definition
                                        let ln = getLine d.Range.StartLine

                                        if
                                            d.Message.Contains("'='", StringComparison.Ordinal)
                                            || d.ErrorNumber = 3360
                                            || ln.TrimStart().StartsWith("type ", StringComparison.Ordinal)
                                        then
                                            let mutable backCol = min ln.Length d.Range.StartColumn - 1

                                            while backCol >= 0 && Char.IsWhiteSpace ln[backCol] do
                                                backCol <- backCol - 1

                                            if backCol >= 0 && ln[backCol] <> '=' then
                                                let startPos: LspPos =
                                                    { Line = uint32 (d.Range.StartLine - 1)
                                                      Character = uint32 (backCol + 1) }

                                                addLocal
                                                    "Add '='"
                                                    CodeActionKind.QuickFix
                                                    lspDiag
                                                    (Some true)
                                                    [| { Range = { Start = startPos; End = lspDiagRange.Start }
                                                         NewText = " = " } |]

                                    | 34 ->
                                        // Family 12a: Signature mismatch FS0034 -> Update val in signature file
                                        let identAtDiag =
                                            (d.Range.Start, ast)
                                            ||> ParsedInput.tryPick (fun _ node ->
                                                match node with
                                                | SyntaxNode.SynBinding(SynBinding(headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = [ id ])))) when
                                                    Range.equals d.Range id.idRange || Range.rangeContainsPos id.idRange cursorPos
                                                    ->
                                                    Some id
                                                | SyntaxNode.SynBinding(SynBinding(headPat = SynPat.Named(SynIdent(id, _), _, _, _))) when
                                                    Range.equals d.Range id.idRange || Range.rangeContainsPos id.idRange cursorPos
                                                    ->
                                                    Some id
                                                | _ -> None)

                                        match identAtDiag with
                                        | Some implIdent ->
                                            let ln = getLine implIdent.idRange.EndLine

                                            match
                                                checkAtCursor.GetSymbolUseAtLocation(
                                                    implIdent.idRange.EndLine,
                                                    implIdent.idRange.EndColumn,
                                                    ln,
                                                    [ implIdent.idText ]
                                                )
                                            with
                                            | Some su ->
                                                match d.ExtendedData with
                                                | Some(:? ValueNotContainedDiagnosticExtendedData as extData) ->
                                                    match extData.ImplementationValue.GetValSignatureText(su.DisplayContext, su.Range) with
                                                    | Some valText ->
                                                        let sigDecl = extData.SignatureValue.DeclarationLocation

                                                        let locTuple =
                                                            Some(
                                                                sigDecl.StartLine,
                                                                sigDecl.StartColumn,
                                                                sigDecl.EndLine,
                                                                sigDecl.EndColumn
                                                            )

                                                        sigCands.Add(UpdateSigVal(implIdent.idText, locTuple, valText, lspDiag))
                                                    | None -> ()
                                                | _ ->
                                                    match su.Symbol with
                                                    | :? FSharpMemberOrFunctionOrValue as mfv ->
                                                        match mfv.GetValSignatureText(su.DisplayContext, su.Range) with
                                                        | Some valText ->
                                                            sigCands.Add(UpdateSigVal(implIdent.idText, None, valText, lspDiag))
                                                        | None -> ()
                                                    | _ -> ()
                                            | None -> ()
                                        | None -> ()

                                    | _ -> ()

                                // Also check token under cursor for `==` / `!=` even if diagnostic range spanned wider
                                let curLineStr = getLine fcsLine

                                if
                                    not (
                                        actions
                                        |> Seq.exists (fun a ->
                                            a.Title = "Replace '==' with '='" || a.Title = "Replace '!=' with '<>'")
                                    )
                                then
                                    for opFrom, opTo, opTitle in
                                        [ "==", "=", "Replace '==' with '='"
                                          "!=", "<>", "Replace '!=' with '<>'" ] do
                                        let searchFrom = max 0 (fcsCol - 2)
                                        let idx = curLineStr.IndexOf(opFrom, min curLineStr.Length searchFrom, StringComparison.Ordinal)

                                        if idx >= 0 && idx <= fcsCol && fcsCol <= idx + 2 then
                                            let opRng: LspRange =
                                                { Start = { Line = uint32 (fcsLine - 1); Character = uint32 idx }
                                                  End = { Line = uint32 (fcsLine - 1); Character = uint32 (idx + 2) } }

                                            if not (sliceLspRange lines opRng = "") then
                                                addLocal opTitle CodeActionKind.QuickFix None (Some true) [| { Range = opRng; NewText = opTo } |]

                                // -------------------------------------------------------------------------
                                // Family 8: Add explicit type annotation (Refactor)
                                // -------------------------------------------------------------------------
                                match tryExplicitTypeAnnotationEdit lines ast checkAtCursor cursorPos with
                                | Some tyEdit ->
                                    addLocal "Add explicit type annotation" CodeActionKind.Refactor None None [| tyEdit |]
                                | None -> ()

                                // -------------------------------------------------------------------------
                                // Family 9: Implement interface & abstract class member stubs
                                // -------------------------------------------------------------------------
                                if config.InterfaceStubGeneration then
                                    match tryImplementInterfaceEdits config lines ast checkAtCursor cursorPos with
                                    | Some ifaceEdits ->
                                        let hasFs366 = overlappingDiags |> Array.exists (fun d -> d.ErrorNumber = 366)
                                        let kind = if hasFs366 then CodeActionKind.QuickFix else CodeActionKind.Refactor
                                        addLocal "Implement interface" kind None (Some hasFs366) ifaceEdits
                                    | None -> ()

                                if config.AbstractClassStubGeneration then
                                    match tryImplementAbstractClassEdits config lines ast checkAtCursor cursorPos with
                                    | Some absEdits ->
                                        let hasFs365 = overlappingDiags |> Array.exists (fun d -> d.ErrorNumber = 365)
                                        let kind = if hasFs365 then CodeActionKind.QuickFix else CodeActionKind.Refactor
                                        addLocal "Implement abstract members" kind None (Some hasFs365) absEdits
                                    | None -> ()

                                // -------------------------------------------------------------------------
                                // Family 10: Generate missing union pattern match cases
                                // -------------------------------------------------------------------------
                                if config.UnionCaseStubGeneration then
                                    match tryGenerateUnionCasesEdits config lines ast checkAtCursor cursorPos with
                                    | Some ucEdits ->
                                        let fs25Diag =
                                            overlappingDiags
                                            |> Array.tryFind (fun d -> d.ErrorNumber = 25)
                                            |> Option.map fcsDiagToLsp

                                        addLocal "Generate union pattern match cases" CodeActionKind.QuickFix fs25Diag (Some true) ucEdits
                                    | None -> ()

                                // -------------------------------------------------------------------------
                                // Family 11: Generate missing record fields
                                // -------------------------------------------------------------------------
                                if config.RecordStubGeneration then
                                    match tryGenerateRecordStubEdits config lines ast checkAtCursor cursorPos with
                                    | Some recEdits ->
                                        let fs764Diag =
                                            overlappingDiags
                                            |> Array.tryFind (fun d -> d.ErrorNumber = 764)
                                            |> Option.map fcsDiagToLsp

                                        addLocal "Generate record stub" CodeActionKind.QuickFix fs764Diag (Some true) recEdits
                                    | None -> ()

                                // -------------------------------------------------------------------------
                                // Family 12b: Add missing module-level binding to paired `.fsi` file
                                // -------------------------------------------------------------------------
                                match ast with
                                | ParsedInput.SigFile _ -> ()
                                | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
                                    let rec findModuleLetAtCursor (modPrefix: string list) (decls: SynModuleDecl list) =
                                        decls
                                        |> List.tryPick (fun d ->
                                            if not (Range.rangeContainsPos d.Range cursorPos) then
                                                None
                                            else
                                                match d with
                                                | SynModuleDecl.Let(bindings = bs) ->
                                                    bs
                                                    |> List.tryPick (fun (SynBinding(accessibility = acc; headPat = hp)) ->
                                                        let isPrivate =
                                                            match acc with
                                                            | Some(SynAccess.Private _) -> true
                                                            | _ -> false

                                                        if isPrivate || not (Range.rangeContainsPos hp.Range cursorPos) then
                                                            None
                                                        else
                                                            match hp with
                                                            | SynPat.LongIdent(longDotId = SynLongIdent(id = [ id ])) -> Some(modPrefix, id)
                                                            | SynPat.Named(SynIdent(id, _), _, _, _) -> Some(modPrefix, id)
                                                            | SynPat.Typed(SynPat.Named(SynIdent(id, _), _, _, _), _, _) -> Some(modPrefix, id)
                                                            | _ -> None)
                                                | SynModuleDecl.NestedModule(
                                                    moduleInfo = ci
                                                    decls = inner) ->
                                                    findModuleLetAtCursor (modPrefix @ (ci.LongIdent |> List.map _.idText)) inner
                                                | _ -> None)

                                    let hitLet =
                                        modules
                                        |> List.tryPick (fun (SynModuleOrNamespace(longId = lid; decls = decls; range = r)) ->
                                            if Range.rangeContainsPos r cursorPos then
                                                findModuleLetAtCursor (lid |> List.map _.idText) decls
                                            else
                                                None)

                                    match hitLet with
                                    | Some(modPath, bindIdent) ->
                                        let alreadyUpdating =
                                            sigCands
                                            |> Seq.exists (function
                                                | UpdateSigVal(name, _, _, _) -> String.Equals(name, bindIdent.idText, StringComparison.Ordinal)
                                                | _ -> false)

                                        if not alreadyUpdating then
                                            let ln = getLine bindIdent.idRange.EndLine

                                            match
                                                checkAtCursor.GetSymbolUseAtLocation(
                                                    bindIdent.idRange.EndLine,
                                                    bindIdent.idRange.EndColumn,
                                                    ln,
                                                    [ bindIdent.idText ]
                                                )
                                            with
                                            | Some su ->
                                                match su.Symbol with
                                                | :? FSharpMemberOrFunctionOrValue as mfv ->
                                                    match mfv.GetValSignatureText(su.DisplayContext, su.Range) with
                                                    | Some valText ->
                                                        sigCands.Add(AddSigVal(modPath, bindIdent.idText, valText))
                                                    | None -> ()
                                                | _ -> ()
                                            | None -> ()
                                    | None -> ()

                                view.Version, actions |> Seq.toList, sigCands |> Seq.toList)),
                        ct
                    )

                if localActions.IsEmpty && sigCandidates.IsEmpty then
                    return Some [||]
                else
                    // Capture document versions and optional paired `.fsi` TextView in a single settled turn on the GraphHost thread!
                    let capturedVersions = Dictionary<string, int option>(StringComparer.OrdinalIgnoreCase)

                    let! parsesOpt =
                        graph.LatestParsesInOneTurn(
                            (fun () ->
                                let loading = scope.IsLoading()

                                let hasPairedSig =
                                    not loading
                                    && not sigCandidates.IsEmpty
                                    && (scope.OwnersOf sigFullPath |> List.exists (fun g -> obj.ReferenceEquals(g, graph)))

                                let requestedPaths =
                                    if hasPairedSig then
                                        [ fullPath; sigFullPath ]
                                    else
                                        [ fullPath ]

                                if not loading then
                                    capturedVersions.Clear()

                                    for fp in requestedPaths do
                                        capturedVersions[fp] <- versionNow fp

                                struct (loading, [| graph, requestedPaths |])),
                            ct
                        )

                    match parsesOpt with
                    | ValueNone -> return Some [||]
                    | ValueSome parsedProjects ->
                        let viewByPath = Dictionary<string, TextView>(StringComparer.OrdinalIgnoreCase)

                        for _, fileViews in parsedProjects do
                            for fp, tv in fileViews do
                                viewByPath[Path.GetFullPath fp] <- tv

                        match viewByPath.TryGetValue fullPath with
                        | true, fsTv when fsTv.Version <> fsVersion ->
                            if attempt < 3 then
                                return! runAttempt (attempt + 1)
                            else
                                return Some [||]
                        | _ ->
                            let sigActions =
                                match viewByPath.TryGetValue sigFullPath with
                                | true, sigTv ->
                                    StackGuard.onLargeStack (fun () ->
                                        sigCandidates |> List.choose (resolveSigSyncAction sigFullPath sigTv))
                                | _ -> []

                            let allRaw = localActions @ sigActions

                            let finalActions =
                                allRaw
                                |> List.filter (fun ra -> matchesOnlyFilter p.Context.Only ra.Kind)
                                |> List.map (fun ra ->
                                    let verOpt =
                                        match capturedVersions.TryGetValue ra.TargetPath with
                                        | true, v -> v
                                        | _ -> None

                                    let wsEdit = buildWorkspaceEdit canDocumentChanges [ ra.TargetPath, verOpt, ra.Edits ]

                                    U2.C2
                                        { Title = ra.Title
                                          Kind = Some ra.Kind
                                          Diagnostics = ra.Diagnostics
                                          IsPreferred = ra.IsPreferred
                                          Disabled = None
                                          Edit = Some wsEdit
                                          Command = None
                                          Data = None })
                                |> Array.ofList

                            return Some finalActions
            }

        runAttempt 0

let provide
    (config: FSharpConfig)
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (p: CodeActionParams)
    (ct: CancellationToken)
    : Task<TextDocumentCodeActionResult option> =
    provideWithCapabilities config false (fun _ -> None) scope graph path p ct
