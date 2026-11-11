module Rfs.Server.InlayHints

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

let inline private posLeq (a: LspPos) (b: LspPos) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let inline private inLspRange (outer: LspRange) (p: LspPos) =
    posLeq outer.Start p && posLeq p outer.End

let inline private fcsPosToLsp (pos: pos) : LspPos =
    { Line = uint32 (max 0 (pos.Line - 1))
      Character = uint32 (max 0 pos.Column) }

let inline private zeroRange (p: LspPos) : LspRange = { Start = p; End = p }

let inline private rangesOverlapLines (lspRange: LspRange) (r: range) =
    if r.IsSynthetic then
        true
    else
        let minLine = int lspRange.Start.Line + 1
        let maxLine = int lspRange.End.Line + 1
        r.EndLine >= minLine && r.StartLine <= maxLine

let private isCompilerArgName (name: string) =
    if String.IsNullOrWhiteSpace name || name = "_" then
        true
    elif name.StartsWith("arg", StringComparison.Ordinal) && name.Length > 3 then
        let mutable allDigits = true

        for i in 3 .. name.Length - 1 do
            if not (Char.IsDigit name[i]) then
                allDigits <- false

        allDigits
    else
        false

let rec private trySimpleArgIdent (expr: SynExpr) : string option =
    match expr with
    | SynExpr.Paren(expr = inner) -> trySimpleArgIdent inner
    | SynExpr.Ident id -> Some id.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
        Some (List.last ids).idText
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
        Some (List.last ids).idText
    | _ -> None

let rec private isNamedArgumentExpr (expr: SynExpr) : bool =
    match expr with
    | SynExpr.Paren(expr = inner) -> isNamedArgumentExpr inner
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; funcExpr = SynExpr.Ident op; argExpr = SynExpr.Ident _)) when
        op.idText = "op_Equality"
        ->
        true
    | _ -> false

let rec private tryExtractCalleeIdent (expr: SynExpr) : (Ident * string list) option =
    match expr with
    | SynExpr.Paren(expr = inner) -> tryExtractCalleeIdent inner
    | SynExpr.TypeApp(expr = inner) -> tryExtractCalleeIdent inner
    | SynExpr.Ident id ->
        if id.idRange.IsSynthetic || PrettyNaming.IsOperatorDisplayName id.idText || id.idText.StartsWith("op_", StringComparison.Ordinal) then
            None
        else
            Some(id, [ id.idText ])
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
        let last = List.last ids

        if last.idRange.IsSynthetic || PrettyNaming.IsOperatorDisplayName last.idText || last.idText.StartsWith("op_", StringComparison.Ordinal) then
            None
        else
            Some(last, ids |> List.map _.idText)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
        let last = List.last ids

        if last.idRange.IsSynthetic || PrettyNaming.IsOperatorDisplayName last.idText || last.idText.StartsWith("op_", StringComparison.Ordinal) then
            None
        else
            Some(last, [ last.idText ])
    | _ -> None

let private buildTooltip (config: InlayHintsConfig) (kind: InlayHintKind) : U2<string, MarkupContent> option =
    if config.DisableLongTooltip then
        None
    else
        let hideCmd =
            if kind = InlayHintKind.Type then
                "fsharp.inlayHints.hideTypeAnnotations"
            else
                "fsharp.inlayHints.hideParameterNames"

        let md =
            String.concat
                "\n"
                [ $"To hide these hints, [click here](command:%s{hideCmd})."
                  "To hide *ALL* hints, [click here](command:fsharp.inlayHints.hideAll)."
                  "Finally, to dismiss this long tooltip forever, [click here](command:fsharp.inlayHints.disableLongTooltip)." ]

        Some(U2.C2 { Kind = MarkupKind.Markdown; Value = md })

let provide
    (config: InlayHintsConfig)
    (graph: ProjectGraph)
    (path: string)
    (range: Range)
    (ct: CancellationToken)
    : Task<InlayHint[] option> =
    if (not config.TypeAnnotations && not config.ParameterNames) || path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
        Task.FromResult(Some [||])
    else
        graph.ReadFile(
            path,
            (fun view -> view.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')),
            (fun view lines ->
                StackGuard.onLargeStack (fun () ->
                    // Promoted vs Unpromoted check discipline:
                    // - When `view.Whole` is true (unpromoted), read `view.Result.CheckResultsNotKept` (never `view.Result.CheckResults`!).
                    // - When `view.Whole` is false (promoted Declarations mode), read `view.CheckResultsAt(fcsLine, fcsCol)` cached per row owner.
                    let wholeCheck = lazy view.Result.CheckResultsNotKept
                    let rowChecks = Dictionary<obj, FSharpCheckFileResults>(HashIdentity.Reference)

                    let checkAt (fcsLine: int) (fcsCol: int) : FSharpCheckFileResults =
                        if view.Whole then
                            wholeCheck.Value
                        else
                            match view.RowAt fcsLine with
                            | ValueSome(struct (owner, _)) ->
                                match rowChecks.TryGetValue owner with
                                | true, cached -> cached
                                | _ ->
                                    let c = view.CheckResultsAt(fcsLine, fcsCol)
                                    rowChecks[owner] <- c
                                    c
                            | ValueNone -> view.CheckResultsAt(fcsLine, fcsCol)

                    let lineAt (fcsLine: int) : string option =
                        let idx = fcsLine - 1
                        if idx >= 0 && idx < lines.Length then Some lines[idx] else None

                    let hints = ResizeArray<InlayHint>()
                    let seenPositions = HashSet<struct (uint32 * uint32 * int)>()

                    let tryFormatIdentType (ident: Ident) : string option =
                        if ident.idRange.IsSynthetic then
                            None
                        else
                            match lineAt ident.idRange.StartLine with
                            | None -> None
                            | Some lineStr ->
                                let check = checkAt ident.idRange.StartLine ident.idRange.EndColumn

                                match check.GetSymbolUseAtLocation(ident.idRange.StartLine, ident.idRange.EndColumn, lineStr, [ ident.idText ]) with
                                | None -> None
                                | Some u ->
                                    match u.Symbol with
                                    | :? FSharpMemberOrFunctionOrValue as mfv ->
                                        try
                                            let formatted = mfv.FullType.Format u.DisplayContext

                                            if String.IsNullOrWhiteSpace formatted || formatted.StartsWith("'_a", StringComparison.Ordinal) then
                                                None
                                            else
                                                Some formatted
                                        with _ ->
                                            None
                                    | :? FSharpField as fld ->
                                        try
                                            Some(fld.FieldType.Format u.DisplayContext)
                                        with _ ->
                                            None
                                    | _ -> None

                    let emitTypeHint (ident: Ident) (wrapInParens: bool) =
                        let endPos = fcsPosToLsp ident.idRange.End

                        if inLspRange range endPos && seenPositions.Add(struct (endPos.Line, endPos.Character, 1)) then
                            match tryFormatIdentType ident with
                            | None -> ()
                            | Some typeStr ->
                                let edits =
                                    if wrapInParens then
                                        let startPos = fcsPosToLsp ident.idRange.Start

                                        [| { Range = zeroRange startPos; NewText = "(" }
                                           { Range = zeroRange endPos; NewText = $": %s{typeStr})" } |]
                                    else
                                        [| { Range = zeroRange endPos; NewText = $": %s{typeStr}" } |]

                                hints.Add
                                    { Position = endPos
                                      Label = U2.C1 $": %s{typeStr}"
                                      Kind = Some InlayHintKind.Type
                                      TextEdits = Some edits
                                      Tooltip = buildTooltip config InlayHintKind.Type
                                      PaddingLeft = Some true
                                      PaddingRight = None
                                      Data = None }

                    let rec checkParamPat (alreadyParenthesized: bool) (pat: SynPat) =
                        match pat with
                        | SynPat.Typed _ -> ()
                        | SynPat.Paren(pat = inner) -> checkParamPat true inner
                        | SynPat.Attrib(pat = inner) -> checkParamPat alreadyParenthesized inner
                        | SynPat.Named(ident = SynIdent(ident, _)) ->
                            if not ident.idRange.IsSynthetic && ident.idText <> "_" then
                                emitTypeHint ident (not alreadyParenthesized)
                        | SynPat.Tuple(elementPats = elems) ->
                            for elem in elems do
                                checkParamPat true elem
                        | _ -> ()

                    let emitParamHintForArg (param: FSharpParameter) (argExpr: SynExpr) =
                        match param.Name with
                        | None -> ()
                        | Some paramName when isCompilerArgName paramName -> ()
                        | Some paramName ->
                            if not (isNamedArgumentExpr argExpr) && not argExpr.Range.IsSynthetic then
                                let isSameAsParam =
                                    match trySimpleArgIdent argExpr with
                                    | Some argIdent -> String.Equals(argIdent, paramName, StringComparison.Ordinal)
                                    | None -> false

                                if not isSameAsParam then
                                    let pos = fcsPosToLsp argExpr.Range.Start

                                    if inLspRange range pos && seenPositions.Add(struct (pos.Line, pos.Character, 2)) then
                                        hints.Add
                                            { Position = pos
                                              Label = U2.C1 $"%s{paramName} ="
                                              Kind = Some InlayHintKind.Parameter
                                              TextEdits = None
                                              Tooltip = buildTooltip config InlayHintKind.Parameter
                                              PaddingLeft = None
                                              PaddingRight = Some true
                                              Data = None }

                    let processCallHints (calleeExpr: SynExpr) (argExprs: SynExpr list) =
                        match tryExtractCalleeIdent calleeExpr with
                        | None -> ()
                        | Some(calleeIdent, names) ->
                            match lineAt calleeIdent.idRange.StartLine with
                            | None -> ()
                            | Some lineStr ->
                                let check = checkAt calleeIdent.idRange.StartLine calleeIdent.idRange.EndColumn

                                match check.GetSymbolUseAtLocation(calleeIdent.idRange.StartLine, calleeIdent.idRange.EndColumn, lineStr, names) with
                                | None -> ()
                                | Some u ->
                                    match u.Symbol with
                                    | :? FSharpMemberOrFunctionOrValue as mfv when
                                        not (PrettyNaming.IsOperatorDisplayName mfv.DisplayName)
                                        && mfv.CurriedParameterGroups.Count > 0
                                        ->
                                        let groups = mfv.CurriedParameterGroups
                                        let count = min argExprs.Length groups.Count

                                        for i in 0 .. count - 1 do
                                            let grp = groups[i]
                                            let arg = argExprs[i]

                                            if grp.Count = 1 then
                                                let unwrapped =
                                                    match arg with
                                                    | SynExpr.Paren(expr = inner) -> inner
                                                    | other -> other

                                                match unwrapped with
                                                | SynExpr.Const(SynConst.Unit, _) -> ()
                                                | _ -> emitParamHintForArg grp[0] arg
                                            elif grp.Count > 1 then
                                                match arg with
                                                | SynExpr.Paren(expr = SynExpr.Tuple(exprs = tupleArgs))
                                                | SynExpr.Tuple(exprs = tupleArgs) ->
                                                    let tCount = min tupleArgs.Length grp.Count

                                                    for j in 0 .. tCount - 1 do
                                                        emitParamHintForArg grp[j] tupleArgs[j]
                                                | _ -> ()
                                    | _ -> ()

                    let rec unwrapCurriedApp (expr: SynExpr) (accArgs: SynExpr list) : SynExpr * SynExpr list =
                        match expr with
                        | SynExpr.App(isInfix = false; funcExpr = fn; argExpr = arg) ->
                            unwrapCurriedApp fn (arg :: accArgs)
                        | _ -> expr, accArgs

                    let rec walkBinding (SynBinding(headPat = headPat; returnInfo = returnInfo; expr = body) as b) =
                        if rangesOverlapLines range b.RangeOfBindingWithRhs then
                            if config.TypeAnnotations then
                                match headPat with
                                | SynPat.Named(ident = SynIdent(ident, _)) when returnInfo.IsNone ->
                                    if not ident.idRange.IsSynthetic && ident.idText <> "_" then
                                        emitTypeHint ident false
                                | SynPat.LongIdent(argPats = SynArgPats.Pats argPats) ->
                                    for p in argPats do
                                        checkParamPat false p
                                | _ -> ()

                            walkExpr body

                    and walkExpr (expr: SynExpr) =
                        if rangesOverlapLines range expr.Range then
                            match expr with
                            | SynExpr.Lambda(args = args; body = body; parsedData = parsedData) ->
                                match parsedData with
                                | Some(pats, realBody) ->
                                    if config.TypeAnnotations then
                                        for p in pats do
                                            checkParamPat false p

                                    walkExpr realBody
                                | None ->
                                    if config.TypeAnnotations then
                                        let rec walkSimplePats sp =
                                            match sp with
                                            | SynSimplePats.SimplePats(pats = spats) ->
                                                for s in spats do
                                                    match s with
                                                    | SynSimplePat.Id(ident = id; isCompilerGenerated = false) ->
                                                        let alreadyParen =
                                                            match lineAt id.idRange.StartLine with
                                                            | Some ln when
                                                                id.idRange.StartColumn > 0
                                                                && id.idRange.EndColumn < ln.Length
                                                                && ln[id.idRange.StartColumn - 1] = '('
                                                                && ln[id.idRange.EndColumn] = ')'
                                                                ->
                                                                true
                                                            | _ -> false

                                                        emitTypeHint id (not alreadyParen)
                                                    | _ -> ()

                                        walkSimplePats args

                                    walkExpr body

                            | SynExpr.App(isInfix = false) ->
                                let head, args = unwrapCurriedApp expr []

                                if config.ParameterNames && not args.IsEmpty then
                                    processCallHints head args

                                walkExpr head

                                for a in args do
                                    walkExpr a

                            | SynExpr.App(isInfix = true; funcExpr = fn; argExpr = arg) ->
                                walkExpr fn
                                walkExpr arg

                            | SynExpr.LetOrUse letOrUse ->
                                for b in letOrUse.Bindings do
                                    walkBinding b

                                walkExpr letOrUse.Body

                            | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
                                walkExpr e1
                                walkExpr e2

                            | SynExpr.IfThenElse(ifExpr = c; thenExpr = t; elseExpr = eOpt) ->
                                walkExpr c
                                walkExpr t
                                eOpt |> Option.iter walkExpr

                            | SynExpr.Match(expr = scrut; clauses = clauses)
                            | SynExpr.MatchBang(expr = scrut; clauses = clauses) ->
                                walkExpr scrut

                                for SynMatchClause(whenExpr = wOpt; resultExpr = res) in clauses do
                                    wOpt |> Option.iter walkExpr
                                    walkExpr res

                            | SynExpr.MatchLambda(matchClauses = clauses) ->
                                for SynMatchClause(whenExpr = wOpt; resultExpr = res) in clauses do
                                    wOpt |> Option.iter walkExpr
                                    walkExpr res

                            | SynExpr.Paren(expr = inner)
                            | SynExpr.Typed(expr = inner)
                            | SynExpr.TypeApp(expr = inner)
                            | SynExpr.Do(expr = inner)
                            | SynExpr.DoBang(expr = inner)
                            | SynExpr.YieldOrReturn(expr = inner)
                            | SynExpr.YieldOrReturnFrom(expr = inner)
                            | SynExpr.ComputationExpr(expr = inner)
                            | SynExpr.Lazy(expr = inner)
                            | SynExpr.Assert(expr = inner)
                            | SynExpr.Upcast(expr = inner)
                            | SynExpr.Downcast(expr = inner)
                            | SynExpr.InferredUpcast(expr = inner)
                            | SynExpr.InferredDowncast(expr = inner)
                            | SynExpr.DotGet(expr = inner) ->
                                walkExpr inner

                            | SynExpr.Tuple(exprs = es)
                            | SynExpr.ArrayOrList(exprs = es) ->
                                for e in es do
                                    walkExpr e

                            | SynExpr.Record(copyInfo = copyOpt; recordFields = fields) ->
                                copyOpt |> Option.iter (fst >> walkExpr)

                                for f in fields do
                                    match f with
                                    | SynExprRecordFieldOrSpread.Field(SynExprRecordField(expr = eOpt), _) ->
                                        eOpt |> Option.iter walkExpr
                                    | SynExprRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) ->
                                        walkExpr e

                            | SynExpr.AnonRecd(copyInfo = copyOpt; recordFields = fields) ->
                                copyOpt |> Option.iter (fst >> walkExpr)

                                for f in fields do
                                    match f with
                                    | SynExprAnonRecordFieldOrSpread.Field(SynExprAnonRecordField(expr = e), _) ->
                                        walkExpr e
                                    | SynExprAnonRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) ->
                                        walkExpr e

                            | SynExpr.For(identBody = e1; toBody = e2; doBody = e3) ->
                                walkExpr e1
                                walkExpr e2
                                walkExpr e3

                            | SynExpr.ForEach(enumExpr = e1; bodyExpr = e2)
                            | SynExpr.While(whileExpr = e1; doExpr = e2)
                            | SynExpr.WhileBang(whileExpr = e1; doExpr = e2)
                            | SynExpr.DotIndexedGet(objectExpr = e1; indexArgs = e2) ->
                                walkExpr e1
                                walkExpr e2

                            | SynExpr.LongIdentSet(expr = rhs) ->
                                walkExpr rhs

                            | SynExpr.DotSet(targetExpr = e1; rhsExpr = e2) ->
                                walkExpr e1
                                walkExpr e2

                            | SynExpr.DotIndexedSet(objectExpr = e1; indexArgs = e2; valueExpr = e3) ->
                                walkExpr e1
                                walkExpr e2
                                walkExpr e3

                            | SynExpr.TryWith(tryExpr = t; withCases = clauses) ->
                                walkExpr t

                                for SynMatchClause(whenExpr = wOpt; resultExpr = res) in clauses do
                                    wOpt |> Option.iter walkExpr
                                    walkExpr res

                            | SynExpr.TryFinally(tryExpr = t; finallyExpr = f) ->
                                walkExpr t
                                walkExpr f

                            | SynExpr.ObjExpr(argOptions = argOpt; bindings = bindings; members = members; extraImpls = impls) ->
                                argOpt |> Option.iter (fst >> walkExpr)

                                for b in bindings do
                                    walkBinding b

                                for m in members do
                                    walkMember m

                                for SynInterfaceImpl(bindings = ibs; members = ims) in impls do
                                    for b in ibs do
                                        walkBinding b

                                    for m in ims do
                                        walkMember m

                            | _ -> ()

                    and walkMember (m: SynMemberDefn) =
                        match m with
                        | SynMemberDefn.Member(memberDefn = b) -> walkBinding b
                        | SynMemberDefn.LetBindings(bindings = bs) ->
                            for b in bs do
                                walkBinding b
                        | SynMemberDefn.GetSetMember(memberDefnForGet = gOpt; memberDefnForSet = sOpt) ->
                            gOpt |> Option.iter walkBinding
                            sOpt |> Option.iter walkBinding
                        | SynMemberDefn.AutoProperty(synExpr = e) -> walkExpr e
                        | SynMemberDefn.Interface(members = msOpt) ->
                            msOpt |> Option.iter (List.iter walkMember)
                        | _ -> ()

                    let rec walkDecls (decls: SynModuleDecl list) =
                        for decl in decls do
                            if rangesOverlapLines range decl.Range then
                                match decl with
                                | SynModuleDecl.Let(bindings = bs) ->
                                    for b in bs do
                                        walkBinding b
                                | SynModuleDecl.Expr(expr = e) -> walkExpr e
                                | SynModuleDecl.NestedModule(decls = nested) -> walkDecls nested
                                | SynModuleDecl.Types(typeDefns = tds) ->
                                    for SynTypeDefn(typeRepr = repr; members = ms) in tds do
                                        match repr with
                                        | SynTypeDefnRepr.ObjectModel(members = oms) ->
                                            for m in oms do
                                                walkMember m
                                        | _ -> ()

                                        for m in ms do
                                            walkMember m
                                | _ -> ()

                    match view.Parse.ParseTree with
                    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
                        for SynModuleOrNamespace(decls = decls) in modules do
                            walkDecls decls
                    | ParsedInput.SigFile _ -> ()

                    hints
                    |> Seq.sortBy (fun h -> h.Position.Line, h.Position.Character, (if h.Kind = Some InlayHintKind.Type then 0 else 1))
                    |> Array.ofSeq
                    |> Some)),
            ct
        )
