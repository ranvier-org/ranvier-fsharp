module Rfs.Server.InlineValue

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

[<Struct>]
type internal PipeStageInfo =
    { StageLine: int
      PrecedingNonPipeLine: int voption
      RootLine: int voption
      RootInputType: string voption
      StageTypes: string[]
      OutputType: string }

let inline private posLeq (a: LspPos) (b: LspPos) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let inline private inLspRange (outer: LspRange) (p: LspPos) =
    posLeq outer.Start p && posLeq p outer.End

let private tryPipeOpIdent (expr: SynExpr) : Ident option =
    let isPipeName (s: string) =
        s = "op_PipeRight" || s = "op_PipeRight2" || s = "op_PipeRight3"

    match expr with
    | SynExpr.Ident id when isPipeName id.idText -> Some id
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty && isPipeName (List.last ids).idText ->
        Some(List.last ids)
    | _ -> None

let private tryMatchPipeStep (expr: SynExpr) : (Ident * SynExpr * SynExpr * range) option =
    match expr with
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.App(isInfix = true; funcExpr = opExpr; argExpr = lhs)
        argExpr = rhs
        range = wholeRange) ->
        match tryPipeOpIdent opExpr with
        | Some opIdent -> Some(opIdent, lhs, rhs, wholeRange)
        | None -> None
    | _ -> None

let rec private unwindPipeChain
    (expr: SynExpr)
    (accStages: (Ident * SynExpr * range) list)
    : SynExpr * (Ident * SynExpr * range) list =
    match tryMatchPipeStep expr with
    | Some(opIdent, lhs, rhs, wholeRange) -> unwindPipeChain lhs ((opIdent, rhs, wholeRange) :: accStages)
    | None -> expr, accStages

let internal collectPipelines (view: FileView) (lines: string[]) : PipeStageInfo[] =
    StackGuard.onLargeStack (fun () ->
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

        let found = ResizeArray<PipeStageInfo>()

        let resolvePipeTypes (opIdent: Ident) : string[] option =
            if opIdent.idRange.IsSynthetic then
                None
            else
                match lineAt opIdent.idRange.StartLine with
                | None -> None
                | Some lineStr ->
                    let check = checkAt opIdent.idRange.StartLine opIdent.idRange.EndColumn

                    match check.GetSymbolUseAtLocation(opIdent.idRange.StartLine, opIdent.idRange.EndColumn, lineStr, [ opIdent.idText ]) with
                    | None -> None
                    | Some u ->
                        if u.GenericArguments.Length >= 2 then
                            try
                                let formatted = [| for (_, t) in u.GenericArguments -> t.Format u.DisplayContext |]

                                if formatted |> Array.forall (fun s -> not (String.IsNullOrWhiteSpace s)) then
                                    Some formatted
                                else
                                    None
                            with _ ->
                                None
                        else
                            None

        let rec walkBinding (SynBinding(expr = body)) = walkExpr body

        and walkExpr (expr: SynExpr) =
            match tryMatchPipeStep expr with
            | Some _ ->
                let rootExpr, stages = unwindPipeChain expr []
                walkExpr rootExpr

                let rootEndLine0 = max 0 (rootExpr.Range.EndLine - 1)
                let mutable prevEndLine0 = rootEndLine0

                stages
                |> List.iteri (fun idx (opIdent, rhs, stageRange) ->
                    let stageEndLine0 = max 0 (stageRange.EndLine - 1)

                    match resolvePipeTypes opIdent with
                    | Some types when types.Length >= 2 ->
                        let outType = types[types.Length - 1]

                        let inType =
                            if types.Length = 2 then
                                types[0]
                            else
                                String.concat " * " types[0 .. types.Length - 2]

                        let pnp =
                            if idx = 0 && rootEndLine0 < stageEndLine0 then
                                ValueSome rootEndLine0
                            elif idx > 0 && prevEndLine0 < stageEndLine0 then
                                ValueNone
                            else
                                ValueNone

                        found.Add
                            { StageLine = stageEndLine0
                              PrecedingNonPipeLine = pnp
                              RootLine = if idx = 0 then ValueSome rootEndLine0 else ValueNone
                              RootInputType = if idx = 0 then ValueSome inType else ValueNone
                              StageTypes = types
                              OutputType = outType }
                    | _ -> ()

                    prevEndLine0 <- stageEndLine0
                    walkExpr rhs)

            | None ->
                match expr with
                | SynExpr.Lambda(body = body; parsedData = parsedData) ->
                    match parsedData with
                    | Some(_, realBody) -> walkExpr realBody
                    | None -> walkExpr body
                | SynExpr.App(funcExpr = fn; argExpr = arg) ->
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
                | SynExpr.DotSet(targetExpr = e1; rhsExpr = e2) ->
                    walkExpr e1
                    walkExpr e2
                | SynExpr.TryWith(tryExpr = t; withCases = clauses) ->
                    walkExpr t

                    for SynMatchClause(whenExpr = wOpt; resultExpr = res) in clauses do
                        wOpt |> Option.iter walkExpr
                        walkExpr res
                | SynExpr.TryFinally(tryExpr = t; finallyExpr = f) ->
                    walkExpr t
                    walkExpr f
                | _ -> ()

        let rec walkMember (m: SynMemberDefn) =
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

        found.ToArray())

let provide
    (config: InlineValuesConfig)
    (graph: ProjectGraph)
    (path: string)
    (range: Range)
    (ct: CancellationToken)
    : Task<InlineValue[] option> =
    if config.Enabled = Some false || path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
        Task.FromResult(Some [||])
    else
        let prefix = if String.IsNullOrWhiteSpace config.Prefix then "//" else config.Prefix.Trim()

        graph.ReadFile(
            path,
            (fun view -> view.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')),
            (fun view lines ->
                let stages = collectPipelines view lines
                let byLine = Dictionary<int, string>()

                for s in stages do
                    match s.RootLine, s.RootInputType with
                    | ValueSome rl, ValueSome rt when rl <> s.StageLine ->
                        byLine[rl] <- rt
                    | _ -> ()

                    byLine[s.StageLine] <- s.OutputType

                let results = ResizeArray<InlineValue>()

                for kv in byLine |> Seq.sortBy _.Key do
                    let ln = kv.Key

                    if ln >= 0 && ln < lines.Length then
                        let endCol = lines[ln].Length
                        let pos: LspPos = { Line = uint32 ln; Character = uint32 endCol }
                        let inRange = inLspRange range pos

                        if inRange then
                            results.Add(
                                U3.C1
                                    { Range = { Start = pos; End = pos }
                                      Text = $"%s{prefix} %s{kv.Value}" }
                            )

                Some(results.ToArray())),
            ct
        )
