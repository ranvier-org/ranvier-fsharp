module Rfs.Server.SelectionRange

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

let inline private posLeq (a: LspPos) (b: LspPos) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let inline private rangeContains (outer: LspRange) (inner: LspRange) =
    posLeq outer.Start inner.Start && posLeq inner.End outer.End

let inline private toLspRange (r: range) : LspRange =
    { Start =
        { Line = uint32 (max 0 (r.StartLine - 1))
          Character = uint32 (max 0 r.StartColumn) }
      End =
        { Line = uint32 (max 0 (r.EndLine - 1))
          Character = uint32 (max 0 r.EndColumn) } }

/// Sorts containing ranges from outermost (smallest Start, largest End) to innermost (largest Start, smallest End).
let private compareOutermostFirst (a: LspRange) (b: LspRange) : int =
    let c1 = a.Start.Line.CompareTo b.Start.Line

    if c1 <> 0 then
        c1
    else
        let c2 = a.Start.Character.CompareTo b.Start.Character

        if c2 <> 0 then
            c2
        else
            let c3 = b.End.Line.CompareTo a.End.Line

            if c3 <> 0 then c3 else b.End.Character.CompareTo a.End.Character

let internal compute (text: string) (parseTree: ParsedInput) (positions: LspPos[]) : SelectionRange[] =
    if isNull (box positions) || positions.Length = 0 then
        [||]
    else
        StackGuard.onLargeStack (fun () ->
            let count = positions.Length

            let fcsPositions =
                Array.init count (fun i ->
                    let p = positions[i]
                    Position.mkPos (int p.Line + 1) (int p.Character))

            let buckets = Array.init count (fun _ -> ResizeArray<LspRange>())

            // Whole-document enclosing range when file is non-empty
            let lines = text.Split('\n') |> Array.map (fun s -> s.TrimEnd('\r'))

            if lines.Length > 1 || (lines.Length = 1 && lines[0].Length > 0) then
                let docRange: LspRange =
                    { Start = { Line = 0u; Character = 0u }
                      End =
                        { Line = uint32 (lines.Length - 1)
                          Character = uint32 lines[lines.Length - 1].Length } }

                for i in 0 .. count - 1 do
                    if posLeq docRange.Start positions[i] && posLeq positions[i] docRange.End then
                        buckets[i].Add docRange

            let considerRange (r: range) =
                if
                    not (Range.equals r Range.range0)
                    && r.StartLine >= 1
                    && (r.EndLine > r.StartLine || (r.EndLine = r.StartLine && r.EndColumn > r.StartColumn))
                then
                    let mutable converted = ValueNone

                    for i in 0 .. count - 1 do
                        if Range.rangeContainsPos r fcsPositions[i] then
                            let lspR =
                                match converted with
                                | ValueSome x -> x
                                | ValueNone ->
                                    let x = toLspRange r
                                    converted <- ValueSome x
                                    x

                            buckets[i].Add lspR

            let considerIdent (id: Ident) = considerRange id.idRange

            let considerLongIdent (lid: LongIdent) =
                match lid with
                | [] -> ()
                | [ single ] -> considerIdent single
                | head :: _ ->
                    let last = List.last lid
                    considerRange (Range.unionRanges head.idRange last.idRange)
                    List.iter considerIdent lid

            let rec walkAttributes (attrs: SynAttributes) =
                for attrList in attrs do
                    considerRange attrList.Range

                    for attr in attrList.Attributes do
                        considerRange attr.Range
                        considerLongIdent attr.TypeName.LongIdent
                        walkExpr attr.ArgExpr

            and walkType (t: SynType) =
                considerRange t.Range

                match t with
                | SynType.WithNull(elem, _, _, _)
                | SynType.Array(_, elem, _)
                | SynType.HashConstraint(elem, _)
                | SynType.MeasurePower(elem, _, _)
                | SynType.Paren(elem, _) -> walkType elem
                | SynType.SignatureParameter(attributes = attrs; id = idOpt; usedType = elem) ->
                    walkAttributes attrs
                    Option.iter considerIdent idOpt
                    walkType elem
                | SynType.Fun(argTy, retTy, _, _)
                | SynType.Or(argTy, retTy, _, _) ->
                    walkType argTy
                    walkType retTy
                | SynType.App(typeName, _, typeArgs, _, _, _, _) ->
                    walkType typeName
                    List.iter walkType typeArgs
                | SynType.LongIdentApp(typeName, SynLongIdent(id = lid), _, typeArgs, _, _, _) ->
                    walkType typeName
                    considerLongIdent lid
                    List.iter walkType typeArgs
                | SynType.LongIdent(SynLongIdent(id = lid)) -> considerLongIdent lid
                | SynType.Var(SynTypar(id, _, _), _) -> considerIdent id
                | SynType.Tuple(_, path, _) ->
                    for seg in path do
                        match seg with
                        | SynTupleTypeSegment.Type elem -> walkType elem
                        | _ -> ()
                | SynType.AnonRecd(_, fields, _) ->
                    for id, fieldTy in fields do
                        considerIdent id
                        walkType fieldTy
                | SynType.WithGlobalConstraints(inner, constraints, _) ->
                    walkType inner
                    List.iter walkTypeConstraint constraints
                | SynType.Intersection(typarOpt, types, _, _) ->
                    typarOpt |> Option.iter (fun (SynTypar(id, _, _)) -> considerIdent id)
                    List.iter walkType types
                | SynType.StaticConstantExpr(expr, _) -> walkExpr expr
                | SynType.StaticConstantNamed(ident, value, _) ->
                    walkType ident
                    walkType value
                | SynType.Anon _
                | SynType.StaticConstant _
                | SynType.StaticConstantNull _
                | SynType.FromParseError _ -> ()

            and walkTypeConstraint (c: SynTypeConstraint) =
                considerRange c.Range

                match c with
                | SynTypeConstraint.WhereTyparDefaultsToType(SynTypar(id, _, _), ty, _)
                | SynTypeConstraint.WhereTyparSubtypeOfType(SynTypar(id, _, _), ty, _) ->
                    considerIdent id
                    walkType ty
                | SynTypeConstraint.WhereSelfConstrained(ty, _) -> walkType ty
                | SynTypeConstraint.WhereTyparIsEnum(SynTypar(id, _, _), tys, _)
                | SynTypeConstraint.WhereTyparIsDelegate(SynTypar(id, _, _), tys, _) ->
                    considerIdent id
                    List.iter walkType tys
                | SynTypeConstraint.WhereTyparSupportsMember(t, memberSig, _) ->
                    walkType t
                    walkMemberSig memberSig
                | SynTypeConstraint.WhereTyparIsValueType(SynTypar(id, _, _), _)
                | SynTypeConstraint.WhereTyparIsReferenceType(SynTypar(id, _, _), _)
                | SynTypeConstraint.WhereTyparIsUnmanaged(SynTypar(id, _, _), _)
                | SynTypeConstraint.WhereTyparSupportsNull(SynTypar(id, _, _), _)
                | SynTypeConstraint.WhereTyparNotSupportsNull(SynTypar(id, _, _), _, _)
                | SynTypeConstraint.WhereTyparIsComparable(SynTypar(id, _, _), _)
                | SynTypeConstraint.WhereTyparIsEquatable(SynTypar(id, _, _), _) -> considerIdent id

            and walkTyparDecls (typars: SynTyparDecls) =
                considerRange typars.Range

                for SynTyparDecl(attributes = attrs; typar = SynTypar(id, _, _); intersectionConstraints = ts) in typars.TyparDecls do
                    considerIdent id
                    walkAttributes attrs
                    List.iter walkType ts

                List.iter walkTypeConstraint typars.Constraints

            and walkValTyparDecls (SynValTyparDecls(typars, _)) = Option.iter walkTyparDecls typars

            and walkPat (pat: SynPat) =
                considerRange pat.Range

                match pat with
                | SynPat.Tuple(elementPats = pats)
                | SynPat.ArrayOrList(elementPats = pats)
                | SynPat.Ands(pats = pats) -> List.iter walkPat pats
                | SynPat.Typed(inner, targetType, _) ->
                    walkPat inner
                    walkType targetType
                | SynPat.Attrib(inner, attrs, _) ->
                    walkPat inner
                    walkAttributes attrs
                | SynPat.Or(lhs, rhs, _, _)
                | SynPat.As(lhs, rhs, _)
                | SynPat.ListCons(lhs, rhs, _, _) ->
                    walkPat lhs
                    walkPat rhs
                | SynPat.LongIdent(longDotId = SynLongIdent(id = lid); typarDecls = typars; argPats = argPats) ->
                    considerLongIdent lid
                    Option.iter walkValTyparDecls typars

                    match argPats with
                    | SynArgPats.Pats ps -> List.iter walkPat ps
                    | SynArgPats.NamePatPairs(pats = pairs; range = r) ->
                        considerRange r

                        for NamePatPairField(fieldName = SynLongIdent(id = flid); pat = p; range = fr) in pairs do
                            considerRange fr
                            considerLongIdent flid
                            walkPat p
                | SynPat.Paren(inner, _) -> walkPat inner
                | SynPat.IsInst(ty, _) -> walkType ty
                | SynPat.QuoteExpr(e, _) -> walkExpr e
                | SynPat.Record(fieldPats = fields) ->
                    for NamePatPairField(fieldName = SynLongIdent(id = flid); pat = p; range = fr) in fields do
                        considerRange fr
                        considerLongIdent flid
                        walkPat p
                | SynPat.Named(SynIdent(id, _), _, _, _)
                | SynPat.OptionalVal(id, _) -> considerIdent id
                | SynPat.InstanceMember _ -> ()
                | SynPat.Const _
                | SynPat.Wild _
                | SynPat.Null _
                | SynPat.FromParseError _ -> ()

            and walkSimplePat (sp: SynSimplePat) =
                considerRange sp.Range

                match sp with
                | SynSimplePat.Id(ident = id) -> considerIdent id
                | SynSimplePat.Typed(inner, ty, _) ->
                    walkSimplePat inner
                    walkType ty
                | SynSimplePat.Attrib(inner, attrs, _) ->
                    walkSimplePat inner
                    walkAttributes attrs

            and walkSimplePats (sps: SynSimplePats) =
                considerRange sps.Range

                match sps with
                | SynSimplePats.SimplePats(pats = pats) -> List.iter walkSimplePat pats

            and walkBinding (SynBinding(attributes = attrs; headPat = headPat; returnInfo = retInfo; expr = body) as b) =
                considerRange b.RangeOfBindingWithRhs
                considerRange b.RangeOfBindingWithoutRhs
                considerRange b.RangeOfHeadPattern
                walkAttributes attrs
                walkPat headPat

                match retInfo with
                | Some(SynBindingReturnInfo(typeName = ty; range = r; attributes = retAttrs)) ->
                    considerRange r
                    walkType ty
                    walkAttributes retAttrs
                | None -> ()

                walkExpr body

            and walkClause (SynMatchClause(pat = pat; whenExpr = whenExpr; resultExpr = resultExpr; range = r) as clause) =
                considerRange r
                considerRange clause.RangeOfGuardAndRhs
                walkPat pat
                Option.iter walkExpr whenExpr
                walkExpr resultExpr

            and walkInterfaceImpl (SynInterfaceImpl(interfaceTy = ty; bindings = bindings; members = members; range = r)) =
                considerRange r
                walkType ty
                List.iter walkBinding bindings
                List.iter walkMemberDefn members

            and walkExpr (expr: SynExpr) =
                considerRange expr.Range

                match expr with
                | SynExpr.Paren(expr = e)
                | SynExpr.InferredUpcast(expr = e)
                | SynExpr.InferredDowncast(expr = e)
                | SynExpr.AddressOf(expr = e)
                | SynExpr.DoBang(expr = e)
                | SynExpr.YieldOrReturn(expr = e)
                | SynExpr.ArrayOrListComputed(expr = e)
                | SynExpr.ComputationExpr(expr = e)
                | SynExpr.Do(expr = e)
                | SynExpr.Assert(expr = e)
                | SynExpr.Lazy(expr = e)
                | SynExpr.YieldOrReturnFrom(expr = e)
                | SynExpr.DotLambda(expr = e)
                | SynExpr.IndexFromEnd(e, _)
                | SynExpr.DebugPoint(innerExpr = e)
                | SynExpr.Fixed(expr = e) -> walkExpr e
                | SynExpr.Typed(e, ty, _)
                | SynExpr.New(_, ty, e, _)
                | SynExpr.TypeTest(e, ty, _)
                | SynExpr.Upcast(e, ty, _)
                | SynExpr.Downcast(e, ty, _) ->
                    walkExpr e
                    walkType ty
                | SynExpr.Quote(op, _, quoted, _, _) ->
                    walkExpr op
                    walkExpr quoted
                | SynExpr.Sequential(expr1 = e1; expr2 = e2) ->
                    let stack = Stack<SynExpr>()
                    stack.Push e2
                    stack.Push e1

                    while stack.Count > 0 do
                        let cur = stack.Pop()
                        considerRange cur.Range

                        match cur with
                        | SynExpr.Sequential(expr1 = a; expr2 = b) ->
                            stack.Push b
                            stack.Push a
                        | other -> walkExpr other
                | SynExpr.SequentialOrImplicitYield(_, e1, e2, ifNotE, _) ->
                    walkExpr e1
                    walkExpr e2
                    walkExpr ifNotE
                | SynExpr.Lambda(args = args; body = body) ->
                    walkSimplePats args
                    walkExpr body
                | SynExpr.Tuple(_, es, _, _)
                | SynExpr.ArrayOrList(_, es, _) -> List.iter walkExpr es
                | SynExpr.App(_, _, e1, e2, _)
                | SynExpr.TryFinally(tryExpr = e1; finallyExpr = e2)
                | SynExpr.While(whileExpr = e1; doExpr = e2)
                | SynExpr.WhileBang(whileExpr = e1; doExpr = e2)
                | SynExpr.Dynamic(funcExpr = e1; argExpr = e2)
                | SynExpr.Set(e1, e2, _) ->
                    walkExpr e1
                    walkExpr e2
                | SynExpr.JoinIn(e1, _, e2, _) ->
                    walkExpr e1
                    walkExpr e2
                | SynExpr.NamedIndexedPropertySet(SynLongIdent(id = lid), e1, e2, _) ->
                    considerLongIdent lid
                    walkExpr e1
                    walkExpr e2
                | SynExpr.Record(copyInfo = copyOpt; recordFields = fields) ->
                    copyOpt |> Option.iter (fun (e, _) -> walkExpr e)

                    for item in fields do
                        match item with
                        | SynExprRecordFieldOrSpread.Field(SynExprRecordField(fieldName = (SynLongIdent(id = lid), _); expr = eOpt), _) ->
                            considerLongIdent lid
                            Option.iter walkExpr eOpt
                        | SynExprRecordFieldOrSpread.Spread(SynExprSpread(expr = e; range = r), _) ->
                            considerRange r
                            walkExpr e
                | SynExpr.AnonRecd(copyInfo = copyOpt; recordFields = fields) ->
                    copyOpt |> Option.iter (fun (e, _) -> walkExpr e)

                    for item in fields do
                        match item with
                        | SynExprAnonRecordFieldOrSpread.Field(SynExprAnonRecordField(fieldName = SynLongIdent(id = lid); expr = e; range = r), _) ->
                            considerRange r
                            considerLongIdent lid
                            walkExpr e
                        | SynExprAnonRecordFieldOrSpread.Spread(SynExprSpread(expr = e; range = r), _) ->
                            considerRange r
                            walkExpr e
                | SynExpr.ObjExpr(objType = ty; argOptions = argOpt; bindings = bindings; members = members; extraImpls = ifaces) ->
                    walkType ty
                    argOpt |> Option.iter (fun (e, idOpt) -> walkExpr e; Option.iter considerIdent idOpt)
                    List.iter walkBinding bindings
                    List.iter walkMemberDefn members
                    List.iter walkInterfaceImpl ifaces
                | SynExpr.For(ident = id; identBody = e1; toBody = e2; doBody = e3) ->
                    considerIdent id
                    walkExpr e1
                    walkExpr e2
                    walkExpr e3
                | SynExpr.ForEach(pat = pat; enumExpr = e1; bodyExpr = e2) ->
                    walkPat pat
                    walkExpr e1
                    walkExpr e2
                | SynExpr.MatchLambda(_, _, clauses, _, _) -> List.iter walkClause clauses
                | SynExpr.Match(expr = e; clauses = clauses)
                | SynExpr.MatchBang(expr = e; clauses = clauses)
                | SynExpr.TryWith(tryExpr = e; withCases = clauses) ->
                    walkExpr e
                    List.iter walkClause clauses
                | SynExpr.TypeApp(e, _, tys, _, _, _, _) ->
                    walkExpr e
                    List.iter walkType tys
                | SynExpr.LetOrUse letOrUse ->
                    considerRange letOrUse.Range
                    List.iter walkBinding letOrUse.Bindings
                    walkExpr letOrUse.Body
                | SynExpr.IfThenElse(ifExpr = e1; thenExpr = e2; elseExpr = e3) ->
                    walkExpr e1
                    walkExpr e2
                    Option.iter walkExpr e3
                | SynExpr.LongIdentSet(SynLongIdent(id = lid), e, _) ->
                    considerLongIdent lid
                    walkExpr e
                | SynExpr.DotGet(e, _, SynLongIdent(id = lid), _) ->
                    walkExpr e
                    considerLongIdent lid
                | SynExpr.DotSet(e1, SynLongIdent(id = lid), e2, _) ->
                    walkExpr e1
                    considerLongIdent lid
                    walkExpr e2
                | SynExpr.DotIndexedGet(e1, e2, _, _) ->
                    walkExpr e1
                    walkExpr e2
                | SynExpr.DotIndexedSet(e1, e2, e3, _, _, _) ->
                    walkExpr e1
                    walkExpr e2
                    walkExpr e3
                | SynExpr.DotNamedIndexedPropertySet(e1, SynLongIdent(id = lid), e2, e3, _) ->
                    walkExpr e1
                    considerLongIdent lid
                    walkExpr e2
                    walkExpr e3
                | SynExpr.TraitCall(ty, memberSig, e, _) ->
                    walkType ty
                    walkMemberSig memberSig
                    walkExpr e
                | SynExpr.InterpolatedString(contents = parts) ->
                    for part in parts do
                        match part with
                        | SynInterpolatedStringPart.FillExpr(e, _) -> walkExpr e
                        | SynInterpolatedStringPart.String _ -> ()
                | SynExpr.IndexRange(e1, _, e2, _, _, _) ->
                    Option.iter walkExpr e1
                    Option.iter walkExpr e2
                | SynExpr.Ident id -> considerIdent id
                | SynExpr.LongIdent(_, SynLongIdent(id = lid), _, _) -> considerLongIdent lid
                | SynExpr.Typar(SynTypar(id, _, _), _) -> considerIdent id
                | SynExpr.Const _
                | SynExpr.Null _
                | SynExpr.ImplicitZero _
                | SynExpr.LibraryOnlyILAssembly _
                | SynExpr.LibraryOnlyStaticOptimization _
                | SynExpr.LibraryOnlyUnionCaseFieldGet _
                | SynExpr.LibraryOnlyUnionCaseFieldSet _
                | SynExpr.ArbitraryAfterError _
                | SynExpr.FromParseError _
                | SynExpr.DiscardAfterMissingQualificationAfterDot _ -> ()

            and walkField (SynField(attributes = attrs; idOpt = idOpt; fieldType = ty; range = r)) =
                considerRange r
                walkAttributes attrs
                Option.iter considerIdent idOpt
                walkType ty

            and walkValSig (SynValSig(attributes = attrs; ident = SynIdent(id, _); explicitTypeParams = typars; synType = ty; synExpr = exprOpt; range = r)) =
                considerRange r
                walkAttributes attrs
                considerIdent id
                walkValTyparDecls typars
                walkType ty
                Option.iter walkExpr exprOpt

            and walkUnionCase (SynUnionCase(attributes = attrs; ident = SynIdent(id, _); caseType = caseKind; range = r)) =
                considerRange r
                walkAttributes attrs
                considerIdent id

                match caseKind with
                | SynUnionCaseKind.Fields fields -> List.iter walkField fields
                | SynUnionCaseKind.FullType(ty, _) -> walkType ty

            and walkEnumCase (SynEnumCase(attributes = attrs; ident = SynIdent(id, _); valueExpr = ve; range = r)) =
                considerRange r
                walkAttributes attrs
                considerIdent id
                walkExpr ve

            and walkComponentInfo (SynComponentInfo(attributes = attrs; typeParams = typars; constraints = constraints; range = r) as info) =
                considerRange r
                walkAttributes attrs
                considerLongIdent info.LongIdent
                Option.iter walkTyparDecls typars
                List.iter walkTypeConstraint constraints

            and walkTypeDefnSimple (repr: SynTypeDefnSimpleRepr) =
                considerRange repr.Range

                match repr with
                | SynTypeDefnSimpleRepr.Enum(cases, _) -> List.iter walkEnumCase cases
                | SynTypeDefnSimpleRepr.Union(_, cases, _) -> List.iter walkUnionCase cases
                | SynTypeDefnSimpleRepr.Record(_, fields, _) ->
                    for item in fields do
                        match item with
                        | SynFieldOrSpread.Field f -> walkField f
                        | SynFieldOrSpread.Spread(SynTypeSpread(ty = ty; range = sr)) ->
                            considerRange sr
                            walkType ty
                | SynTypeDefnSimpleRepr.TypeAbbrev(_, ty, _) -> walkType ty
                | SynTypeDefnSimpleRepr.Exception(SynExceptionDefnRepr(attributes = attrs; caseName = uc; range = er)) ->
                    considerRange er
                    walkAttributes attrs
                    walkUnionCase uc
                | SynTypeDefnSimpleRepr.General _
                | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _
                | SynTypeDefnSimpleRepr.None _ -> ()

            and walkMemberSig (ms: SynMemberSig) =
                considerRange ms.Range

                match ms with
                | SynMemberSig.Inherit(ty, _)
                | SynMemberSig.Interface(ty, _) -> walkType ty
                | SynMemberSig.Member(memberSig = vs) -> walkValSig vs
                | SynMemberSig.ValField(field = f) -> walkField f
                | SynMemberSig.NestedType(nestedType = SynTypeDefnSig(typeInfo = info; typeRepr = repr; members = memberSigs; range = tr)) ->
                    considerRange tr
                    walkComponentInfo info
                    walkTypeDefnSigRepr repr
                    List.iter walkMemberSig memberSigs

            and walkTypeDefnSigRepr (repr: SynTypeDefnSigRepr) =
                considerRange repr.Range

                match repr with
                | SynTypeDefnSigRepr.ObjectModel(_, memberSigs, _) -> List.iter walkMemberSig memberSigs
                | SynTypeDefnSigRepr.Simple(simple, _) -> walkTypeDefnSimple simple
                | SynTypeDefnSigRepr.Exception(SynExceptionDefnRepr(attributes = attrs; caseName = uc; range = er)) ->
                    considerRange er
                    walkAttributes attrs
                    walkUnionCase uc

            and walkMemberDefn (md: SynMemberDefn) =
                considerRange md.Range

                match md with
                | SynMemberDefn.AbstractSlot(slotSig = vs) -> walkValSig vs
                | SynMemberDefn.Member(memberDefn = b) -> walkBinding b
                | SynMemberDefn.GetSetMember(memberDefnForGet = g; memberDefnForSet = s) ->
                    Option.iter walkBinding g
                    Option.iter walkBinding s
                | SynMemberDefn.ImplicitCtor(attributes = attrs; ctorArgs = ctorPat; selfIdentifier = selfOpt) ->
                    walkAttributes attrs
                    walkPat ctorPat
                    Option.iter considerIdent selfOpt
                | SynMemberDefn.ImplicitInherit(inheritType = ty; inheritArgs = args; inheritAlias = aliasOpt) ->
                    walkType ty
                    walkExpr args
                    Option.iter considerIdent aliasOpt
                | SynMemberDefn.LetBindings(bindings = bs) -> List.iter walkBinding bs
                | SynMemberDefn.Interface(interfaceType = ty; members = ms) ->
                    walkType ty
                    Option.iter (List.iter walkMemberDefn) ms
                | SynMemberDefn.Inherit(baseType = tyOpt) -> Option.iter walkType tyOpt
                | SynMemberDefn.ValField(fieldInfo = f) -> walkField f
                | SynMemberDefn.NestedType(typeDefn = td) -> walkTypeDefn td
                | SynMemberDefn.AutoProperty(attributes = attrs; ident = id; typeOpt = tyOpt; synExpr = e) ->
                    walkAttributes attrs
                    considerIdent id
                    Option.iter walkType tyOpt
                    walkExpr e
                | SynMemberDefn.Open(target, _) ->
                    match target with
                    | SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(id = lid), r) ->
                        considerRange r
                        considerLongIdent lid
                    | SynOpenDeclTarget.Type(ty, r) ->
                        considerRange r
                        walkType ty

            and walkTypeDefn (SynTypeDefn(typeInfo = info; typeRepr = repr; members = members; implicitConstructor = ctorOpt; range = r)) =
                considerRange r
                walkComponentInfo info
                considerRange repr.Range

                match repr with
                | SynTypeDefnRepr.ObjectModel(_, defns, _) -> List.iter walkMemberDefn defns
                | SynTypeDefnRepr.Simple(simple, _) -> walkTypeDefnSimple simple
                | SynTypeDefnRepr.Exception(SynExceptionDefnRepr(attributes = attrs; caseName = uc; range = er)) ->
                    considerRange er
                    walkAttributes attrs
                    walkUnionCase uc

                Option.iter walkMemberDefn ctorOpt
                List.iter walkMemberDefn members

            and walkModuleDecl (decl: SynModuleDecl) =
                considerRange decl.Range

                match decl with
                | SynModuleDecl.NamespaceFragment fragment -> walkModuleOrNamespace fragment
                | SynModuleDecl.NestedModule(moduleInfo = info; decls = decls) ->
                    walkComponentInfo info
                    List.iter walkModuleDecl decls
                | SynModuleDecl.Let(bindings = bs) -> List.iter walkBinding bs
                | SynModuleDecl.Expr(expr, _) -> walkExpr expr
                | SynModuleDecl.Types(typeDefns, _) -> List.iter walkTypeDefn typeDefns
                | SynModuleDecl.Attributes(attributes = attrs) -> walkAttributes attrs
                | SynModuleDecl.Exception(SynExceptionDefn(exnRepr = SynExceptionDefnRepr(attributes = attrs; caseName = uc; range = er); members = ms), _) ->
                    considerRange er
                    walkAttributes attrs
                    walkUnionCase uc
                    List.iter walkMemberDefn ms
                | SynModuleDecl.ModuleAbbrev(id, lid, _) ->
                    considerIdent id
                    considerLongIdent lid
                | SynModuleDecl.Open(target, _) ->
                    match target with
                    | SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(id = lid), r) ->
                        considerRange r
                        considerLongIdent lid
                    | SynOpenDeclTarget.Type(ty, r) ->
                        considerRange r
                        walkType ty
                | SynModuleDecl.HashDirective _ -> ()

            and walkModuleOrNamespace (SynModuleOrNamespace(longId = lid; decls = decls; attribs = attrs; range = r)) =
                considerRange r
                considerLongIdent lid
                walkAttributes attrs
                List.iter walkModuleDecl decls

            and walkModuleSigDecl (decl: SynModuleSigDecl) =
                considerRange decl.Range

                match decl with
                | SynModuleSigDecl.NamespaceFragment fragment -> walkModuleOrNamespaceSig fragment
                | SynModuleSigDecl.NestedModule(moduleInfo = info; moduleDecls = decls) ->
                    walkComponentInfo info
                    List.iter walkModuleSigDecl decls
                | SynModuleSigDecl.Val(valSig, _) -> walkValSig valSig
                | SynModuleSigDecl.Types(types, _) ->
                    for SynTypeDefnSig(typeInfo = info; typeRepr = repr; members = memberSigs; range = tr) in types do
                        considerRange tr
                        walkComponentInfo info
                        walkTypeDefnSigRepr repr
                        List.iter walkMemberSig memberSigs
                | SynModuleSigDecl.Exception(SynExceptionSig(exnRepr = SynExceptionDefnRepr(attributes = attrs; caseName = uc; range = er); members = ms), _) ->
                    considerRange er
                    walkAttributes attrs
                    walkUnionCase uc
                    List.iter walkMemberSig ms
                | SynModuleSigDecl.ModuleAbbrev(id, lid, _) ->
                    considerIdent id
                    considerLongIdent lid
                | SynModuleSigDecl.Open(target, _) ->
                    match target with
                    | SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(id = lid), r) ->
                        considerRange r
                        considerLongIdent lid
                    | SynOpenDeclTarget.Type(ty, r) ->
                        considerRange r
                        walkType ty
                | SynModuleSigDecl.HashDirective _ -> ()

            and walkModuleOrNamespaceSig (SynModuleOrNamespaceSig(longId = lid; decls = decls; attribs = attrs; range = r)) =
                considerRange r
                considerLongIdent lid
                walkAttributes attrs
                List.iter walkModuleSigDecl decls

            considerRange parseTree.Range

            match parseTree with
            | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
                List.iter walkModuleOrNamespace modules
            | ParsedInput.SigFile(ParsedSigFileInput(contents = sigModules)) ->
                List.iter walkModuleOrNamespaceSig sigModules

            Array.init count (fun i ->
                let candidates = buckets[i].ToArray()

                if candidates.Length = 0 then
                    let p = positions[i]
                    { Range = { Start = p; End = p }; Parent = None }
                else
                    Array.sortInPlaceWith compareOutermostFirst candidates
                    let mutable current: SelectionRange option = None

                    for r in candidates do
                        match current with
                        | None -> current <- Some { Range = r; Parent = None }
                        | Some prev ->
                            if prev.Range <> r && rangeContains prev.Range r then
                                current <- Some { Range = r; Parent = Some prev }

                    current.Value))

/// Pure-syntax nested selection ranges for `.fs` and `.fsi` via `ProjectGraph.LatestParse` (never waits on typechecking).
let atPositions (graph: ProjectGraph) (path: string) (positions: LspPos[]) (ct: CancellationToken) : Task<SelectionRange[] option> =
    task {
        let! textView = graph.LatestParse(path, ct)
        return Some(compute textView.Text textView.Parse.ParseTree positions)
    }
