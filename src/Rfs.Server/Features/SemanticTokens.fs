module Rfs.Server.SemanticTokens

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

/// Exact 26-entry token type legend matching FSAC's `ClassificationUtils.SemanticTokenTypes` (indices 0..25).
let private tokenTypes =
    [| "namespace"
       "type"
       "class"
       "enum"
       "interface"
       "struct"
       "typeParameter"
       "parameter"
       "variable"
       "property"
       "enumMember"
       "event"
       "function"
       "method"
       "macro"
       "keyword"
       "modifier"
       "comment"
       "string"
       "number"
       "regexp"
       "operator"
       "member"
       "cexpr"
       "text"
       "module" |]

/// Exact 13-entry token modifier legend matching FSAC's `ClassificationUtils.SemanticTokenModifier` (bits 0..12).
let private tokenModifiers =
    [| "declaration"
       "definition"
       "readonly"
       "static"
       "deprecated"
       "abstract"
       "async"
       "modification"
       "documentation"
       "defaultLibrary"
       "mutable"
       "disposable"
       "extension" |]

let legend: SemanticTokensLegend =
    { TokenTypes = tokenTypes
      TokenModifiers = tokenModifiers }

let private modReadonly = 1u <<< 2
let private modDefaultLibrary = 1u <<< 9
let private modMutable = 1u <<< 10
let private modDisposable = 1u <<< 11
let private modExtension = 1u <<< 12

let private mapClassification (t: SemanticClassificationType) : struct (uint32 * uint32) =
    match t with
    | SemanticClassificationType.Operator -> struct (21u, 0u)
    | SemanticClassificationType.ReferenceType
    | SemanticClassificationType.ConstructorForReferenceType -> struct (2u, 0u)
    | SemanticClassificationType.Type
    | SemanticClassificationType.TypeDef -> struct (1u, 0u)
    | SemanticClassificationType.ValueType
    | SemanticClassificationType.ConstructorForValueType -> struct (5u, 0u)
    | SemanticClassificationType.UnionCase -> struct (10u, 0u)
    | SemanticClassificationType.UnionCaseField -> struct (7u, 0u)
    | SemanticClassificationType.Function
    | SemanticClassificationType.IntrinsicFunction -> struct (12u, 0u)
    | SemanticClassificationType.Method -> struct (13u, 0u)
    | SemanticClassificationType.ExtensionMethod -> struct (13u, modExtension)
    | SemanticClassificationType.Property -> struct (9u, 0u)
    | SemanticClassificationType.MutableVar -> struct (8u, modMutable)
    | SemanticClassificationType.MutableRecordField -> struct (9u, modMutable)
    | SemanticClassificationType.Module -> struct (25u, 0u)
    | SemanticClassificationType.Namespace -> struct (0u, 0u)
    | SemanticClassificationType.Printf -> struct (20u, 0u)
    | SemanticClassificationType.ComputationExpression -> struct (23u, 0u)
    | SemanticClassificationType.Enumeration -> struct (3u, 0u)
    | SemanticClassificationType.Interface -> struct (4u, 0u)
    | SemanticClassificationType.TypeArgument -> struct (6u, 0u)
    | SemanticClassificationType.DisposableTopLevelValue
    | SemanticClassificationType.DisposableLocalValue -> struct (8u, modDisposable)
    | SemanticClassificationType.DisposableType -> struct (1u, modDisposable)
    | SemanticClassificationType.Literal -> struct (8u, modReadonly ||| modDefaultLibrary)
    | SemanticClassificationType.RecordField
    | SemanticClassificationType.RecordFieldAsFunction -> struct (9u, modReadonly)
    | SemanticClassificationType.Exception
    | SemanticClassificationType.Field
    | SemanticClassificationType.Event
    | SemanticClassificationType.Delegate
    | SemanticClassificationType.NamedArgument -> struct (22u, 0u)
    | SemanticClassificationType.Value
    | SemanticClassificationType.LocalValue -> struct (8u, 0u)
    | SemanticClassificationType.Plaintext -> struct (24u, 0u)
    | _ -> struct (24u, 0u)

let private onLargeStack (f: unit -> 'T) : 'T =
    let result = ref ValueNone
    let failure = ref null

    let thread =
        Thread(
            (fun () ->
                try
                    result.Value <- ValueSome(f ())
                with ex ->
                    failure.Value <- ExceptionDispatchInfo.Capture ex),
            GraphHost.StackSize,
            IsBackground = true
        )

    thread.Start()
    thread.Join()

    if not (isNull failure.Value) then
        failure.Value.Throw()

    result.Value.Value

/// Collects plain-data 1-based `(StartLine, StartColumn, EndLine, EndColumn)` ranges for:
/// 1) function and lambda parameter identifier declarations (`LocalValue` -> `Parameter`), and
/// 2) `SynType.WithNull` (`| null`) `null` keyword suffixes (`Keyword`), across both `.fs` and `.fsi` ASTs.
let private collectAstSupplements (input: ParsedInput) : HashSet<struct (int * int * int * int)> * (struct (int * int * int * int))[] =
    let paramRanges = HashSet<struct (int * int * int * int)>()
    let nullRanges = ResizeArray<struct (int * int * int * int)>()

    let addRange (target: HashSet<struct (int * int * int * int)>) (r: range) =
        target.Add(struct (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)) |> ignore

    let rec collectParamPat (pat: SynPat) =
        match pat with
        | SynPat.Named(ident = SynIdent(id, _)) -> addRange paramRanges id.idRange
        | SynPat.Typed(pat = inner)
        | SynPat.Paren(pat = inner)
        | SynPat.Attrib(pat = inner) -> collectParamPat inner
        | SynPat.Tuple(elementPats = pats) -> List.iter collectParamPat pats
        | SynPat.OptionalVal(ident = id) -> addRange paramRanges id.idRange
        | SynPat.As(rhsPat = rhs) -> collectParamPat rhs
        | _ -> ()

    let walkAttributes (walkExpr: SynExpr -> unit) (attrs: SynAttributes) =
        for attrList in attrs do
            for attr in attrList.Attributes do
                walkExpr attr.ArgExpr

    let rec walkType (t: SynType) =
        match t with
        | SynType.WithNull(inner, _, fullRange, _) ->
            if fullRange.EndColumn >= 4 then
                nullRanges.Add(struct (fullRange.EndLine, fullRange.EndColumn - 4, fullRange.EndLine, fullRange.EndColumn))
            walkType inner
        | SynType.Array(_, elem, _)
        | SynType.HashConstraint(elem, _)
        | SynType.MeasurePower(elem, _, _)
        | SynType.Paren(elem, _)
        | SynType.SignatureParameter(usedType = elem) -> walkType elem
        | SynType.Fun(argTy, retTy, _, _)
        | SynType.Or(argTy, retTy, _, _) ->
            walkType argTy
            walkType retTy
        | SynType.App(typeName, _, typeArgs, _, _, _, _) ->
            walkType typeName
            List.iter walkType typeArgs
        | SynType.LongIdentApp(_, _, _, typeArgs, _, _, _) ->
            List.iter walkType typeArgs
        | SynType.Tuple(_, path, _) ->
            for seg in path do
                match seg with
                | SynTupleTypeSegment.Type elem -> walkType elem
                | _ -> ()
        | SynType.AnonRecd(_, fields, _) ->
            for _, fieldTy in fields do
                walkType fieldTy
        | SynType.WithGlobalConstraints(inner, constraints, _) ->
            walkType inner
            List.iter walkTypeConstraint constraints
        | SynType.Intersection(_, types, _, _) ->
            List.iter walkType types
        | SynType.StaticConstantExpr(expr, _) -> walkExpr expr
        | SynType.StaticConstantNamed(ident, value, _) ->
            walkType ident
            walkType value
        | SynType.LongIdent _
        | SynType.Var _
        | SynType.Anon _
        | SynType.StaticConstant _
        | SynType.StaticConstantNull _
        | SynType.FromParseError _ -> ()

    and walkTypeConstraint (c: SynTypeConstraint) =
        match c with
        | SynTypeConstraint.WhereTyparDefaultsToType(_, ty, _)
        | SynTypeConstraint.WhereTyparSubtypeOfType(_, ty, _)
        | SynTypeConstraint.WhereSelfConstrained(ty, _) -> walkType ty
        | SynTypeConstraint.WhereTyparIsEnum(_, tys, _)
        | SynTypeConstraint.WhereTyparIsDelegate(_, tys, _) -> List.iter walkType tys
        | SynTypeConstraint.WhereTyparSupportsMember(t, memberSig, _) ->
            walkType t
            walkMemberSig memberSig
        | SynTypeConstraint.WhereTyparIsValueType _
        | SynTypeConstraint.WhereTyparIsReferenceType _
        | SynTypeConstraint.WhereTyparIsUnmanaged _
        | SynTypeConstraint.WhereTyparSupportsNull _
        | SynTypeConstraint.WhereTyparNotSupportsNull _
        | SynTypeConstraint.WhereTyparIsComparable _
        | SynTypeConstraint.WhereTyparIsEquatable _ -> ()

    and walkTyparDecls (typars: SynTyparDecls) =
        for SynTyparDecl(attributes = attrs; intersectionConstraints = ts) in typars.TyparDecls do
            walkAttributes walkExpr attrs
            List.iter walkType ts
        List.iter walkTypeConstraint typars.Constraints

    and walkValTyparDecls (SynValTyparDecls(typars, _)) =
        Option.iter walkTyparDecls typars

    and walkPat (pat: SynPat) =
        match pat with
        | SynPat.Tuple(elementPats = pats)
        | SynPat.ArrayOrList(elementPats = pats)
        | SynPat.Ands(pats = pats) -> List.iter walkPat pats
        | SynPat.Typed(inner, targetType, _) ->
            walkPat inner
            walkType targetType
        | SynPat.Attrib(inner, attrs, _) ->
            walkPat inner
            walkAttributes walkExpr attrs
        | SynPat.Or(lhs, rhs, _, _)
        | SynPat.As(lhs, rhs, _)
        | SynPat.ListCons(lhs, rhs, _, _) ->
            walkPat lhs
            walkPat rhs
        | SynPat.LongIdent(typarDecls = typars; argPats = argPats) ->
            Option.iter walkValTyparDecls typars
            match argPats with
            | SynArgPats.Pats ps -> List.iter walkPat ps
            | SynArgPats.NamePatPairs(pats = pairs) ->
                for NamePatPairField(pat = p) in pairs do
                    walkPat p
        | SynPat.Paren(inner, _) -> walkPat inner
        | SynPat.IsInst(ty, _) -> walkType ty
        | SynPat.QuoteExpr(e, _) -> walkExpr e
        | SynPat.Record(fieldPats = fields) ->
            for NamePatPairField(pat = p) in fields do
                walkPat p
        | SynPat.Named _
        | SynPat.Const _
        | SynPat.Wild _
        | SynPat.Null _
        | SynPat.OptionalVal _
        | SynPat.InstanceMember _
        | SynPat.FromParseError _ -> ()

    and walkSimplePat (sp: SynSimplePat) =
        match sp with
        | SynSimplePat.Id(ident = id; isCompilerGenerated = false; isThisVal = false) ->
            addRange paramRanges id.idRange
        | SynSimplePat.Id _ -> ()
        | SynSimplePat.Typed(inner, ty, _) ->
            walkSimplePat inner
            walkType ty
        | SynSimplePat.Attrib(inner, attrs, _) ->
            walkSimplePat inner
            walkAttributes walkExpr attrs

    and walkSimplePats (sps: SynSimplePats) =
        match sps with
        | SynSimplePats.SimplePats(pats = pats) -> List.iter walkSimplePat pats

    and walkBinding (SynBinding(attributes = attrs; headPat = headPat; returnInfo = retInfo; expr = body)) =
        match headPat with
        | SynPat.LongIdent(argPats = argPats) ->
            match argPats with
            | SynArgPats.Pats ps -> List.iter collectParamPat ps
            | SynArgPats.NamePatPairs(pats = pairs) ->
                for NamePatPairField(pat = p) in pairs do
                    collectParamPat p
        | _ -> ()

        walkAttributes walkExpr attrs
        walkPat headPat
        match retInfo with
        | Some(SynBindingReturnInfo(typeName = ty; attributes = retAttrs)) ->
            walkType ty
            walkAttributes walkExpr retAttrs
        | None -> ()
        walkExpr body

    and walkClause (SynMatchClause(pat = pat; whenExpr = whenExpr; resultExpr = resultExpr)) =
        walkPat pat
        Option.iter walkExpr whenExpr
        walkExpr resultExpr

    and walkInterfaceImpl (SynInterfaceImpl(interfaceTy = ty; bindings = bindings; members = members)) =
        walkType ty
        List.iter walkBinding bindings
        List.iter walkMemberDefn members

    and walkExpr (expr: SynExpr) =
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
            // Iterative walk over sequential chains to avoid deep recursion
            let stack = Stack<SynExpr>()
            stack.Push e2
            stack.Push e1
            while stack.Count > 0 do
                match stack.Pop() with
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
        | SynExpr.JoinIn(e1, _, e2, _)
        | SynExpr.Dynamic(funcExpr = e1; argExpr = e2)
        | SynExpr.NamedIndexedPropertySet(_, e1, e2, _)
        | SynExpr.Set(e1, e2, _) ->
            walkExpr e1
            walkExpr e2
        | SynExpr.Record(copyInfo = copyOpt; recordFields = fields) ->
            copyOpt |> Option.iter (fun (e, _) -> walkExpr e)
            for item in fields do
                match item with
                | SynExprRecordFieldOrSpread.Field(SynExprRecordField(expr = eOpt), _) -> Option.iter walkExpr eOpt
                | SynExprRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) -> walkExpr e
        | SynExpr.AnonRecd(copyInfo = copyOpt; recordFields = fields) ->
            copyOpt |> Option.iter (fun (e, _) -> walkExpr e)
            for item in fields do
                match item with
                | SynExprAnonRecordFieldOrSpread.Field(SynExprAnonRecordField(expr = e), _) -> walkExpr e
                | SynExprAnonRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) -> walkExpr e
        | SynExpr.ObjExpr(objType = ty; argOptions = argOpt; bindings = bindings; members = members; extraImpls = ifaces) ->
            walkType ty
            argOpt |> Option.iter (fun (e, _) -> walkExpr e)
            List.iter walkBinding bindings
            List.iter walkMemberDefn members
            List.iter walkInterfaceImpl ifaces
        | SynExpr.For(identBody = e1; toBody = e2; doBody = e3) ->
            walkExpr e1
            walkExpr e2
            walkExpr e3
        | SynExpr.ForEach(pat = pat; enumExpr = e1; bodyExpr = e2) ->
            walkPat pat
            walkExpr e1
            walkExpr e2
        | SynExpr.MatchLambda(_, _, clauses, _, _) ->
            List.iter walkClause clauses
        | SynExpr.Match(expr = e; clauses = clauses)
        | SynExpr.MatchBang(expr = e; clauses = clauses)
        | SynExpr.TryWith(tryExpr = e; withCases = clauses) ->
            walkExpr e
            List.iter walkClause clauses
        | SynExpr.TypeApp(e, _, tys, _, _, _, _) ->
            walkExpr e
            List.iter walkType tys
        | SynExpr.LetOrUse letOrUse ->
            List.iter walkBinding letOrUse.Bindings
            walkExpr letOrUse.Body
        | SynExpr.IfThenElse(ifExpr = e1; thenExpr = e2; elseExpr = e3) ->
            walkExpr e1
            walkExpr e2
            Option.iter walkExpr e3
        | SynExpr.LongIdentSet(_, e, _)
        | SynExpr.DotGet(e, _, _, _) -> walkExpr e
        | SynExpr.DotSet(e1, _, e2, _)
        | SynExpr.DotIndexedGet(e1, e2, _, _) ->
            walkExpr e1
            walkExpr e2
        | SynExpr.DotIndexedSet(e1, e2, e3, _, _, _)
        | SynExpr.DotNamedIndexedPropertySet(e1, _, e2, e3, _) ->
            walkExpr e1
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
        | SynExpr.Const _
        | SynExpr.Ident _
        | SynExpr.LongIdent _
        | SynExpr.Null _
        | SynExpr.ImplicitZero _
        | SynExpr.Typar _
        | SynExpr.LibraryOnlyILAssembly _
        | SynExpr.LibraryOnlyStaticOptimization _
        | SynExpr.LibraryOnlyUnionCaseFieldGet _
        | SynExpr.LibraryOnlyUnionCaseFieldSet _
        | SynExpr.ArbitraryAfterError _
        | SynExpr.FromParseError _
        | SynExpr.DiscardAfterMissingQualificationAfterDot _ -> ()

    and walkField (SynField(attributes = attrs; fieldType = ty)) =
        walkAttributes walkExpr attrs
        walkType ty

    and walkValSig (SynValSig(attributes = attrs; synType = ty; synExpr = exprOpt)) =
        walkAttributes walkExpr attrs
        walkType ty
        Option.iter walkExpr exprOpt

    and walkUnionCase (SynUnionCase(attributes = attrs; caseType = caseKind)) =
        walkAttributes walkExpr attrs
        match caseKind with
        | SynUnionCaseKind.Fields fields -> List.iter walkField fields
        | SynUnionCaseKind.FullType(ty, _) -> walkType ty

    and walkEnumCase (SynEnumCase(attributes = attrs)) =
        walkAttributes walkExpr attrs

    and walkComponentInfo (SynComponentInfo(attributes = attrs; typeParams = typars; constraints = constraints)) =
        walkAttributes walkExpr attrs
        Option.iter walkTyparDecls typars
        List.iter walkTypeConstraint constraints

    and walkTypeDefnSimple (repr: SynTypeDefnSimpleRepr) =
        match repr with
        | SynTypeDefnSimpleRepr.Enum(cases, _) -> List.iter walkEnumCase cases
        | SynTypeDefnSimpleRepr.Union(_, cases, _) -> List.iter walkUnionCase cases
        | SynTypeDefnSimpleRepr.Record(_, fields, _) ->
            for item in fields do
                match item with
                | SynFieldOrSpread.Field f -> walkField f
                | SynFieldOrSpread.Spread(SynTypeSpread(ty = ty)) -> walkType ty
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, ty, _) -> walkType ty
        | SynTypeDefnSimpleRepr.Exception(SynExceptionDefnRepr(caseName = uc)) -> walkUnionCase uc
        | SynTypeDefnSimpleRepr.General _
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _
        | SynTypeDefnSimpleRepr.None _ -> ()

    and walkMemberSig (ms: SynMemberSig) =
        match ms with
        | SynMemberSig.Inherit(ty, _)
        | SynMemberSig.Interface(ty, _) -> walkType ty
        | SynMemberSig.Member(memberSig = vs) -> walkValSig vs
        | SynMemberSig.ValField(field = f) -> walkField f
        | SynMemberSig.NestedType(nestedType = SynTypeDefnSig(typeInfo = info; typeRepr = repr; members = memberSigs)) ->
            walkComponentInfo info
            walkTypeDefnSigRepr repr
            List.iter walkMemberSig memberSigs

    and walkTypeDefnSigRepr (repr: SynTypeDefnSigRepr) =
        match repr with
        | SynTypeDefnSigRepr.ObjectModel(_, memberSigs, _) -> List.iter walkMemberSig memberSigs
        | SynTypeDefnSigRepr.Simple(simple, _) -> walkTypeDefnSimple simple
        | SynTypeDefnSigRepr.Exception(SynExceptionDefnRepr(caseName = uc)) -> walkUnionCase uc

    and walkMemberDefn (md: SynMemberDefn) =
        match md with
        | SynMemberDefn.AbstractSlot(slotSig = vs) -> walkValSig vs
        | SynMemberDefn.Member(memberDefn = b) -> walkBinding b
        | SynMemberDefn.GetSetMember(memberDefnForGet = g; memberDefnForSet = s) ->
            Option.iter walkBinding g
            Option.iter walkBinding s
        | SynMemberDefn.ImplicitCtor(attributes = attrs; ctorArgs = ctorPat) ->
            walkAttributes walkExpr attrs
            walkPat ctorPat
        | SynMemberDefn.ImplicitInherit(inheritType = ty; inheritArgs = args) ->
            walkType ty
            walkExpr args
        | SynMemberDefn.LetBindings(bindings = bs) -> List.iter walkBinding bs
        | SynMemberDefn.Interface(interfaceType = ty; members = ms) ->
            walkType ty
            Option.iter (List.iter walkMemberDefn) ms
        | SynMemberDefn.Inherit(baseType = tyOpt) -> Option.iter walkType tyOpt
        | SynMemberDefn.ValField(fieldInfo = f) -> walkField f
        | SynMemberDefn.NestedType(typeDefn = td) -> walkTypeDefn td
        | SynMemberDefn.AutoProperty(attributes = attrs; typeOpt = tyOpt; synExpr = e) ->
            walkAttributes walkExpr attrs
            Option.iter walkType tyOpt
            walkExpr e
        | SynMemberDefn.Open _ -> ()

    and walkTypeDefn (SynTypeDefn(typeInfo = info; typeRepr = repr; members = members; implicitConstructor = ctorOpt)) =
        walkComponentInfo info
        match repr with
        | SynTypeDefnRepr.ObjectModel(_, defns, _) -> List.iter walkMemberDefn defns
        | SynTypeDefnRepr.Simple(simple, _) -> walkTypeDefnSimple simple
        | SynTypeDefnRepr.Exception(SynExceptionDefnRepr(caseName = uc)) -> walkUnionCase uc
        Option.iter walkMemberDefn ctorOpt
        List.iter walkMemberDefn members

    and walkModuleDecl (decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.NamespaceFragment fragment -> walkModuleOrNamespace fragment
        | SynModuleDecl.NestedModule(moduleInfo = info; decls = decls) ->
            walkComponentInfo info
            List.iter walkModuleDecl decls
        | SynModuleDecl.Let(bindings = bs) -> List.iter walkBinding bs
        | SynModuleDecl.Expr(expr, _) -> walkExpr expr
        | SynModuleDecl.Types(typeDefns, _) -> List.iter walkTypeDefn typeDefns
        | SynModuleDecl.Attributes(attributes = attrs) -> walkAttributes walkExpr attrs
        | SynModuleDecl.Exception(SynExceptionDefn(exnRepr = SynExceptionDefnRepr(caseName = uc); members = ms), _) ->
            walkUnionCase uc
            List.iter walkMemberDefn ms
        | SynModuleDecl.ModuleAbbrev _
        | SynModuleDecl.Open _
        | SynModuleDecl.HashDirective _ -> ()

    and walkModuleOrNamespace (SynModuleOrNamespace(decls = decls; attribs = attrs)) =
        walkAttributes walkExpr attrs
        List.iter walkModuleDecl decls

    and walkModuleSigDecl (decl: SynModuleSigDecl) =
        match decl with
        | SynModuleSigDecl.NamespaceFragment fragment -> walkModuleOrNamespaceSig fragment
        | SynModuleSigDecl.NestedModule(moduleInfo = info; moduleDecls = decls) ->
            walkComponentInfo info
            List.iter walkModuleSigDecl decls
        | SynModuleSigDecl.Val(valSig, _) -> walkValSig valSig
        | SynModuleSigDecl.Types(types, _) ->
            for SynTypeDefnSig(typeInfo = info; typeRepr = repr; members = memberSigs) in types do
                walkComponentInfo info
                walkTypeDefnSigRepr repr
                List.iter walkMemberSig memberSigs
        | SynModuleSigDecl.Exception(SynExceptionSig(exnRepr = SynExceptionDefnRepr(caseName = uc); members = ms), _) ->
            walkUnionCase uc
            List.iter walkMemberSig ms
        | SynModuleSigDecl.ModuleAbbrev _
        | SynModuleSigDecl.Open _
        | SynModuleSigDecl.HashDirective _ -> ()

    and walkModuleOrNamespaceSig (SynModuleOrNamespaceSig(decls = decls; attribs = attrs)) =
        walkAttributes walkExpr attrs
        List.iter walkModuleSigDecl decls

    match input with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        List.iter walkModuleOrNamespace modules
    | ParsedInput.SigFile(ParsedSigFileInput(contents = sigModules)) ->
        List.iter walkModuleOrNamespaceSig sigModules

    paramRanges, nullRanges.ToArray()

/// Plain-data caches keyed on compiler result identity (`FileResult` or `FileSignature`) and `FSharpParseFileResults`.
let private rawCache = ConditionalWeakTable<obj, SemanticClassificationItem[]>()
let private astCache = ConditionalWeakTable<FSharpParseFileResults, HashSet<struct (int * int * int * int)> * (struct (int * int * int * int))[]>()

let private getAstSupplements (parse: FSharpParseFileResults) =
    match astCache.TryGetValue parse with
    | true, cached -> cached
    | false, _ ->
        let computed = onLargeStack (fun () -> collectAstSupplements parse.ParseTree)
        astCache.GetValue(parse, ConditionalWeakTable.CreateValueCallback(fun _ -> computed))

type private SingleLineSeg = (struct (int * int * int * uint32 * uint32))

/// Pure pipeline: maps FCS classifications, overrides parameter definitions, appends `SynType.WithNull` keywords,
/// splits multiline tokens using actual line lengths, scrubs parent ranges around nested child ranges per line,
/// filters to `lspRange` if requested, and encodes into LSP relative 5-uint32 tuples.
let internal encodeClassified
    (text: string)
    (paramRanges: HashSet<struct (int * int * int * int)>)
    (nullRanges: (struct (int * int * int * int))[])
    (lspRange: Range option)
    (items: SemanticClassificationItem[])
    : uint32[] =
    let rawLines = text.Split('\n')
    let lineCount = rawLines.Length

    if lineCount = 0 then
        [||]
    else
        let lineLens =
            Array.init lineCount (fun i ->
                let s = rawLines[i]
                if s.Length > 0 && s[s.Length - 1] = '\r' then s.Length - 1 else s.Length)

        let minLine0, maxLine0 =
            match lspRange with
            | Some r -> max 0 (int r.Start.Line), min (lineCount - 1) (int r.End.Line)
            | None -> 0, lineCount - 1

        if minLine0 > maxLine0 then
            [||]
        else
            let singleSegs = ResizeArray<SingleLineSeg>(items.Length + nullRanges.Length)

            let addSpan (sl0: int) (sc: int) (el0: int) (ec: int) (ty: uint32) (mods: uint32) =
                if el0 >= minLine0 && sl0 <= maxLine0 && sl0 <= el0 then
                    let startLine = max minLine0 (max 0 sl0)
                    let endLine = min maxLine0 (min (lineCount - 1) el0)

                    if startLine <= endLine then
                        if sl0 = el0 then
                            let lineLen = lineLens[startLine]
                            let c0 = max 0 sc
                            let c1 = min lineLen ec
                            if c1 > c0 then
                                singleSegs.Add(struct (startLine, c0, c1, ty, mods))
                        else
                            for l in startLine .. endLine do
                                let lineLen = lineLens[l]
                                let c0 = if l = sl0 then max 0 sc else 0
                                let c1 = if l = el0 then min lineLen ec else lineLen
                                if c1 > c0 then
                                    singleSegs.Add(struct (l, c0, c1, ty, mods))

            for i in 0 .. items.Length - 1 do
                let item = items[i]
                let r = item.Range
                let key = struct (r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

                let struct (ty, mods) =
                    if item.Type = SemanticClassificationType.LocalValue && paramRanges.Contains key then
                        struct (7u, 0u)
                    else
                        mapClassification item.Type

                addSpan (r.StartLine - 1) r.StartColumn (r.EndLine - 1) r.EndColumn ty mods

            for i in 0 .. nullRanges.Length - 1 do
                let struct (sl, sc, el, ec) = nullRanges[i]
                addSpan (sl - 1) sc (el - 1) ec 15u 0u

            if singleSegs.Count = 0 then
                [||]
            else
                let segArr = singleSegs.ToArray()

                let priority (ty: uint32) =
                    // Prefer specific overrides (`parameter` = 7, `keyword` = 15, `regexp` = 20) on exact range ties
                    if ty = 7u || ty = 15u || ty = 20u then 0 else 1

                Array.sortInPlaceWith
                    (fun (struct (l1: int, s1: int, e1: int, ty1: uint32, m1: uint32)) (struct (l2: int, s2: int, e2: int, ty2: uint32, m2: uint32)) ->
                        let c = l1.CompareTo l2
                        if c <> 0 then c
                        else
                            let c2 = s1.CompareTo s2
                            if c2 <> 0 then c2
                            else
                                let c3 = e1.CompareTo e2
                                if c3 <> 0 then c3
                                else
                                    let c4 = (priority ty1).CompareTo(priority ty2)
                                    if c4 <> 0 then c4
                                    else
                                        let c5 = ty1.CompareTo ty2
                                        if c5 <> 0 then c5 else m1.CompareTo m2)
                    segArr

                // Deduplicate identical `(line, startCol, endCol)` spans before parent scrubbing so identical ranges don't scrub or duplicate
                let uniqueSegs = ResizeArray<SingleLineSeg>(segArr.Length)
                uniqueSegs.Add segArr[0]

                for i in 1 .. segArr.Length - 1 do
                    let struct (lPrev, sPrev, ePrev, _, _) = uniqueSegs[uniqueSegs.Count - 1]
                    let struct (lCur, sCur, eCur, _, _) as cur = segArr[i]
                    if not (lCur = lPrev && sCur = sPrev && eCur = ePrev) then
                        uniqueSegs.Add cur

                // Scrub parent ranges per line around strictly contained child ranges
                let scrubbed = ResizeArray<SingleLineSeg>(uniqueSegs.Count)
                let childrenBuf = ResizeArray<struct (int * int)>()
                let mutable idx = 0

                while idx < uniqueSegs.Count do
                    let struct (line0, _, _, _, _) = uniqueSegs[idx]
                    let lineStart = idx
                    while idx < uniqueSegs.Count && (let struct (l, _, _, _, _) = uniqueSegs[idx] in l = line0) do
                        idx <- idx + 1
                    let lineEnd = idx - 1

                    let lineOut = ResizeArray<SingleLineSeg>(lineEnd - lineStart + 1)

                    for pIdx in lineStart .. lineEnd do
                        let struct (_, pStart, pEnd, pTy, pMods) as p = uniqueSegs[pIdx]
                        childrenBuf.Clear()

                        for cIdx in lineStart .. lineEnd do
                            if cIdx <> pIdx then
                                let struct (_, cStart, cEnd, _, _) = uniqueSegs[cIdx]
                                if cStart >= pStart && cEnd <= pEnd && (cStart > pStart || cEnd < pEnd) then
                                    childrenBuf.Add(struct (cStart, cEnd))

                        if childrenBuf.Count = 0 then
                            lineOut.Add p
                        else
                            let childArr = childrenBuf.ToArray()
                            Array.sortInPlaceWith
                                (fun (struct (s1: int, e1: int)) (struct (s2: int, e2: int)) ->
                                    let c = s1.CompareTo s2
                                    if c <> 0 then c else e1.CompareTo e2)
                                childArr

                            let mutable cursor = pStart

                            for struct (cStart, cEnd) in childArr do
                                if cStart > cursor then
                                    lineOut.Add(struct (line0, cursor, cStart, pTy, pMods))
                                if cEnd > cursor then
                                    cursor <- cEnd

                            if cursor < pEnd then
                                lineOut.Add(struct (line0, cursor, pEnd, pTy, pMods))

                    let lineArr = lineOut.ToArray()
                    Array.sortInPlaceWith
                        (fun (struct (_: int, s1: int, e1: int, _: uint32, _: uint32)) (struct (_: int, s2: int, e2: int, _: uint32, _: uint32)) ->
                            let c = s1.CompareTo s2
                            if c <> 0 then c else e1.CompareTo e2)
                        lineArr

                    let mutable lastEnd = -1
                    for k in 0 .. lineArr.Length - 1 do
                        let struct (l, s, e, ty, mods) as tok = lineArr[k]
                        let inRange =
                            match lspRange with
                            | None -> true
                            | Some r ->
                                let rsl, rsc = int r.Start.Line, int r.Start.Character
                                let rel, rec' = int r.End.Line, int r.End.Character
                                not ((l = rsl && e <= rsc) || (l = rel && s >= rec'))

                        if inRange && s >= lastEnd then
                            scrubbed.Add tok
                            lastEnd <- e

                let finalCount = scrubbed.Count
                let encoded = Array.zeroCreate<uint32> (finalCount * 5)
                let mutable prevLine = 0
                let mutable prevChar = 0

                for i in 0 .. finalCount - 1 do
                    let struct (line0, startCol, endCol, ty, mods) = scrubbed[i]
                    let dLine = line0 - prevLine
                    let dStart = if dLine > 0 then startCol else startCol - prevChar
                    prevLine <- line0
                    prevChar <- startCol

                    let baseIdx = i * 5
                    encoded[baseIdx] <- uint32 dLine
                    encoded[baseIdx + 1] <- uint32 dStart
                    encoded[baseIdx + 2] <- uint32 (endCol - startCol)
                    encoded[baseIdx + 3] <- ty
                    encoded[baseIdx + 4] <- mods

                encoded

let private readClassified
    (graph: ProjectGraph)
    (path: string)
    (ct: CancellationToken)
    : Task<struct (string * HashSet<struct (int * int * int * int)> * (struct (int * int * int * int))[] * SemanticClassificationItem[])> =
    if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
        task {
            let! struct (textView, sigResult, _) = graph.ReadSignature(path, ct)
            let paramRanges, nullRanges = getAstSupplements textView.Parse

            let items =
                match rawCache.TryGetValue(box sigResult) with
                | true, cached -> cached
                | false, _ ->
                    let computed = sigResult.SemanticClassification
                    rawCache.GetValue(box sigResult, ConditionalWeakTable.CreateValueCallback(fun _ -> computed))

            return struct (textView.Text, paramRanges, nullRanges, items)
        }
    else
        let deadline = Stopwatch.StartNew()

        let rec loop () =
            task {
                try
                    return!
                        graph.ReadFile(
                            path,
                            (fun tv -> getAstSupplements tv.Parse),
                            (fun view (paramRanges, nullRanges) ->
                                let items =
                                    match rawCache.TryGetValue(box view.Result) with
                                    | true, cached -> cached
                                    | false, _ ->
                                        let computed = view.Result.GetSemanticClassification()
                                        rawCache.GetValue(box view.Result, ConditionalWeakTable.CreateValueCallback(fun _ -> computed))

                                struct (view.Text, paramRanges, nullRanges, items)),
                            ct
                        )
                with :? FSharp.Compiler.Declarations.SupersededLineDirectivesException when deadline.ElapsedMilliseconds < 250L ->
                    do! Task.Delay(10, ct)
                    return! loop ()
            }

        loop ()

/// Thread-safe per-server cache of the latest encoded semantic token array and `resultId` per open file.
type TokenDeltaCache() =
    let entries = ConcurrentDictionary<string, struct (string * uint32[])>(StringComparer.OrdinalIgnoreCase)
    let mutable nextId = 0L

    let normalize (path: string) = Path.GetFullPath path

    let freshId () =
        string (Interlocked.Increment &nextId)

    let arraysEqual (a: uint32[]) (b: uint32[]) =
        a.AsSpan().SequenceEqual(ReadOnlySpan<uint32>(b))

    let computeEdit (prev: uint32[]) (next: uint32[]) : SemanticTokensEdit =
        let minLen = min prev.Length next.Length
        let mutable prefixLen = 0

        while prefixLen < minLen && prev[prefixLen] = next[prefixLen] do
            prefixLen <- prefixLen + 1

        let maxSuffix = minLen - prefixLen
        let mutable suffixLen = 0

        while suffixLen < maxSuffix && prev[prev.Length - 1 - suffixLen] = next[next.Length - 1 - suffixLen] do
            suffixLen <- suffixLen + 1

        let deleteCount = prev.Length - prefixLen - suffixLen
        let insertLen = next.Length - prefixLen - suffixLen
        let insertData = if insertLen = 0 then None else Some(Array.sub next prefixLen insertLen)

        { Start = uint32 prefixLen
          DeleteCount = uint32 deleteCount
          Data = insertData }

    member _.StoreFull(path: string, data: uint32[]) : SemanticTokens =
        let key = normalize path
        let id = freshId ()
        entries[key] <- struct (id, data)
        { ResultId = Some id; Data = data }

    member _.ComputeDeltaOrFull(path: string, previousResultId: string, newData: uint32[]) : U2<SemanticTokens, SemanticTokensDelta> =
        let key = normalize path
        let id = freshId ()

        match entries.TryGetValue key with
        | true, struct (prevId, prevData) when prevId = previousResultId ->
            entries[key] <- struct (id, newData)

            if arraysEqual prevData newData then
                U2.C2 { ResultId = Some id; Edits = [||] }
            else
                let edit = computeEdit prevData newData
                U2.C2 { ResultId = Some id; Edits = [| edit |] }
        | _ ->
            entries[key] <- struct (id, newData)
            U2.C1 { ResultId = Some id; Data = newData }

    member _.Remove(path: string) : unit =
        entries.TryRemove(normalize path) |> ignore

let full (cache: TokenDeltaCache) (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task<SemanticTokens option> =
    task {
        let! struct (text, paramRanges, nullRanges, items) = readClassified graph path ct
        let data = encodeClassified text paramRanges nullRanges None items
        return Some(cache.StoreFull(path, data))
    }

let fullDelta
    (cache: TokenDeltaCache)
    (graph: ProjectGraph)
    (path: string)
    (previousResultId: string)
    (ct: CancellationToken)
    : Task<U2<SemanticTokens, SemanticTokensDelta> option> =
    task {
        let! struct (text, paramRanges, nullRanges, items) = readClassified graph path ct
        let data = encodeClassified text paramRanges nullRanges None items
        return Some(cache.ComputeDeltaOrFull(path, previousResultId, data))
    }

let range (graph: ProjectGraph) (path: string) (lspRange: Range) (ct: CancellationToken) : Task<SemanticTokens option> =
    task {
        let! struct (text, paramRanges, nullRanges, items) = readClassified graph path ct
        let data = encodeClassified text paramRanges nullRanges (Some lspRange) items
        return Some { ResultId = None; Data = data }
    }
