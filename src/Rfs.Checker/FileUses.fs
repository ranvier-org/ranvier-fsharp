namespace Rfs.Checker

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text

module internal ReadSupport =
    /// Takes the lock exclusively. A grant that arrives after `ct` ended the wait is released at once.
    let acquireExclusive (l: AsyncReaderWriterLock) (ct: CancellationToken) : Task<IDisposable> =
        task {
            let grant = l.WriteAsync()

            try
                return! grant.WaitAsync ct
            with :? OperationCanceledException ->
                grant.ContinueWith(
                    (fun (t: Task<IDisposable>) -> if t.IsCompletedSuccessfully then t.Result.Dispose()),
                    TaskScheduler.Default
                )
                |> ignore

                return raise (OperationCanceledException ct)
        }

/// What a file says of one symbol key.
[<NoComparison; NoEquality>]
type KeyUses =
    { /// Each use's range, with whether it is a definition
      Hits: (struct (range * bool))[]
      /// Where each definition here that is the only one of its key says it is. The location can be stale: it tells
      /// definitions apart and names a signature
      Definitions: range[] }

/// One definition in a file by its raw key (DeclarationChecker.SymbolKey), with what DeclarationChecker's location lookups
/// read of its symbol.
[<Struct; NoComparison; NoEquality>]
type DefinitionSite =
    { /// The raw key, which only a signature's lookup reads (the exact match among what conformed); null in an
      /// implementation's table, whose entries are by the key already
      Key: string
      /// The symbol's signature location
      Location: range voption
      IsGetter: bool
      IsSetter: bool }

/// DeclarationChecker's location lookups over a file's definitions as plain data: DefinitionLocation's for an
/// implementation, SignatureLocation's for a signature. The key handling follows the fork's SymbolKeys (service_decl.fs).
module internal DefinitionSites =
    let private setterSuffix = "|setter"

    let withoutSetter (key: string) =
        if key.EndsWith setterSuffix then key.Substring(0, key.Length - setterSuffix.Length) else key

    /// What a signature's definition shares with an implementation that conformed to it: a member's types can differ
    let conformance (key: string) =
        let key = withoutSetter key

        match key.Split '|' with
        | [| assembly; kind; parent; apparent; name; isInstance; _typars; role; types |] ->
            let arguments = if types = "" then 0 else types.Split(';').Length
            String.concat "|" [ assembly; kind; parent; apparent; name; isInstance; role; string arguments ]
        | _ -> key

    /// A property's getter's location first, or its setter's for the setter's key; two other locations answer nothing
    let locationOf (key: string) (sites: DefinitionSite seq) : range option =
        let wanted (s: DefinitionSite) = if key.EndsWith setterSuffix then s.IsSetter else s.IsGetter

        let locations =
            sites
            |> Seq.sortBy (fun s -> if wanted s then 0 else 1)
            |> Seq.choose (fun s -> match s.Location with ValueSome r -> Some r | ValueNone -> None)
            |> Seq.distinct
            |> Seq.toList

        match locations with
        | [ only ] -> Some only
        | first :: _ when key.Split('|')[1] = "property" -> Some first
        | _ -> None

    let siteOf (key: string) (symbol: FSharpSymbol) : DefinitionSite =
        let getter, setter =
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as v -> v.IsPropertyGetterMethod, v.IsPropertySetterMethod
            | _ -> false, false

        { Key = key
          Location =
            match symbol.SignatureLocation with
            | Some r -> ValueSome r
            | None -> ValueNone
          IsGetter = getter
          IsSetter = setter }

    /// The sites by `by` of each one's key, keeping the key in each site only when `keepKey`
    let group (by: string -> string) (keepKey: bool) (sites: DefinitionSite seq) : Dictionary<string, DefinitionSite[]> =
        let d = Dictionary<string, DefinitionSite[]>()

        for key, g in sites |> Seq.groupBy (fun s -> by s.Key) do
            d[key] <- g |> Seq.map (fun s -> if keepKey then s else { s with Key = null }) |> Array.ofSeq

        d

/// Plain-data glyph classification for declarations indexed in `FileUses` (free of `FSharp.Compiler.*` types).
type SymbolGlyph =
    | Class = 0
    | Struct = 1
    | Interface = 2
    | Enum = 3
    | Delegate = 4
    | Exception = 5
    | Union = 6
    | Type = 7
    | Module = 8
    | Function = 9
    | Method = 10
    | Constructor = 11
    | OverridenMethod = 12
    | ExtensionMethod = 13
    | Property = 14
    | Event = 15
    | Field = 16

[<Struct; NoComparison; NoEquality>]
type CallableDecl =
    { Key: string
      Name: string
      Glyph: SymbolGlyph
      Range: range
      SelectionRange: range }

[<Struct; NoComparison; NoEquality>]
type TypeRelationEntry =
    { Key: string
      Name: string
      Glyph: SymbolGlyph
      Range: range
      SelectionRange: range
      SuperKeys: string[] }

/// <summary>
/// Every symbol use of one check of a file, by key, and its definitions by raw key. Immutable and free of typed-tree
/// objects, so it needs no lock and keeps no check alive.
/// </summary>
[<Sealed>]
type FileUses
    internal
    (
        source: WeakReference,
        byKey: Dictionary<string, KeyUses>,
        definitions: Dictionary<string, DefinitionSite[]>,
        conforming: Dictionary<string, DefinitionSite[]>,
        implConforming: Dictionary<string, DefinitionSite[]>,
        implementations: Dictionary<string, range[]>,
        typesByKey: Dictionary<string, TypeRelationEntry>,
        subtypesBySuper: Dictionary<string, TypeRelationEntry[]>,
        callables: CallableDecl[]
    ) =
    static let nothing = { Hits = [||]; Definitions = [||] }

    let sitesIn (d: Dictionary<string, DefinitionSite[]>) (key: string) =
        match d.TryGetValue key with
        | true, sites -> sites
        | _ -> [||]

    /// The result or signature this was made from, while something else holds it. Weak: holding it would keep the check
    member internal _.Source = source

    /// What the file says of exactly this key
    member _.Of(key: string) : KeyUses =
        match byKey.TryGetValue key with
        | true, uses -> uses
        | _ -> nothing

    /// Concrete member or type implementation ranges in this file for an interface entity key or abstract slot key.
    member _.ImplementationsOf(targetKey: string) : range[] =
        match implementations.TryGetValue(DefinitionSites.withoutSetter targetKey) with
        | true, ranges -> ranges
        | _ -> [||]

    /// Declared types in this file whose immediate base class or interfaces include `superKey`.
    member _.SubtypesOf(superKey: string) : TypeRelationEntry[] =
        match subtypesBySuper.TryGetValue superKey with
        | true, entries -> entries
        | _ -> [||]

    /// Declared type entry in this file by its SymbolKey, including its immediate SuperKeys.
    member _.TypeOf(key: string) : TypeRelationEntry voption =
        match typesByKey.TryGetValue key with
        | true, entry -> ValueSome entry
        | _ -> ValueNone

    /// Innermost callable declaration whose range encloses `pos`.
    member _.EnclosingCallable(pos: pos) : CallableDecl voption =
        let mutable found = ValueNone
        let mutable i = 0

        while found.IsNone && i < callables.Length do
            let c = callables[i]

            if Position.posGeq pos c.Range.Start && Position.posGeq c.Range.End pos then
                found <- ValueSome c

            i <- i + 1

        found

    /// How many keys it holds
    member _.Keys = byKey.Count

    /// An implementation file's answer to DeclarationChecker.DefinitionLocation for the raw key, from the check indexed
    member _.DefinitionOf(key: string) : range option =
        DefinitionSites.locationOf key (sitesIn definitions (DefinitionSites.withoutSetter key))

    /// A signature file's answer to DeclarationChecker.SignatureLocation for the raw key, from the check indexed
    member _.SignatureDefinitionOf(key: string, conformed: bool) : range option =
        let defined = sitesIn conforming (DefinitionSites.conformance key)
        let exact = defined |> Array.filter (fun s -> DefinitionSites.withoutSetter s.Key = DefinitionSites.withoutSetter key)

        if exact.Length = 0 && conformed then
            DefinitionSites.locationOf key defined
        else
            DefinitionSites.locationOf key exact

    /// An implementation file's own declaration site in `.fs` for a raw key (or conformed `.fsi` signature key), ignoring `.fsi` redirection
    member _.ImplementationDefinitionOf(key: string, conformed: bool) : range option =
        let defined = sitesIn implConforming (DefinitionSites.conformance key)
        let exact = defined |> Array.filter (fun s -> DefinitionSites.withoutSetter s.Key = DefinitionSites.withoutSetter key)

        if exact.Length = 0 && conformed then
            DefinitionSites.locationOf key defined
        else
            DefinitionSites.locationOf key exact

    member this.ImplementationDefinitionOf(key: string) : range option =
        this.ImplementationDefinitionOf(key, true)

    /// An estimate of the bytes it holds (entries, key strings, arrays, ranges), for measuring: the uses by key, then the
    /// definitions by raw key
    member internal _.Footprint: struct (int64 * int64) =
        // A range is two int64s; arrays hold structs inline
        let rangeBytes = 16L
        let text (s: string) = 22L + 2L * int64 s.Length
        let entry = 24L
        let mutable keyed = 0L

        for KeyValue(k, u) in byKey do
            keyed <- keyed + entry + text k + 32L + int64 u.Hits.Length * (rangeBytes + 8L) + 24L + int64 u.Definitions.Length * rangeBytes + 24L

        for KeyValue(k, impls) in implementations do
            keyed <- keyed + entry + text k + 24L + int64 impls.Length * rangeBytes

        for KeyValue(k, t) in typesByKey do
            keyed <-
                keyed
                + entry
                + text k
                + (if isNull t.Key then 0L else text t.Key)
                + (if isNull t.Name then 0L else text t.Name)
                + 4L
                + 2L * rangeBytes
                + 24L
                + (t.SuperKeys |> Array.sumBy (fun s -> 8L + (if isNull s then 0L else text s)))

        let subtypeSlotBytes = 8L + 8L + 4L + 2L * rangeBytes + 8L

        for KeyValue(k, subs) in subtypesBySuper do
            keyed <- keyed + entry + text k + 24L + int64 subs.Length * subtypeSlotBytes

        let callableSlotBytes = 8L + 8L + 4L + 2L * rangeBytes
        keyed <- keyed + 24L + int64 callables.Length * callableSlotBytes

        for c in callables do
            keyed <-
                keyed
                + (if isNull c.Key then 0L else text c.Key)
                + (if isNull c.Name then 0L else text c.Name)

        let mutable raw = 0L

        for d in [ definitions; conforming; implConforming ] do
            for KeyValue(k, sites) in d do
                raw <- raw + entry + text k + 24L

                for site in sites do
                    raw <- raw + 8L + (8L + rangeBytes) + 8L + (if isNull site.Key then 0L else text site.Key)

        struct (keyed, raw)

module private AstIndex =
    [<Struct>]
    type RangeKey = RangeKey of int * int * int * int

    let inline rangeKey (r: range) : RangeKey =
        RangeKey(r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

    type Info =
        { TypeRanges: Dictionary<RangeKey, range>
          AugmentationIdentRanges: HashSet<RangeKey>
          ObjExprTypeRanges: HashSet<RangeKey>
          CallableRanges: Dictionary<RangeKey, range> }

    let rec private typeHeadIdents (ty: SynType) : Ident list =
        match ty with
        | SynType.LongIdent(SynLongIdent(lid, _, _)) -> lid
        | SynType.App(typeName = inner) -> typeHeadIdents inner
        | SynType.LongIdentApp(longDotId = SynLongIdent(lid, _, _)) -> lid
        | SynType.Paren(innerType = inner)
        | SynType.WithGlobalConstraints(typeName = inner)
        | SynType.HashConstraint(innerType = inner)
        | SynType.WithNull(innerType = inner) -> typeHeadIdents inner
        | _ -> []

    let rec private headPatIdentRanges (pat: SynPat) : range list =
        match pat with
        | SynPat.Named(ident = SynIdent(id, _)) -> [ id.idRange ]
        | SynPat.LongIdent(longDotId = SynLongIdent(lid, _, _)) ->
            match List.tryLast lid with
            | Some id -> [ id.idRange ]
            | None -> []
        | SynPat.InstanceMember(memberId = id) -> [ id.idRange ]
        | SynPat.Typed(pat = inner)
        | SynPat.Attrib(pat = inner)
        | SynPat.Paren(pat = inner)
        | SynPat.FromParseError(pat = inner) -> headPatIdentRanges inner
        | SynPat.As(lhsPat = l; rhsPat = r) -> headPatIdentRanges l @ headPatIdentRanges r
        | SynPat.Tuple(elementPats = ps)
        | SynPat.Ands(pats = ps)
        | SynPat.ArrayOrList(elementPats = ps) -> ps |> List.collect headPatIdentRanges
        | _ -> []

    let rec private hasFunctionPat (pat: SynPat) : bool =
        match pat with
        | SynPat.LongIdent(argPats = argPats) -> not argPats.Patterns.IsEmpty
        | SynPat.Typed(pat = inner)
        | SynPat.Attrib(pat = inner)
        | SynPat.Paren(pat = inner)
        | SynPat.FromParseError(pat = inner) -> hasFunctionPat inner
        | SynPat.As(lhsPat = l; rhsPat = r) -> hasFunctionPat l || hasFunctionPat r
        | _ -> false

    let rec private isLambdaExpr (e: SynExpr) : bool =
        match e with
        | SynExpr.Lambda _
        | SynExpr.MatchLambda _
        | SynExpr.DotLambda _ -> true
        | SynExpr.Typed(expr = inner)
        | SynExpr.Paren(expr = inner)
        | SynExpr.DebugPoint(innerExpr = inner) -> isLambdaExpr inner
        | _ -> false

    let build (parseTree: ParsedInput option) : Info =
        let typeRanges = Dictionary<RangeKey, range>()
        let augmentations = HashSet<RangeKey>()
        let objExprTypes = HashSet<RangeKey>()
        let callableRanges = Dictionary<RangeKey, range>()

        let recordObjType (newRange: range) (ty: SynType) =
            let hasNew = not (Range.equals newRange Range.range0)

            if hasNew then
                objExprTypes.Add(rangeKey newRange) |> ignore

            let rec loop (t: SynType) =
                objExprTypes.Add(rangeKey t.Range) |> ignore

                if hasNew then
                    objExprTypes.Add(rangeKey (Range.unionRanges newRange t.Range)) |> ignore

                match t with
                | SynType.LongIdent(SynLongIdent(lid, _, _)) ->
                    for id in lid do
                        objExprTypes.Add(rangeKey id.idRange) |> ignore

                        if hasNew then
                            objExprTypes.Add(rangeKey (Range.unionRanges newRange id.idRange)) |> ignore
                | SynType.App(typeName = inner) -> loop inner
                | SynType.LongIdentApp(typeName = inner; longDotId = SynLongIdent(lid, _, _)) ->
                    loop inner

                    for id in lid do
                        objExprTypes.Add(rangeKey id.idRange) |> ignore
                | SynType.Paren(innerType = inner)
                | SynType.WithGlobalConstraints(typeName = inner)
                | SynType.HashConstraint(innerType = inner)
                | SynType.WithNull(innerType = inner) -> loop inner
                | _ -> ()

            loop ty

        let recordTypeHeader (info: SynComponentInfo) (isAugmentation: bool) (fullRange: range) =
            let (SynComponentInfo(synType = synTyOpt; range = infoRange)) = info

            let ids =
                match synTyOpt with
                | Some ty -> typeHeadIdents ty
                | None -> []

            if isAugmentation then
                augmentations.Add(rangeKey infoRange) |> ignore

                for id in ids do
                    augmentations.Add(rangeKey id.idRange) |> ignore
            else
                match List.tryLast ids with
                | Some id -> typeRanges[rangeKey id.idRange] <- fullRange
                | None -> typeRanges[rangeKey infoRange] <- fullRange

        let rec walkExpr (expr: SynExpr) =
            match expr with
            | SynExpr.Paren(expr = e)
            | SynExpr.Quote(quotedExpr = e)
            | SynExpr.Typed(expr = e)
            | SynExpr.New(expr = e)
            | SynExpr.ArrayOrListComputed(expr = e)
            | SynExpr.ComputationExpr(expr = e)
            | SynExpr.Do(expr = e)
            | SynExpr.Assert(expr = e)
            | SynExpr.Lazy(expr = e)
            | SynExpr.TypeApp(expr = e)
            | SynExpr.LongIdentSet(expr = e)
            | SynExpr.DotGet(expr = e)
            | SynExpr.DotLambda(expr = e)
            | SynExpr.TypeTest(expr = e)
            | SynExpr.Upcast(expr = e)
            | SynExpr.Downcast(expr = e)
            | SynExpr.InferredUpcast(expr = e)
            | SynExpr.InferredDowncast(expr = e)
            | SynExpr.AddressOf(expr = e)
            | SynExpr.TraitCall(argExpr = e)
            | SynExpr.YieldOrReturn(expr = e)
            | SynExpr.YieldOrReturnFrom(expr = e)
            | SynExpr.DoBang(expr = e)
            | SynExpr.Fixed(expr = e)
            | SynExpr.FromParseError(expr = e)
            | SynExpr.DiscardAfterMissingQualificationAfterDot(expr = e)
            | SynExpr.IndexFromEnd(expr = e)
            | SynExpr.DebugPoint(innerExpr = e) -> walkExpr e
            | SynExpr.Tuple(exprs = es)
            | SynExpr.ArrayOrList(exprs = es) -> es |> List.iter walkExpr
            | SynExpr.Record(copyInfo = copyOpt; recordFields = fields) ->
                copyOpt |> Option.iter (fst >> walkExpr)

                for f in fields do
                    match f with
                    | SynExprRecordFieldOrSpread.Field(SynExprRecordField(expr = eo), _) -> eo |> Option.iter walkExpr
                    | SynExprRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) -> walkExpr e
            | SynExpr.AnonRecd(copyInfo = copyOpt; recordFields = fields) ->
                copyOpt |> Option.iter (fst >> walkExpr)

                for f in fields do
                    match f with
                    | SynExprAnonRecordFieldOrSpread.Field(SynExprAnonRecordField(expr = e), _) -> walkExpr e
                    | SynExprAnonRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) -> walkExpr e
            | SynExpr.ObjExpr(objType = objTy; argOptions = argOpts; bindings = bindings; members = members; extraImpls = extras; newExprRange = newExprRange) ->
                recordObjType newExprRange objTy

                match argOpts with
                | Some(argExpr, _) ->
                    if not (Range.equals newExprRange Range.range0) then
                        objExprTypes.Add(rangeKey (Range.unionRanges newExprRange argExpr.Range)) |> ignore

                    walkExpr argExpr
                | None -> ()

                bindings |> List.iter (walkBinding true)
                members |> List.iter walkMemberDefn

                for SynInterfaceImpl(interfaceTy = ifcTy; bindings = ibs; members = ims) in extras do
                    recordObjType Range.range0 ifcTy
                    ibs |> List.iter (walkBinding true)
                    ims |> List.iter walkMemberDefn
            | SynExpr.While(whileExpr = e1; doExpr = e2)
            | SynExpr.WhileBang(whileExpr = e1; doExpr = e2)
            | SynExpr.Sequential(expr1 = e1; expr2 = e2)
            | SynExpr.SequentialOrImplicitYield(expr1 = e1; expr2 = e2)
            | SynExpr.App(funcExpr = e1; argExpr = e2)
            | SynExpr.TryFinally(tryExpr = e1; finallyExpr = e2)
            | SynExpr.DotSet(targetExpr = e1; rhsExpr = e2)
            | SynExpr.Set(targetExpr = e1; rhsExpr = e2)
            | SynExpr.DotIndexedGet(objectExpr = e1; indexArgs = e2)
            | SynExpr.NamedIndexedPropertySet(expr1 = e1; expr2 = e2)
            | SynExpr.JoinIn(lhsExpr = e1; rhsExpr = e2)
            | SynExpr.Dynamic(funcExpr = e1; argExpr = e2)
            | SynExpr.ForEach(enumExpr = e1; bodyExpr = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.For(identBody = e1; toBody = e2; doBody = e3)
            | SynExpr.DotIndexedSet(objectExpr = e1; indexArgs = e2; valueExpr = e3)
            | SynExpr.DotNamedIndexedPropertySet(targetExpr = e1; argExpr = e2; rhsExpr = e3) ->
                walkExpr e1
                walkExpr e2
                walkExpr e3
            | SynExpr.IfThenElse(ifExpr = e1; thenExpr = e2; elseExpr = e3Opt) ->
                walkExpr e1
                walkExpr e2
                e3Opt |> Option.iter walkExpr
            | SynExpr.IndexRange(expr1 = e1Opt; expr2 = e2Opt) ->
                e1Opt |> Option.iter walkExpr
                e2Opt |> Option.iter walkExpr
            | SynExpr.Lambda(body = body) -> walkExpr body
            | SynExpr.MatchLambda(matchClauses = clauses) -> clauses |> List.iter walkClause
            | SynExpr.Match(expr = e; clauses = clauses)
            | SynExpr.MatchBang(expr = e; clauses = clauses)
            | SynExpr.TryWith(tryExpr = e; withCases = clauses) ->
                walkExpr e
                clauses |> List.iter walkClause
            | SynExpr.LetOrUse({ Bindings = bs; Body = body }) ->
                bs |> List.iter (walkBinding false)
                walkExpr body
            | SynExpr.InterpolatedString(contents = parts) ->
                for p in parts do
                    match p with
                    | SynInterpolatedStringPart.FillExpr(fillExpr = e) -> walkExpr e
                    | SynInterpolatedStringPart.String _ -> ()
            | SynExpr.LibraryOnlyStaticOptimization(expr = e1; optimizedExpr = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.LibraryOnlyUnionCaseFieldGet(expr = e) -> walkExpr e
            | SynExpr.LibraryOnlyUnionCaseFieldSet(expr = e1; rhsExpr = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.LibraryOnlyILAssembly(args = es) -> es |> List.iter walkExpr
            | _ -> ()

        and walkClause (SynMatchClause(whenExpr = wOpt; resultExpr = res)) =
            wOpt |> Option.iter walkExpr
            walkExpr res

        and walkBinding (isTopOrMember: bool) (b: SynBinding) =
            let (SynBinding(kind = kind; valData = SynValData(memberFlags = memberFlags); headPat = headPat; expr = expr)) = b

            let isCallable =
                match kind with
                | SynBindingKind.Normal -> isTopOrMember || memberFlags.IsSome || hasFunctionPat headPat || isLambdaExpr expr
                | _ -> false

            if isCallable then
                let kw = b.Trivia.LeadingKeyword.Range
                let fullRange = if Range.equals kw Range.range0 then b.RangeOfBindingWithRhs else Range.unionRanges kw b.RangeOfBindingWithRhs

                for idRange in headPatIdentRanges headPat do
                    callableRanges[rangeKey idRange] <- fullRange

            walkExpr expr

        and walkMemberDefn (m: SynMemberDefn) =
            match m with
            | SynMemberDefn.Member(memberDefn = b) -> walkBinding true b
            | SynMemberDefn.GetSetMember(memberDefnForGet = gOpt; memberDefnForSet = sOpt) ->
                gOpt |> Option.iter (walkBinding true)
                sOpt |> Option.iter (walkBinding true)
            | SynMemberDefn.LetBindings(bindings = bs) -> bs |> List.iter (walkBinding true)
            | SynMemberDefn.AutoProperty(ident = id; synExpr = expr; range = r) ->
                callableRanges[rangeKey id.idRange] <- r
                walkExpr expr
            | SynMemberDefn.AbstractSlot(slotSig = SynValSig(ident = SynIdent(id, _)); range = r) ->
                callableRanges[rangeKey id.idRange] <- r
            | SynMemberDefn.Interface(members = msOpt) -> msOpt |> Option.iter (List.iter walkMemberDefn)
            | SynMemberDefn.ImplicitInherit(inheritArgs = e) -> walkExpr e
            | SynMemberDefn.NestedType(typeDefn = td) -> walkTypeDefn td
            | _ -> ()

        and walkTypeDefn (SynTypeDefn(typeInfo = info; typeRepr = repr; members = members; implicitConstructor = implicitCtor; range = fullRange)) =
            let isAugmentation =
                match repr with
                | SynTypeDefnRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation _) -> true
                | _ -> false

            recordTypeHeader info isAugmentation fullRange

            match implicitCtor, info with
            | Some(SynMemberDefn.ImplicitCtor(range = ctorRange)), SynComponentInfo(synType = Some ty) when not isAugmentation ->
                let span =
                    match List.tryLast (typeHeadIdents ty) with
                    | Some id ->
                        let s = Range.unionRanges id.idRange fullRange
                        callableRanges[rangeKey id.idRange] <- s
                        s
                    | None -> fullRange

                callableRanges[rangeKey ctorRange] <- span
            | _ -> ()

            match repr with
            | SynTypeDefnRepr.ObjectModel(members = ms) -> ms |> List.iter walkMemberDefn
            | SynTypeDefnRepr.Simple _
            | SynTypeDefnRepr.Exception _ -> ()

            members |> List.iter walkMemberDefn

        let rec walkModuleDecl (d: SynModuleDecl) =
            match d with
            | SynModuleDecl.NestedModule(decls = ds) -> ds |> List.iter walkModuleDecl
            | SynModuleDecl.NamespaceFragment(SynModuleOrNamespace(decls = ds)) -> ds |> List.iter walkModuleDecl
            | SynModuleDecl.Let(bindings = bs) -> bs |> List.iter (walkBinding true)
            | SynModuleDecl.Expr(expr = e) -> walkExpr e
            | SynModuleDecl.Types(typeDefns = tds) -> tds |> List.iter walkTypeDefn
            | SynModuleDecl.Exception(exnDefn = SynExceptionDefn(exnRepr = SynExceptionDefnRepr(caseName = SynUnionCase(ident = SynIdent(id, _))); members = ms; range = fullRange)) ->
                typeRanges[rangeKey id.idRange] <- fullRange
                ms |> List.iter walkMemberDefn
            | _ -> ()

        let rec walkMemberSig (ms: SynMemberSig) =
            match ms with
            | SynMemberSig.Member(memberSig = SynValSig(ident = SynIdent(id, _)); range = r) ->
                callableRanges[rangeKey id.idRange] <- r
            | SynMemberSig.NestedType(nestedType = td) -> walkTypeDefnSig td
            | _ -> ()

        and walkTypeDefnSig (SynTypeDefnSig(typeInfo = info; typeRepr = repr; members = ms; range = fullRange)) =
            let isAugmentation =
                match repr with
                | SynTypeDefnSigRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation _) -> true
                | _ -> false

            recordTypeHeader info isAugmentation fullRange

            match repr with
            | SynTypeDefnSigRepr.ObjectModel(memberSigs = sigs) -> sigs |> List.iter walkMemberSig
            | SynTypeDefnSigRepr.Simple _
            | SynTypeDefnSigRepr.Exception _ -> ()

            ms |> List.iter walkMemberSig

        let rec walkSigDecl (d: SynModuleSigDecl) =
            match d with
            | SynModuleSigDecl.NestedModule(moduleDecls = ds) -> ds |> List.iter walkSigDecl
            | SynModuleSigDecl.NamespaceFragment(SynModuleOrNamespaceSig(decls = ds)) -> ds |> List.iter walkSigDecl
            | SynModuleSigDecl.Val(valSig = SynValSig(ident = SynIdent(id, _)); range = r) ->
                callableRanges[rangeKey id.idRange] <- r
            | SynModuleSigDecl.Types(types = tds) -> tds |> List.iter walkTypeDefnSig
            | SynModuleSigDecl.Exception(exnSig = SynExceptionSig(exnRepr = SynExceptionDefnRepr(caseName = SynUnionCase(ident = SynIdent(id, _))); members = ms; range = fullRange)) ->
                typeRanges[rangeKey id.idRange] <- fullRange
                ms |> List.iter walkMemberSig
            | _ -> ()

        match parseTree with
        | Some(ParsedInput.ImplFile(ParsedImplFileInput(contents = modules))) ->
            for SynModuleOrNamespace(decls = ds) in modules do
                ds |> List.iter walkModuleDecl
        | Some(ParsedInput.SigFile(ParsedSigFileInput(contents = modules))) ->
            for SynModuleOrNamespaceSig(decls = ds) in modules do
                ds |> List.iter walkSigDecl
        | None -> ()

        { TypeRanges = typeRanges
          AugmentationIdentRanges = augmentations
          ObjExprTypeRanges = objExprTypes
          CallableRanges = callableRanges }


/// Symbol keys as references count them, and the index of a file's uses by them.
module SymbolUses =
    /// What FCS counts as one symbol, as far as a key can say it: a constructor's uses are its type's; what a key sends to
    /// its case or type (a union's `IsCase`, a generated member) is a symbol of its own; a pattern's cases are told apart.
    let keyOf (symbol: FSharpSymbol) : string option =
        try
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as v when v.IsConstructor ->
                v.DeclaringEntity |> Option.bind (fun e -> DeclarationChecker.SymbolKey e) |> Option.orElse (DeclarationChecker.SymbolKey symbol)
            | :? FSharpMemberOrFunctionOrValue as v ->
                DeclarationChecker.SymbolKey symbol
                |> Option.map (fun key ->
                    match key.Split('|')[1] with
                    | "case"
                    | "type" -> key + "|" + v.LogicalName
                    | _ -> key)
            | :? FSharpActivePatternCase as c -> DeclarationChecker.SymbolKey symbol |> Option.map (fun key -> key + "|" + c.Name)
            // One symbol by its names and place wherever it is written, with no definition of its own
            | :? FSharpField as f when f.IsAnonRecordField ->
                let details, _, index = f.AnonRecordFieldDetails
                let names = String.concat "," details.SortedFieldNames
                Some $"%s{symbol.Assembly.SimpleName}|anonymous|%s{names}|%d{index}"
            | _ -> DeclarationChecker.SymbolKey symbol
        with :? InvalidOperationException ->
            None

    /// Keys of a file's uses. A pattern's case inside its own definition names no value: it is the case of the last pattern
    /// of its name defined before it in the file.
    let keysIn (check: FSharpCheckFileResults) (ct: CancellationToken) : FSharpSymbolUse -> string option =
        let patterns =
            lazy
                (check.GetAllUsesOfAllSymbolsInFile ct
                 |> Seq.choose (fun u ->
                     match u.Symbol with
                     | :? FSharpMemberOrFunctionOrValue as v when u.IsFromDefinition && v.IsActivePattern ->
                         DeclarationChecker.SymbolKey u.Symbol |> Option.map (fun key -> u.Range, v.LogicalName.Split('|'), key)
                     | _ -> None)
                 |> Seq.toArray)

        fun (u: FSharpSymbolUse) ->
            match keyOf u.Symbol, u.Symbol with
            // The `new` of a constructor is that constructor's, where its calls are its type's
            | _, (:? FSharpMemberOrFunctionOrValue as v) when u.IsFromDefinition && v.IsConstructor -> DeclarationChecker.SymbolKey u.Symbol
            | None, (:? FSharpActivePatternCase as c) ->
                patterns.Value
                |> Array.filter (fun (r, names, _) -> Array.contains c.Name names && Position.posGeq u.Range.Start r.Start)
                |> Array.tryLast
                |> Option.map (fun (_, _, key) -> key + "|" + c.Name)
            | key, _ -> key

    /// Whether a definition with a key is the only one of that key. A type has its constructors' key, and many of them; a
    /// pattern's case says where its use is, not where it is defined.
    let definesItsKey (symbol: FSharpSymbol) =
        match symbol with
        | :? FSharpActivePatternCase -> false
        | :? FSharpMemberOrFunctionOrValue as v -> not v.IsConstructor
        | _ -> true

    let private plainIdentifier (name: string) =
        name.Length > 0
        && (Char.IsLetter name[0] || name[0] = '_')
        && name |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '\'')
        && not (name.StartsWith "op_")

    /// <summary>
    /// The name every use of the symbol has in the text of its file, for the kinds of symbol that can be shown to be
    /// written by name; None for every other. A whitelist of kinds: what isn't listed scans every file.
    /// </summary>
    /// <remarks>
    /// Left out, each for reaching a symbol with no such text (ReferencesGridTexts has a shape of each): members, as the
    /// compiler looks up `Bind`, `Dispose`, `GetEnumerator`, `Item`, `GetSlice`, `op_Implicit` and so on by name, and a
    /// custom operation has another spelling; types, modules and namespaces, through abbreviations, `FooAttribute` and
    /// constructors; active patterns; anything not a plain identifier, as an operator is spelled otherwise than its name; a
    /// value of a `NumericLiteralQ` module, which `1Q` calls with no text. That guard is by the declaring module's name, and a
    /// module abbreviation (`module NumericLiteralZ = Other`) gets past it, as the compiler resolves the module by its name
    /// in the environment: it is safe only because FCS records the module at the literal and not the value. The shape
    /// "written by name" pins that. A case's generated `IsCase` has a key of its own.
    /// FSharp.Core's values and cases (`Some` for an optional argument, `GetArraySlice` for a slice) are reached from no
    /// text too: `References` applies this only to what a loaded project defines.
    /// </remarks>
    let spelledName (symbol: FSharpSymbol) : string option =
        let name =
            match symbol with
            | :? FSharpUnionCase as c -> Some c.Name
            | :? FSharpField as f when
                not f.IsAnonRecordField
                && not f.IsCompilerGenerated
                && f.DeclaringUnionCase.IsNone
                && f.DeclaringEntity |> Option.exists _.IsFSharpRecord
                ->
                Some f.Name
            | :? FSharpMemberOrFunctionOrValue as v when
                v.IsModuleValueOrMember
                && not v.IsMember
                && not v.IsExtensionMember
                && not v.IsActivePattern
                && not v.IsCompilerGenerated
                && v.DeclaringEntity |> Option.exists (fun e -> e.IsFSharpModule && not (e.LogicalName.StartsWith "NumericLiteral"))
                ->
                Some v.LogicalName
            | _ -> None

        name |> Option.filter plainIdentifier

    let private isTrivialSuper (e: FSharpEntity) =
        match e.TryFullName with
        | Some("System.Object" | "System.ValueType" | "System.Enum") -> true
        | _ -> false

    let private entityGlyphOf (e: FSharpEntity) : SymbolGlyph =
        try
            if e.IsInterface then SymbolGlyph.Interface
            elif e.IsEnum then SymbolGlyph.Enum
            elif e.IsDelegate then SymbolGlyph.Delegate
            elif e.IsFSharpExceptionDeclaration then SymbolGlyph.Exception
            elif e.IsValueType then SymbolGlyph.Struct
            elif e.IsFSharpUnion then SymbolGlyph.Union
            elif e.IsFSharpRecord then SymbolGlyph.Type
            else SymbolGlyph.Class
        with _ ->
            SymbolGlyph.Class

    let private callableGlyphOf (v: FSharpMemberOrFunctionOrValue) : SymbolGlyph =
        try
            if v.IsConstructor then SymbolGlyph.Constructor
            elif v.IsEvent then SymbolGlyph.Event
            elif v.IsProperty || v.IsPropertyGetterMethod || v.IsPropertySetterMethod then SymbolGlyph.Property
            elif v.IsExtensionMember then SymbolGlyph.ExtensionMethod
            elif v.IsOverrideOrExplicitInterfaceImplementation then SymbolGlyph.OverridenMethod
            elif v.IsMember then SymbolGlyph.Method
            elif v.FullType.IsFunctionType then SymbolGlyph.Function
            else SymbolGlyph.Field
        with _ ->
            SymbolGlyph.Method

    let private callableNameOf (v: FSharpMemberOrFunctionOrValue) : string =
        try
            let raw = v.DisplayNameCore

            if (v.IsPropertyGetterMethod || v.IsPropertySetterMethod) && (raw.StartsWith("get_", StringComparison.Ordinal) || raw.StartsWith("set_", StringComparison.Ordinal)) then
                raw.Substring 4
            else
                raw
        with _ ->
            v.LogicalName

    let private callableKeyOf (r: range) (v: FSharpMemberOrFunctionOrValue) : string =
        let own =
            if v.IsConstructor then
                DeclarationChecker.SymbolKey v
            else
                keyOf v |> Option.map DefinitionSites.withoutSetter

        match own with
        | Some k -> k
        | None ->
            let asm = try v.Assembly.SimpleName with _ -> "local"
            $"%s{asm}|local|%s{r.FileName}|%d{r.StartLine}:%d{r.StartColumn}|%s{v.LogicalName}"

    let private superKeysOf (e: FSharpEntity) : string[] =
        let acc = ResizeArray<string>()

        try
            match e.BaseType with
            | Some b when b.HasTypeDefinition && not (isTrivialSuper b.TypeDefinition) ->
                keyOf b.TypeDefinition |> Option.iter acc.Add
            | _ -> ()

            for ifc in e.DeclaredInterfaces do
                if ifc.HasTypeDefinition then
                    keyOf ifc.TypeDefinition |> Option.iter acc.Add
        with _ ->
            ()

        acc |> Seq.distinct |> Seq.toArray

    let private ancestorSuperKeysOf (includeSelf: bool) (e: FSharpEntity) : string[] =
        let acc = ResizeArray<string>()
        let visited = HashSet<string>(StringComparer.Ordinal)

        let addEntity (ent: FSharpEntity) =
            match keyOf ent with
            | Some k when visited.Add k -> acc.Add k
            | _ -> ()

        try
            if includeSelf then
                addEntity e

            let mutable cur = e.BaseType
            let mutable guard = 0

            while cur.IsSome && guard < 64 do
                guard <- guard + 1

                match cur with
                | Some b when b.HasTypeDefinition && not (isTrivialSuper b.TypeDefinition) ->
                    let td = b.TypeDefinition
                    addEntity td
                    cur <- td.BaseType
                | _ ->
                    cur <- None

            for ifc in e.AllInterfaces do
                if ifc.HasTypeDefinition then
                    addEntity ifc.TypeDefinition
        with _ ->
            ()

        acc.ToArray()

    let private buildRelations
        (isImplFile: bool)
        (hasParseTree: bool)
        (ast: AstIndex.Info)
        (defs: seq<struct (range * FSharpSymbol)>)
        (extraImpls: seq<struct (string * range)>)
        =
        let implBuilders = Dictionary<string, ResizeArray<range>>()
        let implSeen = Dictionary<string, HashSet<AstIndex.RangeKey>>()

        let addImpl (rawKey: string) (r: range) =
            let k = DefinitionSites.withoutSetter rawKey
            let rk = AstIndex.rangeKey r

            let seen =
                match implSeen.TryGetValue k with
                | true, s -> s
                | _ ->
                    let s = HashSet<AstIndex.RangeKey>()
                    implSeen[k] <- s
                    s

            if seen.Add rk then
                match implBuilders.TryGetValue k with
                | true, l -> l.Add r
                | _ ->
                    let l = ResizeArray<range>()
                    l.Add r
                    implBuilders[k] <- l

        for struct (k, r) in extraImpls do
            addImpl k r

        let typesByKey = Dictionary<string, TypeRelationEntry>()
        let subtypesBuilders = Dictionary<string, ResizeArray<TypeRelationEntry>>()
        let callableList = ResizeArray<CallableDecl>()
        let callableSeen = HashSet<struct (string * AstIndex.RangeKey)>()

        for struct (r, symbol) in defs do
            let rk = AstIndex.rangeKey r

            match symbol with
            | :? FSharpEntity as e ->
                let isRealType =
                    try
                        not e.IsNamespace && not e.IsFSharpModule && not e.IsFSharpAbbreviation
                    with _ ->
                        false

                if isRealType && not (ast.AugmentationIdentRanges.Contains rk) then
                    match keyOf e with
                    | Some typeKey when not (typesByKey.ContainsKey typeKey) ->
                        let supers = superKeysOf e

                        let fullRange =
                            match ast.TypeRanges.TryGetValue rk with
                            | true, fr -> fr
                            | _ -> r

                        let name = try e.DisplayNameCore with _ -> e.LogicalName

                        let entry: TypeRelationEntry =
                            { Key = typeKey
                              Name = name
                              Glyph = entityGlyphOf e
                              Range = fullRange
                              SelectionRange = r
                              SuperKeys = supers }

                        typesByKey[typeKey] <- entry

                        for superKey in supers do
                            match subtypesBuilders.TryGetValue superKey with
                            | true, l -> l.Add entry
                            | _ ->
                                let l = ResizeArray<TypeRelationEntry>()
                                l.Add entry
                                subtypesBuilders[superKey] <- l

                        let isConcrete =
                            try
                                not e.IsInterface && not e.IsAbstractClass
                            with _ ->
                                false

                        if isImplFile && isConcrete then
                            for ancestorKey in ancestorSuperKeysOf false e do
                                addImpl ancestorKey r
                    | _ -> ()

            | :? FSharpMemberOrFunctionOrValue as v ->
                let isGenerated = try v.IsCompilerGenerated with _ -> true

                if not isGenerated then
                    // Record callable declaration for EnclosingCallable
                    let callableRangeOpt =
                        match ast.CallableRanges.TryGetValue rk with
                        | true, fullRange -> ValueSome fullRange
                        | _ when not hasParseTree && (try v.IsModuleValueOrMember with _ -> false) -> ValueSome r
                        | _ -> ValueNone

                    match callableRangeOpt with
                    | ValueSome fullRange ->
                        let cKey = callableKeyOf r v
                        let cRangeKey = AstIndex.rangeKey fullRange

                        if callableSeen.Add(struct (cKey, cRangeKey)) then
                            callableList.Add
                                { Key = cKey
                                  Name = callableNameOf v
                                  Glyph = callableGlyphOf v
                                  Range = fullRange
                                  SelectionRange = r }
                    | ValueNone -> ()

            | _ -> ()

        let implementations = Dictionary<string, range[]>(implBuilders.Count)

        for KeyValue(k, l) in implBuilders do
            implementations[k] <- l.ToArray()

        let subtypesBySuper = Dictionary<string, TypeRelationEntry[]>(subtypesBuilders.Count)

        for KeyValue(k, l) in subtypesBuilders do
            subtypesBySuper[k] <- l.ToArray()

        let callables = callableList.ToArray()

        let inline comparePos (p1: pos) (p2: pos) =
            let c = compare p1.Line p2.Line
            if c <> 0 then c else compare p1.Column p2.Column

        Array.sortInPlaceWith
            (fun (a: CallableDecl) (b: CallableDecl) ->
                let aLines = a.Range.EndLine - a.Range.StartLine
                let bLines = b.Range.EndLine - b.Range.StartLine

                if aLines <> bLines then
                    compare aLines bLines
                else
                    let cStart = comparePos b.Range.Start a.Range.Start

                    if cStart <> 0 then
                        cStart
                    else
                        comparePos a.Range.End b.Range.End)
            callables

        implementations, typesByKey, subtypesBySuper, callables

    let private build
        (source: obj)
        (definitionSites: Dictionary<string, DefinitionSite[]>)
        (conforming: Dictionary<string, DefinitionSite[]>)
        (implConforming: Dictionary<string, DefinitionSite[]>)
        (implementations: Dictionary<string, range[]>)
        (typesByKey: Dictionary<string, TypeRelationEntry>)
        (subtypesBySuper: Dictionary<string, TypeRelationEntry[]>)
        (callables: CallableDecl[])
        (uses: seq<struct (string * range * bool * range voption)>)
        =
        let hits = Dictionary<string, List<struct (range * bool)>>()
        let definitions = Dictionary<string, List<range>>()

        for struct (key, r, isDefinition, location) in uses do
            match hits.TryGetValue key with
            | true, l -> l.Add(struct (r, isDefinition))
            | _ -> hits[key] <- List [ struct (r, isDefinition) ]

            match location with
            | ValueSome at ->
                match definitions.TryGetValue key with
                | true, l -> l.Add at
                | _ -> definitions[key] <- List [ at ]
            | ValueNone -> ()

        let byKey = Dictionary<string, KeyUses>(hits.Count)

        for KeyValue(key, l) in hits do
            byKey[key] <-
                { Hits = l.ToArray()
                  Definitions =
                    match definitions.TryGetValue key with
                    | true, d -> d.ToArray()
                    | _ -> [||] }

        FileUses(WeakReference source, byKey, definitionSites, conforming, implConforming, implementations, typesByKey, subtypesBySuper, callables)

    let private noSites = Dictionary<string, DefinitionSite[]>()

    /// Each definition's site by its raw key, constructors included. A union case's or exception's fields too: the
    /// compiler reports a definition only for a named one, and an unnamed one (`Item1`, `Data0`) can be named in a use.
    let private sitesOf (definitions: seq<FSharpSymbol>) : DefinitionSite list =
        let withFields (symbol: FSharpSymbol) : FSharpSymbol seq =
            seq {
                yield symbol

                match symbol with
                | :? FSharpUnionCase as c -> yield! c.Fields |> Seq.cast<FSharpSymbol>
                | :? FSharpEntity as e when e.IsFSharpExceptionDeclaration -> yield! e.FSharpFields |> Seq.cast<FSharpSymbol>
                | _ -> ()
            }

        [ for symbol in definitions |> Seq.collect withFields do
              match DeclarationChecker.SymbolKey symbol with
              | Some key -> DefinitionSites.siteOf key symbol
              | None -> () ]

    /// Indexes an implementation file's check, using `parseTree` when provided for declaration body ranges and object expressions.
    let internal ofCheckWithParse
        (source: obj)
        (parseTree: ParsedInput option)
        (check: FSharpCheckFileResults)
        (ct: CancellationToken)
        : FileUses =
        let keys = keysIn check ct
        let all = check.GetAllUsesOfAllSymbolsInFile ct |> Seq.toArray
        let ast = AstIndex.build parseTree

        let definitions =
            all |> Seq.filter _.IsFromDefinition |> Seq.map _.Symbol |> sitesOf |> DefinitionSites.group DefinitionSites.withoutSetter false

        let implConforming =
            all
            |> Seq.choose (fun u ->
                if not u.IsFromDefinition || ast.AugmentationIdentRanges.Contains(AstIndex.rangeKey u.Range) then
                    None
                else
                    let skipGenerated =
                        match u.Symbol with
                        | :? FSharpMemberOrFunctionOrValue as v ->
                            (try v.IsCompilerGenerated with _ -> false)
                            && not v.IsPropertyGetterMethod
                            && not v.IsPropertySetterMethod
                            && not v.IsConstructor
                        | _ -> false

                    if skipGenerated then
                        None
                    else
                        match DeclarationChecker.SymbolKey u.Symbol with
                        | None -> None
                        | Some rawKey ->
                            let getter, setter =
                                match u.Symbol with
                                | :? FSharpMemberOrFunctionOrValue as v -> v.IsPropertyGetterMethod, v.IsPropertySetterMethod
                                | _ -> false, false

                            Some
                                { Key = rawKey
                                  Location = ValueSome u.Range
                                  IsGetter = getter
                                  IsSetter = setter })
            |> DefinitionSites.group DefinitionSites.conformance true

        let extraImpls =
            seq {
                for u in all do
                    if u.IsFromDispatchSlotImplementation then
                        match keys u with
                        | Some k -> yield struct (k, u.Range)
                        | None -> ()
                    elif not u.IsFromDefinition && ast.ObjExprTypeRanges.Contains(AstIndex.rangeKey u.Range) then
                        match u.Symbol with
                        | :? FSharpEntity as objEnt ->
                            for k in ancestorSuperKeysOf true objEnt do
                                yield struct (k, u.Range)
                        | :? FSharpMemberOrFunctionOrValue as ctor when ctor.IsConstructor ->
                            match ctor.DeclaringEntity with
                            | Some objEnt ->
                                for k in ancestorSuperKeysOf true objEnt do
                                    yield struct (k, u.Range)
                            | None ->
                                match keyOf u.Symbol with
                                | Some k -> yield struct (k, u.Range)
                                | None -> ()
                        | _ ->
                            match keyOf u.Symbol with
                            | Some k -> yield struct (k, u.Range)
                            | None -> ()
            }

        let defPairs =
            all
            |> Seq.choose (fun u -> if u.IsFromDefinition then Some(struct (u.Range, u.Symbol)) else None)

        let implementations, typesByKey, subtypesBySuper, callables =
            buildRelations true parseTree.IsSome ast defPairs extraImpls

        all
        |> Seq.choose (fun u ->
            keys u
            |> Option.map (fun key ->
                let location =
                    if u.IsFromDefinition && definesItsKey u.Symbol then
                        match u.Symbol.SignatureLocation |> Option.orElse u.Symbol.DeclarationLocation with
                        | Some at -> ValueSome at
                        | None -> ValueNone
                    else
                        ValueNone

                struct (key, u.Range, u.IsFromDefinition, location)))
        |> build source definitions noSites implConforming implementations typesByKey subtypesBySuper callables

    /// Indexes an implementation file's check.
    let internal ofCheck (source: obj) (check: FSharpCheckFileResults) (ct: CancellationToken) : FileUses =
        ofCheckWithParse source None check ct

    /// Indexes what a signature file's check resolved, using `parseTree` when provided for full declaration ranges.
    let internal ofSignatureWithParse
        (source: obj)
        (parseTree: ParsedInput option)
        (uses: (struct (range * bool * FSharpSymbol))[])
        (ct: CancellationToken)
        : FileUses =
        // What SignatureLocation walks is the file's own module type. A module's enclosing namespaces are in it, with no
        // definition use; a `type System.String with` augmentation's use is a definition of what another file defines.
        let enclosing (symbol: FSharpSymbol) : FSharpSymbol seq =
            seq {
                yield symbol

                match symbol with
                | :? FSharpEntity as e ->
                    let mutable parent = e.DeclaringEntity

                    while parent |> Option.exists _.IsNamespace do
                        yield parent.Value :> FSharpSymbol
                        parent <- parent.Value.DeclaringEntity
                | _ -> ()
            }

        let file =
            uses |> Array.tryPick (fun (struct (r, definition, _)) -> if definition then Some r.FileName else None)

        let conforming =
            uses
            |> Seq.choose (fun (struct (_, definition, symbol)) -> if definition then Some symbol else None)
            |> Seq.collect enclosing
            |> sitesOf
            |> List.filter (fun s ->
                match s.Location, file with
                | ValueSome r, Some f -> String.Equals(r.FileName, f, StringComparison.OrdinalIgnoreCase)
                | _ -> true)
            |> DefinitionSites.group DefinitionSites.conformance true

        // As in an implementation file (keysIn): a pattern's case names no value in the signature that lists the pattern
        let patterns =
            uses
            |> Array.choose (fun (struct (r, definition, symbol)) ->
                match symbol with
                | :? FSharpMemberOrFunctionOrValue as v when definition && v.IsActivePattern ->
                    DeclarationChecker.SymbolKey symbol |> Option.map (fun k -> r, v.LogicalName.Split('|'), k)
                | _ -> None)

        let ast = AstIndex.build parseTree

        let defPairs =
            uses
            |> Seq.choose (fun (struct (r, definition, symbol)) -> if definition then Some(struct (r, symbol)) else None)

        let implementations, typesByKey, subtypesBySuper, callables =
            buildRelations false parseTree.IsSome ast defPairs Seq.empty

        uses
        |> Seq.choose (fun (struct (r, definition, symbol)) ->
            ct.ThrowIfCancellationRequested()

            let own =
                match symbol with
                | :? FSharpMemberOrFunctionOrValue as v when definition && v.IsConstructor -> DeclarationChecker.SymbolKey symbol
                | :? FSharpActivePatternCase as c when (keyOf symbol).IsNone ->
                    patterns
                    |> Array.filter (fun (at, names, _) -> Array.contains c.Name names && Position.posGeq r.Start at.Start)
                    |> Array.tryLast
                    |> Option.map (fun (_, _, k) -> k + "|" + c.Name)
                | _ -> keyOf symbol

            own |> Option.map (fun key -> struct (key, r, definition, ValueNone)))
        |> build source noSites conforming noSites implementations typesByKey subtypesBySuper callables

    /// Indexes what a signature file's check resolved. Its definitions are kept by raw key for SignatureLocation, and have
    /// no locations to tell apart in the uses.
    let internal ofSignature (source: obj) (uses: (struct (range * bool * FSharpSymbol))[]) (ct: CancellationToken) : FileUses =
        ofSignatureWithParse source None uses ct
