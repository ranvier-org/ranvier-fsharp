namespace Rfs.Checker

open System.Collections.Generic
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml

/// <summary>
/// A file's `///` comments by the path of the declaration each is on, read from a parse alone: what FCS's tooltip reads a
/// doc from (FSharpDocSource), so a hover shows the comment of the latest text with no check.
/// </summary>
/// <remarks>
/// The paths are FSharpDocOwner's, as the fork makes them from the typed tree: the enclosing namespaces, modules and types
/// by the names the source gives them, then the kind and name, and for a member its place among the members of its type
/// in the file with that kind and name (`#0`, `#1`...), augmentations included.
/// </remarks>
[<Sealed>]
type FileDocs private (docs: IReadOnlyDictionary<string, XmlDoc>) =
    static let empty = FileDocs(Dictionary())

    /// The comment on the declaration at `path`: XmlDoc.Empty when the file has none there, or has no such declaration
    member _.Comment(path: string) : XmlDoc =
        match docs.TryGetValue path with
        | true, d -> d
        | _ -> XmlDoc.Empty

    member _.Count = docs.Count

    /// Whether the file has a declaration at `path`
    member _.Has(path: string) = docs.ContainsKey path

    static member Empty = empty

    static member OfParse(input: ParsedInput) : FileDocs =
        let docs = Dictionary<string, XmlDoc>()
        let ordinals = Dictionary<string, int>()

        let add (path: string) (doc: PreXmlDoc) =
            if not (docs.ContainsKey path) then
                docs[path] <- doc.ToXmlDoc(false, None)

        let addMember (typePath: string) (tag: string) (doc: PreXmlDoc) =
            let key = typePath + "." + tag

            let n =
                match ordinals.TryGetValue key with
                | true, n -> n
                | _ -> 0

            ordinals[key] <- n + 1
            add ("M:" + key + "#" + string n) doc

        let join (path: string) (name: string) = if path = "" then name else path + "." + name

        // A type's name with its number of type parameters, as `Box` and `Box<'T>` are two types
        let typeName (info: SynComponentInfo) =
            let (SynComponentInfo(typeParams = typars)) = info
            let name = (List.last info.LongIdent).idText

            match typars with
            | Some t when not t.TyparDecls.IsEmpty -> name + "`" + string t.TyparDecls.Length
            | _ -> name

        let rec namesOf (pat: SynPat) : string list =
            match pat with
            | SynPat.Named(ident = SynIdent(id, _)) -> [ id.idText ]
            | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) -> [ (List.last ids).idText ]
            | SynPat.Typed(pat = p)
            | SynPat.Paren(pat = p)
            | SynPat.Attrib(pat = p) -> namesOf p
            | SynPat.Tuple(elementPats = ps) -> ps |> List.collect namesOf
            | SynPat.As(lhsPat = l; rhsPat = r) -> namesOf l @ namesOf r
            | _ -> []

        let tagOf (flags: SynMemberFlags) (name: string) =
            if flags.IsDispatchSlot then
                "abstract " + name
            else
                match flags.MemberKind with
                | SynMemberKind.Constructor -> "new"
                | SynMemberKind.PropertyGet
                | SynMemberKind.PropertySet
                | SynMemberKind.PropertyGetSet -> "property " + name
                | _ -> "member " + name

        let rec headPlaces (pat: SynPat) : range list =
            match pat with
            | SynPat.Named(ident = SynIdent(id, _)) -> [ id.idRange ]
            | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> [ (List.last ids).idRange ]
            | SynPat.Typed(pat = p)
            | SynPat.Paren(pat = p)
            | SynPat.Attrib(pat = p) -> headPlaces p
            | SynPat.Tuple(elementPats = ps) -> ps |> List.collect headPlaces
            | SynPat.As(lhsPat = l; rhsPat = r) -> headPlaces l @ headPlaces r
            | _ -> []

        let addBindingPlaces (pat: SynPat) (doc: PreXmlDoc) =
            for r in headPlaces pat do
                add $"@%d{r.StartLine},%d{r.StartColumn}" doc

        let rec walkBinding (SynBinding(xmlDoc = doc; headPat = pat; expr = expr)) =
            addBindingPlaces pat doc
            walkExpr expr

        and walkExpr (expr: SynExpr) =
            match expr with
            | SynExpr.Paren(expr = e)
            | SynExpr.Quote(quotedExpr = e)
            | SynExpr.Typed(expr = e)
            | SynExpr.ArrayOrListComputed(expr = e)
            | SynExpr.ComputationExpr(expr = e)
            | SynExpr.Do(expr = e)
            | SynExpr.Assert(expr = e)
            | SynExpr.AddressOf(expr = e)
            | SynExpr.InferredDowncast(expr = e)
            | SynExpr.InferredUpcast(expr = e)
            | SynExpr.DotGet(expr = e)
            | SynExpr.YieldOrReturn(expr = e)
            | SynExpr.YieldOrReturnFrom(expr = e)
            | SynExpr.DoBang(expr = e)
            | SynExpr.Downcast(expr = e)
            | SynExpr.Upcast(expr = e)
            | SynExpr.TypeTest(expr = e)
            | SynExpr.Lazy(expr = e)
            | SynExpr.TraitCall(argExpr = e)
            | SynExpr.IndexFromEnd(expr = e)
            | SynExpr.Lambda(body = e) -> walkExpr e
            | SynExpr.Tuple(exprs = es)
            | SynExpr.ArrayOrList(exprs = es) -> es |> List.iter walkExpr
            | SynExpr.Sequential(expr1 = e1; expr2 = e2)
            | SynExpr.App(funcExpr = e1; argExpr = e2)
            | SynExpr.TryFinally(tryExpr = e1; finallyExpr = e2)
            | SynExpr.While(whileExpr = e1; doExpr = e2)
            | SynExpr.DotSet(targetExpr = e1; rhsExpr = e2)
            | SynExpr.Set(targetExpr = e1; rhsExpr = e2) ->
                walkExpr e1
                walkExpr e2
            | SynExpr.IfThenElse(ifExpr = g; thenExpr = t; elseExpr = eOpt) ->
                walkExpr g
                walkExpr t
                eOpt |> Option.iter walkExpr
            | SynExpr.LetOrUse synLetOrUse ->
                synLetOrUse.Bindings |> List.iter walkBinding
                walkExpr synLetOrUse.Body
            | SynExpr.Match(expr = e; clauses = cs)
            | SynExpr.MatchBang(expr = e; clauses = cs) ->
                walkExpr e
                for SynMatchClause(whenExpr = wOpt; resultExpr = r) in cs do
                    wOpt |> Option.iter walkExpr
                    walkExpr r
            | SynExpr.TryWith(tryExpr = t; withCases = cs) ->
                walkExpr t
                for SynMatchClause(whenExpr = wOpt; resultExpr = r) in cs do
                    wOpt |> Option.iter walkExpr
                    walkExpr r
            | SynExpr.For(identBody = b; toBody = t) ->
                walkExpr b
                walkExpr t
            | SynExpr.ForEach(enumExpr = e; bodyExpr = b) ->
                walkExpr e
                walkExpr b
            | SynExpr.Record(copyInfo = copyOpt; recordFields = rfs) ->
                copyOpt |> Option.iter (fst >> walkExpr)
                for rf in rfs do
                    match rf with
                    | SynExprRecordFieldOrSpread.Field(SynExprRecordField(expr = eOpt), _) -> eOpt |> Option.iter walkExpr
                    | SynExprRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) -> walkExpr e
            | SynExpr.AnonRecd(copyInfo = copyOpt; recordFields = rfs) ->
                copyOpt |> Option.iter (fst >> walkExpr)
                for rf in rfs do
                    match rf with
                    | SynExprAnonRecordFieldOrSpread.Field(SynExprAnonRecordField(expr = e), _) -> walkExpr e
                    | SynExprAnonRecordFieldOrSpread.Spread(SynExprSpread(expr = e), _) -> walkExpr e
            | SynExpr.ObjExpr(bindings = bs; members = ms; argOptions = aOpt) ->
                aOpt |> Option.iter (fst >> walkExpr)
                bs |> List.iter walkBinding
                ms |> List.iter (memberDefn "")
            | SynExpr.DotIndexedGet(objectExpr = o; indexArgs = a) ->
                walkExpr o
                walkExpr a
            | SynExpr.DotIndexedSet(objectExpr = o; indexArgs = a; valueExpr = v) ->
                walkExpr o
                walkExpr a
                walkExpr v
            | SynExpr.IndexRange(expr1 = e1; expr2 = e2) ->
                e1 |> Option.iter walkExpr
                e2 |> Option.iter walkExpr
            | _ -> ()

        and bindingMember (typePath: string) (SynBinding(xmlDoc = doc; valData = SynValData(memberFlags = flags); headPat = pat; expr = expr)) =
            match flags, namesOf pat with
            | Some flags, names ->
                let name = match names with [] -> "" | ns -> List.last ns

                // A member with no arguments is a property, which the check decides, not the parser
                let flags =
                    match flags.MemberKind, pat with
                    | SynMemberKind.Member, SynPat.LongIdent(argPats = SynArgPats.Pats []) -> { flags with MemberKind = SynMemberKind.PropertyGet }
                    | _ -> flags

                addMember typePath (tagOf flags name) doc
                addBindingPlaces pat doc
                walkExpr expr
            | None, _ -> ()

        and fields (typePath: string) (fs: SynField list) =
            for SynField(idOpt = id; xmlDoc = doc) in fs do
                match id with
                | Some id -> add ("F:" + typePath + "." + id.idText) doc
                | None -> ()

        and simple (typePath: string) (repr: SynTypeDefnSimpleRepr) =
            match repr with
            | SynTypeDefnSimpleRepr.Union(unionCases = cases) ->
                for SynUnionCase(ident = SynIdent(id, _); xmlDoc = doc) in cases do
                    add ("C:" + typePath + "." + id.idText) doc
            | SynTypeDefnSimpleRepr.Record(recordFieldsAndSpreads = fs) ->
                fields typePath (fs |> List.choose (function SynFieldOrSpread.Field f -> Some f | _ -> None))
            | SynTypeDefnSimpleRepr.Enum(cases = cases) ->
                for SynEnumCase(ident = SynIdent(id, _); xmlDoc = doc) in cases do
                    add ("F:" + typePath + "." + id.idText) doc
            | _ -> ()

        and memberDefn (typePath: string) (m: SynMemberDefn) =
            match m with
            | SynMemberDefn.Member(memberDefn = b) -> bindingMember typePath b
            | SynMemberDefn.GetSetMember(get, set, _, _) ->
                match get, set with
                | Some b, _
                | None, Some b -> bindingMember typePath b
                | None, None -> ()
            | SynMemberDefn.ImplicitCtor(xmlDoc = doc) -> addMember typePath "new" doc
            | SynMemberDefn.AbstractSlot(slotSig = SynValSig(ident = SynIdent(id, _); xmlDoc = doc); flags = flags) ->
                addMember typePath (tagOf flags id.idText) doc
            | SynMemberDefn.AutoProperty(ident = id; xmlDoc = doc) -> addMember typePath ("property " + id.idText) doc
            | SynMemberDefn.Interface(members = Some ms) -> ms |> List.iter (memberDefn typePath)
            | SynMemberDefn.ValField(fieldInfo = f) -> fields typePath [ f ]
            // A class `let`: a value of the type, of a kind of its own, as a module of the type's name shares its path
            | SynMemberDefn.LetBindings(bindings = bs) ->
                for SynBinding(xmlDoc = doc; headPat = pat) as b in bs do
                    for name in namesOf pat do
                        add ("L:" + typePath + "." + name) doc
                    walkBinding b
            | SynMemberDefn.NestedType(typeDefn = t) -> typeDefn typePath t
            | _ -> ()

        and typeDefn (path: string) (SynTypeDefn(typeInfo = info; typeRepr = repr; members = members; implicitConstructor = ctor)) =
            let (SynComponentInfo(xmlDoc = doc)) = info
            let typePath = join path (typeName info)

            match repr with
            | SynTypeDefnRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation _) -> ()
            | _ -> add ("T:" + typePath) doc

            ctor |> Option.iter (memberDefn typePath)

            match repr with
            | SynTypeDefnRepr.ObjectModel(members = ms) ->
                // The implicit constructor is among them too
                for m in ms do
                    match m with
                    | SynMemberDefn.ImplicitCtor _ when ctor.IsSome -> ()
                    | m -> memberDefn typePath m
            | SynTypeDefnRepr.Simple(simpleRepr = s) -> simple typePath s
            | SynTypeDefnRepr.Exception _ -> ()

            members |> List.iter (memberDefn typePath)

        let exceptionRepr (path: string) (SynExceptionDefnRepr(caseName = SynUnionCase(ident = SynIdent(id, _)); xmlDoc = doc)) =
            let typePath = join path id.idText
            add ("T:" + typePath) doc
            typePath

        let rec decl (path: string) (d: SynModuleDecl) =
            match d with
            | SynModuleDecl.Let(bindings = bs) ->
                for SynBinding(xmlDoc = doc; headPat = pat) as b in bs do
                    for name in namesOf pat do
                        add ("V:" + path + "." + name) doc
                    walkBinding b
            | SynModuleDecl.Types(typeDefns = ts) -> ts |> List.iter (typeDefn path)
            | SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(xmlDoc = doc) as info; decls = ds) ->
                let inner = join path (List.last info.LongIdent).idText
                add ("N:" + inner) doc
                ds |> List.iter (decl inner)
            | SynModuleDecl.Exception(exnDefn = SynExceptionDefn(exnRepr = r; members = ms)) ->
                let typePath = exceptionRepr path r
                ms |> List.iter (memberDefn typePath)
            | SynModuleDecl.NamespaceFragment fragment -> moduleOrNamespace fragment
            | _ -> ()

        and moduleOrNamespace (SynModuleOrNamespace(longId = lid; kind = kind; decls = ds; xmlDoc = doc)) =
            let path = lid |> List.map _.idText |> String.concat "."

            if kind.IsModule && not lid.IsEmpty then
                add ("N:" + path) doc

            ds |> List.iter (decl path)

        let memberSig (typePath: string) (m: SynMemberSig) =
            match m with
            | SynMemberSig.Member(memberSig = SynValSig(ident = SynIdent(id, _); xmlDoc = doc); flags = flags) ->
                addMember typePath (tagOf flags id.idText) doc
            | SynMemberSig.ValField(field = f) -> fields typePath [ f ]
            | _ -> ()

        let typeSig (path: string) (SynTypeDefnSig(typeInfo = SynComponentInfo(xmlDoc = doc) as info; typeRepr = repr; members = members)) =
            let typePath = join path (typeName info)

            match repr with
            | SynTypeDefnSigRepr.ObjectModel(kind = SynTypeDefnKind.Augmentation _) -> ()
            | _ -> add ("T:" + typePath) doc

            match repr with
            | SynTypeDefnSigRepr.ObjectModel(memberSigs = ms) -> ms |> List.iter (memberSig typePath)
            | SynTypeDefnSigRepr.Simple(repr = s) -> simple typePath s
            | SynTypeDefnSigRepr.Exception _ -> ()

            members |> List.iter (memberSig typePath)

        let rec sigDecl (path: string) (d: SynModuleSigDecl) =
            match d with
            | SynModuleSigDecl.Val(valSig = SynValSig(ident = SynIdent(id, _); xmlDoc = doc)) -> add ("V:" + path + "." + id.idText) doc
            | SynModuleSigDecl.Types(types = ts) -> ts |> List.iter (typeSig path)
            | SynModuleSigDecl.NestedModule(moduleInfo = SynComponentInfo(xmlDoc = doc) as info; moduleDecls = ds) ->
                let inner = join path (List.last info.LongIdent).idText
                add ("N:" + inner) doc
                ds |> List.iter (sigDecl inner)
            | SynModuleSigDecl.Exception(exnSig = SynExceptionSig(exnRepr = r; members = ms)) ->
                let typePath = exceptionRepr path r
                ms |> List.iter (memberSig typePath)
            | SynModuleSigDecl.NamespaceFragment fragment -> sigModuleOrNamespace fragment
            | _ -> ()

        and sigModuleOrNamespace (SynModuleOrNamespaceSig(longId = lid; kind = kind; decls = ds; xmlDoc = doc)) =
            let path = lid |> List.map _.idText |> String.concat "."

            if kind.IsModule && not lid.IsEmpty then
                add ("N:" + path) doc

            ds |> List.iter (sigDecl path)

        match input with
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) -> modules |> List.iter moduleOrNamespace
        | ParsedInput.SigFile(ParsedSigFileInput(contents = modules)) -> modules |> List.iter sigModuleOrNamespace

        FileDocs docs

    /// The comment of an owner whose file `docsOf` knows, or ValueNone, for FSharpDocSource.Compose
    static member CommentOf(docsOf: string -> FileDocs voption) : FSharpDocOwner -> XmlDoc voption =
        fun owner ->
            match docsOf owner.FileName with
            // A place the parse has no member at: returns ValueNone. Any miss of a workspace owner is marked stale
            // by Hover (StaleMark) rather than silently returning the check's doc, as doc edits check zero declarations.
            | ValueSome docs when owner.Path.StartsWith "@" && not (docs.Has owner.Path) -> ValueNone
            | ValueSome docs -> ValueSome(docs.Comment owner.Path)
            | ValueNone -> ValueNone
