module Rfs.Server.CallHierarchy

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

let private setterSuffix = "|setter"

let private withoutSetter (key: string) =
    if key.EndsWith(setterSuffix, StringComparison.Ordinal) then
        key.Substring(0, key.Length - setterSuffix.Length)
    else
        key

let private sameFile (a: string) (b: string) =
    if String.IsNullOrWhiteSpace a || String.IsNullOrWhiteSpace b then
        false
    else
        try
            String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)
        with _ ->
            String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

let private normPath (p: string) =
    try
        Path.GetFullPath p
    with _ ->
        p

let internal symbolGlyphToKind (glyph: SymbolGlyph) : SymbolKind =
    match glyph with
    | SymbolGlyph.Function -> SymbolKind.Function
    | SymbolGlyph.Method
    | SymbolGlyph.OverridenMethod
    | SymbolGlyph.ExtensionMethod -> SymbolKind.Method
    | SymbolGlyph.Constructor -> SymbolKind.Constructor
    | SymbolGlyph.Property -> SymbolKind.Property
    | SymbolGlyph.Event -> SymbolKind.Event
    | SymbolGlyph.Field -> SymbolKind.Field
    | SymbolGlyph.Interface -> SymbolKind.Interface
    | SymbolGlyph.Struct -> SymbolKind.Struct
    | SymbolGlyph.Enum -> SymbolKind.Enum
    | SymbolGlyph.Delegate -> SymbolKind.Function
    | SymbolGlyph.Module -> SymbolKind.Module
    | SymbolGlyph.Class
    | SymbolGlyph.Exception
    | SymbolGlyph.Type
    | SymbolGlyph.Union -> SymbolKind.Class
    | _ -> SymbolKind.Function

let inline private toLspRange (r: range) : LspRange =
    { Start =
        { Line = uint32 (max 0 (r.StartLine - 1))
          Character = uint32 (max 0 r.StartColumn) }
      End =
        { Line = uint32 (max 0 (r.EndLine - 1))
          Character = uint32 (max 0 r.EndColumn) } }

let inline private posLeq (a: LspPos) (b: LspPos) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let inline private unionLspRanges (a: LspRange) (b: LspRange) : LspRange =
    { Start = if posLeq a.Start b.Start then a.Start else b.Start
      End = if posLeq a.End b.End then b.End else a.End }

let private compareLspRange (a: LspRange) (b: LspRange) =
    let c1 = compare a.Start.Line b.Start.Line

    if c1 <> 0 then
        c1
    else
        let c2 = compare a.Start.Character b.Start.Character

        if c2 <> 0 then
            c2
        else
            let c3 = compare a.End.Line b.End.Line
            if c3 <> 0 then c3 else compare a.End.Character b.End.Character

[<NoComparison; NoEquality>]
type private CallableTarget =
    { OwnKey: string
      CallKey: string
      IsCtor: bool
      IsEntityFallback: bool
      Landed: string option }

let private serializeData (ownKey: string) (callKey: string) (isCtor: bool) (isEntity: bool) (declPath: string) (landedPath: string) : JToken =
    let obj = JObject()
    obj["key"] <- JValue ownKey
    obj["callKey"] <- JValue callKey
    obj["isCtor"] <- JValue isCtor
    obj["isEntity"] <- JValue isEntity
    obj["path"] <- JValue declPath
    obj["landed"] <- JValue landedPath
    obj :> JToken

let private tryParseData (item: CallHierarchyItem) : (string * string * bool * bool * string * string) option =
    match item.Data with
    | Some(:? JObject as obj) ->
        let str (name: string) =
            match obj[name] with
            | null -> None
            | tok ->
                let s = tok.Value<string>()
                if String.IsNullOrEmpty s then None else Some s

        let boolean (name: string) =
            match obj[name] with
            | null -> false
            | tok ->
                try
                    tok.Value<bool>()
                with _ ->
                    false

        match str "key", str "path" with
        | Some key, Some declPath ->
            let callKey = str "callKey" |> Option.defaultValue key
            let landed = str "landed" |> Option.defaultValue declPath
            Some(key, callKey, boolean "isCtor", boolean "isEntity", declPath, landed)
        | _ -> None
    | _ -> None

let private makeItemFromCallable (declPath: string) (landedPath: string) (isCtor: bool) (c: CallableDecl) : CallHierarchyItem =
    let sel = toLspRange c.SelectionRange
    let full = unionLspRanges (toLspRange c.Range) sel
    let ownKey = c.Key
    let actualCtor = isCtor || c.Glyph = SymbolGlyph.Constructor

    { Name = c.Name
      Kind = symbolGlyphToKind c.Glyph
      Tags = None
      Detail = None
      Uri = Paths.toUri declPath
      Range = full
      SelectionRange = sel
      Data = Some(serializeData ownKey ownKey actualCtor false declPath landedPath) }

let private makeItemFromType (declPath: string) (landedPath: string) (t: TypeRelationEntry) : CallHierarchyItem =
    let sel = toLspRange t.SelectionRange
    let full = unionLspRanges (toLspRange t.Range) sel

    { Name = t.Name
      Kind = SymbolKind.Constructor
      Tags = None
      Detail = None
      Uri = Paths.toUri declPath
      Range = full
      SelectionRange = sel
      Data = Some(serializeData t.Key t.Key false true declPath landedPath) }

let rec private unwrapEntity (depth: int) (e: FSharpEntity) : FSharpEntity option =
    if depth > 16 then
        Some e
    elif (try e.IsNamespace || e.IsFSharpModule with _ -> true) then
        None
    elif (try e.IsFSharpAbbreviation with _ -> false) then
        match (try Some e.AbbreviatedType with _ -> None) with
        | Some ty when (try ty.HasTypeDefinition with _ -> false) ->
            unwrapEntity (depth + 1) ty.TypeDefinition
        | _ -> None
    else
        Some e

let private landedOf (fallbackPath: string option) (symbol: FSharpSymbol) : string option =
    symbol.SignatureLocation
    |> Option.orElse symbol.DeclarationLocation
    |> Option.filter (fun r -> not (r.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)))
    |> Option.map _.FileName
    |> Option.orElse fallbackPath

let private extractTarget (fallbackPath: string option) (symbol: FSharpSymbol) : CallableTarget option =
    match symbol with
    | :? FSharpParameter -> None
    | :? FSharpMemberOrFunctionOrValue as v ->
        let skipGen =
            (try v.IsCompilerGenerated with _ -> true)
            && not v.IsPropertyGetterMethod
            && not v.IsPropertySetterMethod
            && not v.IsConstructor

        if skipGen then
            None
        elif v.IsConstructor then
            match DeclarationChecker.SymbolKey symbol with
            | Some ck ->
                Some
                    { OwnKey = ck
                      CallKey = ck
                      IsCtor = true
                      IsEntityFallback = false
                      Landed = landedOf fallbackPath symbol }
            | None -> None
        else
            let isCallable =
                v.IsProperty
                || v.IsPropertyGetterMethod
                || v.IsPropertySetterMethod
                || v.IsEvent
                || v.IsMember
                || v.IsExtensionMember
                || v.IsOverrideOrExplicitInterfaceImplementation
                || (try v.CurriedParameterGroups.Count > 0 with _ -> false)
                || (try v.FullType.IsFunctionType with _ -> false)

            if not isCallable then
                None
            else
                match SymbolUses.keyOf symbol with
                | Some raw ->
                    let k = withoutSetter raw

                    Some
                        { OwnKey = k
                          CallKey = k
                          IsCtor = false
                          IsEntityFallback = false
                          Landed = landedOf fallbackPath symbol }
                | None -> None
    | :? FSharpEntity as rawEnt ->
        match unwrapEntity 0 rawEnt with
        | None -> None
        | Some e ->
            let isInterfaceOrEnum = try e.IsInterface || e.IsEnum with _ -> true

            if isInterfaceOrEnum then
                None
            else
                let primaryCtorOpt =
                    try
                        e.MembersFunctionsAndValues
                        |> Seq.tryFind (fun m -> m.IsConstructor && not (try m.IsCompilerGenerated with _ -> false))
                    with _ ->
                        None

                match primaryCtorOpt with
                | Some ctor ->
                    match DeclarationChecker.SymbolKey(ctor :> FSharpSymbol) with
                    | Some ck ->
                        Some
                            { OwnKey = ck
                              CallKey = ck
                              IsCtor = true
                              IsEntityFallback = false
                              Landed = landedOf fallbackPath (ctor :> FSharpSymbol) |> Option.orElse (landedOf fallbackPath (e :> FSharpSymbol)) }
                    | None -> None
                | None ->
                    let isRecOrUnion = try e.IsFSharpRecord || e.IsFSharpUnion with _ -> false

                    if isRecOrUnion then
                        match SymbolUses.keyOf(e :> FSharpSymbol) with
                        | Some ek ->
                            Some
                                { OwnKey = ek
                                  CallKey = ek
                                  IsCtor = false
                                  IsEntityFallback = true
                                  Landed = landedOf fallbackPath (e :> FSharpSymbol) }
                        | None -> None
                    else
                        None
    | _ -> None

let private upstreamClosure (roots: ProjectGraph list) (host: GraphHost) : ProjectGraph list =
    let visited = HashSet<ProjectGraph>(HashIdentity.Reference)
    let acc = ResizeArray<ProjectGraph>()

    let rec visit (g: ProjectGraph) =
        if obj.ReferenceEquals(g.Host, host) && visited.Add g then
            acc.Add g

            for r in g.References do
                visit r

    for r in roots do
        visit r

    List.ofSeq acc

let private definersFor (ownersOf: string -> ProjectGraph list) (fallbackGraph: ProjectGraph) (landedOpt: string option) : ProjectGraph list =
    let fromLanded =
        match landedOpt with
        | Some file ->
            let impl =
                if file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                    Path.ChangeExtension(file, ".fs")
                else
                    file

            let sigFile = Path.ChangeExtension(file, ".fsi")
            (ownersOf file @ ownersOf impl @ ownersOf sigFile) |> List.distinct
        | None -> []

    match fromLanded with
    | [] -> upstreamClosure [ fallbackGraph ] fallbackGraph.Host
    | ds -> ds |> List.filter (fun g -> obj.ReferenceEquals(g.Host, fallbackGraph.Host))

let private tryResolveInTurnFiles
    (preferFsiPath: string option)
    (target: CallableTarget)
    (projects: (ProjectGraph * TurnFile[])[])
    : CallHierarchyItem option =
    let landedFallback = target.Landed

    let tryFromUses (filePath: string) (uses: FileUses) =
        let landed = defaultArg landedFallback filePath

        if target.IsEntityFallback then
            match uses.TypeOf target.OwnKey with
            | ValueSome t -> Some(makeItemFromType filePath landed t)
            | ValueNone -> None
        else
            match uses.CallableOf(target.OwnKey, true) with
            | ValueSome c when c.Glyph <> SymbolGlyph.Field -> Some(makeItemFromCallable filePath landed target.IsCtor c)
            | _ -> None

    let mutable fromPreferredFsi = None
    let mutable fromFs = None
    let mutable fromAnyFsi = None

    for _, turnFiles in projects do
        for tf in turnFiles do
            match tf with
            | TurnFile.Signature(filePath, _, uses) ->
                match preferFsiPath with
                | Some p when sameFile filePath p && fromPreferredFsi.IsNone ->
                    fromPreferredFsi <- tryFromUses filePath uses
                | _ -> ()

                if fromAnyFsi.IsNone then
                    fromAnyFsi <- tryFromUses filePath uses
            | TurnFile.Indexed(filePath, _, uses) ->
                if fromFs.IsNone then
                    fromFs <- tryFromUses filePath uses
            | TurnFile.Skipped _ -> ()

    fromPreferredFsi |> Option.orElse fromFs |> Option.orElse fromAnyFsi

let prepare
    (ownersOf: string -> ProjectGraph list)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (ct: CancellationToken)
    : Task<CallHierarchyItem[] option> =
    task {
        let isAskingFsi = path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

        let! targetOpt =
            if isAskingFsi then
                graph.ReadSignature(
                    path,
                    (fun view -> Hover.lookupOf view.Text line character),
                    (fun _ lookup _ sigUses ->
                        let endColOpt = lookup |> Option.map (fun (_, _, ec) -> ec)

                        match Definition.pickSignatureUse line character endColOpt sigUses with
                        | Some(struct (_, _, symbol)) -> extractTarget (Some path) symbol
                        | None -> None),
                    ct
                )
            else
                graph.ReadFile(
                    path,
                    (fun view -> Hover.lookupOf view.Text line character),
                    (fun view lookup ->
                        match lookup with
                        | None -> None
                        | Some(lineStr, names, endColumn) ->
                            let check = if view.Whole then view.Result.CheckResultsNotKept else view.Result.CheckResults

                            match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
                            | Some u -> extractTarget None u.Symbol
                            | None -> None),
                    ct
                )

        match targetOpt with
        | None -> return None
        | Some target ->
            let definers = definersFor ownersOf graph target.Landed

            let turnScope () : TurnScope<unit> =
                { Loading = false
                  Projects = [| for p in definers -> p, Array.toList p.FilesOf |]
                  Name = None
                  Extra = () }

            match! graph.ReadInOneTurn(turnScope, ct) with
            | TurnRead.Gone -> return None
            | TurnRead.Read((), projects) ->
                let preferFsi = if isAskingFsi then Some path else None

                match tryResolveInTurnFiles preferFsi target projects with
                | Some item -> return Some [| item |]
                | None -> return None
    }

let private resolveItemData
    (ownersOf: string -> ProjectGraph list)
    (graph: ProjectGraph)
    (item: CallHierarchyItem)
    (ct: CancellationToken)
    : Task<(string * string * bool * bool * string * string) option> =
    task {
        match tryParseData item with
        | Some parsed -> return Some parsed
        | None ->
            let itemPath = Paths.ofUri item.Uri
            let! prepared = prepare ownersOf graph itemPath (int item.SelectionRange.Start.Line) (int item.SelectionRange.Start.Character) ct

            return
                prepared
                |> Option.bind Array.tryHead
                |> Option.bind tryParseData
    }

let incoming
    (scope: References.Scope)
    (hostGraph: ProjectGraph)
    (item: CallHierarchyItem)
    (ct: CancellationToken)
    : Task<CallHierarchyIncomingCall[] option> =
    task {
        let! dataOpt = resolveItemData scope.OwnersOf hostGraph item ct

        match dataOpt with
        | None -> return Some [||]
        | Some(_ownKey, callKey, isCtor, _isEntity, declPath, landedPath) ->
            let definersNow () =
                let impl =
                    if landedPath.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                        Path.ChangeExtension(landedPath, ".fs")
                    else
                        landedPath

                let sigFile = Path.ChangeExtension(landedPath, ".fsi")
                (scope.OwnersOf landedPath @ scope.OwnersOf impl @ scope.OwnersOf sigFile) |> List.distinct

            let turnScope () : TurnScope<unit> =
                if scope.IsLoading() then
                    { Loading = true
                      Projects = [||]
                      Name = None
                      Extra = () }
                else
                    let definers = definersNow ()

                    let asking =
                        match scope.OwnersOf declPath with
                        | g :: _ -> g
                        | [] -> hostGraph

                    let projects =
                        match definers with
                        | [] -> scope.Projects()
                        | ds -> ds @ (ds |> List.collect scope.ReferencingProjects)
                        |> fun found -> asking :: found
                        |> List.filter (fun p -> obj.ReferenceEquals(p.Host, hostGraph.Host))
                        |> List.distinct

                    { Loading = false
                      Projects = [| for p in projects -> p, scope.FilesOf p |]
                      Name = None
                      Extra = () }

            match! hostGraph.ReadInOneTurn(turnScope, ct) with
            | TurnRead.Gone -> return None
            | TurnRead.Read((), projects) ->
                let seenFiles = HashSet<string>(StringComparer.OrdinalIgnoreCase)
                let grouped = Dictionary<struct (string * string), CallHierarchyItem * ResizeArray<LspRange>>()

                for _, turnFiles in projects do
                    for tf in turnFiles do
                        match tf with
                        | TurnFile.Indexed(filePath, _, uses) ->
                            let norm = normPath filePath

                            if seenFiles.Add norm then
                                let rawCallRanges =
                                    if isCtor then
                                        uses.ConstructorCallsOf callKey
                                    else
                                        let ownHits = (uses.Of callKey).Hits
                                        let setterHits = (uses.Of(callKey + setterSuffix)).Hits

                                        [| for struct (r, isDef) in ownHits do
                                               if not isDef then
                                                   yield r
                                           for struct (r, isDef) in setterHits do
                                               if not isDef then
                                                   yield r |]

                                for hitRange in rawCallRanges do
                                    if sameFile hitRange.FileName filePath then
                                        match uses.EnclosingCallable hitRange.Start with
                                        | ValueSome callerDecl when not (Range.equals hitRange callerDecl.SelectionRange) ->
                                            let groupKey = struct (norm.ToUpperInvariant(), callerDecl.Key)
                                            let lspHit = toLspRange hitRange

                                            match grouped.TryGetValue groupKey with
                                            | true, (_, ranges) -> ranges.Add lspHit
                                            | _ ->
                                                let callerItem =
                                                    makeItemFromCallable
                                                        filePath
                                                        filePath
                                                        (callerDecl.Glyph = SymbolGlyph.Constructor)
                                                        callerDecl

                                                let ranges = ResizeArray<LspRange>()
                                                ranges.Add lspHit
                                                grouped[groupKey] <- (callerItem, ranges)
                                        | _ -> ()
                        | _ -> ()

                let results =
                    [| for KeyValue(_, (callerItem, ranges)) in grouped do
                           let deduped =
                               ranges
                               |> Seq.distinct
                               |> Seq.toArray

                           Array.sortInPlaceWith compareLspRange deduped

                           yield
                               { From = callerItem
                                 FromRanges = deduped } |]

                Array.sortInPlaceWith
                    (fun (a: CallHierarchyIncomingCall) (b: CallHierarchyIncomingCall) ->
                        let cUri = String.Compare(a.From.Uri, b.From.Uri, StringComparison.OrdinalIgnoreCase)

                        if cUri <> 0 then
                            cUri
                        else
                            compareLspRange a.From.SelectionRange b.From.SelectionRange)
                    results

                return Some results
    }

[<NoComparison; NoEquality>]
type private RawOutgoingHit =
    { Target: CallableTarget
      FromRange: LspRange }

let outgoing
    (ownersOf: string -> ProjectGraph list)
    (graph: ProjectGraph)
    (item: CallHierarchyItem)
    (ct: CancellationToken)
    : Task<CallHierarchyOutgoingCall[] option> =
    task {
        let! dataOpt = resolveItemData ownersOf graph item ct

        match dataOpt with
        | None -> return Some [||]
        | Some(ownKey, _callKey, _isCtor, isEntity, declPath, landedPath) ->
            if isEntity then
                return Some [||]
            else
                let candidateImplPath =
                    if declPath.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                        Path.ChangeExtension(declPath, ".fs")
                    elif landedPath.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                        Path.ChangeExtension(landedPath, ".fs")
                    else
                        declPath

                let implOwnerOpt =
                    match ownersOf candidateImplPath with
                    | g :: _ when g.Holds candidateImplPath -> Some(g, candidateImplPath)
                    | _ ->
                        if not (candidateImplPath.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) && graph.Holds candidateImplPath then
                            Some(graph, candidateImplPath)
                        else
                            None

                match implOwnerOpt with
                | None -> return Some [||]
                | Some(implGraph, implPath) ->
                    let! rawHits =
                        implGraph.ReadFile(
                            implPath,
                            (fun view -> view.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')),
                            (fun view lines ->
                                let check = if view.Whole then view.Result.CheckResultsNotKept else view.Result.CheckResults
                                let localUses = SymbolUses.ofCheckWithParse (box view.Result) (Some view.Parse.ParseTree) check ct

                                match localUses.CallableOf(ownKey, true) with
                                | ValueNone -> [||]
                                | ValueSome callerDecl ->
                                    let hits = ResizeArray<RawOutgoingHit>()

                                    for u in check.GetAllUsesOfAllSymbolsInFile ct do
                                        if
                                            not u.IsFromDefinition
                                            && not u.IsFromType
                                            && sameFile u.Range.FileName implPath
                                            && Range.rangeContainsRange callerDecl.Range u.Range
                                            && not (Range.equals u.Range callerDecl.SelectionRange)
                                        then
                                            let belongsToCaller =
                                                match localUses.EnclosingCallable u.Range.Start with
                                                | ValueSome enc ->
                                                    enc.Key = callerDecl.Key
                                                    || (callerDecl.Glyph = SymbolGlyph.Constructor && enc.Glyph = SymbolGlyph.Field)
                                                | ValueNone -> true

                                            if belongsToCaller then
                                                match extractTarget None u.Symbol with
                                                | Some callee when callee.Landed.IsSome ->
                                                    let fromLsp =
                                                        DocumentHighlight.stripQualification lines u.Range
                                                        |> Option.defaultWith (fun () -> toLspRange u.Range)

                                                    hits.Add
                                                        { Target = callee
                                                          FromRange = fromLsp }
                                                | _ -> ()

                                    hits.ToArray()),
                            ct
                        )

                    if rawHits.Length = 0 then
                        return Some [||]
                    else
                        let extraRoots =
                            rawHits
                            |> Array.collect (fun h ->
                                match h.Target.Landed with
                                | Some lp ->
                                    let impl = Path.ChangeExtension(lp, ".fs")
                                    let sigF = Path.ChangeExtension(lp, ".fsi")
                                    (ownersOf lp @ ownersOf impl @ ownersOf sigF) |> List.toArray
                                | None -> [||])
                            |> Array.toList

                        let targetProjects = upstreamClosure (implGraph :: extraRoots) implGraph.Host

                        let turnScope () : TurnScope<unit> =
                            { Loading = false
                              Projects = [| for p in targetProjects -> p, Array.toList p.FilesOf |]
                              Name = None
                              Extra = () }

                        match! implGraph.ReadInOneTurn(turnScope, ct) with
                        | TurnRead.Gone -> return None
                        | TurnRead.Read((), projects) ->
                            let grouped = Dictionary<struct (string * string), CallHierarchyItem * ResizeArray<LspRange>>()

                            for h in rawHits do
                                match tryResolveInTurnFiles None h.Target projects with
                                | None -> ()
                                | Some calleeItem ->
                                    let groupKey = struct (calleeItem.Uri.ToUpperInvariant(), h.Target.OwnKey)

                                    match grouped.TryGetValue groupKey with
                                    | true, (_, ranges) -> ranges.Add h.FromRange
                                    | _ ->
                                        let ranges = ResizeArray<LspRange>()
                                        ranges.Add h.FromRange
                                        grouped[groupKey] <- (calleeItem, ranges)

                            let results =
                                [| for KeyValue(_, (calleeItem, ranges)) in grouped do
                                       let deduped = ranges |> Seq.distinct |> Seq.toArray
                                       Array.sortInPlaceWith compareLspRange deduped

                                       yield
                                           { To = calleeItem
                                             FromRanges = deduped } |]

                            Array.sortInPlaceWith
                                (fun (a: CallHierarchyOutgoingCall) (b: CallHierarchyOutgoingCall) ->
                                    let c1 = compareLspRange a.FromRanges[0] b.FromRanges[0]

                                    if c1 <> 0 then
                                        c1
                                    else
                                        String.Compare(a.To.Name, b.To.Name, StringComparison.Ordinal))
                                results

                            return Some results
    }
