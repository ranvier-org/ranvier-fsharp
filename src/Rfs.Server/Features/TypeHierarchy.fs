module Rfs.Server.TypeHierarchy

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

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

let private serializeData (entityKey: string) (declPath: string) (landedPath: string) : JToken =
    let obj = JObject()
    obj["key"] <- JValue entityKey
    obj["path"] <- JValue declPath
    obj["landed"] <- JValue landedPath
    obj :> JToken

let private tryParseData (item: TypeHierarchyItem) : (string * string * string) option =
    match item.Data with
    | Some(:? JObject as obj) ->
        let str (name: string) =
            match obj[name] with
            | null -> None
            | tok ->
                let s = tok.Value<string>()
                if String.IsNullOrEmpty s then None else Some s

        match str "key", str "path" with
        | Some key, Some declPath ->
            let landed = str "landed" |> Option.defaultValue declPath
            Some(key, declPath, landed)
        | _ -> None
    | _ -> None

let private makeItem (declPath: string) (landedPath: string) (entry: TypeRelationEntry) : TypeHierarchyItem =
    let sel = toLspRange entry.SelectionRange
    let full = unionLspRanges (toLspRange entry.Range) sel

    { Name = entry.Name
      Kind = CallHierarchy.symbolGlyphToKind entry.Glyph
      Tags = None
      Detail = None
      Uri = Paths.toUri declPath
      Range = full
      SelectionRange = sel
      Data = Some(serializeData entry.Key declPath landedPath) }

let rec private unwrapType (depth: int) (ty: FSharpType) : FSharpEntity option =
    if depth > 16 then
        None
    else
        let isAbbr = try ty.IsAbbreviation with _ -> false

        if isAbbr then
            match (try Some ty.AbbreviatedType with _ -> None) with
            | Some inner -> unwrapType (depth + 1) inner
            | None -> None
        else
            let stripped = try ty.StripAbbreviations() with _ -> ty

            if (try stripped.HasTypeDefinition with _ -> false) then
                unwrapEntity (depth + 1) stripped.TypeDefinition
            elif (try ty.HasTypeDefinition with _ -> false) then
                unwrapEntity (depth + 1) ty.TypeDefinition
            else
                None

and private unwrapEntity (depth: int) (e: FSharpEntity) : FSharpEntity option =
    if depth > 16 then
        Some e
    elif (try e.IsNamespace || e.IsFSharpModule with _ -> true) then
        None
    elif (try e.IsFSharpAbbreviation with _ -> false) then
        match (try Some e.AbbreviatedType with _ -> None) with
        | Some ty -> unwrapType (depth + 1) ty
        | None -> None
    else
        Some e

let private extractEntityTarget (fallbackPath: string option) (symbol: FSharpSymbol) : (string * string option) option =
    let entOpt =
        match symbol with
        | :? FSharpEntity as e -> unwrapEntity 0 e
        | :? FSharpMemberOrFunctionOrValue as v when v.IsConstructor ->
            v.DeclaringEntity |> Option.bind (unwrapEntity 0)
        | _ -> None

    match entOpt with
    | None -> None
    | Some ent ->
        match SymbolUses.keyOf(ent :> FSharpSymbol) with
        | None -> None
        | Some key ->
            let landed =
                (ent :> FSharpSymbol).SignatureLocation
                |> Option.orElse (ent :> FSharpSymbol).DeclarationLocation
                |> Option.filter (fun r -> not (r.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)))
                |> Option.map _.FileName
                |> Option.orElse fallbackPath

            Some(key, landed)

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

let private definersOf (ownersOf: string -> ProjectGraph list) (fallbackGraph: ProjectGraph) (landedOpt: string option) : ProjectGraph list =
    let found =
        match landedOpt with
        | Some file ->
            let impl = Path.ChangeExtension(file, ".fs")
            let sigFile = Path.ChangeExtension(file, ".fsi")
            (ownersOf file @ ownersOf impl @ ownersOf sigFile)
            |> List.filter (fun g -> obj.ReferenceEquals(g.Host, fallbackGraph.Host))
            |> List.distinct
        | None -> []

    match found with
    | [] -> upstreamClosure [ fallbackGraph ] fallbackGraph.Host
    | ds -> ds

let private tryResolveEntityItem
    (preferFsiPath: string option)
    (entityKey: string)
    (landedFallback: string option)
    (projects: (ProjectGraph * TurnFile[])[])
    : TypeHierarchyItem option =
    let mutable fromPreferredFsi = None
    let mutable fromFs = None
    let mutable fromAnyFsi = None

    for _, turnFiles in projects do
        for tf in turnFiles do
            match tf with
            | TurnFile.Signature(filePath, _, uses) ->
                match uses.TypeOf entityKey with
                | ValueSome entry ->
                    let item = makeItem filePath (defaultArg landedFallback filePath) entry

                    match preferFsiPath with
                    | Some p when sameFile filePath p && fromPreferredFsi.IsNone ->
                        fromPreferredFsi <- Some item
                    | _ -> ()

                    if fromAnyFsi.IsNone then
                        fromAnyFsi <- Some item
                | ValueNone -> ()
            | TurnFile.Indexed(filePath, _, uses) ->
                if fromFs.IsNone then
                    match uses.TypeOf entityKey with
                    | ValueSome entry ->
                        fromFs <- Some(makeItem filePath (defaultArg landedFallback filePath) entry)
                    | ValueNone -> ()
            | TurnFile.Skipped _ -> ()

    fromPreferredFsi |> Option.orElse fromFs |> Option.orElse fromAnyFsi

let prepare
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (ct: CancellationToken)
    : Task<TypeHierarchyItem[] option> =
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
                        | Some(struct (_, _, symbol)) -> extractEntityTarget (Some path) symbol
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
                            let check =
                                if view.Whole then
                                    view.Result.CheckResultsNotKept
                                else
                                    view.CheckResultsAt(line + 1, endColumn)

                            match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
                            | Some u -> extractEntityTarget None u.Symbol
                            | None -> None),
                    ct
                )

        match targetOpt with
        | None -> return None
        | Some(entityKey, landedOpt) ->
            let turnScope () : TurnScope<unit> =
                if scope.IsLoading() then
                    { Loading = true
                      Projects = [||]
                      Name = None
                      Extra = () }
                else
                    let definers = definersOf scope.OwnersOf graph landedOpt

                    let extraAsking =
                        if definers |> List.exists (fun p -> obj.ReferenceEquals(p, graph)) then
                            [||]
                        else
                            [| graph, [] |]

                    { Loading = false
                      Projects = Array.append [| for p in definers -> p, scope.FilesOf p |] extraAsking
                      Name = None
                      Extra = () }

            match! graph.ReadInOneTurn(turnScope, ct) with
            | TurnRead.Gone -> return None
            | TurnRead.Read((), projects) ->
                let preferFsi = if isAskingFsi then Some path else None

                match tryResolveEntityItem preferFsi entityKey landedOpt projects with
                | Some item -> return Some [| item |]
                | None -> return None
    }

let private resolveItemData
    (scope: References.Scope)
    (graph: ProjectGraph)
    (item: TypeHierarchyItem)
    (ct: CancellationToken)
    : Task<(string * string * string) option> =
    task {
        match tryParseData item with
        | Some parsed -> return Some parsed
        | None ->
            let itemPath = Paths.ofUri item.Uri
            let! prepared = prepare scope graph itemPath (int item.SelectionRange.Start.Line) (int item.SelectionRange.Start.Character) ct

            return
                prepared
                |> Option.bind Array.tryHead
                |> Option.bind tryParseData
    }

let supertypes
    (scope: References.Scope)
    (graph: ProjectGraph)
    (item: TypeHierarchyItem)
    (ct: CancellationToken)
    : Task<TypeHierarchyItem[] option> =
    task {
        let! dataOpt = resolveItemData scope graph item ct

        match dataOpt with
        | None -> return Some [||]
        | Some(entityKey, declPath, landedPath) ->
            let turnScope () : TurnScope<unit> =
                if scope.IsLoading() then
                    { Loading = true
                      Projects = [||]
                      Name = None
                      Extra = () }
                else
                    let definers =
                        (definersOf scope.OwnersOf graph (Some landedPath)
                         @ definersOf scope.OwnersOf graph (Some declPath))
                        |> List.distinct

                    let searchProjects = upstreamClosure (definers @ [ graph ]) graph.Host

                    { Loading = false
                      Projects = [| for p in searchProjects -> p, scope.FilesOf p |]
                      Name = None
                      Extra = () }

            match! graph.ReadInOneTurn(turnScope, ct) with
            | TurnRead.Gone -> return None
            | TurnRead.Read((), projects) ->
                let superKeyList = ResizeArray<string>()
                let superSeen = HashSet<string>(StringComparer.Ordinal)

                // Collect immediate SuperKeys from both .fs and .fsi so hidden .fs interfaces/inheritance
                // on .fsi-signed types are discovered.
                for _, turnFiles in projects do
                    for tf in turnFiles do
                        let usesOpt =
                            match tf with
                            | TurnFile.Indexed(_, _, u)
                            | TurnFile.Signature(_, _, u) -> ValueSome u
                            | TurnFile.Skipped _ -> ValueNone

                        match usesOpt with
                        | ValueSome uses ->
                            match uses.TypeOf entityKey with
                            | ValueSome entry ->
                                for sk in entry.SuperKeys do
                                    if superSeen.Add sk then
                                        superKeyList.Add sk
                            | ValueNone -> ()
                        | ValueNone -> ()

                if superKeyList.Count = 0 then
                    return Some [||]
                else
                    let resolved = ResizeArray<TypeHierarchyItem>()

                    for sk in superKeyList do
                        match tryResolveEntityItem None sk None projects with
                        | Some superItem -> resolved.Add superItem
                        | None -> ()

                    let arr = resolved.ToArray()

                    Array.sortInPlaceWith
                        (fun (a: TypeHierarchyItem) (b: TypeHierarchyItem) ->
                            let cUri = String.Compare(a.Uri, b.Uri, StringComparison.OrdinalIgnoreCase)
                            if cUri <> 0 then cUri else compareLspRange a.SelectionRange b.SelectionRange)
                        arr

                    return Some arr
    }

let subtypes
    (scope: References.Scope)
    (hostGraph: ProjectGraph)
    (item: TypeHierarchyItem)
    (ct: CancellationToken)
    : Task<TypeHierarchyItem[] option> =
    task {
        let! dataOpt = resolveItemData scope hostGraph item ct

        match dataOpt with
        | None -> return Some [||]
        | Some(entityKey, declPath, landedPath) ->
            let definersNow () =
                let impl = Path.ChangeExtension(landedPath, ".fs")
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
                let seenTypeKeys = HashSet<string>(StringComparer.Ordinal)
                let results = ResizeArray<TypeHierarchyItem>()

                // First pass: .fs implementation files
                for _, turnFiles in projects do
                    for tf in turnFiles do
                        match tf with
                        | TurnFile.Indexed(filePath, _, uses) ->
                            let norm = normPath filePath

                            if seenFiles.Add norm then
                                for entry in uses.SubtypesOf entityKey do
                                    if seenTypeKeys.Add entry.Key then
                                        results.Add(makeItem filePath filePath entry)
                        | _ -> ()

                // Second pass: standalone .fsi files for any subtype not already emitted from .fs
                for _, turnFiles in projects do
                    for tf in turnFiles do
                        match tf with
                        | TurnFile.Signature(filePath, _, uses) ->
                            let norm = normPath filePath

                            if seenFiles.Add norm then
                                for entry in uses.SubtypesOf entityKey do
                                    if seenTypeKeys.Add entry.Key then
                                        results.Add(makeItem filePath filePath entry)
                        | _ -> ()

                let arr = results.ToArray()

                Array.sortInPlaceWith
                    (fun (a: TypeHierarchyItem) (b: TypeHierarchyItem) ->
                        let cUri = String.Compare(a.Uri, b.Uri, StringComparison.OrdinalIgnoreCase)
                        if cUri <> 0 then cUri else compareLspRange a.SelectionRange b.SelectionRange)
                    arr

                return Some arr
    }
