module Rfs.Server.CodeLens

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

let private isSignature (file: string) =
    file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

let inline private toLspRange (r: range) : LspRange =
    { Start =
        { Line = uint32 (max 0 (r.StartLine - 1))
          Character = uint32 (max 0 r.StartColumn) }
      End =
        { Line = uint32 (max 0 (r.EndLine - 1))
          Character = uint32 (max 0 r.EndColumn) } }

let private shouldEmitCodeLens (item: NavigationItem) : bool =
    if String.IsNullOrWhiteSpace item.LogicalName || Range.equals item.Range Range.range0 then
        false
    else
        match item.Glyph with
        | FSharpGlyph.Method
        | FSharpGlyph.OverridenMethod
        | FSharpGlyph.ExtensionMethod
        | FSharpGlyph.Delegate
        | FSharpGlyph.Variable
        | FSharpGlyph.Constant
        | FSharpGlyph.Property
        | FSharpGlyph.Field -> true
        | _ -> false

let private makeData (uri: string) (kind: string) (line: int) (char: int) : JToken =
    let o = JObject()
    o["uri"] <- JValue uri
    o["kind"] <- JValue kind
    o["line"] <- JValue line
    o["char"] <- JValue char
    o :> JToken

let private tryParseData (lens: CodeLens) : (string * string * int * int) option =
    match lens.Data with
    | Some(:? JObject as o) ->
        let uriTok = o["uri"]
        let kindTok = o["kind"]
        let lineTok = o["line"]
        let charTok = o["char"]

        if isNull uriTok || isNull kindTok || isNull lineTok || isNull charTok then
            None
        else
            try
                Some(uriTok.Value<string>(), kindTok.Value<string>(), lineTok.Value<int>(), charTok.Value<int>())
            with _ ->
                None
    | _ -> None

let private formatSymbolSignature (displayContext: FSharpDisplayContext) (symbol: FSharpSymbol) : string option =
    try
        match symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
            let groups = mfv.CurriedParameterGroups

            if groups.Count = 0 then
                Some(mfv.FullType.Format displayContext)
            else
                let formattedGroups =
                    [ for grp in groups ->
                          if grp.Count = 0 then
                              "unit"
                          else
                              grp
                              |> Seq.map (fun p ->
                                  let tStr = p.Type.Format displayContext

                                  match p.Name with
                                  | Some n when not (String.IsNullOrWhiteSpace n) -> $"%s{n}: %s{tStr}"
                                  | _ -> tStr)
                              |> String.concat " * " ]

                let retStr = mfv.ReturnParameter.Type.Format displayContext
                Some(String.concat " -> " (formattedGroups @ [ retStr ]))
        | :? FSharpField as fld -> Some(fld.FieldType.Format displayContext)
        | _ -> None
    with _ ->
        None

/// Non-blocking CodeLens generation over `ProjectGraph.LatestParse` for both `.fs` and `.fsi`.
let lenses
    (config: FSharpConfig)
    (graph: ProjectGraph)
    (path: string)
    (ct: CancellationToken)
    : Task<CodeLens[] option> =
    if
        String.Equals(config.LineLens.Enabled, "replaceCodeLens", StringComparison.OrdinalIgnoreCase)
        || (not config.CodeLenses.SignatureEnabled && not config.CodeLenses.ReferencesEnabled)
    then
        Task.FromResult(Some [||])
    else
        task {
            let! textView = graph.LatestParse(path, ct)
            let uri = Paths.toUri path
            let lines = textView.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')

            let findDeclColumn (decl: NavigationItem) (line0: int) (baseCol: int) : int =
                if line0 < 0 || line0 >= lines.Length || String.IsNullOrWhiteSpace decl.LogicalName then
                    baseCol
                else
                    let lineText = lines[line0]
                    let startSearch = min (max 0 baseCol) lineText.Length
                    let idx = lineText.IndexOf(decl.LogicalName, startSearch, StringComparison.Ordinal)
                    if idx >= 0 then idx else baseCol

            let items =
                StackGuard.onLargeStack (fun () ->
                    let nav = Navigation.getNavigation textView.Parse.ParseTree
                    let out = ResizeArray<CodeLens>()

                    let emitFor (decl: NavigationItem) =
                        if shouldEmitCodeLens decl then
                            let lspRange = toLspRange decl.Range
                            let line = max 0 (decl.Range.StartLine - 1)
                            let col = findDeclColumn decl line (max 0 decl.Range.StartColumn)

                            if config.CodeLenses.SignatureEnabled then
                                out.Add
                                    { Range = lspRange
                                      Command = None
                                      Data = Some(makeData uri "signature" line col) }

                            if config.CodeLenses.ReferencesEnabled then
                                out.Add
                                    { Range = lspRange
                                      Command = None
                                      Data = Some(makeData uri "references" line col) }

                    for top in nav.Declarations do
                        emitFor top.Declaration

                        for nested in top.Nested do
                            emitFor nested

                    out.ToArray())

            return Some items
        }

let private setterSuffix = "|setter"

let private withoutSetter (key: string) =
    if key.EndsWith(setterSuffix, StringComparison.Ordinal) then
        key.Substring(0, key.Length - setterSuffix.Length)
    else
        key

let private countReferencesAcrossWorkspace
    (scope: References.Scope)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (char: int)
    (ct: CancellationToken)
    : Task<range[]> =
    task {
        if not (isSignature path) then
            match! References.find scope graph path line char ct with
            | References.Answer.Moved -> return [||]
            | References.Answer.Found uses ->
                return
                    [| for struct (r, isDef) in uses do
                           if not isDef then
                               yield r |]
        else
            // `.fsi` signature CodeLens reference resolution: resolve symbol key via `ReadSignature`, then query `ReadInOneTurn` on the GraphHost thread.
            let! keyAndLanded =
                graph.ReadSignature(
                    path,
                    (fun view _ sigUses ->
                        let fcsLine = line + 1

                        let hitOpt =
                            match Hover.lookupOf view.Text line char with
                            | Some(_, names, endCol) ->
                                sigUses
                                |> Array.tryFind (fun (struct (r, _, _)) ->
                                    r.StartLine <= fcsLine
                                    && fcsLine <= r.EndLine
                                    && (r.EndLine <> fcsLine || r.EndColumn = endCol || (r.StartColumn <= char && char <= r.EndColumn)))
                                |> Option.map (fun h -> h, Some names)
                            | None -> None
                            |> Option.orElseWith (fun () ->
                                sigUses
                                |> Array.tryFind (fun (struct (r, isDef, _)) -> isDef && r.StartLine = fcsLine)
                                |> Option.map (fun h -> h, None))

                        match hitOpt with
                        | None -> None
                        | Some(struct (_, isDef, sym), namesOpt) ->
                            SymbolUses.keyOf sym
                            |> Option.map (fun key ->
                                let landed =
                                    if isDef then
                                        Some path
                                    else
                                        sym.SignatureLocation |> Option.orElse sym.DeclarationLocation |> Option.map _.FileName

                                let spelled =
                                    SymbolUses.spelledName sym
                                    |> Option.filter (fun n ->
                                        match namesOpt with
                                        | Some names -> List.tryLast names = Some n
                                        | None -> true)

                                key, landed, spelled)),
                    ct
                )

            match keyAndLanded with
            | None -> return [||]
            | Some(key, landed, spelled) ->
                let turnScope () : TurnScope<unit> =
                    if scope.IsLoading() then
                        { Loading = true
                          Projects = [||]
                          Name = None
                          Extra = () }
                    else
                        let definers =
                            match landed with
                            | Some f -> scope.OwnersOf f
                            | None -> []

                        let projects =
                            match definers with
                            | [] -> scope.Projects()
                            | defs -> defs @ (defs |> List.collect scope.ReferencingProjects)
                            |> fun ps -> graph :: ps
                            |> List.distinct

                        { Loading = false
                          Projects = [| for p in projects -> p, scope.FilesOf p |]
                          Name = if definers.IsEmpty then None else spelled
                          Extra = () }

                match! graph.ReadInOneTurn(turnScope, ct) with
                | TurnRead.Gone -> return [||]
                | TurnRead.Read((), projectFiles) ->
                    let plain = withoutSetter key

                    return
                        [| for _, turnFiles in projectFiles do
                               for tf in turnFiles do
                                   match tf with
                                   | TurnFile.Indexed(_, _, uses)
                                   | TurnFile.Signature(_, _, uses) ->
                                       for struct (r, isDef) in (uses.Of plain).Hits do
                                           if not isDef then
                                               yield r

                                       for struct (r, isDef) in (uses.Of(plain + setterSuffix)).Hits do
                                           if not isDef then
                                               yield r
                                   | TurnFile.Skipped _ -> () |]
                        |> Array.distinct
    }

let private resolveSignatureText (graph: ProjectGraph) (path: string) (line: int) (char: int) (ct: CancellationToken) : Task<string option> =
    if isSignature path then
        graph.ReadSignature(
            path,
            (fun view _ sigUses ->
                let fcsLine = line + 1

                let hit =
                    match Hover.lookupOf view.Text line char with
                    | Some(_, _, endCol) ->
                        sigUses
                        |> Array.tryFind (fun (struct (r, _, _)) ->
                            r.StartLine <= fcsLine
                            && fcsLine <= r.EndLine
                            && (r.EndLine <> fcsLine || r.EndColumn = endCol || (r.StartColumn <= char && char <= r.EndColumn)))
                    | None -> None
                    |> Option.orElseWith (fun () ->
                        sigUses |> Array.tryFind (fun (struct (r, isDef, _)) -> isDef && r.StartLine = fcsLine))

                match hit with
                | None -> None
                | Some(struct (_, _, sym)) ->
                    formatSymbolSignature FSharpDisplayContext.Empty sym
                    |> Option.map (fun s -> s.Replace("Microsoft.FSharp.Core.", "", StringComparison.Ordinal))),
            ct
        )
    else
        graph.ReadFile(
            path,
            (fun view -> Hover.lookupOf view.Text line char),
            (fun view lookup ->
                match lookup with
                | None -> None
                | Some(lineStr, names, endCol) ->
                    let check =
                        if view.Whole then
                            view.Result.CheckResultsNotKept
                        else
                            view.CheckResultsAt(line + 1, endCol)

                    match check.GetSymbolUseAtLocation(line + 1, endCol, lineStr, names) with
                    | Some u ->
                        formatSymbolSignature u.DisplayContext u.Symbol
                        |> Option.orElseWith (fun () ->
                            let (ToolTipText elems) = check.GetToolTip(line + 1, endCol, lineStr, names, FSharp.Compiler.Tokenization.FSharpTokenTag.Identifier)

                            elems
                            |> List.collect (function ToolTipElement.Group items -> items | _ -> [])
                            |> List.tryHead
                            |> Option.map (fun item -> ((item.MainDescription.Text.Split '\n')[0]).Trim()))
                    | None -> None),
            ct
        )

let private locationJson (r: range) : JToken =
    let loc = JObject()
    loc["uri"] <- JValue(Paths.toUri r.FileName)
    let rng = JObject()
    let s = JObject()
    s["line"] <- JValue(max 0 (r.StartLine - 1))
    s["character"] <- JValue(max 0 r.StartColumn)
    let e = JObject()
    e["line"] <- JValue(max 0 (r.EndLine - 1))
    e["character"] <- JValue(max 0 r.EndColumn)
    rng["start"] <- s
    rng["end"] <- e
    loc["range"] <- rng
    loc :> JToken

let resolve
    (scope: References.Scope)
    (ownersOf: string -> ProjectGraph list)
    (lens: CodeLens)
    (ct: CancellationToken)
    : Task<CodeLens> =
    task {
        match tryParseData lens with
        | None ->
            return
                { lens with
                    Command = Some { Title = ""; Command = ""; Arguments = None } }
        | Some(uri, kind, line, char) ->
            let path = Paths.ofUri uri

            match ownersOf path with
            | [] ->
                return
                    { lens with
                        Command = Some { Title = ""; Command = ""; Arguments = None } }
            | graph :: _ ->
                if kind = "signature" then
                    let! sigOpt = resolveSignatureText graph path line char ct
                    let title = sigOpt |> Option.defaultValue ""

                    return
                        { lens with
                            Command = Some { Title = title; Command = ""; Arguments = None } }
                elif kind = "references" || kind = "reference" then
                    let! nonDefRanges = countReferencesAcrossWorkspace scope graph path line char ct

                    let cmd =
                        if nonDefRanges.Length = 0 then
                            { Title = "0 References"
                              Command = ""
                              Arguments = None }
                        else
                            let posJson = JObject(JProperty("line", line), JProperty("character", char)) :> JToken
                            let locsJson = JArray(nonDefRanges |> Array.map locationJson) :> JToken

                            { Title = $"%d{nonDefRanges.Length} References"
                              Command = "fsharp.showReferences"
                              Arguments = Some [| JValue uri :> JToken; posJson; locsJson |] }

                    return { lens with Command = Some cmd }
                else
                    return
                        { lens with
                            Command = Some { Title = ""; Command = ""; Arguments = None } }
    }
