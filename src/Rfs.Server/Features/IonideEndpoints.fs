namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

/// Request payload for `fsharp/project` and `fsharp/lineLens` (`ProjectParms` in FSAC).
type ProjectParms = { Project: TextDocumentIdentifier }

/// Request payload for `fsharp/pipelineHint` (`FSharpPipelineHintRequest` in FSAC).
type PipelineHintRequest = { TextDocument: TextDocumentIdentifier }

/// Request payload for `fsharp/documentationSymbol` (`DocumentationForSymbolRequest` in FSAC).
type DocumentationForSymbolRequest =
    { XmlSig: string
      Assembly: string }

module IonideEndpoints =
    let private isSignature (file: string) =
        file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

    let private sameFile (a: string) (b: string) =
        if String.IsNullOrWhiteSpace a || String.IsNullOrWhiteSpace b then
            false
        else
            try
                String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)
            with _ ->
                String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    let private envelope (kind: string) (data: JsonNode) : string =
        let o = JsonObject()
        o["Kind"] <- JsonValue.Create kind
        o["Data"] <- data
        o.ToJsonString()

    let private strArray (xs: string seq) : JsonArray =
        JsonArray(xs |> Seq.map (fun s -> JsonValue.Create s :> JsonNode) |> Array.ofSeq)

    let private glyphIcon (g: FSharpGlyph) : string * string =
        match g with
        | FSharpGlyph.Class -> "Class", "C"
        | FSharpGlyph.Constant -> "Constant", "Cn"
        | FSharpGlyph.Delegate -> "Delegate", "D"
        | FSharpGlyph.Enum -> "Enum", "E"
        | FSharpGlyph.EnumMember -> "Property", "P"
        | FSharpGlyph.Event -> "Event", "e"
        | FSharpGlyph.Exception -> "Exception", "X"
        | FSharpGlyph.Field -> "Field", "F"
        | FSharpGlyph.Interface -> "Interface", "I"
        | FSharpGlyph.Method -> "Method", "M"
        | FSharpGlyph.OverridenMethod -> "Method", "M"
        | FSharpGlyph.Module -> "Module", "N"
        | FSharpGlyph.NameSpace -> "Namespace", "N"
        | FSharpGlyph.Property -> "Property", "P"
        | FSharpGlyph.Struct -> "Struct", "S"
        | FSharpGlyph.Typedef -> "Class", "C"
        | FSharpGlyph.Type -> "Type", "T"
        | FSharpGlyph.TypeParameter -> "Type", "T"
        | FSharpGlyph.Union -> "Type", "T"
        | FSharpGlyph.Variable -> "Variable", "V"
        | FSharpGlyph.ExtensionMethod -> "Extension Method", "M"
        | FSharpGlyph.Error
        | _ -> "Error", "E"

    let private enclosingChar (k: NavigationEntityKind) : string =
        match k with
        | NavigationEntityKind.Namespace -> "N"
        | NavigationEntityKind.Module -> "M"
        | NavigationEntityKind.Class -> "C"
        | NavigationEntityKind.Exception -> "E"
        | NavigationEntityKind.Interface -> "I"
        | NavigationEntityKind.Record -> "R"
        | NavigationEntityKind.Enum -> "En"
        | NavigationEntityKind.Union -> "D"

    let private fcsRangeJson (r: range) : JsonObject =
        let o = JsonObject()
        o["StartColumn"] <- JsonValue.Create(r.StartColumn + 1)
        o["StartLine"] <- JsonValue.Create r.StartLine
        o["EndColumn"] <- JsonValue.Create(r.EndColumn + 1)
        o["EndLine"] <- JsonValue.Create r.EndLine
        o

    let private declarationJson (filePath: string) (item: NavigationItem) : JsonObject =
        let glyph, glyphChar = glyphIcon item.Glyph
        let o = JsonObject()
        o["UniqueName"] <- JsonValue.Create item.UniqueName
        o["Name"] <- JsonValue.Create item.LogicalName
        o["Glyph"] <- JsonValue.Create glyph
        o["GlyphChar"] <- JsonValue.Create glyphChar
        o["IsTopLevel"] <- JsonValue.Create item.IsSingleTopLevel
        o["Range"] <- fcsRangeJson item.Range
        o["BodyRange"] <- fcsRangeJson item.BodyRange
        o["File"] <- JsonValue.Create filePath
        o["EnclosingEntity"] <- JsonValue.Create(enclosingChar item.EnclosingEntityKind)
        o["IsAbstract"] <- JsonValue.Create item.IsAbstract
        o

    let private trySymbolXmlDocSig (sym: FSharpSymbol) : string =
        try
            match sym with
            | :? FSharpEntity as e -> e.XmlDocSig
            | :? FSharpMemberOrFunctionOrValue as m -> m.XmlDocSig
            | :? FSharpField as f -> f.XmlDocSig
            | :? FSharpUnionCase as u -> u.XmlDocSig
            | :? FSharpActivePatternCase as ap -> ap.XmlDocSig
            | _ -> ""
        with _ ->
            ""

    let private trySymbolXmlDoc (sym: FSharpSymbol) : FSharpXmlDoc =
        try
            match sym with
            | :? FSharpEntity as e -> e.XmlDoc
            | :? FSharpMemberOrFunctionOrValue as m -> m.XmlDoc
            | :? FSharpField as f -> f.XmlDoc
            | :? FSharpUnionCase as u -> u.XmlDoc
            | :? FSharpActivePatternCase as ap -> ap.XmlDoc
            | _ -> FSharpXmlDoc.None
        with _ ->
            FSharpXmlDoc.None

    let private xmlKeyOf (sym: FSharpSymbol) : string =
        let raw = trySymbolXmlDocSig sym

        if not (String.IsNullOrWhiteSpace raw) then
            raw
        else
            try
                match sym with
                | :? FSharpEntity as e -> "T:" + e.FullName
                | :? FSharpMemberOrFunctionOrValue as m -> "M:" + m.FullName
                | :? FSharpField as f ->
                    match f.DeclaringEntity with
                    | Some e -> "F:" + e.FullName + "." + f.Name
                    | None -> "F:" + f.Name
                | :? FSharpUnionCase as uc -> "T:" + uc.ReturnType.TypeDefinition.FullName + "." + uc.Name
                | _ -> "S:" + sym.FullName
            with _ ->
                "S:" + sym.DisplayName

    let private formatConciseSymbolSignature (ctx: FSharpDisplayContext) (sym: FSharpSymbol) : string =
        try
            match sym with
            | :? FSharpMemberOrFunctionOrValue as mfv ->
                let kw =
                    if mfv.IsMember then "member" else "val"

                let tStr = mfv.FullType.Format ctx
                $"%s{kw} %s{mfv.DisplayName}: %s{tStr}"
            | :? FSharpEntity as ent -> $"type %s{ent.DisplayName}"
            | :? FSharpField as fld -> $"%s{fld.Name}: %s{fld.FieldType.Format ctx}"
            | :? FSharpUnionCase as uc -> uc.DisplayName
            | _ -> sym.DisplayName
            |> fun s -> s.Replace("Microsoft.FSharp.Core.", "", StringComparison.Ordinal)
        with _ ->
            sym.DisplayName

    let private extractPrecedingTripleSlashComment (text: string) (fcsDeclLine: int) : string option =
        let lines = text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')
        let mutable idx = fcsDeclLine - 2

        // Skip attributes `[<...>]` immediately above the declaration if present
        while idx >= 0 && lines[idx].TrimStart().StartsWith("[<", StringComparison.Ordinal) do
            idx <- idx - 1

        let collected = ResizeArray<string>()

        while idx >= 0 && lines[idx].TrimStart().StartsWith("///", StringComparison.Ordinal) do
            let trimmed = lines[idx].TrimStart()
            collected.Add(trimmed.Substring(3).Trim())
            idx <- idx - 1

        if collected.Count = 0 then
            None
        else
            collected.Reverse()
            let rawXml = String.concat "\n" collected
            let fakeDoc = FSharpXmlDoc.FromXmlText(FSharp.Compiler.Xml.XmlDoc(collected.ToArray(), Range.range0))
            Hover.docText fakeDoc |> Option.orElse (if String.IsNullOrWhiteSpace rawXml then None else Some rawXml)

    let private entityMembersArrays (ent: FSharpEntity) : string[] * string[] * string[] * string[] * string[] * string[] =
        try
            let ctors =
                [| for m in ent.MembersFunctionsAndValues do
                       if m.IsConstructor then
                           yield m.DisplayName |]

            let funcs =
                [| for m in ent.MembersFunctionsAndValues do
                       if not m.IsConstructor then
                           yield m.DisplayName |]

            let fields = [| for f in ent.FSharpFields -> f.Name |]

            let ifaces =
                [| for i in ent.DeclaredInterfaces do
                       if i.HasTypeDefinition then
                           yield i.TypeDefinition.DisplayName |]

            let attrs = [| for a in ent.Attributes -> a.AttributeType.DisplayName |]
            let nested = [| for n in ent.NestedEntities -> n.DisplayName |]
            ctors, fields, funcs, ifaces, attrs, nested
        with _ ->
            [||], [||], [||], [||], [||], [||]

    let private formatDocumentationJson
        (xmlKey: string)
        (signatureText: string)
        (commentText: string)
        (footerLines: string[])
        (symOpt: FSharpSymbol option)
        : string =
        let ctors, fields, funcs, ifaces, attrs, nested =
            match symOpt with
            | Some(:? FSharpEntity as ent) -> entityMembersArrays ent
            | _ -> [||], [||], [||], [||], [||], [||]

        let data = JsonObject()
        data["XmlKey"] <- JsonValue.Create xmlKey
        data["Constructors"] <- strArray ctors
        data["Fields"] <- strArray fields
        data["Functions"] <- strArray funcs
        data["Interfaces"] <- strArray ifaces
        data["Attributes"] <- strArray attrs
        data["DeclaredTypes"] <- strArray nested
        data["Signature"] <- JsonValue.Create signatureText
        data["Comment"] <- JsonValue.Create commentText
        data["FooterLines"] <- strArray footerLines
        envelope "formattedDocumentation" data

    /// `fsharp/signature` -> `{"Kind":"typesig","Data":"..."}`
    let signature
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<string option> =
        if isSignature path then
            graph.ReadSignature(
                path,
                (fun view _ sigUses ->
                    match Hover.lookupOf view.Text line character with
                    | None -> None
                    | Some(_, _, endCol) ->
                        let fcsLine = line + 1

                        let hit =
                            sigUses
                            |> Array.tryFind (fun (struct (r, _, _)) ->
                                r.StartLine <= fcsLine
                                && fcsLine <= r.EndLine
                                && (r.EndLine <> fcsLine || r.EndColumn = endCol || (r.StartColumn <= character && character <= r.EndColumn)))

                        hit
                        |> Option.map (fun (struct (_, _, sym)) ->
                            let sigStr = formatConciseSymbolSignature FSharpDisplayContext.Empty sym
                            envelope "typesig" (JsonValue.Create sigStr))),
                ct
            )
        else
            graph.ReadFile(
                path,
                (fun view -> Hover.lookupOf view.Text line character),
                (fun view lookup ->
                    match lookup with
                    | None -> None
                    | Some(lineStr, names, endCol) ->
                        let check =
                            if view.Whole then
                                view.Result.CheckResultsNotKept
                            else
                                view.CheckResultsAt(line + 1, endCol)

                        let (ToolTipText elems) = check.GetToolTip(line + 1, endCol, lineStr, names, FSharpTokenTag.Identifier)

                        let tipSig =
                            elems
                            |> List.collect (function ToolTipElement.Group items -> items | _ -> [])
                            |> List.tryHead
                            |> Option.map (fun item -> item.MainDescription.Text.Trim())
                            |> Option.filter (not << String.IsNullOrWhiteSpace)

                        let sigOpt =
                            tipSig
                            |> Option.orElseWith (fun () ->
                                check.GetSymbolUseAtLocation(line + 1, endCol, lineStr, names)
                                |> Option.map (fun u -> formatConciseSymbolSignature u.DisplayContext u.Symbol))

                        sigOpt |> Option.map (fun s -> envelope "typesig" (JsonValue.Create s))),
                ct
            )

    /// `fsharp/signatureData` -> `{"Kind":"signatureData","Data":{"OutputType":...,"Parameters":...,"Generics":...}}`
    let signatureData
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<string option> =
        let buildFromSymbol (rawCtx: FSharpDisplayContext) (sym: FSharpSymbol) : string option =
            try
                let ctx = rawCtx.WithPrefixGenericParameters()

                let renderSignatureData (outType: string) (parms: (string * string) list list) (generics: string list) : string =
                    let paramGroups =
                        JsonArray(
                            [| for grp in parms ->
                                   JsonArray(
                                       [| for pName, pType in grp ->
                                              let po = JsonObject()
                                              po["Name"] <- JsonValue.Create pName
                                              po["Type"] <- JsonValue.Create pType
                                              po :> JsonNode |]
                                   )
                                   :> JsonNode |]
                        )

                    let data = JsonObject()
                    data["OutputType"] <- JsonValue.Create outType
                    data["Parameters"] <- paramGroups
                    data["Generics"] <- strArray generics
                    envelope "signatureData" data

                match sym with
                | :? FSharpMemberOrFunctionOrValue as mfv ->
                    let outType =
                        if mfv.IsPropertySetterMethod then
                            mfv.CurriedParameterGroups
                            |> Seq.collect id
                            |> Seq.tryLast
                            |> Option.map (fun p -> p.Type.Format ctx)
                            |> Option.defaultWith (fun () -> mfv.ReturnParameter.Type.Format ctx)
                        else
                            mfv.ReturnParameter.Type.Format ctx

                    let isReadOnlyPropWithoutIndex =
                        mfv.IsProperty
                        && not mfv.HasSetterMethod
                        && (mfv.CurriedParameterGroups.Count = 0
                            || (mfv.CurriedParameterGroups.Count = 1 && mfv.CurriedParameterGroups[0].Count = 0))

                    if mfv.IsPropertyGetterMethod || isReadOnlyPropWithoutIndex then
                        Some(renderSignatureData outType [] [])
                    else
                        let effectiveSym =
                            if mfv.HasGetterMethod && mfv.HasSetterMethod then
                                mfv.SetterMethod
                            else
                                mfv

                        let parms =
                            [ for grp in effectiveSym.CurriedParameterGroups ->
                                  [ for (idx, p) in Seq.indexed grp ->
                                        let pName =
                                            if not (String.IsNullOrEmpty p.DisplayName) then
                                                p.DisplayName
                                            else
                                                p.Name |> Option.defaultWith (fun () -> $"arg%d{idx}")

                                        pName, p.Type.Format ctx ] ]

                        let generics =
                            [ for gp in effectiveSym.GenericParameters ->
                                  let n = gp.Name
                                  if n.StartsWith("'", StringComparison.Ordinal) then n.Substring 1 else n ]

                        match parms with
                        | [ [] ] when effectiveSym.IsMember && not effectiveSym.IsProperty && not effectiveSym.IsPropertyGetterMethod ->
                            Some(renderSignatureData outType [ [ ("unit", "unit") ] ] [])
                        | _ -> Some(renderSignatureData outType parms generics)
                | :? FSharpField as fld -> Some(renderSignatureData (fld.FieldType.Format ctx) [] [])
                | _ -> None
            with _ ->
                None

        if isSignature path then
            graph.ReadSignature(
                path,
                (fun view _ sigUses ->
                    match Hover.lookupOf view.Text line character with
                    | None -> None
                    | Some(_, _, endCol) ->
                        let fcsLine = line + 1

                        sigUses
                        |> Array.tryFind (fun (struct (r, _, _)) ->
                            r.StartLine <= fcsLine
                            && fcsLine <= r.EndLine
                            && (r.EndLine <> fcsLine || r.EndColumn = endCol || (r.StartColumn <= character && character <= r.EndColumn)))
                        |> Option.bind (fun (struct (_, _, sym)) -> buildFromSymbol FSharpDisplayContext.Empty sym)),
                ct
            )
        else
            graph.ReadFile(
                path,
                (fun view -> Hover.lookupOf view.Text line character),
                (fun view lookup ->
                    match lookup with
                    | None -> None
                    | Some(lineStr, names, endCol) ->
                        let check =
                            if view.Whole then
                                view.Result.CheckResultsNotKept
                            else
                                view.CheckResultsAt(line + 1, endCol)

                        check.GetSymbolUseAtLocation(line + 1, endCol, lineStr, names)
                        |> Option.bind (fun u -> buildFromSymbol u.DisplayContext u.Symbol)),
                ct
            )

    /// `fsharp/lineLens` -> non-blocking over `ProjectGraph.LatestParse(path, ct)`.
    let lineLens (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task<string option> =
        task {
            let! textView = graph.LatestParse(path, ct)
            let fullPath = Path.GetFullPath path

            let json =
                StackGuard.onLargeStack (fun () ->
                    let nav = Navigation.getNavigation textView.Parse.ParseTree

                    let arr =
                        JsonArray(
                            [| for top in nav.Declarations ->
                                   let item = JsonObject()
                                   item["Declaration"] <- declarationJson fullPath top.Declaration

                                   item["Nested"] <-
                                       JsonArray(
                                           [| for n in top.Nested -> declarationJson fullPath n :> JsonNode |]
                                       )

                                   item :> JsonNode |]
                        )

                    envelope "declarations" arr)

            return Some json
        }

    /// `fsharp/pipelineHint` -> `{"Kind":"pipelineHint","Data":[{"Line":...,"Types":[...],"PrecedingNonPipeExprLine":...}]}`
    let pipelineHint (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task<string option> =
        if isSignature path then
            Task.FromResult(Some(envelope "pipelineHint" (JsonArray())))
        else
            graph.ReadFile(
                path,
                (fun view -> view.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')),
                (fun view lines ->
                    let stages = InlineValue.collectPipelines view lines

                    let arr =
                        JsonArray(
                            [| for s in stages ->
                                   let o = JsonObject()
                                   o["Line"] <- JsonValue.Create s.StageLine
                                   o["Types"] <- strArray s.StageTypes

                                   o["PrecedingNonPipeExprLine"] <-
                                       match s.PrecedingNonPipeLine with
                                       | ValueSome ln -> JsonValue.Create ln :> JsonNode
                                       | ValueNone -> null

                                   o :> JsonNode |]
                        )

                    Some(envelope "pipelineHint" arr)),
                ct
            )

    /// `fsharp/f1Help` -> `{"Kind":"help","Data":"..."}`
    let f1Help
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<string option> =
        if isSignature path then
            graph.ReadSignature(
                path,
                (fun view _ sigUses ->
                    match Hover.lookupOf view.Text line character with
                    | None -> None
                    | Some(_, _, endCol) ->
                        let fcsLine = line + 1

                        sigUses
                        |> Array.tryFind (fun (struct (r, _, _)) ->
                            r.StartLine <= fcsLine
                            && fcsLine <= r.EndLine
                            && (r.EndLine <> fcsLine || r.EndColumn = endCol || (r.StartColumn <= character && character <= r.EndColumn)))
                        |> Option.map (fun (struct (_, _, sym)) -> envelope "help" (JsonValue.Create sym.FullName))),
                ct
            )
        else
            graph.ReadFile(
                path,
                (fun view -> Hover.lookupOf view.Text line character),
                (fun view lookup ->
                    match lookup with
                    | None -> None
                    | Some(lineStr, names, endCol) ->
                        let check =
                            if view.Whole then
                                view.Result.CheckResultsNotKept
                            else
                                view.CheckResultsAt(line + 1, endCol)

                        let kwOpt =
                            check.GetF1Keyword(line + 1, endCol, lineStr, names)
                            |> Option.orElseWith (fun () ->
                                check.GetSymbolUseAtLocation(line + 1, endCol, lineStr, names)
                                |> Option.map _.Symbol.FullName)

                        kwOpt |> Option.map (fun kw -> envelope "help" (JsonValue.Create kw))),
                ct
            )

    /// `fsharp/documentation` -> `{"Kind":"formattedDocumentation","Data":{...}}`
    let documentation
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<string option> =
        if isSignature path then
            graph.ReadSignature(
                path,
                (fun view _ sigUses ->
                    match Hover.lookupOf view.Text line character with
                    | None -> None
                    | Some(_, _, endCol) ->
                        let fcsLine = line + 1

                        sigUses
                        |> Array.tryFind (fun (struct (r, _, _)) ->
                            r.StartLine <= fcsLine
                            && fcsLine <= r.EndLine
                            && (r.EndLine <> fcsLine || r.EndColumn = endCol || (r.StartColumn <= character && character <= r.EndColumn)))
                        |> Option.map (fun (struct (r, _, sym)) ->
                            let sigText = formatConciseSymbolSignature FSharpDisplayContext.Empty sym

                            let comment =
                                Hover.docText (trySymbolXmlDoc sym)
                                |> Option.orElseWith (fun () -> extractPrecedingTripleSlashComment view.Text r.StartLine)
                                |> Option.defaultValue ""

                            let footer = [| $"Full name: %s{sym.FullName}" |]
                            formatDocumentationJson (xmlKeyOf sym) sigText comment footer (Some sym))),
                ct
            )
        else
            let rec attempt () =
                task {
                    let! step1 =
                        graph.ReadFile(
                            path,
                            (fun view -> Hover.lookupOf view.Text line character),
                            (fun view lookup ->
                                match lookup with
                                | None -> None
                                | Some(lineStr, names, endCol) ->
                                    let check =
                                        if view.Whole then
                                            view.Result.CheckResultsNotKept
                                        else
                                            view.CheckResultsAt(line + 1, endCol)

                                    let parts = check.GetToolTipParts(line + 1, endCol, lineStr, names, FSharpTokenTag.Identifier)

                                    match check.GetSymbolUseAtLocation(line + 1, endCol, lineStr, names) with
                                    | None -> None
                                    | Some u ->
                                        let sigStr =
                                            let (ToolTipText elems), _ = parts

                                            elems
                                            |> List.collect (function ToolTipElement.Group items -> items | _ -> [])
                                            |> List.tryHead
                                            |> Option.map (fun i -> i.MainDescription.Text.Trim())
                                            |> Option.filter (not << String.IsNullOrWhiteSpace)
                                            |> Option.defaultWith (fun () -> formatConciseSymbolSignature u.DisplayContext u.Symbol)

                                        let xmlKey = xmlKeyOf u.Symbol
                                        let asmName = u.Symbol.Assembly.SimpleName
                                        let fullName =
                                            try
                                                u.Symbol.FullName
                                            with _ ->
                                                u.Symbol.DisplayName

                                        let footer =
                                            [| $"Full name: %s{fullName}"
                                               if not (String.IsNullOrWhiteSpace asmName) then
                                                   $"Assembly: %s{asmName}" |]

                                        let ctors, fields, funcs, ifaces, attrs, nested =
                                            match u.Symbol with
                                            | :? FSharpEntity as ent -> entityMembersArrays ent
                                            | _ -> [||], [||], [||], [||], [||], [||]

                                        let fallbackLocalComment =
                                            match u.Symbol.DeclarationLocation with
                                            | Some loc when sameFile loc.FileName path ->
                                                extractPrecedingTripleSlashComment view.Text loc.StartLine
                                            | _ -> None

                                        Some(parts, view.Identity, view.Docs, xmlKey, sigStr, footer, (ctors, fields, funcs, ifaces, attrs, nested), fallbackLocalComment)),
                            ct
                        )

                    match step1 with
                    | None -> return None
                    | Some(((_, sources) as parts), identity, docsOfAsking, xmlKey, sigStr, footer, memberArrays, fallbackLocalComment) ->
                        let held =
                            sources
                            |> List.choose id
                            |> List.collect (fun s -> s.Owners @ s.InheritOwners)
                            |> List.map _.FileName
                            |> List.filter (fun f -> f <> "" && not (sameFile f path))
                            |> List.distinctBy (fun f -> f.ToUpperInvariant())
                            |> List.choose (fun file ->
                                ownersOf file
                                |> List.tryFind (fun g -> g.Holds file && g.SharesHost graph)
                                |> Option.map (fun g -> g, file))
                            |> Array.ofList

                        let! otherDocsOpt =
                            if held.Length = 0 then
                                Task.FromResult(ValueSome [||])
                            else
                                graph.DocsInTurn(path, identity, held, ct)

                        match otherDocsOpt with
                        | ValueNone -> return! attempt ()
                        | ValueSome otherDocs ->
                            let docsMap = Dictionary<string, FileDocs>(StringComparer.OrdinalIgnoreCase)

                            for (_, file), d in Array.zip held otherDocs do
                                docsMap[file] <- d

                            docsMap[path] <- docsOfAsking

                            let commentOf =
                                FileDocs.CommentOf(fun file ->
                                    match docsMap.TryGetValue file with
                                    | true, d -> ValueSome d
                                    | _ -> ValueNone)

                            let (ToolTipText composedElems) = Hover.compose commentOf parts

                            let comment =
                                composedElems
                                |> List.collect (function ToolTipElement.Group items -> items | _ -> [])
                                |> List.tryPick (fun item -> Hover.docText item.XmlDoc)
                                |> Option.orElse fallbackLocalComment
                                |> Option.defaultValue ""

                            let ctors, fields, funcs, ifaces, attrs, nested = memberArrays
                            let data = JsonObject()
                            data["XmlKey"] <- JsonValue.Create xmlKey
                            data["Constructors"] <- strArray ctors
                            data["Fields"] <- strArray fields
                            data["Functions"] <- strArray funcs
                            data["Interfaces"] <- strArray ifaces
                            data["Attributes"] <- strArray attrs
                            data["DeclaredTypes"] <- strArray nested
                            data["Signature"] <- JsonValue.Create sigStr
                            data["Comment"] <- JsonValue.Create comment
                            data["FooterLines"] <- strArray footer
                            return Some(envelope "formattedDocumentation" data)
                }

            attempt ()

    let private tryFindSymbolByXmlKeyInEntities (xmlSig: string) (entities: IList<FSharpEntity>) : FSharpSymbol option =
        let rec searchEntity (ent: FSharpEntity) : FSharpSymbol option =
            if xmlKeyOf ent = xmlSig || (try ent.XmlDocSig = xmlSig with _ -> false) then
                Some(ent :> FSharpSymbol)
            else
                let inMembers =
                    ent.MembersFunctionsAndValues
                    |> Seq.tryFind (fun m -> xmlKeyOf m = xmlSig || (try m.XmlDocSig = xmlSig with _ -> false))
                    |> Option.map (fun m -> m :> FSharpSymbol)

                match inMembers with
                | Some s -> Some s
                | None ->
                    let inFields =
                        ent.FSharpFields
                        |> Seq.tryFind (fun f -> xmlKeyOf f = xmlSig || (try f.XmlDocSig = xmlSig with _ -> false))
                        |> Option.map (fun f -> f :> FSharpSymbol)

                    match inFields with
                    | Some s -> Some s
                    | None ->
                        let inCases =
                            ent.UnionCases
                            |> Seq.tryFind (fun u -> xmlKeyOf u = xmlSig || (try u.XmlDocSig = xmlSig with _ -> false))
                            |> Option.map (fun u -> u :> FSharpSymbol)

                        match inCases with
                        | Some s -> Some s
                        | None -> ent.NestedEntities |> Seq.tryPick searchEntity

        entities |> Seq.tryPick searchEntity

    let rec private unwrapExn (e: exn) : exn =
        match e with
        | :? AggregateException as a when a.InnerExceptions.Count = 1 -> unwrapExn a.InnerException
        | _ -> e

    let private searchSymbolInProjects
        (projects: ProjectGraph list)
        (xmlSig: string)
        (assembly: string)
        (ct: CancellationToken)
        : Task<string option> =
        task {
            if isNull xmlSig || String.IsNullOrWhiteSpace xmlSig || isNull assembly then
                return None
            else
                let mutable answer: string option = None

                for g in projects do
                    if answer.IsNone then
                        // Inspect the last `.fs` implementation file first so `PartialAssemblySignature` includes all earlier files.
                        let implFiles = g.FilesOf |> Array.filter (not << isSignature) |> Array.rev
                        let mutable checkedRefsForGraph = false

                        for implFile in implFiles do
                            if answer.IsNone then
                                let scanRefs = not checkedRefsForGraph
                                checkedRefsForGraph <- true

                                let! foundInFile =
                                    task {
                                        try
                                            return!
                                                g.ReadFile(
                                                    implFile,
                                                    ignore,
                                                    (fun view () ->
                                                        let check =
                                                            if view.Whole then
                                                                view.Result.CheckResultsNotKept
                                                            else
                                                                // On an open/promoted file, `CheckResultsAt(1, 0)` builds `PartialAssemblySignature`
                                                                // BEFORE line 1 (excluding declarations inside `implFile`). Check at the last line instead.
                                                                let textLines = view.Text.Split '\n'
                                                                let lastLine = max 1 textLines.Length
                                                                let lastCol = textLines[lastLine - 1].TrimEnd('\r').Length
                                                                view.CheckResultsAt(lastLine, lastCol)

                                                        let ownMatch =
                                                            try
                                                                tryFindSymbolByXmlKeyInEntities xmlSig check.PartialAssemblySignature.Entities
                                                            with _ ->
                                                                None

                                                        let symOpt =
                                                            match ownMatch with
                                                            | Some s -> Some s
                                                            | None when scanRefs ->
                                                                try
                                                                    let refs = check.ProjectContext.GetReferencedAssemblies()

                                                                    refs
                                                                    |> Seq.filter (fun a ->
                                                                        String.IsNullOrWhiteSpace assembly
                                                                        || String.Equals(a.SimpleName, assembly, StringComparison.OrdinalIgnoreCase))
                                                                    |> Seq.tryPick (fun a -> tryFindSymbolByXmlKeyInEntities xmlSig a.Contents.Entities)
                                                                with _ ->
                                                                    None
                                                            | None -> None

                                                        match symOpt with
                                                        | None -> None
                                                        | Some sym ->
                                                            let sigStr = formatConciseSymbolSignature FSharpDisplayContext.Empty sym

                                                            let comment =
                                                                Hover.docText (trySymbolXmlDoc sym)
                                                                |> Option.orElseWith (fun () ->
                                                                    match sym.DeclarationLocation with
                                                                    | Some loc when sameFile loc.FileName implFile ->
                                                                        extractPrecedingTripleSlashComment view.Text loc.StartLine
                                                                    | _ -> None)
                                                                |> Option.defaultValue ""

                                                            let asmName =
                                                                try
                                                                    sym.Assembly.SimpleName
                                                                with _ ->
                                                                    assembly

                                                            let fullName =
                                                                try
                                                                    sym.FullName
                                                                with _ ->
                                                                    sym.DisplayName

                                                            let footer =
                                                                [| $"Full name: %s{fullName}"
                                                                   if not (String.IsNullOrWhiteSpace asmName) then
                                                                       $"Assembly: %s{asmName}" |]

                                                            Some(formatDocumentationJson (xmlKeyOf sym) sigStr comment footer (Some sym))),
                                                    ct
                                                )
                                        with ex ->
                                            match unwrapExn ex with
                                            | :? ObjectDisposedException -> return None
                                            | :? FSharp.Compiler.Declarations.SupersededLineDirectivesException -> return None
                                            | :? OperationCanceledException when not ct.IsCancellationRequested -> return None
                                            | _ -> return raise ex
                                    }

                                if foundInFile.IsSome then
                                    answer <- foundInFile

                return answer
        }

    /// `fsharp/documentationSymbol` lookup: evaluates `loadingNow` and `projectsNow` strictly on the `GraphHost` thread.
    let documentationSymbol
        (host: GraphHost)
        (projectsNow: unit -> ProjectGraph list)
        (loadingNow: unit -> bool)
        (xmlSig: string)
        (assembly: string)
        (ct: CancellationToken)
        : Task<string option> =
        if isNull xmlSig || String.IsNullOrWhiteSpace xmlSig || isNull assembly then
            Task.FromResult None
        else
            task {
                let! projects =
                    host.WhenSettled(
                        (fun () ->
                            if loadingNow () then
                                ValueNone
                            else
                                ValueSome(projectsNow ())),
                        ct
                    )

                return! searchSymbolInProjects projects xmlSig assembly ct
            }

    let private extractParamNamesFromPat (pat: SynPat) : string list =
        let rec collect (p: SynPat) : string list =
            match p with
            | SynPat.Paren(pat = inner) -> collect inner
            | SynPat.Typed(pat = inner) -> collect inner
            | SynPat.Attrib(pat = inner) -> collect inner
            | SynPat.OptionalVal(ident = id) -> [ id.idText ]
            | SynPat.Named(ident = SynIdent(id, _)) ->
                if id.idText = "_" || id.idRange.IsSynthetic then [] else [ id.idText ]
            | SynPat.Tuple(elementPats = elems) -> elems |> List.collect collect
            | SynPat.Const(SynConst.Unit, _) -> []
            | _ -> []

        collect pat

    let private extractTyparNames (typarDecls: SynValTyparDecls option) : string list =
        match typarDecls with
        | None -> []
        | Some(SynValTyparDecls(typars = typarsOpt)) ->
            match typarsOpt with
            | None -> []
            | Some td ->
                let decls =
                    match td with
                    | SynTyparDecls.PostfixList(decls = ds) -> ds
                    | SynTyparDecls.PrefixList(decls = ds) -> ds
                    | SynTyparDecls.SinglePrefix(decl = d) -> [ d ]

                [ for SynTyparDecl(typar = SynTypar(ident, _, _)) in decls -> "'" + ident.idText ]

    let private tryFindDocTargetOnLine
        (parseTree: ParsedInput)
        (fcsTargetLine: int)
        : (int * string list * string list) option =
        StackGuard.onLargeStack (fun () ->
            let mutable found: (int * string list * string list) option = None
            let mutable alreadyDocumented = false

            let firstAttrLineOr (attrs: SynAttributes) (fallbackLine: int) =
                attrs
                |> List.collect _.Attributes
                |> List.tryHead
                |> Option.map _.Range.StartLine
                |> Option.defaultValue fallbackLine

            let extractSynTyparDecls (tpOpt: SynTyparDecls option) =
                match tpOpt with
                | None -> []
                | Some td ->
                    let ds =
                        match td with
                        | SynTyparDecls.PostfixList(decls = ds) -> ds
                        | SynTyparDecls.PrefixList(decls = ds) -> ds
                        | SynTyparDecls.SinglePrefix(decl = d) -> [ d ]

                    [ for SynTyparDecl(typar = SynTypar(id, _, _)) in ds -> "'" + id.idText ]

            let checkBinding (SynBinding(attributes = attrs; xmlDoc = xmlDoc; headPat = headPat) as b) =
                if found.IsNone && not alreadyDocumented then
                    let headerRange = b.RangeOfBindingWithoutRhs
                    let startLine = firstAttrLineOr attrs headerRange.StartLine

                    if startLine <= fcsTargetLine && fcsTargetLine <= headerRange.EndLine then
                        if not xmlDoc.IsEmpty then
                            alreadyDocumented <- true
                        else
                            let typars, paramNames =
                                match headPat with
                                | SynPat.LongIdent(typarDecls = tpOpt; argPats = SynArgPats.Pats pats) ->
                                    extractTyparNames tpOpt, (pats |> List.collect extractParamNamesFromPat)
                                | _ -> [], []

                            found <- Some(startLine, typars, paramNames)

            let rec walkMember (m: SynMemberDefn) =
                if found.IsNone && not alreadyDocumented then
                    match m with
                    | SynMemberDefn.Member(memberDefn = b) -> checkBinding b
                    | SynMemberDefn.LetBindings(bindings = bs) ->
                        for b in bs do
                            checkBinding b
                    | SynMemberDefn.AbstractSlot(
                        slotSig = SynValSig(
                            attributes = sAttrs
                            xmlDoc = sXmlDoc
                            explicitTypeParams = tp
                            arity = SynValInfo(curriedArgInfos, _)
                            range = r)) ->
                        let startLine = firstAttrLineOr sAttrs r.StartLine

                        if startLine <= fcsTargetLine && fcsTargetLine <= r.EndLine then
                            if not sXmlDoc.IsEmpty then
                                alreadyDocumented <- true
                            else
                                let pNames =
                                    [ for grp in curriedArgInfos do
                                          for SynArgInfo(ident = idOpt) in grp do
                                              match idOpt with
                                              | Some id when id.idText <> "_" -> yield id.idText
                                              | _ -> () ]

                                found <- Some(startLine, extractTyparNames (Some tp), pNames)
                    | SynMemberDefn.Interface(members = msOpt) ->
                        msOpt |> Option.iter (List.iter walkMember)
                    | _ -> ()

            let rec walkDecls (decls: SynModuleDecl list) =
                for d in decls do
                    if found.IsNone && not alreadyDocumented && d.Range.StartLine <= fcsTargetLine && fcsTargetLine <= d.Range.EndLine then
                        match d with
                        | SynModuleDecl.Let(bindings = bs) ->
                            for b in bs do
                                checkBinding b
                        | SynModuleDecl.NestedModule(decls = nested) -> walkDecls nested
                        | SynModuleDecl.Types(typeDefns = tds) ->
                            for SynTypeDefn(
                                    typeInfo = SynComponentInfo(attributes = tAttrs; xmlDoc = tXmlDoc; typeParams = tpOpt; range = compRange)
                                    typeRepr = repr
                                    members = ms
                                    range = tr) in tds do
                                let mutable headerEndLine = compRange.EndLine

                                match repr with
                                | SynTypeDefnRepr.ObjectModel(members = oms) ->
                                    for m in oms do
                                        match m with
                                        | SynMemberDefn.ImplicitCtor(range = ctorR) ->
                                            if ctorR.EndLine > headerEndLine then
                                                headerEndLine <- ctorR.EndLine
                                        | _ -> walkMember m
                                | _ -> ()

                                for m in ms do
                                    walkMember m

                                let tStartLine = firstAttrLineOr tAttrs (min tr.StartLine compRange.StartLine)

                                if found.IsNone && not alreadyDocumented && tStartLine <= fcsTargetLine && fcsTargetLine <= headerEndLine then
                                    if not tXmlDoc.IsEmpty then
                                        alreadyDocumented <- true
                                    else
                                        found <- Some(tStartLine, extractSynTyparDecls tpOpt, [])
                        | _ -> ()

            let rec walkSigDecls (decls: SynModuleSigDecl list) =
                for d in decls do
                    if found.IsNone && not alreadyDocumented && d.Range.StartLine <= fcsTargetLine && fcsTargetLine <= d.Range.EndLine then
                        match d with
                        | SynModuleSigDecl.Val(
                            valSig = SynValSig(
                                attributes = vAttrs
                                xmlDoc = vXmlDoc
                                explicitTypeParams = tp
                                arity = SynValInfo(curriedArgInfos, _)
                                range = r)) ->
                            let startLine = firstAttrLineOr vAttrs r.StartLine

                            if startLine <= fcsTargetLine && fcsTargetLine <= r.EndLine then
                                if not vXmlDoc.IsEmpty then
                                    alreadyDocumented <- true
                                else
                                    let pNames =
                                        [ for grp in curriedArgInfos do
                                              for SynArgInfo(ident = idOpt) in grp do
                                                  match idOpt with
                                                  | Some id when id.idText <> "_" -> yield id.idText
                                                  | _ -> () ]

                                    found <- Some(startLine, extractTyparNames (Some tp), pNames)
                        | SynModuleSigDecl.Types(types = sigTds) ->
                            for SynTypeDefnSig(
                                    typeInfo = SynComponentInfo(attributes = tAttrs; xmlDoc = tXmlDoc; typeParams = tpOpt; range = compRange)
                                    range = tr) in sigTds do
                                let tStartLine = firstAttrLineOr tAttrs (min tr.StartLine compRange.StartLine)

                                if found.IsNone && not alreadyDocumented && tStartLine <= fcsTargetLine && fcsTargetLine <= compRange.EndLine then
                                    if not tXmlDoc.IsEmpty then
                                        alreadyDocumented <- true
                                    else
                                        found <- Some(tStartLine, extractSynTyparDecls tpOpt, [])
                        | SynModuleSigDecl.NestedModule(moduleDecls = nested) -> walkSigDecls nested
                        | _ -> ()

            match parseTree with
            | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
                for SynModuleOrNamespace(decls = decls) in modules do
                    walkDecls decls
            | ParsedInput.SigFile(ParsedSigFileInput(contents = modules)) ->
                for SynModuleOrNamespaceSig(decls = decls) in modules do
                    walkSigDecls decls

            found)

    let internal documentationGeneratorWithCapabilities
        (canDocumentChanges: bool)
        (versionNow: string -> int option)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (_character: int)
        (ct: CancellationToken)
        : Task<WorkspaceEdit option> =
        task {
            let fullPath = Path.GetFullPath path
            let mutable capturedLspVersion: int option = None

            let! parsesOpt =
                graph.LatestParsesInOneTurn(
                    (fun () ->
                        capturedLspVersion <- versionNow fullPath
                        struct (false, [| graph, [ fullPath ] |])),
                    ct
                )

            let textViewOpt =
                match parsesOpt with
                | ValueNone -> None
                | ValueSome parsedProjects ->
                    parsedProjects
                    |> Seq.collect snd
                    |> Seq.tryPick (fun (fp, tv) -> if sameFile fp fullPath then Some tv else None)

            match textViewOpt with
            | None -> return None
            | Some textView ->
                let lines = textView.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')

                if line < 0 || line >= lines.Length then
                    return None
                else
                    match tryFindDocTargetOnLine textView.Parse.ParseTree (line + 1) with
                    | None -> return None
                    | Some(insertFcsLine, typars, paramNames) ->
                        let insertLine0 = max 0 (insertFcsLine - 1)
                        let targetLineText = if insertLine0 < lines.Length then lines[insertLine0] else ""
                        let indentLen = targetLineText.Length - targetLineText.TrimStart(' ').Length
                        let indent = String(' ', indentLen)

                        let docLines =
                            [ yield $"%s{indent}/// <summary></summary>"
                              for tp in typars do
                                  yield $"%s{indent}/// <typeparam name=\"%s{tp}\"></typeparam>"
                              for p in paramNames do
                                  yield $"%s{indent}/// <param name=\"%s{p}\"></param>"
                              yield $"%s{indent}/// <returns></returns>" ]

                        let newText = (String.concat "\n" docLines) + "\n"
                        let insertPos: Position = { Line = uint32 insertLine0; Character = 0u }
                        let edit: TextEdit = { Range = { Start = insertPos; End = insertPos }; NewText = newText }
                        let uri = Paths.toUri path

                        if canDocumentChanges then
                            let docId: OptionalVersionedTextDocumentIdentifier =
                                { Uri = uri; Version = capturedLspVersion }

                            return
                                Some
                                    { Changes = None
                                      DocumentChanges = Some [| U4.C1 { TextDocument = docId; Edits = [| U2.C1 edit |] } |]
                                      ChangeAnnotations = None }
                        else
                            return
                                Some
                                    { Changes = Some(Map.ofList [ uri, [| edit |] ])
                                      DocumentChanges = None
                                      ChangeAnnotations = None }
        }

    /// `fsharp/documentationGenerator` -> generates XML `///` documentation stub `WorkspaceEdit`.
    let documentationGenerator
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<WorkspaceEdit option> =
        documentationGeneratorWithCapabilities false (fun _ -> None) graph path line character ct

    /// `fsharp/compilerLocation` -> `{"Kind":"compilerlocation","Data":{"Fsc":...,"Fsi":...,"MSBuild":...,"SdkRoot":...}}`
    let compilerLocation () : string =
        let tryFindDotnetRoot () : string option =
            let envRoot = Environment.GetEnvironmentVariable "DOTNET_ROOT"

            if not (String.IsNullOrWhiteSpace envRoot) && Directory.Exists envRoot then
                Some envRoot
            else
                let exeName = if OperatingSystem.IsWindows() then "dotnet.exe" else "dotnet"
                let pathVar = Environment.GetEnvironmentVariable "PATH"

                if String.IsNullOrWhiteSpace pathVar then
                    None
                else
                    pathVar.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries)
                    |> Array.tryPick (fun dir ->
                        try
                            let candidate = Path.Combine(dir.Trim(), exeName)
                            if File.Exists candidate then Some(Path.GetFullPath(dir.Trim())) else None
                        with _ ->
                            None)

        let sdkRootOpt = tryFindDotnetRoot ()

        let latestSdkDirOpt =
            sdkRootOpt
            |> Option.bind (fun root ->
                let sdkBase = Path.Combine(root, "sdk")

                if Directory.Exists sdkBase then
                    try
                        Directory.GetDirectories sdkBase
                        |> Array.sortDescending
                        |> Array.tryHead
                    with _ ->
                        None
                else
                    None)

        let probeFile (dirOpt: string option) (relPaths: string list) : string option =
            dirOpt
            |> Option.bind (fun d ->
                relPaths
                |> List.tryPick (fun rel ->
                    let full = Path.Combine(d, rel)
                    if File.Exists full then Some full else None))

        let fscOpt = probeFile latestSdkDirOpt [ Path.Combine("FSharp", "fsc.dll"); Path.Combine("FSharp", "fsc.exe") ]
        let fsiOpt = probeFile latestSdkDirOpt [ Path.Combine("FSharp", "fsi.dll"); Path.Combine("FSharp", "fsi.exe") ]
        let msbuildOpt = probeFile latestSdkDirOpt [ "MSBuild.dll" ]

        let toNode (sOpt: string option) : JsonNode =
            match sOpt with
            | Some s -> JsonValue.Create s :> JsonNode
            | None -> null

        let data = JsonObject()
        data["Fsc"] <- toNode fscOpt
        data["Fsi"] <- toNode fsiOpt
        data["MSBuild"] <- toNode msbuildOpt
        data["SdkRoot"] <- toNode sdkRootOpt
        envelope "compilerlocation" data
