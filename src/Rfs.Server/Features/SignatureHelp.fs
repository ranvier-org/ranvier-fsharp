namespace Rfs.Server

open System
open System.Collections.Generic
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

/// <summary>
/// Signature help (<c>textDocument/signatureHelp</c>), supporting overloaded method/constructor calls
/// (<c>FindParameterLocations</c> + <c>GetMethods</c>), curried F# function applications across parameter groups,
/// pipeline saturation (<c>|&gt;</c>, <c>||&gt;</c>, <c>|||&gt;</c>), and live <c>///</c> XML doc updates via <c>DocsInTurn</c>.
/// </summary>
module SignatureHelp =

    [<NoComparison; NoEquality>]
    type private MethodPrep =
        { Names: string list
          LidEnd: pos
          LineText: string
          OpenParen: pos
          IsStaticArgTip: bool
          ArgumentIndex: int }

    [<NoComparison; NoEquality>]
    type private FuncAppPrep =
        { CalleeEnd: pos
          CalleeLineText: string
          CalleeNames: string list
          CalleeEndCol: int }

    [<NoComparison; NoEquality>]
    type private Prepared =
        { CaretPos: pos
          PrevNonWsChar: char voption
          Method: MethodPrep option
          FuncApp: FuncAppPrep option }

    [<NoComparison; NoEquality>]
    type private OverloadRaw =
        { Label: string
          FallbackDoc: string option
          Parameters: ParameterInformation[] }

    [<NoComparison; NoEquality>]
    type private ComputedRaw =
        { Overloads: OverloadRaw[]
          ActiveSignature: uint32 option
          ActiveParameter: uint32 option
          TipParts: Hover.Signature option
          Identity: obj
          AskingDocs: FileDocs }

    let private cleanParamDisplay (s: string) =
        let trimmed = s.Trim()
        if trimmed = "" then None else Some trimmed

    let private extractLabelAndFallbackDoc (ToolTipText elements) (xmlDoc: FSharpXmlDoc) : string * string option =
        let items =
            elements
            |> List.collect (function
                | ToolTipElement.Group g -> g
                | _ -> [])

        match items with
        | head :: _ ->
            let label = head.MainDescription.Text
            let doc = Hover.docText head.XmlDoc |> Option.orElseWith (fun () -> Hover.docText xmlDoc)
            label, doc
        | [] -> "", Hover.docText xmlDoc

    let private prepareAt (triggerChar: char option) (view: TextView) (line: int) (character: int) : Prepared option =
        let lines = view.Text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')

        if line < 0 || line >= lines.Length then
            None
        else
            let col = min (max 0 character) lines[line].Length
            let caretPos = Position.mkPos (line + 1) col

            let tryGetLine (p: pos) =
                if p.Line >= 1 && p.Line <= lines.Length then Some lines[p.Line - 1] else None

            let tryGetChar (p: pos) =
                match tryGetLine p with
                | Some l when p.Column >= 0 && p.Column < l.Length -> Some l[p.Column]
                | _ -> None

            let prevPos (p: pos) : pos option =
                if p.Column > 0 then
                    Some(Position.mkPos p.Line (p.Column - 1))
                else
                    let mutable prevLine = p.Line - 1
                    let mutable found = None

                    while found.IsNone && prevLine >= 1 do
                        let l = lines[prevLine - 1]
                        found <- Some(Position.mkPos prevLine l.Length)
                        prevLine <- prevLine - 1

                    found

            let tryGetPrevChar (p: pos) : (pos * char) option =
                let rec step curLine curCol =
                    if curCol > 0 then
                        let l = lines[curLine - 1]
                        let idx = min (curCol - 1) (l.Length - 1)

                        if idx >= 0 then
                            Some(Position.mkPos curLine idx, l[idx])
                        elif curLine > 1 then
                            step (curLine - 1) lines[curLine - 2].Length
                        else
                            None
                    elif curLine > 1 then
                        let prevLen = lines[curLine - 2].Length
                        // Represent newline between lines as '\n' so whitespace skipping continues cleanly
                        Some(Position.mkPos (curLine - 1) prevLen, '\n')
                    else
                        None

                step p.Line p.Column

            let nextPos (p: pos) : pos option =
                match tryGetLine p with
                | Some l when p.Column < l.Length -> Some(Position.mkPos p.Line (p.Column + 1))
                | Some _ when p.Line < lines.Length -> Some(Position.mkPos (p.Line + 1) 0)
                | _ -> None

            let prevNonWs =
                let rec skipWs (p: pos, ch: char) =
                    if Char.IsWhiteSpace ch then
                        match tryGetPrevChar p with
                        | Some next -> skipWs next
                        | None -> None
                    else
                        Some(p, ch)

                match tryGetPrevChar caretPos with
                | Some first -> skipWs first
                | None -> None

            StackGuard.onLargeStack (fun () ->
                let methodPrep =
                    match view.Parse.FindParameterLocations caretPos with
                    | None -> None
                    | Some paramLocations ->
                        let lidEnd = paramLocations.LongIdEndLocation

                        match tryGetLine lidEnd with
                        | None -> None
                        | Some lineText ->
                            let isStaticArgTip = (tryGetChar paramLocations.OpenParenLocation = Some '<')

                            let endPos =
                                let last = Array.last paramLocations.TupleEndLocations

                                if paramLocations.IsThereACloseParen then
                                    prevPos last |> Option.defaultValue last
                                else
                                    last

                            let startOfArgs = nextPos paramLocations.OpenParenLocation

                            let tupleEnds =
                                [| yield! Option.toList startOfArgs
                                   for i in 0 .. paramLocations.TupleEndLocations.Length - 2 do
                                       yield paramLocations.TupleEndLocations[i]
                                   yield endPos |]

                            let wrongTriggerPos =
                                match triggerChar with
                                | Some('<' | '(' | ',') ->
                                    not (tupleEnds |> Array.exists (fun lp -> lp.Column = caretPos.Column))
                                | _ -> false

                            if wrongTriggerPos then
                                None
                            else
                                let argumentIndex =
                                    tupleEnds
                                    |> Array.pairwise
                                    |> Array.map (fun (lp1, lp2) -> Range.mkRange "" lp1 lp2)
                                    |> Array.tryFindIndex (fun r -> Range.rangeContainsPos r caretPos)
                                    |> Option.defaultValue 0

                                Some
                                    { Names = paramLocations.LongId
                                      LidEnd = lidEnd
                                      LineText = lineText
                                      OpenParen = paramLocations.OpenParenLocation
                                      IsStaticArgTip = isStaticArgTip
                                      ArgumentIndex = argumentIndex }

                let funcAppPrep =
                    match prevNonWs, triggerChar with
                    | _, Some('(' | ',' | '<') -> None
                    | Some(_, (',' | '(' | '<')), _ -> None
                    | Some(nonWsPos, _), _ ->
                        // endOfPreviousIdentPos in FCS is exclusive end column (nonWsPos.Column + 1)
                        let prevIdentEnd = Position.mkPos nonWsPos.Line (nonWsPos.Column + 1)

                        let appSymEnd =
                            if view.Parse.IsPosContainedInApplication prevIdentEnd then
                                match view.Parse.TryRangeOfFunctionOrMethodBeingApplied prevIdentEnd with
                                | Some funcRange -> Some funcRange.End
                                | None -> None
                            else
                                // Only trigger bare curried call right after space/identifier when caret is separated by whitespace
                                if Position.posGt caretPos prevIdentEnd then Some prevIdentEnd else None

                        match appSymEnd with
                        | None -> None
                        | Some symEnd when symEnd.Line >= 1 && symEnd.Line <= lines.Length ->
                            let lookupCol = max 0 (symEnd.Column - 1)

                            match Hover.lookupOf view.Text (symEnd.Line - 1) lookupCol with
                            | Some(calleeLineStr, calleeNames, calleeEndCol) when not calleeNames.IsEmpty ->
                                Some
                                    { CalleeEnd = Position.mkPos symEnd.Line calleeEndCol
                                      CalleeLineText = calleeLineStr
                                      CalleeNames = calleeNames
                                      CalleeEndCol = calleeEndCol }
                            | _ -> None
                        | _ -> None
                    | None, _ -> None

                let prevChVOpt =
                    match prevNonWs with
                    | Some(_, c) -> ValueSome c
                    | None -> ValueNone

                if methodPrep.IsNone && funcAppPrep.IsNone then
                    None
                else
                    Some
                        { CaretPos = caretPos
                          PrevNonWsChar = prevChVOpt
                          Method = methodPrep
                          FuncApp = funcAppPrep })

    let private computeMethod (view: FileView) (wholeCheck: Lazy<FSharpCheckFileResults>) (mPrep: MethodPrep) : ComputedRaw option =
        let check =
            if view.Whole then
                wholeCheck.Value
            else
                view.CheckResultsAt(mPrep.LidEnd.Line, mPrep.LidEnd.Column)

        let methodGroup = check.GetMethods(mPrep.LidEnd.Line, mPrep.LidEnd.Column, mPrep.LineText, Some mPrep.Names)
        let methods = methodGroup.Methods

        if methods.Length = 0 || methodGroup.MethodName.EndsWith("> )", StringComparison.Ordinal) then
            None
        else
            let filtered =
                [| for m in methods do
                       if (mPrep.IsStaticArgTip && m.StaticParameters.Length > 0) || (not mPrep.IsStaticArgTip && m.HasParameters) then
                           yield m |]

            if filtered.Length = 0 then
                None
            else
                let activeOverload =
                    filtered
                    |> Array.tryFindIndex (fun m ->
                        let len = if mPrep.IsStaticArgTip then m.StaticParameters.Length else m.Parameters.Length
                        len >= mPrep.ArgumentIndex + 1)
                    |> Option.orElse (Some 0)
                    |> Option.map uint32

                let overloads =
                    filtered
                    |> Array.map (fun m ->
                        let label, fallbackDoc = extractLabelAndFallbackDoc m.Description m.XmlDoc
                        let rawParams = if mPrep.IsStaticArgTip then m.StaticParameters else m.Parameters

                        let parameters =
                            rawParams
                            |> Array.map (fun p ->
                                { ParameterInformation.Label = U2.C1 p.ParameterName
                                  Documentation = cleanParamDisplay p.Display.Text |> Option.map U2.C1 })

                        { Label = label
                          FallbackDoc = fallbackDoc
                          Parameters = parameters })

                let tipParts =
                    if mPrep.Names.IsEmpty then
                        None
                    else
                        Some(check.GetToolTipParts(mPrep.LidEnd.Line, mPrep.LidEnd.Column, mPrep.LineText, mPrep.Names, FSharpTokenTag.Identifier))

                Some
                    { Overloads = overloads
                      ActiveSignature = activeOverload
                      ActiveParameter = Some(uint32 mPrep.ArgumentIndex)
                      TipParts = tipParts
                      Identity = view.Identity
                      AskingDocs = view.Docs }

    let private computeFuncApp (view: FileView) (wholeCheck: Lazy<FSharpCheckFileResults>) (caretPos: pos) (fPrep: FuncAppPrep) : ComputedRaw option =
        let check =
            if view.Whole then
                wholeCheck.Value
            else
                view.CheckResultsAt(fPrep.CalleeEnd.Line, fPrep.CalleeEndCol)

        match check.GetSymbolUseAtLocation(fPrep.CalleeEnd.Line, fPrep.CalleeEndCol, fPrep.CalleeLineText, fPrep.CalleeNames) with
        | None -> None
        | Some symbolUse ->
            let isValid (mfv: FSharpMemberOrFunctionOrValue) =
                not (PrettyNaming.IsOperatorDisplayName mfv.DisplayName)
                && not mfv.IsProperty
                && mfv.CurriedParameterGroups.Count > 0

            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as mfv when isValid mfv ->
                let tipParts =
                    check.GetToolTipParts(fPrep.CalleeEnd.Line, fPrep.CalleeEndCol, fPrep.CalleeLineText, fPrep.CalleeNames, FSharpTokenTag.Identifier)

                match fst tipParts with
                | ToolTipText []
                | ToolTipText [ ToolTipElement.None ] -> None
                | _ ->
                    let symbolStart = symbolUse.Range.Start

                    let numArgsAlreadyApplied =
                        view.Parse.TryIdentOfPipelineContainingPosAndNumArgsApplied symbolStart
                        |> Option.map snd
                        |> Option.defaultValue 0

                    let definedArgs = mfv.CurriedParameterGroups |> Seq.toArray
                    let numDefinedArgs = definedArgs.Length

                    let curriedArgsInSource =
                        view.Parse.GetAllArgumentsForFunctionApplicationAtPosition symbolStart
                        |> Option.defaultValue []
                        |> Array.ofList

                    if numDefinedArgs < curriedArgsInSource.Length then
                        None
                    else
                        let argumentIndexOpt =
                            match curriedArgsInSource |> Array.tryFindIndex (fun argRange -> Range.rangeContainsPos argRange caretPos) with
                            | Some idx -> Some idx
                            | None ->
                                match curriedArgsInSource |> Array.tryFindIndex (fun argRange -> Position.posGeq argRange.Start caretPos) with
                                | Some idx -> Some idx
                                | None ->
                                    if numDefinedArgs - numArgsAlreadyApplied > curriedArgsInSource.Length then
                                        Some curriedArgsInSource.Length
                                    else
                                        None

                        match argumentIndexOpt with
                        | None -> None
                        | Some argumentIndex ->
                            let flattenedParams =
                                definedArgs
                                |> Array.map (fun group ->
                                    let groupParams = group |> Seq.toArray

                                    let name, typeDisplay =
                                        match groupParams with
                                        | [| p |] ->
                                            let pName =
                                                match p.Name with
                                                | Some n when n <> "" -> n
                                                | _ -> p.DisplayName

                                            pName, p.Type.Format symbolUse.DisplayContext
                                        | _ ->
                                            let names =
                                                groupParams
                                                |> Array.map (fun p ->
                                                    match p.Name with
                                                    | Some n when n <> "" -> n
                                                    | _ -> p.DisplayName)
                                                |> String.concat ", "

                                            let types =
                                                groupParams
                                                |> Array.map (fun p -> p.Type.Format symbolUse.DisplayContext)
                                                |> String.concat " * "

                                            sprintf "(%s)" names, sprintf "(%s)" types

                                    { ParameterInformation.Label = U2.C1 name
                                      Documentation = if typeDisplay = "" then None else Some(U2.C1 typeDisplay) })

                            let methods =
                                let lines = view.Text.Split '\n'

                                let startLineText =
                                    if symbolStart.Line >= 1 && symbolStart.Line <= lines.Length then
                                        lines[symbolStart.Line - 1].TrimEnd '\r'
                                    else
                                        fPrep.CalleeLineText

                                check.GetMethods(symbolStart.Line, symbolUse.Range.EndColumn, startLineText, None).Methods

                            let overloads =
                                if methods.Length > 0 then
                                    methods
                                    |> Array.map (fun m ->
                                        let label, fallbackDoc = extractLabelAndFallbackDoc m.Description m.XmlDoc

                                        { Label = label
                                          FallbackDoc = fallbackDoc
                                          Parameters = flattenedParams })
                                else
                                    let label, fallbackDoc = extractLabelAndFallbackDoc (fst tipParts) mfv.XmlDoc

                                    [| { Label = label
                                         FallbackDoc = fallbackDoc
                                         Parameters = flattenedParams } |]

                            Some
                                { Overloads = overloads
                                  ActiveSignature = Some 0u
                                  ActiveParameter = Some(uint32 argumentIndex)
                                  TipParts = Some tipParts
                                  Identity = view.Identity
                                  AskingDocs = view.Docs }
            | _ -> None

    let private computeInRead (view: FileView) (prepOpt: Prepared option) : ComputedRaw option =
        match prepOpt with
        | None -> None
        | Some prep ->
            StackGuard.onLargeStack (fun () ->
                let wholeCheck = lazy view.Result.CheckResultsNotKept

                let preferMethod =
                    match prep.Method, prep.FuncApp, prep.PrevNonWsChar with
                    | Some _, _, ValueSome(',' | '(' | '<') -> true
                    | Some _, None, _ -> true
                    | Some m, Some f, _ ->
                        // If the open '(' is at or after the function identifier end, the `(` belongs to that callee itself
                        Position.posGeq m.OpenParen f.CalleeEnd
                    | None, _, _ -> false

                if preferMethod then
                    match prep.Method |> Option.bind (computeMethod view wholeCheck) with
                    | Some r -> Some r
                    | None -> prep.FuncApp |> Option.bind (computeFuncApp view wholeCheck prep.CaretPos)
                else
                    match prep.FuncApp |> Option.bind (computeFuncApp view wholeCheck prep.CaretPos) with
                    | Some r -> Some r
                    | None -> prep.Method |> Option.bind (computeMethod view wholeCheck))

    let internal atWithTrigger
        (triggerChar: char option)
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<SignatureHelp option> =
        let rec attempt () =
            task {
                let! rawOpt =
                    graph.ReadFile(
                        path,
                        (fun view -> prepareAt triggerChar view line character),
                        computeInRead,
                        ct
                    )

                match rawOpt with
                | None -> return None
                | Some raw ->
                    match raw.TipParts with
                    | None ->
                        let sigs =
                            raw.Overloads
                            |> Array.map (fun o ->
                                { SignatureInformation.Label = o.Label
                                  Documentation =
                                    o.FallbackDoc
                                    |> Option.map (fun d -> U2.C2 { Kind = MarkupKind.Markdown; Value = d })
                                  Parameters = Some o.Parameters
                                  ActiveParameter = None })

                        return
                            Some
                                { Signatures = sigs
                                  ActiveSignature = raw.ActiveSignature
                                  ActiveParameter = raw.ActiveParameter }
                    | Some((_, sources) as tipSignature) ->
                        let held =
                            sources
                            |> List.choose id
                            |> List.collect (fun s -> s.Owners @ s.InheritOwners)
                            |> List.map _.FileName
                            |> List.filter (fun f -> f <> "")
                            |> List.distinctBy (fun f -> f.ToUpperInvariant())
                            |> List.filter (fun f -> not (String.Equals(f, path, StringComparison.OrdinalIgnoreCase)))
                            |> List.choose (fun file ->
                                ownersOf file
                                |> List.tryFind (fun g -> g.Holds file && g.SharesHost graph)
                                |> Option.map (fun g -> g, file))
                            |> Array.ofList

                        let! docsRead =
                            if held.Length = 0 then
                                Task.FromResult(ValueSome [||])
                            else
                                graph.DocsInTurn(path, raw.Identity, held, ct)

                        match docsRead with
                        | ValueNone -> return! attempt ()
                        | ValueSome readArr ->
                            let docs = Dictionary<string, FileDocs>(StringComparer.OrdinalIgnoreCase)

                            for (_, file), d in Array.zip held readArr do
                                docs[file] <- d

                            docs[path] <- raw.AskingDocs

                            let commentOf =
                                FileDocs.CommentOf(fun file ->
                                    match docs.TryGetValue file with
                                    | true, d -> ValueSome d
                                    | _ -> ValueNone)

                            let (ToolTipText composedElements) = Hover.compose commentOf tipSignature

                            let composedItems =
                                composedElements
                                |> List.collect (function
                                    | ToolTipElement.Group items -> items
                                    | _ -> [])
                                |> Array.ofList

                            let sigs =
                                raw.Overloads
                                |> Array.mapi (fun idx o ->
                                    let liveDoc =
                                        composedItems
                                        |> Array.tryFind (fun item -> item.MainDescription.Text = o.Label)
                                        |> Option.orElseWith (fun () ->
                                            if composedItems.Length = raw.Overloads.Length then Some composedItems[idx]
                                            else None)
                                        |> Option.bind (fun item -> Hover.docText item.XmlDoc)
                                        |> Option.orElse o.FallbackDoc

                                    { SignatureInformation.Label = o.Label
                                      Documentation =
                                        liveDoc
                                        |> Option.map (fun d -> U2.C2 { Kind = MarkupKind.Markdown; Value = d })
                                      Parameters = Some o.Parameters
                                      ActiveParameter = None })

                            return
                                Some
                                    { Signatures = sigs
                                      ActiveSignature = raw.ActiveSignature
                                      ActiveParameter = raw.ActiveParameter }
            }

        attempt ()

    let at
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<SignatureHelp option> =
        atWithTrigger None ownersOf graph path line character ct
