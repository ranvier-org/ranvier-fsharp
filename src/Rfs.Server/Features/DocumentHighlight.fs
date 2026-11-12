namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

/// <summary>
/// Same-file symbol occurrence highlighting (<c>textDocument/documentHighlight</c>) for both <c>.fs</c> and <c>.fsi</c> files.
/// Strips leading <c>Module.</c>/<c>Type.</c> qualifiers while preserving <c>``backticked``</c> identifiers, and marks
/// definitions as <c>Write</c> and references as <c>Read</c>.
/// </summary>
module DocumentHighlight =

    let private setterSuffix = "|setter"

    let private withoutSetter (key: string) =
        if key.EndsWith(setterSuffix, StringComparison.Ordinal) then
            key.Substring(0, key.Length - setterSuffix.Length)
        else
            key

    let private sameFile (a: string) (b: string) =
        try
            String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)
        with _ ->
            String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    /// Strips any leading `Namespace.Module.Type.` qualification from an FCS range using the settled file lines.
    let internal stripQualification (lines: string[]) (r: range) : Ionide.LanguageServerProtocol.Types.Range option =
        let sl = r.StartLine - 1
        let el = r.EndLine - 1

        if sl < 0 || el < sl || el >= lines.Length then
            None
        elif sl = el then
            let lineStr = lines[sl]
            let sc = min (max 0 r.StartColumn) lineStr.Length
            let ec = min (max sc r.EndColumn) lineStr.Length

            if ec <= sc then
                None
            else
                let raw = lineStr.Substring(sc, ec - sc)

                let strippedOffset =
                    if raw.Length >= 4 && raw.EndsWith("``", StringComparison.Ordinal) then
                        let openTicks = raw.LastIndexOf("``", raw.Length - 3, StringComparison.Ordinal)

                        if openTicks > 0 then
                            let before = raw.Substring(0, openTicks).TrimEnd()
                            if before.EndsWith(".", StringComparison.Ordinal) then openTicks else 0
                        else
                            0
                    else
                        let lastDot = raw.LastIndexOf '.'

                        if
                            lastDot >= 0
                            && lastDot + 1 < raw.Length
                            && (Char.IsLetterOrDigit raw[lastDot + 1] || raw[lastDot + 1] = '_')
                        then
                            lastDot + 1
                        else
                            0

                let finalSc = sc + strippedOffset

                Some
                    { Start = { Line = uint32 sl; Character = uint32 finalSc }
                      End = { Line = uint32 el; Character = uint32 ec } }
        else
            let endLineStr = lines[el]
            let ec = min (max 0 r.EndColumn) endLineStr.Length
            let tail = endLineStr.Substring(0, ec)

            let finalSc =
                if tail.Length >= 4 && tail.EndsWith("``", StringComparison.Ordinal) then
                    let openTicks = tail.LastIndexOf("``", tail.Length - 3, StringComparison.Ordinal)
                    if openTicks >= 0 then openTicks else 0
                else
                    let lastDot = tail.LastIndexOf '.'

                    if lastDot >= 0 && lastDot + 1 < tail.Length then
                        let mutable i = lastDot + 1

                        while i < tail.Length && Char.IsWhiteSpace tail[i] do
                            i <- i + 1

                        i
                    else
                        let mutable i = 0

                        while i < tail.Length && Char.IsWhiteSpace tail[i] do
                            i <- i + 1

                        i

            Some
                { Start = { Line = uint32 el; Character = uint32 finalSc }
                  End = { Line = uint32 el; Character = uint32 ec } }

    let private buildHighlights
        (path: string)
        (text: string)
        (isPropAccessorFilter: string option)
        (hits: seq<struct (range * bool)>)
        : DocumentHighlight[] option =
        let lines = text.Split '\n' |> Array.map (fun s -> s.TrimEnd '\r')
        let bySpan = Dictionary<struct (uint32 * uint32 * uint32 * uint32), bool>()

        for struct (r, isDef) in hits do
            if sameFile r.FileName path then
                match stripQualification lines r with
                | None -> ()
                | Some lspRange ->
                    let skipSyntheticGetSet =
                        match isPropAccessorFilter with
                        | Some propName when lspRange.Start.Line = lspRange.End.Line ->
                            let l = lines[int lspRange.Start.Line]
                            let sc = int lspRange.Start.Character
                            let ec = int lspRange.End.Character
                            let tok = l.Substring(sc, ec - sc)
                            (tok = "get" || tok = "set") && propName <> tok
                        | _ -> false

                    if not skipSyntheticGetSet then
                        let k =
                            struct (lspRange.Start.Line, lspRange.Start.Character, lspRange.End.Line, lspRange.End.Character)

                        match bySpan.TryGetValue k with
                        | true, existingDef -> bySpan[k] <- existingDef || isDef
                        | _ -> bySpan[k] <- isDef

        if bySpan.Count = 0 then
            None
        else
            let arr =
                [| for KeyValue(struct (sl, sc, el, ec), isDef) in bySpan ->
                       { Range =
                           { Start = { Line = sl; Character = sc }
                             End = { Line = el; Character = ec } }
                         Kind = Some(if isDef then DocumentHighlightKind.Write else DocumentHighlightKind.Read) } |]

            Array.sortInPlaceWith
                (fun (a: DocumentHighlight) (b: DocumentHighlight) ->
                    let c1 = compare a.Range.Start.Line b.Range.Start.Line
                    if c1 <> 0 then c1
                    else
                        let c2 = compare a.Range.Start.Character b.Range.Start.Character
                        if c2 <> 0 then c2
                        else
                            let c3 = compare a.Range.End.Line b.Range.End.Line
                            if c3 <> 0 then c3 else compare a.Range.End.Character b.Range.End.Character)
                arr

            Some arr

    let internal pickSignatureUse
        (line: int)
        (character: int)
        (endColOpt: int option)
        (sigUses: (struct (range * bool * FSharpSymbol))[])
        : struct (range * bool * FSharpSymbol) option =
        Definition.pickSignatureUse line character endColOpt sigUses

    let at
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<DocumentHighlight[] option> =
        if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
            graph.ReadSignature(
                path,
                (fun view -> Hover.lookupOf view.Text line character),
                (fun view lookup uses sigUses ->
                    let endColOpt = lookup |> Option.map (fun (_, _, ec) -> ec)

                    match pickSignatureUse line character endColOpt sigUses with
                    | None -> None
                    | Some(struct (_, isDef, symbol)) ->
                        let propFilter =
                            match symbol with
                            | :? FSharpMemberOrFunctionOrValue as v when v.IsProperty || v.IsPropertyGetterMethod || v.IsPropertySetterMethod ->
                                Some v.LogicalName
                            | _ -> None

                        let keyOpt =
                            match symbol with
                            | :? FSharpMemberOrFunctionOrValue as v when isDef && v.IsConstructor -> DeclarationChecker.SymbolKey symbol
                            | _ -> SymbolUses.keyOf symbol

                        let rawHits =
                            match keyOpt with
                            | Some key ->
                                let plain = withoutSetter key
                                let own = (uses.Of plain).Hits
                                let setter = (uses.Of(plain + setterSuffix)).Hits
                                Array.append own setter |> Seq.ofArray
                            | None ->
                                sigUses
                                |> Seq.choose (fun (struct (r, d, s)) -> if s.IsEffectivelySameAs symbol then Some(struct (r, d)) else None)

                        buildHighlights path view.Text propFilter rawHits),
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
                                view.Result.CheckResults

                        match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
                        | None -> None
                        | Some u ->
                            let propFilter =
                                match u.Symbol with
                                | :? FSharpMemberOrFunctionOrValue as v when v.IsProperty || v.IsPropertyGetterMethod || v.IsPropertySetterMethod ->
                                    Some v.LogicalName
                                | _ -> None

                            let keys = SymbolUses.keysIn check ct

                            let rawHits =
                                match keys u with
                                | Some key ->
                                    let plain = withoutSetter key

                                    check.GetAllUsesOfAllSymbolsInFile ct
                                    |> Seq.choose (fun d ->
                                        match keys d with
                                        | Some dk when withoutSetter dk = plain -> Some(struct (d.Range, d.IsFromDefinition))
                                        | _ -> None)
                                | None ->
                                    check.GetUsesOfSymbolInFile(u.Symbol, cancellationToken = ct)
                                    |> Seq.map (fun d -> struct (d.Range, d.IsFromDefinition))

                            buildHighlights path view.Text propFilter rawHits),
                ct
            )
