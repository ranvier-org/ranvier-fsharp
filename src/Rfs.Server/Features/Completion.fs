namespace Rfs.Server

open System
open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Checker

/// <summary>
/// Completion, ported from FSAC's <c>TryGetCompletions</c> (ParseAndCheckResults.fs) and its
/// <c>TextDocumentCompletion</c> and <c>CompletionItemResolve</c> handlers.
/// </summary>
/// <remarks>
/// Not ported: items from namespaces not opened (external autocomplete), keywords and hash directives, and the retry
/// when the request comes in before its edit (<c>Publisher.Barrier</c> answers that). Resolve looks the item up again:
/// its description reads the typed tree, which only a read may, so an item outlives no edit of its row.
/// A stated limit in a file with a signature: a row checked again for a query (FileView.CheckResultsAt) is not conformed
/// to the signature, by design, so an item from that row may show no documentation. It never shows another's. The list
/// of items is not affected.
/// </remarks>
module Completion =
    let private kindOf (glyph: FSharpGlyph) : CompletionItemKind option =
        match glyph with
        | FSharpGlyph.Class
        | FSharpGlyph.Exception
        | FSharpGlyph.Typedef
        | FSharpGlyph.Type
        | FSharpGlyph.Union -> Some CompletionItemKind.Class
        | FSharpGlyph.Constant -> Some CompletionItemKind.Constant
        | FSharpGlyph.Delegate -> Some CompletionItemKind.Function
        | FSharpGlyph.Enum -> Some CompletionItemKind.Enum
        | FSharpGlyph.EnumMember -> Some CompletionItemKind.EnumMember
        | FSharpGlyph.Event -> Some CompletionItemKind.Event
        | FSharpGlyph.Field -> Some CompletionItemKind.Field
        | FSharpGlyph.Interface -> Some CompletionItemKind.Interface
        | FSharpGlyph.Method
        | FSharpGlyph.OverridenMethod
        | FSharpGlyph.ExtensionMethod -> Some CompletionItemKind.Method
        | FSharpGlyph.Module
        | FSharpGlyph.NameSpace -> Some CompletionItemKind.Module
        | FSharpGlyph.Property -> Some CompletionItemKind.Property
        | FSharpGlyph.Struct -> Some CompletionItemKind.Struct
        | FSharpGlyph.Variable -> Some CompletionItemKind.Variable
        | _ -> None

    let private inString (pos: pos) (ast: ParsedInput) =
        let inType =
            (pos, ast)
            ||> ParsedInput.tryPick (fun _ node ->
                match node with
                | SyntaxNode.SynType _ -> Some()
                | _ -> None)

        inType.IsNone
        && (match ParsedInput.tryNode pos ast with
            | Some(SyntaxNode.SynExpr(SynExpr.Const(SynConst.String _, _)), _) -> true
            | _ -> false)

    let private kindPriority (kind: FSharp.Compiler.EditorServices.CompletionItemKind) =
        match kind with
        | FSharp.Compiler.EditorServices.CompletionItemKind.SuggestedName
        | FSharp.Compiler.EditorServices.CompletionItemKind.CustomOperation -> 0
        | FSharp.Compiler.EditorServices.CompletionItemKind.Argument
        | FSharp.Compiler.EditorServices.CompletionItemKind.Property -> 1
        | FSharp.Compiler.EditorServices.CompletionItemKind.Field -> 2
        | FSharp.Compiler.EditorServices.CompletionItemKind.Method(isExtension = false) -> 3
        | FSharp.Compiler.EditorServices.CompletionItemKind.Event -> 4
        | FSharp.Compiler.EditorServices.CompletionItemKind.Other -> 6
        | FSharp.Compiler.EditorServices.CompletionItemKind.Method(isExtension = true) -> 7

    let private order (x: DeclarationListItem) (y: DeclarationListItem) =
        let mutable n = (not x.IsResolved).CompareTo(not y.IsResolved)

        if n = 0 then
            n <- (kindPriority x.Kind).CompareTo(kindPriority y.Kind)

        if n = 0 then
            n <- (not x.IsOwnMember).CompareTo(not y.IsOwnMember)

        if n = 0 then
            n <- String.Compare(x.NameInList, y.NameInList, StringComparison.OrdinalIgnoreCase)

        if n = 0 then
            n <- x.MinorPriority.CompareTo y.MinorPriority

        n

    /// The items at the 0-based line and character of the text, in FSAC's order. Empty inside a string.
    let itemsOf (check: FSharpCheckFileResults) (parse: FSharpParseFileResults) (text: string) (line: int) (character: int) : DeclarationListItem[] =
        let lines = text.Split '\n'

        if line < 0 || line >= lines.Length then
            [||]
        else
            let lineStr = lines[line].TrimEnd '\r'
            let pos = Position.mkPos (line + 1) character

            if inString pos parse.ParseTree then
                [||]
            else
                let longName = QuickParse.GetPartialLongNameEx(lineStr, character - 1)
                let context = ParsedInput.TryGetCompletionContext(pos, parse.ParseTree, lineStr)
                let found = check.GetDeclarationListInfo(Some parse, pos.Line, lineStr, longName, completionContextAtPos = (pos, context))
                Array.sortWith order found.Items

    /// The column a list at the 0-based character reads its names from (FSharpCheckerResults' `loc`), which is what picks
    /// the row that answers: at a row's start, the one before it.
    let private queryColumn (text: string) (line: int) (character: int) =
        let lines = text.Split '\n'

        if line < 0 || line >= lines.Length then
            character
        else
            let lineStr = lines[line].TrimEnd '\r'

            if character >= lineStr.Length then lineStr.Length
            elif lineStr[character] = '.' then character + 1
            elif character = 0 then 0
            else character - 1

    /// Runs `f` on a thread of its own with the graph thread's stack: reading a parse tree has no StackGuard, and a row of
    /// 4000 `let`s overflows a pool thread's, which kills the process.
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
                GraphHost.StackSize
            )

        thread.Start()
        thread.Join()

        if not (isNull failure.Value) then
            failure.Value.Throw()

        result.Value.Value

    /// The items at the 0-based line and character of the read's text, through FileView.CheckResultsAt.
    let itemsAt (view: FileView) (line: int) (character: int) =
        itemsOf (view.CheckResultsAt(line + 1, queryColumn view.Text line character)) view.Parse view.Text line character

    /// What an item carries for its resolve.
    let dataOf (path: string) (line: int) (character: int) : JToken =
        JObject(JProperty("path", path), JProperty("line", line), JProperty("character", character)) :> JToken

    let private itemOf (path: string) (line: int) (character: int) (id: int) (d: DeclarationListItem) : CompletionItem =
        { CompletionItem.Create d.NameInList with
            Data = Some(dataOf path line character)
            Kind = kindOf d.Glyph
            InsertText = Some d.NameInCode
            SortText = Some(sprintf "%06d" id)
            FilterText = Some d.NameInList }

    /// The list at the position of `path`, once the graph has settled on the file's latest text. The lookup holds off
    /// the file's checks; a kept row may be checked again in it (FileView.CheckResultsAt). The items carry their names:
    /// reading them reads no typed tree.
    let at (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<CompletionItem[]> =
        task {
            let! items =
                graph.ReadFile(
                    path,
                    (fun view -> onLargeStack (fun () -> itemsAt view line character)),
                    ct
                )

            return items |> Array.mapi (itemOf path line character)
        }

    /// Where an item came from, when it says.
    let positionOf (item: CompletionItem) : (string * int * int) option =
        match item.Data with
        | Some(:? JObject as data) ->
            match data["path"], data["line"], data["character"] with
            | null, _, _
            | _, null, _
            | _, _, null -> None
            | path, line, character -> Some(path.ToObject<string>(), line.ToObject<int>(), character.ToObject<int>())
        | _ -> None

    /// The item with its signature and documentation, or as it is when its position lists it no more. Only the description
    /// is read inside the lookup: formatting it may load an XML file.
    let resolve (graph: ProjectGraph) (item: CompletionItem) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<CompletionItem> =
        task {
            let! tip =
                graph.ReadFile(
                    path,
                    (fun view ->
                        itemsAt view line character
                        |> Array.tryFind (fun d -> d.NameInList = item.Label)
                        |> Option.map _.Description),
                    ct
                )

            let detail (ToolTipText elements) =
                elements
                |> List.tryPick (function
                    | ToolTipElement.Group(first :: _) -> Some first.MainDescription.Text
                    | _ -> None)

            return
                match tip with
                | None -> item
                | Some tip ->
                    { item with
                        Detail = detail tip
                        Documentation =
                            Hover.format tip
                            |> Option.map (fun text -> U2.C2 { Kind = MarkupKind.Markdown; Value = text }) }
        }
