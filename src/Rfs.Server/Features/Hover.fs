namespace Rfs.Server

open System
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Hover, ported from FSAC's <c>TryGetToolTipEnhanced</c> (ParseAndCheckResults.fs) and the part of its TipFormatter that
/// renders a tooltip as Markdown: the signature in a code block, then the summary of the XML documentation. Left out:
/// keyword and hash-directive tooltips, the enhanced (parameter, returns, remarks) comment styles and documentation links.
/// </summary>
module Hover =
    let private tags = Regex("<[^>]+>", RegexOptions.Compiled)
    let private summary = Regex("<summary>(.*?)</summary>", RegexOptions.Compiled ||| RegexOptions.Singleline)

    let private docText (doc: FSharpXmlDoc) : string option =
        match doc with
        | FSharpXmlDoc.FromXmlText x ->
            let xml = String.Join("\n", x.GetElaboratedXmlLines())

            let body =
                match summary.Match xml with
                | m when m.Success -> m.Groups[1].Value
                | _ -> xml

            let text = tags.Replace(body, "").Replace("&lt;", "<").Replace("&gt;", ">").Trim()
            if text = "" then None else Some text
        | _ -> None

    /// The tooltip as Markdown, or None when it holds nothing
    let format (ToolTipText elements) : string option =
        let data = elements |> List.collect (function ToolTipElement.Group items -> items | _ -> [])

        let blocks =
            [ for item in data ->
                  let signature = item.MainDescription.Text
                  let code = $"```fsharp\n%s{signature}\n```"

                  match docText item.XmlDoc with
                  | Some d -> code + "\n\n" + d
                  | None -> code ]

        let errors = elements |> List.choose (function ToolTipElement.CompositionError e -> Some e | _ -> None)

        match blocks @ errors with
        | [] -> None
        | all -> Some(String.concat "\n\n---\n\n" all)

    /// The Markdown for the symbol at the 0-based line and character of the view's text.
    let tooltipOf (check: FSharpCheckFileResults) (text: string) (line: int) (character: int) : string option =
        let lines = text.Split '\n'

        if line < 0 || line >= lines.Length then
            None
        else
            let lineStr = lines[line].TrimEnd '\r'

            match QuickParse.GetCompleteIdentifierIsland false lineStr character with
            | None -> None
            | Some(ident, endColumn, _) ->
                let names = ident.Split '.' |> List.ofArray
                let tip = check.GetToolTip(line + 1, endColumn, lineStr, names, FSharpTokenTag.Identifier)
                format tip

    let tooltipAt (view: FileView) (line: int) (character: int) : string option =
        tooltipOf view.Result.CheckResults view.Text line character

    /// Hovers at the position of `path`, once the graph has settled on the file's latest text.
    let at (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<string option> =
        graph.ReadFile(path, (fun view -> tooltipAt view line character), ct)
