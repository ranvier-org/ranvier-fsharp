namespace Rfs.Server

open System
open System.Collections.Concurrent
open System.IO
open System.Net
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open System.Xml.Linq
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Tokenization
open Rfs.Checker

/// <summary>
/// Hover, ported from FSAC's <c>TryGetToolTipEnhanced</c> (ParseAndCheckResults.fs) and the part of its TipFormatter that
/// renders a tooltip as Markdown: the signature in a code block, then the summary of the XML documentation, from the
/// source's doc comment or from the referenced assembly's XML file.
/// </summary>
/// <remarks>
/// Not ported (hover answers nothing, or shows less): keyword and hash-directive tooltips, the parameter, returns and
/// remarks sections and FSAC's comment-style modes, generic type mappings, <c>inheritdoc</c>, and documentation links.
/// The identifier under the cursor is found with the tokenizer, as FSAC's <c>Lexer.findLongIdents</c> does: operators
/// answer, and strings and comments do not.
/// </remarks>
module Hover =
    let private tags = Regex("<[^>]+>", RegexOptions.Compiled)
    let private cref = Regex("""<(?:see|seealso)\s+cref="(?:[A-Z]:)?([^"]*)"\s*/>""", RegexOptions.Compiled)
    let private paramref = Regex("""<(?:paramref|typeparamref)\s+name="([^"]*)"\s*/>""", RegexOptions.Compiled)
    let private summary = Regex("<summary>(.*?)</summary>", RegexOptions.Compiled ||| RegexOptions.Singleline)

    /// The summary of a doc comment's XML, as plain text
    let private plain (xml: string) : string option =
        let body =
            match summary.Match xml with
            | m when m.Success -> m.Groups[1].Value
            | _ -> xml

        let referenced = cref.Replace(body, (fun m -> let name = m.Groups[1].Value in name.Substring(name.LastIndexOf '.' + 1)))
        let named = paramref.Replace(referenced, "$1")
        let text = WebUtility.HtmlDecode(tags.Replace(named, "")).Trim()
        let compact = Regex.Replace(text, @"\s*\n\s*", " ")
        if compact = "" then None else Some compact

    /// An assembly's XML documentation file, by member name, once it has been read. One that is missing or can't be read
    /// is not kept: a project reference's appears when it is built.
    let private xmlFiles = ConcurrentDictionary<string, System.Collections.Generic.Dictionary<string, string>>()

    let private membersOf (dllName: string) : System.Collections.Generic.Dictionary<string, string> option =
        match xmlFiles.TryGetValue dllName with
        | true, members -> Some members
        | _ ->
            let xml = Path.ChangeExtension(dllName, ".xml")

            try
                if File.Exists xml then
                    let members = System.Collections.Generic.Dictionary<string, string>()

                    for m in XDocument.Load(xml).Descendants "member" do
                        match m.Attribute "name" with
                        | null -> ()
                        | name -> members[name.Value] <- String.concat "" (m.Nodes() |> Seq.map _.ToString())

                    xmlFiles[dllName] <- members
                    Some members
                else
                    None
            with
            | :? System.Xml.XmlException
            | :? IOException
            | :? UnauthorizedAccessException -> None

    let internal docText (doc: FSharpXmlDoc) : string option =
        match doc with
        | FSharpXmlDoc.FromXmlText x -> plain (String.Join("\n", x.GetElaboratedXmlLines()))
        | FSharpXmlDoc.FromXmlFile(dllName, xmlSig) ->
            match membersOf dllName with
            | Some members ->
                match members.TryGetValue xmlSig with
                | true, xml -> plain xml
                | _ -> None
            | None -> None
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

    [<Struct>]
    type private Tok =
        { Left: int
          Right: int
          Kind: FSharpTokenCharKind
          Text: string }

    /// What the next line starts from: only a string or block comment that is still open. The tokenizer has no defines,
    /// so the `#if` state it would carry says nothing of what the project compiles; FSAC lexes each line on its own.
    let private carried (next: FSharpTokenizerLexState) : FSharpTokenizerLexState =
        match FSharpLineTokenizer.ColorStateOfLexState next with
        | FSharpTokenizerColorState.String
        | FSharpTokenizerColorState.VerbatimString
        | FSharpTokenizerColorState.TripleQuoteString
        | FSharpTokenizerColorState.Comment
        | FSharpTokenizerColorState.StringInComment
        | FSharpTokenizerColorState.VerbatimStringInComment
        | FSharpTokenizerColorState.TripleQuoteStringInComment -> next
        | _ -> FSharpTokenizerLexState.Initial

    /// The tokens of line `target`, lexed from the top so a block comment or string that opened above is known
    let private tokensOf (lines: string[]) (target: int) : Tok list =
        let tokenizer = FSharpSourceTokenizer([], Some "hover.fs", None)
        let mutable state = FSharpTokenizerLexState.Initial
        let mutable result = []

        for i in 0..target do
            let lineStr = lines[i].TrimEnd '\r'
            let line = tokenizer.CreateLineTokenizer lineStr
            let found = ResizeArray()
            let mutable more = true

            while more do
                match line.ScanToken state with
                | Some t, next ->
                    state <- next
                    let right = min t.RightColumn (lineStr.Length - 1)

                    if right >= t.LeftColumn then
                        found.Add
                            { Left = t.LeftColumn
                              Right = right
                              Kind = t.CharClass
                              Text = lineStr.Substring(t.LeftColumn, right - t.LeftColumn + 1) }
                | None, next ->
                    state <- carried next
                    more <- false

            if i = target then
                result <- List.ofSeq found

        result

    let private unquote (name: string) =
        if name.Length > 4 && name.StartsWith "``" && name.EndsWith "``" then name.Substring(2, name.Length - 4) else name

    /// The identifier or operator under the 0-based character: its long name up to it, and the column its text ends at
    let private nameAt (lines: string[]) (line: int) (character: int) : (string list * int) option =
        let tokens = tokensOf lines line |> Array.ofList

        match tokens |> Array.tryFindIndex (fun t -> t.Left <= character && character <= t.Right) with
        | None -> None
        | Some k ->
            let t = tokens[k]

            match t.Kind with
            | FSharpTokenCharKind.Identifier ->
                let mutable first = k

                while first >= 2 && tokens[first - 1].Text = "." && tokens[first - 2].Kind = FSharpTokenCharKind.Identifier do
                    first <- first - 2

                let names = [ for i in first..2 .. k -> unquote tokens[i].Text ]
                Some(names, t.Right + 1)
            | FSharpTokenCharKind.Operator -> Some([ t.Text ], t.Right + 1)
            | _ -> None

    /// What to ask the check for at a position of the text: the line as the check wants it, the long name and its end
    let internal lookupOf (text: string) (line: int) (character: int) : (string * string list * int) option =
        let lines = text.Split '\n'

        if line < 0 || line >= lines.Length then
            None
        else
            nameAt lines line character
            |> Option.map (fun (names, endColumn) -> lines[line].TrimEnd '\r', names, endColumn)

    let private toolTip (check: FSharpCheckFileResults) (line: int) (lineStr: string, names: string list, endColumn: int) : ToolTipText =
        check.GetToolTip(line + 1, endColumn, lineStr, names, FSharpTokenTag.Identifier)

    /// The Markdown for the symbol at the 0-based line and character of the text.
    let tooltipOf (check: FSharpCheckFileResults) (text: string) (line: int) (character: int) : string option =
        lookupOf text line character |> Option.bind (toolTip check line >> format)

    /// Hovers at the position of `path`, once the graph has settled on the file's latest text (a request node,
    /// ProjectGraph.ReadFile). Only the check is asked under the lock, which holds off the file's checks: the tokenizer
    /// reads the file from the top, before the lock, and `format` may load an XML file, after the read.
    let at (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<string option> =
        task {
            let! tip =
                graph.ReadFile(
                    path,
                    (fun view -> lookupOf view.Text line character),
                    (fun view lookup -> lookup |> Option.map (toolTip view.Result.CheckResults line)),
                    ct
                )

            return tip |> Option.bind format
        }
