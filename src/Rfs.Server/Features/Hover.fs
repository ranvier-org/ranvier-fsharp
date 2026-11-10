namespace Rfs.Server

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Runtime.CompilerServices
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
/// remarks sections and FSAC's comment-style modes, generic type mappings, and documentation links.
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

    /// A tooltip without the documentation FCS knows the source of, and those sources (GetToolTipParts)
    type internal Signature = ToolTipText * FSharpDocSource option list

    /// Signatures by the declaration result that answers the position and the position in it, or for a file checked
    /// whole by its check: a hover again at the same place of a kept declaration renders nothing, after any edit that
    /// keeps it. Nothing in a signature depends on a `///` comment.
    let private signatures = ConditionalWeakTable<obj, ConcurrentDictionary<struct (int * int * string * string), Signature>>()

    /// Signatures naming an `@` owner, by the published result (FileView.Identity) and the position in the file. While
    /// one result is published its rows, their places and the environment `At` checks a stale row in are fixed, so a
    /// signature with an extension's place, or the one `At` renders for `@?`, holds until the result is replaced.
    let private placedSignatures =
        ConditionalWeakTable<obj, ConcurrentDictionary<struct (int * int * string * string), Signature>>()

    let private rendered = ConcurrentDictionary<string, int64>(StringComparer.OrdinalIgnoreCase)

    /// How many signatures have been rendered for hovers in the file. For tests.
    let internal signaturesRendered (path: string) =
        match rendered.TryGetValue path with
        | true, n -> n
        | _ -> 0L

    let private signatureOf (path: string) (view: FileView) (line: int) (lineStr: string, names: string list, endColumn: int) : Signature =
        let owner, row =
            match view.RowAt(line + 1) with
            | ValueSome(struct (result, first)) -> result, line + 1 - first
            | ValueNone -> box view.Result.CheckResults, line

        let cache = signatures.GetValue(owner, fun _ -> ConcurrentDictionary())

        let render (check: FSharpCheckFileResults) =
            rendered.AddOrUpdate(path, 1L, (fun _ n -> n + 1L)) |> ignore
            check.GetToolTipParts(line + 1, endColumn, lineStr, names, FSharpTokenTag.Identifier)

        let placed (predicate: string -> bool) ((_, sources): Signature) =
            sources
            |> List.exists (function
                | Some s -> s.Owners @ s.InheritOwners |> List.exists (fun o -> predicate o.Path)
                | None -> false)

        let joined = String.Join("\n", names)
        let key = struct (row, endColumn, lineStr, joined)
        let byResult = placedSignatures.GetValue(view.Identity, fun _ -> ConcurrentDictionary())
        let placedKey = struct (line, endColumn, lineStr, joined)

        match cache.TryGetValue key with
        | true, s -> s
        | _ ->
            match byResult.TryGetValue placedKey with
            | true, s -> s
            | _ ->
                let s = render view.Result.CheckResults

                // An `@` owner is the place of an extension's value as the check has it now. A kept row's result can
                // outlive that: the row that makes the value can be checked again, or shifted, after it. So it is kept
                // by the published result only.
                if not (placed (fun p -> p.StartsWith "@") s) then
                    cache.TryAdd(key, s) |> ignore
                    s
                else
                    // `@?` is a value a kept row still refers to after the row that made it was checked again: no key
                    // names it in the parse. Checked again in the current environment (RowView.at), as any query reading
                    // its names would be, the row refers to the current value, which a binding of the check makes and
                    // whose place is in the parse.
                    let s = if placed (fun p -> p = "@?") s then render (view.At (line + 1) endColumn) else s
                    byResult.TryAdd(placedKey, s) |> ignore
                    s

    /// The tooltip with each element's documentation made from the current comments of the declarations it is read
    /// from, as `commentOf` gives them
    let internal compose (commentOf: FSharpDocOwner -> FSharp.Compiler.Xml.XmlDoc voption) ((ToolTipText elements, sources): Signature) : ToolTipText =
        let mutable rest = sources

        let next () =
            match rest with
            | s :: tail ->
                rest <- tail
                s
            | [] -> None

        elements
        |> List.map (function
            | ToolTipElement.Group items ->
                ToolTipElement.Group(
                    items
                    |> List.map (fun item ->
                        match next () with
                        | Some source -> { item with XmlDoc = source.Compose commentOf }
                        | None -> item)
                )
            | other -> other)
        |> ToolTipText

    /// The comments of a parse, once per parse: the asking file's docs come from the parse its published result is of,
    /// as the read gave it, so they need no graph turn
    let private parsedDocs = ConditionalWeakTable<FSharpParseFileResults, FileDocs>()

    let private docsOfParse (parse: FSharpParseFileResults) =
        parsedDocs.GetValue(parse, fun p -> FileDocs.OfParse p.ParseTree)

    /// Runs between a hover's signature and its docs, with the file. For tests.
    let mutable internal afterSignature: string -> Task = fun _ -> Task.CompletedTask

    /// <summary>
    /// Hovers at the position of `path`, once the graph has settled on the file's latest text (a request node,
    /// ProjectGraph.ReadFile). The signature is the check's, cached by the declaration result and position, or by the
    /// published result when it names an extension by its place (`@`, signatureOf). The documentation is made from the
    /// comments of the files it is read from: the asking file's from the parse its result is of, read with it, and each
    /// other file's from its latest parse (ProjectGraph.DocsInTurn), in this project, a later file, or another project
    /// `owners` names. A `///` edit is seen at once, with no check of anything.
    /// </summary>
    /// <remarks>
    /// Only the check is asked under the lock, which holds off the file's checks: the tokenizer reads the file from the
    /// top, before the lock, and `format` may load an XML file, after the read. A declaration in no file of the
    /// workspace, as in a referenced assembly, keeps the documentation FCS gives.
    /// </remarks>
    let at (owners: string -> ProjectGraph list) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<string option> =
        let rec attempt () =
            task {
                let! signature =
                    graph.ReadFile(
                        path,
                        (fun view -> lookupOf view.Text line character),
                        (fun view lookup -> lookup |> Option.map (fun l -> signatureOf path view line l, view.Identity, view.Parse)),
                        ct
                    )

                match signature with
                | None -> return None
                | Some(((_, sources) as signature), identity, parse) ->
                    let held =
                        sources
                        |> List.choose id
                        |> List.collect (fun s -> s.Owners @ s.InheritOwners)
                        |> List.map _.FileName
                        |> List.filter (fun f -> f <> "")
                        |> List.distinctBy (fun f -> f.ToUpperInvariant())
                        // The asking file's are of the parse its result is of, read with it
                        |> List.filter (fun f -> not (String.Equals(f, path, StringComparison.OrdinalIgnoreCase)))
                        |> List.choose (fun file ->
                            owners file
                            |> List.tryFind (fun g -> g.Holds file && g.SharesHost graph)
                            |> Option.map (fun g -> g, file))
                        |> Array.ofList

                    do! afterSignature path

                    // Other files' docs in one turn with the signature's result still published: an edit of the asking
                    // file, or one that unsettles it, landing between them reads the file again. The asking file's docs
                    // are of the parse read with the signature, so an edit of it after the read needs nothing more.
                    // With no other file of the workspace to read, no turn
                    let! read = if held.Length = 0 then Task.FromResult(ValueSome [||]) else graph.DocsInTurn(path, identity, held, ct)
                    let own =
                        sources
                        |> List.exists (function
                            | Some s -> s.Owners @ s.InheritOwners |> List.exists (fun o -> String.Equals(o.FileName, path, StringComparison.OrdinalIgnoreCase))
                            | None -> false)

                    match read with
                    | ValueNone -> return! attempt ()
                    | ValueSome read ->
                        let docs = Dictionary<string, FileDocs>(StringComparer.OrdinalIgnoreCase)

                        for (_, file), d in Array.zip held read do
                            docs[file] <- d

                        if own then
                            docs[path] <- docsOfParse parse

                        let commentOf =
                            FileDocs.CommentOf(fun file ->
                                match docs.TryGetValue file with
                                | true, d -> ValueSome d
                                | _ -> ValueNone)

                        return format (compose commentOf signature)
            }

        attempt ()

    /// The first identifier of the text that a hover would ask for, near its top
    let private firstLookup (text: string) : (int * (string * string list * int)) option =
        let lines = text.Split '\n'

        seq {
            for line in 0 .. min (lines.Length - 1) 200 do
                for m in Regex.Matches(lines[line], @"\b[A-Za-z_][A-Za-z0-9_]*") do
                    yield line, m.Index
        }
        |> Seq.truncate 64
        |> Seq.tryPick (fun (line, character) -> lookupOf text line character |> Option.map (fun l -> line, l))

    let mutable private warmed = 0

    /// <summary>
    /// Once in a process, compiles what a first hover runs: the tooltip parts, a file's comment walk, and a docs read of
    /// another file (ProjectGraph.DocsInTurn). On a real file of `graph`, whose answer is dropped.
    /// </summary>
    /// <remarks>
    /// The request node takes the file's lock only to read the published result's fields; GetToolTipParts runs after
    /// it is released, so no hover waits on it. Any failure is dropped: nothing reads what this computes.
    /// </remarks>
    let warm (graph: ProjectGraph) (files: string list) : Task =
        if Interlocked.Exchange(&warmed, 1) <> 0 then
            Task.CompletedTask
        else
            Task.Run<unit>(fun () ->
                task {
                    try
                        match files |> List.filter (fun f -> not (f.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase))) with
                        | [] -> ()
                        | file :: rest ->
                            let! found =
                                graph.ReadFile(
                                    file,
                                    (fun text -> firstLookup text.Text, docsOfParse text.Parse),
                                    (fun view (lookup, docs) -> lookup |> Option.map (fun l -> view.Result.CheckResults, view.Identity, docs, l)),
                                    CancellationToken.None
                                )

                            match found with
                            | None -> ()
                            | Some(check, identity, docs, (line, (lineStr, names, endColumn))) ->
                                let parts = check.GetToolTipParts(line + 1, endColumn, lineStr, names, FSharpTokenTag.Identifier)
                                let commentOf = FileDocs.CommentOf(fun _ -> ValueSome docs)
                                format (compose commentOf parts) |> ignore

                                match rest with
                                | other :: _ -> do! graph.DocsInTurn(file, identity, [| graph, other |], CancellationToken.None) :> Task
                                | [] -> ()
                    with _ ->
                        ()
                })
            :> Task
