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

    /// The tokens of a line lexed from `state`, and the state the next line starts in: only a string or block comment
    /// still open is carried (PositionMap.carried). The tokenizer has no defines, so the `#if` state it would carry says
    /// nothing of what the project compiles; FSAC lexes each line on its own.
    let private lineTokens (tokenizer: FSharpSourceTokenizer) (state: FSharpTokenizerLexState) (lineStr: string) =
        let line = tokenizer.CreateLineTokenizer lineStr
        let found = ResizeArray()
        let mutable state = state
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
                state <- PositionMap.carried next
                more <- false

        found.ToArray(), state

    /// The tokens of line `target`, lexed from the top so a block comment or string that opened above is known
    let private tokensOf (lines: string[]) (target: int) : Tok[] =
        let tokenizer = PositionMap.tokenizer ()
        let mutable state = FSharpTokenizerLexState.Initial
        let mutable result = [||]

        for i in 0..target do
            let tokens, next = lineTokens tokenizer state (lines[i].TrimEnd '\r')
            state <- next

            if i = target then
                result <- tokens

        result

    let private unquote (name: string) =
        if name.Length > 4 && name.StartsWith "``" && name.EndsWith "``" then name.Substring(2, name.Length - 4) else name

    /// The identifier or operator under the 0-based character: its long name up to it, and the column its text ends at
    let private nameIn (tokens: Tok[]) (character: int) : (string list * int) option =
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
            nameIn (tokensOf lines line) character
            |> Option.map (fun (names, endColumn) -> lines[line].TrimEnd '\r', names, endColumn)

    /// lookupOf in a shape, from the state its line starts in: the text is lexed once per shape
    let internal lookupIn (shape: FileShape) (line: int) (character: int) : (string * string list * int) option =
        if line < 0 || line >= shape.Lines.Length then
            None
        else
            let tokens, _ = lineTokens (PositionMap.tokenizer ()) shape.States.Value[line] shape.Lines[line]
            nameIn tokens character |> Option.map (fun (names, endColumn) -> shape.Lines[line], names, endColumn)

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

    let private placedRendered = ConcurrentDictionary<string, int64>(StringComparer.OrdinalIgnoreCase)

    /// How many signatures rendered for hovers in the file named an `@` owner. For tests.
    let internal placedSignaturesRendered (path: string) =
        match placedRendered.TryGetValue path with
        | true, n -> n
        | _ -> 0L

    /// The tooltip parts at the 0-based line, as a hover asks for them. It reads the check, so it runs inside a read.
    let private partsAt (check: FSharpCheckFileResults) (line: int) (lineStr: string, names: string list, endColumn: int) : Signature =
        check.GetToolTipParts(line + 1, endColumn, lineStr, names, FSharpTokenTag.Identifier)

    /// Whether a signature names an `@` owner from outside its declaration: one kept by the published result, not by its row
    let private namesExternalPlace (path: string) (view: FileView) (line: int) ((_, sources): Signature) =
        let myOwner =
            match view.RowAt(line + 1) with
            | ValueSome(struct (result, _)) -> ValueSome result
            | ValueNone -> ValueNone

        sources
        |> List.exists (function
            | Some s ->
                s.Owners @ s.InheritOwners
                |> List.exists (fun o ->
                    if not (String.Equals(o.FileName, path, StringComparison.OrdinalIgnoreCase)) then
                        false
                    elif o.Path = "@?" then
                        true
                    elif o.Path.StartsWith "@" then
                        match o.Path.Substring(1).Split(',') with
                        | [| lStr; _ |] ->
                            match Int32.TryParse lStr with
                            | true, defLine ->
                                match view.RowAt defLine, myOwner with
                                | ValueSome(struct (defOwner, _)), ValueSome owner -> not (obj.ReferenceEquals(defOwner, owner))
                                | _ -> true
                            | _ -> true
                        | _ -> true
                    else
                        false)
            | None -> false)

    let internal signatureOf (path: string) (view: FileView) (line: int) (lineStr: string, names: string list, endColumn: int) : Signature =
        let owner, row =
            match view.RowAt(line + 1) with
            | ValueSome(struct (result, first)) -> result, line + 1 - first
            | ValueNone -> box view.Result.CheckResults, line

        let cache = signatures.GetValue(owner, fun _ -> ConcurrentDictionary())

        let render (check: FSharpCheckFileResults) =
            rendered.AddOrUpdate(path, 1L, (fun _ n -> n + 1L)) |> ignore
            partsAt check line (lineStr, names, endColumn)

        let placed (predicate: string -> bool) ((_, sources): Signature) =
            sources
            |> List.exists (function
                | Some s -> s.Owners @ s.InheritOwners |> List.exists (fun o -> predicate o.Path)
                | None -> false)

        let joined = String.Join("\n", names)
        let key = struct (row, endColumn, lineStr, joined)
        let placedKey = struct (line, endColumn, lineStr, joined)

        let cachedPlaced () =
            match placedSignatures.TryGetValue view.Identity with
            | true, byResult ->
                match byResult.TryGetValue placedKey with
                | true, s -> ValueSome s
                | _ -> ValueNone
            | _ -> ValueNone

        match cache.TryGetValue key with
        | true, s -> s
        | _ ->
            match cachedPlaced () with
            | ValueSome s -> s
            | ValueNone ->
                let s = render view.Result.CheckResults

                // An `@` owner outside this row is the place of an extension's value as the check has it now. A kept row's result can
                // outlive that: the row that makes the value can be checked again, or shifted, after it. So it is kept
                // by the published result only. Inner bindings (locals, class lets) are inside this row and kept with it.
                if not (namesExternalPlace path view line s) then
                    cache.TryAdd(key, s) |> ignore
                    s
                else
                    // `@?` is a value a kept row still refers to after the row that made it was checked again: no key
                    // names it in the parse. Checked again in the current environment (RowView.at), as any query reading
                    // its names would be, the row refers to the current value, which a binding of the check makes and
                    // whose place is in the parse.
                    placedRendered.AddOrUpdate(path, 1L, (fun _ n -> n + 1L)) |> ignore
                    let s = if placed (fun p -> p = "@?") s then render (view.At (line + 1) endColumn) else s
                    placedSignatures.GetValue(view.Identity, fun _ -> ConcurrentDictionary()).TryAdd(placedKey, s) |> ignore
                    s

    /// Tooltip compose that checks whether any workspace owner was missing from docs, falling back to a stale check doc
    let internal composeWithMiss
        (isWorkspaceOwner: FSharpDocOwner -> bool)
        (commentOf: FSharpDocOwner -> FSharp.Compiler.Xml.XmlDoc voption)
        ((ToolTipText elements, sources): Signature)
        : ToolTipText * bool =
        let mutable rest = sources
        let mutable hasMiss = false

        let next () =
            match rest with
            | s :: tail ->
                rest <- tail
                s
            | [] -> None

        let composedElements =
            elements
            |> List.map (function
                | ToolTipElement.Group items ->
                    ToolTipElement.Group(
                        items
                        |> List.map (fun item ->
                            match next () with
                            | Some source ->
                                let wsOwners = source.Owners @ source.InheritOwners |> List.filter isWorkspaceOwner
                                if not wsOwners.IsEmpty then
                                    let missed =
                                        wsOwners |> List.exists (fun o ->
                                            if o.Path = "@?" then true
                                            else
                                                match commentOf o with
                                                | ValueNone -> true
                                                | ValueSome _ -> false)
                                    if missed then hasMiss <- true

                                { item with XmlDoc = source.Compose commentOf }
                            | None ->
                                item)
                    )
                | other -> other)
        ToolTipText composedElements, hasMiss

    /// The tooltip with each element's documentation made from the current comments of the declarations it is read
    /// from, as `commentOf` gives them
    let internal compose (commentOf: FSharpDocOwner -> FSharp.Compiler.Xml.XmlDoc voption) (signature: Signature) : ToolTipText =
        fst (composeWithMiss (fun _ -> false) commentOf signature)

    /// Runs between a hover's signature and its docs, with the file. For tests.
    let mutable internal afterSignature: string -> Task = fun _ -> Task.CompletedTask

    /// The first line of a hover answered from the last state while the file's check is pending
    [<Literal>]
    let StaleMark = "*From before the latest edit: still checking.*"

    let private linesOf (doc: FSharp.Compiler.Xml.XmlDoc voption) =
        doc |> ValueOption.map _.UnprocessedLines

    /// An answer's place relative to its owner (a row's result, or a published result): the line from the owner's first
    /// line, the column the long name ends at, the line's text and the long name
    type internal AnswerKey = (struct (int * int * string * string))

    /// A hover given on a settled result, with every comment it showed by owner, to be the same when it is given again
    [<NoEquality; NoComparison>]
    type internal Answer =
        { Markdown: string
          /// Other files' graphs, weakly, and the files their comments are read from
          Docs: struct (WeakReference<ProjectGraph> * string)[]
          /// Each owner's file, path and comment lines, the asking file's included
          Comments: (string * string * string[] voption)[]
          mutable Node: LinkedListNode<struct (WeakReference * AnswerKey)> }

    /// <summary>
    /// The hovers a server gave on settled results, by the row result that answered them and the place relative to the
    /// row, or by the published result for a place outside every row or a signature that names an extension's place.
    /// </summary>
    /// <remarks>
    /// A kept row keeps its result across publishes, so an answer given on an earlier publish is still the answer of a
    /// later one that kept the row: its signature is (signatureOf), and its comments are compared when it is given
    /// again. Owners are held weakly, as are graphs: nothing here keeps a result or a graph alive. At most `capacity`
    /// answers, the least recently used dropped first. The shapes of the parses read, kept while the parse lives.
    /// </remarks>
    [<Sealed>]
    type LastAnswers(capacity: int) =
        let answers = ConditionalWeakTable<obj, Dictionary<AnswerKey, Answer>>()
        let order = LinkedList<struct (WeakReference * AnswerKey)>()
        let gate = obj ()
        // Lazy, so a second asker waits for the build under way instead of making another
        let rowShapes = ConditionalWeakTable<FSharpParseFileResults, Lazy<FileShape>>()
        let wholeShapes = ConditionalWeakTable<FSharpParseFileResults, Lazy<FileShape>>()
        let answered = System.Collections.Concurrent.ConcurrentDictionary<string, unit>(StringComparer.OrdinalIgnoreCase)
        let mutable marked = 0L

        new() = LastAnswers(4096)

        /// How many hovers were answered from the last state
        member _.Marked = Interlocked.Read &marked

        member internal _.CountMarked() = Interlocked.Increment &marked |> ignore

        member internal _.Count = lock gate (fun () -> order.Count)

        member internal _.Add(owner: obj, key: AnswerKey, answer: Answer) =
            lock gate (fun () ->
                let byOwner = answers.GetValue(owner, fun _ -> Dictionary())

                match byOwner.TryGetValue key with
                | true, known -> order.Remove known.Node
                | _ -> ()

                answer.Node <- order.AddFirst(struct (WeakReference owner, key))
                byOwner[key] <- answer

                while order.Count > capacity do
                    let struct (w, k) = order.Last.Value
                    order.RemoveLast()

                    match w.Target with
                    | null -> ()
                    | o ->
                        match answers.TryGetValue o with
                        | true, d -> d.Remove k |> ignore
                        | _ -> ())

        member internal _.Find(owner: obj, key: AnswerKey) : Answer voption =
            lock gate (fun () ->
                match answers.TryGetValue owner with
                | true, d ->
                    match d.TryGetValue key with
                    | true, a ->
                        order.Remove a.Node
                        order.AddFirst a.Node
                        ValueSome a
                    | _ -> ValueNone
                | _ -> ValueNone)

        /// The parse's shape, with rows when `byDeclaration`, made once while the parse lives
        member internal _.ShapeOf(text: string, parse: FSharpParseFileResults, byDeclaration: bool) : FileShape =
            if byDeclaration then
                rowShapes.GetValue(parse, fun p -> lazy (PositionMap.shapeOf text p true)).Value
            else
                wholeShapes.GetValue(parse, fun _ -> lazy (PositionMap.textOnly text)).Value

        /// The parse's shape, its directives and lexer states made, waiting at most until `ct`: the build goes on for
        /// whoever asks next
        member internal this.ShapeAsync(text: string, parse: FSharpParseFileResults, byDeclaration: bool, ct: CancellationToken) : Task<FileShape> =
            let table = if byDeclaration then rowShapes else wholeShapes
            let made = table.GetValue(parse, fun p -> lazy (if byDeclaration then PositionMap.shapeOf text p true else PositionMap.textOnly text))

            if made.IsValueCreated && made.Value.States.IsValueCreated && made.Value.Directives.IsValueCreated then
                Task.FromResult made.Value
            else
                Task
                    .Run<FileShape>(fun () ->
                        let s = made.Value
                        s.Directives.Force() |> ignore
                        s.States.Force() |> ignore
                        s)
                    .WaitAsync(ct)

        /// Notes that a hover of the file was answered into the cache
        member internal _.NoteAnswered(path: string) = answered.TryAdd(Path.GetFullPath path, ()) |> ignore

        /// Whether a hover of the file was answered into the cache
        member internal _.HasAnswered(path: string) = answered.ContainsKey(Path.GetFullPath path)

    /// <summary>
    /// Hovers at the position of `path`, once the graph has settled on the file's latest text (a request node,
    /// ProjectGraph.ReadFile), and gives the answer to `answers` when there are some. The signature is the check's,
    /// cached by the declaration result and position, or by the published result when it names an extension by its place
    /// (`@`, signatureOf). The documentation is made from the comments of the files it is read from: the asking file's
    /// from the parse its result is of, read with it, and each other file's from its latest parse
    /// (ProjectGraph.DocsInTurn), in this project, a later file, or another project `owners` names. A `///` edit is seen
    /// at once, with no check of anything.
    /// </summary>
    /// <remarks>
    /// Only the check is asked under the lock, which holds off the file's checks: the tokenizer reads the file from the
    /// top, before the lock, and `format` may load an XML file, after the read. A declaration in no file of the
    /// workspace, as in a referenced assembly, keeps the documentation FCS gives.
    /// </remarks>
    let private settledHover
        (answers: LastAnswers voption)
        (owners: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (onUnsettled: (unit -> unit) option)
        (ct: CancellationToken)
        : Task<string option> =
        let rec attempt () =
            task {
                let! signature =
                    graph.ReadFile(
                        path,
                        (fun view -> lookupOf view.Text line character),
                        (fun view lookup ->
                            lookup
                            |> Option.map (fun l ->
                                let s = signatureOf path view line l

                                // The answer's owner: its row's result, or the published result for a place outside every
                                // row and for an extension's place, which is of the published result only (signatureOf)
                                let owner =
                                    match (if namesExternalPlace path view line s then ValueNone else view.RowAt(line + 1)) with
                                    | ValueSome(struct (result, first)) -> struct (result, line + 1 - first)
                                    | ValueNone -> struct (view.Identity, line)

                                s, view.Identity, view.Docs, struct (owner, l))),
                        false,
                        onUnsettled,
                        ct
                    )

                match signature with
                | None -> return None
                | Some(((_, sources) as signature), identity, docsOfAsking, struct (struct (owner, relative), lookup)) ->
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
                            docs[path] <- docsOfAsking

                        let comments = ResizeArray()

                        let commentOf =
                            let of' =
                                FileDocs.CommentOf(fun file ->
                                    match docs.TryGetValue file with
                                    | true, d -> ValueSome d
                                    | _ -> ValueNone)

                            fun (o: FSharpDocOwner) ->
                                let c = of' o
                                comments.Add((o.FileName, o.Path, linesOf c))
                                c

                        let isWorkspaceOwner (o: FSharpDocOwner) =
                            o.Path = "@?" || o.Path.StartsWith "@"
                            || String.Equals(o.FileName, path, StringComparison.OrdinalIgnoreCase)
                            || held |> Array.exists (fun (_, f) -> String.Equals(f, o.FileName, StringComparison.OrdinalIgnoreCase))

                        let composed, hasMiss = composeWithMiss isWorkspaceOwner commentOf signature
                        let answer =
                            format composed
                            |> Option.map (fun md -> if hasMiss then StaleMark + "\n\n" + md else md)

                        match answer, answers with
                        | Some markdown, ValueSome cache ->
                            let lineStr, names, endColumn = lookup
                            cache.NoteAnswered path

                            cache.Add(
                                owner,
                                struct (relative, endColumn, lineStr, String.Join("\n", names)),
                                { Markdown = markdown
                                  Docs = held |> Array.map (fun (g, f) -> struct (WeakReference<ProjectGraph> g, f))
                                  Comments = comments.ToArray()
                                  Node = null }
                            )
                        | _ -> ()

                        return answer
            }

        attempt ()

    /// Hovers at the position of `path` once the graph has settled on the file's latest text (settledHover).
    let at (owners: string -> ProjectGraph list) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<string option> =
        settledHover ValueNone owners graph path line character None ct

    /// What a hover from the last state found: a refusal, which means wait for the check, or the answer with the place
    /// in the old text it is of
    [<RequireQualifiedAccess>]
    type internal LastAnswer =
        | Refused of Refusal
        /// The file's result is settled: a hover answers it at once
        | Settled
        | Marked of markdown: string * at: OldPosition

    /// <summary>
    /// The hover at the position of `path`'s latest text from the file's last published result, while its check is
    /// pending: an answer given on a settled result, found by the row result at the place the position had in that result's
    /// text (PositionMap.map), marked. Never renders: a place not answered is refused.
    /// </summary>
    /// <remarks>
    /// `busy` (a reload under way) is read in the turn that reads the text and the published result, and again in the
    /// turn that reads the answer's comments. The comments are compared: the asking file's with the published result's
    /// parse, other files' with their latest parses; any that differs, or is pending, refuses. Reads the file's text and
    /// parse, never its check, so it needs no lock and waits for no check.
    /// </remarks>
    let internal fromLastState
        (answers: LastAnswers)
        (busy: unit -> bool)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<LastAnswer> =
        task {
            let! latest = graph.LatestText(path, [||], busy, ct)

            if latest.Busy then
                return LastAnswer.Refused Refusal.Reloading
            elif latest.Settled then
                return LastAnswer.Settled
            else
                match latest.Published with
                | ValueNone -> return LastAnswer.Refused Refusal.NoSnapshot
                | ValueSome published ->
                    let old = answers.ShapeOf(published.Text, published.Parse, not published.Whole)

                    let current =
                        if String.Equals(latest.View.Text, published.Text, StringComparison.Ordinal) then old
                        elif old.Rows.Length = 0 then PositionMap.textOnly latest.View.Text
                        else answers.ShapeOf(latest.View.Text, latest.View.Parse, true)

                    match PositionMap.map old current line character with
                    | Error refusal -> return LastAnswer.Refused refusal
                    | Ok at ->
                        match lookupIn old at.OldLine at.OldCharacter with
                        | None -> return LastAnswer.Refused Refusal.NotAnswered
                        | Some(lineStr, names, endColumn) ->
                            let joined = String.Join("\n", names)
                            let oneBased = at.OldLine + 1

                            let owners =
                                [ match published.Rows |> Array.tryFind (fun struct (_, first, last) -> first <= oneBased && oneBased <= last) with
                                  | Some(struct (result, first, _)) -> yield result, oneBased - first
                                  | None -> ()
                                  yield published.Identity, at.OldLine ]

                            match owners |> List.tryPick (fun (o, relative) -> answers.Find(o, struct (relative, endColumn, lineStr, joined)) |> ValueOption.toOption) with
                            | None -> return LastAnswer.Refused Refusal.NotAnswered
                            | Some answer ->
                                let graphs =
                                    answer.Docs
                                    |> Array.map (fun struct (w, f) ->
                                        match w.TryGetTarget() with
                                        | true, g -> Some(g, f)
                                        | _ -> None)

                                if graphs |> Array.exists Option.isNone then
                                    return LastAnswer.Refused Refusal.DocsChanged
                                else
                                    let docsOf = graphs |> Array.choose id
                                    let! again = graph.LatestText(path, docsOf, busy, ct)

                                    let samePublished =
                                        match again.Published with
                                        | ValueSome p -> obj.ReferenceEquals(p.Identity, published.Identity)
                                        | ValueNone -> false

                                    if again.Busy then
                                        return LastAnswer.Refused Refusal.Reloading
                                    elif again.Settled then
                                        return LastAnswer.Settled
                                    elif again.View.Version <> latest.View.Version || not samePublished then
                                        return LastAnswer.Refused Refusal.Moved
                                    else
                                        let docs = Dictionary<string, FileDocs>(StringComparer.OrdinalIgnoreCase)
                                        let mutable pending = false

                                        for (_, file), d in Array.zip docsOf again.Docs do
                                            match d with
                                            | ValueSome d -> docs[file] <- d
                                            | ValueNone -> pending <- true

                                        // The asking file's are of the published result's parse
                                        docs[path] <- published.Docs
                                        let commentOf = FileDocs.CommentOf(fun file -> match docs.TryGetValue file with | true, d -> ValueSome d | _ -> ValueNone)

                                        let same =
                                            answer.Comments
                                            |> Array.forall (fun (file, ownerPath, lines) -> linesOf (commentOf { FileName = file; Path = ownerPath }) = lines)

                                        if pending || not same then
                                            return LastAnswer.Refused Refusal.DocsChanged
                                        else
                                            answers.CountMarked()
                                            return LastAnswer.Marked(StaleMark + "\n\n" + answer.Markdown, at)
        }

    /// <summary>
    /// A hover: from the last state, marked (fromLastState), when the file's check is pending and the position is safe
    /// and was answered on a settled result; otherwise once the check settles, as `at`, giving the answer to `answers`.
    /// Whichever completes first decides, and a settled answer once there is always the one given.
    /// </summary>
    let answer
        (answers: LastAnswers)
        (busy: unit -> bool)
        (owners: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<string option> =
        if not (answers.HasAnswered path) then
            settledHover (ValueSome answers) owners graph path line character None ct
        else
            task {
                use cts = CancellationTokenSource.CreateLinkedTokenSource ct
                let lastTcs = TaskCompletionSource<LastAnswer>()
                let mutable lastStarted = 0

                let startLastState () =
                    if Interlocked.Exchange(&lastStarted, 1) = 0 then
                        Task.Run(fun () ->
                            task {
                                try
                                    let! ans = fromLastState answers busy graph path line character cts.Token
                                    lastTcs.TrySetResult ans |> ignore
                                with
                                | :? OperationCanceledException ->
                                    lastTcs.TrySetResult (LastAnswer.Refused Refusal.NotAnswered) |> ignore
                                | e ->
                                    Diagnostics.Trace.WriteLine $"Rfs.Server: a hover from the last state failed: %O{e}"
                                    lastTcs.TrySetResult (LastAnswer.Refused Refusal.NotAnswered) |> ignore
                            } :> Task) |> ignore

                let settled = settledHover (ValueSome answers) owners graph path line character (Some startLastState) cts.Token
                settled.ContinueWith(fun (t: Task<_>) -> t.Exception |> ignore) |> ignore

                let! first = Task.WhenAny(settled :> Task, lastTcs.Task :> Task)

                if not (obj.ReferenceEquals(first, settled)) && not settled.IsCompleted then
                    match lastTcs.Task.Result with
                    | LastAnswer.Marked(markdown, _) ->
                        cts.Cancel()
                        return Some markdown
                    | _ -> return! settled
                else
                    let! found = settled
                    cts.Cancel()
                    return found
            }

    /// The name of the first `let` or `member` binding near the top of the text, as a hover would ask for it
    let private firstLookup (text: string) : (int * (string * string list * int)) option =
        let lines = text.Split '\n'
        let binding = Regex(@"^\s*(?:let|member)\s+(?:(?:inline|private|internal|mutable|rec|static)\s+)*(?:[A-Za-z_][A-Za-z0-9_]*\.)?([A-Za-z_][A-Za-z0-9_]*)")

        seq {
            for line in 0 .. min (lines.Length - 1) 400 do
                match binding.Match lines[line] with
                | m when m.Success -> yield line, m.Groups[1].Index
                | _ -> ()
        }
        |> Seq.truncate 16
        |> Seq.tryPick (fun (line, character) -> lookupOf text line character |> Option.map (fun l -> line, l))

    /// 0 not warmed, 1 warming, 2 warmed
    let mutable private warmed = 0

    /// <summary>
    /// Once in a process, compiles what a first hover runs: the tooltip parts of a binding's name, a file's comment walk,
    /// and a docs read of another file (ProjectGraph.DocsInTurn). On a real file of `graph`, whose answer is dropped.
    /// </summary>
    /// <remarks>
    /// The parts are rendered inside the read, under the file's lock, as a hover's are: holding it costs a file's checks
    /// and hovers one render, once. The docs read takes no lock and comes after it. A warm-up that renders nothing, or
    /// fails, leaves the next project's load to try again.
    /// </remarks>
    let warm (graph: ProjectGraph) (files: string list) : Task =
        if Interlocked.CompareExchange(&warmed, 1, 0) <> 0 then
            Task.CompletedTask
        else
            Task.Run<unit>(fun () ->
                task {
                    let mutable done' = false

                    try
                        try
                            match files |> List.filter (fun f -> not (f.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase))) with
                            | [] -> ()
                            | file :: rest ->
                                let! found =
                                    graph.ReadFile(
                                        file,
                                        (fun text -> firstLookup text.Text, text.Docs),
                                        (fun view (lookup, docs) ->
                                            lookup |> Option.map (fun (line, l) -> partsAt view.Result.CheckResults line l, view.Identity, docs)),
                                        CancellationToken.None
                                    )

                                match found with
                                | None -> ()
                                | Some(parts, identity, docs) ->
                                    format (compose (FileDocs.CommentOf(fun _ -> ValueSome docs)) parts) |> ignore

                                    match rest |> List.tryFind (fun f -> not (f.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase))) with
                                    | None -> ()
                                    | Some other ->
                                        do! graph.DocsInTurn(file, identity, [| graph, other |], CancellationToken.None) :> Task

                                    done' <- true
                        with ex ->
                            eprintfn "[ranvierautocomplete] hover warm-up failed: %O" ex
                    finally
                        Volatile.Write(&warmed, (if done' then 2 else 0))
                })
            :> Task
