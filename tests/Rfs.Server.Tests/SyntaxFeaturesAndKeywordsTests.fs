module Rfs.Server.Tests.SyntaxFeaturesAndKeywordsTests

open System
open System.IO
open System.Runtime.ExceptionServices
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 60.)
let private withinFast (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 8.)
let private withinUnit (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 60.)

let private parseChecker = lazy (FSharpChecker.Create(keepAssemblyContents = false))

let private runOnLargeStack (f: unit -> 'T) : 'T =
    let result = ref ValueNone
    let failure = ref null

    let thread =
        Thread(
            (fun () ->
                try
                    result.Value <- ValueSome(f ())
                with ex ->
                    failure.Value <- ExceptionDispatchInfo.Capture ex),
            GraphHost.StackSize,
            IsBackground = true
        )

    thread.Start()
    thread.Join()

    if not (isNull failure.Value) then
        failure.Value.Throw()

    result.Value.Value

let private parseSource (fileName: string) (source: string) : ParsedInput =
    runOnLargeStack (fun () ->
        let opts = { FSharpParsingOptions.Default with SourceFiles = [| fileName |] }
        let res = parseChecker.Value.ParseFile(fileName, SourceText.ofString source, opts) |> Async.RunSynchronously
        res.ParseTree)

let private posLeq (a: Ionide.LanguageServerProtocol.Types.Position) (b: Ionide.LanguageServerProtocol.Types.Position) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let private rangeContains (outer: Ionide.LanguageServerProtocol.Types.Range) (inner: Ionide.LanguageServerProtocol.Types.Range) =
    posLeq outer.Start inner.Start && posLeq inner.End outer.End

let private findPosOf (source: string) (needle: string) (nth: int) : Ionide.LanguageServerProtocol.Types.Position =
    let mutable idx = -1

    for _ in 0..nth do
        idx <- source.IndexOf(needle, idx + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual idx 0 $"Expected '%s{needle}' occurrence %d{nth} in source"
    let before = source.Substring(0, idx)
    let lines = before.Split('\n')
    let line = lines.Length - 1
    let col = lines[lines.Length - 1].TrimEnd('\r').Length
    { Line = uint32 line; Character = uint32 col }

let private findAfter (source: string) (needle: string) (nth: int) : int * int =
    let mutable idx = -1

    for _ in 0..nth do
        idx <- source.IndexOf(needle, idx + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual idx 0 $"Expected '%s{needle}' occurrence %d{nth} in source"
    let before = source.Substring(0, idx + needle.Length)
    let lines = before.Split('\n')
    let line = lines.Length - 1
    let col = lines[lines.Length - 1].TrimEnd('\r').Length
    line, col

let private sliceLspRange (source: string) (r: Ionide.LanguageServerProtocol.Types.Range) : string =
    let lines = source.Split('\n') |> Array.map (fun s -> s.TrimEnd('\r'))
    let sl, sc = int r.Start.Line, int r.Start.Character
    let el, ec = int r.End.Line, int r.End.Character
    Expect.isTrue (sl >= 0 && sl < lines.Length && el >= sl && el < lines.Length) $"Range %A{r} lines out of bounds"

    if sl = el then
        lines[sl].Substring(sc, ec - sc)
    else
        let parts = ResizeArray<string>()
        parts.Add(lines[sl].Substring(sc))

        for l in (sl + 1) .. (el - 1) do
            parts.Add(lines[l])

        parts.Add(lines[el].Substring(0, ec))
        String.concat "\n" parts

let rec private assertValidDocumentSymbol (sym: DocumentSymbol) : unit =
    Expect.isFalse (String.IsNullOrWhiteSpace sym.Name) $"DocumentSymbol name must not be whitespace: %A{sym}"

    Expect.isTrue
        (rangeContains sym.Range sym.SelectionRange)
        $"DocumentSymbol '%s{sym.Name}' SelectionRange %A{sym.SelectionRange} MUST be strictly inside Range %A{sym.Range}"

    match sym.Children with
    | None -> ()
    | Some children ->
        for child in children do
            assertValidDocumentSymbol child

            Expect.isTrue
                (rangeContains sym.Range child.Range)
                $"Parent DocumentSymbol '%s{sym.Name}' Range %A{sym.Range} must contain child '%s{child.Name}' Range %A{child.Range}"

let rec private assertValidJsonDocumentSymbol (sym: JsonNode) : unit =
    let name = sym["name"].GetValue<string>()
    Expect.isFalse (String.IsNullOrWhiteSpace name) "JSON DocumentSymbol name must be non-empty"

    let toRange (n: JsonNode) : Ionide.LanguageServerProtocol.Types.Range =
        { Start =
            { Line = n["start"].["line"].GetValue<uint32>()
              Character = n["start"].["character"].GetValue<uint32>() }
          End =
            { Line = n["end"].["line"].GetValue<uint32>()
              Character = n["end"].["character"].GetValue<uint32>() } }

    let fullRange = toRange sym["range"]
    let selRange = toRange sym["selectionRange"]

    Expect.isTrue
        (rangeContains fullRange selRange)
        $"JSON DocumentSymbol '%s{name}' selectionRange %A{selRange} MUST be strictly inside range %A{fullRange}"

    match sym["children"] with
    | null -> ()
    | :? JsonArray as arr ->
        for child in arr do
            assertValidJsonDocumentSymbol child
            let childRange = toRange child["range"]
            let childName = child["name"].GetValue<string>()

            Expect.isTrue
                (rangeContains fullRange childRange)
                $"JSON Parent '%s{name}' range %A{fullRange} must contain child '%s{childName}' range %A{childRange}"
    | _ -> ()

let private copyDeps () : string * IDisposable =
    let fixturesRoot = Path.GetDirectoryName PublishTests.depsDir
    let repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)
    let root = Path.Combine(Path.GetTempPath(), "rfs-synfeat-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let target = Path.Combine(root, "Deps")
    Directory.CreateDirectory target |> ignore
    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(root, "global.json"))

    for f in Directory.GetFiles PublishTests.depsDir do
        File.Copy(f, Path.Combine(target, Path.GetFileName f))

    let cleanup =
        { new IDisposable with
            member _.Dispose() =
                try
                    if Directory.Exists root then
                        Directory.Delete(root, true)
                with _ ->
                    () }

    target, cleanup

let private loadProject (lsp: LspClient) (projPath: string) : Task =
    task {
        let doc = JsonObject()
        doc["Uri"] <- JsonValue.Create(Paths.toUri projPath)
        let docs = JsonArray()
        docs.Add doc
        let p = JsonObject()
        p["TextDocuments"] <- docs
        let! _ = lsp.Request("fsharp/workspaceLoad", p) |> within
        ()
    }

let private waitUntil (predicate: unit -> bool) : Task =
    task {
        let sw = Diagnostics.Stopwatch.StartNew()

        while not (predicate ()) && sw.Elapsed < TimeSpan.FromSeconds 10. do
            do! Task.Delay 25

        Expect.isTrue (predicate ()) "Timed out waiting for condition"
    }

[<Tests>]
let unitTests =
    testList
        "SyntaxFeaturesAndKeywords.Unit"
        [ testCase "Folding.compute maps Open->Imports, Comment/XmlDoc->Comment, code->None with exact 0-based lines on .fs and .fsi"
          <| fun () ->
              let fsSource =
                  String.concat
                      "\r\n"
                      [ "module Deps.FoldSample" // line 0
                        "" // line 1
                        "open System" // line 2
                        "open System.IO" // line 3
                        "" // line 4
                        "// multi-line comment line 1" // line 5
                        "// multi-line comment line 2" // line 6
                        "" // line 7
                        "/// xml doc line 1" // line 8
                        "/// xml doc line 2" // line 9
                        "let compute x =" // line 10
                        "    let doubled = x * 2" // line 11
                        "    doubled + 1" ] // line 12

              let fsTree = parseSource "FoldSample.fs" fsSource
              let fsRanges = Folding.compute fsSource fsTree

              // Open block on 0-based lines 2..3 -> Imports
              let openFold =
                  fsRanges
                  |> Array.tryFind (fun r -> r.Kind = Some FoldingRangeKind.Imports)

              Expect.isSome openFold "Open block must produce FoldingRangeKind.Imports"
              Expect.equal openFold.Value.StartLine 2u "Open block 0-based StartLine = 2"
              Expect.equal openFold.Value.EndLine 3u "Open block 0-based EndLine = 3"

              // Single-line multi-row comment on 0-based lines 5..6 -> Comment
              let lineCommentFold =
                  fsRanges
                  |> Array.tryFind (fun r -> r.StartLine = 5u && r.EndLine = 6u)

              Expect.isSome lineCommentFold "Multi-line // comment on 0-based lines 5..6 must be present"
              Expect.equal lineCommentFold.Value.Kind (Some FoldingRangeKind.Comment) "Multi-line // comment kind = Comment"

              // XML doc comment on 0-based lines 8..9 -> Comment
              let xmlCommentFold =
                  fsRanges
                  |> Array.tryFind (fun r -> r.StartLine = 8u && r.EndLine = 9u)

              Expect.isSome xmlCommentFold "Multi-line /// doc comment on 0-based lines 8..9 must be present"
              Expect.equal xmlCommentFold.Value.Kind (Some FoldingRangeKind.Comment) "XML doc comment kind = Comment"

              // Let binding on 0-based lines 10..12 -> None
              let letFold =
                  fsRanges
                  |> Array.tryFind (fun r -> r.StartLine = 10u && r.EndLine = 12u)

              Expect.isSome letFold "Multi-line let binding on 0-based lines 10..12 must be present"
              Expect.isNone letFold.Value.Kind "Code scope LetOrUse must have Kind = None"

              // .fsi signature file folding
              let fsiSource =
                  String.concat
                      "\n"
                      [ "module Deps.FoldSig" // line 0
                        "" // line 1
                        "open System" // line 2
                        "open System.Text" // line 3
                        "" // line 4
                        "/// sig xml 1" // line 5
                        "/// sig xml 2" // line 6
                        "type IRunner =" // line 7
                        "    abstract Run: int -> string" ] // line 8

              let fsiTree = parseSource "FoldSig.fsi" fsiSource
              let fsiRanges = Folding.compute fsiSource fsiTree

              Expect.isTrue
                  (fsiRanges |> Array.exists (fun r -> r.StartLine = 2u && r.EndLine = 3u && r.Kind = Some FoldingRangeKind.Imports))
                  ".fsi open block must fold as Imports on 0-based lines 2..3"

              Expect.isTrue
                  (fsiRanges |> Array.exists (fun r -> r.StartLine = 5u && r.EndLine = 6u && r.Kind = Some FoldingRangeKind.Comment))
                  ".fsi /// comment block must fold as Comment on 0-based lines 5..6"

              Expect.isTrue
                  (fsiRanges |> Array.exists (fun r -> r.StartLine = 7u && r.EndLine = 8u && r.Kind.IsNone))
                  ".fsi multi-line type signature must fold with Kind = None on 0-based lines 7..8"

          testCase "SelectionRange.compute builds strictly widening innermost-to-outermost chains on .fs and .fsi with 1-to-1 length"
          <| fun () ->
              let fsSource =
                  String.concat
                      "\n"
                      [ "module Deps.SelSample"
                        ""
                        "let evaluate (itemOpt: int option) ="
                        "    match itemOpt with"
                        "    | Some innerVal -> innerVal + 10"
                        "    | None -> 0" ]

              let fsTree = parseSource "SelSample.fs" fsSource
              let posInnerVal = findPosOf fsSource "innerVal + 10" 0
              let posEvaluate = findPosOf fsSource "evaluate" 0
              let posBeyondEof: Ionide.LanguageServerProtocol.Types.Position = { Line = 99u; Character = 5u }

              let fsResults = SelectionRange.compute fsSource fsTree [| posInnerVal; posEvaluate; posBeyondEof |]
              Expect.equal fsResults.Length 3 "SelectionRange array length must equal requested positions length (1-to-1)"

              let rec collectChain (sr: SelectionRange) =
                  [ yield sr.Range
                    match sr.Parent with
                    | Some p -> yield! collectChain p
                    | None -> () ]

              let innerChain = collectChain fsResults[0]
              Expect.isGreaterThanOrEqual innerChain.Length 5 "Cursor on innerVal must have >= 5 nested selection steps"
              Expect.equal (sliceLspRange fsSource innerChain.Head) "innerVal" "Innermost SelectionRange must be exact identifier 'innerVal'"

              // Every parent must strictly contain and differ from its child
              for child, parent in List.pairwise innerChain do
                  Expect.notEqual child parent "SelectionRange chain must deduplicate identical ranges"
                  Expect.isTrue (rangeContains parent child) $"Parent %A{parent} must strictly contain child %A{child}"

              let chainSlices = innerChain |> List.map (sliceLspRange fsSource)
              Expect.contains chainSlices "innerVal + 10" "Chain must include binary expression 'innerVal + 10'"
              Expect.equal (List.last innerChain).Start { Line = 0u; Character = 0u } "Outermost SelectionRange starts at (0,0)"

              let evalChain = collectChain fsResults[1]
              Expect.equal (sliceLspRange fsSource evalChain.Head) "evaluate" "Second position innermost SelectionRange is 'evaluate'"

              // Out-of-bounds position still gets a valid fallback SelectionRange
              Expect.equal fsResults[2].Range.Start posBeyondEof "Out-of-bounds position gets zero-width fallback at requested position"

              // .fsi signature file SelectionRange
              let fsiSource =
                  String.concat
                      "\n"
                      [ "module Deps.SelSig"
                        ""
                        "type IProcessor ="
                        "    abstract Process: (int * string) -> bool"
                        ""
                        "val computeTotal: count: int -> string" ]

              let fsiTree = parseSource "SelSig.fsi" fsiSource
              let posTupleString = findPosOf fsiSource "string)" 0
              let posValName = findPosOf fsiSource "computeTotal" 0
              let fsiResults = SelectionRange.compute fsiSource fsiTree [| posTupleString; posValName |]
              Expect.equal fsiResults.Length 2 ".fsi SelectionRange array length matches positions"

              let tupleChain = collectChain fsiResults[0]
              Expect.isGreaterThanOrEqual tupleChain.Length 4 ".fsi cursor on 'string' inside tuple type must walk through SynType -> MemberSig -> TypeDefnSig -> ModuleSig"
              Expect.equal (sliceLspRange fsiSource tupleChain.Head) "string" ".fsi innermost range on 'string' is exact type ident"
              let tupleSlices = tupleChain |> List.map (sliceLspRange fsiSource)
              Expect.contains tupleSlices "int * string" ".fsi chain includes tuple SynType 'int * string'"

              for child, parent in List.pairwise tupleChain do
                  Expect.notEqual child parent ".fsi chain deduplicates ranges"
                  Expect.isTrue (rangeContains parent child) $".fsi parent %A{parent} must contain child %A{child}"

              let valChain = collectChain fsiResults[1]
              Expect.equal (sliceLspRange fsiSource valChain.Head) "computeTotal" ".fsi innermost range on val is 'computeTotal'"
              Expect.isGreaterThanOrEqual valChain.Length 3 ".fsi val chain includes ident -> SynValSig -> ModuleOrNamespaceSig"

          testTask "SelectionRange, Folding, and DocumentSymbols handle deeply nested ASTs on small-stack ThreadPool without StackOverflow" {
              // Build a 2500-deep nested let/paren expression and run on a default ThreadPool worker
              let sb = Text.StringBuilder()
              sb.Append("module Deps.DeepAst\nlet deep =\n    ") |> ignore

              for _ in 1..2500 do
                  sb.Append("(1 + ") |> ignore

              sb.Append("0") |> ignore

              for _ in 1..2500 do
                  sb.Append(")") |> ignore

              let deepSource = sb.ToString()
              let deepTree = parseSource "DeepAst.fs" deepSource

              let! chainLen =
                  Task.Run(fun () ->
                      let _ = Folding.compute deepSource deepTree
                      let _ = DocumentSymbols.compute deepTree
                      let sel = SelectionRange.compute deepSource deepTree [| { Line = 2u; Character = uint32 (4 + 2500 * 5) } |]
                      let mutable count = 0
                      let mutable cur = Some sel[0]

                      while cur.IsSome do
                          count <- count + 1
                          cur <- cur.Value.Parent

                      count)
                  |> within

              Expect.isGreaterThan chainLen 2500 "Deeply nested AST produces >2500 SelectionRange levels stack-safely"
          }

          testCase "DocumentSymbols.compute builds hierarchical symbols with strict SelectionRange containment on .fs and .fsi"
          <| fun () ->
              let fsSource =
                  String.concat
                      "\n"
                      [ "module Deps.DocSymSample"
                        ""
                        "[<Literal>]"
                        "let MaxRetries = 5"
                        ""
                        "[<Struct>]"
                        "type Metric ="
                        "    { Label: string"
                        "      Score: float }"
                        ""
                        "type Status ="
                        "    | Idle = 0"
                        "    | Running = 1"
                        ""
                        "type Engine() ="
                        "    member _.Step(delta: int) = delta + MaxRetries"
                        ""
                        "let runAll x = x + 1" ]

              let fsTree = parseSource "DocSymSample.fs" fsSource
              let fsSyms = DocumentSymbols.compute fsTree
              Expect.isNonEmpty fsSyms ".fs DocumentSymbols must be non-empty"
              Array.iter assertValidDocumentSymbol fsSyms

              let findSym name (syms: DocumentSymbol[]) =
                  syms
                  |> Array.tryFind (fun s -> s.Name = name || s.Name.EndsWith("." + name, StringComparison.Ordinal))
                  |> Option.defaultWith (fun () ->
                      let names = syms |> Array.map _.Name
                      failtestf "Expected symbol '%s', found %A" name names)

              let modSym = findSym "Deps.DocSymSample" fsSyms
              Expect.equal modSym.Kind SymbolKind.Module "Module symbol kind"
              Expect.isSome modSym.Children "Top-level module must expose nested Children"
              let modChildNames = modSym.Children.Value |> Array.map _.Name
              Expect.contains modChildNames "MaxRetries" "Module Children includes MaxRetries"
              Expect.contains modChildNames "runAll" "Module Children includes runAll"

              let metricSym = findSym "Metric" fsSyms
              Expect.equal metricSym.Range.Start.Line 5u "Attributed [<Struct>] type Metric Range must include attribute line 5"
              Expect.isSome metricSym.Children "Record Metric must expose field Children"
              let metricFields = metricSym.Children.Value |> Array.map _.Name
              Expect.contains metricFields "Label" "Record Children includes Label"
              Expect.contains metricFields "Score" "Record Children includes Score"

              let statusSym = findSym "Status" fsSyms
              Expect.equal statusSym.Kind SymbolKind.Enum "Enum Status kind"
              Expect.isSome statusSym.Children "Enum Status must expose EnumMember Children"
              Expect.isTrue
                  (statusSym.Children.Value |> Array.exists (fun c -> c.Name = "Idle" && c.Kind = SymbolKind.EnumMember))
                  "Enum child Idle has SymbolKind.EnumMember"

              let engineSym = findSym "Engine" fsSyms
              Expect.equal engineSym.Kind SymbolKind.Class "Class Engine kind"
              Expect.isSome engineSym.Children "Class Engine must expose member Children"
              Expect.isTrue
                  (engineSym.Children.Value |> Array.exists (fun c -> c.Name = "Step" && c.Kind = SymbolKind.Method))
                  "Class child Step has SymbolKind.Method"

              // .fsi signature file DocumentSymbols (including attributed type + val + anonymous/global namespace fallback)
              let fsiSource =
                  String.concat
                      "\n"
                      [ "module Deps.DocSymSig"
                        ""
                        "[<Sealed>]"
                        "type SigEngine ="
                        "    new: unit -> SigEngine"
                        "    member Step: int -> int"
                        ""
                        "val runAll: int -> int" ]

              let fsiTree = parseSource "DocSymSig.fsi" fsiSource
              let fsiSyms = DocumentSymbols.compute fsiTree
              Expect.isNonEmpty fsiSyms ".fsi DocumentSymbols must be non-empty"
              Array.iter assertValidDocumentSymbol fsiSyms

              let sigMod = findSym "Deps.DocSymSig" fsiSyms
              Expect.isSome sigMod.Children ".fsi module must expose val Children"
              Expect.contains (sigMod.Children.Value |> Array.map _.Name) "runAll" ".fsi module Children includes val runAll"

              let sigEngine = findSym "SigEngine" fsiSyms
              Expect.equal sigEngine.Range.Start.Line 2u ".fsi [<Sealed>] type SigEngine Range starts at attribute line 2"
              Expect.isSome sigEngine.Children ".fsi SigEngine exposes member Children"
              Expect.contains (sigEngine.Children.Value |> Array.map _.Name) "Step" ".fsi SigEngine Children includes Step" ]

let private runLspProtocolTest () =
    task {
              let targetDir, cleanup = copyDeps ()
              use _ = cleanup
              let projPath = Path.Combine(targetDir, "Deps.fsproj")
              let dPath = Path.Combine(targetDir, "D.fs")
              let eSigPath = Path.Combine(targetDir, "E.fsi")

              let! lsp, owner, serverTask = TestServer.startCapturing ()
              use _ = owner

              let initParams =
                  JsonNode.Parse
                      $"""{{"processId":null,"rootUri":%s{JsonValue.Create(Paths.toUri targetDir).ToJsonString()},"capabilities":{{}}}}"""

              let! initReply, _ = lsp.Request("initialize", initParams) |> within
              do! lsp.Notify("initialized", JsonObject()) |> withinUnit
              let! server = serverTask |> within

              // 1) Verify capabilities in InitializeResult
              let caps = initReply["capabilities"]
              Expect.isTrue (caps["foldingRangeProvider"].GetValue<bool>()) "foldingRangeProvider must be true"
              Expect.isTrue (caps["selectionRangeProvider"].GetValue<bool>()) "selectionRangeProvider must be true"
              Expect.isTrue (caps["documentSymbolProvider"].GetValue<bool>()) "documentSymbolProvider must be true"

              let triggerChars =
                  [| for c in caps["completionProvider"].["triggerCharacters"].AsArray() -> c.GetValue<string>() |]

              Expect.equal triggerChars [| "."; "'"; "{" |] "completionProvider.triggerCharacters must include '{'"

              do! loadProject lsp projPath
              let graph = (server.Workspace.ProjectOf dPath).Value

              // Open D.fs and E.fsi and let promotion / initial check settle
              let initialD = File.ReadAllText dPath
              let initialESig = File.ReadAllText eSigPath
              do! lsp.DidOpen(dPath, initialD) |> withinUnit
              do! lsp.DidOpen(eSigPath, initialESig) |> withinUnit
              let! _ = graph.WhenPromoted(dPath, CancellationToken.None) |> within
              let! _ = graph.AllDiagnostics() |> within

              // 2) CRITICAL NON-BLOCKING PROOF (.fs AND .fsi):
              //    Block typechecking (`dependencies` and `finish` flights) on the ProjectGraph, edit BOTH D.fs and E.fsi
              //    via textDocument/didChange, verify typecheck-dependent semanticTokens/full is genuinely blocked, and
              //    prove foldingRange, selectionRange, and documentSymbol ALL answer immediately from the NEW edited text!
              use gate = new ManualResetEventSlim(false)

              graph.OnFlight(fun kind _ ct ->
                  if kind = "dependencies" || kind = "finish" then
                      gate.Wait ct)

              let editedD =
                  String.concat
                      "\n"
                      [ "module Deps.D"
                        ""
                        "open System"
                        "open System.Text"
                        ""
                        "// blocked-edit comment 1"
                        "// blocked-edit comment 2"
                        ""
                        "[<Struct>]"
                        "type FreshRecord = { FreshField: int }"
                        ""
                        "let newlyAddedWhileBlocked (freshArg: FreshRecord) ="
                        "    freshArg.FreshField + 42" ]

              let editedESig =
                  String.concat
                      "\n"
                      [ "module Deps.E"
                        ""
                        "open System"
                        "open System.Text"
                        ""
                        "/// blocked-sig doc 1"
                        "/// blocked-sig doc 2"
                        "val user: int"
                        "val newlyAddedSigWhileBlocked: (int * string) -> int" ]

              try
                  let fullLineCount (s: string) = s.Split('\n').Length

                  do!
                      lsp.DidChange(
                          dPath,
                          1,
                          { Range = { Start = { Line = 0; Character = 0 }; End = { Line = fullLineCount initialD + 2; Character = 0 } }
                            Text = editedD }
                      )
                      |> withinUnit

                  do!
                      lsp.DidChange(
                          eSigPath,
                          1,
                          { Range = { Start = { Line = 0; Character = 0 }; End = { Line = fullLineCount initialESig + 2; Character = 0 } }
                            Text = editedESig }
                      )
                      |> withinUnit

                  let docJson (p: string) =
                      JsonNode.Parse $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri p).ToJsonString()}}}}}"""

                  // Start typecheck-backed requests to prove typechecking is actively blocked on both D.fs and E.fsi
                  let blockedFsCheck = lsp.Request("textDocument/semanticTokens/full", docJson dPath)
                  let blockedFsiCheck = lsp.Request("textDocument/semanticTokens/full", docJson eSigPath)
                  do! Task.Delay 150
                  Expect.isFalse blockedFsCheck.IsCompleted "D.fs typecheck must still be blocked by FlightHook"
                  Expect.isFalse blockedFsiCheck.IsCompleted "E.fsi signature check must still be blocked by FlightHook"

                  // Query foldingRange, selectionRange, and documentSymbol on D.fs (.fs) while typecheck is blocked!
                  let! fsFoldReply, _ = lsp.Request("textDocument/foldingRange", docJson dPath) |> withinFast
                  Expect.isNotNull fsFoldReply "foldingRange on .fs must return immediately while typecheck is blocked"
                  let fsFolds = fsFoldReply.AsArray()

                  Expect.isTrue
                      (fsFolds |> Seq.exists (fun f -> f["startLine"].GetValue<int>() = 2 && f["endLine"].GetValue<int>() = 3 && f["kind"] <> null && f["kind"].GetValue<string>() = "imports"))
                      ".fs foldingRange reflects newly edited open block (lines 2..3) without waiting for check"

                  Expect.isTrue
                      (fsFolds |> Seq.exists (fun f -> f["startLine"].GetValue<int>() = 5 && f["endLine"].GetValue<int>() = 6 && f["kind"] <> null && f["kind"].GetValue<string>() = "comment"))
                      ".fs foldingRange reflects newly edited comment block (lines 5..6) without waiting for check"

                  let fsSelPos = findPosOf editedD "FreshField + 42" 0

                  let selJson (p: string) (positions: Ionide.LanguageServerProtocol.Types.Position[]) =
                      let posArr =
                          positions
                          |> Array.map (fun pos -> $"""{{"line":%d{pos.Line},"character":%d{pos.Character}}}""")
                          |> String.concat ","

                      JsonNode.Parse
                          $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri p).ToJsonString()}}},"positions":[%s{posArr}]}}"""

                  let! fsSelReply, _ =
                      lsp.Request("textDocument/selectionRange", selJson dPath [| fsSelPos; { Line = 0u; Character = 7u } |])
                      |> withinFast

                  Expect.isNotNull fsSelReply "selectionRange on .fs must return immediately while typecheck is blocked"
                  let fsSelArr = fsSelReply.AsArray()
                  Expect.equal fsSelArr.Count 2 ".fs selectionRange returns 1-to-1 array for 2 positions"
                  Expect.isNotNull fsSelArr[0].["parent"] ".fs selectionRange has nested parent chain on newly added FreshField"

                  let! fsSymReply, _ = lsp.Request("textDocument/documentSymbol", docJson dPath) |> withinFast
                  Expect.isNotNull fsSymReply "documentSymbol on .fs must return immediately while typecheck is blocked"
                  let fsSymArr = fsSymReply.AsArray()
                  for sym in fsSymArr do
                      assertValidJsonDocumentSymbol sym

                  let allFsNestedNames =
                      [ for sym in fsSymArr do
                            yield sym["name"].GetValue<string>()
                            match sym["children"] with
                            | :? JsonArray as ch ->
                                for c in ch -> c["name"].GetValue<string>()
                            | _ -> () ]

                  Expect.contains allFsNestedNames "newlyAddedWhileBlocked" ".fs documentSymbol reflects newly edited binding immediately"
                  Expect.contains allFsNestedNames "FreshField" ".fs documentSymbol includes nested FreshField child immediately"

                  // Query foldingRange, selectionRange, and documentSymbol on E.fsi (.fsi) while check is STILL blocked!
                  let! fsiFoldReply, _ = lsp.Request("textDocument/foldingRange", docJson eSigPath) |> withinFast
                  Expect.isNotNull fsiFoldReply "foldingRange on .fsi must return immediately while signature check is blocked"

                  Expect.isTrue
                      (fsiFoldReply.AsArray() |> Seq.exists (fun f -> f["startLine"].GetValue<int>() = 2 && f["endLine"].GetValue<int>() = 3 && f["kind"] <> null && f["kind"].GetValue<string>() = "imports"))
                      ".fsi foldingRange reflects newly edited open block immediately"

                  let fsiSelPos = findPosOf editedESig "string) -> int" 0
                  let! fsiSelReply, _ = lsp.Request("textDocument/selectionRange", selJson eSigPath [| fsiSelPos |]) |> withinFast
                  Expect.isNotNull fsiSelReply "selectionRange on .fsi must return immediately while signature check is blocked"
                  let fsiSelArr = fsiSelReply.AsArray()
                  Expect.equal fsiSelArr.Count 1 ".fsi selectionRange 1-to-1 count"
                  Expect.isNotNull fsiSelArr[0].["parent"] ".fsi selectionRange has nested parent chain"

                  let! fsiSymReply, _ = lsp.Request("textDocument/documentSymbol", docJson eSigPath) |> withinFast
                  Expect.isNotNull fsiSymReply "documentSymbol on .fsi must return immediately while signature check is blocked"
                  let fsiSymArr = fsiSymReply.AsArray()
                  for sym in fsiSymArr do
                      assertValidJsonDocumentSymbol sym

                  let allFsiNestedNames =
                      [ for sym in fsiSymArr do
                            yield sym["name"].GetValue<string>()
                            match sym["children"] with
                            | :? JsonArray as ch ->
                                for c in ch -> c["name"].GetValue<string>()
                            | _ -> () ]

                  Expect.contains allFsiNestedNames "newlyAddedSigWhileBlocked" ".fsi documentSymbol reflects newly edited val immediately"

                  // Confirm typechecking WAS STILL BLOCKED the entire time!
                  Expect.isFalse blockedFsCheck.IsCompleted "D.fs typecheck remained blocked while all 3 pure-syntax endpoints answered"
                  Expect.isFalse blockedFsiCheck.IsCompleted "E.fsi signature check remained blocked while all 3 pure-syntax endpoints answered"

                  // Now release the gate and let blocked checks finish cleanly
                  gate.Set()
                  let! _ = blockedFsCheck |> within
                  let! _ = blockedFsiCheck |> within
                  ()
              finally
                  gate.Set()
                  graph.OnFlight(fun _ _ _ -> ())

              // 3) Completion Parity: KeywordsAutocomplete = false (default) vs true vs dot-member guard (`r.` and `"hi".`)
              let compSource =
                  String.concat
                      "\n"
                      [ "module Deps.D"
                        ""
                        "type Boxed = { Amount: int }"
                        ""
                        "let demo (b: Boxed) ="
                        "    let fromDot = b."
                        "    let fromStrDot = \"hi\"."
                        "    let fromModDot = System.IO."
                        "    let inExpr = ma"
                        "    let inLiteral = \"ab\""
                        "    inExpr" ]

              do!
                  lsp.DidChange(
                      dPath,
                      2,
                      { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 20; Character = 0 } }
                        Text = compSource }
                  )
                  |> withinUnit

              let compReq (line: int, ch: int) =
                  JsonNode.Parse
                      $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri dPath).ToJsonString()}}},"position":{{"line":%d{line},"character":%d{ch}}}}}"""

              let exprPos = findAfter compSource "let inExpr = ma" 0
              let dotPos = findAfter compSource "let fromDot = b." 0
              let strDotPos = findAfter compSource "let fromStrDot = \"hi\"." 0
              let modDotPos = findAfter compSource "let fromModDot = System.IO." 0
              let inStrPos = findAfter compSource "let inLiteral = \"a" 0

              // (a) Default KeywordsAutocomplete = false -> NO keywords in expression completion
              let! defaultComp, _ = lsp.Request("textDocument/completion", compReq exprPos) |> within
              let defaultLabels = [| for i in defaultComp["items"].AsArray() -> i["label"].GetValue<string>() |] |> Set.ofArray
              Expect.contains defaultLabels "b" "Default completion includes parameter 'b'"
              Expect.isFalse (defaultLabels.Contains "match") "KeywordsAutocomplete=false must omit 'match' keyword"
              Expect.isFalse (defaultLabels.Contains "while") "KeywordsAutocomplete=false must omit 'while' keyword"

              // (b) Enable KeywordsAutocomplete = true via workspace/didChangeConfiguration
              do!
                  lsp.Notify(
                      "workspace/didChangeConfiguration",
                      JsonNode.Parse """{"settings":{"FSharp":{"keywordsAutocomplete":true}}}"""
                  )
                  |> withinUnit

              do! waitUntil (fun () -> server.Config.KeywordsAutocomplete)

              let! kwComp, _ = lsp.Request("textDocument/completion", compReq exprPos) |> within
              let kwItems = kwComp["items"].AsArray()
              let findItem label = kwItems |> Seq.tryFind (fun i -> i["label"].GetValue<string>() = label)

              let matchItem = findItem "match"
              Expect.isSome matchItem "KeywordsAutocomplete=true must include 'match' keyword in expression completion"
              Expect.equal (matchItem.Value["kind"].GetValue<int>()) 14 "'match' item kind must be CompletionItemKind.Keyword (14)"

              let matchDoc =
                  match matchItem.Value["documentation"] with
                  | null -> ""
                  | d when d["value"] <> null -> d["value"].GetValue<string>()
                  | d -> d.GetValue<string>()

              Expect.isNotEmpty matchDoc "'match' keyword item must carry official FSharpKeywords description"
              Expect.isSome (findItem "let") "KeywordsAutocomplete=true includes 'let'"
              Expect.isSome (findItem "yield") "KeywordsAutocomplete=true includes 'yield'"
              Expect.isSome (findItem "b") "KeywordsAutocomplete=true preserves regular symbol 'b'"

              // Verify completionItem/resolve on keyword item preserves documentation
              let! resolvedMatch, _ =
                  lsp.Request("completionItem/resolve", JsonNode.Parse(matchItem.Value.ToJsonString()))
                  |> within
              Expect.isNotNull resolvedMatch["documentation"] "completionItem/resolve on keyword preserves documentation"

              // (c) Member-access dot guard: `b.` AND `"hi".` MUST NOT include keywords even when KeywordsAutocomplete = true!
              let! dotComp, _ = lsp.Request("textDocument/completion", compReq dotPos) |> within
              let dotLabels = [| for i in dotComp["items"].AsArray() -> i["label"].GetValue<string>() |] |> Set.ofArray
              Expect.contains dotLabels "Amount" "`b.` dot-completion includes record field 'Amount'"
              Expect.isFalse (dotLabels.Contains "match") "`b.` dot-completion MUST NOT include 'match' keyword"
              Expect.isFalse (dotLabels.Contains "let") "`b.` dot-completion MUST NOT include 'let' keyword"

              let! strDotComp, _ = lsp.Request("textDocument/completion", compReq strDotPos) |> within
              let strDotLabels = [| for i in strDotComp["items"].AsArray() -> i["label"].GetValue<string>() |] |> Set.ofArray
              Expect.contains strDotLabels "Length" "`\"hi\".` dot-completion includes 'Length'"
              Expect.isFalse (strDotLabels.Contains "match") "`\"hi\".` expression receiver dot-completion MUST NOT include keywords"

              let! modDotComp, _ = lsp.Request("textDocument/completion", compReq modDotPos) |> within
              let modDotLabels = [| for i in modDotComp["items"].AsArray() -> i["label"].GetValue<string>() |] |> Set.ofArray
              Expect.contains modDotLabels "Path" "`System.IO.` namespace dot-completion includes 'Path'"
              Expect.isFalse (modDotLabels.Contains "match") "`System.IO.` namespace dot-completion MUST NOT include keywords"

              // (d) Inside string literal: still 0 items even when KeywordsAutocomplete = true
              let! strLitComp, _ = lsp.Request("textDocument/completion", compReq inStrPos) |> within
              Expect.equal (strLitComp["items"].AsArray().Count) 0 "String literal completion must still return 0 items when KeywordsAutocomplete=true"

              // (e) Disable KeywordsAutocomplete = false again -> keywords omitted again
              do!
                  lsp.Notify(
                      "workspace/didChangeConfiguration",
                      JsonNode.Parse """{"settings":{"FSharp":{"keywordsAutocomplete":false}}}"""
                  )
                  |> withinUnit

              do! waitUntil (fun () -> not server.Config.KeywordsAutocomplete)
              let! disabledAgain, _ = lsp.Request("textDocument/completion", compReq exprPos) |> within
              let disabledLabels = [| for i in disabledAgain["items"].AsArray() -> i["label"].GetValue<string>() |] |> Set.ofArray
              Expect.isFalse (disabledLabels.Contains "match") "Toggling KeywordsAutocomplete back to false omits keywords"
    }

[<Tests>]
let lspProtocolTests =
    testSequenced
    <| testList
        "SyntaxFeaturesAndKeywords.LspProtocol"
        [ testTask "capabilities, non-blocking LatestParse proof under blocked typecheck (.fs & .fsi), and completion keyword parity" {
              do! runLspProtocolTest ()
          } ]
