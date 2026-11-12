module Rfs.Server.Tests.SemanticTokensAndConfigTests

open System
open System.Collections.Generic
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Lsp
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 60.)
let private withinUnit (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 60.)

let private expectedTokenTypes =
    [| "namespace"
       "type"
       "class"
       "enum"
       "interface"
       "struct"
       "typeParameter"
       "parameter"
       "variable"
       "property"
       "enumMember"
       "event"
       "function"
       "method"
       "macro"
       "keyword"
       "modifier"
       "comment"
       "string"
       "number"
       "regexp"
       "operator"
       "member"
       "cexpr"
       "text"
       "module" |]

let private expectedTokenModifiers =
    [| "declaration"
       "definition"
       "readonly"
       "static"
       "deprecated"
       "abstract"
       "async"
       "modification"
       "documentation"
       "defaultLibrary"
       "mutable"
       "disposable"
       "extension" |]

type private DecodedToken =
    { Line: int
      StartChar: int
      Length: int
      TokenType: string
      TokenTypeIdx: uint32
      Modifiers: uint32
      Text: string }

let private parseUint32Array (node: JsonNode) : uint32[] =
    match node with
    | :? JsonArray as arr -> [| for item in arr -> item.GetValue<uint32>() |]
    | _ -> failtestf "Expected JsonArray of uint32, got %A" node

let private decodeTokens (source: string) (data: uint32[]) : DecodedToken[] =
    Expect.equal (data.Length % 5) 0 "Semantic token data length must be a multiple of 5"
    let lines = source.Split('\n') |> Array.map (fun s -> s.TrimEnd('\r'))
    let count = data.Length / 5
    let out = Array.zeroCreate count
    let mutable line = 0
    let mutable col = 0

    for i in 0 .. count - 1 do
        let baseIdx = i * 5
        let dLine = int data[baseIdx]
        let dStart = int data[baseIdx + 1]
        let len = int data[baseIdx + 2]
        let tyIdx = data[baseIdx + 3]
        let mods = data[baseIdx + 4]

        if dLine > 0 then
            line <- line + dLine
            col <- dStart
        else
            col <- col + dStart

        Expect.isTrue (line >= 0 && line < lines.Length) $"Decoded line {line} out of bounds (lines={lines.Length}) at token {i}"
        let lineText = lines[line]
        Expect.isTrue (len > 0) $"Token {i} at ({line},{col}) must have positive length, got {len}"
        Expect.isTrue
            (col >= 0 && col + len <= lineText.Length)
            $"Decoded token {i} ({line},{col}..{col + len}) out of line '{lineText}' (len={lineText.Length})"

        let tyName =
            if int tyIdx < expectedTokenTypes.Length then expectedTokenTypes[int tyIdx]
            else $"unknown({tyIdx})"

        out[i] <-
            { Line = line
              StartChar = col
              Length = len
              TokenType = tyName
              TokenTypeIdx = tyIdx
              Modifiers = mods
              Text = lineText.Substring(col, len) }

    out

let private applyDelta (prev: uint32[]) (editsNode: JsonArray) : uint32[] =
    let result = ResizeArray<uint32>(prev)
    // Apply edits in descending start order (or single contiguous edit)
    let edits =
        [| for e in editsNode ->
               let start = e["start"].GetValue<int>()
               let deleteCount = e["deleteCount"].GetValue<int>()
               let insertData =
                   match e["data"] with
                   | null -> [||]
                   | d -> parseUint32Array d
               start, deleteCount, insertData |]
        |> Array.sortByDescending (fun (s, _, _) -> s)

    for start, deleteCount, insertData in edits do
        Expect.isTrue (start >= 0 && start <= result.Count) $"Delta edit start {start} out of range (len={result.Count})"
        Expect.isTrue (deleteCount >= 0 && start + deleteCount <= result.Count) $"Delta edit deleteCount {deleteCount} at {start} out of range (len={result.Count})"
        result.RemoveRange(start, deleteCount)
        result.InsertRange(start, insertData)

    result.ToArray()

let private copyDeps () : string * IDisposable =
    let fixturesRoot = Path.GetDirectoryName PublishTests.depsDir
    let repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)
    let root = Path.Combine(Path.GetTempPath(), "rfs-semtok-" + Guid.NewGuid().ToString("N").Substring(0, 8))
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
let configUnitTests =
    testList
        "SemanticTokensAndConfig.ConfigUnit"
        [ testCase "FSharpConfig.UpdateFromJson parses unwrapped camelCase and PascalCase InitializationOptions"
          <| fun () ->
              let json =
                  JToken.Parse
                      """{
                        "automaticWorkspaceInit": true,
                        "WorkspaceModePeekDeepLevel": 5,
                        "excludeProjectDirectories": ["bin", "obj", ".git"],
                        "keywordsAutocomplete": true,
                        "resolveNamespaces": true,
                        "indentationSize": 2,
                        "unionCaseStubGeneration": true,
                        "unionCaseStubGenerationBody": "todo ()",
                        "recordStubGeneration": true,
                        "recordStubGenerationBody": "recTodo ()",
                        "interfaceStubGeneration": true,
                        "interfaceStubGenerationObjectIdentifier": "self",
                        "interfaceStubGenerationMethodBody": "ifaceTodo ()",
                        "abstractClassStubGeneration": false,
                        "abstractClassStubGenerationObjectIdentifier": "me",
                        "abstractClassStubGenerationMethodBody": "absTodo ()",
                        "unusedOpensAnalyzer": true,
                        "unusedDeclarationsAnalyzer": true,
                        "simplifyNameAnalyzer": true,
                        "unnecessaryParenthesesAnalyzer": false,
                        "workspaceDiagnostics": true,
                        "lineLens": { "enabled": "replaceCodeLens", "prefix": " // " },
                        "codeLenses": { "signature": { "enabled": false }, "references": { "enabled": true } },
                        "inlayHints": { "typeAnnotations": false, "parameterNames": true, "disableLongTooltip": false },
                        "pipelineHints": { "enabled": true, "prefix": "//=>" }
                      }"""

              Expect.isFalse FSharpConfig.Default.WorkspaceDiagnostics "WorkspaceDiagnostics defaults to false"
              let updated = FSharpConfig.UpdateFromJson FSharpConfig.Default json
              Expect.isTrue updated.AutomaticWorkspaceInit "AutomaticWorkspaceInit"
              Expect.isTrue updated.WorkspaceDiagnostics "WorkspaceDiagnostics"
              Expect.equal updated.WorkspaceModePeekDeepLevel 5 "WorkspaceModePeekDeepLevel"
              Expect.equal updated.ExcludeProjectDirectories [ "bin"; "obj"; ".git" ] "ExcludeProjectDirectories"
              Expect.isTrue updated.KeywordsAutocomplete "KeywordsAutocomplete"
              Expect.isTrue updated.ResolveNamespaces "ResolveNamespaces"
              Expect.equal updated.IndentationSize 2 "IndentationSize"
              Expect.isTrue updated.UnionCaseStubGeneration "UnionCaseStubGeneration"
              Expect.equal updated.UnionCaseStubGenerationBody "todo ()" "UnionCaseStubGenerationBody"
              Expect.isTrue updated.RecordStubGeneration "RecordStubGeneration"
              Expect.equal updated.RecordStubGenerationBody "recTodo ()" "RecordStubGenerationBody"
              Expect.isTrue updated.InterfaceStubGeneration "InterfaceStubGeneration"
              Expect.equal updated.InterfaceStubGenerationObjectIdentifier "self" "InterfaceStubGenerationObjectIdentifier"
              Expect.equal updated.InterfaceStubGenerationMethodBody "ifaceTodo ()" "InterfaceStubGenerationMethodBody"
              Expect.isFalse updated.AbstractClassStubGeneration "AbstractClassStubGeneration"
              Expect.equal updated.AbstractClassStubGenerationObjectIdentifier "me" "AbstractClassStubGenerationObjectIdentifier"
              Expect.equal updated.AbstractClassStubGenerationMethodBody "absTodo ()" "AbstractClassStubGenerationMethodBody"
              Expect.isTrue updated.UnusedOpensAnalyzer "UnusedOpensAnalyzer"
              Expect.isTrue updated.UnusedDeclarationsAnalyzer "UnusedDeclarationsAnalyzer"
              Expect.isTrue updated.SimplifyNameAnalyzer "SimplifyNameAnalyzer"
              Expect.isFalse updated.UnnecessaryParenthesesAnalyzer "UnnecessaryParenthesesAnalyzer"
              Expect.equal updated.LineLens { Enabled = "replaceCodeLens"; Prefix = " // " } "LineLens"
              Expect.equal updated.CodeLenses { SignatureEnabled = false; ReferencesEnabled = true } "CodeLenses"
              Expect.equal
                  updated.InlayHints
                  { TypeAnnotations = false
                    ParameterNames = true
                    DisableLongTooltip = false }
                  "InlayHints"
              Expect.equal updated.InlineValues { Enabled = Some true; Prefix = "//=>" } "InlineValues"

          testCase "FSharpConfig.UpdateFromJson unwraps nested FSharp section and workspace/configuration array"
          <| fun () ->
              let nested =
                  JToken.Parse
                      """{ "FSharp": { "IndentationSize": 8, "InlayHints": { "parameterNames": false }, "CodeLenses": { "references": { "enabled": false } } } }"""

              let c1 = FSharpConfig.UpdateFromJson FSharpConfig.Default nested
              Expect.equal c1.IndentationSize 8 "Nested FSharp IndentationSize"
              Expect.isFalse c1.InlayHints.ParameterNames "Nested InlayHints.ParameterNames"
              Expect.isTrue c1.InlayHints.TypeAnnotations "Untouched InlayHints.TypeAnnotations preserved"
              Expect.isFalse c1.CodeLenses.ReferencesEnabled "Nested CodeLenses.ReferencesEnabled"
              Expect.isTrue c1.CodeLenses.SignatureEnabled "Untouched CodeLenses.SignatureEnabled preserved"

              let arr = JToken.Parse """[ { "keywordsAutocomplete": true, "inlineValues": { "enabled": false, "prefix": "//:" } } ]"""
              let c2 = FSharpConfig.UpdateFromJson c1 arr
              Expect.equal c2.IndentationSize 8 "Previous IndentationSize preserved across partial array update"
              Expect.isTrue c2.KeywordsAutocomplete "Array element keywordsAutocomplete applied"
              Expect.equal c2.InlineValues { Enabled = Some false; Prefix = "//:" } "Array element inlineValues applied"

          testCase "FSharpConfig.UpdateFromJson ignores null, primitives, and malformed field types without throwing"
          <| fun () ->
              let baseCfg =
                  FSharpConfig.UpdateFromJson FSharpConfig.Default (JToken.Parse """{ "IndentationSize": 6, "KeywordsAutocomplete": true }""")

              Expect.equal (FSharpConfig.UpdateFromJson baseCfg null) baseCfg "null token leaves config intact"
              Expect.equal (FSharpConfig.UpdateFromJson baseCfg (JValue.CreateNull())) baseCfg "JValue null leaves config intact"
              Expect.equal (FSharpConfig.UpdateFromJson baseCfg (JToken.Parse "[null]")) baseCfg "[null] array leaves config intact"
              Expect.equal (FSharpConfig.UpdateFromJson baseCfg (JToken.Parse "\"not-an-object\"")) baseCfg "string token leaves config intact"

              let badFields =
                  JToken.Parse
                      """{
                        "FSharp": {
                          "IndentationSize": "not-an-int",
                          "KeywordsAutocomplete": "not-a-bool",
                          "WorkspaceDiagnostics": "not-a-bool",
                          "ExcludeProjectDirectories": 42,
                          "InlayHints": "not-an-object",
                          "CodeLenses": { "signature": 99 }
                        }
                      }"""

              let afterBad = FSharpConfig.UpdateFromJson baseCfg badFields
              Expect.equal afterBad baseCfg "Malformed field types leave existing values intact" ]

[<Tests>]
let encoderUnitTests =
    testList
        "SemanticTokensAndConfig.EncoderUnit"
        [ testCase "SemanticTokens.legend matches exact FSAC 26 token types and 13 modifiers"
          <| fun () ->
              Expect.equal SemanticTokens.legend.TokenTypes expectedTokenTypes "Exact 26 SemanticTokenTypes in index order"
              Expect.equal SemanticTokens.legend.TokenModifiers expectedTokenModifiers "Exact 13 SemanticTokenModifiers in bit order"

          testCase "encodeClassified splits multiline tokens across lines with exact line lengths and scrubs parent ranges"
          <| fun () ->
              let text = "first line\r\n  mid %d end\r\nlast!"
              let mkRange sl sc el ec =
                  Range.mkRange "test.fs" (Position.mkPos sl sc) (Position.mkPos el ec)

              // A 3-line token from line 1 col 6 ("line") -> line 2 ("  mid %d end") -> line 3 col 0..4 ("last")
              // PLUS a child Printf token on line 2 col 6..8 ("%d") which must scrub line 2 into "  mid " (0..6), "%d" (6..8), " end" (8..12)
              let items =
                  [| SemanticClassificationItem((mkRange 1 6 3 4, SemanticClassificationType.Plaintext))
                     SemanticClassificationItem((mkRange 2 6 2 8, SemanticClassificationType.Printf)) |]

              let encoded =
                  SemanticTokens.encodeClassified text (HashSet()) [||] None items

              let decoded = decodeTokens text encoded

              Expect.equal
                  [ for t in decoded -> t.Line, t.StartChar, t.Length, t.TokenType, t.Text ]
                  [ 0, 6, 4, "text", "line"
                    1, 0, 6, "text", "  mid "
                    1, 6, 2, "regexp", "%d"
                    1, 8, 4, "text", " end"
                    2, 0, 4, "text", "last" ]
                  "3-line token split by actual line lengths and scrubbed around nested Printf token" ]

let private runConfigLspProtocolTest () =
    task {
        let! lsp, owner, serverTask = TestServer.startCapturing ()
        use _ = owner

        let initParams =
            JsonNode.Parse
                """{
                  "processId": null,
                  "rootUri": null,
                  "capabilities": {
                    "workspace": {
                      "configuration": true,
                      "didChangeConfiguration": { "dynamicRegistration": true }
                    }
                  },
                  "initializationOptions": {
                    "workspaceModePeekDeepLevel": 4,
                    "keywordsAutocomplete": true
                  }
                }"""

        let! initReply, _ = lsp.Request("initialize", initParams) |> within
        let! server = serverTask |> within

        // Verify capability advertisement
        let stCap = initReply["capabilities"].["semanticTokensProvider"]
        Expect.isNotNull stCap "semanticTokensProvider capability must be advertised"
        let typesJson = [| for x in stCap["legend"].["tokenTypes"].AsArray() -> x.GetValue<string>() |]
        let modsJson = [| for x in stCap["legend"].["tokenModifiers"].AsArray() -> x.GetValue<string>() |]
        Expect.equal typesJson expectedTokenTypes "Advertised legend.tokenTypes matches FSAC"
        Expect.equal modsJson expectedTokenModifiers "Advertised legend.tokenModifiers matches FSAC"
        Expect.isTrue (stCap["range"].GetValue<bool>()) "Advertised semanticTokensProvider.range = true"
        Expect.isTrue (stCap["full"].["delta"].GetValue<bool>()) "Advertised semanticTokensProvider.full.delta = true"

        // Verify initializationOptions applied
        Expect.equal server.Config.WorkspaceModePeekDeepLevel 4 "initializationOptions set WorkspaceModePeekDeepLevel"
        Expect.isTrue server.Config.KeywordsAutocomplete "initializationOptions set KeywordsAutocomplete"

        // 1) Test workspace/didChangeConfiguration with nested {"FSharp": ...} when workspace/configuration returns [null]
        do! lsp.Notify("initialized", JsonObject()) |> withinUnit

        do!
            lsp.Notify(
                "workspace/didChangeConfiguration",
                JsonNode.Parse """{"settings":{"FSharp":{"indentationSize":2,"resolveNamespaces":true,"inlayHints":{"parameterNames":false}}}}"""
            )
            |> withinUnit

        do! waitUntil (fun () -> server.Config.IndentationSize = 2 && server.Config.ResolveNamespaces && not server.Config.InlayHints.ParameterNames)

        // 2) Test workspace/configuration pull when didChangeConfiguration sends null settings
        lsp.AnswerWith(
            "workspace/configuration",
            fun _ ->
                Task.FromResult(
                    Ok(JsonNode.Parse """[{"unusedOpensAnalyzer":true,"codeLenses":{"signature":{"enabled":false}}}]""")
                )
        )

        do! lsp.Notify("workspace/didChangeConfiguration", JsonNode.Parse """{"settings":null}""") |> withinUnit
        do! waitUntil (fun () -> server.Config.UnusedOpensAnalyzer && not server.Config.CodeLenses.SignatureEnabled)
        Expect.equal server.Config.IndentationSize 2 "Previous IndentationSize preserved after pull merge"
    }

let private runSemanticTokensLspProtocolTest () =
    task {
              let targetDir, cleanup = copyDeps ()
              use _ = cleanup
              let projPath = Path.Combine(targetDir, "Deps.fsproj")
              let dPath = Path.Combine(targetDir, "D.fs")
              let eSigPath = Path.Combine(targetDir, "E.fsi")
              let eImplPath = Path.Combine(targetDir, "E.fs")

              let! lsp, owner, _ = TestServer.startCapturing ()
              use _ = owner

              let! _ = lsp.Initialize(Paths.toUri targetDir, JsonNode.Parse "{}") |> within
              do! loadProject lsp projPath

              let richFsSource =
                  String.concat
                      "\n"
                      [ "module Deps.D"
                        ""
                        "[<Literal>]"
                        "let MaxItems = 100"
                        ""
                        "type Color ="
                        "    | Red = 1"
                        "    | Blue = 2"
                        ""
                        "type IWorker ="
                        "    abstract Run: int -> int"
                        ""
                        "[<Struct>]"
                        "type PointStruct = { X: int }"
                        ""
                        "type Shape ="
                        "    | Circle of radius: float"
                        ""
                        "type CounterRec = { Label: string; mutable Count: int }"
                        ""
                        "type MyHandle() ="
                        "    interface System.IDisposable with"
                        "        member _.Dispose() = ()"
                        ""
                        "type System.String with"
                        "    member s.Dup() = s + s"
                        ""
                        "let showcase (alpha: int) (nullableTag: string | null) ="
                        "    let mutable acc = alpha + MaxItems"
                        "    use handle = new MyHandle()"
                        "    let recVal = { Label = \"hi\".Dup(); Count = acc }"
                        "    recVal.Count <- acc + 1"
                        "    let formatted = sprintf \"n=%d s=%s\" acc recVal.Label"
                        "    let interp = $\"hex=%04x{MaxItems}\""
                        "    let mapper = fun lambdaParam -> lambdaParam + formatted.Length + interp.Length + (handle |> ignore; 0)"
                        "    match nullableTag with"
                        "    | null -> mapper 0"
                        "    | s -> mapper s.Length" ]

              do! lsp.DidOpen(dPath, richFsSource) |> withinUnit

              let fullParams (path: string) =
                  JsonNode.Parse $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri path).ToJsonString()}}}}}"""

              let! fullResult, _ = lsp.Request("textDocument/semanticTokens/full", fullParams dPath) |> within
              Expect.isNotNull fullResult "semanticTokens/full result on .fs must not be null"
              let resultId1 = fullResult["resultId"].GetValue<string>()
              Expect.isNotEmpty resultId1 "resultId must be non-empty"
              let rawFull = parseUint32Array fullResult["data"]
              Expect.isTrue (rawFull.Length > 0) "semanticTokens/full data must be non-empty"

              // Explicitly assert relative encoding resets reference column to 0 whenever deltaLine > 0
              let mutable sawCrossLineAfterNonZeroCol = false
              let mutable prevCol = 0

              for i in 0 .. (rawFull.Length / 5) - 1 do
                  let dLine = int rawFull[i * 5]
                  let dStart = int rawFull[i * 5 + 1]

                  if dLine > 0 && prevCol > 0 then
                      sawCrossLineAfterNonZeroCol <- true

                  prevCol <- if dLine > 0 then dStart else prevCol + dStart

              Expect.isTrue sawCrossLineAfterNonZeroCol "Fixture must exercise deltaLine > 0 after a non-zero column token"
              let tokens = decodeTokens richFsSource rawFull

              let findToken line needle =
                  tokens
                  |> Array.tryFind (fun t -> t.Line = line && t.Text = needle)
                  |> Option.defaultWith (fun () ->
                      let onLine = tokens |> Array.filter (fun t -> t.Line = line) |> Array.map (fun t -> $"{t.Text}:{t.TokenType}({t.Modifiers})")
                      failtestf "Missing token '%s' on line %d. Found: %A" needle line onLine)

              // 1) Module
              let modTok = findToken 0 "D"
              Expect.equal modTok.TokenType "module" "Module token type"

              // 2) [<Literal>] -> variable (8) with Readonly (4) ||| DefaultLibrary (512) = 516u
              let litDef = findToken 3 "MaxItems"
              Expect.equal litDef.TokenType "variable" "Literal token type"
              Expect.equal litDef.Modifiers 516u "Literal modifiers must be Readonly (4) ||| DefaultLibrary (512) = 516u"

              // 3) Enum, Interface, Struct, UnionCase, UnionCaseField
              Expect.equal (findToken 5 "Color").TokenType "enum" "Enum token type"
              Expect.equal (findToken 9 "IWorker").TokenType "interface" "Interface token type"
              Expect.equal (findToken 13 "PointStruct").TokenType "struct" "Struct token type"
              Expect.equal (findToken 16 "Circle").TokenType "enumMember" "UnionCase token type"
              Expect.equal (findToken 16 "radius").TokenType "parameter" "UnionCaseField token type"

              // 4) Record fields: immutable (readonly = 4u) and mutable (mutable = 1024u)
              let labelField = findToken 18 "Label"
              Expect.equal labelField.TokenType "property" "Immutable record field type"
              Expect.equal labelField.Modifiers 4u "Immutable record field modifier = Readonly (4u)"
              let countField = findToken 18 "Count"
              Expect.equal countField.TokenType "property" "Mutable record field type"
              Expect.equal countField.Modifiers 1024u "Mutable record field modifier = Mutable (1024u)"

              // 5) Disposable type & value (disposable = 2048u)
              let dispType = findToken 20 "MyHandle"
              Expect.equal dispType.TokenType "type" "Disposable type token type"
              Expect.equal dispType.Modifiers 2048u "Disposable type modifier = Disposable (2048u)"
              let dispVal = findToken 29 "handle"
              Expect.equal dispVal.TokenType "variable" "Disposable local value token type"
              Expect.equal dispVal.Modifiers 2048u "Disposable local value modifier = Disposable (2048u)"

              // 6) Extension method (method + extension = 4096u; FCS spans receiver..member on string literal receiver, matching FSAC)
              let extCall = findToken 30 "\"hi\".Dup"
              Expect.equal extCall.TokenType "method" "Extension method call token type"
              Expect.equal extCall.Modifiers 4096u "Extension method call modifier = Extension (4096u)"

              // 7) AST parameter override & SynType.WithNull keyword supplement on line 27:
              // `let showcase (alpha: int) (nullableTag: string | null) =`
              let alphaParam = findToken 27 "alpha"
              Expect.equal alphaParam.TokenType "parameter" "Function parameter binding must be overridden to parameter"
              Expect.equal alphaParam.Modifiers 0u "Function parameter modifiers = 0u"
              let nullableParam = findToken 27 "nullableTag"
              Expect.equal nullableParam.TokenType "parameter" "Second function parameter binding must be parameter"
              let nullKw = findToken 27 "null"
              Expect.equal nullKw.TokenType "keyword" "SynType.WithNull 'null' must be supplemented as keyword"
              Expect.equal nullKw.Modifiers 0u "SynType.WithNull keyword modifiers = 0u"

              // Local mutable var `acc` on line 28 is variable + mutable (1024u), NOT parameter
              let accDef = findToken 28 "acc"
              Expect.equal accDef.TokenType "variable" "Local mutable binding is variable"
              Expect.equal accDef.Modifiers 1024u "Local mutable binding has Mutable (1024u)"

              // Non-parameter local binding `formatted` on line 32 is variable (0u), while lambda `lambdaParam` on line 34 is parameter!
              let formattedDef = findToken 32 "formatted"
              Expect.equal formattedDef.TokenType "variable" "Local non-parameter let binding stays variable"
              Expect.equal formattedDef.Modifiers 0u "Immutable local variable modifiers = 0u"

              let lambdaParamTokens = tokens |> Array.filter (fun t -> t.Line = 34 && t.Text = "lambdaParam")
              Expect.isTrue (lambdaParamTokens.Length >= 2) "lambdaParam appears at binding and body use"
              Expect.equal lambdaParamTokens[0].TokenType "parameter" "Lambda parameter binding is overridden to parameter"
              Expect.equal lambdaParamTokens[1].TokenType "variable" "Lambda parameter body use stays variable"

              // 8) Printf & interpolated string format specifiers -> regexp
              Expect.equal (findToken 32 "%d").TokenType "regexp" "sprintf %d format specifier"
              Expect.equal (findToken 32 "%s").TokenType "regexp" "sprintf %s format specifier"
              Expect.equal (findToken 33 "%04x").TokenType "regexp" "Interpolated string %04x format specifier"

              // 9) semanticTokens/range on .fs: lines 32..33 only
              let rangeParams (path: string) (sl: int) (sc: int) (el: int) (ec: int) =
                  JsonNode.Parse
                      $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri path).ToJsonString()}}},"range":{{"start":{{"line":%d{sl},"character":%d{sc}}},"end":{{"line":%d{el},"character":%d{ec}}}}}}}"""

              let! rangeReply, _ = lsp.Request("textDocument/semanticTokens/range", rangeParams dPath 32 0 33 80) |> within
              Expect.isNotNull rangeReply "semanticTokens/range reply must not be null"
              let rangeTokens = decodeTokens richFsSource (parseUint32Array rangeReply["data"])
              Expect.isNonEmpty rangeTokens "semanticTokens/range must return tokens in lines 32..33"
              Expect.all rangeTokens (fun t -> t.Line >= 32 && t.Line <= 33) "All range tokens must lie within lines 32..33"
              let expectedSlice = tokens |> Array.filter (fun t -> t.Line >= 32 && t.Line <= 33)
              Expect.equal rangeTokens expectedSlice "semanticTokens/range tokens match exact slice of full tokens"

              // 10) .fsi signature file semanticTokens/full and semanticTokens/range (including SynType.WithNull in .fsi!)
              let sigSource =
                  String.concat
                      "\n"
                      [ "module Deps.E"
                        ""
                        "type ISigCheck ="
                        "    abstract Eval: int -> int"
                        ""
                        "val user: int"
                        "val compute: (string | null) -> int"
                        "val unrelated: int" ]

              let implSource =
                  String.concat
                      "\n"
                      [ "module Deps.E"
                        ""
                        "type ISigCheck ="
                        "    abstract Eval: int -> int"
                        ""
                        "let user = 2"
                        "let compute (s: string | null) = match s with | null -> 0 | v -> v.Length"
                        "let unrelated = 7" ]

              do! lsp.DidOpen(eSigPath, sigSource) |> withinUnit
              do! lsp.DidOpen(eImplPath, implSource) |> withinUnit

              let! sigFull, _ = lsp.Request("textDocument/semanticTokens/full", fullParams eSigPath) |> within
              Expect.isNotNull sigFull "semanticTokens/full on .fsi must not be null"
              let sigTokens = decodeTokens sigSource (parseUint32Array sigFull["data"])
              Expect.isTrue (sigTokens |> Array.exists (fun t -> t.Line = 2 && t.Text = "ISigCheck" && t.TokenType = "interface")) ".fsi interface token"
              Expect.isTrue (sigTokens |> Array.exists (fun t -> t.Line = 6 && t.Text = "compute")) ".fsi val compute token"
              Expect.isTrue (sigTokens |> Array.exists (fun t -> t.Line = 6 && t.Text = "null" && t.TokenType = "keyword")) ".fsi SynType.WithNull 'null' keyword token"

              let! sigRange, _ = lsp.Request("textDocument/semanticTokens/range", rangeParams eSigPath 6 0 6 50) |> within
              Expect.isNotNull sigRange "semanticTokens/range on .fsi must not be null"
              let sigRangeTokens = decodeTokens sigSource (parseUint32Array sigRange["data"])
              Expect.isNonEmpty sigRangeTokens ".fsi range tokens on line 6 must be non-empty"
              Expect.all sigRangeTokens (fun t -> t.Line = 6) ".fsi range tokens restricted to line 6"

              // 11) semanticTokens/full/delta lifecycle on E.fs:
              //     (a) initial full -> id0
              //     (b) identical full/delta -> empty edits = []
              //     (c) growing edit -> compact delta reconstructs exact full data
              //     (d) shrinking edit with repeated identical middle lines -> non-overlapping prefix/suffix delta reconstructs exact full data
              //     (e) stale previousResultId -> falls back to full data
              //     (f) didClose -> evicts cache so old resultId falls back to full data
              let deltaSource0 =
                  String.concat
                      "\n"
                      [ "module Deps.E"
                        "type ISigCheck ="
                        "    abstract Eval: int -> int"
                        "let user = 2"
                        "let compute (s: string | null) = match s with | null -> 0 | v -> v.Length"
                        "let dupVal = 10"
                        "let unrelated = 7" ]

              do!
                  lsp.DidChange(
                      eImplPath,
                      1,
                      { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 7; Character = 17 } }
                        Text = deltaSource0 }
                  )
                  |> withinUnit

              let! baseFull, _ = lsp.Request("textDocument/semanticTokens/full", fullParams eImplPath) |> within
              let id0 = baseFull["resultId"].GetValue<string>()
              let data0 = parseUint32Array baseFull["data"]

              let deltaParams (path: string) (prevId: string) =
                  JsonNode.Parse
                      $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri path).ToJsonString()}}},"previousResultId":%s{JsonValue.Create(prevId).ToJsonString()}}}"""

              // (b) Identical re-query -> empty edits
              let! sameDelta, _ = lsp.Request("textDocument/semanticTokens/full/delta", deltaParams eImplPath id0) |> within
              Expect.isNull sameDelta["data"] "Identical delta response must return SemanticTokensDelta (no 'data' field)"
              Expect.equal (sameDelta["edits"].AsArray().Count) 0 "Identical delta response must have empty edits"
              let id1 = sameDelta["resultId"].GetValue<string>()
              Expect.notEqual id1 id0 "Each delta response issues a fresh resultId"

              // (c) Growing edit: insert duplicate line `let dupVal = 10` (renamed `dupVal2` with same length so relative tokens match!)
              let deltaSourceGrow =
                  String.concat
                      "\n"
                      [ "module Deps.E"
                        "type ISigCheck ="
                        "    abstract Eval: int -> int"
                        "let user = 2"
                        "let compute (s: string | null) = match s with | null -> 0 | v -> v.Length"
                        "let dupVal = 10"
                        "let dupVa2 = 10"
                        "let unrelated = 7" ]

              do!
                  lsp.DidChange(
                      eImplPath,
                      2,
                      { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 6; Character = 17 } }
                        Text = deltaSourceGrow }
                  )
                  |> withinUnit

              let! growDelta, _ = lsp.Request("textDocument/semanticTokens/full/delta", deltaParams eImplPath id1) |> within
              Expect.isNull growDelta["data"] "Growing delta response must be SemanticTokensDelta"
              let growEdits = growDelta["edits"].AsArray()
              Expect.equal growEdits.Count 1 "Growing localized edit produces 1 contiguous SemanticTokensEdit"
              Expect.isTrue (growEdits[0].["start"].GetValue<int>() > 0) "Growing edit shares non-empty prefix"
              let id2 = growDelta["resultId"].GetValue<string>()
              let reconstructedGrow = applyDelta data0 growEdits

              let! verifyGrowFull, _ = lsp.Request("textDocument/semanticTokens/full", fullParams eImplPath) |> within
              let id3 = verifyGrowFull["resultId"].GetValue<string>()
              let expectedGrowData = parseUint32Array verifyGrowFull["data"]
              Expect.equal reconstructedGrow expectedGrowData "Applying growing delta edit must bit-for-bit match semanticTokens/full"

              // (d) Shrinking edit: remove `let dupVa2 = 10` (tests shrinking across identical relative token sequences!)
              do!
                  lsp.DidChange(
                      eImplPath,
                      3,
                      { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 7; Character = 17 } }
                        Text = deltaSource0 }
                  )
                  |> withinUnit

              let! shrinkDelta, _ = lsp.Request("textDocument/semanticTokens/full/delta", deltaParams eImplPath id3) |> within
              Expect.isNull shrinkDelta["data"] "Shrinking delta response must be SemanticTokensDelta"
              let shrinkEdits = shrinkDelta["edits"].AsArray()
              Expect.equal shrinkEdits.Count 1 "Shrinking localized edit produces 1 contiguous SemanticTokensEdit"
              let reconstructedShrink = applyDelta expectedGrowData shrinkEdits
              Expect.equal reconstructedShrink data0 "Applying shrinking delta edit must bit-for-bit reconstruct original full data"
              let id4 = shrinkDelta["resultId"].GetValue<string>()

              // (e) Unknown/stale previousResultId -> falls back to full SemanticTokens
              let! staleReply, _ = lsp.Request("textDocument/semanticTokens/full/delta", deltaParams eImplPath "non-existent-id") |> within
              Expect.isNull staleReply["edits"] "Unknown previousResultId must fall back to full SemanticTokens (no 'edits')"
              Expect.equal (parseUint32Array staleReply["data"]) data0 "Fallback full data matches current tokens"
              let id5 = staleReply["resultId"].GetValue<string>()

              // (f) didClose evicts TokenDeltaCache -> old id5 now misses and returns full SemanticTokens
              do!
                  lsp.Notify(
                      "textDocument/didClose",
                      JsonNode.Parse $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri eImplPath).ToJsonString()}}}}}"""
                  )
                  |> withinUnit

              do! lsp.DidOpen(eImplPath, deltaSource0) |> withinUnit
              let! afterCloseReply, _ = lsp.Request("textDocument/semanticTokens/full/delta", deltaParams eImplPath id5) |> within
              Expect.isNull afterCloseReply["edits"] "didClose must evict TokenDeltaCache so prior resultId falls back to full SemanticTokens"
              Expect.isNotNull afterCloseReply["data"] "After didClose, full SemanticTokens data is returned"
              ignore id2
              ignore id4
    }

[<Tests>]
let lspProtocolTests =
    testSequenced
    <| testList
        "SemanticTokensAndConfig.LspProtocol"
        [ testTask "initialize capability, initializationOptions, didChangeConfiguration wrapper, and workspace/configuration pull" {
              do! runConfigLspProtocolTest ()
          }
          testTask "semanticTokens full, full/delta, and range on .fs and .fsi over LSP" {
              do! runSemanticTokensLspProtocolTest ()
          } ]
