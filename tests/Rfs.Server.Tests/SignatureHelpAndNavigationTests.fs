module Rfs.Server.Tests.SignatureHelpAndNavigationTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 60.)
let private withinUnit (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 60.)

let private findPos (source: string) (needle: string) (nth: int) (inside: int) : int * int =
    let mutable idx = -1

    for _ in 0..nth do
        idx <- source.IndexOf(needle, idx + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual idx 0 $"Expected '%s{needle}' occurrence %d{nth} in source"
    let target = idx + inside
    let before = source.Substring(0, target)
    let lines = before.Split('\n')
    let line = lines.Length - 1
    let col = lines[lines.Length - 1].TrimEnd('\r').Length
    line, col

let private findAfter (source: string) (needle: string) (nth: int) : int * int =
    findPos source needle nth needle.Length

let private sliceLspRange (source: string) (r: Ionide.LanguageServerProtocol.Types.Range) : string =
    let lines = source.Split('\n') |> Array.map (fun s -> s.TrimEnd('\r'))
    let sl, sc = int r.Start.Line, int r.Start.Character
    let el, ec = int r.End.Line, int r.End.Character
    Expect.isTrue (sl >= 0 && sl < lines.Length && el >= sl && el < lines.Length) $"Range %A{r} lines out of bounds"

    if sl = el then
        lines[sl].Substring(sc, ec - sc)
    else
        failtest $"Expected single-line highlight range, got %A{r}"

let private sliceFcsRange (source: string) (r: range) : string =
    let lines = source.Split('\n') |> Array.map (fun s -> s.TrimEnd('\r'))
    let sl, sc = r.StartLine - 1, r.StartColumn
    let el, ec = r.EndLine - 1, r.EndColumn
    Expect.isTrue (sl >= 0 && sl < lines.Length && el >= sl && el < lines.Length) $"FCS range %A{r} lines out of bounds"

    if sl = el then
        lines[sl].Substring(sc, ec - sc)
    else
        failtest $"Expected single-line identifier range, got %A{r}"

let private sameFile (a: string) (b: string) =
    String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

let private withTwoProjectWorkspace
    (libFiles: (string * string) list)
    (appFiles: (string * string) list)
    (body: ProjectGraph -> ProjectGraph -> (string -> string) -> (string -> string) -> References.Scope -> Task<unit>)
    : Task<unit> =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-task4-"

        try
            let libDir = Path.Combine(dir.FullName, "Lib")
            let appDir = Path.Combine(dir.FullName, "App")
            Directory.CreateDirectory libDir |> ignore
            Directory.CreateDirectory appDir |> ignore

            let libPath (name: string) = Path.Combine(libDir, name)
            let appPath (name: string) = Path.Combine(appDir, name)

            for name, text in libFiles do
                File.WriteAllText(libPath name, text)

            for name, text in appFiles do
                File.WriteAllText(appPath name, text)

            let libPaths = libFiles |> List.map (fst >> libPath)
            let appPaths = appFiles |> List.map (fst >> appPath)

            let! baseSpec = ProjectSpec.load PublishTests.depsProject
            let baseSeq = ProjectSpec.sequential baseSpec
            let libDll = Path.Combine(libDir, "bin", "Lib.dll")

            let libSpec =
                { baseSeq with
                    ProjectFile = libPath "Lib.fsproj"
                    SourceFiles = libPaths
                    GeneratedFiles = []
                    OtherOptions =
                        baseSeq.OtherOptions
                        |> List.map (fun flag ->
                            if flag.StartsWith("-o:", StringComparison.Ordinal) then "-o:" + libDll
                            elif flag.StartsWith("--out:", StringComparison.Ordinal) then "--out:" + libDll
                            else flag) }

            let appDll = Path.Combine(appDir, "bin", "App.dll")

            let appSpec =
                { baseSeq with
                    ProjectFile = appPath "App.fsproj"
                    SourceFiles = appPaths
                    GeneratedFiles = []
                    ReferencedProjects = [ libSpec.ProjectFile ]
                    ReferencedFrameworks = [ baseSeq.TargetFramework ]
                    OtherOptions =
                        (baseSeq.OtherOptions
                         |> List.map (fun flag ->
                             if flag.StartsWith("-o:", StringComparison.Ordinal) then "-o:" + appDll
                             elif flag.StartsWith("--out:", StringComparison.Ordinal) then "--out:" + appDll
                             else flag))
                        @ [ "-r:" + libDll ] }

            use host = GraphHost.Start "task4"
            let! libGraph = ProjectGraph.Create(host, libSpec)
            let! appGraph = ProjectGraph.Create(host, appSpec, [ libGraph ])

            let! _ = libGraph.AllDiagnostics() |> within
            let! _ = appGraph.AllDiagnostics() |> within

            let ownersOf (f: string) =
                let full = Path.GetFullPath f

                if libPaths |> List.exists (sameFile full) then [ libGraph ]
                elif appPaths |> List.exists (sameFile full) then [ appGraph ]
                else []

            let scope: References.Scope =
                { OwnersOf = ownersOf
                  ReferencingProjects = fun g -> if obj.ReferenceEquals(g, libGraph) then [ appGraph ] else []
                  Projects = fun () -> [ libGraph; appGraph ]
                  FilesOf = fun g -> if obj.ReferenceEquals(g, libGraph) then libPaths else appPaths
                  IsLoading = fun () -> false }

            do! body libGraph appGraph libPath appPath scope
            appGraph.Dispose()
            libGraph.Dispose()
        finally
            try
                dir.Delete true
            with _ ->
                ()
    }

let private typesFs =
    String.concat
        "\n"
        [ "module Lib.Types"
          ""
          "type Customer = { Id: int; Name: string }"
          ""
          "type OrderKind ="
          "    | Urgent of Customer"
          "    | Standard"
          ""
          "type CustomerAlias = Customer"
          ""
          "type IWorker ="
          "    abstract Execute: count: int -> string"
          ""
          "[<AbstractClass>]"
          "type BaseProcessor() ="
          "    abstract Process: item: string -> int"
          ""
          "type LocalWorker() ="
          "    interface IWorker with"
          "        member _.Execute(count: int) = string count"
          ""
          "type ConcreteProcessor() ="
          "    inherit BaseProcessor()"
          "    override _.Process(item: string) = item.Length"
          ""
          "/// Combines three curried arguments into a label."
          "let combineThree (alpha: int) (beta: string) (gamma: bool) ="
          "    if gamma then sprintf \"%s:%d\" beta alpha else beta"
          ""
          "/// Subtracts second from first."
          "let subTwo (firstVal: int) (secondVal: int) = firstVal - secondVal"
          ""
          "type OverloadedCalc() ="
          "    /// Computes from two inputs."
          "    static member Compute(first: int, second: string) = sprintf \"%s-%d\" second first"
          "    /// Computes from three inputs."
          "    static member Compute(first: int, second: string, third: bool) = if third then second else string first" ]

let private signedFsi =
    String.concat
        "\n"
        [ "module Lib.Signed"
          ""
          "open Lib.Types"
          ""
          "type SignedBox ="
          "    { Item: Customer }"
          "    member Tag: string"
          ""
          "val makeCustomer: id: int -> Customer"
          ""
          "val ``special value``: Customer" ]

let private signedFs =
    String.concat
        "\n"
        [ "module Lib.Signed"
          ""
          "open Lib.Types"
          ""
          "type SignedBox ="
          "    { Item: Customer }"
          "    member _.Tag = \"ok\""
          ""
          "let internal helperSecret x = x + 1"
          ""
          "let makeCustomer (id: int) : Customer ="
          "    let nextId = helperSecret id"
          "    { Id = nextId; Name = \"cust\" }"
          ""
          "let ``special value`` : Customer = makeCustomer 7" ]

let private appUseFs =
    String.concat
        "\n"
        [ "module App.Use"
          ""
          "open Lib.Types"
          ""
          "type AppWorker() ="
          "    interface IWorker with"
          "        member _.Execute(count: int) = sprintf \"app:%d\" count"
          ""
          "let objWorker ="
          "    { new IWorker with"
          "        member _.Execute(count: int) = sprintf \"obj:%d\" count }"
          ""
          "let runCalls () ="
          "    let m0 = OverloadedCalc.Compute(10, \"a\")"
          "    let m1 = OverloadedCalc.Compute(10, \"a\", true)"
          "    let c0 = combineThree 1 \"b\" false"
          "    let p0 = 5 |> subTwo "
          "    let pSat = 5 |> subTwo 20 "
          "    let cust = Lib.Signed.makeCustomer 3"
          "    let spec = Lib.Signed.``special value``"
          "    let aliasVal: CustomerAlias = cust"
          "    let boxVal: Lib.Signed.SignedBox = { Item = cust }"
          "    let kind = Urgent cust"
          "    let useWorker (w: IWorker) (bp: BaseProcessor) = w.Execute(1), bp.Process(\"x\")"
          "    cust.Name, spec, aliasVal, boxVal.Item, kind, useWorker" ]

let private copyMulti () : string * IDisposable =
    let fixturesRoot = Path.GetDirectoryName PublishTests.depsDir
    let multiSrc = Path.Combine(fixturesRoot, "Multi")
    let repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)
    let root = Path.Combine(Path.GetTempPath(), "rfs-task4-lsp-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let libDst = Path.Combine(root, "Lib")
    let appDst = Path.Combine(root, "App")
    Directory.CreateDirectory libDst |> ignore
    Directory.CreateDirectory appDst |> ignore
    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(root, "global.json"))

    for f in [ "Directory.Build.props"; "Directory.Packages.props" ] do
        let src = Path.Combine(multiSrc, f)

        if File.Exists src then
            File.Copy(src, Path.Combine(root, f))

    for f in Directory.GetFiles(Path.Combine(multiSrc, "Lib")) do
        File.Copy(f, Path.Combine(libDst, Path.GetFileName f))

    for f in Directory.GetFiles(Path.Combine(multiSrc, "App")) do
        File.Copy(f, Path.Combine(appDst, Path.GetFileName f))

    let cleanup =
        { new IDisposable with
            member _.Dispose() =
                try
                    if Directory.Exists root then
                        Directory.Delete(root, true)
                with _ ->
                    () }

    root, cleanup

let private loadProjects (lsp: LspClient) (projPaths: string list) : Task =
    task {
        let docs = JsonArray()

        for projPath in projPaths do
            let doc = JsonObject()
            doc["Uri"] <- JsonValue.Create(Paths.toUri projPath)
            docs.Add doc

        let p = JsonObject()
        p["TextDocuments"] <- docs
        let! _ = lsp.Request("fsharp/workspaceLoad", p) |> within
        ()
    }

let private parseLocations (node: JsonNode) : (string * int * int * int * int)[] =
    let one (loc: JsonNode) =
        let uri = Paths.ofUri (loc["uri"].GetValue<string>())
        let r = loc["range"]
        let sl = r["start"].["line"].GetValue<int>()
        let sc = r["start"].["character"].GetValue<int>()
        let el = r["end"].["line"].GetValue<int>()
        let ec = r["end"].["character"].GetValue<int>()
        Path.GetFullPath(uri).ToLowerInvariant(), sl, sc, el, ec

    match node with
    | null -> [||]
    | :? JsonArray as arr -> [| for item in arr -> one item |]
    | obj -> [| one obj |]

[<Tests>]
let featureTests =
    testList
        "SignatureHelpAndNavigation.Features"
        [ testTask "SignatureHelp: overloaded methods, curried functions, pipeline saturation, and live /// doc updates" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", typesFs; "Signed.fsi", signedFsi; "Signed.fs", signedFs ]
                      [ "Use.fs", appUseFs ]
                      (fun libGraph appGraph libPath appPath scope ->
                          task {
                              let usePath = appPath "Use.fs"
                              let typesPath = libPath "Types.fs"

                              // 1) Method call at arg 0 (`Compute(|10, "a")`) -> ActiveParameter = 0
                              let posArg0 = findAfter appUseFs "let m0 = OverloadedCalc.Compute(" 0
                              let! sh0 = SignatureHelp.at scope.OwnersOf appGraph usePath (fst posArg0) (snd posArg0) CancellationToken.None
                              Expect.isSome sh0 "SignatureHelp at first method arg must return Some"
                              Expect.equal sh0.Value.Signatures.Length 2 "OverloadedCalc.Compute has 2 overloads"
                              Expect.equal sh0.Value.ActiveParameter (Some 0u) "First method arg -> ActiveParameter = 0u"
                              Expect.equal sh0.Value.ActiveSignature (Some 0u) "First method arg -> ActiveSignature = 0u"

                              // 2) Method call at arg 1 (`Compute(10, |"a")`) -> ActiveParameter = 1
                              let posArg1 = findAfter appUseFs "let m0 = OverloadedCalc.Compute(10, " 0
                              let! sh1 = SignatureHelp.at scope.OwnersOf appGraph usePath (fst posArg1) (snd posArg1) CancellationToken.None
                              Expect.isSome sh1 "SignatureHelp after first comma must return Some"
                              Expect.equal sh1.Value.ActiveParameter (Some 1u) "Second method arg -> ActiveParameter = 1u"

                              // 3) Method call at arg 2 (`Compute(10, "a", |true)`) -> ActiveParameter = 2, ActiveSignature = 1 (3-param overload)
                              let posArg2 = findAfter appUseFs "let m1 = OverloadedCalc.Compute(10, \"a\", " 0
                              let! sh2 = SignatureHelp.at scope.OwnersOf appGraph usePath (fst posArg2) (snd posArg2) CancellationToken.None
                              Expect.isSome sh2 "SignatureHelp after second comma must return Some"
                              Expect.equal sh2.Value.ActiveParameter (Some 2u) "Third method arg -> ActiveParameter = 2u"
                              Expect.equal sh2.Value.ActiveSignature (Some 1u) "Third method arg must select 3-parameter overload (index 1)"

                              let threeParamOverload = sh2.Value.Signatures[1]
                              Expect.isSome threeParamOverload.Parameters "Overload 1 must expose Parameters"
                              let paramLabels = threeParamOverload.Parameters.Value |> Array.map (fun p -> match p.Label with U2.C1 s -> s | _ -> "")
                              Expect.equal paramLabels [| "first"; "second"; "third" |] "Overload 1 parameter labels"

                              // 4) Curried function application across all 3 curried groups (`combineThree 1 "b" false`)
                              let curPos0 = findAfter appUseFs "let c0 = combineThree " 0
                              let curPos1 = findAfter appUseFs "let c0 = combineThree 1 " 0
                              let curPos2 = findAfter appUseFs "let c0 = combineThree 1 \"b\" " 0

                              let! csh0 = SignatureHelp.at scope.OwnersOf appGraph usePath (fst curPos0) (snd curPos0) CancellationToken.None
                              let! csh1 = SignatureHelp.at scope.OwnersOf appGraph usePath (fst curPos1) (snd curPos1) CancellationToken.None
                              let! csh2 = SignatureHelp.at scope.OwnersOf appGraph usePath (fst curPos2) (snd curPos2) CancellationToken.None

                              Expect.isSome csh0 "Curried arg 0 must return SignatureHelp"
                              Expect.isSome csh1 "Curried arg 1 must return SignatureHelp"
                              Expect.isSome csh2 "Curried arg 2 must return SignatureHelp"
                              Expect.equal csh0.Value.ActiveParameter (Some 0u) "Curried arg 0 -> ActiveParameter = 0u"
                              Expect.equal csh1.Value.ActiveParameter (Some 1u) "Curried arg 1 -> ActiveParameter = 1u"
                              Expect.equal csh2.Value.ActiveParameter (Some 2u) "Curried arg 2 -> ActiveParameter = 2u"

                              let curriedParamNames =
                                  csh2.Value.Signatures[0].Parameters.Value
                                  |> Array.map (fun p -> match p.Label with U2.C1 s -> s | _ -> "")

                              Expect.equal curriedParamNames [| "alpha"; "beta"; "gamma" |] "All 3 curried parameter groups must be flattened into Parameters"

                              // 5) Pipeline curried call: `5 |> subTwo |` -> ActiveParameter = 0u; `5 |> subTwo 20 |` (saturated!) -> None
                              let pipeOpenPos = findAfter appUseFs "let p0 = 5 |> subTwo " 0
                              let pipeSatPos = findAfter appUseFs "let pSat = 5 |> subTwo 20 " 0
                              let! pshOpen = SignatureHelp.at scope.OwnersOf appGraph usePath (fst pipeOpenPos) (snd pipeOpenPos) CancellationToken.None
                              let! pshSat = SignatureHelp.at scope.OwnersOf appGraph usePath (fst pipeSatPos) (snd pipeSatPos) CancellationToken.None
                              Expect.isSome pshOpen "Unsaturated pipeline `5 |> subTwo |` must return SignatureHelp"
                              Expect.equal pshOpen.Value.ActiveParameter (Some 0u) "Unsaturated pipeline `5 |> subTwo |` highlights first explicit parameter (0u)"
                              Expect.isNone pshSat "Saturated pipeline `5 |> subTwo 20 |` must return None"

                              // 6) Live `///` XML doc update in Lib/Types.fs reflected immediately in App/Use.fs SignatureHelp
                              let extractDoc (sh: SignatureHelp) =
                                  match sh.Signatures[0].Documentation with
                                  | Some(U2.C2 mc) -> mc.Value
                                  | Some(U2.C1 s) -> s
                                  | None -> ""

                              Expect.stringContains (extractDoc csh0.Value) "Combines three curried arguments" "Initial /// doc present"
                              let updatedTypes = typesFs.Replace("Combines three curried arguments into a label.", "UPDATED LIVE SIGNATURE DOC.")
                              libGraph.SetText(typesPath, updatedTypes) |> ignore
                              let! cshUpdated = SignatureHelp.at scope.OwnersOf appGraph usePath (fst curPos0) (snd curPos0) CancellationToken.None
                              Expect.isSome cshUpdated "Updated SignatureHelp must return Some"
                              Expect.stringContains (extractDoc cshUpdated.Value) "UPDATED LIVE SIGNATURE DOC." "Live /// doc update reflected cross-project"
                          })
          }

          testTask "DocumentHighlight: .fs and .fsi occurrences, Write vs Read kinds, qualifier stripping, backticks, and locals" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", typesFs; "Signed.fsi", signedFsi; "Signed.fs", signedFs ]
                      [ "Use.fs", appUseFs ]
                      (fun libGraph appGraph libPath appPath _ ->
                          task {
                              let usePath = appPath "Use.fs"
                              let fsiPath = libPath "Signed.fsi"
                              let fsPath = libPath "Signed.fs"

                              // 1) Qualifier stripping in Use.fs: `Lib.Signed.makeCustomer` -> `makeCustomer` (Read)
                              let mcUsePos = findPos appUseFs "Lib.Signed.makeCustomer" 0 14
                              let! mcHighlights = DocumentHighlight.at appGraph usePath (fst mcUsePos) (snd mcUsePos) CancellationToken.None
                              Expect.isSome mcHighlights "DocumentHighlight on qualified symbol must return Some"
                              Expect.equal mcHighlights.Value.Length 1 "makeCustomer occurs once in Use.fs"
                              Expect.equal (sliceLspRange appUseFs mcHighlights.Value[0].Range) "makeCustomer" "Leading Lib.Signed. qualifier must be stripped"
                              Expect.equal mcHighlights.Value[0].Kind (Some DocumentHighlightKind.Read) "Use site must have DocumentHighlightKind.Read"

                              // 2) Backticked qualified symbol `Lib.Signed.``special value``` -> preserves ``special value``
                              let specPos = findPos appUseFs "Lib.Signed.``special value``" 0 15
                              let! specHighlights = DocumentHighlight.at appGraph usePath (fst specPos) (snd specPos) CancellationToken.None
                              Expect.isSome specHighlights "DocumentHighlight on backticked symbol must return Some"
                              Expect.equal specHighlights.Value.Length 1 "``special value`` occurs once in Use.fs"
                              Expect.equal (sliceLspRange appUseFs specHighlights.Value[0].Range) "``special value``" "Backticked identifier span must stay intact"

                              // 3) Signed.fs: `makeCustomer` has 1 Write (definition) and 1 Read (use in `makeCustomer 7`)
                              let mcDefPos = findPos signedFs "let makeCustomer" 0 6
                              let! fsHighlights = DocumentHighlight.at libGraph fsPath (fst mcDefPos) (snd mcDefPos) CancellationToken.None
                              Expect.isSome fsHighlights "DocumentHighlight in Signed.fs must return Some"
                              Expect.equal fsHighlights.Value.Length 2 "makeCustomer occurs twice in Signed.fs"
                              let writes = fsHighlights.Value |> Array.filter (fun h -> h.Kind = Some DocumentHighlightKind.Write)
                              let reads = fsHighlights.Value |> Array.filter (fun h -> h.Kind = Some DocumentHighlightKind.Read)
                              Expect.equal writes.Length 1 "Exactly 1 Write highlight at definition"
                              Expect.equal reads.Length 1 "Exactly 1 Read highlight at call site"
                              for h in fsHighlights.Value do
                                  Expect.equal (sliceLspRange signedFs h.Range) "makeCustomer" "Highlight range in Signed.fs covers identifier only"

                              // 4) Local variable in Signed.fs (`nextId` has 1 Write and 1 Read)
                              let localPos = findPos signedFs "let nextId" 0 5
                              let! localHighlights = DocumentHighlight.at libGraph fsPath (fst localPos) (snd localPos) CancellationToken.None
                              Expect.isSome localHighlights "DocumentHighlight on local binding must return Some"
                              Expect.equal localHighlights.Value.Length 2 "nextId has 1 definition and 1 use"
                              Expect.equal (localHighlights.Value |> Array.filter (fun h -> h.Kind = Some DocumentHighlightKind.Write) |> Array.length) 1 "Local definition -> Write"
                              Expect.equal (localHighlights.Value |> Array.filter (fun h -> h.Kind = Some DocumentHighlightKind.Read) |> Array.length) 1 "Local reference -> Read"

                              // 5) Signed.fsi (.fsi support): `Customer` appears 3 times as Read, `makeCustomer` appears once as Write
                              let fsiTypePos = findPos signedFsi "Item: Customer" 0 8
                              let! fsiCustHighlights = DocumentHighlight.at libGraph fsiPath (fst fsiTypePos) (snd fsiTypePos) CancellationToken.None
                              Expect.isSome fsiCustHighlights "DocumentHighlight in .fsi on Customer must return Some"
                              Expect.equal fsiCustHighlights.Value.Length 3 "Customer occurs 3 times in Signed.fsi"
                              for h in fsiCustHighlights.Value do
                                  Expect.equal h.Kind (Some DocumentHighlightKind.Read) "All Customer references in Signed.fsi are Read"
                                  Expect.equal (sliceLspRange signedFsi h.Range) "Customer" ".fsi Customer highlight range"

                              let fsiValPos = findPos signedFsi "val makeCustomer" 0 6
                              let! fsiValHighlights = DocumentHighlight.at libGraph fsiPath (fst fsiValPos) (snd fsiValPos) CancellationToken.None
                              Expect.isSome fsiValHighlights "DocumentHighlight in .fsi on val makeCustomer must return Some"
                              Expect.equal fsiValHighlights.Value.Length 1 "makeCustomer declared once in Signed.fsi"
                              Expect.equal fsiValHighlights.Value[0].Kind (Some DocumentHighlightKind.Write) "Signature declaration in .fsi is Write"
                          })
          }

          testTask "Definition (.fsi <-> .fs toggle) and Declaration (.fsi preference + .fs fallback)" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", typesFs; "Signed.fsi", signedFsi; "Signed.fs", signedFs ]
                      [ "Use.fs", appUseFs ]
                      (fun libGraph appGraph libPath appPath scope ->
                          task {
                              let typesPath = libPath "Types.fs"
                              let fsiPath = libPath "Signed.fsi"
                              let fsPath = libPath "Signed.fs"
                              let usePath = appPath "Use.fs"

                              // 1) Definition.at from .fsi declaration (`val makeCustomer` and `type SignedBox`) -> jumps to paired Signed.fs implementation!
                              let fsiMakePos = findPos signedFsi "val makeCustomer" 0 6
                              let! defFromFsiVal = Definition.at scope.OwnersOf libGraph fsiPath (fst fsiMakePos) (snd fsiMakePos) CancellationToken.None
                              Expect.isSome defFromFsiVal "Definition on val in .fsi must jump to paired .fs"
                              Expect.isTrue (sameFile defFromFsiVal.Value.FileName fsPath) "Definition on val in Signed.fsi lands in Signed.fs"
                              Expect.equal (sliceFcsRange signedFs defFromFsiVal.Value) "makeCustomer" "Lands on makeCustomer implementation in Signed.fs"

                              let fsiBoxPos = findPos signedFsi "type SignedBox" 0 7
                              let! defFromFsiType = Definition.at scope.OwnersOf libGraph fsiPath (fst fsiBoxPos) (snd fsiBoxPos) CancellationToken.None
                              Expect.isSome defFromFsiType "Definition on type in .fsi must jump to paired .fs"
                              Expect.isTrue (sameFile defFromFsiType.Value.FileName fsPath) "Definition on type SignedBox in Signed.fsi lands in Signed.fs"
                              Expect.equal (sliceFcsRange signedFs defFromFsiType.Value) "SignedBox" "Lands on SignedBox in Signed.fs"

                              // 2) Definition.at from .fsi reference to an earlier file's symbol (`Customer` in Signed.fsi) -> jumps to Types.fs!
                              let fsiCustPos = findPos signedFsi "Item: Customer" 0 8
                              let! defFromFsiRef = Definition.at scope.OwnersOf libGraph fsiPath (fst fsiCustPos) (snd fsiCustPos) CancellationToken.None
                              Expect.isSome defFromFsiRef "Definition on Customer use in Signed.fsi must jump to Types.fs"
                              Expect.isTrue (sameFile defFromFsiRef.Value.FileName typesPath) "Lands in Types.fs"
                              Expect.equal (sliceFcsRange typesFs defFromFsiRef.Value) "Customer" "Lands on Customer definition in Types.fs"

                              // 3) Definition.at from .fs use site (`Lib.Signed.makeCustomer` in Use.fs) -> untouched, lands in Signed.fsi
                              let useMakePos = findPos appUseFs "Lib.Signed.makeCustomer" 0 15
                              let! defFromUse = Definition.at scope.OwnersOf appGraph usePath (fst useMakePos) (snd useMakePos) CancellationToken.None
                              Expect.isSome defFromUse "Definition on signed symbol from Use.fs must succeed"
                              Expect.isTrue (sameFile defFromUse.Value.FileName fsiPath) "Existing .fs use-site Definition lands in Signed.fsi"

                              // 4) Declaration.at resolves .fsi signature declaration from Use.fs, from Signed.fs implementation, AND from Signed.fsi
                              let! declFromUse = Declaration.at scope.OwnersOf appGraph usePath (fst useMakePos) (snd useMakePos) CancellationToken.None
                              Expect.isSome declFromUse "Declaration from Use.fs must succeed"
                              Expect.isTrue (sameFile declFromUse.Value.FileName fsiPath) "Declaration from Use.fs lands in Signed.fsi"
                              Expect.equal (sliceFcsRange signedFsi declFromUse.Value) "makeCustomer" "Declaration span in Signed.fsi"

                              let implMakePos = findPos signedFs "let makeCustomer" 0 6
                              let! declFromImpl = Declaration.at scope.OwnersOf libGraph fsPath (fst implMakePos) (snd implMakePos) CancellationToken.None
                              Expect.isSome declFromImpl "Declaration from Signed.fs implementation binding must find Signed.fsi"
                              Expect.isTrue (sameFile declFromImpl.Value.FileName fsiPath) "Declaration from Signed.fs implementation lands in Signed.fsi"
                              Expect.equal (sliceFcsRange signedFsi declFromImpl.Value) "makeCustomer" "Declaration span in Signed.fsi"

                              let implPropPos = findPos signedFs "member _.Tag" 0 10
                              let! declFromImplProp = Declaration.at scope.OwnersOf libGraph fsPath (fst implPropPos) (snd implPropPos) CancellationToken.None
                              Expect.isSome declFromImplProp "Declaration from Signed.fs property implementation must find Signed.fsi"
                              Expect.isTrue (sameFile declFromImplProp.Value.FileName fsiPath) "Declaration on property in Signed.fs lands in Signed.fsi"
                              Expect.equal (sliceFcsRange signedFsi declFromImplProp.Value) "Tag" "Declaration span for property Tag in Signed.fsi"

                              let! declFromFsi = Declaration.at scope.OwnersOf libGraph fsiPath (fst fsiMakePos) (snd fsiMakePos) CancellationToken.None
                              Expect.isSome declFromFsi "Declaration on .fsi signature declaration returns that .fsi declaration"
                              Expect.isTrue (sameFile declFromFsi.Value.FileName fsiPath) "Declaration in Signed.fsi stays on Signed.fsi signature"

                              // 5) Declaration.at falls back to primary .fs definition when symbol has NO .fsi (`combineThree` in Types.fs, `helperSecret` in Signed.fs)
                              let useCombinePos = findPos appUseFs "combineThree 1" 0 3
                              let! declUnsigned = Declaration.at scope.OwnersOf appGraph usePath (fst useCombinePos) (snd useCombinePos) CancellationToken.None
                              Expect.isSome declUnsigned "Declaration on unsigned symbol must fall back to .fs definition"
                              Expect.isTrue (sameFile declUnsigned.Value.FileName typesPath) "Unsigned symbol Declaration lands in Types.fs"
                              Expect.equal (sliceFcsRange typesFs declUnsigned.Value) "combineThree" "Lands on combineThree in Types.fs"

                              let secretUsePos = findPos signedFs "helperSecret id" 0 3
                              let! declPrivate = Declaration.at scope.OwnersOf libGraph fsPath (fst secretUsePos) (snd secretUsePos) CancellationToken.None
                              Expect.isSome declPrivate "Declaration on internal symbol unexposed by .fsi falls back to Signed.fs"
                              Expect.isTrue (sameFile declPrivate.Value.FileName fsPath) "Unexposed symbol Declaration lands in Signed.fs"
                              Expect.equal (sliceFcsRange signedFs declPrivate.Value) "helperSecret" "Lands on helperSecret in Signed.fs"
                          })
          }

          testTask "TypeDefinition: value, parameter, record field, union case, type abbreviation unwrapping, .fs and .fsi" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", typesFs; "Signed.fsi", signedFsi; "Signed.fs", signedFs ]
                      [ "Use.fs", appUseFs ]
                      (fun libGraph appGraph libPath appPath scope ->
                          task {
                              let typesPath = libPath "Types.fs"
                              let fsiPath = libPath "Signed.fsi"
                              let fsPath = libPath "Signed.fs"
                              let usePath = appPath "Use.fs"

                              // 1) Local value `cust` (of type `Customer`) -> Customer in Types.fs
                              let custValPos = findPos appUseFs "let cust =" 0 5
                              let! tdCust = TypeDefinition.at scope.OwnersOf appGraph usePath (fst custValPos) (snd custValPos) CancellationToken.None
                              Expect.isSome tdCust "TypeDefinition on value `cust` must resolve Customer"
                              Expect.isTrue (sameFile tdCust.Value.FileName typesPath) "Customer type is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs tdCust.Value) "Customer" "Lands on Customer type definition"

                              // 2) Function parameter `w` in `(w: IWorker)` -> IWorker in Types.fs
                              let paramPos = findPos appUseFs "(w: IWorker)" 0 1
                              let! tdParam = TypeDefinition.at scope.OwnersOf appGraph usePath (fst paramPos) (snd paramPos) CancellationToken.None
                              Expect.isSome tdParam "TypeDefinition on parameter `w` must resolve IWorker"
                              Expect.isTrue (sameFile tdParam.Value.FileName typesPath) "IWorker is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs tdParam.Value) "IWorker" "Lands on IWorker"

                              // 3) Record field `boxVal.Item` (field type is `Customer`) -> Customer in Types.fs
                              let fieldPos = findPos appUseFs "boxVal.Item" 0 8
                              let! tdField = TypeDefinition.at scope.OwnersOf appGraph usePath (fst fieldPos) (snd fieldPos) CancellationToken.None
                              Expect.isSome tdField "TypeDefinition on record field `.Item` must resolve field type Customer"
                              Expect.isTrue (sameFile tdField.Value.FileName typesPath) "Customer is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs tdField.Value) "Customer" "Lands on Customer"

                              // 4) Union case `Urgent` -> OrderKind in Types.fs
                              let casePos = findPos appUseFs "Urgent cust" 0 2
                              let! tdCase = TypeDefinition.at scope.OwnersOf appGraph usePath (fst casePos) (snd casePos) CancellationToken.None
                              Expect.isSome tdCase "TypeDefinition on union case `Urgent` must resolve OrderKind"
                              Expect.isTrue (sameFile tdCase.Value.FileName typesPath) "OrderKind is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs tdCase.Value) "OrderKind" "Lands on OrderKind"

                              // 5) Type abbreviation unwrapping: `aliasVal` (typed `CustomerAlias = Customer`) and `CustomerAlias` entity -> underlying `Customer`!
                              let aliasValPos = findPos appUseFs "let aliasVal:" 0 6
                              let! tdAliasVal = TypeDefinition.at scope.OwnersOf appGraph usePath (fst aliasValPos) (snd aliasValPos) CancellationToken.None
                              Expect.isSome tdAliasVal "TypeDefinition on value of abbreviated type CustomerAlias must unwrap to Customer"
                              Expect.isTrue (sameFile tdAliasVal.Value.FileName typesPath) "Underlying Customer is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs tdAliasVal.Value) "Customer" "Unwraps CustomerAlias to Customer"

                              let aliasEntityPos = findPos appUseFs "CustomerAlias = cust" 0 4
                              let! tdAliasEnt = TypeDefinition.at scope.OwnersOf appGraph usePath (fst aliasEntityPos) (snd aliasEntityPos) CancellationToken.None
                              Expect.isSome tdAliasEnt "TypeDefinition on type alias CustomerAlias itself unwraps to underlying Customer"
                              Expect.equal (sliceFcsRange typesFs tdAliasEnt.Value) "Customer" "Unwraps CustomerAlias entity to Customer"

                              // 6) .fsi support: ```special value``` in Signed.fsi has type `Customer` -> Customer in Types.fs
                              let fsiValPos = findPos signedFsi "val ``special value``" 0 8
                              let! tdFsiVal = TypeDefinition.at scope.OwnersOf libGraph fsiPath (fst fsiValPos) (snd fsiValPos) CancellationToken.None
                              Expect.isSome tdFsiVal "TypeDefinition on val in .fsi must resolve its type Customer"
                              Expect.isTrue (sameFile tdFsiVal.Value.FileName typesPath) "Customer is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs tdFsiVal.Value) "Customer" "Lands on Customer"

                              // Signed type `boxVal` -> lands on SignedBox in Signed.fsi (or Signed.fs via CurrentLocation)
                              let boxValPos = findPos appUseFs "let boxVal:" 0 5
                              let! tdSigned = TypeDefinition.at scope.OwnersOf appGraph usePath (fst boxValPos) (snd boxValPos) CancellationToken.None
                              Expect.isSome tdSigned "TypeDefinition on boxVal must resolve SignedBox"
                              Expect.isTrue (sameFile tdSigned.Value.FileName fsiPath || sameFile tdSigned.Value.FileName fsPath) "SignedBox resolved in workspace"
                          })
          }

          testTask "Implementation: interface, object expression, abstract member, cross-project referencing scope, and .fsi -> .fs" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", typesFs; "Signed.fsi", signedFsi; "Signed.fs", signedFs ]
                      [ "Use.fs", appUseFs ]
                      (fun libGraph appGraph libPath appPath scope ->
                          task {
                              let typesPath = libPath "Types.fs"
                              let fsiPath = libPath "Signed.fsi"
                              let fsPath = libPath "Signed.fs"
                              let usePath = appPath "Use.fs"

                              // 1) Interface `IWorker` in Types.fs -> finds LocalWorker (Types.fs), AppWorker (Use.fs), AND `{ new IWorker ... }` object expression (Use.fs)!
                              let iworkerPos = findPos typesFs "type IWorker" 0 7
                              let! ifaceRes = Implementation.find scope libGraph typesPath (fst iworkerPos) (snd iworkerPos) CancellationToken.None
                              let ifaceRanges = Expect.wantOk ifaceRes "Implementation.find on IWorker must succeed"
                              Expect.equal ifaceRanges.Length 3 "IWorker has 3 implementations across Lib and referencing App (LocalWorker, AppWorker, object expr)"

                              let filesHit = ifaceRanges |> Array.map (fun r -> Path.GetFileName r.FileName)
                              Expect.contains filesHit "Types.fs" "Includes LocalWorker in Lib/Types.fs"
                              Expect.equal (filesHit |> Array.filter ((=) "Use.fs") |> Array.length) 2 "Includes AppWorker and object expression in referencing App/Use.fs"

                              // Deterministic sort check
                              let sortedRanges =
                                  ifaceRanges
                                  |> Array.sortBy (fun r -> r.FileName.ToUpperInvariant(), r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
                              Expect.equal ifaceRanges sortedRanges "Implementation ranges must be deterministically sorted"

                              // 2) Interface member dispatch slot `w.Execute(1)` in Use.fs -> finds all 3 concrete `Execute` member implementations!
                              let execCallPos = findPos appUseFs "w.Execute(1)" 0 4
                              let! execRes = Implementation.find scope appGraph usePath (fst execCallPos) (snd execCallPos) CancellationToken.None
                              let execRanges = Expect.wantOk execRes "Implementation.find on IWorker.Execute must succeed"
                              Expect.equal execRanges.Length 3 "IWorker.Execute has 3 concrete member implementations across Lib and App"
                              for r in execRanges do
                                  let src = if sameFile r.FileName typesPath then typesFs else appUseFs
                                  Expect.equal (sliceFcsRange src r) "Execute" "Each implementation range points to Execute member identifier"

                              // 3) Abstract class member `bp.Process("x")` -> finds `ConcreteProcessor.Process` override in Types.fs
                              let procCallPos = findPos appUseFs "bp.Process(\"x\")" 0 4
                              let! procRes = Implementation.find scope appGraph usePath (fst procCallPos) (snd procCallPos) CancellationToken.None
                              let procRanges = Expect.wantOk procRes "Implementation.find on BaseProcessor.Process must succeed"
                              Expect.equal procRanges.Length 1 "BaseProcessor.Process has 1 concrete override"
                              Expect.isTrue (sameFile procRanges[0].FileName typesPath) "Override is in Types.fs"
                              Expect.equal (sliceFcsRange typesFs procRanges[0]) "Process" "Points to Process override"

                              // 4) .fsi signature declaration (`val makeCustomer` and `type SignedBox` in Signed.fsi) -> includes paired Signed.fs implementation!
                              let fsiValPos = findPos signedFsi "val makeCustomer" 0 6
                              let! fsiValImpl = Implementation.find scope libGraph fsiPath (fst fsiValPos) (snd fsiValPos) CancellationToken.None
                              let fsiValRanges = Expect.wantOk fsiValImpl "Implementation.find on .fsi val must succeed"
                              Expect.equal fsiValRanges.Length 1 "Signed.fsi val makeCustomer has 1 implementation in Signed.fs"
                              Expect.isTrue (sameFile fsiValRanges[0].FileName fsPath) "Points to Signed.fs"
                              Expect.equal (sliceFcsRange signedFs fsiValRanges[0]) "makeCustomer" "Points to makeCustomer in Signed.fs"

                              let fsiTypePos = findPos signedFsi "type SignedBox" 0 7
                              let! fsiTypeImpl = Implementation.find scope libGraph fsiPath (fst fsiTypePos) (snd fsiTypePos) CancellationToken.None
                              let fsiTypeRanges = Expect.wantOk fsiTypeImpl "Implementation.find on .fsi type must succeed"
                              Expect.equal fsiTypeRanges.Length 1 "Signed.fsi type SignedBox has 1 implementation in Signed.fs"
                              Expect.isTrue (sameFile fsiTypeRanges[0].FileName fsPath) "Points to Signed.fs"
                              Expect.equal (sliceFcsRange signedFs fsiTypeRanges[0]) "SignedBox" "Points to SignedBox in Signed.fs"
                          })
          } ]

let private runLspProtocolTest () =
    task {
        let multiRoot, cleanup = copyMulti ()
        use _ = cleanup
        let libProj = Path.Combine(multiRoot, "Lib", "Lib.fsproj")
        let appProj = Path.Combine(multiRoot, "App", "App.fsproj")
        Fixtures.build appProj

        let libFsPath = Path.Combine(multiRoot, "Lib", "Lib.fs")
        let shapesFsPath = Path.Combine(multiRoot, "Lib", "Shapes.fs")
        let signedFsiPath = Path.Combine(multiRoot, "Lib", "Signed.fsi")
        let signedFsPath = Path.Combine(multiRoot, "Lib", "Signed.fs")
        let useFsPath = Path.Combine(multiRoot, "App", "Use.fs")

        let! lsp, owner, serverTask = TestServer.startCapturing ()
        use _ = owner

        let initParams =
            JsonNode.Parse
                $"""{{"processId":null,"rootUri":%s{JsonValue.Create(Paths.toUri multiRoot).ToJsonString()},"capabilities":{{}}}}"""

        let! initReply, _ = lsp.Request("initialize", initParams) |> within
        do! lsp.Notify("initialized", JsonObject()) |> withinUnit
        let! _server = serverTask |> within

        // 1) Capabilities check
        let caps = initReply["capabilities"]
        Expect.isTrue (caps["documentHighlightProvider"].GetValue<bool>()) "documentHighlightProvider advertised"
        Expect.isTrue (caps["declarationProvider"].GetValue<bool>()) "declarationProvider advertised"
        Expect.isTrue (caps["typeDefinitionProvider"].GetValue<bool>()) "typeDefinitionProvider advertised"
        Expect.isTrue (caps["implementationProvider"].GetValue<bool>()) "implementationProvider advertised"

        let sigTriggers =
            [| for c in caps["signatureHelpProvider"].["triggerCharacters"].AsArray() -> c.GetValue<string>() |]

        let sigRetriggers =
            [| for c in caps["signatureHelpProvider"].["retriggerCharacters"].AsArray() -> c.GetValue<string>() |]

        Expect.equal sigTriggers [| "("; ","; " " |] "signatureHelpProvider.triggerCharacters"
        Expect.equal sigRetriggers [| ","; ")"; " " |] "signatureHelpProvider.retriggerCharacters"

        do! loadProjects lsp [ libProj; appProj ]

        // Enrich Signed.fsi / Signed.fs / Lib.fs / Use.fs via DidOpen so we exercise all 6 endpoints end-to-end over JSON-RPC
        let richLibFs =
            String.concat
                "\n"
                [ "module Lib.Api"
                  ""
                  "open System.Runtime.CompilerServices"
                  "open Lib.Shapes"
                  ""
                  "[<assembly: InternalsVisibleTo(\"App\")>]"
                  "do ()"
                  ""
                  "type IRunner ="
                  "    abstract Run: steps: int -> int"
                  ""
                  "/// Adds two ints."
                  "let f (x: int) : int = x + 1"
                  "let internal g (x: int) : int = x + 2"
                  "let shapeOf () : Shape = Circle 1" ]

        let richUseFs =
            String.concat
                "\n"
                [ "module App.Use"
                  ""
                  "open Lib.Api"
                  ""
                  "type AppRunner() ="
                  "    interface IRunner with"
                  "        member _.Run(steps: int) = steps * 2"
                  ""
                  "let y = f 1"
                  "let w = y + Lib.Signed.signed"
                  "let shape = shapeOf ()" ]

        do! lsp.DidOpen(libFsPath, richLibFs) |> withinUnit
        do! lsp.DidOpen(signedFsiPath, File.ReadAllText signedFsiPath) |> withinUnit
        do! lsp.DidOpen(signedFsPath, File.ReadAllText signedFsPath) |> withinUnit
        do! lsp.DidOpen(useFsPath, richUseFs) |> withinUnit

        let posJson (p: string) (line: int, ch: int) =
            JsonNode.Parse
                $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri p).ToJsonString()}}},"position":{{"line":%d{line},"character":%d{ch}}}}}"""

        // a) textDocument/signatureHelp on `let y = f |1`
        let shPos = findAfter richUseFs "let y = f " 0
        let! shReply, _ = lsp.Request("textDocument/signatureHelp", posJson useFsPath shPos) |> within
        Expect.isNotNull shReply "signatureHelp result must be non-null"
        Expect.equal (shReply["activeParameter"].GetValue<int>()) 0 "signatureHelp activeParameter = 0"

        // b) textDocument/documentHighlight on `y` in Use.fs (1 Write + 1 Read)
        let hlPos = findPos richUseFs "let y =" 0 4
        let! hlReply, _ = lsp.Request("textDocument/documentHighlight", posJson useFsPath hlPos) |> within
        Expect.isNotNull hlReply "documentHighlight result must be non-null"
        Expect.equal (hlReply.AsArray().Count) 2 "y has 2 highlights in Use.fs"

        // c) textDocument/definition on `val signed: int` in Signed.fsi -> jumps to Signed.fs!
        let fsiText = File.ReadAllText signedFsiPath
        let fsiPos = findPos fsiText "val signed" 0 5
        let! defFsiReply, _ = lsp.Request("textDocument/definition", posJson signedFsiPath fsiPos) |> within
        let defFsiLocs = parseLocations defFsiReply
        Expect.equal defFsiLocs.Length 1 "definition on .fsi returns 1 location"
        let defFile, _, _, _, _ = defFsiLocs[0]
        Expect.isTrue (sameFile defFile signedFsPath) "textDocument/definition on Signed.fsi jumps to Signed.fs"

        // d) textDocument/declaration on `let signed = 1` in Signed.fs -> jumps to Signed.fsi!
        let fsText = File.ReadAllText signedFsPath
        let fsPos = findPos fsText "let signed" 0 5
        let! declReply, _ = lsp.Request("textDocument/declaration", posJson signedFsPath fsPos) |> within
        let declLocs = parseLocations declReply
        Expect.equal declLocs.Length 1 "declaration returns 1 location"
        let declFile, _, _, _, _ = declLocs[0]
        Expect.isTrue (sameFile declFile signedFsiPath) "textDocument/declaration on Signed.fs jumps to Signed.fsi"

        // e) textDocument/typeDefinition on `let shape =` in Use.fs -> jumps to `Shape` in Shapes.fs!
        let tdPos = findPos richUseFs "let shape =" 0 5
        let! tdReply, _ = lsp.Request("textDocument/typeDefinition", posJson useFsPath tdPos) |> within
        let tdLocs = parseLocations tdReply
        Expect.equal tdLocs.Length 1 "typeDefinition returns 1 location"
        let tdFile, _, _, _, _ = tdLocs[0]
        Expect.isTrue (sameFile tdFile shapesFsPath) "textDocument/typeDefinition on `shape` jumps to Shapes.fs"

        // f) textDocument/implementation on `type IRunner` in Lib.fs -> jumps to `AppRunner` in Use.fs!
        let implPos = findPos richLibFs "type IRunner" 0 6
        let! implReply, _ = lsp.Request("textDocument/implementation", posJson libFsPath implPos) |> within
        let implLocs = parseLocations implReply
        Expect.equal implLocs.Length 1 "implementation returns 1 location"
        let implFile, _, _, _, _ = implLocs[0]
        Expect.isTrue (sameFile implFile useFsPath) "textDocument/implementation on IRunner finds AppRunner in Use.fs"
    }

[<Tests>]
let lspProtocolTests =
    testSequenced
    <| testList
        "SignatureHelpAndNavigation.LspProtocol"
        [ testTask "LSP capabilities and JSON-RPC roundtrips for signatureHelp, documentHighlight, declaration, typeDefinition, implementation, and .fsi definition" {
              do! runLspProtocolTest ()
          } ]
