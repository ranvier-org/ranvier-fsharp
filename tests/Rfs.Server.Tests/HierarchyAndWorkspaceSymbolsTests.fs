module Rfs.Server.Tests.HierarchyAndWorkspaceSymbolsTests

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 60.)
let private withinFast (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 8.)
let private withinUnit (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 60.)

let private posLeq (a: Ionide.LanguageServerProtocol.Types.Position) (b: Ionide.LanguageServerProtocol.Types.Position) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let private rangeContains (outer: Ionide.LanguageServerProtocol.Types.Range) (inner: Ionide.LanguageServerProtocol.Types.Range) =
    posLeq outer.Start inner.Start && posLeq inner.End inner.End && posLeq inner.End outer.End

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

let private sameFile (a: string) (b: string) =
    if String.IsNullOrWhiteSpace a || String.IsNullOrWhiteSpace b then
        false
    else
        String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

let private assertPlainData (label: string) (root: obj) =
    let seen = HashSet<obj>(HashIdentity.Reference)

    let rec walk (o: obj) =
        if not (isNull o) && seen.Add o then
            let t = o.GetType()

            if not t.IsArray && t.Namespace <> null && t.Namespace.StartsWith("FSharp.Compiler", StringComparison.Ordinal) then
                failtest $"%s{label} retains compiler object of type %s{t.FullName}"

            match o with
            | :? string -> ()
            | :? Newtonsoft.Json.Linq.JToken -> ()
            | :? Array as a when not (t.GetElementType().IsPrimitive) ->
                for elem in a do
                    walk elem
            | _ when t.IsPrimitive -> ()
            | _ ->
                for f in t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) do
                    if not f.FieldType.IsPointer then
                        walk (f.GetValue o)

    walk root

let private withTwoProjectWorkspace
    (libFiles: (string * string) list)
    (appFiles: (string * string) list)
    (body: GraphHost -> ProjectGraph -> ProjectGraph -> (string -> string) -> (string -> string) -> References.Scope -> Task<unit>)
    : Task<unit> =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-task5-"

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

            use host = GraphHost.Start "task5"
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

            do! body host libGraph appGraph libPath appPath scope
            appGraph.Dispose()
            libGraph.Dispose()
        finally
            try
                dir.Delete true
            with _ ->
                ()
    }

let private libTypesFs =
    String.concat
        "\n"
        [ "module Lib.Types"
          ""
          "type Customer = { Id: int; Name: string }"
          ""
          "type IWorker ="
          "    abstract Execute: count: int -> string"
          ""
          "type WorkerAlias = IWorker"
          ""
          "[<AbstractClass>]"
          "type BaseProcessor() ="
          "    abstract Process: item: string -> int"
          ""
          "type ConcreteProcessor() ="
          "    inherit BaseProcessor()"
          "    member _.Label = \"concrete\""
          "    override _.Process(item: string) = item.Length"
          "    interface IWorker with"
          "        member _.Execute(count: int) = string count"
          ""
          "let GetSemanticClassification (line: int) = line + 1"
          ""
          "let makeCustomer (id: int) (name: string) = { Id = id; Name = name }"
          ""
          "let makeconstant (id: int) = id"
          ""
          "let someConstant (id: int) = id + 1"
          ""
          "let combineValues (alpha: int) (beta: int) = alpha + beta"
          ""
          "let repeatCombine (seed: int) ="
          "    let first = combineValues seed 1"
          "    let second = combineValues first 2"
          "    second"
          ""
          "let rec countdown (n: int) : int ="
          "    if n <= 0 then 0 else countdown (n - 1)"
          ""
          "let siblingOnlyHelper (v: int) = v * 3" ]

let private libSignedFsi =
    String.concat
        "\n"
        [ "module Lib.Signed"
          ""
          "type SignedWorker ="
          "    new: unit -> SignedWorker"
          ""
          "val signedEntry: x: int -> int" ]

let private libSignedFs =
    String.concat
        "\n"
        [ "module Lib.Signed"
          ""
          "open Lib.Types"
          ""
          "type SignedWorker() ="
          "    interface IWorker with"
          "        member _.Execute(count: int) = string (count * 2)"
          ""
          "let signedEntry (x: int) = combineValues x 10" ]

let private appRunnerFs =
    String.concat
        "\n"
        [ "module App.Runner"
          ""
          "open Lib.Types"
          "open Lib.Signed"
          ""
          "type IAppTag ="
          "    abstract Tag: string"
          ""
          "type AppWorker() ="
          "    interface IWorker with"
          "        member _.Execute(count: int) = string (count + 100)"
          ""
          "type AppSpecialProcessor() ="
          "    inherit ConcreteProcessor()"
          "    interface IAppTag with"
          "        member _.Tag = \"special\""
          ""
          "let localHelper (n: int) = n + 5"
          ""
          "let inspectProcessor (p: ConcreteProcessor) = p.Label"
          ""
          "let unrelatedCaller (z: int) = siblingOnlyHelper z"
          ""
          "let runPipeline (input: int) ="
          "    let localNum = 10"
          "    let step1 = combineValues input localNum"
          "    let step2 = combineValues step1 3"
          "    let step3 = signedEntry step2"
          "    let proc = ConcreteProcessor()"
          "    let step4 = proc.Process(\"abc\")"
          "    localHelper (step3 + step4)" ]

let private extractWorkspaceSymbols (res: U2<SymbolInformation[], WorkspaceSymbol[]> option) : WorkspaceSymbol[] =
    Expect.isSome res "Expected Some workspace/symbol result"

    match res.Value with
    | U2.C2 syms -> syms
    | U2.C1 _ -> failtest "Expected U2.C2 WorkspaceSymbol[]"

[<Tests>]
let featureUnitTests =
    testSequenced
    <| testList
        "HierarchyAndWorkspaceSymbols.Unit"
        [ testTask "WorkspaceSymbols: empty workspace, substring, camelCase/PascalCase subsequence, 0-based locations, .fs + .fsi, and shared-file deduplication" {
              // 1. Empty workspace (0 loaded projects) returns Some(U2.C2 [||]) cleanly
              let! emptyWs = WorkspaceSymbols.search (fun () -> []) (fun () -> false) (fun _ -> []) "Customer" CancellationToken.None |> within
              let emptyArr = extractWorkspaceSymbols emptyWs
              Expect.isEmpty emptyArr "0 loaded projects must return empty WorkspaceSymbol[]"

              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", libTypesFs; "Signed.fsi", libSignedFsi; "Signed.fs", libSignedFs ]
                      [ "Runner.fs", appRunnerFs ]
                      (fun _ libGraph appGraph libPath appPath scope ->
                          task {
                              // 2. Empty query returns declarations across .fs and .fsi with exact 0-based LSP locations
                              let! allRes = WorkspaceSymbols.search scope.Projects scope.IsLoading scope.FilesOf "" CancellationToken.None |> within
                              let allSyms = extractWorkspaceSymbols allRes
                              let allNames = allSyms |> Array.map _.Name |> Set.ofArray
                              Expect.contains allNames "Customer" "Includes Customer from Lib/Types.fs"
                              Expect.contains allNames "GetSemanticClassification" "Includes GetSemanticClassification"
                              Expect.contains allNames "signedEntry" "Includes signedEntry from .fsi/.fs"
                              Expect.contains allNames "AppSpecialProcessor" "Includes AppSpecialProcessor from App/Runner.fs"

                              // Verify exact 0-based Location range slices declaration in source (FSAC fcsRangeToLsp decl.Range)
                              let getSemSym =
                                  allSyms
                                  |> Array.find (fun s -> s.Name = "GetSemanticClassification")

                              match getSemSym.Location with
                              | U2.C1 loc ->
                                  Expect.isTrue (sameFile (Paths.ofUri loc.Uri) (libPath "Types.fs")) "Location URI matches Types.fs"
                                  Expect.equal loc.Range.Start.Line 20u "GetSemanticClassification 0-based Start.Line = 20"
                                  Expect.equal loc.Range.End.Line 20u "GetSemanticClassification 0-based End.Line = 20"
                                  let sliced = sliceLspRange libTypesFs loc.Range
                                  Expect.equal sliced "GetSemanticClassification (line: int) = line + 1" "0-based Location.Range must slice exact declaration"
                              | U2.C2 _ -> failtest "Expected U2.C1 Location"

                              // Verify .fsi symbol is emitted when querying Signed.fsi specifically
                              let! fsiOnlyRes =
                                  WorkspaceSymbols.search
                                      (fun () -> [ libGraph ])
                                      (fun () -> false)
                                      (fun _ -> [ libPath "Signed.fsi" ])
                                      "signedEntry"
                                      CancellationToken.None
                                  |> within

                              let fsiSyms = extractWorkspaceSymbols fsiOnlyRes
                              Expect.hasLength fsiSyms 1 "Signed.fsi emits signedEntry"
                              match fsiSyms[0].Location with
                              | U2.C1 loc ->
                                  Expect.equal loc.Range.Start.Line 5u "signedEntry 0-based Start.Line = 5"
                                  Expect.equal loc.Range.End.Line 5u "signedEntry 0-based End.Line = 5"
                                  let fsiSliced = sliceLspRange libSignedFsi loc.Range
                                  Expect.stringContains fsiSliced "signedEntry" "0-based .fsi Location.Range slices signedEntry declaration"
                              | U2.C2 _ -> failtest "Expected U2.C1 Location"

                              // 3. Case-insensitive substring
                              let! subRes = WorkspaceSymbols.search scope.Projects scope.IsLoading scope.FilesOf "semanticclass" CancellationToken.None |> within
                              let subNames = extractWorkspaceSymbols subRes |> Array.map _.Name
                              Expect.equal subNames [| "GetSemanticClassification" |] "Case-insensitive substring matches GetSemanticClassification"

                              // 4. CamelCase / PascalCase subsequence (including negative non-hump & mid-word checks!)
                              let! gscRes = WorkspaceSymbols.search scope.Projects scope.IsLoading scope.FilesOf "gSC" CancellationToken.None |> within
                              let gscNames = extractWorkspaceSymbols gscRes |> Array.map _.Name
                              Expect.contains gscNames "GetSemanticClassification" "'gSC' must match 'GetSemanticClassification'"

                              let! mcRes = WorkspaceSymbols.search scope.Projects scope.IsLoading scope.FilesOf "mC" CancellationToken.None |> within
                              let mcNames = extractWorkspaceSymbols mcRes |> Array.map _.Name |> Set.ofArray
                              Expect.contains mcNames "makeCustomer" "'mC' must match 'makeCustomer'"
                              Expect.isFalse (mcNames.Contains "makeconstant") "'mC' MUST NOT match 'makeconstant' (lowercase 'c' is not a camelCase hump)"
                              Expect.isFalse (mcNames.Contains "someConstant") "'mC' MUST NOT match 'someConstant' ('m' is mid-word, not a word start)"

                              let! mkCustRes = WorkspaceSymbols.search scope.Projects scope.IsLoading scope.FilesOf "mkCust" CancellationToken.None |> within
                              let mkCustNames = extractWorkspaceSymbols mkCustRes |> Array.map _.Name
                              Expect.equal mkCustNames [| "makeCustomer" |] "'mkCust' must match 'makeCustomer'"

                              // 5. Shared-file deduplication across multi-target graphs
                              let! baseSpec = ProjectSpec.load PublishTests.depsProject
                              let baseSeq = ProjectSpec.sequential baseSpec
                              let dupSpec =
                                  { baseSeq with
                                      ProjectFile = Path.Combine(Path.GetDirectoryName(libPath "Lib.fsproj"), "LibNet8.fsproj")
                                      SourceFiles = [ libPath "Types.fs" ]
                                      GeneratedFiles = [] }

                              use dupGraph = (ProjectGraph.Create(libGraph.Host, dupSpec) |> within).Result
                              let! _ = dupGraph.AllDiagnostics() |> within

                              let! dedupRes =
                                  WorkspaceSymbols.search
                                      (fun () -> [ libGraph; dupGraph; appGraph ])
                                      (fun () -> false)
                                      (fun g ->
                                          if obj.ReferenceEquals(g, dupGraph) then [ libPath "Types.fs" ]
                                          else scope.FilesOf g)
                                      "GetSemanticClassification"
                                      CancellationToken.None
                                  |> within

                              let dedupSyms = extractWorkspaceSymbols dedupRes
                              Expect.hasLength dedupSyms 1 "Shared file across multiple ProjectGraphs must be deduplicated to 1 symbol"

                              // 6. workspaceSymbol/resolve preserves symbol & location
                              let resolved = WorkspaceSymbols.resolve dedupSyms[0]
                              Expect.equal resolved.Name "GetSemanticClassification" "resolve preserves Name"
                              Expect.equal resolved.Location dedupSyms[0].Location "resolve preserves Location"
                          })
          }

          testTask "CallHierarchy: prepare (.fs & .fsi, call site & decl, non-callable None), cross-project incomingCalls (grouping & non-def), and outgoingCalls (body isolation & .fsi->.fs)" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", libTypesFs; "Signed.fsi", libSignedFsi; "Signed.fs", libSignedFs ]
                      [ "Runner.fs", appRunnerFs ]
                      (fun _ libGraph appGraph libPath appPath scope ->
                          task {
                              let typesPath = libPath "Types.fs"
                              let signedFsiPath = libPath "Signed.fsi"
                              let signedFsPath = libPath "Signed.fs"
                              let runnerPath = appPath "Runner.fs"

                              // 1. Non-callable parameter & local variable return None
                              let paramLine, paramCol = findPos libTypesFs "(alpha: int)" 0 2
                              let! nonCallableParam = CallHierarchy.prepare scope.OwnersOf libGraph typesPath paramLine paramCol CancellationToken.None |> within
                              Expect.isNone nonCallableParam "Parameter must not prepare a CallHierarchyItem"

                              let localLine, localCol = findPos appRunnerFs "let localNum = 10" 0 5
                              let! nonCallableLocal = CallHierarchy.prepare scope.OwnersOf appGraph runnerPath localLine localCol CancellationToken.None |> within
                              Expect.isNone nonCallableLocal "Non-function local variable must not prepare a CallHierarchyItem"

                              // 2. Prepare on combineValues call site in App/Runner.fs resolves Lib/Types.fs declaration
                              let callLine, callCol = findPos appRunnerFs "combineValues input localNum" 0 3
                              let! prepCombineOpt = CallHierarchy.prepare scope.OwnersOf appGraph runnerPath callLine callCol CancellationToken.None |> within
                              Expect.isSome prepCombineOpt "prepare on combineValues call site succeeds"
                              Expect.hasLength prepCombineOpt.Value 1 "Single CallHierarchyItem"
                              let combineItem = prepCombineOpt.Value[0]
                              Expect.equal combineItem.Name "combineValues" "Prepared item name"
                              Expect.equal combineItem.Kind SymbolKind.Function "Function kind"
                              Expect.isTrue (sameFile (Paths.ofUri combineItem.Uri) typesPath) "Resolved to Lib/Types.fs"
                              Expect.isTrue (rangeContains combineItem.Range combineItem.SelectionRange) "Range must enclose SelectionRange"
                              Expect.equal (sliceLspRange libTypesFs combineItem.SelectionRange) "combineValues" "SelectionRange slices identifier"
                              assertPlainData "CallHierarchyItem" combineItem

                              // 3. incomingCalls on combineValues across Lib + App:
                              //    - repeatCombine in Lib/Types.fs (2 call sites grouped into 1 item with FromRanges.Length = 2!)
                              //    - signedEntry in Lib/Signed.fs (1 call site)
                              //    - runPipeline in App/Runner.fs (2 call sites grouped into 1 item with FromRanges.Length = 2!)
                              //    - combineValues itself MUST NOT appear (definition hit excluded!)
                              let! incomingCombineOpt = CallHierarchy.incoming scope libGraph combineItem CancellationToken.None |> within
                              Expect.isSome incomingCombineOpt "incomingCalls on combineValues succeeds"
                              let incomingCombine = incomingCombineOpt.Value
                              let callerNames = incomingCombine |> Array.map (fun c -> c.From.Name) |> Set.ofArray
                              Expect.equal callerNames (Set.ofList [ "repeatCombine"; "signedEntry"; "runPipeline" ]) "Exact callers across Lib and App (excluding own definition)"

                              let repeatCall = incomingCombine |> Array.find (fun c -> c.From.Name = "repeatCombine")
                              Expect.hasLength repeatCall.FromRanges 2 "repeatCombine calls combineValues twice -> grouped into 2 FromRanges"
                              Expect.isTrue (repeatCall.FromRanges |> Array.forall (rangeContains repeatCall.From.Range)) "All FromRanges inside caller Range"
                              assertPlainData "CallHierarchyIncomingCall" repeatCall

                              let pipelineCall = incomingCombine |> Array.find (fun c -> c.From.Name = "runPipeline")
                              Expect.hasLength pipelineCall.FromRanges 2 "Cross-project runPipeline calls combineValues twice -> 2 FromRanges"

                              // 4. Recursive function countdown: incomingCalls includes countdown ONCE with FromRanges.Length = 1 (recursive call only, NOT header!)
                              let recLine, recCol = findPos libTypesFs "let rec countdown" 0 9
                              let! prepRecOpt = CallHierarchy.prepare scope.OwnersOf libGraph typesPath recLine recCol CancellationToken.None |> within
                              Expect.isSome prepRecOpt "prepare on countdown succeeds"
                              let! incomingRecOpt = CallHierarchy.incoming scope libGraph prepRecOpt.Value[0] CancellationToken.None |> within
                              Expect.isSome incomingRecOpt "incomingCalls on countdown succeeds"
                              Expect.hasLength incomingRecOpt.Value 1 "countdown has 1 incoming caller (itself)"
                              Expect.equal incomingRecOpt.Value[0].From.Name "countdown" "Self caller is countdown"
                              Expect.hasLength incomingRecOpt.Value[0].FromRanges 1 "Only recursive body call is in FromRanges (definition excluded)"

                              // 5. Constructor incomingCalls: ConcreteProcessor() in runPipeline is included, whereas `(p: ConcreteProcessor)` type annotation in inspectProcessor is NOT!
                              let ctorCallLine, ctorCallCol = findPos appRunnerFs "ConcreteProcessor()" 0 3
                              let! prepCtorOpt = CallHierarchy.prepare scope.OwnersOf appGraph runnerPath ctorCallLine ctorCallCol CancellationToken.None |> within
                              Expect.isSome prepCtorOpt "prepare on ConcreteProcessor() call succeeds"
                              let! incomingCtorOpt = CallHierarchy.incoming scope libGraph prepCtorOpt.Value[0] CancellationToken.None |> within
                              Expect.isSome incomingCtorOpt "incomingCalls on ConcreteProcessor ctor succeeds"
                              let ctorCallerNames = incomingCtorOpt.Value |> Array.map (fun c -> c.From.Name) |> Set.ofArray
                              Expect.contains ctorCallerNames "runPipeline" "runPipeline constructs ConcreteProcessor()"
                              Expect.isFalse (ctorCallerNames.Contains "inspectProcessor") "Type annotation `(p: ConcreteProcessor)` must NOT count as incoming constructor call"

                              // 6. outgoingCalls on App.Runner.runPipeline:
                              //    Includes combineValues (2 FromRanges), signedEntry (1), ConcreteProcessor ctor (1), Process (1), localHelper (1)
                              //    Excludes siblingOnlyHelper (called in sibling unrelatedCaller, outside runPipeline!) and runPipeline itself!
                              let pipeLine, pipeCol = findPos appRunnerFs "let runPipeline" 0 6
                              let! prepPipeOpt = CallHierarchy.prepare scope.OwnersOf appGraph runnerPath pipeLine pipeCol CancellationToken.None |> within
                              Expect.isSome prepPipeOpt "prepare on runPipeline succeeds"
                              let! outgoingPipeOpt = CallHierarchy.outgoing scope.OwnersOf appGraph prepPipeOpt.Value[0] CancellationToken.None |> within
                              Expect.isSome outgoingPipeOpt "outgoingCalls on runPipeline succeeds"
                              let outgoingPipe = outgoingPipeOpt.Value
                              let calleeNames = outgoingPipe |> Array.map (fun c -> c.To.Name) |> Set.ofArray
                              Expect.contains calleeNames "combineValues" "Calls combineValues"
                              Expect.contains calleeNames "signedEntry" "Calls signedEntry"
                              Expect.contains calleeNames "Process" "Calls ConcreteProcessor.Process"
                              Expect.contains calleeNames "localHelper" "Calls same-file localHelper"
                              Expect.isTrue
                                  (outgoingPipe |> Array.exists (fun c -> c.To.Kind = SymbolKind.Constructor))
                                  "Calls ConcreteProcessor constructor"
                              Expect.isFalse (calleeNames.Contains "siblingOnlyHelper") "Sibling function's calls MUST NOT leak into runPipeline outgoingCalls"
                              Expect.isFalse (calleeNames.Contains "runPipeline") "Caller header itself MUST NOT appear in outgoingCalls"

                              let outCombine = outgoingPipe |> Array.find (fun c -> c.To.Name = "combineValues")
                              Expect.hasLength outCombine.FromRanges 2 "Two calls to combineValues inside runPipeline grouped into 2 FromRanges"

                              // Type annotation `(p: ConcreteProcessor)` inside inspectProcessor MUST NOT emit constructor in outgoingCalls
                              let inspLine, inspCol = findPos appRunnerFs "let inspectProcessor" 0 6
                              let! prepInspOpt = CallHierarchy.prepare scope.OwnersOf appGraph runnerPath inspLine inspCol CancellationToken.None |> within
                              Expect.isSome prepInspOpt "prepare on inspectProcessor succeeds"
                              let! outInspOpt = CallHierarchy.outgoing scope.OwnersOf appGraph prepInspOpt.Value[0] CancellationToken.None |> within
                              Expect.isSome outInspOpt "outgoingCalls on inspectProcessor succeeds"
                              Expect.isFalse
                                  (outInspOpt.Value |> Array.exists (fun c -> c.To.Kind = SymbolKind.Constructor))
                                  "Type annotation `(p: ConcreteProcessor)` must NOT appear as Constructor in outgoingCalls"

                              // 7. Prepare on Signed.fsi `val signedEntry` -> outgoingCalls resolves body in paired Signed.fs -> finds combineValues!
                              let fsiValLine, fsiValCol = findPos libSignedFsi "val signedEntry" 0 5
                              let! prepFsiOpt = CallHierarchy.prepare scope.OwnersOf libGraph signedFsiPath fsiValLine fsiValCol CancellationToken.None |> within
                              Expect.isSome prepFsiOpt "prepare on .fsi val signedEntry succeeds"
                              Expect.isTrue (sameFile (Paths.ofUri prepFsiOpt.Value[0].Uri) signedFsiPath) "Prepared on .fsi points to .fsi"
                              let! outFromFsiOpt = CallHierarchy.outgoing scope.OwnersOf libGraph prepFsiOpt.Value[0] CancellationToken.None |> within
                              Expect.isSome outFromFsiOpt "outgoingCalls from .fsi item succeeds via paired .fs"
                              let fsiCalleeNames = outFromFsiOpt.Value |> Array.map (fun c -> c.To.Name)
                              Expect.equal fsiCalleeNames [| "combineValues" |] "signedEntry in Signed.fs calls combineValues"
                              Expect.isTrue
                                  (outFromFsiOpt.Value[0].FromRanges |> Array.forall (fun r -> sliceLspRange libSignedFs r = "combineValues"))
                                  "FromRanges in paired Signed.fs slice exact call site"
                              let _ = signedFsPath
                              ()
                          })
          }

          testTask "TypeHierarchy: prepare (decl, annotation, abbreviation, .fsi), cross-project supertypes (.fsi->.fs fallback), and cross-project subtypes" {
              do!
                  withTwoProjectWorkspace
                      [ "Types.fs", libTypesFs; "Signed.fsi", libSignedFsi; "Signed.fs", libSignedFs ]
                      [ "Runner.fs", appRunnerFs ]
                      (fun _ libGraph appGraph libPath appPath scope ->
                          task {
                              let typesPath = libPath "Types.fs"
                              let signedFsiPath = libPath "Signed.fsi"
                              let runnerPath = appPath "Runner.fs"

                              // 1. Non-type function returns None
                              let fnLine, fnCol = findPos libTypesFs "let combineValues" 0 6
                              let! nonType = TypeHierarchy.prepare scope.OwnersOf libGraph typesPath fnLine fnCol CancellationToken.None |> within
                              Expect.isNone nonType "Function must not prepare a TypeHierarchyItem"

                              // 2. Prepare on type abbreviation WorkerAlias unwraps to IWorker
                              let aliasLine, aliasCol = findPos libTypesFs "type WorkerAlias" 0 7
                              let! aliasPrep = TypeHierarchy.prepare scope.OwnersOf libGraph typesPath aliasLine aliasCol CancellationToken.None |> within
                              Expect.isSome aliasPrep "Type abbreviation unwraps to underlying type"
                              Expect.equal aliasPrep.Value[0].Name "IWorker" "WorkerAlias unwraps to IWorker"
                              Expect.equal aliasPrep.Value[0].Kind SymbolKind.Interface "IWorker is Interface"
                              assertPlainData "TypeHierarchyItem" aliasPrep.Value[0]

                              // 3. Prepare on interface reference `interface IWorker` in App/Runner.fs resolves IWorker in Lib/Types.fs
                              let refLine, refCol = findPos appRunnerFs "interface IWorker" 0 12
                              let! iWorkerPrep = TypeHierarchy.prepare scope.OwnersOf appGraph runnerPath refLine refCol CancellationToken.None |> within
                              Expect.isSome iWorkerPrep "prepare on interface reference in App resolves IWorker"
                              let iWorkerItem = iWorkerPrep.Value[0]
                              Expect.equal iWorkerItem.Name "IWorker" "Resolved IWorker"
                              Expect.isTrue (sameFile (Paths.ofUri iWorkerItem.Uri) typesPath) "Points to Lib/Types.fs"
                              Expect.isTrue (rangeContains iWorkerItem.Range iWorkerItem.SelectionRange) "Range encloses SelectionRange"
                              Expect.equal (sliceLspRange libTypesFs iWorkerItem.SelectionRange) "IWorker" "SelectionRange slices IWorker"

                              // 4. Cross-project subtypes of IWorker: ConcreteProcessor (Lib), SignedWorker (Lib), AppWorker (App)
                              let! iWorkerSubsOpt = TypeHierarchy.subtypes scope libGraph iWorkerItem CancellationToken.None |> within
                              Expect.isSome iWorkerSubsOpt "subtypes of IWorker succeeds"
                              let iWorkerSubNames = iWorkerSubsOpt.Value |> Array.map _.Name |> Set.ofArray
                              Expect.equal
                                  iWorkerSubNames
                                  (Set.ofList [ "ConcreteProcessor"; "SignedWorker"; "AppWorker" ])
                                  "Cross-project subtypes of IWorker include Lib and App implementers"

                              // 5. Cross-project supertypes of AppSpecialProcessor (in App): ConcreteProcessor (in Lib) + IAppTag (in App)
                              let appSpecLine, appSpecCol = findPos appRunnerFs "type AppSpecialProcessor" 0 8
                              let! appSpecPrep = TypeHierarchy.prepare scope.OwnersOf appGraph runnerPath appSpecLine appSpecCol CancellationToken.None |> within
                              Expect.isSome appSpecPrep "prepare on AppSpecialProcessor succeeds"
                              let! appSpecSupersOpt = TypeHierarchy.supertypes scope.OwnersOf appGraph appSpecPrep.Value[0] CancellationToken.None |> within
                              Expect.isSome appSpecSupersOpt "supertypes of AppSpecialProcessor succeeds"
                              let appSpecSuperNames = appSpecSupersOpt.Value |> Array.map _.Name |> Set.ofArray
                              Expect.equal
                                  appSpecSuperNames
                                  (Set.ofList [ "ConcreteProcessor"; "IAppTag" ])
                                  "Cross-project supertypes include ConcreteProcessor from Lib and IAppTag from App"

                              // Second hop supertypes on ConcreteProcessor -> BaseProcessor + IWorker
                              let concreteItem = appSpecSupersOpt.Value |> Array.find (fun t -> t.Name = "ConcreteProcessor")
                              let! concreteSupersOpt = TypeHierarchy.supertypes scope.OwnersOf libGraph concreteItem CancellationToken.None |> within
                              Expect.isSome concreteSupersOpt "second-hop supertypes on ConcreteProcessor succeeds"
                              let concreteSuperNames = concreteSupersOpt.Value |> Array.map _.Name |> Set.ofArray
                              Expect.equal concreteSuperNames (Set.ofList [ "BaseProcessor"; "IWorker" ]) "ConcreteProcessor supertypes are BaseProcessor and IWorker"

                              // Root interface supertypes excludes trivial System.Object -> empty
                              let! rootSupersOpt = TypeHierarchy.supertypes scope.OwnersOf libGraph iWorkerItem CancellationToken.None |> within
                              Expect.isSome rootSupersOpt "supertypes on root interface returns Some"
                              Expect.isEmpty rootSupersOpt.Value "IWorker has 0 workspace supertypes (System.Object excluded)"

                              // 6. Prepare on Signed.fsi `type SignedWorker` (where interface IWorker is hidden in Signed.fs!) -> supertypes still finds IWorker!
                              let fsiTypeLine, fsiTypeCol = findPos libSignedFsi "type SignedWorker" 0 7
                              let! fsiTypePrep = TypeHierarchy.prepare scope.OwnersOf libGraph signedFsiPath fsiTypeLine fsiTypeCol CancellationToken.None |> within
                              Expect.isSome fsiTypePrep "prepare on .fsi type SignedWorker succeeds"
                              let! fsiTypeSupers = TypeHierarchy.supertypes scope.OwnersOf libGraph fsiTypePrep.Value[0] CancellationToken.None |> within
                              Expect.isSome fsiTypeSupers "supertypes on .fsi SignedWorker succeeds"
                              Expect.equal (fsiTypeSupers.Value |> Array.map _.Name) [| "IWorker" |] ".fsi SignedWorker resolves hidden IWorker supertype from paired Signed.fs"
                          })
          } ]

let private copyDepsForLsp () : string * IDisposable =
    let fixturesRoot = Path.GetDirectoryName PublishTests.depsDir
    let repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)
    let root = Path.Combine(Path.GetTempPath(), "rfs-task5-lsp-" + Guid.NewGuid().ToString("N").Substring(0, 8))
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

let private runLspProtocolTest () : Task<unit> =
    task {
        let target, cleanup = copyDepsForLsp ()
        use _cleanup = cleanup
        let projPath = Path.Combine(target, "Deps.fsproj")
        let dPath = Path.Combine(target, "D.fs")
        let eSigPath = Path.Combine(target, "E.fsi")
        let ePath = Path.Combine(target, "E.fs")

        let! lsp, owner, serverTask = TestServer.startCapturing () |> within
        use _owner = owner

        // 1. Initialize & verify all 3 capabilities are advertised
        let initParams =
            JsonNode.Parse
                $"""{{"processId":null,"rootUri":%s{JsonValue.Create(Paths.toUri target).ToJsonString()},"initializationOptions":{{"automaticWorkspaceInit":false}},"capabilities":{{}}}}"""

        let! initReply, _ = lsp.Request("initialize", initParams) |> within
        do! lsp.Notify("initialized", JsonObject()) |> withinUnit
        let! server = serverTask |> within

        let caps = initReply["capabilities"]
        Expect.isNotNull caps["workspaceSymbolProvider"] "workspaceSymbolProvider advertised"
        Expect.equal (caps["workspaceSymbolProvider"].["resolveProvider"].GetValue<bool>()) true "workspaceSymbolProvider.resolveProvider = true"
        Expect.equal (caps["callHierarchyProvider"].GetValue<bool>()) true "callHierarchyProvider = true"
        Expect.equal (caps["typeHierarchyProvider"].GetValue<bool>()) true "typeHierarchyProvider = true"

        // 2. 0-project workspace/symbol over JSON-RPC returns [] cleanly
        let! zeroProjSyms, _ = lsp.Request("workspace/symbol", JsonNode.Parse """{"query":"anything"}""") |> withinFast
        Expect.isNotNull zeroProjSyms "workspace/symbol with 0 loaded projects must return array"
        Expect.equal (zeroProjSyms.AsArray().Count) 0 "workspace/symbol with 0 loaded projects returns []"

        // 3. Load Deps project and open D.fs, E.fsi, E.fs
        do! loadProject lsp projPath

        let hierarchySourceD =
            String.concat
                "\n"
                [ "module Deps.D"
                  ""
                  "type IAction ="
                  "    abstract Run: int -> int"
                  ""
                  "type BaseAction() ="
                  "    abstract Step: int -> int"
                  "    default _.Step(x: int) = x + 1"
                  ""
                  "type ConcreteAction() ="
                  "    inherit BaseAction()"
                  "    interface IAction with"
                  "        member _.Run(x: int) = x * 2"
                  ""
                  "let leafCalc (v: int) = v + 10"
                  ""
                  "let midStep (v: int) = leafCalc v + leafCalc (v + 1)"
                  ""
                  "let topEntry (v: int) ="
                  "    let act = ConcreteAction()"
                  "    midStep v + act.Step(v)" ]

        do! lsp.DidOpen(dPath, File.ReadAllText dPath) |> withinUnit
        do! lsp.DidOpen(eSigPath, File.ReadAllText eSigPath) |> withinUnit
        do! lsp.DidOpen(ePath, File.ReadAllText ePath) |> withinUnit

        let graph = server.Workspace.ProjectOf dPath |> Option.get
        let! _ = graph.WhenPromoted(dPath, CancellationToken.None) |> within

        do!
            lsp.DidChange(
                dPath,
                2,
                { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 50; Character = 0 } }
                  Text = hierarchySourceD }
            )
            |> withinUnit

        let! _ = graph.AllDiagnostics() |> within

        // 4. Multi-hop Call Hierarchy over JSON-RPC: topEntry -> midStep -> leafCalc & reverse
        let posJson (path: string) (line: int, col: int) =
            JsonNode.Parse
                $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri path).ToJsonString()}}},"position":{{"line":%d{line},"character":%d{col}}}}}"""

        let midLine, midCol = findPos hierarchySourceD "let midStep" 0 5
        let! prepMidReply, _ = lsp.Request("textDocument/prepareCallHierarchy", posJson dPath (midLine, midCol)) |> within
        Expect.isNotNull prepMidReply "prepareCallHierarchy succeeded"
        let midItemJson = prepMidReply.AsArray().[0]
        Expect.equal (midItemJson["name"].GetValue<string>()) "midStep" "Prepared midStep"

        let! inMidReply, _ =
            lsp.Request("callHierarchy/incomingCalls", JsonNode.Parse $"""{{"item":%s{midItemJson.ToJsonString()}}}""")
            |> within
        Expect.isNotNull inMidReply "callHierarchy/incomingCalls succeeded"
        let inMidArr = inMidReply.AsArray()
        Expect.equal inMidArr.Count 1 "topEntry calls midStep"
        Expect.equal (inMidArr[0].["from"].["name"].GetValue<string>()) "topEntry" "Incoming caller is topEntry"

        let! outMidReply, _ =
            lsp.Request("callHierarchy/outgoingCalls", JsonNode.Parse $"""{{"item":%s{midItemJson.ToJsonString()}}}""")
            |> within
        Expect.isNotNull outMidReply "callHierarchy/outgoingCalls succeeded"
        let outMidArr = outMidReply.AsArray()
        Expect.equal outMidArr.Count 1 "midStep calls leafCalc (grouped)"
        Expect.equal (outMidArr[0].["to"].["name"].GetValue<string>()) "leafCalc" "Outgoing callee is leafCalc"
        Expect.equal (outMidArr[0].["fromRanges"].AsArray().Count) 2 "Two calls to leafCalc grouped in fromRanges"

        // 5. Multi-hop Type Hierarchy over JSON-RPC: ConcreteAction -> supertypes (BaseAction, IAction) & IAction -> subtypes (ConcreteAction)
        let concLine, concCol = findPos hierarchySourceD "type ConcreteAction" 0 7
        let! prepTypeReply, _ = lsp.Request("textDocument/prepareTypeHierarchy", posJson dPath (concLine, concCol)) |> within
        Expect.isNotNull prepTypeReply "prepareTypeHierarchy succeeded"
        let concTypeJson = prepTypeReply.AsArray().[0]
        Expect.equal (concTypeJson["name"].GetValue<string>()) "ConcreteAction" "Prepared ConcreteAction"

        let! supersReply, _ =
            lsp.Request("typeHierarchy/supertypes", JsonNode.Parse $"""{{"item":%s{concTypeJson.ToJsonString()}}}""")
            |> within
        Expect.isNotNull supersReply "typeHierarchy/supertypes succeeded"
        let supersArr = supersReply.AsArray()
        let superNames = [| for s in supersArr -> s["name"].GetValue<string>() |] |> Set.ofArray
        Expect.equal superNames (Set.ofList [ "BaseAction"; "IAction" ]) "ConcreteAction supertypes over JSON-RPC"

        let iActionJson = supersArr |> Seq.find (fun s -> s["name"].GetValue<string>() = "IAction")
        let! subsReply, _ =
            lsp.Request("typeHierarchy/subtypes", JsonNode.Parse $"""{{"item":%s{iActionJson.ToJsonString()}}}""")
            |> within
        Expect.isNotNull subsReply "typeHierarchy/subtypes succeeded"
        let subNames = [| for s in subsReply.AsArray() -> s["name"].GetValue<string>() |]
        Expect.equal subNames [| "ConcreteAction" |] "IAction subtypes over JSON-RPC"

        // 6. NON-BLOCKING PROOF: Block typechecking on BOTH D.fs and E.fsi, edit both via didChange,
        //    prove semanticTokens/full is blocked while workspace/symbol answers immediately with new .fs and .fsi symbols!
        use gate = new ManualResetEventSlim(false)
        use fsCheckEntered = new ManualResetEventSlim(false)

        graph.OnFlight(fun kind _ ct ->
            if kind = "dependencies" || kind = "finish" then
                fsCheckEntered.Set()
                gate.Wait(ct))

        try
            let editedD = "module Deps.D\nopen System.Text\n" + hierarchySourceD.Substring(hierarchySourceD.IndexOf('\n') + 1) + "\n\nlet newlyAddedWorkspaceFn (z: int) = z + 99\n"
            let editedESig =
                String.concat
                    "\n"
                    [ "module Deps.E"
                      "open System.Text"
                      ""
                      "val user: int"
                      "val compute: unit -> int"
                      "val unrelated: int"
                      "val newlyAddedSigWorkspaceFn: int -> int" ]

            do!
                lsp.DidChange(
                    dPath,
                    3,
                    { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 60; Character = 0 } }
                      Text = editedD }
                )
                |> withinUnit

            do!
                lsp.DidChange(
                    eSigPath,
                    2,
                    { Range = { Start = { Line = 0; Character = 0 }; End = { Line = 60; Character = 0 } }
                      Text = editedESig }
                )
                |> withinUnit

            let docOnlyJson (p: string) =
                JsonNode.Parse $"""{{"textDocument":{{"uri":%s{JsonValue.Create(Paths.toUri p).ToJsonString()}}}}}"""

            let blockedFsCheck = lsp.Request("textDocument/semanticTokens/full", docOnlyJson dPath)
            let blockedFsiCheck = lsp.Request("textDocument/semanticTokens/full", docOnlyJson eSigPath)

            Expect.isTrue (fsCheckEntered.Wait(TimeSpan.FromSeconds 10.)) "D.fs typecheck flight entered gate"
            do! Task.Delay 50
            Expect.isFalse blockedFsCheck.IsCompleted "D.fs semanticTokens/full MUST be blocked on typecheck"
            Expect.isFalse blockedFsiCheck.IsCompleted "E.fsi semanticTokens/full MUST be blocked on signature check"

            // Query workspace/symbol via camelCase subsequence while typecheck is BLOCKED!
            let! wsFsReply, _ = lsp.Request("workspace/symbol", JsonNode.Parse """{"query":"nAWF"}""") |> withinFast
            Expect.isNotNull wsFsReply "workspace/symbol must succeed immediately while typecheck is blocked"
            let wsFsNames = [| for s in wsFsReply.AsArray() -> s["name"].GetValue<string>() |]
            Expect.contains wsFsNames "newlyAddedWorkspaceFn" "workspace/symbol immediately returns newly edited .fs symbol via camelCase 'nAWF'"

            let! wsFsiReply, _ = lsp.Request("workspace/symbol", JsonNode.Parse """{"query":"nASWF"}""") |> withinFast
            Expect.isNotNull wsFsiReply "workspace/symbol on .fsi must succeed immediately while check is blocked"
            let wsFsiNames = [| for s in wsFsiReply.AsArray() -> s["name"].GetValue<string>() |]
            Expect.contains wsFsiNames "newlyAddedSigWorkspaceFn" "workspace/symbol immediately returns newly edited .fsi symbol via camelCase 'nASWF'"

            // Also verify workspaceSymbol/resolve while check is STILL blocked!
            let firstWsSym = wsFsReply.AsArray().[0]
            let! resolvedWsSym, _ =
                lsp.Request("workspaceSymbol/resolve", JsonNode.Parse(firstWsSym.ToJsonString()))
                |> withinFast
            Expect.isNotNull resolvedWsSym "workspaceSymbol/resolve succeeds while typecheck is blocked"
            Expect.equal (resolvedWsSym["name"].GetValue<string>()) (firstWsSym["name"].GetValue<string>()) "workspaceSymbol/resolve preserves symbol"

            // Confirm typechecks remained blocked the entire time!
            Expect.isFalse blockedFsCheck.IsCompleted "D.fs typecheck remained blocked while workspace/symbol answered"
            Expect.isFalse blockedFsiCheck.IsCompleted "E.fsi signature check remained blocked while workspace/symbol answered"

            gate.Set()
            let! _ = blockedFsCheck |> within
            let! _ = blockedFsiCheck |> within
            ()
        finally
            gate.Set()
            graph.OnFlight(fun _ _ _ -> ())
    }

[<Tests>]
let lspProtocolTests =
    testSequenced
    <| testList
        "HierarchyAndWorkspaceSymbols.LspProtocol"
        [ testTask "live JSON-RPC capabilities, 0-project workspace/symbol, non-blocking workspace/symbol under blocked typecheck (.fs & .fsi), and multi-hop callHierarchy & typeHierarchy" {
              do! runLspProtocolTest ()
          } ]
