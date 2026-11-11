module Rfs.Server.Tests.RenameAndIonideTests

open System
open System.Collections.Concurrent
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

let private disposing (d: IDisposable) =
    { new IAsyncDisposable with
        member _.DisposeAsync() =
            d.Dispose()
            ValueTask() }

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

let private applyLspEdits (source: string) (edits: TextEdit[]) : string =
    let sorted =
        edits
        |> Array.sortByDescending (fun e -> int e.Range.Start.Line, int e.Range.Start.Character, int e.Range.End.Line, int e.Range.End.Character)

    let mutable current = source

    for e in sorted do
        let edit: Rfs.Lsp.Edit =
            { Range =
                { Start = { Line = int e.Range.Start.Line; Character = int e.Range.Start.Character }
                  End = { Line = int e.Range.End.Line; Character = int e.Range.End.Character } }
              Text = e.NewText }

        current <- Rfs.Lsp.Document.apply current edit

    current

let private editsByPath (wsEdit: WorkspaceEdit) : Map<string, TextEdit[]> =
    let fromChanges =
        match wsEdit.Changes with
        | Some m ->
            m
            |> Seq.map (fun kv -> LspClient.NormalizePath(Paths.ofUri kv.Key), kv.Value)
            |> Map.ofSeq
        | None -> Map.empty

    if not fromChanges.IsEmpty then
        fromChanges
    else
        match wsEdit.DocumentChanges with
        | Some docChanges ->
            docChanges
            |> Seq.choose (function
                | U4.C1 tde ->
                    let edits =
                        tde.Edits
                        |> Array.map (function
                            | U2.C1 te -> te
                            | U2.C2 ate -> { Range = ate.Range; NewText = ate.NewText })

                    Some(LspClient.NormalizePath(Paths.ofUri tde.TextDocument.Uri), edits)
                | _ -> None)
            |> Map.ofSeq
        | None -> Map.empty

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
    (body: GraphHost -> ProjectGraph -> ProjectGraph -> (string -> string) -> (string -> string) -> References.Scope -> (string -> ProjectGraph list) -> Task<unit>)
    : Task<unit> =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-task6-"

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

            use host = GraphHost.Start "task6"
            let! libGraph = ProjectGraph.Create(host, libSpec)
            let! appGraph = ProjectGraph.Create(host, appSpec, [ libGraph ])

            let! _ = libGraph.AllDiagnostics() |> within
            let! _ = appGraph.AllDiagnostics() |> within

            let! graphThreadId = host.Read(fun () -> Thread.CurrentThread.ManagedThreadId) |> within

            let assertGraphThread (memberName: string) =
                if Thread.CurrentThread.ManagedThreadId <> graphThreadId then
                    failtest
                        $"References.Scope.%s{memberName} called off GraphHost thread (actual=%d{Thread.CurrentThread.ManagedThreadId}, expected=%d{graphThreadId})"

            let ownersAnyThread (f: string) =
                let full = Path.GetFullPath f

                if libPaths |> List.exists (sameFile full) then [ libGraph ]
                elif appPaths |> List.exists (sameFile full) then [ appGraph ]
                else []

            let ownersGraphThread (f: string) =
                assertGraphThread "OwnersOf"
                ownersAnyThread f

            let scope: References.Scope =
                { OwnersOf = ownersGraphThread
                  ReferencingProjects =
                    fun g ->
                        assertGraphThread "ReferencingProjects"
                        if obj.ReferenceEquals(g, libGraph) then [ appGraph ] else []
                  Projects =
                    fun () ->
                        assertGraphThread "Projects"
                        [ libGraph; appGraph ]
                  FilesOf =
                    fun g ->
                        assertGraphThread "FilesOf"
                        if obj.ReferenceEquals(g, libGraph) then libPaths else appPaths
                  IsLoading =
                    fun () ->
                        assertGraphThread "IsLoading"
                        false }

            do! body host libGraph appGraph libPath appPath scope ownersAnyThread
            appGraph.Dispose()
            libGraph.Dispose()
        finally
            try
                dir.Delete true
            with _ ->
                ()
    }

[<Tests>]
let renameTests =
    testList
        "Task6.Rename"
        [ testTask "prepare and cross-project + .fsi rename strips module qualifiers and updates all sites in one turn" {
              let libFsi =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "module Lib ="
                        "    val computeValue: x: int -> int"
                        "    val ``my value``: int" ]

              let libFs =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "module Lib ="
                        "    let computeValue (x: int) = x + 1"
                        "    let ``my value`` = computeValue 10" ]

              let appFs =
                  String.concat
                      "\n"
                      [ "namespace App"
                        "module Consumer ="
                        "    let run () ="
                        "        let a = Demo.Lib.computeValue 5"
                        "        let b = Demo.Lib.``my value``"
                        "        a + b" ]

              do!
                  withTwoProjectWorkspace
                      [ "Lib.fsi", libFsi; "Lib.fs", libFs ]
                      [ "Consumer.fs", appFs ]
                      (fun _ libGraph appGraph libPath appPath scope _ ->
                          task {
                              let ct = CancellationToken.None
                              let consumerFile = appPath "Consumer.fs"
                              let libFsFile = libPath "Lib.fs"
                              let libFsiFile = libPath "Lib.fsi"

                              // Prepare on qualified call `Demo.Lib.computeValue` — range must cover ONLY `computeValue`!
                              let callLine, callCol = findPos appFs "Demo.Lib.computeValue" 0 12
                              let! prep = Rename.prepare scope.OwnersOf appGraph consumerFile callLine callCol ct |> within
                              Expect.isSome prep "prepareRename should succeed on workspace symbol"

                              match prep.Value with
                              | U3.C2 pr ->
                                  Expect.equal pr.Placeholder "computeValue" "placeholder should be clean display name"
                                  Expect.equal (sliceLspRange appFs pr.Range) "computeValue" "prepare range must strip Demo.Lib. qualifier"
                              | other -> failtestf "Expected RangeWithPlaceholder (U3.C2), got %A" other

                              // Also prepare from .fsi signature declaration!
                              let fsiLine, fsiCol = findPos libFsi "computeValue" 0 3
                              let! prepFsi = Rename.prepare scope.OwnersOf libGraph libFsiFile fsiLine fsiCol ct |> within
                              Expect.isSome prepFsi "prepareRename should succeed on .fsi declaration"

                              // Rename `computeValue` -> `calculateTotal` across Lib.fsi, Lib.fs, and Consumer.fs
                              let! renamed = Rename.rename scope appGraph consumerFile callLine callCol "calculateTotal" ct |> within
                              match renamed with
                              | Error e -> failtest $"Expected Ok rename, got Error: %s{e}"
                              | Ok None -> failtest "Expected Some WorkspaceEdit"
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let normFsi = LspClient.NormalizePath libFsiFile
                                  let normFs = LspClient.NormalizePath libFsFile
                                  let normApp = LspClient.NormalizePath consumerFile

                                  Expect.isTrue (edits.ContainsKey normFsi) "Lib.fsi should be edited"
                                  Expect.isTrue (edits.ContainsKey normFs) "Lib.fs should be edited"
                                  Expect.isTrue (edits.ContainsKey normApp) "Consumer.fs should be edited"

                                  let newFsi = applyLspEdits libFsi edits[normFsi]
                                  let newFs = applyLspEdits libFs edits[normFs]
                                  let newApp = applyLspEdits appFs edits[normApp]

                                  Expect.stringContains newFsi "val calculateTotal: x: int -> int" "Lib.fsi updated"
                                  Expect.stringContains newFs "let calculateTotal (x: int) = x + 1" "Lib.fs def updated"
                                  Expect.stringContains newFs "let ``my value`` = calculateTotal 10" "Lib.fs internal call updated"
                                  Expect.stringContains newApp "let a = Demo.Lib.calculateTotal 5" "Consumer.fs preserves Demo.Lib. qualifier"

                              // Rename qualified backticked identifier `Demo.Lib.``my value``` -> `baseAmount`
                              let btLine, btCol = findPos appFs "Demo.Lib.``my value``" 0 12
                              let! renamedBt = Rename.rename scope appGraph consumerFile btLine btCol "baseAmount" ct |> within
                              match renamedBt with
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newApp = applyLspEdits appFs edits[LspClient.NormalizePath consumerFile]
                                  let newFsi = applyLspEdits libFsi edits[LspClient.NormalizePath libFsiFile]
                                  Expect.stringContains newApp "let b = Demo.Lib.baseAmount" "backticked qualified site preserves Demo.Lib."
                                  Expect.stringContains newFsi "val baseAmount: int" "backticked .fsi site updated to bare identifier"
                              | other -> failtestf "Expected Ok(Some _) for backticked rename, got %A" other
                          })
          }

          testTask "rename auto-backticks F# keywords and spaced names, strips redundant backticks, and rejects invalid names" {
              let libFs =
                  String.concat
                      "\n"
                      [ "module Sample"
                        "let itemCount = 42"
                        "let doubled = itemCount * 2" ]

              do!
                  withTwoProjectWorkspace
                      [ "Sample.fs", libFs ]
                      [ "App.fs", "module App\nlet v = Sample.itemCount" ]
                      (fun _ libGraph _ libPath appPath scope _ ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Sample.fs"
                              let appFile = appPath "App.fs"
                              let line, col = findPos libFs "itemCount" 0 2

                              // Keyword -> auto-wrapped in double backticks
                              let! kwRename = Rename.rename scope libGraph file line col "match" ct |> within
                              match kwRename with
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newSample = applyLspEdits libFs edits[LspClient.NormalizePath file]
                                  let newApp = applyLspEdits "module App\nlet v = Sample.itemCount" edits[LspClient.NormalizePath appFile]
                                  Expect.stringContains newSample "let ``match`` = 42" "keyword wrapped in double backticks"
                                  Expect.stringContains newApp "let v = Sample.``match``" "qualified keyword wrapped in double backticks"
                              | other -> failtestf "Expected Ok(Some _) for keyword rename, got %A" other

                              // Name with spaces -> auto-wrapped in double backticks
                              let! spacedRename = Rename.rename scope libGraph file line col "my total count" ct |> within
                              match spacedRename with
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newSample = applyLspEdits libFs edits[LspClient.NormalizePath file]
                                  Expect.stringContains newSample "let ``my total count`` = 42" "spaced name wrapped in double backticks"
                              | other -> failtestf "Expected Ok(Some _) for spaced rename, got %A" other

                              // Redundant backticks around a valid identifier -> unwrapped
                              let! redundantBt = Rename.rename scope libGraph file line col "``cleanName``" ct |> within
                              match redundantBt with
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newSample = applyLspEdits libFs edits[LspClient.NormalizePath file]
                                  Expect.stringContains newSample "let cleanName = 42" "redundant backticks stripped"
                              | other -> failtestf "Expected Ok(Some _) for redundant-backtick rename, got %A" other

                              // Invalid names -> Error (including unbalanced/trailing single backtick `foo``)
                              for bad in [ ""; "   "; "foo\nbar"; "foo\r\nbar"; "``   ``"; "foo``bar"; "````"; "foo`" ] do
                                  let! res = Rename.rename scope libGraph file line col bad ct |> within
                                  Expect.isError res $"Expected Error for invalid newName '%s{bad}'"
                          })
          }

          testTask "rename supports workspace custom operators across Lib and App, validates newName per symbol kind, and still rejects external +" {
              let libFs =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "module Ops ="
                        "    let (|+|) (a: int) (b: int) = a + b"
                        "    let ( |*| ) (a: int) (b: int) = a * b"
                        "    let combine x y = (x |+| y) + (( |*| ) x y)" ]

              let appFs =
                  String.concat
                      "\n"
                      [ "namespace App"
                        "open Demo.Ops"
                        "module UseOps ="
                        "    let run () ="
                        "        let p = 3 |+| 4"
                        "        let q = Demo.Ops.(|+|) 5 6"
                        "        p + q" ]

              do!
                  withTwoProjectWorkspace
                      [ "Ops.fs", libFs ]
                      [ "UseOps.fs", appFs ]
                      (fun _ libGraph appGraph libPath appPath scope _ ->
                          task {
                              let ct = CancellationToken.None
                              let opsFile = libPath "Ops.fs"
                              let useFile = appPath "UseOps.fs"

                              // Prepare on parenthesized definition `let (|+|)` covers only inner `|+|`
                              let defLine, defCol = findPos libFs "(|+|)" 0 2
                              let! prepDef = Rename.prepare scope.OwnersOf libGraph opsFile defLine defCol ct |> within
                              Expect.isSome prepDef "prepare should succeed on parenthesized custom operator definition"

                              match prepDef.Value with
                              | U3.C2 pr ->
                                  Expect.equal pr.Placeholder "|+|" "operator placeholder is inner operator name"
                                  Expect.equal (sliceLspRange libFs pr.Range) "|+|" "prepare range covers inner operator without parens"
                              | other -> failtestf "Expected U3.C2, got %A" other

                              // Prepare on infix use `3 |+| 4` and qualified `Demo.Ops.(|+|)`
                              let infixLine, infixCol = findPos appFs "3 |+| 4" 0 3
                              let! prepInfix = Rename.prepare scope.OwnersOf appGraph useFile infixLine infixCol ct |> within
                              Expect.isSome prepInfix "prepare should succeed on infix custom operator"

                              let qualLine, qualCol = findPos appFs "Demo.Ops.(|+|)" 0 11
                              let! prepQual = Rename.prepare scope.OwnersOf appGraph useFile qualLine qualCol ct |> within
                              Expect.isSome prepQual "prepare should succeed on module-qualified parenthesized operator"

                              match prepQual.Value with
                              | U3.C2 pr ->
                                  Expect.equal (sliceLspRange appFs pr.Range) "|+|" "qualified operator range strips Demo.Ops. and parens"
                              | other -> failtestf "Expected U3.C2, got %A" other

                              // Cross-project rename `|+|` -> `|++|` from infix call site in App
                              let! renamedOp = Rename.rename scope appGraph useFile infixLine infixCol "|++|" ct |> within

                              match renamedOp with
                              | Error e -> failtest $"Expected custom operator rename to succeed, got Error: %s{e}"
                              | Ok None -> failtest "Expected Some WorkspaceEdit for custom operator rename"
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newLib = applyLspEdits libFs edits[LspClient.NormalizePath opsFile]
                                  let newApp = applyLspEdits appFs edits[LspClient.NormalizePath useFile]
                                  Expect.stringContains newLib "let (|++|) (a: int) (b: int) = a + b" "parenthesized def updated without touching +"
                                  Expect.stringContains newLib "let combine x y = (x |++| y) + (( |*| ) x y)" "infix use in Lib updated"
                                  Expect.stringContains newApp "let p = 3 |++| 4" "infix use in App updated"
                                  Expect.stringContains newApp "let q = Demo.Ops.(|++|) 5 6" "qualified parenthesized use in App updated"

                              // Spaced parenthesized operator `( |*| )` -> `|**|`
                              let spacedOpLine, spacedOpCol = findPos libFs "( |*| )" 0 3
                              let! renamedSpacedOp = Rename.rename scope libGraph opsFile spacedOpLine spacedOpCol "|**|" ct |> within

                              match renamedSpacedOp with
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newLib = applyLspEdits libFs edits[LspClient.NormalizePath opsFile]
                                  Expect.stringContains newLib "let ( |**| ) (a: int) (b: int) = a * b" "spaced parenthesized def preserves spaces"
                                  Expect.stringContains newLib "(( |**| ) x y)" "spaced parenthesized call updated"
                              | other -> failtestf "Expected Ok(Some _) for spaced operator rename, got %A" other

                              // Reject non-operator newName `"notAnOp"` on operator symbol
                              let! badOpName = Rename.rename scope libGraph opsFile defLine defCol "notAnOp" ct |> within
                              Expect.isError badOpName "Non-operator name 'notAnOp' must be rejected when renaming an operator"

                              // Reject external `+` in FSharp.Core even with a valid operator newName
                              let plusLine, plusCol = findPos libFs "a + b" 0 2
                              let! prepPlus = Rename.prepare scope.OwnersOf libGraph opsFile plusLine plusCol ct |> within
                              Expect.isNone prepPlus "External + must still be rejected by prepare"
                              let! renPlus = Rename.rename scope libGraph opsFile plusLine plusCol "|++|" ct |> within
                              Expect.isError renPlus "External + must still be rejected by rename"
                          })
          }

          testTask "rename and CodeLens unify mutable property getter/setter across Lib and App, skip get/set keywords, handle Attribute shorthand, and capture Version on GraphHost thread" {
              let libFs =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "type TagAttribute() ="
                        "    inherit System.Attribute()"
                        ""
                        "[<Tag>]"
                        "type Counter() ="
                        "    let mutable count = 0"
                        "    member _.Prop"
                        "        with get () = count"
                        "        and set (v: int) = count <- v"
                        "    member _.LocalCalc (x: int) ="
                        "        let tempVal = x + 1"
                        "        tempVal * 2" ]

              let appFs =
                  String.concat
                      "\n"
                      [ "namespace App"
                        "open Demo"
                        "[<TagAttribute>]"
                        "module UseCounter ="
                        "    let run () ="
                        "        let c = Counter()"
                        "        c.Prop <- 42"
                        "        c.Prop + 1" ]

              do!
                  withTwoProjectWorkspace
                      [ "Counter.fs", libFs ]
                      [ "UseCounter.fs", appFs ]
                      (fun host libGraph appGraph libPath appPath scope ownersAnyThread ->
                          task {
                              let ct = CancellationToken.None
                              let libFile = libPath "Counter.fs"
                              let appFile = appPath "UseCounter.fs"
                              let! graphThreadId = host.Read(fun () -> Thread.CurrentThread.ManagedThreadId) |> within

                              // 1. CodeLens.resolve on mutable `Prop` must count BOTH getter read and setter write (`2 References`)!
                              let! lensesOpt = CodeLens.lenses FSharpConfig.Default libGraph libFile ct |> within
                              Expect.isSome lensesOpt "CodeLens.lenses should succeed"
                              let propLine, _ = findPos libFs "member _.Prop" 0 10
                              let propLenses = lensesOpt.Value |> Array.filter (fun l -> int l.Range.Start.Line = propLine)
                              Expect.equal propLenses.Length 2 "Prop should have signature + references lenses"

                              let! resolvedPropLenses =
                                  Task.WhenAll(propLenses |> Array.map (fun l -> CodeLens.resolve scope ownersAnyThread l ct))
                                  |> within

                              let propTitles = resolvedPropLenses |> Array.choose (fun l -> l.Command |> Option.map _.Title)
                              Expect.contains propTitles "2 References" $"CodeLens must unify plain and |setter uses (got %A{propTitles})"

                              // 2. Rename `Prop` -> `Amount` initiated from the SETTER write `c.Prop <- 42` in App,
                              //    capturing OptionalVersionedTextDocumentIdentifier.Version strictly on the GraphHost thread!
                              let setterLine, setterCol = findPos appFs "c.Prop <- 42" 0 3
                              let mutable versionCheckedOnGraphThread = 0

                              let versionNow (p: string) =
                                  if Thread.CurrentThread.ManagedThreadId <> graphThreadId then
                                      failtest "versionNow called off GraphHost thread!"

                                  Interlocked.Increment(&versionCheckedOnGraphThread) |> ignore
                                  if sameFile p libFile then Some 5 elif sameFile p appFile then Some 9 else None

                              let! renamedProp =
                                  Rename.renameWithCapabilities true versionNow scope appGraph appFile setterLine setterCol "Amount" ct
                                  |> within

                              Expect.isGreaterThan versionCheckedOnGraphThread 0 "versionNow must execute on GraphHost thread"

                              match renamedProp with
                              | Error e -> failtest $"Expected mutable property rename to succeed without tripping on get/set keywords, got: %s{e}"
                              | Ok None -> failtest "Expected WorkspaceEdit for mutable property rename"
                              | Ok(Some wsEdit) ->
                                  Expect.isSome wsEdit.DocumentChanges "canDocumentChanges=true emits DocumentChanges"

                                  let versionsByPath =
                                      wsEdit.DocumentChanges.Value
                                      |> Array.choose (function
                                          | U4.C1 tde -> Some(LspClient.NormalizePath(Paths.ofUri tde.TextDocument.Uri), tde.TextDocument.Version)
                                          | _ -> None)
                                      |> Map.ofArray

                                  Expect.equal versionsByPath[LspClient.NormalizePath libFile] (Some 5) "Lib version captured in-turn"
                                  Expect.equal versionsByPath[LspClient.NormalizePath appFile] (Some 9) "App version captured in-turn"

                                  let edits = editsByPath wsEdit
                                  let newLib = applyLspEdits libFs edits[LspClient.NormalizePath libFile]
                                  let newApp = applyLspEdits appFs edits[LspClient.NormalizePath appFile]
                                  Expect.stringContains newLib "member _.Amount" "Property declaration renamed"
                                  Expect.stringContains newLib "with get () = count" "Explicit get accessor keyword preserved"
                                  Expect.stringContains newLib "and set (v: int) = count <- v" "Explicit set accessor keyword preserved"
                                  Expect.stringContains newApp "c.Amount <- 42" "Setter write site renamed"
                                  Expect.stringContains newApp "c.Amount + 1" "Getter read site renamed"

                              // 3. LocalTarget rename also captures Version on the GraphHost thread!
                              let localLine, localCol = findPos libFs "let tempVal =" 0 6
                              let mutable localVerChecked = 0

                              let localVersionNow (_: string) =
                                  if Thread.CurrentThread.ManagedThreadId <> graphThreadId then
                                      failtest "LocalTarget versionNow called off GraphHost thread!"

                                  Interlocked.Increment(&localVerChecked) |> ignore
                                  Some 13

                              let! renamedLocal =
                                  Rename.renameWithCapabilities true localVersionNow scope libGraph libFile localLine localCol "stepVal" ct
                                  |> within

                              Expect.isGreaterThan localVerChecked 0 "LocalTarget versionNow ran on GraphHost thread"

                              match renamedLocal with
                              | Ok(Some wsEdit) ->
                                  match wsEdit.DocumentChanges.Value[0] with
                                  | U4.C1 tde -> Expect.equal tde.TextDocument.Version (Some 13) "LocalTarget stamped in-turn LspVersion"
                                  | other -> failtestf "Expected U4.C1, got %A" other
                              | other -> failtestf "Expected LocalTarget rename Ok(Some _), got %A" other

                              // 4. Attribute rename `TagAttribute` -> `MarkerAttribute` handles `[<Tag>]` shorthand AND `[<TagAttribute>]`
                              let attrLine, attrCol = findPos libFs "type TagAttribute" 0 7
                              let! renamedAttr = Rename.rename scope libGraph libFile attrLine attrCol "MarkerAttribute" ct |> within

                              match renamedAttr with
                              | Error e -> failtest $"Expected Attribute rename to handle shorthand [<Tag>], got Error: %s{e}"
                              | Ok None -> failtest "Expected WorkspaceEdit for Attribute rename"
                              | Ok(Some wsEdit) ->
                                  let edits = editsByPath wsEdit
                                  let newLib = applyLspEdits libFs edits[LspClient.NormalizePath libFile]
                                  let newApp = applyLspEdits appFs edits[LspClient.NormalizePath appFile]
                                  Expect.stringContains newLib "type MarkerAttribute() =" "Attribute type declaration renamed"
                                  Expect.stringContains newLib "[<Marker>]" "Shorthand [<Tag>] updated to [<Marker>]"
                                  Expect.stringContains newApp "[<MarkerAttribute>]" "Full [<TagAttribute>] updated to [<MarkerAttribute>]"
                          })
          }

          testTask "prepare and rename refuse external BCL/FSharp.Core symbols, operators, constructors, and active patterns" {
              let src =
                  String.concat
                      "\n"
                      [ "module Guards"
                        "type Boxed(x: int) ="
                        "    member _.Value = x"
                        "let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd"
                        "let check xs ="
                        "    let mapped = List.map (fun v -> v + 1) xs"
                        "    let s = System.String.Concat(\"a\", \"b\")"
                        "    let inst = new Boxed(1)"
                        "    match mapped.Length with"
                        "    | Even -> s.Length + inst.Value"
                        "    | Odd -> 0" ]

              do!
                  withTwoProjectWorkspace
                      [ "Guards.fs", src ]
                      [ "App.fs", "module App\nlet z = 1" ]
                      (fun _ libGraph _ libPath _ scope _ ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Guards.fs"

                              // External FSharp.Core List.map
                              let mapLine, mapCol = findPos src "List.map" 0 6
                              let! prepMap = Rename.prepare scope.OwnersOf libGraph file mapLine mapCol ct |> within
                              Expect.isNone prepMap "prepare must refuse external FSharp.Core List.map"
                              let! renMap = Rename.rename scope libGraph file mapLine mapCol "myMap" ct |> within
                              Expect.isError renMap "rename must refuse external FSharp.Core List.map"

                              // External BCL System.String.Concat
                              let bclLine, bclCol = findPos src "System.String.Concat" 0 16
                              let! prepBcl = Rename.prepare scope.OwnersOf libGraph file bclLine bclCol ct |> within
                              Expect.isNone prepBcl "prepare must refuse external System.String.Concat"
                              let! renBcl = Rename.rename scope libGraph file bclLine bclCol "myConcat" ct |> within
                              Expect.isError renBcl "rename must refuse external System.String.Concat"

                              // Operator `+`
                              let opLine, opCol = findPos src "v + 1" 0 2
                              let! prepOp = Rename.prepare scope.OwnersOf libGraph file opLine opCol ct |> within
                              Expect.isNone prepOp "prepare must refuse operator +"

                              // Active pattern case `Even`
                              let apLine, apCol = findPos src "| Even ->" 0 3
                              let! prepAp = Rename.prepare scope.OwnersOf libGraph file apLine apCol ct |> within
                              Expect.isNone prepAp "prepare must refuse active pattern case"
                              let! renAp = Rename.rename scope libGraph file apLine apCol "IsEven" ct |> within
                              Expect.isError renAp "rename must refuse active pattern case"

                              // Constructor `new` keyword
                              let newLine, newCol = findPos src "new Boxed" 0 1
                              let! prepNew = Rename.prepare scope.OwnersOf libGraph file newLine newCol ct |> within
                              Expect.isNone prepNew "prepare must refuse `new` constructor keyword"

                              // Local variable `mapped` IS renameable within the file!
                              let locLine, locCol = findPos src "let mapped =" 0 5
                              let! prepLoc = Rename.prepare scope.OwnersOf libGraph file locLine locCol ct |> within
                              Expect.isSome prepLoc "prepare should allow local variable"
                              let! renLoc = Rename.rename scope libGraph file locLine locCol "items" ct |> within
                              match renLoc with
                              | Ok(Some wsEdit) ->
                                  let locEdits = editsByPath wsEdit
                                  let newSrc = applyLspEdits src locEdits[LspClient.NormalizePath file]
                                  Expect.stringContains newSrc "let items = List.map" "local def renamed"
                                  Expect.stringContains newSrc "match items.Length with" "local use renamed"
                              | other -> failtestf "Expected local rename to succeed, got %A" other
                          })
          }

          testTask "promoted file rename & inlay hints use CheckResultsAt (AdHocChecks) when an earlier declaration changed" {
              let initial =
                  String.concat
                      "\n"
                      [ "module Promoted"
                        "let seed = 10"
                        "let compute (x: float) ="
                        "    let localVal = x + 1.5"
                        "    localVal" ]

              do!
                  withTwoProjectWorkspace
                      [ "Promoted.fs", initial ]
                      [ "App.fs", "module App\nlet r = Promoted.compute 1.0" ]
                      (fun host libGraph _ libPath _ scope _ ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Promoted.fs"

                              libGraph.Promote file
                              let! mode = libGraph.WhenPromoted(file, ct) |> within
                              Expect.equal mode FileMode.Declarations "Promoted.fs should settle in Declarations mode"

                              // Edit only `seed` from int to string while `compute` is UNTOUCHED and independent of `seed`:
                              // `compute`'s declaration row is KEPT, while an earlier declaration's result changed,
                              // so `view.CheckResultsAt` must run an ad-hoc check (`AdHocChecks > beforeAdHoc`)!
                              let updated =
                                  String.concat
                                      "\n"
                                      [ "module Promoted"
                                        "let seed = \"changed\""
                                        "let compute (x: float) ="
                                        "    let localVal = x + 1.5"
                                        "    localVal" ]

                              let! _ = host.Read(fun () -> libGraph.SetText(file, updated)) |> within
                              let! _ = libGraph.Diagnostics file |> within

                              let beforeAdHoc = libGraph.AdHocChecks
                              let fullRange: Ionide.LanguageServerProtocol.Types.Range =
                                  { Start = { Line = 0u; Character = 0u }
                                    End = { Line = 10u; Character = 0u } }

                              let! hints = InlayHints.provide FSharpConfig.Default.InlayHints libGraph file fullRange ct |> within
                              Expect.isSome hints "InlayHints should return hints on promoted file"
                              Expect.isGreaterThan libGraph.AdHocChecks beforeAdHoc "CheckResultsAt must trigger AdHocChecks on stale kept row"

                              let floatHints =
                                  hints.Value
                                  |> Array.choose (fun h ->
                                      match h.Label with
                                      | U2.C1 s when s.Contains "float" -> Some s
                                      | _ -> None)

                              Expect.isNonEmpty floatHints "InlayHints must resolve `float` type on kept promoted declaration"

                              let locLine, locCol = findPos updated "localVal" 0 2
                              let! prep = Rename.prepare scope.OwnersOf libGraph file locLine locCol ct |> within
                              Expect.isSome prep "Rename.prepare works on promoted file"
                          })
          } ]

[<Tests>]
let inlayCodeLensInlineValueTests =
    testList
        "Task6.InlayCodeLensInlineValue"
        [ testTask "InlayHints provides type annotations with valid TextEdits, parameter annotations on return-typed functions, parameter names, suppresses x = x, and respects range & config" {
              let src =
                  String.concat
                      "\n"
                      [ "module Hints"
                        "let add (amount: int) (tax: int) = amount + tax"
                        "let addExplicit x y : int = x + y"
                        "let valExplicit : int = 42"
                        "let compute () ="
                        "    let total = add 100 20"
                        "    let amount = 50"
                        "    let sameName = add amount 5"
                        "    let mapped = [ 1; 2 ] |> List.map (fun item -> item + 1)"
                        "    total + sameName + mapped.Length + addExplicit 1 2 + valExplicit" ]

              do!
                  withTwoProjectWorkspace
                      [ "Hints.fs", src ]
                      [ "App.fs", "module App\nlet v = Hints.compute ()" ]
                      (fun _ libGraph _ libPath _ _ _ ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Hints.fs"
                              let allRange: Ionide.LanguageServerProtocol.Types.Range =
                                  { Start = { Line = 0u; Character = 0u }
                                    End = { Line = 20u; Character = 0u } }

                              let! hintsOpt = InlayHints.provide FSharpConfig.Default.InlayHints libGraph file allRange ct |> within
                              Expect.isSome hintsOpt "InlayHints should return Some"
                              let hints = hintsOpt.Value

                              let labels =
                                  hints
                                  |> Array.map (fun h ->
                                      match h.Label with
                                      | U2.C1 s -> s
                                      | U2.C2 parts -> parts |> Array.map _.Value |> String.concat "")

                              // Type annotations present for `total` (`: int`) and lambda `item` (`: int`)
                              Expect.isTrue (labels |> Array.exists (fun s -> s = ": int")) $"Expected ': int' type hint in %A{labels}"

                              // Function with explicit return type `let addExplicit x y : int = x + y` MUST still emit parameter type hints for `x` and `y`!
                              let explicitFnLine, _ = findPos src "let addExplicit" 0 0
                              let explicitFnTypeHints =
                                  hints
                                  |> Array.filter (fun h -> h.Kind = Some InlayHintKind.Type && int h.Position.Line = explicitFnLine)
                              Expect.equal explicitFnTypeHints.Length 2 "Both unannotated parameters `x` and `y` on `let addExplicit x y : int` must receive `: int` hints"

                              // Value binding with explicit return type `let valExplicit : int = 42` MUST NOT emit a duplicate value type hint!
                              let explicitValLine, _ = findPos src "let valExplicit" 0 0
                              let explicitValTypeHints =
                                  hints
                                  |> Array.filter (fun h -> h.Kind = Some InlayHintKind.Type && int h.Position.Line = explicitValLine)
                              Expect.isEmpty explicitValTypeHints "Explicitly typed value `let valExplicit : int = 42` must not emit a duplicate type hint"

                              // Bare lambda param `item` TextEdits should wrap `(item: int)` cleanly
                              let itemLine, _ = findPos src "fun item ->" 0 4
                              let itemHint =
                                  hints
                                  |> Array.tryFind (fun h -> h.Kind = Some InlayHintKind.Type && int h.Position.Line = itemLine && int h.Position.Character > 20)

                              Expect.isSome itemHint "Lambda parameter `item` should have a Type inlay hint"
                              Expect.isSome itemHint.Value.TextEdits "Type inlay hint must carry TextEdits"
                              let withLambdaType = applyLspEdits src itemHint.Value.TextEdits.Value
                              Expect.stringContains withLambdaType "fun (item: int) ->" "Bare lambda parameter TextEdits should wrap in parens"

                              // Parameter name hints on `add 100 20` -> `amount =` and `tax =`
                              let callLine, _ = findPos src "add 100 20" 0 0
                              let callLineParamLabels =
                                  hints
                                  |> Array.filter (fun h -> h.Kind = Some InlayHintKind.Parameter && int h.Position.Line = callLine)
                                  |> Array.choose (fun h -> match h.Label with U2.C1 s -> Some s | _ -> None)

                              Expect.contains callLineParamLabels "amount =" "First literal arg gets `amount =`"
                              Expect.contains callLineParamLabels "tax =" "Second literal arg gets `tax =`"

                              // Suppression: on `add amount 5`, `amount` matches param name `amount`, so ONLY `tax =` should appear!
                              let sameLine, _ = findPos src "add amount 5" 0 0
                              let sameLineParamLabels =
                                  hints
                                  |> Array.filter (fun h -> h.Kind = Some InlayHintKind.Parameter && int h.Position.Line = sameLine)
                                  |> Array.choose (fun h -> match h.Label with U2.C1 s -> Some s | _ -> None)

                              Expect.isFalse (sameLineParamLabels |> Array.contains "amount =") "Redundant `amount = amount` parameter hint must be suppressed"
                              Expect.contains sameLineParamLabels "tax =" "Non-matching second arg `5` still gets `tax =`"

                              // Viewport range filtering: requesting only line 3 returns hints ONLY on line 3
                              let narrowRange: Ionide.LanguageServerProtocol.Types.Range =
                                  { Start = { Line = uint32 callLine; Character = 0u }
                                    End = { Line = uint32 callLine; Character = 100u } }

                              let! narrow = InlayHints.provide FSharpConfig.Default.InlayHints libGraph file narrowRange ct |> within
                              Expect.isSome narrow "narrow range returns Some"
                              Expect.isNonEmpty narrow.Value "narrow range has line 3 hints"
                              Expect.isTrue (narrow.Value |> Array.forall (fun h -> int h.Position.Line = callLine)) "Every hint must be inside requested range"

                              // Config toggles
                              let! noneWhenBothOff =
                                  InlayHints.provide
                                      { TypeAnnotations = false; ParameterNames = false; DisableLongTooltip = true }
                                      libGraph
                                      file
                                      allRange
                                      ct
                                  |> within

                              Expect.equal noneWhenBothOff (Some [||]) "Disabling both TypeAnnotations and ParameterNames returns empty array"
                          })
          }

          testTask "CodeLens.lenses is non-blocking under held typecheck, respects replaceCodeLens, keeps plain JSON Data, and resolves signature + cross-project references" {
              let libFsi =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "module Math ="
                        "    val square: x: int -> int"
                        "    val unusedHelper: unit -> int" ]

              let libFs =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "module Math ="
                        "    let square (x: int) = x * x"
                        "    let unusedHelper () = 0" ]

              let appFs =
                  String.concat
                      "\n"
                      [ "namespace App"
                        "module Use ="
                        "    let a = Demo.Math.square 3"
                        "    let b = Demo.Math.square 4" ]

              do!
                  withTwoProjectWorkspace
                      [ "Math.fsi", libFsi; "Math.fs", libFs ]
                      [ "Use.fs", appFs ]
                      (fun host libGraph _ libPath _ scope ownersAnyThread ->
                          task {
                              let ct = CancellationToken.None
                              let mathFs = libPath "Math.fs"
                              let mathFsi = libPath "Math.fsi"

                              // replaceCodeLens config suppresses CodeLens
                              let replacedCfg =
                                  { FSharpConfig.Default with
                                      LineLens = { FSharpConfig.Default.LineLens with Enabled = "replaceCodeLens" } }

                              let! suppressed = CodeLens.lenses replacedCfg libGraph mathFs ct |> within
                              Expect.equal suppressed (Some [||]) "replaceCodeLens must return empty CodeLens[]"

                              // Non-blocking check: hold typecheck gate, edit Math.fs, verify CodeLens.lenses returns immediately!
                              use checkStarted = new ManualResetEventSlim(false)
                              use releaseCheck = new ManualResetEventSlim(false)

                              libGraph.OnFlight(fun kind _ flightCt ->
                                  if not (kind.StartsWith("parse", StringComparison.Ordinal)) then
                                      checkStarted.Set()
                                      releaseCheck.Wait(flightCt))

                              let editedFs = libFs + "\n    let extraValue = 99"
                              let! _ = host.Read(fun () -> libGraph.SetText(mathFs, editedFs)) |> within

                              let! lensesWhileHeld = CodeLens.lenses FSharpConfig.Default libGraph mathFs ct |> withinFast
                              Expect.isSome lensesWhileHeld "CodeLens.lenses must respond without waiting on held typecheck"
                              Expect.isNonEmpty lensesWhileHeld.Value "CodeLens.lenses must emit lenses for parsed declarations"
                              let extraLine, _ = findPos editedFs "let extraValue" 0 0
                              Expect.isTrue (lensesWhileHeld.Value |> Array.exists (fun l -> int l.Range.Start.Line = extraLine)) "CodeLens.lenses reflects newly parsed extraValue while typecheck is held"
                              assertPlainData "CodeLens.lenses" lensesWhileHeld.Value

                              // Release typecheck so resolve can settle
                              libGraph.OnFlight(fun _ _ _ -> ())
                              releaseCheck.Set()
                              let! _ = libGraph.AllDiagnostics() |> within

                              // Resolve signature & reference lenses on Math.fs
                              let sqLine, _ = findPos editedFs "let square" 0 0
                              let sqLenses = lensesWhileHeld.Value |> Array.filter (fun l -> int l.Range.Start.Line = sqLine)
                              Expect.equal sqLenses.Length 2 "Expected signature + references lenses on `square`"

                              let! resolvedSq =
                                  Task.WhenAll(sqLenses |> Array.map (fun l -> CodeLens.resolve scope ownersAnyThread l ct))
                                  |> within

                              let titles = resolvedSq |> Array.choose (fun l -> l.Command |> Option.map _.Title)
                              Expect.isTrue (titles |> Array.exists (fun t -> t.Contains("int -> int"))) $"Expected signature lens in %A{titles}"
                              Expect.isTrue (titles |> Array.exists (fun t -> t = "2 References")) $"Expected '2 References' lens across App in %A{titles}"

                              let refLens = resolvedSq |> Array.find (fun l -> l.Command.Value.Title = "2 References")
                              Expect.equal refLens.Command.Value.Command "fsharp.showReferences" "Non-zero reference lens uses fsharp.showReferences"
                              Expect.isSome refLens.Command.Value.Arguments "fsharp.showReferences includes arguments"

                              // Unused symbol gets "0 References" with empty command
                              let unusedLine, _ = findPos editedFs "let unusedHelper" 0 0
                              let unusedLenses = lensesWhileHeld.Value |> Array.filter (fun l -> int l.Range.Start.Line = unusedLine)
                              let! resolvedUnused =
                                  Task.WhenAll(unusedLenses |> Array.map (fun l -> CodeLens.resolve scope ownersAnyThread l ct))
                                  |> within

                              let zeroRef = resolvedUnused |> Array.find (fun l -> l.Command.Value.Title = "0 References")
                              Expect.equal zeroRef.Command.Value.Command "" "0 References lens has empty command"

                              // .fsi CodeLens + resolve
                              let! fsiLenses = CodeLens.lenses FSharpConfig.Default libGraph mathFsi ct |> within
                              Expect.isSome fsiLenses ".fsi CodeLens.lenses succeeds"
                              Expect.isNonEmpty fsiLenses.Value ".fsi declarations produce lenses"
                              let fsiSqLine, _ = findPos libFsi "val square" 0 0
                              let fsiSq = fsiLenses.Value |> Array.filter (fun l -> int l.Range.Start.Line = fsiSqLine)
                              let! resolvedFsi =
                                  Task.WhenAll(fsiSq |> Array.map (fun l -> CodeLens.resolve scope ownersAnyThread l ct))
                                  |> within

                              let fsiTitles = resolvedFsi |> Array.choose (fun l -> l.Command |> Option.map _.Title)
                              Expect.isTrue (fsiTitles |> Array.exists (fun t -> t.Contains("int -> int"))) $"Expected .fsi signature lens in %A{fsiTitles}"
                          })
          }

          testTask "CodeLens.lenses excludes record fields, union cases, enum members, exception fields, and abstract interface/class slots" {
              let src =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "type SampleRecord = { FieldOne: int; FieldTwo: string }"
                        "type SampleUnion ="
                        "    | Alpha"
                        "    | Beta of int"
                        "type SampleEnum ="
                        "    | Zero = 0"
                        "    | One = 1"
                        "exception CustomBoom of string"
                        "type IWorker ="
                        "    abstract member Execute: int -> int"
                        "[<AbstractClass>]"
                        "type BaseWorker() ="
                        "    abstract member Step: int -> int"
                        "    member _.ConcreteHelper (x: int) = x + 1"
                        "module RealFns ="
                        "    let topFn (x: int) = x * 2" ]

              do!
                  withTwoProjectWorkspace
                      [ "Filter.fs", src ]
                      [ "App.fs", "module App\nlet y = Demo.RealFns.topFn 3" ]
                      (fun _ libGraph _ libPath _ _ _ ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Filter.fs"
                              let! lensesOpt = CodeLens.lenses FSharpConfig.Default libGraph file ct |> within
                              Expect.isSome lensesOpt "CodeLens.lenses returns Some"
                              let lensLines = lensesOpt.Value |> Array.map (fun l -> int l.Range.Start.Line) |> Set.ofArray

                              for forbiddenNeedle in
                                  [ "FieldOne"
                                    "| Alpha"
                                    "| Beta"
                                    "| Zero = 0"
                                    "| One = 1"
                                    "exception CustomBoom"
                                    "abstract member Execute"
                                    "abstract member Step" ] do
                                  let fline, _ = findPos src forbiddenNeedle 0 0
                                  Expect.isFalse (lensLines.Contains fline) $"CodeLens must NOT be emitted for '%s{forbiddenNeedle}' (line %d{fline})"

                              let concreteLine, _ = findPos src "member _.ConcreteHelper" 0 0
                              let topFnLine, _ = findPos src "let topFn" 0 0
                              Expect.isTrue (lensLines.Contains concreteLine) "Concrete class member receives CodeLens"
                              Expect.isTrue (lensLines.Contains topFnLine) "Module function receives CodeLens"
                          })
          }

          testTask "InlineValue provides end-of-line pipeline stage types within range, honors Enabled=false, and normalizes ||> / |||> StageTypes to [inType; outType]" {
              let src =
                  String.concat
                      "\n"
                      [ "module Pipes"
                        "let run () ="
                        "    [ 1; 2; 3 ]"
                        "    |> List.map (fun x -> x * 2)"
                        "    |> List.sum"
                        "let pairPipe () ="
                        "    (10, 20)"
                        "    ||> (+)"
                        "let triplePipe () ="
                        "    (1, 2, 3)"
                        "    |||> (fun a b c -> a + b + c)" ]

              do!
                  withTwoProjectWorkspace
                      [ "Pipes.fs", src ]
                      [ "App.fs", "module App\nlet x = Pipes.run ()" ]
                      (fun _ libGraph _ libPath _ _ _ ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Pipes.fs"
                              let fullRange: Ionide.LanguageServerProtocol.Types.Range =
                                  { Start = { Line = 0u; Character = 0u }
                                    End = { Line = 15u; Character = 0u } }

                              let cfg = { Enabled = Some true; Prefix = "//" }
                              let! valuesOpt = InlineValue.provide cfg libGraph file fullRange ct |> within
                              Expect.isSome valuesOpt "InlineValue should return Some"
                              Expect.isNonEmpty valuesOpt.Value "Multi-stage pipeline should emit inline values"

                              let texts =
                                  valuesOpt.Value
                                  |> Array.choose (function
                                      | U3.C1 ivt -> Some(int ivt.Range.Start.Line, ivt.Text)
                                      | _ -> None)

                              Expect.isTrue
                                  (texts |> Array.exists (fun (l, t) -> l = 2 && t.StartsWith("//") && t.Contains("int list")))
                                  $"Expected line 2 '// int list' in %A{texts}"

                              Expect.isTrue
                                  (texts |> Array.exists (fun (l, t) -> l = 3 && t.StartsWith("//") && t.Contains("int list")))
                                  $"Expected line 3 '// int list' in %A{texts}"

                              Expect.isTrue
                                  (texts |> Array.exists (fun (l, t) -> l = 4 && t.StartsWith("//") && t.Contains("int")))
                                  $"Expected line 4 '// int' in %A{texts}"

                              // Verify fsharp/pipelineHint StageTypes has length 2 ([inType; outType]) for ||> and |||>!
                              let! phJson = IonideEndpoints.pipelineHint libGraph file ct |> within
                              Expect.isSome phJson "pipelineHint returns Some JSON"
                              let phNode = JsonNode.Parse phJson.Value
                              let phData = phNode["Data"].AsArray()
                              let pairLine, _ = findPos src "||> (+)" 0 0
                              let tripleLine, _ = findPos src "|||> (fun" 0 0

                              let pairStage =
                                  phData
                                  |> Seq.find (fun n -> n["Line"].GetValue<int>() = pairLine)

                              let pairTypes = [| for t in pairStage["Types"].AsArray() -> t.GetValue<string>() |]
                              Expect.equal pairTypes.Length 2 $"||> stage Types must have length 2 ([inType; outType]), got %A{pairTypes}"
                              Expect.equal pairTypes [| "int * int"; "int" |] "||> stage Types are [\"int * int\"; \"int\"]"

                              let tripleStage =
                                  phData
                                  |> Seq.find (fun n -> n["Line"].GetValue<int>() = tripleLine)

                              let tripleTypes = [| for t in tripleStage["Types"].AsArray() -> t.GetValue<string>() |]
                              Expect.equal tripleTypes.Length 2 $"|||> stage Types must have length 2 ([inType; outType]), got %A{tripleTypes}"
                              Expect.equal tripleTypes [| "int * int * int"; "int" |] "|||> stage Types are [\"int * int * int\"; \"int\"]"

                              // Range containment filter: asking only for line 2 returns ONLY line 2!
                              let line2Only: Ionide.LanguageServerProtocol.Types.Range =
                                  { Start = { Line = 2u; Character = 0u }
                                    End = { Line = 2u; Character = 80u } }

                              let! narrow = InlineValue.provide cfg libGraph file line2Only ct |> within
                              Expect.isSome narrow "narrow InlineValue returns Some"
                              Expect.isNonEmpty narrow.Value "line 2 has pipeline stage"
                              Expect.isTrue
                                  (narrow.Value |> Array.forall (function U3.C1 ivt -> ivt.Range.Start.Line = 2u | _ -> false))
                                  "InlineValue must filter strictly to requested viewport range"

                              // Disabled config
                              let! disabled = InlineValue.provide { Enabled = Some false; Prefix = "//" } libGraph file fullRange ct |> within
                              Expect.equal disabled (Some [||]) "InlineValues.Enabled = Some false returns empty array"
                          })
          } ]

[<Tests>]
let ionideAndProtocolTests =
    testList
        "Task6.IonideAndWireProtocol"
        [ testTask "All IonideEndpoints produce exact FSAC PascalCase CommandResponse JSON schemas, documentationSymbol works on promoted files on GraphHost, and lineLens is non-blocking" {
              let libFs =
                  String.concat
                      "\n"
                      [ "namespace Demo"
                        "exception DocFailure of string"
                        "type DocEnum ="
                        "    | First = 0"
                        "    | Second = 1"
                        "type DocUnion ="
                        "    | LeftCase"
                        "    | RightCase of int"
                        "type DocBox() ="
                        "    let mutable v = 0"
                        "    member _.ReadOnly = v"
                        "    member _.MutableProp"
                        "        with get () = v"
                        "        and set (nx: int) = v <- nx"
                        "    member _.Reset () = v <- 0"
                        "module Docs ="
                        "    /// Computes the weighted score."
                        "    let computeScore<'T> (weight: int) (tag: 'T) : 'T list ="
                        "        let piped ="
                        "            [ 1; 2; 3 ]"
                        "            |> List.map (fun n -> n * weight)"
                        "            |> List.sum"
                        "        let b = DocBox()"
                        "        b.MutableProp <- piped"
                        "        b.Reset()"
                        "        let _ = b.ReadOnly"
                        "        [ tag ]"
                        "    let undocFn<'U> (alpha: int) (beta: 'U) ="
                        "        let bodyLocal = alpha + 1"
                        "        sprintf \"%d-%A\" bodyLocal beta" ]

              do!
                  withTwoProjectWorkspace
                      [ "Docs.fs", libFs ]
                      [ "App.fs", "module App\nlet s = Demo.Docs.computeScore 2 \"ok\"" ]
                      (fun host libGraph _ libPath _ scope ownersAnyThread ->
                          task {
                              let ct = CancellationToken.None
                              let file = libPath "Docs.fs"
                              let! graphThreadId = host.Read(fun () -> Thread.CurrentThread.ManagedThreadId) |> within
                              let fnLine, fnCol = findPos libFs "computeScore" 0 3

                              // 1. fsharp/signature -> Kind = "typesig"
                              let! sigJson = IonideEndpoints.signature libGraph file fnLine fnCol ct |> within
                              Expect.isSome sigJson "signature should return Some JSON"
                              let sigNode = JsonNode.Parse sigJson.Value
                              Expect.equal (sigNode["Kind"].GetValue<string>()) "typesig" "Kind is typesig"
                              Expect.stringContains (sigNode["Data"].GetValue<string>()) "int" "Data contains signature"

                              // 2. fsharp/signatureData -> exact FSAC parity (bare generic "T", prefix "'T" types, getter/setter/unit-member)
                              let! sigDataJson = IonideEndpoints.signatureData libGraph file fnLine fnCol ct |> within
                              Expect.isSome sigDataJson "signatureData should return Some JSON"
                              let sdNode = JsonNode.Parse sigDataJson.Value
                              Expect.equal (sdNode["Kind"].GetValue<string>()) "signatureData" "Kind is signatureData"
                              Expect.equal (sdNode["Data"].["OutputType"].GetValue<string>()) "list<'T>" "OutputType uses prefix generic parameter"
                              Expect.equal (sdNode["Data"].["Parameters"].AsArray().Count) 2 "2 curried parameter groups"
                              Expect.equal (sdNode["Data"].["Parameters"].[1].[0].["Type"].GetValue<string>()) "'T" "Parameter type uses prefix 'T"
                              let genArr = [| for g in sdNode["Data"].["Generics"].AsArray() -> g.GetValue<string>() |]
                              Expect.equal genArr [| "T" |] "Generics emits bare 'T' without leading apostrophe"

                              // ReadOnly property getter use -> Parameters = [], Generics = []
                              let roUseLine, roUseCol = findPos libFs "b.ReadOnly" 0 4
                              let! roSd = IonideEndpoints.signatureData libGraph file roUseLine roUseCol ct |> within
                              let roData = (JsonNode.Parse roSd.Value)["Data"]
                              Expect.equal (roData["OutputType"].GetValue<string>()) "int" "Getter OutputType is int"
                              Expect.equal (roData["Parameters"].AsArray().Count) 0 "Getter Parameters = []"
                              Expect.equal (roData["Generics"].AsArray().Count) 0 "Getter Generics = []"

                              // Mutable property setter write `b.MutableProp <- piped` -> OutputType = property value type "int" (not "unit")
                              let setUseLine, setUseCol = findPos libFs "b.MutableProp <-" 0 4
                              let! setSd = IonideEndpoints.signatureData libGraph file setUseLine setUseCol ct |> within
                              let setData = (JsonNode.Parse setSd.Value)["Data"]
                              Expect.equal (setData["OutputType"].GetValue<string>()) "int" "Setter OutputType is property value type 'int'"

                              // Unit-arg member `b.Reset()` -> Parameters = [[ {"Name":"unit","Type":"unit"} ]]
                              let resetLine, resetCol = findPos libFs "b.Reset()" 0 3
                              let! resetSd = IonideEndpoints.signatureData libGraph file resetLine resetCol ct |> within
                              let resetNode = JsonNode.Parse resetSd.Value
                              let resetParams = resetNode["Data"].["Parameters"].AsArray()
                              Expect.equal resetParams.Count 1 "Unit member has 1 group"
                              Expect.equal (resetParams[0].[0].["Name"].GetValue<string>()) "unit" "Unit member param Name = unit"
                              Expect.equal (resetParams[0].[0].["Type"].GetValue<string>()) "unit" "Unit member param Type = unit"

                              // 3. fsharp/pipelineHint -> Kind = "pipelineHint"
                              let! pipeJson = IonideEndpoints.pipelineHint libGraph file ct |> within
                              Expect.isSome pipeJson "pipelineHint should return Some JSON"
                              let pipeNode = JsonNode.Parse pipeJson.Value
                              Expect.equal (pipeNode["Kind"].GetValue<string>()) "pipelineHint" "Kind is pipelineHint"
                              Expect.isGreaterThanOrEqual (pipeNode["Data"].AsArray().Count) 2 "At least 2 pipeline stages"

                              // 4. fsharp/f1Help -> Kind = "help"
                              let mapLine, mapCol = findPos libFs "List.map" 0 6
                              let! helpJson = IonideEndpoints.f1Help libGraph file mapLine mapCol ct |> within
                              Expect.isSome helpJson "f1Help should return Some JSON on List.map"
                              let helpNode = JsonNode.Parse helpJson.Value
                              Expect.equal (helpNode["Kind"].GetValue<string>()) "help" "Kind is help"
                              Expect.stringContains (helpNode["Data"].GetValue<string>()) "Microsoft.FSharp.Collections.ListModule.Map" "MSDN help slug for List.map"

                              // 5. fsharp/documentation -> Kind = "formattedDocumentation"
                              let! docJson = IonideEndpoints.documentation ownersAnyThread libGraph file fnLine fnCol ct |> within
                              Expect.isSome docJson "documentation should return Some JSON"
                              let docNode = JsonNode.Parse docJson.Value
                              Expect.equal (docNode["Kind"].GetValue<string>()) "formattedDocumentation" "Kind is formattedDocumentation"
                              Expect.stringContains (docNode["Data"].["Signature"].GetValue<string>()) "computeScore" "Signature contains symbol"
                              Expect.stringContains (docNode["Data"].["Comment"].GetValue<string>()) "weighted score" "XML doc summary included"
                              let xmlKey = docNode["Data"].["XmlKey"].GetValue<string>()
                              Expect.isNotEmpty xmlKey "XmlKey populated"

                              // 6. fsharp/documentationSymbol -> promote single-file Lib (`Docs.fs`) to Declarations mode and call with `scope.Projects` / `scope.IsLoading` (assertGraphThread active!)
                              libGraph.Promote file
                              let! mode = libGraph.WhenPromoted(file, ct) |> within
                              Expect.equal mode FileMode.Declarations "Docs.fs promoted to Declarations mode"

                              let! symDocJson =
                                  IonideEndpoints.documentationSymbol
                                      host
                                      scope.Projects
                                      scope.IsLoading
                                      xmlKey
                                      "Lib"
                                      ct
                                  |> within

                              Expect.isSome symDocJson "documentationSymbol must resolve symbol in open/promoted single-file project on GraphHost"
                              let symDocNode = JsonNode.Parse symDocJson.Value
                              Expect.equal (symDocNode["Kind"].GetValue<string>()) "formattedDocumentation" "Kind is formattedDocumentation"
                              Expect.stringContains (symDocNode["Data"].["Signature"].GetValue<string>()) "computeScore" "Resolved signature matches"

                              // Null guards on documentationSymbol
                              let! nullXmlRes = IonideEndpoints.documentationSymbol host scope.Projects scope.IsLoading null "Lib" ct |> within
                              Expect.isNone nullXmlRes "null xmlSig returns None cleanly"
                              let! nullAsmRes = IonideEndpoints.documentationSymbol host scope.Projects scope.IsLoading xmlKey null ct |> within
                              Expect.isNone nullAsmRes "null assembly returns None cleanly"

                              libGraph.Demote file

                              // 7. fsharp/documentationGenerator -> skips already-documented symbol, skips body lines, generates on undocumented header, and captures Version on GraphHost thread!
                              let! alreadyDocRes = IonideEndpoints.documentationGenerator libGraph file fnLine fnCol ct |> within
                              Expect.isNone alreadyDocRes "documentationGenerator must return None when symbol already has XML docs"

                              let bodyLine, bodyCol = findPos libFs "let bodyLocal =" 0 4
                              let! onBodyRes = IonideEndpoints.documentationGenerator libGraph file bodyLine bodyCol ct |> within
                              Expect.isNone onBodyRes "documentationGenerator must return None on body lines of multi-line binding"

                              let undocLine, undocCol = findPos libFs "let undocFn" 0 6
                              let mutable docGenVerCheckedOnGraph = 0

                              let docGenVerNow (_: string) =
                                  if Thread.CurrentThread.ManagedThreadId <> graphThreadId then
                                      failtest "documentationGenerator versionNow called off GraphHost thread!"

                                  Interlocked.Increment(&docGenVerCheckedOnGraph) |> ignore
                                  Some 11

                              let! genEdit =
                                  IonideEndpoints.documentationGeneratorWithCapabilities true docGenVerNow libGraph file undocLine undocCol ct
                                  |> within

                              Expect.isGreaterThan docGenVerCheckedOnGraph 0 "documentationGenerator versionNow ran on GraphHost thread"
                              Expect.isSome genEdit "documentationGenerator should return WorkspaceEdit on undocumented header"

                              match genEdit.Value.DocumentChanges.Value[0] with
                              | U4.C1 tde -> Expect.equal tde.TextDocument.Version (Some 11) "documentationGenerator stamped in-turn LspVersion"
                              | other -> failtestf "Expected U4.C1, got %A" other

                              let genEdits = editsByPath genEdit.Value
                              let genFileEdits = genEdits[LspClient.NormalizePath file]
                              let inserted = genFileEdits[0].NewText
                              Expect.stringContains inserted "/// <summary></summary>" "Includes <summary>"
                              Expect.stringContains inserted "/// <typeparam name=\"'U\"></typeparam>" "Includes 'U <typeparam>"
                              Expect.stringContains inserted "/// <param name=\"alpha\"></param>" "Includes alpha <param>"
                              Expect.stringContains inserted "/// <param name=\"beta\"></param>" "Includes beta <param>"
                              Expect.stringContains inserted "/// <returns></returns>" "Includes <returns>"

                              // 8. fsharp/compilerLocation -> Kind = "compilerlocation"
                              let compLoc = IonideEndpoints.compilerLocation ()
                              Expect.isNotEmpty compLoc "compilerLocation returns non-empty JSON"
                              let compNode = JsonNode.Parse compLoc
                              Expect.equal (compNode["Kind"].GetValue<string>()) "compilerlocation" "Kind is compilerlocation"
                              Expect.isNotNull compNode["Data"] "Data object present"

                              // 9. fsharp/lineLens -> non-blocking via LatestParse + exact FSAC 1-based columns & Glyph/EnclosingEntity strings!
                              use release = new ManualResetEventSlim(false)
                              libGraph.OnFlight(fun kind _ flightCt ->
                                  if not (kind.StartsWith("parse", StringComparison.Ordinal)) then
                                      release.Wait(flightCt))
                              let! _ = host.Read(fun () -> libGraph.SetText(file, libFs + "\n    let afterEdit = 123")) |> within

                              let! llJson = IonideEndpoints.lineLens libGraph file ct |> withinFast
                              Expect.isSome llJson "lineLens must respond immediately without waiting for typecheck"
                              Expect.stringContains llJson.Value "afterEdit" "lineLens reflects newly parsed afterEdit while typecheck is held"
                              let llNode = JsonNode.Parse llJson.Value
                              Expect.equal (llNode["Kind"].GetValue<string>()) "declarations" "Kind is declarations"
                              let topDecls = llNode["Data"].AsArray()
                              Expect.isNonEmpty topDecls "Declarations array non-empty"

                              let allDeclNodes =
                                  [| for top in topDecls do
                                         yield top["Declaration"]
                                         for nested in top["Nested"].AsArray() do
                                             yield nested |]

                              // Verify 1-based StartColumn / EndColumn (`module Docs =` starts at column 0 in 0-based -> 1 in 1-based FSAC rangeWriter!)
                              let docsModDecl =
                                  allDeclNodes |> Array.find (fun d -> d["Name"].GetValue<string>() = "Docs")
                              Expect.equal (docsModDecl["Glyph"].GetValue<string>()) "Module" "Module Glyph = 'Module'"
                              Expect.equal (docsModDecl["GlyphChar"].GetValue<string>()) "N" "Module GlyphChar = 'N' (FSAC parity)"
                              Expect.equal (docsModDecl["Range"].["StartColumn"].GetValue<int>()) 1 "Top-level `module Docs` Range.StartColumn is 1-based (0 + 1 = 1)"
                              Expect.equal (docsModDecl["Range"].["EndColumn"].GetValue<int>()) 24 "Top-level `module Docs` Range.EndColumn is 1-based (23 + 1 = 24)"
                              Expect.equal (docsModDecl["BodyRange"].["StartColumn"].GetValue<int>()) 12 "Top-level `module Docs` BodyRange.StartColumn is 1-based (11 + 1 = 12)"
                              Expect.equal (docsModDecl["BodyRange"].["EndColumn"].GetValue<int>()) 24 "Top-level `module Docs` BodyRange.EndColumn is 1-based (23 + 1 = 24)"

                              let exnDecl =
                                  allDeclNodes |> Array.find (fun d -> d["Name"].GetValue<string>() = "DocFailure")
                              Expect.equal (exnDecl["Glyph"].GetValue<string>()) "Exception" "Exception Glyph = 'Exception'"
                              Expect.equal (exnDecl["GlyphChar"].GetValue<string>()) "X" "Exception GlyphChar = 'X' (FSAC parity)"

                              let enumMemberDecl =
                                  allDeclNodes |> Array.find (fun d -> d["Name"].GetValue<string>() = "First")
                              Expect.equal (enumMemberDecl["Glyph"].GetValue<string>()) "Property" "EnumMember Glyph = 'Property' (FSAC parity)"
                              Expect.equal (enumMemberDecl["GlyphChar"].GetValue<string>()) "P" "EnumMember GlyphChar = 'P' (FSAC parity)"
                              Expect.equal (enumMemberDecl["EnclosingEntity"].GetValue<string>()) "En" "Enum EnclosingEntity = 'En' (FSAC parity)"

                              let unionDecl =
                                  allDeclNodes |> Array.find (fun d -> d["Name"].GetValue<string>() = "DocUnion")
                              Expect.equal (unionDecl["Glyph"].GetValue<string>()) "Type" "Union Glyph = 'Type' (FSAC parity)"
                              Expect.equal (unionDecl["GlyphChar"].GetValue<string>()) "T" "Union GlyphChar = 'T' (FSAC parity)"

                              libGraph.OnFlight(fun _ _ _ -> ())
                              release.Set()
                          })
          }

          testTask "Full JSON-RPC wire round-trips: capabilities, fsharp/fileParsed & documentAnalyzed, Workspace metadata, rename (-32602 on bad name), inlayHint, codeLens, inlineValue, and fsharp/* endpoints" {
              let fixturesRoot = Path.GetDirectoryName PublishTests.depsDir
              let repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)
              let tempRoot = Path.Combine(Path.GetTempPath(), "rfs-task6-wire-" + Guid.NewGuid().ToString("N").Substring(0, 8))
              let tempDepsDir = Path.Combine(tempRoot, "Deps")
              Directory.CreateDirectory tempDepsDir |> ignore
              File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(tempRoot, "global.json"))
              for f in Directory.GetFiles PublishTests.depsDir do
                  File.Copy(f, Path.Combine(tempDepsDir, Path.GetFileName f))
              let tempDepsProject = Path.Combine(tempDepsDir, "Deps.fsproj")
              let tempAPath = Path.Combine(tempDepsDir, "A.fs")
              use _cleanup =
                  disposing
                      { new IDisposable with
                          member _.Dispose() =
                              try
                                  if Directory.Exists tempRoot then
                                      Directory.Delete(tempRoot, true)
                              with _ -> () }

              let! (lsp: Rfs.Lsp.LspClient), owner, serverTask = TestServer.startCapturing ()
              use _ = disposing owner

              let seenNotifications = ConcurrentQueue<string * JsonNode>()
              use _sub =
                  disposing (
                      lsp.Notifications.Subscribe
                          { new IObserver<Received> with
                              member _.OnNext r =
                                  let m = r.Message["method"]
                                  if not (isNull m) then
                                      seenNotifications.Enqueue((m.GetValue<string>(), r.Message["params"]))
                              member _.OnError _ = ()
                              member _.OnCompleted() = () }
                  )

              // Check initialize response capabilities directly over JSON-RPC + live requests
              let initJson =
                  JsonNode.Parse
                      $"""{{"processId":null,"rootUri":%s{JsonValue.Create(Paths.toUri tempDepsDir).ToJsonString()},"initializationOptions":{{"automaticWorkspaceInit":false}},"capabilities":{{"workspace":{{"applyEdit":true,"workspaceEdit":{{"documentChanges":true}}}}}}}}"""

              let! (initReply: JsonNode), _ = lsp.Request("initialize", initJson) |> within
              do! lsp.Notify("initialized", JsonObject()) |> fun t -> t.WaitAsync(TimeSpan.FromSeconds 30.)
              let! (server: RanvierServer) = serverTask |> within

              // Item 5: Verify AggregateException unwrapping & SupersededLineDirectivesException / cancellation handling in Server.fs
              let supersededExn =
                  let t = typeof<FSharp.Compiler.Declarations.SupersededLineDirectivesException>
                  System.Runtime.CompilerServices.RuntimeHelpers.GetUninitializedObject(t) :?> exn

              Expect.equal
                  (server.HandleReadExn("fsharp/documentationSymbol", None, AggregateException(supersededExn)))
                  (Ok None)
                  "AggregateException(SupersededLineDirectivesException) must unwrap to Ok None"

              Expect.equal
                  (server.HandleReadExn("codeLens/resolve", "fallback", AggregateException(OperationCanceledException())))
                  (Ok "fallback")
                  "AggregateException(OperationCanceledException) must unwrap to Ok fallback"

              Expect.equal
                  (server.HandleReadExn("codeLens/resolve", "fallback", AggregateException(ObjectDisposedException("disposed"))))
                  (Ok "fallback")
                  "AggregateException(ObjectDisposedException) must unwrap to Ok fallback"

              match server.HandleReadExn("fsharp/documentationSymbol", None, AggregateException(InvalidOperationException("boom"))) with
              | Error err -> Expect.equal err.Code -32603 "Real fault inside AggregateException unwraps and returns -32603"
              | Ok v -> failtestf "Expected Error(-32603), got Ok %A" v

              let caps = initReply["capabilities"]
              Expect.isNotNull caps["renameProvider"] "RenameProvider advertised"
              Expect.equal (caps["renameProvider"].["prepareProvider"].GetValue<bool>()) true "prepareProvider = true"
              Expect.isNotNull caps["inlayHintProvider"] "InlayHintProvider advertised"
              Expect.isNotNull caps["codeLensProvider"] "CodeLensProvider advertised"
              Expect.equal (caps["codeLensProvider"].["resolveProvider"].GetValue<bool>()) true "codeLens resolveProvider = true"
              Expect.isNotNull caps["inlineValueProvider"] "InlineValueProvider advertised"

              let req (method: string) (p: JsonNode) : Task<JsonNode> =
                  task {
                      let! r, _ = lsp.Request(method, p) |> within
                      return r
                  }

              let jstr (n: JsonNode) (k: string) = n[k].GetValue<string>()
              let jprop (n: JsonNode) (k: string) = n[k]
              let jarr (n: JsonNode) = n.AsArray()

              // Load Deps project via fsharp/project and verify Output, OutputType, TargetFramework in response & notifyWorkspace!
              let projReq = JsonObject()
              let projId = JsonObject()
              projId["Uri"] <- JsonValue.Create(Paths.toUri tempDepsProject)
              projReq["Project"] <- projId
              let! projRes = req "fsharp/project" projReq
              let projBody = JsonNode.Parse(jstr projRes "content")
              Expect.equal (jstr projBody "Kind") "project" "fsharp/project returns project Kind"
              let outputStr = jstr (jprop projBody "Data") "Output"
              Expect.isTrue (outputStr.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)) $"Expected real .dll Output path, got '%s{outputStr}'"
              let tfmStr = jstr (jprop (jprop projBody "Data") "Info") "TargetFramework"
              Expect.isTrue (tfmStr.StartsWith("net", StringComparison.OrdinalIgnoreCase)) $"Expected net* TargetFramework, got '%s{tfmStr}'"
              let outType = jstr (jprop projBody "Data") "OutputType"
              Expect.isTrue (outType = "library" || outType = "lib" || outType = "exe") $"Expected valid OutputType, got '%s{outType}'"

              // Open A.fs and verify fsharp/fileParsed + fsharp/documentAnalyzed arrive!
              let aPath = tempAPath
              let aUri = Paths.toUri aPath
              let aText =
                  String.concat
                      "\n"
                      [ "module A"
                        "let greet name = \"Hello, \" + name"
                        "let (|+|) (x: int) (y: int) = x + y"
                        "let numbers () ="
                        "    [ 1; 2; 3 ]"
                        "    |> List.map (fun n -> n |+| 1)" ]

              do! lsp.DidOpen(aPath, aText)

              do!
                  PublishTests.eventually "fsharp/fileParsed and fsharp/documentAnalyzed" (fun () ->
                      let arr = seenNotifications.ToArray()
                      let hasParsed = arr |> Array.exists (fun (m, _) -> m = "fsharp/fileParsed")
                      let hasAnalyzed = arr |> Array.exists (fun (m, _) -> m = "fsharp/documentAnalyzed")
                      hasParsed && hasAnalyzed)

              // Wire: textDocument/prepareRename & textDocument/rename (valid + invalid -32602 for identifier `foo`` and operator `notAnOp`)
              let greetLine, greetCol = findPos aText "greet" 0 2
              let opWireLine, opWireCol = findPos aText "(|+|)" 0 2
              let posParams (line: int) (col: int) =
                  let o = JsonObject()
                  let td = JsonObject()
                  td["uri"] <- JsonValue.Create aUri
                  let pos = JsonObject()
                  pos["line"] <- JsonValue.Create line
                  pos["character"] <- JsonValue.Create col
                  o["textDocument"] <- td
                  o["position"] <- pos
                  o

              let! prepWire = req "textDocument/prepareRename" (posParams greetLine greetCol)
              Expect.isNotNull prepWire "textDocument/prepareRename returns range + placeholder"
              Expect.equal (jstr prepWire "placeholder") "greet" "prepareRename placeholder"

              let renameReqAt (line: int) (col: int) (newName: string) =
                  let o = posParams line col
                  o["newName"] <- JsonValue.Create newName
                  o

              let renameReq (newName: string) = renameReqAt greetLine greetCol newName

              let! renameWire = req "textDocument/rename" (renameReq "welcome")
              Expect.isNotNull renameWire "textDocument/rename returns WorkspaceEdit"

              // Custom operator rename over JSON-RPC succeeds for `|++|`
              let! opRenameWire = req "textDocument/rename" (renameReqAt opWireLine opWireCol "|++|")
              Expect.isNotNull opRenameWire "textDocument/rename on custom operator (|+|) -> |++| returns WorkspaceEdit"

              // Invalid rename names over JSON-RPC must fail with -32602 (empty, malformed identifier `foo``, and non-operator `notAnOp` on operator!)
              for badLine, badCol, badName, desc in
                  [ greetLine, greetCol, "", "empty newName"
                    greetLine, greetCol, "foo`", "malformed single-backtick identifier"
                    opWireLine, opWireCol, "notAnOp", "non-operator newName on custom operator" ] do
                  let! badOutcome =
                      task {
                          try
                              let! _ = req "textDocument/rename" (renameReqAt badLine badCol badName)
                              return None
                          with ex ->
                              return Some ex.Message
                      }

                  Expect.isSome badOutcome $"Expected JSON-RPC error for %s{desc}"
                  Expect.stringContains badOutcome.Value "-32602" $"Error code must be InvalidParams (-32602) for %s{desc}"

              // Wire: textDocument/inlayHint, textDocument/codeLens, codeLens/resolve, textDocument/inlineValue
              let rangeReq () =
                  let o = JsonObject()
                  let td = JsonObject()
                  td["uri"] <- JsonValue.Create aUri
                  let r = JsonObject()
                  let s = JsonObject()
                  s["line"] <- JsonValue.Create 0
                  s["character"] <- JsonValue.Create 0
                  let e = JsonObject()
                  e["line"] <- JsonValue.Create 10
                  e["character"] <- JsonValue.Create 0
                  r["start"] <- s
                  r["end"] <- e
                  o["textDocument"] <- td
                  o["range"] <- r
                  o

              let! inlayWire = req "textDocument/inlayHint" (rangeReq ())
              Expect.isNotNull inlayWire "textDocument/inlayHint returns array"
              Expect.isNonEmpty (jarr inlayWire) "textDocument/inlayHint non-empty"

              let clReq = JsonObject()
              let tdOnly = JsonObject()
              tdOnly["uri"] <- JsonValue.Create aUri
              clReq["textDocument"] <- tdOnly
              let! clWire = req "textDocument/codeLens" clReq
              Expect.isNotNull clWire "textDocument/codeLens returns array"
              let clArr = jarr clWire
              let firstLens = clArr[0].DeepClone()
              let! resolvedWire = req "codeLens/resolve" firstLens
              Expect.isNotNull (jprop resolvedWire "command") "codeLens/resolve populates command"

              let ivReq = rangeReq ()
              let ivCtx = JsonObject()
              ivCtx["frameId"] <- JsonValue.Create 0
              let stopped = JsonObject()
              let zeroPos () =
                  let p = JsonObject()
                  p["line"] <- JsonValue.Create 0
                  p["character"] <- JsonValue.Create 0
                  p
              stopped["start"] <- zeroPos ()
              stopped["end"] <- zeroPos ()
              ivCtx["stoppedLocation"] <- stopped
              ivReq["context"] <- ivCtx
              let! ivWire = req "textDocument/inlineValue" ivReq
              Expect.isNotNull ivWire "textDocument/inlineValue returns array"
              Expect.isNonEmpty (jarr ivWire) "textDocument/inlineValue non-empty"

              // Wire: fsharp/signature, fsharp/signatureData, fsharp/lineLens, fsharp/pipelineHint, fsharp/documentation, fsharp/documentationGenerator, fsharp/compilerLocation, fsharp/loadAnalyzers
              let! sigWire = req "fsharp/signature" (posParams greetLine greetCol)
              Expect.stringContains (jstr sigWire "content") "\"Kind\":\"typesig\"" "fsharp/signature wire"

              let! sdWire = req "fsharp/signatureData" (posParams greetLine greetCol)
              Expect.stringContains (jstr sdWire "content") "\"Kind\":\"signatureData\"" "fsharp/signatureData wire"

              let llReq = JsonObject()
              llReq["Project"] <- tdOnly.DeepClone()
              let! llWire = req "fsharp/lineLens" llReq
              Expect.stringContains (jstr llWire "content") "\"Kind\":\"declarations\"" "fsharp/lineLens wire"

              let phReq = JsonObject()
              phReq["TextDocument"] <- tdOnly.DeepClone()
              let! phWire = req "fsharp/pipelineHint" phReq
              Expect.stringContains (jstr phWire "content") "\"Kind\":\"pipelineHint\"" "fsharp/pipelineHint wire"

              let! docWire = req "fsharp/documentation" (posParams greetLine greetCol)
              Expect.stringContains (jstr docWire "content") "\"Kind\":\"formattedDocumentation\"" "fsharp/documentation wire"

              let! clocWire = req "fsharp/compilerLocation" (JsonObject())
              Expect.stringContains (jstr clocWire "content") "\"Kind\":\"compilerlocation\"" "fsharp/compilerLocation wire"

              let! _ = req "fsharp/loadAnalyzers" (JsonObject())

              let! _ = req "fsharp/documentationGenerator" (posParams greetLine greetCol)
              let applyReqs =
                  lsp.ServerRequests
                  |> Array.filter (fun r -> r["method"].GetValue<string>() = "workspace/applyEdit")
              Expect.isNonEmpty applyReqs "fsharp/documentationGenerator must send workspace/applyEdit to client"

              // Item 8: Hold `parse` on the initial owner graph, send DidChange (Epoch = 1), then Reload (disposing the old
              // owner graph while LatestParse is in flight), and verify `fsharp/fileParsed` reactively retries and fires!
              let oldOwner = (server.Workspace.ProjectOf aPath).Value
              let parseEntered = new ManualResetEventSlim(false)
              let releaseParse = new ManualResetEventSlim(false)
              use _pe = disposing parseEntered
              use _rp = disposing releaseParse

              oldOwner.OnFlight(fun kind _ flightCt ->
                  if kind = "parse" then
                      parseEntered.Set()
                      releaseParse.Wait(flightCt))

              let parsedBeforeReload =
                  seenNotifications.ToArray()
                  |> Array.filter (fun (m, _) -> m = "fsharp/fileParsed")
                  |> Array.length

              let nextAText = aText + "\nlet afterReload = 99"
              let fullLines = aText.Split('\n')

              let fullEdit: Rfs.Lsp.Edit =
                  { Range =
                      { Start = { Line = 0; Character = 0 }
                        End = { Line = fullLines.Length; Character = 0 } }
                    Text = nextAText }

              do! lsp.DidChange(aPath, 1, fullEdit)
              Expect.isTrue (parseEntered.Wait(TimeSpan.FromSeconds 10.)) "Initial owner LatestParse entered held parse flight"

              try
                  do! server.Workspace.Reload tempDepsProject |> withinUnit

                  do!
                      PublishTests.eventually "fsharp/fileParsed retried on reloaded owner graph" (fun () ->
                          let nowParsed =
                              seenNotifications.ToArray()
                              |> Array.filter (fun (m, _) -> m = "fsharp/fileParsed")
                              |> Array.length

                          nowParsed > parsedBeforeReload)
              finally
                  oldOwner.OnFlight(fun _ _ _ -> ())
                  releaseParse.Set()
          } ]
