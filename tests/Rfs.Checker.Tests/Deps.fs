/// A ProjectGraph over the Deps fixture, edited in memory and compared with the oracle.
module Rfs.Checker.Tests.Deps

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Rfs.Checker
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects

let depsDir =
    let rec up (d: DirectoryInfo) =
        let candidate = Path.Combine(d.FullName, "tests", "fixtures", "Deps")
        if Directory.Exists candidate then candidate else up d.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

let depsProject = Path.Combine(depsDir, "Deps.fsproj")

/// Two independent chains: A.fs ← B.fs and C.fs ← D.fs
let chainsProject = Path.Combine(Path.GetDirectoryName depsDir, "Chains", "Chains.fsproj")

let fileOf (name: string) = Path.Combine(depsDir, name)

type Session =
    { Host: GraphHost
      Graph: ProjectGraph
      /// How the graph checks every file
      Mode: FileMode
      Spec: ProjectSpec
      Oracle: OracleClient
      /// A project context of its own, for checking the current texts from scratch
      Fresh: ProjectContext
      /// The current text of every source file
      Texts: Dictionary<string, string> }

    interface IAsyncDisposable with
        member s.DisposeAsync() =
            s.Host.Dispose()
            (s.Oracle :> IAsyncDisposable).DisposeAsync()

/// A wait long enough for a busy machine, short enough to fail a hang.
let settle (t: Task<'T>) : Task<'T> =
    task {
        let! winner = Task.WhenAny(t, Task.Delay(TimeSpan.FromMinutes 2.))

        if not (obj.ReferenceEquals(winner, t)) then
            failtest "timed out waiting for the graph to settle"

        return! t
    }

/// Waits until promoting every file has settled for its latest text (at once for a file that is not wanted).
let whenPromoted (graph: ProjectGraph) (spec: ProjectSpec) : Task =
    task {
        for f in spec.SourceFiles do
            let! _ = graph.WhenPromoted(f, CancellationToken.None) |> settle
            ()
    }

/// A graph over `project` checking every file in `mode`, settled on the files as they are on disk; in Declarations
/// mode, once every implementation file is promoted.
let startOn (project: string) (mode: FileMode) : Task<Session> =
    task {
        let! spec = ProjectSpec.load project
        let host = GraphHost.Start "deps"
        let! graph = ProjectGraph.CreateWithMode(host, spec, mode)
        let! _ = graph.AllDiagnostics() |> settle
        do! whenPromoted graph spec
        let! oracle = OracleClient.start ()
        let! fresh = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.StartAsTask
        let texts = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for f in spec.SourceFiles do
            texts[f] <- File.ReadAllText f

        return
            { Host = host
              Graph = graph
              Mode = mode
              Spec = spec
              Oracle = oracle
              Fresh = fresh
              Texts = texts }
    }

/// A graph over Deps checking every file in `mode`.
let startWith (mode: FileMode) = startOn depsProject mode

/// A graph over Deps as ProjectGraph.Create makes it: every file file-level.
let start () = startWith FileLevel

type Edit =
    { /// How many CheckDeclaration results were published (AdoptedChecks): superseded flights are not counted
      Delta: int64
      /// The newly published results, in order
      Checked: (DeclarationKey * string) list
      /// Every CheckDeclaration that returned, in order
      Ran: (DeclarationKey * string) list
      Diagnostics: IReadOnlyDictionary<string, FSharpDiagnostic[]>
      /// Each file's SettledDiagnostics, asked for right after the text was set, before anything settled
      Settled: IReadOnlyDictionary<string, FSharpDiagnostic[]> }

    /// The names of the published results of a file
    member e.NamesIn(file: string) =
        e.Checked
        |> List.filter (fun (_, f) -> String.Equals(f, file, StringComparison.OrdinalIgnoreCase))
        |> List.map (fun (k, _) -> k.Name)
        |> List.distinct
        |> List.sort

    member e.KeysIn(file: string) =
        e.Checked
        |> List.filter (fun (_, f) -> String.Equals(f, file, StringComparison.OrdinalIgnoreCase))
        |> List.map fst
        |> Set.ofList

    /// How many times each declaration of a file was checked
    member e.RunsIn(file: string) =
        e.Ran
        |> List.filter (fun (_, f) -> String.Equals(f, file, StringComparison.OrdinalIgnoreCase))
        |> List.countBy (fun (k, _) -> k.Name)
        |> Map.ofList

/// Sets the text of `file`, runs `beforeReading`, and waits until every file is settled, and when `promoted`, until
/// promoting every wanted file has settled too.
let editAfter (beforeReading: unit -> Task) (promoted: bool) (s: Session) (file: string) (text: string) : Task<Edit> =
    task {
        let events = ConcurrentQueue<DeclarationKey * string>()
        let ran = ConcurrentQueue<DeclarationKey * string>()
        use _ = s.Graph.Checked |> Observable.subscribe events.Enqueue
        use _ = s.Graph.Ran |> Observable.subscribe ran.Enqueue
        let before = s.Graph.AdoptedChecks
        s.Texts[file] <- text
        s.Graph.SetText(file, text) |> ignore
        do! beforeReading ()
        let settledEach = [ for f in s.Spec.SourceFiles -> f, s.Graph.SettledDiagnostics f ]
        let! _ = s.Graph.AllDiagnostics() |> settle
        let settled = Dictionary<string, FSharpDiagnostic[]>(StringComparer.OrdinalIgnoreCase)

        for f, t in settledEach do
            let! struct (_, d) = t |> settle
            settled[f] <- d

        if promoted then
            do! whenPromoted s.Graph s.Spec

        let! diagnostics = s.Graph.AllDiagnostics() |> settle

        return
            { Delta = s.Graph.AdoptedChecks - before
              Checked = List.ofSeq events
              Ran = List.ofSeq ran
              Diagnostics = diagnostics
              Settled = settled }
    }

/// Sets the text of `file` and waits until every file is settled, and when `promoted`, until promoting every wanted
/// file has settled too.
let editWith (promoted: bool) (s: Session) (file: string) (text: string) : Task<Edit> =
    editAfter (fun () -> Task.CompletedTask) promoted s file text

/// Sets the text of `file` and waits until every file is settled and every wanted file's promotion has.
let edit (s: Session) (file: string) (text: string) : Task<Edit> = editWith true s file text

let replace (s: Session) (file: string) (oldText: string) (newText: string) =
    let text = s.Texts[file]

    if not (text.Contains oldText) then
        failtest $"%s{Path.GetFileName file} does not contain %s{oldText}"

    edit s file (text.Replace(oldText, newText))

/// The lines by which the graph's diagnostics differ from a full check of the current texts.
let oracleDiff (s: Session) (diagnostics: IReadOnlyDictionary<string, FSharpDiagnostic[]>) : Task<string list> =
    task {
        let! expected = s.Oracle.Check s.Spec.ProjectFile (s.Texts |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
        let actual = [ for kv in diagnostics do yield! kv.Value |> Seq.map FcsDiagnostics.ofFcs ]
        return Diagnostics.compare expected actual
    }

/// Each implementation file's final fingerprints, and its file signature's, from a declaration-by-declaration check of
/// the current texts from scratch, with nothing kept.
let freshFingerprints (s: Session) =
    let mutable before = []
    let result = Dictionary<string, IReadOnlyDictionary<DeclarationKey, string> * string>(StringComparer.OrdinalIgnoreCase)

    for file in s.Spec.SourceFiles do
        let parse = DeclarationChecker.Parse(s.Fresh, file, s.Texts[file])

        if file.EndsWith ".fsi" then
            before <- before @ [ DeclarationChecker.CheckSignatureFile(s.Fresh, before, parse) ]
        elif DeclarationChecker.CountDeclarations parse <= 1 then
            // Never checked by declaration: a fragment replayed row by row is not what a file like this means
            let whole = DeclarationChecker.CheckFile(s.Fresh, before, parse)
            result[file] <- (whole.Fingerprints, whole.Signature.Fingerprint)
            before <- before @ [ whole.Signature ]
        else
            let start, env0 = DeclarationChecker.BeginFile(s.Fresh, before, parse)

            let results =
                DeclarationChecker.Split parse
                |> Array.mapFold
                    (fun env decl ->
                        let r = DeclarationChecker.CheckDeclaration(start, env, decl, CancellationToken.None)
                        r, DeclarationChecker.ExtendEnv(env, r))
                    env0
                |> fst

            let finished = DeclarationChecker.FinishFile(start, results, parse)
            result[file] <- (finished.Fingerprints, finished.Signature.Fingerprint)
            before <- before @ [ finished.Signature ]

    result

/// The lines by which the graph's final fingerprints differ from a check from scratch: a kept result that a full check
/// would give differently, even when the diagnostics agree. A result of a file checked whole has no per-declaration
/// fingerprints, so its file signature's fingerprint is compared. Fingerprints that are not describable (`?`) are
/// skipped.
let fingerprintDiff (s: Session) : Task<string list> =
    task {
        let! graph = s.Graph.AllFileResults() |> settle
        let fresh = freshFingerprints s

        return
            [ for KeyValue(file, (expected, expectedFile)) in fresh do
                  let byDeclaration = graph[file].Fingerprints.Count > 0
                  let actualFile = graph[file].Signature.Fingerprint

                  if not (actualFile = expectedFile || actualFile.StartsWith "?" || expectedFile.StartsWith "?") then
                      $"%s{Path.GetFileName file}: file signature %s{actualFile} <> fresh %s{expectedFile}"

                  if byDeclaration then
                      let actual = graph[file].Fingerprints

                      for KeyValue(key, fp) in expected do
                          match actual.TryGetValue key with
                          | true, a when a = fp || a.StartsWith "?" || fp.StartsWith "?" -> ()
                          | true, a -> $"%s{Path.GetFileName file} %A{key.Name}: %s{a} <> fresh %s{fp}"
                          | _ -> $"%s{Path.GetFileName file} %A{key.Name}: missing"

                      if actual.Count <> expected.Count then
                          $"%s{Path.GetFileName file}: %d{actual.Count} declarations <> fresh %d{expected.Count}" ]
    }

/// The graph's diagnostics equal the oracle's, its fingerprints equal a check from scratch, and the D1 premise held.
let expectOracle (s: Session) (diagnostics: IReadOnlyDictionary<string, FSharpDiagnostic[]>) (what: string) =
    task {
        let! diff = oracleDiff s diagnostics
        Expect.isEmpty diff $"%s{what}: diagnostics equal a full check:\n%s{String.concat Environment.NewLine diff}"
        let! fingerprints = fingerprintDiff s
        Expect.isEmpty fingerprints $"%s{what}: fingerprints equal a check from scratch:\n%s{String.concat Environment.NewLine fingerprints}"
        Expect.equal s.Graph.PremiseBreaks 0L $"%s{what}: no final fingerprint differed from its check's (D1)"
    }

/// Every symbol use of the file's results, with where its symbol is declared, in the signature and in the
/// implementation: what go to definition and find references read. An active pattern case's declaration is its tag's
/// (the subclass's DeclarationLocation, which the base property does not reach). For a namespace, only the use is
/// compared: it is declared by every file that opens a fragment of it, and which of those its location names depends
/// on the order checks combined them. The signature location is `signatureOf`'s, which is the query for the graph's
/// results: read off a kept symbol it stays wrong (an older check's objects, which conformance does not reach, and for a
/// symbol another file declares its signature, reused while its fingerprint is the same). The declared and implementation
/// locations are read off the symbol: only a kept row's own items are shifted, so for a symbol another file declares they
/// are not compared, and Task 17, which reads locations too, goes through ProjectGraph.CurrentLocation. A symbol with no
/// key answers only inside the asking file, so elsewhere only that is compared. In a file that doesn't compile, a key two
/// definitions share (`ties`) answers nothing, so only the use is compared.
let private locationsOf (ties: Set<string>) (signatureOf: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse -> FSharp.Compiler.Text.range option) (file: string) (results: FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults) =
    results.GetAllUsesOfAllSymbolsInFile()
    |> Seq.map (fun u ->
        let inOtherFile (m: FSharp.Compiler.Text.range) =
            not (String.Equals(m.FileName, file, StringComparison.OrdinalIgnoreCase))

        let elsewhere = u.Symbol.DeclarationLocation |> Option.exists inOtherFile
        let key = FSharp.Compiler.Declarations.DeclarationChecker.SymbolKey u.Symbol

        match u.Symbol with
        | :? FSharp.Compiler.Symbols.FSharpEntity as e when e.IsNamespace -> $"%A{u.Range} %s{u.Symbol.FullName}: a namespace"
        | :? FSharp.Compiler.Symbols.FSharpParameter -> $"%A{u.Range} %s{u.Symbol.FullName}: a parameter"
        | _ when key.IsNone && u.Symbol.SignatureLocation |> Option.exists inOtherFile -> $"%A{u.Range} %s{u.Symbol.FullName}: no key, and elsewhere"
        | _ when key.IsSome && ties.Contains key.Value -> $"%A{u.Range} %s{u.Symbol.FullName}: a tie"
        | _ when elsewhere -> $"%A{u.Range} %s{u.Symbol.FullName}: declared elsewhere, signature %A{signatureOf u}"
        | :? FSharp.Compiler.Symbols.FSharpActivePatternCase as case -> $"%A{u.Range} %s{u.Symbol.FullName}: tag %A{case.DeclarationLocation}"
        | _ ->
            $"%A{u.Range} %s{u.Symbol.FullName}: declared %A{u.Symbol.DeclarationLocation}, signature %A{signatureOf u}, implementation %A{u.Symbol.ImplementationLocation}")
    |> Set.ofSeq

/// The keys that two definitions of a file with errors share
let private tiesIn (results: FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults) =
    if results.Diagnostics |> Array.exists (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error) then
        results.GetAllUsesOfAllSymbolsInFile()
        |> Seq.filter _.IsFromDefinition
        |> Seq.choose (fun u -> FSharp.Compiler.Declarations.DeclarationChecker.SymbolKey u.Symbol |> Option.map (fun k -> k, u.Symbol.SignatureLocation))
        |> Seq.distinct
        |> Seq.countBy fst
        |> Seq.filter (fun (_, n) -> n > 1)
        |> Seq.map fst
        |> Set.ofSeq
    else
        Set.empty

/// What the graph's query answers for each use of the results whose symbol a source file of the project defines. An
/// external assembly's symbol says for itself, and so does a definition outside a signature, which is the row's own.
let private currentSignatures (s: Session) (file: string) (results: FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults) =
    task {
        let answers = Dictionary<string * string, FSharp.Compiler.Text.range option>()

        let ownDefinition (u: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse) (landed: FSharp.Compiler.Text.range) =
            u.IsFromDefinition && not (landed.FileName.EndsWith ".fsi")

        for u in results.GetAllUsesOfAllSymbolsInFile() do
            match u.Symbol, FSharp.Compiler.Declarations.DeclarationChecker.SymbolKey u.Symbol, u.Symbol.SignatureLocation with
            | :? FSharp.Compiler.Symbols.FSharpEntity as e, _, _ when e.IsNamespace -> ()
            | _, Some key, Some landed when
                not (ownDefinition u landed)
                && not (answers.ContainsKey(key, landed.FileName))
                && s.Spec.SourceFiles |> List.exists (fun f -> String.Equals(f, landed.FileName, StringComparison.OrdinalIgnoreCase))
                ->
                let! answer = s.Graph.CurrentLocation(file, landed.FileName, key, System.Threading.CancellationToken.None) |> settle
                answers[(key, landed.FileName)] <- answer
            | _ -> ()

        return
            fun (u: FSharp.Compiler.CodeAnalysis.FSharpSymbolUse) ->
                match FSharp.Compiler.Declarations.DeclarationChecker.SymbolKey u.Symbol, u.Symbol.SignatureLocation with
                | Some key, Some landed when not (ownDefinition u landed) && answers.ContainsKey(key, landed.FileName) -> answers[(key, landed.FileName)]
                | _, own -> own
    }

/// The lines by which the symbol locations of the graph's published results differ from the interactive checker's for
/// the current texts, in each of the files (Task 7 review I1: a shifted row must not point at another declaration's old
/// items).
let locationDiffs (s: Session) (files: string list) : Task<string list> =
    task {
        let! graph = s.Graph.AllFileResults() |> settle
        let checker = FSharp.Compiler.SourceCodeServices.InteractiveChecker.Create(ProjectSpec.toFcsOptions (ProjectSpec.sequential s.Spec))
        let texts = Dictionary<string, string>(s.Texts, StringComparer.OrdinalIgnoreCase)
        let sourceReader (f: string) = let t = texts[f] in hash t, lazy t
        let differences = ResizeArray<string>()

        for file in files do
            let results = graph[file].CheckResults
            let! signatureOf = currentSignatures s file results

            let! _, reference, _ =
                checker.ParseAndCheckFileInProject(s.Spec.ProjectFile, Array.ofList s.Spec.SourceFiles, sourceReader, file)
                |> Async.StartAsTask

            let ties = tiesIn reference
            let actual = locationsOf ties signatureOf file results
            let expected = locationsOf ties _.Symbol.SignatureLocation file reference
            let name = IO.Path.GetFileName file

            for l in Set.difference expected actual do
                differences.Add $"%s{name}: expected only: %s{l}"

            for l in Set.difference actual expected do
                differences.Add $"%s{name}: actual only: %s{l}"

        return List.ofSeq differences
    }

let locationDiff (s: Session) (file: string) : Task<string list> = locationDiffs s [ file ]

let expectLocations (s: Session) (file: string) (what: string) =
    task {
        let! diff = locationDiff s file
        Expect.isEmpty diff $"%s{what}: every symbol's declaration equals a full check's:\n%s{String.concat Environment.NewLine diff}"
    }
