namespace Rfs.Checkpoint

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices
open Rfs.Checker
open Rfs.Lsp
open Rfs.Projects

/// How the fork's graph check obtained each file's result (FICKL_FCS_GRAPH_LOG).
type GraphCheckCounts = { Reused: int; Kept: int; Replaced: int }

/// The whole project's diagnostics after a check.
type EngineResult =
    { Diagnostics: Diagnostic list
      /// Declaration checks this call adopted (published); None for an engine that does not check by declaration.
      DeclsChecked: int option
      /// The fork's graph-check line for this call, when FICKL_FCS_GRAPH_LOG is set and the project checks in graph mode.
      GraphCheck: GraphCheckCounts option }

/// ProjectGraph's running totals, read between edits.
type EngineCounters =
    { /// CheckDeclaration runs, superseded flights included.
      Checked: int64
      /// CheckDeclaration results that were published.
      Adopted: int64
      /// Stopwatch ticks inside FinishFile.
      FinishTicks: int64
      /// Rows checked inside R11/R12 segments, now.
      SegmentRows: int
      /// Published results whose final fingerprint broke the D1 premise.
      PremiseBreaks: int64
      /// Stopwatch ticks of each part of FinishFile (FileResult.PhaseTicks).
      FinishPhaseTicks: Map<string, int64>
      /// Bytes each phase of the file-level graph allocated, on the threads that ran it.
      PhaseAllocated: Map<string, int64>
      /// CheckFile runs, superseded flights included.
      FileLevelChecks: int64
      /// Setups from a file's dependencies the checker refused (a stale dependency snapshot), checked from every earlier
      /// file instead.
      ChainFallbacks: int64
      /// Results FinishFile reused from a file's previous finish.
      FinishReused: int64 }

/// Which files a promotion left checked by declaration, and which it could not promote (checked whole).
type Promotion =
    { Promoted: string list
      NotPromoted: string list }

/// A checker holding one project in memory, created from the texts on disk.
type IEngine =
    inherit IDisposable
    /// Running totals, for an engine that keeps them; read outside the timed region.
    abstract Counters: unit -> EngineCounters option
    /// The first full check.
    abstract Check: unit -> Task<EngineResult>
    /// Replaces a source file's text and checks again.
    abstract SetText: file: string * text: string -> Task<EngineResult>
    /// The first and last line (1-based) of each declaration of `text` as the file's text, for an engine that checks
    /// by declaration; read outside the timed region.
    abstract DeclarationLines: file: string * text: string -> (int * int)[] option
    /// Promotes the files (as an editor's didOpen would) and waits until promoting each has settled; None for an
    /// engine that does not promote.
    abstract Promote: files: string list -> Task<Promotion option>

type EngineFactory =
    { Name: string
      Create: ProjectSpec -> Task<IEngine> }

/// `everyFile`: every implementation file is wanted (a Declarations graph), so each check also waits until all of them
/// are checked by declaration again.
[<Sealed>]
type private DeclEngine(host: GraphHost, graph: ProjectGraph, spec: ProjectSpec, everyFile: bool) =
    /// Each file with the mode its promotion settled in.
    let whenPromoted (files: string list) =
        task {
            let modes = ResizeArray()

            for f in files do
                let! mode = graph.WhenPromoted(f, Threading.CancellationToken.None)
                modes.Add((f, mode))

            return List.ofSeq modes
        }

    let settle (before: int64) =
        task {
            let! _ = graph.AllDiagnostics()

            if everyFile then
                let! _ = whenPromoted spec.SourceFiles
                ()

            let! all = graph.AllDiagnostics()
            let failures = Array.append (host.TakePostErrors()) (host.TakePumpErrors())

            if failures.Length > 0 then
                raise (AggregateException("the graph host reported errors", failures))

            return
                { Diagnostics = [ for KeyValue(_, ds) in all do for d in ds -> FcsDiagnostics.ofFcs d ]
                  DeclsChecked = Some(int (graph.AdoptedChecks - before))
                  GraphCheck = None }
        }

    interface IEngine with
        member _.Counters() =
            Some
                { Checked = graph.CheckedCount
                  Adopted = graph.AdoptedChecks
                  FinishTicks = graph.FinishTicks
                  SegmentRows = graph.SegmentRowsTotal
                  PremiseBreaks = graph.PremiseBreaks
                  FinishPhaseTicks = graph.FinishPhaseTicks |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
                  PhaseAllocated = graph.PhaseAllocated |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Map.ofSeq
                  FileLevelChecks = graph.FileLevelChecks
                  ChainFallbacks = graph.ChainFallbacks
                  FinishReused = graph.FinishReused }

        member _.DeclarationLines(file, text) = Some(graph.DeclarationLines(file, text))

        member _.Promote files =
            task {
                for f in files do
                    graph.Promote f

                let! modes = whenPromoted files
                let promoted, notPromoted = modes |> List.partition (fun (_, mode) -> mode = Rfs.Checker.FileMode.Declarations)
                return Some { Promoted = List.map fst promoted; NotPromoted = List.map fst notPromoted }
            }

        member _.Check() = settle 0L

        member _.SetText(file, text) =
            let before = graph.AdoptedChecks
            graph.SetText(file, text) |> ignore
            settle before

        member _.Dispose() = host.Dispose()

/// Takes the fork's "Graph check: R reused, K kept, P replaced" lines off stderr, passing every other line on.
[<Sealed>]
type private GraphCheckLog(inner: TextWriter) =
    inherit TextWriter()
    static let pattern = Regex(@"^Graph check: (\d+) reused, (\d+) kept, (\d+) replaced\s*$")
    let line = StringBuilder()
    let mutable last = None

    static let installed =
        lazy
            (let log = new GraphCheckLog(Console.Error)
             Console.SetError(TextWriter.Synchronized log)
             log)

    /// Captures the process's stderr from now on.
    static member Install() = installed.Value

    override _.Encoding = inner.Encoding

    override _.Write(c: char) =
        lock line (fun () ->
            if c = '\n' then
                let text = line.ToString().TrimEnd('\r')
                line.Clear() |> ignore
                let m = pattern.Match text

                if m.Success then
                    last <-
                        Some
                            { Reused = int m.Groups[1].Value
                              Kept = int m.Groups[2].Value
                              Replaced = int m.Groups[3].Value }
                else
                    inner.WriteLine text
            else
                line.Append c |> ignore)

    /// The counts of the latest graph check since the last call, if any.
    member _.Take() =
        lock line (fun () ->
            let r = last
            last <- None
            r)

/// The fork's whole-project check, reusing earlier results file by file. In graph checking mode a changed file whose
/// signature fingerprint is unchanged keeps its dependents' results; in sequential mode every file from the edited
/// one on is checked again.
[<Sealed>]
type private FileSlimEngine(spec: ProjectSpec) =
    let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
    let files = Array.ofList spec.SourceFiles

    // Hashed once per text, not on every read: the reader is called for every file on every check.
    let texts = Dictionary<string, int * Lazy<string>>(StringComparer.OrdinalIgnoreCase)

    let store (file: string) (text: string) =
        texts[Path.GetFullPath file] <- (hash text, lazy text)

    do
        for f in files do
            store f (File.ReadAllText f)

    let reader (file: string) = texts[Path.GetFullPath file]

    // The fork reads the variable on every graph check.
    let log =
        if isNull (Environment.GetEnvironmentVariable "FICKL_FCS_GRAPH_LOG") then
            None
        else
            Some(GraphCheckLog.Install())

    let check () =
        task {
            log |> Option.iter (fun l -> l.Take() |> ignore)
            let! results = checker.ParseAndCheckProject(spec.ProjectFile, files, reader) |> Async.StartAsTask

            return
                { Diagnostics = [ for d in results.Diagnostics -> FcsDiagnostics.ofFcs d ]
                  DeclsChecked = None
                  GraphCheck = log |> Option.bind _.Take() }
        }

    interface IEngine with
        member _.Counters() = None
        member _.DeclarationLines(_, _) = None
        member _.Promote _ = Task.FromResult None
        member _.Check() = check ()

        member _.SetText(file, text) =
            if not (texts.ContainsKey(Path.GetFullPath file)) then
                invalidArg (nameof file) $"%s{file} is not a source file of the project"

            store file text
            check ()

        member _.Dispose() = ()

module Engines =
    let private graphEngine (everyFile: bool) (create: GraphHost -> ProjectSpec -> Task<ProjectGraph>) (spec: ProjectSpec) =
        task {
            let host = GraphHost.Start "checkpoint"

            try
                let! graph = create host spec
                return new DeclEngine(host, graph, spec, everyFile) :> IEngine
            with e ->
                host.Dispose()
                return raise e
        }

    /// ProjectGraph as it is created: every file checked whole on a Ranvier graph, set up from the files it depends on;
    /// the files the scenario edits are promoted before the first edit.
    let decl: EngineFactory =
        { Name = "decl"
          Create = graphEngine false (fun host spec -> ProjectGraph.Create(host, spec)) }

    /// ProjectGraph with every implementation file promoted, each check waiting until all of them are checked by
    /// declaration; not one of the checkpoint's engines.
    let declarations: EngineFactory =
        { Name = "declarations"
          Create = graphEngine true (fun host spec -> ProjectGraph.CreateWithMode(host, spec, Rfs.Checker.FileMode.Declarations)) }

    /// The fork's InteractiveChecker with its file-level signature cutoff, in the project's own type-checking mode: the
    /// cutoff exists only in graph mode, so the project is not forced sequential as the oracle is.
    let fileSlim: EngineFactory =
        { Name = "file-slim"
          Create =
            fun spec ->
                // The fork reads the variable once, when its checker module initialises.
                if Environment.GetEnvironmentVariable "FICKL_FCS_SIGNATURE_CUTOFF" = "0" then
                    failwith "file-slim measures the fork's signature cutoff, which FICKL_FCS_SIGNATURE_CUTOFF=0 turns off; unset it"

                Task.FromResult(new FileSlimEngine(spec) :> IEngine) }

    /// In the order the checkpoint runs them: the baseline first.
    let all = [ fileSlim; decl ]
