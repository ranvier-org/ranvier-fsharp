namespace Rfs.Checkpoint

open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open System.Threading.Tasks
open Rfs.Checker
open Rfs.Projects

/// The D1 premise breaks of a project checked by declaration from cold: each declaration whose final fingerprint
/// differs from the one its check left (ProjectGraph.PremiseBroken).
module Premise =

    /// Where two fingerprints first differ, as the index of the first differing '|'-separated part and the parts there
    /// (with up to `context` parts before it); None when they are equal.
    let firstDifference (context: int) (provisional: string) (final: string) =
        let a = provisional.Split '|'
        let b = final.Split '|'
        let n = min a.Length b.Length

        let at =
            match Seq.init n id |> Seq.tryFind (fun i -> a[i] <> b[i]) with
            | Some i -> Some i
            | None when a.Length <> b.Length -> Some n
            | None -> None

        at
        |> Option.map (fun i ->
            let part (parts: string[]) = if i < parts.Length then parts[i] else "(end)"
            let before = a[max 0 (i - context) .. i - 1] |> String.concat "|"
            i, before, part a, part b)

    /// One line per break: file:line, key, kind, and the first differing part of the provisional against the final
    /// fingerprint, grouped by kind and that difference.
    let markdown (corpus: string) (projectDir: string) (segmentRows: int) (breaks: PremiseBreak list) =
        let relative (file: string) = Path.GetRelativePath(projectDir, file).Replace('\\', '/')
        // A part can hold %A's line breaks
        let code (s: string) = "`" + s.Replace("`", "'").Replace("\r", "").Replace("\n", " ") + "`"

        let described =
            breaks
            |> List.sortBy (fun b -> relative b.File, b.Line)
            |> List.map (fun b ->
                let difference = firstDifference 3 b.Provisional b.Final
                let cause =
                    match difference with
                    | Some(_, _, p, f) -> $"%A{b.Key.Kind}: %s{code p} -> %s{code f}"
                    | None -> $"%A{b.Key.Kind}: equal"

                let name = String.concat "." (b.Key.ModulePath @ [ b.Key.Name ])

                let line =
                    match difference with
                    | Some(i, before, p, f) ->
                        $"- %s{relative b.File}:%d{b.Line} %s{code name} (%A{b.Key.Kind}): part %d{i} after %s{code before}: provisional %s{code p}, final %s{code f}"
                    | None -> $"- %s{relative b.File}:%d{b.Line} %s{code name} (%A{b.Key.Kind}): equal"

                cause, line)

        [ yield $"# D1 premise breaks: %s{corpus}"
          yield ""
          yield $"%d{breaks.Length} breaks; %d{segmentRows} segment rows after the cold check, every file checked by declaration."

          for cause, lines in described |> List.groupBy fst |> List.sortByDescending (snd >> List.length) do
              yield ""
              yield $"## %s{cause} (%d{lines.Length})"
              yield ""
              yield! lines |> List.map snd ]
        |> String.concat "\n"

    /// Checks the project with every file promoted and returns its breaks and the segment rows after that.
    let collect (spec: ProjectSpec) : Task<PremiseBreak list * int> =
        task {
            let host = GraphHost.Start "premise"

            try
                let! (graph: ProjectGraph) = ProjectGraph.CreateWithMode(host, spec, Rfs.Checker.FileMode.Declarations)
                let breaks = ConcurrentQueue<PremiseBreak>()
                use _ = graph.PremiseBroken |> Observable.subscribe breaks.Enqueue
                let! _ = graph.AllDiagnostics()

                for f in spec.SourceFiles do
                    let! _ = graph.WhenPromoted(f, CancellationToken.None)
                    ()

                let! _ = graph.AllDiagnostics()
                let failures = Array.append (host.TakePostErrors()) (host.TakePumpErrors())

                if failures.Length > 0 then
                    raise (AggregateException("the graph host reported errors", failures))

                if int64 breaks.Count <> graph.PremiseBreaks then
                    failwith $"%d{graph.PremiseBreaks} premise breaks counted, %d{breaks.Count} seen: one came before the subscription"

                return List.ofSeq breaks, graph.SegmentRowsTotal
            finally
                host.Dispose()
        }
