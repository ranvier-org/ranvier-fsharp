/// A ProjectGraph over the Deps fixture, edited in memory and compared with the oracle.
module Rfs.Checker.Tests.Deps

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
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

let fileOf (name: string) = Path.Combine(depsDir, name)

type Session =
    { Host: GraphHost
      Graph: ProjectGraph
      Spec: ProjectSpec
      Oracle: OracleClient
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

let start () : Task<Session> =
    task {
        let! spec = ProjectSpec.load depsProject
        let host = GraphHost.Start "deps"
        let! graph = ProjectGraph.Create(host, spec)
        let! _ = graph.AllDiagnostics() |> settle
        let! oracle = OracleClient.start ()
        let texts = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)

        for f in spec.SourceFiles do
            texts[f] <- File.ReadAllText f

        return
            { Host = host
              Graph = graph
              Spec = spec
              Oracle = oracle
              Texts = texts }
    }

type Edit =
    { /// How many times CheckDeclaration ran
      Delta: int64
      /// The newly published results, in order
      Checked: (DeclarationKey * string) list
      Diagnostics: IReadOnlyDictionary<string, FSharpDiagnostic[]> }

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

/// Sets the text of `file` and waits until every file is settled.
let edit (s: Session) (file: string) (text: string) : Task<Edit> =
    task {
        let events = ConcurrentQueue<DeclarationKey * string>()
        use _ = s.Graph.Checked |> Observable.subscribe events.Enqueue
        let before = s.Graph.CheckedCount
        s.Texts[file] <- text
        s.Graph.SetText(file, text)
        let! diagnostics = s.Graph.AllDiagnostics() |> settle

        return
            { Delta = s.Graph.CheckedCount - before
              Checked = List.ofSeq events
              Diagnostics = diagnostics }
    }

let replace (s: Session) (file: string) (oldText: string) (newText: string) =
    let text = s.Texts[file]

    if not (text.Contains oldText) then
        failtest $"%s{Path.GetFileName file} does not contain %s{oldText}"

    edit s file (text.Replace(oldText, newText))

/// The lines by which the graph's diagnostics differ from a full check of the current texts.
let oracleDiff (s: Session) (diagnostics: IReadOnlyDictionary<string, FSharpDiagnostic[]>) : Task<string list> =
    task {
        let! expected = s.Oracle.Check depsProject (s.Texts |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
        let actual = [ for kv in diagnostics do yield! kv.Value |> Seq.map FcsDiagnostics.ofFcs ]
        return Diagnostics.compare expected actual
    }

let expectOracle (s: Session) (diagnostics: IReadOnlyDictionary<string, FSharpDiagnostic[]>) (what: string) =
    task {
        let! diff = oracleDiff s diagnostics
        Expect.isEmpty diff $"%s{what}: diagnostics equal a full check:\n%s{String.concat Environment.NewLine diff}"
    }
