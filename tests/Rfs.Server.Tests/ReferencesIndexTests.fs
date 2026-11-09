module Rfs.Server.Tests.ReferencesIndexTests

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests.PublishTests

let private within (t: Task<'a>) = t.WaitAsync(TimeSpan.FromSeconds 110.)

/// A project of the given files in a directory of its own, with a scope over it alone.
let private withProjectOn (promote: bool) (files: (string * string) list) (body: GraphHost -> ProjectGraph -> (string -> string) -> References.Scope -> Task<unit>) : Task<unit> =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-refs-index-"

        try
            let path (name: string) = Path.Combine(dir.FullName, name)

            for name, text in files do
                File.WriteAllText(path name, text)

            let paths = files |> List.map (fst >> path)
            let! deps = ProjectSpec.load depsProject
            use host = GraphHost.Start "refs index"
            let! (g: ProjectGraph) = ProjectGraph.Create(host, { ProjectSpec.sequential deps with ProjectFile = path "P.fsproj"; SourceFiles = paths })

            if promote then
                for f in paths do
                    g.Promote f

            let! _ = g.AllDiagnostics() |> within

            if promote then
                for f in paths do
                    let! _ = g.WhenPromoted(f, CancellationToken.None) |> within
                    ()

            let scope: References.Scope =
                { OwnersOf = fun f -> if List.contains f paths then [ g ] else []
                  ReferencingProjects = fun _ -> []
                  Projects = fun () -> [ g ]
                  FilesOf = fun _ -> paths
                  IsLoading = fun () -> false }

            do! body host g path scope
            g.Dispose()
        finally
            try
                dir.Delete true
            with :? IOException | :? UnauthorizedAccessException ->
                ()
    }

let private withProject (promote: bool) (files: (string * string) list) (body: ProjectGraph -> (string -> string) -> References.Scope -> Task<unit>) : Task<unit> =
    withProjectOn promote files (fun _ g path scope -> body g path scope)

let private collected (refs: WeakReference list) =
    for _ in 1..3 do
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

    refs |> List.map (fun r -> not r.IsAlive)

/// Drops the graph's event log, which a Debug build of Ranvier (RANVIER_TRACE) keeps with every value its nodes
/// published: it would keep everything the graph ever computed alive. A no-op for an untraced Ranvier.
let private dropTrace (host: GraphHost) : Task =
    host.Read(fun () ->
        let graph = host.Graph

        let traceModule =
            [ "Ranvier.TraceModule"; "Ranvier.Trace" ]
            |> List.tryPick (fun name ->
                match typeof<Ranvier.Graph>.Assembly.GetType name with
                | null -> None
                | t -> Option.ofObj (t.GetMethod "lastSeq") |> Option.map (fun m -> t, m))

        match traceModule with
        | Some(t, lastSeq) ->
            let last = lastSeq.Invoke(null, [| box graph |])
            t.GetMethod("checkpoint").Invoke(null, [| box graph; last; box "dropped" |]) |> ignore
        | None -> ())

/// A weak reference to the file's published result, made off this state machine so no local of it holds the result
let private resultOf (g: ProjectGraph) (file: string) : WeakReference =
    Task.Run(fun () -> g.ReadFile(file, (fun view -> WeakReference view.Result), CancellationToken.None).GetAwaiter().GetResult()).Result

/// The same, through the file's index, which a read builds
let private indexedResultOf (g: ProjectGraph) (file: string) : WeakReference =
    Task.Run(fun () ->
        let read = g.ReadUses(file, CancellationToken.None).GetAwaiter().GetResult()
        WeakReference read.Result).Result

let private aText = "module Proj.A\n\ntype T = { F: int }\n\nlet shared = 1\n"
let private bText = "module Proj.B\n\nlet b = Proj.A.shared\n"
let private cText = "module Proj.C\n\nlet c = 2\n"
let private cUsing = "module Proj.C\n\nlet c = Proj.A.shared\n"
let private threeFiles = [ "A.fs", aText; "B.fs", bText; "C.fs", cText ]

/// `shared` at its definition, and `T` at its
let private sharedAt = (4, 5)
let private typeAt = (2, 5)

let private lines (name: string) (answer: References.Answer) =
    match answer with
    | References.Answer.Moved -> failtest "answered Moved"
    | References.Answer.Found uses ->
        uses
        |> List.filter (fun (struct (r, _)) -> r.FileName = name)
        |> List.map (fun (struct (r, _)) -> r.StartLine)
        |> List.sort

let private whenTrue (what: string) (seconds: float) (condition: unit -> bool) : Task<unit> =
    task {
        let clock = Diagnostics.Stopwatch.StartNew()

        while not (condition ()) do
            if clock.Elapsed.TotalSeconds > seconds then
                failtest $"never: %s{what}"

            do! Task.Delay 10
    }

[<Tests>]
let tests =
    testList
        "ReferencesIndex"
        [ testTask "a symbol written by name reads only the files that mention it, and later requests build nothing" {
              do!
                  withProject false threeFiles (fun g path scope ->
                      task {
                          let! (first: References.Answer) = References.find scope g (path "A.fs") (fst sharedAt) (snd sharedAt) CancellationToken.None
                          Expect.equal (lines (path "B.fs") first) [ 3 ] "B's use"
                          Expect.equal g.UsesBuilt 2L "A and B: C's text doesn't have the name"
                          let! (again: References.Answer) = References.find scope g (path "A.fs") (fst sharedAt) (snd sharedAt) CancellationToken.None
                          Expect.equal (lines (path "B.fs") again) [ 3 ] "the same"
                          Expect.equal g.UsesBuilt 2L "a later request is a lookup"
                      })
          }

          testTask "an unsure symbol reads every file" {
              do!
                  withProject false threeFiles (fun g path scope ->
                      task {
                          let! (answer: References.Answer) = References.find scope g (path "A.fs") (fst typeAt) (snd typeAt) CancellationToken.None
                          Expect.equal (lines (path "A.fs") answer) [ 3 ] "the type"
                          Expect.equal g.UsesBuilt 3L "every file"
                      })
          }

          // The read asks for the files at each turn it tries, on the graph thread: an edit posted from there lands
          // between that turn and the next, while the request waits
          testTask "an edit that brings a use into a skipped file during the request is in the answer" {
              do!
                  withProject false threeFiles (fun g path scope ->
                      task {
                          let c = path "C.fs"
                          let mutable asks = 0

                          let scope =
                              { scope with
                                  FilesOf =
                                      fun g' ->
                                          asks <- asks + 1
                                          if asks = 3 then g.SetText(c, cUsing) |> ignore
                                          scope.FilesOf g' }

                          let! (answer: References.Answer) = References.find scope g (path "A.fs") (fst sharedAt) (snd sharedAt) CancellationToken.None
                          Expect.isGreaterThanOrEqual asks 3 "the edit was made during the request"
                          Expect.equal (lines c answer) [ 3 ] "C's new use"
                      })
          }

          testTask "a use that came and went in a skipped file during the request isn't answered" {
              do!
                  withProject false threeFiles (fun g path scope ->
                      task {
                          let c = path "C.fs"
                          let mutable asks = 0

                          let scope =
                              { scope with
                                  FilesOf =
                                      fun g' ->
                                          asks <- asks + 1

                                          if asks = 3 then
                                              g.SetText(c, cUsing) |> ignore
                                              g.SetText(c, cText) |> ignore

                                          scope.FilesOf g' }

                          let! (answer: References.Answer) = References.find scope g (path "A.fs") (fst sharedAt) (snd sharedAt) CancellationToken.None
                          Expect.isGreaterThanOrEqual asks 3 "the edits were made during the request"
                          Expect.equal (lines c answer) [] "its last text has none"
                      })
          }

          testTask "a cancelled request ends its wait and leaves the build for the next" {
              do!
                  // Two declarations: a file of one is checked whole, with no lock to hold
                  withProject true [ "A.fs", aText; "B.fs", bText + "\nlet b2 = (None: Proj.A.T option)\n"; "C.fs", cText ] (fun g path scope ->
                      task {
                          let a, b = path "A.fs", path "B.fs"
                          let gate = new ManualResetEventSlim(false)
                          let entered = new ManualResetEventSlim(false)

                          // Holds B's lock, so its build waits there
                          let held =
                              g.ReadFile(
                                  b,
                                  (fun _ ->
                                      entered.Set()
                                      gate.Wait(TimeSpan.FromSeconds 60.) |> ignore),
                                  CancellationToken.None
                              )

                          let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
                          use cancelled = new CancellationTokenSource()
                          let pending = References.find scope g a (fst typeAt) (snd typeAt) cancelled.Token
                          do! whenTrue "A's index is built" 60. (fun () -> g.UsesBuilt = 1L)
                          do! Task.Delay 200
                          Expect.isFalse pending.IsCompleted "waits for B"
                          cancelled.Cancel()

                          let! outcome =
                              task {
                                  try
                                      let! (_: References.Answer) = pending.WaitAsync(TimeSpan.FromSeconds 20.)
                                      return "answered"
                                  with :? OperationCanceledException ->
                                      return "cancelled"
                              }

                          Expect.equal outcome "cancelled" "the request ended"
                          gate.Set()
                          let! _ = held |> within
                          do! whenTrue "B's build finishes with no request waiting" 60. (fun () -> g.UsesBuilt = 2L)
                          let! (answer: References.Answer) = References.find scope g a (fst typeAt) (snd typeAt) CancellationToken.None
                          Expect.equal (lines a answer) [ 3 ] "answers"
                          Expect.equal g.UsesBuilt 3L "B's wasn't built again; only C's was left"
                      })
          }

          testTask "a file leaving the project during a wait ends the wait, and answers from the files now" {
              do!
                  withProject true [ "A.fs", aText; "B.fs", bText + "\nlet b2 = (None: Proj.A.T option)\n"; "C.fs", cText ] (fun g path scope ->
                      task {
                          let a, b, c = path "A.fs", path "B.fs", path "C.fs"
                          let gate = new ManualResetEventSlim(false)
                          let entered = new ManualResetEventSlim(false)

                          // Holds B's lock, so its build waits there
                          let held =
                              g.ReadFile(
                                  b,
                                  (fun _ ->
                                      entered.Set()
                                      gate.Wait(TimeSpan.FromSeconds 60.) |> ignore),
                                  CancellationToken.None
                              )

                          let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
                          let mutable files = [ a; b; c ]
                          let scope = { scope with FilesOf = fun _ -> files }
                          let pending = References.find scope g a (fst typeAt) (snd typeAt) CancellationToken.None
                          do! whenTrue "A's index is built" 60. (fun () -> g.UsesBuilt = 1L)
                          do! Task.Delay 200
                          Expect.isFalse pending.IsCompleted "waits for B"
                          files <- [ a; c ]
                          // Anything that reaches the graph thread runs the waiting read again
                          let! _ = g.Mode a

                          let! outcome =
                              task {
                                  try
                                      let! (answer: References.Answer) = pending.WaitAsync(TimeSpan.FromSeconds 20.)
                                      return Some answer
                                  with :? TimeoutException ->
                                      return None
                              }

                          gate.Set()
                          let! _ = held |> within
                          Expect.isSome outcome "the wait ended"
                          Expect.equal (lines a outcome.Value) [ 3 ] "the type, from the files the project has now"
                          Expect.equal (lines b outcome.Value) [] "nothing of the file that left"
                      })
          }

          testTask "a build finishes during continuous edits of an earlier file" {
              let rows = String.Join("\n", [ for i in 0..3000 -> $"let v%d{i} = %d{i} + 1" ])
              let late = $"module Proj.Late\n\n%s{rows}\n"
              let early (n: int) = $"module Proj.Early\n\nlet e = {n}\n"

              do!
                  withProject false [ "Early.fs", early 0; "Late.fs", late ] (fun g path _ ->
                      task {
                          let mutable n = 0
                          let reading = g.ReadUses(path "Late.fs", CancellationToken.None)
                          let clock = Diagnostics.Stopwatch.StartNew()

                          while g.UsesBuilt = 0L && clock.Elapsed.TotalSeconds < 90. do
                              n <- n + 1
                              g.SetText(path "Early.fs", early n) |> ignore
                              do! Task.Delay 20

                          Expect.equal g.UsesBuilt 1L $"built once, after %d{n} edits"
                          Expect.isGreaterThan n 2 "edits ran during the build"
                          let! _ = g.AllDiagnostics() |> within
                          let! (read: UsesRead) = reading |> within
                          Expect.isGreaterThan read.Uses.Keys 3000 "it has the file's uses"
                      })
          }

          testTask "a superseded result is collected after its index was read, as one no index was read of is" {
              do!
                  withProjectOn false threeFiles (fun host g path _ ->
                      task {
                          let b, c = path "B.fs", path "C.fs"
                          let indexed = indexedResultOf g b
                          let control = resultOf g c

                          for n in 1..2 do
                              g.SetText(b, bText + $"\nlet more = %d{n}\n") |> ignore
                              g.SetText(c, cText + $"\nlet more = %d{n}\n") |> ignore
                              let! _ = g.AllDiagnostics() |> within
                              ()

                          do! dropTrace host
                          Expect.equal (collected [ control ]) [ true ] "the control: a result nothing indexed is collected"
                          Expect.equal (collected [ indexed ]) [ true ] "the indexed result is collected too"
                          let! (read: UsesRead) = g.ReadUses(b, CancellationToken.None) |> within
                          Expect.isGreaterThan read.Uses.Keys 0 "and the index answers for the new result"
                      })
          }

          testTask "demoting a file after a request releases its declaration state, as for a file no index was read of" {
              let twoRows (t: string) = t + "\nlet second = 2\n"

              do!
                  withProjectOn true [ "A.fs", aText; "B.fs", twoRows bText; "C.fs", twoRows cText ] (fun host g path _ ->
                      task {
                          let b, c = path "B.fs", path "C.fs"
                          indexedResultOf g b |> ignore
                          let! (indexed: WeakReference list) = g.DeclarationObjects b |> within
                          let! (control: WeakReference list) = g.DeclarationObjects c |> within
                          Expect.equal indexed.Length 3 "B's declaration state"
                          Expect.equal control.Length 3 "C's declaration state"

                          for f in [ b; c ] do
                              g.Demote f
                              g.SetText(f, twoRows (if f = b then bText else cText) + "\nlet third = 3\n") |> ignore

                          let! _ = g.AllDiagnostics() |> within
                          do! dropTrace host
                          Expect.equal (collected control) [ true; true; true ] "the control: C's declaration state is collected"
                          Expect.equal (collected indexed) [ true; true; true ] "B's too, its index read before"
                      })
          }

          testTask "the index is plain data: nothing in it reaches a compiler object" {
              do!
                  withProject false threeFiles (fun g path _ ->
                      task {
                          let! (read: UsesRead) = g.ReadUses(path "B.fs", CancellationToken.None) |> within
                          let seen = HashSet<obj>(HashIdentity.Reference)

                          let rec walk (o: obj) =
                              if not (isNull o) && seen.Add o then
                                  let t = o.GetType()

                                  if not t.IsArray && t.Namespace <> null && t.Namespace.StartsWith "FSharp.Compiler" && t <> typeof<FSharp.Compiler.Text.range> then
                                      failtest $"the index reaches %s{t.FullName}"

                                  match o with
                                  | :? string -> ()
                                  | :? Array as a when not (t.GetElementType().IsPrimitive) ->
                                      for item in a do
                                          walk item
                                  | _ when t.IsPrimitive || t = typeof<FSharp.Compiler.Text.range> -> ()
                                  | _ ->
                                      for f in t.GetFields(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) do
                                          if not f.FieldType.IsPointer then
                                              walk (f.GetValue o)

                          walk read.Uses
                          Expect.isGreaterThan seen.Count 5 "it walked the index"
                          Expect.isTrue (obj.ReferenceEquals(read.Uses.Source.Target, read.Result)) "the source is known only weakly"
                      })
          } ]
