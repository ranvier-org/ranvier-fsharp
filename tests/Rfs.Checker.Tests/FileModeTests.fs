module Rfs.Checker.Tests.FileModeTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Checker
open Rfs.Checker.Tests.Deps
open Rfs.Harness
open Rfs.Projects

let private a = fileOf "A.fs"
let private b = fileOf "B.fs"
let private d = fileOf "D.fs"
let private e = fileOf "E.fs"

let private name (file: string) = Path.GetFileName file

/// Runs `body` on a session, then disposes it.
let private withSession (session: Task<Session>) (body: Session -> Task) =
    task {
        let! s = session
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }
        do! body s
    }

/// Runs `body` on a fresh file-level graph over Deps, settled on the files as they are on disk.
let private withDeps (body: Session -> Task) = withSession (start ()) body

/// Each source file's dependencies as FileDependencies gives them for the session's current texts.
let private dependencies (s: Session) : Map<string, string list> =
    let files = Array.ofList s.Spec.SourceFiles
    let parses = files |> Array.map (fun f -> DeclarationChecker.Parse(s.Fresh, f, s.Texts[f]))
    let lists = DeclarationChecker.FileDependencies(s.Fresh, parses)
    Array.zip files lists |> Array.map (fun (f, deps) -> f, [ for i in deps -> files[i] ]) |> Map.ofArray

let private implementationFiles (s: Session) = s.Spec.SourceFiles |> List.filter (fun f -> f.EndsWith ".fs")

/// The implementation files a signature change of `file` re-checks: the file and those depending on it.
let private recheckedBy (s: Session) (file: string) =
    let deps = dependencies s
    implementationFiles s |> List.filter (fun f -> f = file || List.contains file deps[f])

/// Applies an edit and returns how many CheckFile calls it took.
let private counted (s: Session) (work: unit -> Task<Edit>) =
    task {
        let before = s.Graph.FileLevelChecks
        let! r = work ()
        return r, s.Graph.FileLevelChecks - before
    }

/// Changes the type of D.fs's `after`; no other implementation file depends on D.fs in the fixture as it is
let private afterType = "let after = 42", "let after : int64 = 42L"

let private c = fileOf "C.fs"

/// Promotes the files and waits until promoting each has settled: the mode each settled in.
let private promoteAs (s: Session) (files: string list) =
    task {
        for f in files do
            s.Graph.Promote f

        let modes = ResizeArray()

        for f in files do
            let! mode = s.Graph.WhenPromoted(f, CancellationToken.None) |> settle
            modes.Add mode

        return List.ofSeq modes
    }

/// Promotes the files and waits until each is checked by declaration for its latest text.
let private promote (s: Session) (files: string list) =
    task {
        let! modes = promoteAs s files
        Expect.equal modes (files |> List.map (fun _ -> Declarations)) $"promoted: %A{List.map name files}"
    }

/// Whether each weakly held object is collected after full collections.
let private collected (refs: WeakReference list) =
    for _ in 1..3 do
        GC.Collect()
        GC.WaitForPendingFinalizers()
        GC.Collect()

    refs |> List.map (fun r -> not r.IsAlive)

/// Drops the graph's event log, which a Debug build of Ranvier (RANVIER_TRACE) keeps with every value its nodes
/// published: it would keep everything the graph ever computed alive. A no-op for an untraced Ranvier.
let private dropTrace (s: Session) : Task =
    s.Host.Read(fun () ->
        let graph = s.Host.Graph
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

let private sigUnmatchedProject = Path.Combine(Path.GetDirectoryName depsDir, "SigUnmatched", "SigUnmatched.fsproj")

let private expectMode (s: Session) (file: string) (expected: FileMode) (what: string) =
    task {
        let! mode = s.Graph.Mode file
        Expect.equal mode expected $"%s{name file}: %s{what}"
    }

/// The edit's CheckDeclaration runs, by file name
let private ranFiles (r: Edit) = r.Ran |> List.map (snd >> name) |> List.distinct |> List.sort

/// A body whose default fails, which makes the file's whole check fail, before a generic lambda (C1 of the Task 5 review)
let private failingDefault =
    "\n\nlet scaleDefaults (x: int) : int = x * 2\n\nlet areaDefaults (r: int) : int = 3 * scaleDefaults * r\n\nlet gDefaults () =\n    (fun x -> x + 1.0)\n    1\n"

let private recProject = Path.Combine(Path.GetDirectoryName depsDir, "Rec", "Rec.fsproj")

[<Tests>]
let tests =
    testSequenced
    <| testList
        "FileMode"
        [ testTask "cold check is file-level" {
              let! (spec: ProjectSpec) = ProjectSpec.load depsProject
              let host = GraphHost.Start "cold"
              let! (oracle: OracleClient) = OracleClient.start ()

              try
                  let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec)
                  let! (all: Collections.Generic.IReadOnlyDictionary<string, FSharp.Compiler.Diagnostics.FSharpDiagnostic[]>) = graph.AllDiagnostics() |> settle

                  for f in spec.SourceFiles do
                      let! mode = graph.Mode f
                      Expect.equal mode FileLevel $"%s{name f} starts file-level"

                  let implementations = spec.SourceFiles |> List.filter (fun f -> f.EndsWith ".fs")
                  Expect.equal graph.FileLevelChecks (int64 implementations.Length) "one CheckFile per implementation file"
                  Expect.equal graph.CheckedCount 0L "no declaration is checked on its own"
                  Expect.equal graph.ChainFallbacks 0L "every setup is the scoped one"
                  Expect.equal graph.Context.CheckResultsBuilt 0 "the cold check builds no sink and no check results"
                  let! expected = oracle.Check depsProject Map.empty
                  let actual = [ for kv in all do yield! kv.Value |> Seq.map FcsDiagnostics.ofFcs ]
                  Expect.isEmpty (Rfs.Lsp.Diagnostics.compare expected actual) "diagnostics equal a full check"

                  let! (failure: exn option) =
                      task {
                          try
                              let! _ = graph.Results a |> settle
                              return None
                          with ex ->
                              return Some ex
                      }

                  Expect.isTrue (failure |> Option.exists (fun ex -> ex :? InvalidOperationException)) $"a file-level file has no declaration results: %A{failure}"
              finally
                  host.Dispose()
                  (oracle :> IAsyncDisposable).DisposeAsync().AsTask().Wait()
          }

          testTask "unrelated file is not re-checked" {
              do!
                  withDeps (fun s ->
                      task {
                          Expect.equal (recheckedBy s d) [ d ] "no implementation file depends on D.fs"
                          let! r, checks = counted s (fun () -> replace s d (fst afterType) (snd afterType))
                          Expect.equal checks 1L "D.fs only: E.fs, after it, does not depend on it"
                          do! expectOracle s r.Diagnostics "after is an int64"
                      })
          }

          testTask "adding a module changes dependencies" {
              do!
                  withDeps (fun s ->
                      task {
                          Expect.isFalse (List.contains d (dependencies s).[e]) "E.fs does not use Deps.D yet"
                          let! r, checks = counted s (fun () -> replace s e "let hidden = 1" "open Deps.D\n\nlet hidden = 1")
                          Expect.equal checks 1L "E.fs re-checks; its signature is E.fsi's, so nothing else does"
                          do! expectOracle s r.Diagnostics "E.fs opens Deps.D"
                          Expect.isTrue (List.contains d (dependencies s).[e]) "E.fs now depends on D.fs"
                          Expect.equal (recheckedBy s d) [ d; e ] "E.fs is D.fs's dependent"
                          let! r, checks = counted s (fun () -> replace s d (fst afterType) (snd afterType))
                          Expect.equal checks 2L "D.fs, and E.fs, which now opens it"
                          do! expectOracle s r.Diagnostics "after is an int64, E.fs opens Deps.D"
                      })
          }

          testTask "settled diagnostics carry the version" {
              do!
                  withDeps (fun s ->
                      task {
                          let first = s.Texts[a].Replace("let baseValue = 10", "let baseValue = 11")
                          let second = s.Texts[a].Replace("let baseValue = 10", "let baseValue : string = 10")
                          let v1 = s.Graph.SetText(a, first)
                          let v2 = s.Graph.SetText(a, second)
                          s.Texts[a] <- second
                          Expect.isGreaterThan v2 v1 "each SetText posts a later version"
                          let! struct (version, diagnostics) = s.Graph.SettledDiagnostics a |> settle
                          Expect.equal version v2 "settled at the second version"
                          Expect.isNonEmpty diagnostics "the second text's error"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          Expect.equal (Array.map string diagnostics) (Array.map string all[a]) "the diagnostics of that version"
                          do! expectOracle s all "baseValue is a string"
                      })
          }

          testTask "unresolved open falls back to the chain" {
              do!
                  withDeps (fun s ->
                      task {
                          let! r, checks = counted s (fun () -> replace s b "open Deps.A" "open Deps.A.Nope")
                          Expect.equal (dependencies s).[b] [] "B.fs no longer depends on A.fs: its setup lacks Deps.A"
                          Expect.equal checks 2L "B.fs is checked from its dependencies, then from every earlier file; its signature is unchanged"
                          Expect.isTrue (r.Diagnostics[b] |> Array.exists (fun x -> x.Message.Contains "'Nope'")) "Nope is what is not defined"
                          do! expectOracle s r.Diagnostics "open Deps.A.Nope"
                      })
          }

          testTask "an edit in one chain does not re-check the other" {
              do! withSession (startOn chainsProject FileLevel) (fun s -> task {
              let chain name = Path.Combine(Path.GetDirectoryName chainsProject, name)
              let deps = dependencies s
              Expect.equal deps[chain "D.fs"] [ chain "C.fs" ] "D.fs depends on C.fs only"
              Expect.equal deps[chain "C.fs"] [] "C.fs depends on nothing"
              Expect.equal (recheckedBy s (chain "A.fs")) [ chain "A.fs"; chain "B.fs" ] "only B.fs depends on A.fs"
              let! r, checks = counted s (fun () -> replace s (chain "A.fs") "let scale (x: int) : int = x * 2" "let scale (x: int) : int64 = int64 (x * 2)")
              Expect.equal checks 2L "A.fs and B.fs; C.fs and D.fs are not re-checked"
              Expect.isNonEmpty r.Diagnostics[chain "B.fs"] "B.fs sees scale's new type"
              do! expectOracle s r.Diagnostics "scale returns int64"
              })
          }

          testTask "stale snapshot falls back to the chain" {
              do!
                  withDeps (fun s ->
                      task {
                          let mutable fired = 0
                          // A newer parse of D.fs lands while E.fs is being checked: the dependency snapshot E.fs was set up
                          // from is no longer current for the files before it
                          s.Graph.OnScopedCheck(fun path ->
                              if String.Equals(path, e, StringComparison.OrdinalIgnoreCase) && Interlocked.Exchange(&fired, 1) = 0 then
                                  DeclarationChecker.Parse(s.Graph.Context, d, s.Texts[d]) |> ignore)

                          let! r = replace s e "let unrelated = 7" "let unrelated = 8"
                          Expect.equal fired 1 "the hook ran"
                          Expect.isGreaterThan s.Graph.ChainFallbacks 0L "the refused setup fell back to the chain"
                          do! expectOracle s r.Diagnostics "after the stale snapshot"
                      })
          }

          testTask "a check refused for another parse's line directives waits for its own" {
              do!
                  withDeps (fun s ->
                      task {
                          let mutable fired = 0
                          let mutable restored: Task = Task.CompletedTask

                          // Another context parses E.fs with a #line directive just before E.fs is checked, and parses
                          // a directive-free text again a moment later, as DeclarationLines' second context does
                          s.Graph.OnScopedCheck(fun path ->
                              if String.Equals(path, e, StringComparison.OrdinalIgnoreCase) && Interlocked.Exchange(&fired, 1) = 0 then
                                  let text = s.Texts[e]
                                  DeclarationChecker.Parse(s.Fresh, e, "# 1 \"elsewhere.fsy\"\n" + text) |> ignore

                                  restored <-
                                      Task.Run(fun () ->
                                          Thread.Sleep 300
                                          DeclarationChecker.Parse(s.Fresh, e, text) |> ignore))

                          // A warning in E.fs, which the directive would move out of the project
                          let! r = replace s e "let unrelated = 7" "let unrelated = 7\n\nlet pair = [ 1, 2 ]"
                          do! restored
                          Expect.equal fired 1 "the hook ran"
                          Expect.exists r.Diagnostics[e] (fun d -> d.ErrorNumber = 3886) "E.fs reports its warning"
                          Expect.isGreaterThan s.Graph.DirectiveRetries 0L "the check was refused and tried again"
                          do! expectOracle s r.Diagnostics "after the other context's parse"
                      })
          }

          // C1 and C2 of the Task 3b re-review: setups from a signature file without its implementation file
          testTask "signature-only setups equal the oracle, cold and after a header edit" {
              let fixture name = Path.Combine(Path.GetDirectoryName depsDir, name, name + ".fsproj")

              for name in [ "SigShadow"; "SigShadowNs"; "SigShadowExt" ] do
                  do!
                      withSession (startOn (fixture name) FileLevel) (fun s ->
                          task {
                              let! all = s.Graph.AllDiagnostics() |> settle
                              do! expectOracle s all $"%s{name} cold"
                          })

              do!
                  withSession (startOn (fixture "SigQualName") FileLevel) (fun s ->
                      task {
                          let x = s.Spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "X.fs")
                          let y = s.Spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "Y.fs")
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "SigQualName cold"

                          for header, text in [ "module Ab", "module Ab\n\nlet x = 1\n"; "no header", "let x = 1\n"; "back", "module A\n\nlet x = 1\n" ] do
                              let! r = edit s x text
                              do! expectOracle s r.Diagnostics $"X.fs: %s{header}"

                              if header <> "back" then
                                  Expect.exists r.Diagnostics[y] (fun d -> d.ErrorNumber = 39) $"%s{header}: Y.fs reports A as not defined"
                      })
          }

          // Task 4: promotion
          testTask "promote then body edit re-checks one declaration" {
              do!
                  withDeps (fun s ->
                      task {
                          do! promote s [ a ]
                          do! expectMode s a Declarations "promoted"
                          let! r, checks = counted s (fun () -> replace s a "x * 2" "x * 3")
                          Expect.equal r.Delta 1L "one CheckDeclaration adopted: scale"
                          Expect.equal (r.NamesIn a) [ "scale" ] "only scale is published again"
                          Expect.equal checks 0L "no file is checked whole"
                          do! expectMode s a Declarations "still promoted"
                          do! expectOracle s r.Diagnostics "body edit of a promoted file"
                      })
          }

          testTask "signature edit restarts dependents file-level" {
              do!
                  withDeps (fun s ->
                      task {
                          do! promote s [ a ]
                          let dependents = recheckedBy s a |> List.filter ((<>) a)
                          Expect.isNonEmpty dependents "files depend on A.fs"

                          let! r, checks =
                              counted s (fun () -> replace s a "let scale (x: int) : int = x * 2" "let scale (x: int) : int64 = int64 (x * 2)")

                          do! expectMode s a Declarations "the edited file keeps its setup"
                          Expect.equal (r.NamesIn a) [ "doubled"; "scale" ] "scale and its user in A.fs"
                          Expect.equal (ranFiles r) [ "A.fs" ] "no declaration of another file is checked"
                          Expect.isGreaterThanOrEqual checks (int64 dependents.Length) $"each dependent is checked whole: %A{List.map name dependents}"

                          for f in dependents do
                              do! expectMode s f FileLevel "not wanted, checked whole"

                          do! expectOracle s r.Diagnostics "scale returns int64"
                      })
          }

          testTask "restarted promoted file comes back" {
              do!
                  withDeps (fun s ->
                      task {
                          Expect.contains (dependencies s).[c] a "C.fs depends on A.fs"
                          do! promote s [ a; c ]
                          do! expectMode s c Declarations "promoted"
                          // C.fs's build after the restart waits here, so its file-level state can be seen
                          use release = new ManualResetEventSlim(false)
                          let mutable armed = 1

                          s.Graph.OnBeginFile(fun path ->
                              if String.Equals(path, c, StringComparison.OrdinalIgnoreCase) && Volatile.Read &armed = 1 then
                                  release.Wait(TimeSpan.FromMinutes 2.) |> ignore)

                          let! r =
                              editWith false s a (s.Texts[a].Replace("let scale (x: int) : int = x * 2", "let scale (x: int) : int64 = int64 (x * 2)"))

                          do! expectMode s c FileLevel "its setup changed: checked whole until it is built again"
                          do! expectOracle s r.Diagnostics "C.fs checked whole"
                          Volatile.Write(&armed, 0)
                          release.Set()
                          let! mode = s.Graph.WhenPromoted(c, CancellationToken.None) |> settle
                          Expect.equal mode Declarations "C.fs settles by declaration"
                          do! expectMode s c Declarations "built again for the new setup"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "C.fs promoted again"
                      })
          }

          testTask "module rec file stays file-level" {
              do!
                  withSession (startOn recProject FileLevel) (fun s ->
                      task {
                          let r = s.Spec.SourceFiles |> List.find (fun f -> name f = "R.fs")
                          let! modes = promoteAs s [ r ]
                          Expect.equal modes [ FileLevel ] "promoting it settles file-level (M1)"
                          do! expectMode s r FileLevel "a module rec file splits into one declaration"
                          Expect.equal s.Graph.DeclarationRoots 0L "no declaration nodes are created for it (M7)"
                          let! e, checks = counted s (fun () -> replace s r "let start = 10" "let start = 11")
                          Expect.equal e.Delta 0L "no declaration is checked"
                          Expect.isGreaterThan checks 0L "checked whole"
                          do! expectMode s r FileLevel "still file-level"
                          do! expectOracle s e.Diagnostics "start = 11"
                          let! e = replace s r "let start = 11" "let start : string = 11"
                          do! expectOracle s e.Diagnostics "start is a string"
                          Expect.equal s.Graph.DeclarationRoots 0L "still none after its edits"
                      })
          }

          testTask "typing during promotion" {
              do!
                  withDeps (fun s ->
                      task {
                          s.Graph.Promote a
                          let original = s.Texts[a]

                          let texts =
                              [ for i in 1..5 ->
                                    if i % 2 = 0 then original.Replace("let baseValue = 10", $"let baseValue : string = %d{i}")
                                    else original.Replace("let baseValue = 10", $"let baseValue = %d{i}") ]

                          let waits =
                              [ for text in texts do
                                    s.Texts[a] <- text
                                    let version = s.Graph.SetText(a, text)
                                    yield version, s.Graph.SettledDiagnostics a ]

                          let! results = Task.WhenAll(waits |> List.map snd) |> settle
                          let versions = [ for struct (v, _) in results -> v ]
                          Expect.equal versions (List.sort versions) $"the settled versions, in the order asked, rise: %A{versions}"

                          for (posted, _), struct (v, _) in List.zip waits (List.ofArray results) do
                              Expect.isGreaterThanOrEqual v posted "a settled version is never older than the one posted before the wait"

                          let struct (last, diagnostics) = Array.last results
                          Expect.equal last (fst (List.last waits)) "the last settles at the last version"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          Expect.equal (Array.map string diagnostics) (Array.map string all[a]) "the last text's diagnostics"
                          do! expectOracle s all "the last text"
                          let! mode = s.Graph.WhenPromoted(a, CancellationToken.None) |> settle
                          Expect.equal mode Declarations "A.fs settles by declaration"
                          do! expectMode s a Declarations "promoted for the last text"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "the last text, by declaration"
                      })
          }

          testTask "demote drops declaration results" {
              do!
                  withDeps (fun s ->
                      task {
                          do! promote s [ a ]
                          // Counted in a task of its own, so the test keeps no declaration result alive
                          let! count = task { let! results = s.Graph.Results a |> settle in return results.Count }
                          Expect.isGreaterThan count 0 "declaration results while promoted"
                          let! held = s.Graph.DeclarationObjects a
                          Expect.equal held.Length 3 "a declaration result, its environment and its FileStart"
                          s.Graph.Demote a
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "the kept result after demotion"
                          do! expectMode s a FileLevel "demoted"
                          // I1: only the last FileResult is kept. A Debug Ranvier (RANVIER_TRACE) logs every value its
                          // nodes publish, so its log is dropped first.
                          do! dropTrace s
                          let gone = collected held
                          Expect.equal gone [ true; true; true ] $"the declaration result, its environment and its FileStart are collected: %A{gone}"

                          let! (failure: exn option) =
                              task {
                                  try
                                      let! _ = s.Graph.Results a |> settle
                                      return None
                                  with ex ->
                                      return Some ex
                              }

                          Expect.isTrue (failure |> Option.exists (fun ex -> ex :? InvalidOperationException)) $"no declaration results: %A{failure}"
                          let! r, checks = counted s (fun () -> replace s a "x * 2" "x * 3")
                          Expect.equal checks 1L "the next edit checks A.fs whole"
                          Expect.equal r.Delta 0L "and no declaration"
                          do! expectOracle s r.Diagnostics "body edit after demotion"
                      })
          }

          // M5: a build that fails leaves the builder's place to the files after it
          testTask "a failed build does not hold up later files" {
              do!
                  withDeps (fun s ->
                      task {
                          let mutable failing = 1

                          s.Graph.OnBeginFile(fun path ->
                              if String.Equals(path, a, StringComparison.OrdinalIgnoreCase) && Volatile.Read &failing = 1 then
                                  failwith "BeginFile failed (test)")

                          let! modes = promoteAs s [ a; c ]
                          Expect.equal modes [ FileLevel; Declarations ] "A.fs's build failed, C.fs, after it, is built"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "A.fs checked whole"
                          // A new text of A.fs builds it again
                          Volatile.Write(&failing, 0)
                          let! r = replace s a "x * 2" "x * 3"
                          do! expectMode s a Declarations "built for its new text"
                          do! expectOracle s r.Diagnostics "A.fs promoted after its text changed"
                      })
          }

          // M4: one build at a time, across edits that leave an earlier file's state pending
          testTask "builds never overlap" {
              do!
                  withDeps (fun s ->
                      task {
                          // The files whose BeginFile is running, with how many flights of each: a file's superseded
                          // flight may still be running beside its new one, another file's never may
                          let running = Collections.Generic.Dictionary<string, int>()
                          let mutable most = 0
                          let log = Collections.Concurrent.ConcurrentQueue<string>()

                          s.Graph.OnBeginFile(fun path ->
                              lock running (fun () ->
                                  running[path] <- (match running.TryGetValue path with | true, n -> n | _ -> 0) + 1
                                  most <- max most running.Count)

                              log.Enqueue $"+%s{name path}"
                              Thread.Sleep 50
                              log.Enqueue $"-%s{name path}"

                              lock running (fun () ->
                                  if running[path] = 1 then running.Remove path |> ignore else running[path] <- running[path] - 1))

                          let files = implementationFiles s

                          for f in files do
                              s.Graph.Promote f

                          // Edits to the earlier files while the later ones are being built
                          for i in 1..4 do
                              let! _ = editWith false s a (s.Texts[a].Replace($"let baseValue = %d{9 + i}", $"let baseValue = %d{10 + i}"))
                              ()

                          do! whenPromoted s.Graph s.Spec
                          Expect.equal most 1 $"one file's BeginFile at a time: %A{List.ofSeq log}"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "every file promoted"
                      })
          }

          // M6: a declaration whose name is undefined where it is used, though defined later in the file, re-checks when
          // an earlier declaration brings a nearer name into scope under the same key
          testTask "suggestions follow an earlier declaration that keeps its key" {
              do!
                  withSession (startOn chainsProject Declarations) (fun s ->
                      task {
                          let bFile = Path.Combine(Path.GetDirectoryName chainsProject, "B.fs")

                          // Defined later in the file, as the review's scenario has it, and defined nowhere
                          for later, what in [ "\nlet shifted = 1\n", "defined later"; "", "defined nowhere" ] do
                              let text = "module Chains.B\n\ntype Marker =\n    | First\n\nlet added = shifted\n" + later
                              let! r = edit s bFile text
                              do! expectOracle s r.Diagnostics $"%s{what}: shifted is not defined where it is used"
                              let! r = edit s bFile (text.Replace("| First", "| Shifted"))
                              Expect.exists r.Diagnostics[bFile] (fun d -> d.ErrorNumber = 39 && d.Message.Contains "Shifted") $"%s{what}: the suggestion names the new case"
                              do! expectOracle s r.Diagnostics $"%s{what}: Marker gains a case like the undefined name"
                              let! r = edit s bFile text
                              do! expectOracle s r.Diagnostics $"%s{what}: and loses it again"

                          // A pattern that keeps its first name, and so its key, and binds a nearer one
                          let text = "module Chains.B\n\nlet first, other = 1, 2\n\nlet added = shifted\n"
                          let! r = edit s bFile text
                          do! expectOracle s r.Diagnostics "a tuple binding before the undefined name"
                          let! r = edit s bFile (text.Replace("first, other", "first, shiftedd"))
                          Expect.exists r.Diagnostics[bFile] (fun d -> d.ErrorNumber = 39 && d.Message.Contains "shiftedd") "the suggestion names the new value"
                          do! expectOracle s r.Diagnostics "the binding gains a name like the undefined one"
                      })
          }

          // M3: the Declarations fuzz's Chains seed 1515 step 19, at small size. The texts its first 18 steps left, then
          // B.fs's literal: at Task 4 the graph settled here only with an effect observing every file's signature (a lost
          // wake-up). It no longer needs one, and the test no longer fails on a Ranvier without #303.
          testTask "a literal edit after a run of restarts settles" {
              do!
                  withSession (startOn chainsProject Declarations) (fun s ->
                      task {
                          let file name = Path.Combine(Path.GetDirectoryName chainsProject, name)

                          let texts =
                              [ "A.fs", "module Chains.A\nlet added2 = \"text\"\nlet baseValue = 10\nlet added1 = counted\n\n\nlet doubled = shift baseValue\n"
                                "B.fs", "module Chains.B\n\nopen Chains.A\n\n\nlet counted = twice baseValue + 1\n"
                                "C.fs", "module Chains.C\n\n\nlet baseValue = 76\nlet shift (x: int) : int = x + offset\n\n"
                                "D.fs", "module Chains.D\n\nopen Chains.C\n\nlet shiftTwice (x: int) : int = shift (shift x)\n\nlet total = shiftTwice offset + 1\n" ]

                          for name, text in texts do
                              let! r = edit s (file name) text
                              do! expectOracle s r.Diagnostics $"%s{name} as step 18 left it"

                          let b = file "B.fs"
                          let! r = edit s b (s.Texts[b].Replace("+ 1", "+ 66"))
                          do! expectOracle s r.Diagnostics "B.fs's literal"
                      })
          }

          // M9: the T4-C3 header transition through the graph, in both modes
          testTask "an implementation file becoming unmatched by its signature file, through the graph" {
              for mode in [ FileLevel; Declarations ] do
                  do!
                      withSession (startOn sigUnmatchedProject mode) (fun s ->
                          task {
                              let x = s.Spec.SourceFiles |> List.find (fun f -> name f = "X.fs")
                              let! all = s.Graph.AllDiagnostics() |> settle
                              do! expectOracle s all $"%A{mode} cold (unmatched)"

                              for header, text in
                                  [ "module A", "module A\n\nlet x = 1\nlet f = \"s\"\n"
                                    "AutoOpen Ab", "[<AutoOpen>]\nmodule Ab\n\nlet f = \"s\"\n"
                                    "module A again", "module A\n\nlet x = 1\nlet f = \"s\"\n" ] do
                                  let! r = edit s x text
                                  do! expectOracle s r.Diagnostics $"%A{mode}: X.fs is %s{header}"
                          })
          }

          // M2 (Task 5): CheckFile and FinishFile give a file with compiler-generated names one fingerprint, so a restart
          // of a promoted file changes what the files after it see once, not again when it is built by declaration
          testTask "an upstream signature edit re-checks the dependents of a promoted file with generated names once" {
              do!
                  withDeps (fun s ->
                      task {
                          let holders =
                              "\ntype Holder1() as this =\n    let value = baseValue\n    member _.V = value\n    member _.Self = this\n"
                              + "\ntype Holder2() as this =\n    let value = baseValue\n    member _.V = value\n    member _.Self = this\n"
                              + "\nlet adder = fun (x: int) -> x + 1\n"

                          let! _ = edit s b (s.Texts[b] + holders)
                          Expect.contains (dependencies s).[c] b "C.fs depends on B.fs"
                          do! promote s [ b ]
                          let mutable checksOfC = 0

                          s.Graph.OnScopedCheck(fun path ->
                              if String.Equals(path, c, StringComparison.OrdinalIgnoreCase) then
                                  Interlocked.Increment &checksOfC |> ignore)

                          // B.fs's Start and Holders take baseValue's type, so B.fs's signature changes with it
                          let! r = replace s a "let baseValue = 10" "let baseValue = 10L"
                          do! expectMode s b Declarations "B.fs is built again for its new setup"
                          Expect.equal (Volatile.Read &checksOfC) 1 "C.fs is checked once"
                          do! expectOracle s r.Diagnostics "baseValue is an int64"
                      })
          }

          // M12: a promoted file whose finish from its dependencies reports an unresolved name is checked whole, and not
          // also by declaration, while the whole check from its dependencies still reports it
          testTask "a promoted file with an unresolved name is checked whole only" {
              do!
                  withSession (startOn chainsProject FileLevel) (fun s ->
                      task {
                          // Set up from C.fs alone, fewer than every earlier file
                          let d = Path.Combine(Path.GetDirectoryName chainsProject, "D.fs")
                          let! _ = edit s d (s.Texts[d] + "\nlet bad = notDefinedAnywhere\n")
                          let! modes = promoteAs s [ d ]
                          Expect.equal modes [ FileLevel ] "its finish reports an unresolved name"
                          let! r, checks = counted s (fun () -> replace s d "shiftTwice offset + 1" "shiftTwice offset + 2")
                          Expect.isFalse (List.contains "D.fs" (ranFiles r)) $"no declaration of D.fs is checked: %A{r.Ran}"
                          Expect.equal checks 2L "D.fs is checked whole, from its dependencies and from every earlier file"
                          do! expectOracle s r.Diagnostics "total + 2"
                          let! r = replace s d "\nlet bad = notDefinedAnywhere\n" "\n"
                          let! mode = s.Graph.WhenPromoted(d, CancellationToken.None) |> settle
                          Expect.equal mode Declarations "built by declaration once the name is gone"
                          do! expectOracle s r.Diagnostics "the unresolved name removed"
                      })
          }

          // N1: a promoted file whose declaration setup fails is checked whole, and the files after it are still built
          testTask "a failed declaration setup does not hold up later files" {
              do!
                  withDeps (fun s ->
                      task {
                          let mutable failing = 1

                          s.Graph.OnSetup(fun path ->
                              if String.Equals(path, a, StringComparison.OrdinalIgnoreCase) && Volatile.Read &failing = 1 then
                                  failwith "the declaration setup failed (test)")

                          let! modes = promoteAs s [ a; c ]
                          Expect.equal modes [ FileLevel; Declarations ] "A.fs's setup failed, C.fs, after it, is built"
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "A.fs checked whole"
                          Volatile.Write(&failing, 0)
                          let! r = replace s a "x * 2" "x * 3"
                          do! expectMode s a Declarations "set up again for its new text"
                          do! expectOracle s r.Diagnostics "A.fs promoted after its text changed"
                      })
          }

          // R12, found by the fuzz in fix round 1: a declaration whose inference variables a later one solved in place
          // is checked again when that later one goes, also when it is then the file's last declaration
          testTask "the last declaration is re-checked when the later use that solved it goes" {
              let fixture = Path.Combine(Path.GetDirectoryName depsDir, "SigQualName", "SigQualName.fsproj")

              do!
                  withSession (startOn fixture FileLevel) (fun s ->
                      task {
                          let y = s.Spec.SourceFiles |> List.find (fun f -> name f = "Y.fs")
                          let withUse = "module Y\n\nlet y  = A.x\n\nlet fresh30 = ref []\n\nlet usefresh30 () = fresh30.Value <- [ 95 ]\n"
                          let! _ = edit s y withUse
                          do! promote s [ y ]
                          let! r = edit s y (withUse.Replace("let usefresh30 () = fresh30.Value <- [ 95 ]\n", ""))
                          Expect.equal (r.NamesIn y) [ "fresh30" ] "fresh30 is checked again"
                          Expect.exists r.Diagnostics[y] (fun d -> d.ErrorNumber = 30) "its value restriction is reported"
                          do! expectOracle s r.Diagnostics "the use removed"
                      })
          }
          // Fix round 2: an implementation file whose check fails adds nothing to the files after it, its signature
          // file's contents included (ParseAndCheckInputs.CheckOneInput's PartialResultOnError). A file set up from the
          // signature file alone sees it through the implementation file then, as the unmatched pairs of Task 3b.
          yield!
              [ for fixtureName, implementation, user, usage in
                    [ "SigShadow", "A.fs", "C.fs", "let z = A.f\n"; "SigQualName", "X.fs", "Y.fs", "" ] do
                    for mode in [ FileLevel; Declarations ] ->
                        testTask $"a failed implementation file's signature gives the files after it nothing (%s{fixtureName}, %A{mode})" {
                            let fixture = Path.Combine(Path.GetDirectoryName depsDir, fixtureName, fixtureName + ".fsproj")

                            do!
                                withSession (startOn fixture mode) (fun s ->
                                    task {
                                        let impl = s.Spec.SourceFiles |> List.find (fun f -> name f = implementation)
                                        let userFile = s.Spec.SourceFiles |> List.find (fun f -> name f = user)

                                        if usage <> "" then
                                            let! r = edit s userFile (s.Texts[userFile] + usage)
                                            do! expectOracle s r.Diagnostics $"%s{user} uses the signature's value"

                                        let! r = edit s impl (s.Texts[impl].TrimEnd('\n') + failingDefault)
                                        Expect.exists r.Diagnostics[userFile] (fun d -> d.ErrorNumber = 39) $"%s{user} sees nothing of %s{implementation}"
                                        do! expectOracle s r.Diagnostics $"%s{implementation} fails"
                                        let! r = replace s impl "3 * scaleDefaults * r" "3 * scaleDefaults r"
                                        do! expectOracle s r.Diagnostics $"%s{implementation} fixed"
                                    })
                        } ]

          // Fix round 2: the dependents of the signature file re-check when the implementation file starts or stops
          // failing, not on its other edits
          testTask "the dependents of a signature file re-check only when its implementation file starts or stops failing" {
              let fixture = Path.Combine(Path.GetDirectoryName depsDir, "SigQualName", "SigQualName.fsproj")

              for mode in [ FileLevel; Declarations ] do
                  do!
                      withSession (startOn fixture mode) (fun s ->
                          task {
                              let x = s.Spec.SourceFiles |> List.find (fun f -> name f = "X.fs")
                              let y = s.Spec.SourceFiles |> List.find (fun f -> name f = "Y.fs")
                              let mutable checksOfY = 0

                              s.Graph.OnScopedCheck(fun path ->
                                  if String.Equals(path, y, StringComparison.OrdinalIgnoreCase) then
                                      Interlocked.Increment &checksOfY |> ignore)

                              let step (what: string) (text: string) (rechecks: bool) =
                                  task {
                                      let before = Volatile.Read &checksOfY
                                      let! r = edit s x text
                                      do! expectOracle s r.Diagnostics $"%A{mode}: %s{what}"
                                      let ran = Volatile.Read &checksOfY - before

                                      if rechecks then
                                          Expect.isGreaterThan ran 0 $"%A{mode}: %s{what}: Y.fs is checked again"
                                      else
                                          Expect.equal ran 0 $"%A{mode}: %s{what}: Y.fs is not checked again"
                                  }

                              let ok = s.Texts[x]
                              let broken = ok.TrimEnd('\n') + failingDefault
                              do! step "a body edit" (ok.Replace("let x = 1", "let x = 2")) false
                              do! step "X.fs fails" broken true
                              do! step "a body edit while it fails" (broken.Replace("x + 1.0", "x + 2.0")) false
                              do! step "X.fs fixed" (broken.Replace("3 * scaleDefaults * r", "3 * scaleDefaults r")) true
                              do! step "X.fs fails again" broken true
                          })
          }
          // S1-R: a file whose setup read an implementation file's failed status while that file was checked again is
          // not settled until the status is
          testTask "a dependent of a signature file does not settle on a stale failed status" {
              let fixture = Path.Combine(Path.GetDirectoryName depsDir, "SigQualName", "SigQualName.fsproj")

              for mode in [ FileLevel; Declarations ] do
                  do!
                      withSession (startOn fixture mode) (fun s ->
                          task {
                              let x = s.Spec.SourceFiles |> List.find (fun f -> name f = "X.fs")
                              let y = s.Spec.SourceFiles |> List.find (fun f -> name f = "Y.fs")
                              let ok = s.Texts[x]

                              for round in 1..3 do
                                  for what, text in [ "fails", ok.TrimEnd('\n') + failingDefault; "fixed", ok ] do
                                      s.Texts[x] <- text
                                      s.Graph.SetText(x, text) |> ignore
                                      let settled = s.Graph.SettledDiagnostics y
                                      let diagnostics = s.Graph.Diagnostics y
                                      let! struct (_, settledY) = settled |> settle
                                      let! diagnosticsY = diagnostics |> settle
                                      let! all = s.Graph.AllDiagnostics() |> settle
                                      do! expectOracle s all $"%A{mode} round %d{round}: X.fs %s{what}"
                                      let codes (ds: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) = ds |> Array.map _.ErrorNumber |> Array.sort
                                      Expect.equal (codes settledY) (codes all[y]) $"%A{mode} round %d{round}: X.fs %s{what}: SettledDiagnostics(Y.fs)"
                                      Expect.equal (codes diagnosticsY) (codes all[y]) $"%A{mode} round %d{round}: X.fs %s{what}: Diagnostics(Y.fs)"
                          })
          }
          // S1-R2: a status two pairs away. W.fs failing stops X.fs failing, so Y.fs, set up from X.fsi alone, sees
          // A.x : int (FS0001). X.fs's signature is Ready all through W.fs's check, computed from W.fs's last status.
          testTask "a file does not settle on a signature computed from a stale failed status two pairs away" {
              let fixture = Path.Combine(Path.GetDirectoryName depsDir, "SigChain2", "SigChain2.fsproj")

              for mode in [ FileLevel; Declarations ] do
                  do!
                      withSession (startOn fixture mode) (fun s ->
                          task {
                              let w = s.Spec.SourceFiles |> List.find (fun f -> name f = "W.fs")
                              let y = s.Spec.SourceFiles |> List.find (fun f -> name f = "Y.fs")
                              let ok = s.Texts[w]
                              let codes (ds: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) = ds |> Array.map _.ErrorNumber |> Array.sort

                              for round in 1..3 do
                                  for what, text in [ "fails", ok.TrimEnd('\n') + failingDefault; "fixed", ok ] do
                                      s.Texts[w] <- text
                                      s.Graph.SetText(w, text) |> ignore

                                      let asked =
                                          [ for f in s.Spec.SourceFiles -> f, s.Graph.SettledDiagnostics f, s.Graph.Diagnostics f ]

                                      let! all = s.Graph.AllDiagnostics() |> settle
                                      do! expectOracle s all $"%A{mode} round %d{round}: W.fs %s{what}"

                                      if what = "fails" then
                                          Expect.contains (codes all[y]) 1 $"%A{mode} round %d{round}: Y.fs sees A.x"

                                      for f, settled, diagnostics in asked do
                                          let! struct (_, settledF) = settled |> settle
                                          let! diagnosticsF = diagnostics |> settle
                                          Expect.equal (codes settledF) (codes all[f]) $"%A{mode} round %d{round}: W.fs %s{what}: SettledDiagnostics(%s{name f})"
                                          Expect.equal (codes diagnosticsF) (codes all[f]) $"%A{mode} round %d{round}: W.fs %s{what}: Diagnostics(%s{name f})"
                          })
          }
          // m4 of re-review 2: WhenPromoted does not answer Declarations for a finish whose setup read a stale status,
          // which a status flip then sets up again
          testTask "a promoted file does not report Declarations on a stale failed status" {
              let fixture = Path.Combine(Path.GetDirectoryName depsDir, "SigChain2", "SigChain2.fsproj")

              do!
                  withSession (startOn fixture Declarations) (fun s ->
                      task {
                          let w = s.Spec.SourceFiles |> List.find (fun f -> name f = "W.fs")
                          let y = s.Spec.SourceFiles |> List.find (fun f -> name f = "Y.fs")
                          // Two declarations, so Y.fs is built by declaration
                          let! _ = edit s y (s.Texts[y] + "\nlet z = 2\n")
                          let ok = s.Texts[w]
                          let mutable setups = 0

                          s.Graph.OnBeginFile(fun path ->
                              if String.Equals(path, y, StringComparison.OrdinalIgnoreCase) then
                                  Interlocked.Increment &setups |> ignore)

                          for round in 1..3 do
                              for what, text in [ "fails", ok.TrimEnd('\n') + failingDefault; "fixed", ok ] do
                                  s.Texts[w] <- text
                                  s.Graph.SetText(w, text) |> ignore
                                  let! mode = s.Graph.WhenPromoted(y, CancellationToken.None) |> settle
                                  let at = Volatile.Read &setups
                                  let! all = s.Graph.AllDiagnostics() |> settle
                                  do! whenPromoted s.Graph s.Spec
                                  do! expectOracle s all $"round %d{round}: W.fs %s{what}"
                                  Expect.equal mode Declarations $"round %d{round}: W.fs %s{what}: Y.fs is promoted"
                                  Expect.equal (Volatile.Read &setups) at $"round %d{round}: W.fs %s{what}: Y.fs is not set up again after WhenPromoted answered"
                      })
          }
          // M5 of the Task 5 review: only a failing setup is contained; a broken invariant of the graph is not
          testTask "a broken invariant in a declaration setup is not contained" {
              do!
                  withDeps (fun s ->
                      task {
                          s.Graph.OnSetup(fun path ->
                              if String.Equals(path, a, StringComparison.OrdinalIgnoreCase) then
                                  raise (GraphInvariantException "an invariant of the setup broke (test)"))

                          s.Graph.Promote a

                          let! (failure: exn option) =
                              task {
                                  try
                                      let! _ = s.Graph.WhenPromoted(a, CancellationToken.None) |> settle
                                      return None
                                  with e ->
                                      return Some e
                              }

                          Expect.isTrue
                              (failure |> Option.exists (fun e -> string e |> fun t -> t.Contains "an invariant of the setup broke (test)"))
                              $"promoting A.fs fails with the broken invariant: %A{failure}"
                      })
          }
          // N2: a wanted file whose text becomes one declaration keeps none of its declaration state
          testTask "a wanted file that becomes one declaration drops its declaration nodes" {
              do!
                  withDeps (fun s ->
                      task {
                          do! promote s [ a ]
                          let! held = s.Graph.DeclarationObjects a
                          Expect.equal held.Length 3 "a declaration result, its environment and its FileStart"
                          let roots = s.Graph.DeclarationRoots
                          let original = s.Texts[a]
                          let! r = edit s a (original.Replace("module Deps.A", "module rec Deps.A"))
                          do! expectMode s a FileLevel "a module rec file is checked whole"
                          do! expectOracle s r.Diagnostics "module rec A.fs"
                          do! dropTrace s
                          let gone = collected held
                          Expect.equal gone [ true; true; true ] $"the declaration result, its environment and its FileStart are collected: %A{gone}"
                          let! r = edit s a original
                          let! mode = s.Graph.WhenPromoted(a, CancellationToken.None) |> settle
                          Expect.equal mode Declarations "still wanted: built again once it has declarations"
                          Expect.equal s.Graph.DeclarationRoots (roots + 1L) "with new declaration nodes"
                          do! expectOracle s r.Diagnostics "A.fs back to its declarations"
                      })
          }

          // The finish's phases add up to the time spent in FinishFile, so the phase profile attributes all of it
          testTask "the finish phases add up to the finish time" {
              do!
                  withDeps (fun s ->
                      task {
                          do! promote s [ a ]
                          let finishPhases = set [ "wait"; "refilter"; "leftovers"; "assemble"; "replay"; "tail"; "typedUses"; "fingerprints"; "signature"; "render"; "sinks" ]
                          let phases () = s.Graph.FinishPhaseTicks |> Seq.filter (fun kv -> finishPhases.Contains kv.Key) |> Seq.sumBy _.Value
                          let ticks0, phases0 = s.Graph.FinishTicks, phases ()
                          let! r, checks = counted s (fun () -> replace s a "x * 2" "x * 3")
                          Expect.equal checks 0L "no file is checked whole, so the phases are the finish's alone"
                          Expect.equal r.Delta 1L "scale is checked and A.fs finished"
                          let toMs (t: int64) = float t * 1000. / float Diagnostics.Stopwatch.Frequency
                          let finishMs, phaseMs = toMs (s.Graph.FinishTicks - ticks0), toMs (phases () - phases0)
                          Expect.isGreaterThan finishMs 0. "A.fs is finished"
                          Expect.isLessThanOrEqual (abs (finishMs - phaseMs)) (max 1. (finishMs * 0.05)) $"the phases (%.3f{phaseMs} ms) add up to the finish (%.3f{finishMs} ms)"
                      })
          } ]
