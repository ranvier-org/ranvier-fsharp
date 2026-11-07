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

/// Promotes the files and waits until each is checked by declaration for its latest text.
let private promote (s: Session) (files: string list) =
    task {
        for f in files do
            s.Graph.Promote f

        for f in files do
            do! s.Graph.WhenPromoted(f, CancellationToken.None) |> settle
    }

let private expectMode (s: Session) (file: string) (expected: FileMode) (what: string) =
    task {
        let! mode = s.Graph.Mode file
        Expect.equal mode expected $"%s{name file}: %s{what}"
    }

/// The edit's CheckDeclaration runs, by file name
let private ranFiles (r: Edit) = r.Ran |> List.map (snd >> name) |> List.distinct |> List.sort

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
                          do! s.Graph.WhenPromoted(c, CancellationToken.None) |> settle
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
                          do! promote s [ r ]
                          do! expectMode s r FileLevel "a module rec file splits into one declaration"
                          let! e, checks = counted s (fun () -> replace s r "let start = 10" "let start = 11")
                          Expect.equal e.Delta 0L "no declaration is checked"
                          Expect.isGreaterThan checks 0L "checked whole"
                          do! expectMode s r FileLevel "still file-level"
                          do! expectOracle s e.Diagnostics "start = 11"
                          let! e = replace s r "let start = 11" "let start : string = 11"
                          do! expectOracle s e.Diagnostics "start is a string"
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
                          do! s.Graph.WhenPromoted(a, CancellationToken.None) |> settle
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
                          let! results = s.Graph.Results a |> settle
                          Expect.isNonEmpty results "declaration results while promoted"
                          s.Graph.Demote a
                          let! all = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s all "the kept result after demotion"
                          do! expectMode s a FileLevel "demoted"

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
          } ]
