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

/// Runs `body` on a fresh file-level graph over Deps, settled on the files as they are on disk.
let private withDeps (body: Session -> Task) =
    task {
        let! s = start ()
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }
        do! body s
    }

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
          } ]
