module Rfs.Checker.Tests.ProjectGraphTests

open System
open System.Collections.Concurrent
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Checker
open Rfs.Checker.Tests.Deps

let private a = fileOf "A.fs"
let private b = fileOf "B.fs"
let private c = fileOf "C.fs"
let private d = fileOf "D.fs"
let private e = fileOf "E.fs"

/// Runs `body` on a fresh graph over Deps, settled on the files as they are on disk.
let private withDeps (body: Session -> Task) =
    task {
        let! s = start ()
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }
        do! body s
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "ProjectGraph"
        [ testTask "the fixture as it is equals the oracle" {
              do!
                  withDeps (fun s ->
                      task {
                          let! diagnostics = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s diagnostics "initial"
                      })
          }

          testTask "body edit re-checks one declaration" {
              do!
                  withDeps (fun s ->
                      task {
                          let! r = replace s a "x * 2" "x * 3"
                          Expect.equal (r.NamesIn a) [ "scale" ] "only scale is published again"
                          Expect.equal r.Delta 1L "one CheckDeclaration"
                          do! expectOracle s r.Diagnostics "body edit"
                      })
          }

          testTask "signature edit re-checks dependents only" {
              do!
                  withDeps (fun s ->
                      task {
                          // helper is hidden by E.fsi, so no later file sees the change; compute and other use it.
                          let! r = replace s e "let helper () : int = 5" "let helper () : string = \"5\""
                          Expect.equal (r.NamesIn e) [ "compute"; "helper"; "other" ] "helper and its two users"
                          Expect.equal r.Delta 3L "1 + the 2 declarations of E.fs that use helper; unrelated, user and hidden kept"
                          do! expectOracle s r.Diagnostics "signature edit in E.fs"
                      })
          }

          testTask "signature edit with a user in a later file begins the later files again (R16)" {
              do!
                  withDeps (fun s ->
                      task {
                          let later = [ b; c; d; e ]
                          let rows = Collections.Generic.Dictionary<string, Set<DeclarationKey>>()

                          for f in later do
                              let! results = s.Graph.Results f
                              rows[f] <- Set.ofSeq results.Keys

                          let! r = replace s a "let scale (x: int) : int = x * 2" "let scale (x: int) : int64 = int64 (x * 2)"
                          Expect.equal (r.NamesIn a) [ "doubled"; "scale" ] "scale and its user in A.fs"

                          for f in later do
                              Expect.equal (r.KeysIn f) rows[f] $"every declaration of %s{IO.Path.GetFileName f} re-checks: the file begins again"

                          let laterCount = later |> List.sumBy (fun f -> rows[f].Count)
                          Expect.equal r.Delta (2L + int64 laterCount) "scale, doubled, and each declaration of each later file once"
                          do! expectOracle s r.Diagnostics "scale returns int64"
                      })
          }

          testTask "shadowing re-checks the user" {
              do!
                  withDeps (fun s ->
                      task {
                          let! r = edit s a (s.Texts[a] + "\nlet length = 0\n")
                          Expect.contains (r.NamesIn c) "total" "total uses length, which Deps.A now shadows"
                          Expect.isNonEmpty (r.Diagnostics[c]) "length is no longer a function in C.fs"
                          do! expectOracle s r.Diagnostics "length shadowed"
                      })
          }

          testTask "free typars widen" {
              do!
                  withDeps (fun s ->
                      task {
                          let! r = replace s d "let after = 42" "let after = 43"
                          Expect.equal (r.NamesIn d) [ "after"; "fill"; "r" ] "r, which fill solves in place, and everything after it"
                          Expect.equal r.Delta 3L "start is kept"
                          do! expectOracle s r.Diagnostics "after r edited"
                      })
          }

          testTask "stale version never wins" {
              do!
                  withDeps (fun s ->
                      task {
                          let events = ConcurrentQueue<string>()
                          use _ = s.Graph.Checked |> Observable.subscribe (fun (k, _) -> events.Enqueue k.Name)
                          let original = s.Texts[e]
                          s.Graph.SetText(e, original + "\nlet onlyInV1 = 1\n")
                          let v2 = original + "\nlet onlyInV2: string = 2\n"
                          s.Texts[e] <- v2
                          s.Graph.SetText(e, v2)
                          // Runs on the graph thread after the second write is applied
                          do! s.Host.Read(fun () -> events.Enqueue "<v2 set>")
                          let! diagnostics = s.Graph.AllDiagnostics() |> settle
                          do! expectOracle s diagnostics "v2"
                          let afterV2 = events |> Seq.skipWhile ((<>) "<v2 set>") |> List.ofSeq
                          Expect.isFalse (List.contains "onlyInV1" afterV2) "no v1 result is published after v2 is set"
                          Expect.contains afterV2 "onlyInV2" "v2's declaration is published"
                      })
          }

          testTask "broken parse keeps keys" {
              do!
                  withDeps (fun s ->
                      task {
                          let! before = s.Graph.Results a
                          // grouped is A's last declaration: an unclosed paren takes in the declarations after it, and
                          // error recovery leaves inference variables free, so the rows after a broken one re-check (R12).
                          let! r = replace s a "let grouped = (baseValue + 1) * 2" "let grouped = (baseValue + 1 * 2"
                          let! after = s.Graph.Results a
                          Expect.equal (Set.ofSeq after.Keys) (Set.ofSeq before.Keys) "the same declarations"

                          for KeyValue(key, result) in before do
                              let kept = obj.ReferenceEquals(after[key], result)

                              if key.Name = "grouped" then
                                  Expect.isFalse kept "grouped re-checks"
                              else
                                  Expect.isTrue kept $"%s{key.Name} keeps its result"

                          Expect.equal r.Delta 1L "only grouped"
                          do! expectOracle s r.Diagnostics "closing paren deleted"
                      })
          }

          testTask "hidden value edit keeps FS1182 right" {
              do!
                  withDeps (fun s ->
                      task {
                          let! r = replace s e "let hidden = 1" "let hidden = 2"
                          Expect.equal (r.NamesIn e) [ "hidden" ] "only hidden"
                          Expect.equal r.Delta 1L "user is kept"

                          Expect.isFalse
                              (r.Diagnostics[e] |> Array.exists (fun x -> x.ErrorNumber = 1182 && x.Range.StartLine = 3))
                              "hidden is used by the kept user"

                          do! expectOracle s r.Diagnostics "hidden edited"
                      })
          }

          testTask "header edit re-checks inner rows" {
              do!
                  withDeps (fun s ->
                      task {
                          let! r = replace s b "module Util =" "module Util2 ="
                          let keys = r.KeysIn b
                          Expect.isTrue (keys |> Set.exists (fun k -> k.Name = "Util2" && k.Kind = DeclarationKind.Module)) "the header"

                          for name in [ "length"; "twice" ] do
                              Expect.isTrue
                                  (keys |> Set.exists (fun k -> k.Name = name && List.contains "Util2" k.ModulePath))
                                  $"%s{name}, under the renamed module"

                          do! expectOracle s r.Diagnostics "Util renamed"
                      })
          }

          testTask "header accessibility edit re-checks inner rows with the same keys" {
              do!
                  withDeps (fun s ->
                      task {
                          let! before = s.Graph.Results b
                          let! r = replace s b "module Util =" "module internal Util ="
                          let inner = before.Keys |> Seq.filter (fun k -> List.contains "Util" k.ModulePath) |> Set.ofSeq
                          Expect.isNonEmpty inner "Util has declarations"
                          Expect.isTrue (Set.isSubset inner (r.KeysIn b)) $"every declaration under the header, through Within: %A{r.KeysIn b}"
                          do! expectOracle s r.Diagnostics "Util internal"
                      })
          } ]
