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

/// Runs `body` on a fresh graph over Deps checking by declaration, settled on the files as they are on disk.
let private withDeps (body: Session -> Task) =
    task {
        let! s = startWith Declarations
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

          testTask "one settle wait per file settles" {
              do!
                  withDeps (fun s ->
                      task {
                          s.Graph.SetText(a, s.Texts[a] + "\nlet waited = 0\n") |> ignore
                          // One wait per file, as an LSP layer would park one per open file
                          let! _ = Task.WhenAll [ for f in s.Spec.SourceFiles -> s.Graph.Diagnostics f ] |> settle
                          ()
                      })
          }

          testTask "the D1 premise holds on load, and only R11 and R12 make segments" {
              let! (spec: Rfs.Projects.ProjectSpec) = Rfs.Projects.ProjectSpec.load depsProject
              let host = GraphHost.Start "premise"

              try
                  let! (graph: ProjectGraph) = ProjectGraph.CreateWithMode(host, spec, Declarations)
                  let! _ = graph.AllDiagnostics() |> settle
                  do! whenPromoted graph spec
                  // Shape's generated comparison members are fingerprinted by their check-time slots, so its final
                  // fingerprint is its check's; A.fs has no free inference variables and no augmentation, so no segment
                  Expect.equal graph.PremiseBreaks 0L "no final fingerprint differs from its check's"
                  Expect.equal (graph.SegmentRows a) 0 "A.fs checks every declaration on its own"
                  Expect.equal (graph.SegmentRows b) 2 "Counter and its augmentation"
                  Expect.equal (graph.SegmentRows d) 4 "r, fill, size, after"
              finally
                  host.Dispose()
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
                          Expect.equal r.Delta 3L $"1 + the 2 declarations of E.fs that use helper; unrelated, user and hidden kept: %A{r.Ran}"
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
                          // doubled is the only declaration of A.fs that names scale; nothing in A.fs is in a segment
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
                          Expect.equal (s.Graph.SegmentRows d) 4 "r's segment: r, fill, size, after"
                          let! r = replace s d "let after = 42" "let after = 43"

                          Expect.equal
                              (r.NamesIn d)
                              [ "after"; "fill"; "r"; "size" ]
                              "r, which fill and size use (fill solves it in place), and everything after it"

                          Expect.equal r.Delta 4L "start is kept"
                          do! expectOracle s r.Diagnostics "after r edited"
                      })
          }

          testTask "a declaration that newly leaves free typars forms its segment" {
              do!
                  withDeps (fun s ->
                      task {
                          // Same lines: start becomes `ref []`, and after becomes its second user (fill is the first)
                          let text =
                              s.Texts[d]
                                  .Replace("let start = baseValue + 1", "let start = ref []")
                                  .Replace("let after = 42", "let after = start.Value |> List.length")

                          let! r = edit s d text
                          let runs = r.RunsIn d
                          // start is checked on its own once (its text changed), then its segment checks start..after once;
                          // a later row may run once more on its own before the segment forms, alone (the write-lock guard).
                          for KeyValue(name, n) in runs do
                              Expect.isLessThanOrEqual n 3 $"%s{name} ran %d{n} times: %A{runs}"

                          Expect.equal (s.Graph.SegmentRows d) 5 "start leads the segment to the end of D.fs"
                          do! expectOracle s r.Diagnostics "start = ref []"
                          printfn $"runs per declaration after start became ref []: %A{runs}"
                      })
          }

          testTask "stale version never wins" {
              do!
                  withDeps (fun s ->
                      task {
                          let events = ConcurrentQueue<string>()
                          use _ = s.Graph.Checked |> Observable.subscribe (fun (k, _) -> events.Enqueue k.Name)
                          let original = s.Texts[e]
                          let v1Ran = TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                          use _ =
                              s.Graph.Ran
                              |> Observable.subscribe (fun (k, _) -> if k.Name = "onlyInV1" then v1Ran.TrySetResult(()) |> ignore)

                          s.Graph.SetText(e, original + "\nlet onlyInV1 = 1\n") |> ignore
                          // v2 arrives once a v1 check has run, while v1's finish is still to come
                          let! () = settle v1Ran.Task
                          let v2 = original + "\nlet onlyInV2: string = 2\n"
                          s.Texts[e] <- v2
                          s.Graph.SetText(e, v2) |> ignore
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

                          // No declaration names grouped, and nothing in A.fs is in a segment
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
          }

          testTask "a line inserted above a declaration with a warning moves its warning (W1)" {
              do!
                  withDeps (fun s ->
                      task {
                          let warnedAt (r: Edit) =
                              r.Diagnostics[e] |> Array.filter (fun x -> x.ErrorNumber = 1182) |> Array.map _.StartLine

                          let shiftedBefore = s.Graph.ShiftedRows
                          let! r = replace s e "let other () =" "// a comment\nlet other () ="
                          Expect.equal (warnedAt r) [| 12 |] "other's unused-value warning, one line lower"
                          // Task 7: the declarations that moved are shifted, not checked again
                          Expect.equal (r.NamesIn e) [] "no declaration is checked again"
                          Expect.equal (s.Graph.ShiftedRows - shiftedBefore) 2L "other and unrelated, which moved, are shifted"
                          do! expectOracle s r.Diagnostics "line inserted above other"
                      })
          }

          testTask "a file whose finish fails adds nothing to later files" {
              do!
                  withDeps (fun s ->
                      task {
                          let original = s.Texts[a]
                          // Applying a default fails where the tail does not catch it: A.fs then contributes nothing,
                          // though no declaration's fingerprint changes, and B.fs, which uses A, must begin again
                          let! r = replace s a "| Circle r -> 3 * r * r" "| Circle r -> 3 * scale * r"
                          Expect.isTrue (r.KeysIn b |> Set.exists (fun k -> k.Name = "counted")) "B.fs begins again"
                          Expect.isNonEmpty r.Diagnostics[b] "B.fs no longer sees Deps.A"
                          do! expectOracle s r.Diagnostics "A's finish fails"
                          let! r = edit s a original
                          Expect.isEmpty r.Diagnostics[b] "B.fs sees Deps.A again"
                          do! expectOracle s r.Diagnostics "A's finish succeeds again"
                      })
          } ]
