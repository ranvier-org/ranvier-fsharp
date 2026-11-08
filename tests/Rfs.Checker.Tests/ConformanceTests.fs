/// Edits of what checking a file against its signature file overwrites in its items (an argument name, an arity, a type
/// parameter's name), with a later declaration that uses the item: the graph's cutoff and its re-checks see the items
/// as a full check does, before the conformance check.
module Rfs.Checker.Tests.ConformanceTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Checker.Tests.Deps

/// A.fsi, A.fs (whose later declarations use T.M by its argument name, f and g) and B.fs (which uses A through its
/// signature)
let sigConformProject = Path.Combine(Path.GetDirectoryName depsDir, "SigConform", "SigConform.fsproj")

let private a () = Path.Combine(Path.GetDirectoryName sigConformProject, "A.fs")

/// A graph over SigConform in `mode`, with A.fs checked by declaration (promoted, as an editor's didOpen would).
let private sigConform (mode: FileMode) =
    task {
        let! s = startOn sigConformProject mode
        s.Graph.Promote(a ())
        do! whenPromoted s.Graph s.Spec
        return s
    }

/// The graph's diagnostics and fingerprints equal the oracle's after the edit, and so does each file's
/// SettledDiagnostics, asked for before anything settled.
let private expectEdit (s: Session) (r: Edit) (what: string) =
    task {
        do! expectOracle s r.Diagnostics what

        for KeyValue(f, settled) in r.Settled do
            let diff =
                Rfs.Lsp.Diagnostics.compare
                    (r.Diagnostics[f] |> Seq.map Rfs.Projects.FcsDiagnostics.ofFcs |> List.ofSeq)
                    (settled |> Seq.map Rfs.Projects.FcsDiagnostics.ofFcs |> List.ofSeq)

            Expect.isEmpty diff $"%s{what}: %s{Path.GetFileName f}'s SettledDiagnostics:\n%s{String.concat Environment.NewLine diff}"
    }

let private withSession (mode: FileMode) (work: Session -> Task) =
    task {
        let! s = sigConform mode
        use _ = { new IDisposable with member _.Dispose() = (s :> IAsyncDisposable).DisposeAsync().AsTask().Wait() }
        do! work s
    }

/// Replaces `before` with `after` in A.fs, then back, checking each against the oracle.
let private roundTrip (mode: FileMode) (before: string) (after: string) =
    withSession mode (fun s ->
        task {
            let! r = replace s (a ()) before after
            do! expectEdit s r $"`%s{before}` -> `%s{after}`"
            let! r = replace s (a ()) after before
            do! expectEdit s r $"`%s{after}` -> `%s{before}`"
        })

let private inMode (mode: FileMode) =
    testSequenced
    <| testList
        $"%A{mode}"
        [ testTask "an argument name the signature gives, which a later named argument needs (C1)" {
              do! roundTrip mode "member _.M(t: int) = t" "member _.M(_: int) = 1"
          }

          testTask "an arity the signature truncates (two arguments to its one)" { do! roundTrip mode "let f (x: int) y = x + y" "let f (x: int) = let add y = x + y in add" }

          testTask "a type parameter the signature renames" { do! roundTrip mode "let g (x: 'a) = x" "let g (x: 'b) = x" }

          // C2: after a finish conformed T, a re-check of useM alone sees T as a full check does, unconformed
          testTask "a later declaration edited after the finish conformed the item it uses (C2)" {
              do!
                  withSession mode (fun s ->
                      task {
                          let text = s.Texts[a ()]

                          let unnamed =
                              text.Replace("member _.M(t: int) = t", "member _.M(_: int) = 1").Replace("let useM = T().M(t = 1)", "let useM = 0")

                          let! r = edit s (a ()) unnamed
                          do! expectEdit s r "T unnamed, useM = 0"
                          let! r = replace s (a ()) "let useM = 0" "let useM = T().M(t = 1)"
                          // useM's error leaves its type open, so the declarations after it re-check with it (R12)
                          Expect.equal (r.NamesIn(a ())) [ "useF"; "useG"; "useM" ] "T keeps the result the finish conformed"
                          do! expectEdit s r "useM uses the name only the signature gives"
                      })
          } ]

[<Tests>]
let tests = testList "SignatureConformance" [ inMode FileLevel; inMode Declarations ]
