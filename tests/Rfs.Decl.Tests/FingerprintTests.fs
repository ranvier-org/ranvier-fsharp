module Rfs.Decl.Tests.FingerprintTests

open System
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private hAlone =
    """module Tiny.C

let h x = x + x
"""

// The later use solves h's type, which h leaves open: alone it defaults to int.
let private hUsed = hAlone + "\nlet _ = h 1.0\n"

let private generics =
    """module Tiny.C

let id2 x = x

let pair x y = (x, y)

type Box<'T> = { Value: 'T }

let wrap x = { Value = x }

let unwrap (b: Box<'T>) = b.Value
"""

let private valueRestriction =
    """module Tiny.C

let three = Tiny.B.two + 1

let r = ref []
"""

type private Checked =
    { Spec: ProjectSpec
      Context: ProjectContext
      Texts: Map<string, string>
      Files: CheckedFile list }

    member this.File(name: string) = this.Files |> List.find (fun f -> Path.GetFileName f.File = name)

let private checkTiny (overrides: (string * string) list) =
    async {
        let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
        let texts = sources spec (Map.ofList overrides)

        return
            { Spec = spec
              Context = ctx
              Texts = texts
              Files = declByDecl ctx spec texts }
    }

let private checkC (cText: string) = checkTiny [ "C.fs", cText ]

let private diagnosticTexts (diagnostics: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) =
    [ for d in diagnostics -> d.ErrorNumber, d.Range.StartLine, d.Range.StartColumn, d.Message ]

let private fingerprints (fileResult: FileResult) =
    [ for KeyValue(key, fingerprint) in fileResult.Fingerprints -> key, fingerprint ] |> List.sortBy fst

let private fingerprintOf (fileResult: FileResult) (name: string) =
    fileResult.Fingerprints
    |> Seq.filter (fun (KeyValue(key, _)) -> key.Name = name)
    |> Seq.exactlyOne
    |> fun (KeyValue(_, fingerprint)) -> fingerprint

/// Expects the file's diagnostics to equal a full check of the project with C.fs replaced by `cText`.
let private expectFileCheck (spec: ProjectSpec) (cText: string) (file: string) (fileResult: FileResult) (what: string) =
    async {
        let texts = sources spec (Map.ofList [ "C.fs", cText ])
        let! expected = oracle spec texts file
        let diff = Diagnostics.compare (toLsp expected) (toLsp fileResult.Diagnostics)
        Expect.isEmpty diff $"%s{what}: diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"
    }

/// Checks every declaration of `text` in order, on an existing file setup.
let private checkAll (c: CheckedFile) (ctx: ProjectContext) (text: string) =
    DeclarationChecker.Split(DeclarationChecker.Parse(ctx, c.File, text))
    |> Array.mapFold
        (fun env decl ->
            let result = DeclarationChecker.CheckDeclaration(c.Start, env, decl, CancellationToken.None)
            result, result.EnvAfter)
        c.Env0
    |> fst

let private finalFingerprint (before: string) (after: string) (name: string) =
    async {
        let! a = checkC before
        let! b = checkC after
        return fingerprintOf (a.File "C.fs").FileResult name, fingerprintOf (b.File "C.fs").FileResult name
    }

[<Tests>]
let tests =
    testList
        "Fingerprint"
        [ testAsync "body edit keeps fingerprint" {
              let! before, after = finalFingerprint "module Tiny.C\n\nlet f x = x + 1\n" "module Tiny.C\n\nlet f x = x + 2\n" "f"
              Expect.equal after before "same signature"
          }

          testAsync "return type edit changes fingerprint" {
              let! before, after = finalFingerprint "module Tiny.C\n\nlet f x = x + 1\n" "module Tiny.C\n\nlet f x = string (x + 1)\n" "f"
              Expect.notEqual after before "int -> int became int -> string"
          }

          testAsync "inline body edit changes fingerprint" {
              let! before, after =
                  finalFingerprint "module Tiny.C\n\nlet inline g x = x + 1\n" "module Tiny.C\n\nlet inline g x = x + 2\n" "g"

              Expect.notEqual after before "dependents inline the body"
          }

          testAsync "inline body edit of a member changes fingerprint" {
              let text body = $"module Tiny.C\n\ntype T() =\n    member inline _.M(x: int) = x + %d{body}\n"
              let! before, after = finalFingerprint (text 1) (text 2) "T"
              Expect.notEqual after before "dependents inline the member's body"
          }

          testAsync "inline function renaming a local keeps fingerprint" {
              let! before, after =
                  finalFingerprint
                      "module Tiny.C\n\nlet inline g x = let y = x + 1 in y * y\n"
                      "module Tiny.C\n\nlet inline g x = let z = x + 1 in z * z\n"
                      "g"

              Expect.equal after before "locals are numbered, not named"
          }

          testAsync "default changes fingerprint" {
              let! alone = checkC hAlone
              let! used = checkC hUsed
              let aloneC, usedC = alone.File "C.fs", used.File "C.fs"
              do! expectFileCheck alone.Spec hAlone aloneC.File aloneC.FileResult "h alone"
              do! expectFileCheck used.Spec hUsed usedC.File usedC.FileResult "h used"

              let hAloneFinal, hUsedFinal = fingerprintOf aloneC.FileResult "h", fingerprintOf usedC.FileResult "h"
              Expect.notEqual hAloneFinal aloneC.Results[0].Fingerprint "the default solved h's type after its check"
              Expect.notEqual hUsedFinal hAloneFinal "int -> int alone, float -> float when used"
          }

          testAsync "fingerprint independent of check order" {
              let! checkedTiny = checkC generics
              let c = checkedTiny.File "C.fs"

              let reversed =
                  [| for i in c.Declarations.Length - 1 .. -1 .. 0 ->
                         let envBefore = if i = 0 then c.Env0 else c.Results[i - 1].EnvAfter
                         i, DeclarationChecker.CheckDeclaration(c.Start, envBefore, c.Declarations[i], CancellationToken.None) |]
                  |> Array.sortBy fst
                  |> Array.map snd

              for i in 0 .. c.Results.Length - 1 do
                  Expect.equal reversed[i].Fingerprint c.Results[i].Fingerprint $"provisional fingerprint of declaration %d{i}"

              let finished = DeclarationChecker.FinishFile(c.Start, reversed)
              Expect.equal (fingerprints finished) (fingerprints c.FileResult) "same final fingerprints"
          }

          testAsync "fsi conformance error appears" {
              let fsi = "module Tiny.B\n\nval two: int\n\nval b: int\n"
              let fs = "module Tiny.B\n\nlet two = Tiny.A.one + 1\n\nlet b = \"\"\n"
              let! checkedTiny = checkTiny [ "B.fsi", fsi; "B.fs", fs ]
              let b = checkedTiny.File "B.fs"
              let! expected = oracle checkedTiny.Spec checkedTiny.Texts b.File
              Expect.isTrue (expected |> Array.exists (fun d -> d.ErrorNumber = 34)) "the file check reports the mismatch"
              let diff = Diagnostics.compare (toLsp expected) (toLsp b.FileResult.Diagnostics)
              Expect.isEmpty diff $"diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"
          }

          testAsync "finish twice gives equal results" {
              let! checkedTiny = checkC valueRestriction
              let c = checkedTiny.File "C.fs"
              do! expectFileCheck checkedTiny.Spec valueRestriction c.File c.FileResult "first finish"
              Expect.isTrue (c.FileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 30)) "the value restriction"

              let again = DeclarationChecker.FinishFile(c.Start, c.Results)
              Expect.equal (diagnosticTexts again.Diagnostics) (diagnosticTexts c.FileResult.Diagnostics) "same diagnostics"
              Expect.equal (fingerprints again) (fingerprints c.FileResult) "same fingerprints"
          }

          // Each version's declarations are checked afresh, so no result is reused across versions; the reuse case is the
          // pending R12 test below.
          testAsync "finishing fresh results of two versions alternately on one setup gives equal results" {
              let! checkedTiny = checkC hAlone
              let c = checkedTiny.File "C.fs"
              let usedResults = checkAll c checkedTiny.Context hUsed

              let aloneFirst = c.FileResult
              let usedFirst = DeclarationChecker.FinishFile(c.Start, usedResults)
              let aloneAgain = DeclarationChecker.FinishFile(c.Start, c.Results)
              let usedAgain = DeclarationChecker.FinishFile(c.Start, usedResults)

              do! expectFileCheck checkedTiny.Spec hAlone c.File aloneFirst "alone"
              do! expectFileCheck checkedTiny.Spec hUsed c.File usedFirst "used"
              do! expectFileCheck checkedTiny.Spec hAlone c.File aloneAgain "alone again"
              do! expectFileCheck checkedTiny.Spec hUsed c.File usedAgain "used again"
              Expect.equal (fingerprints aloneAgain) (fingerprints aloneFirst) "alone: same fingerprints"
              Expect.equal (fingerprints usedAgain) (fingerprints usedFirst) "used: same fingerprints"
              Expect.notEqual (fingerprintOf usedFirst "h") (fingerprintOf aloneFirst "h") "the use changes h's type"
          }

          testAsync "a finished result checked against by a new declaration matches file check" {
              let! checkedTiny = checkC hAlone
              let c = checkedTiny.File "C.fs"
              let newUse = (DeclarationChecker.Split(DeclarationChecker.Parse(checkedTiny.Context, c.File, hUsed)))[1]

              // The finish defaulted h's type to int; the new declaration must see it as h's check left it.
              let used = DeclarationChecker.CheckDeclaration(c.Start, c.Results[0].EnvAfter, newUse, CancellationToken.None)
              let finished = DeclarationChecker.FinishFile(c.Start, [| c.Results[0]; used |])
              do! expectFileCheck checkedTiny.Spec hUsed c.File finished "old h with a new use"

              let! fresh = checkC hUsed
              Expect.equal (fingerprintOf finished "h") (fingerprintOf (fresh.File "C.fs").FileResult "h") "h as a fresh check has it"
          }

          // R12 (Task 13): the use's check solved h's free type in place, and nothing undoes it. The guard discards h's
          // result and re-checks h when a later declaration of the file changes or is removed.
          ptestCase "later use removed re-checks the free-typar decl" <| fun () ->
              async {
                  let! checkedTiny = checkC hUsed
                  let c = checkedTiny.File "C.fs"
                  let finished = DeclarationChecker.FinishFile(c.Start, [| c.Results[0] |])
                  do! expectFileCheck checkedTiny.Spec hAlone c.File finished "h without its use"

                  let! fresh = checkC hAlone
                  Expect.equal (fingerprints finished) (fingerprints (fresh.File "C.fs").FileResult) "h as a fresh check has it"
              }
              |> Async.RunSynchronously

          testAsync "inline body calling a void method is described" {
              let! before, after =
                  finalFingerprint
                      "module Tiny.C\n\nlet inline log (s: string) = System.Console.WriteLine s\n"
                      "module Tiny.C\n\nlet inline log (s: string) = System.Console.Write s\n"
                      "log"

              Expect.isFalse (before.StartsWith "?") "describable"
              Expect.notEqual after before "a different method"
          }

          testAsync "inline body passing a byref is described" {
              let text = "module Tiny.C\n\nlet inline parse (s: string) =\n    let mutable x = 0\n    System.Int32.TryParse(s, &x), x\n"
              let! checkedTiny = checkC text
              let c = checkedTiny.File "C.fs"
              do! expectFileCheck checkedTiny.Spec text c.File c.FileResult "TryParse"
              Expect.isFalse ((fingerprintOf c.FileResult "parse").StartsWith "?") "describable"

              let bump name = $"module Tiny.C\n\nlet inline bump () =\n    let mutable x = 0\n    System.Threading.Interlocked.%s{name}(&x)\n"
              let! before, after = finalFingerprint (bump "Increment") (bump "Decrement") "bump"
              Expect.notEqual after before "a different method"
          }

          testAsync "inline float constant change at the 11th digit changes fingerprint" {
              let! before, after =
                  finalFingerprint
                      "module Tiny.C\n\nlet inline g x = x + 1.00000000001\n"
                      "module Tiny.C\n\nlet inline g x = x + 1.00000000002\n"
                      "g"

              Expect.notEqual after before "constants are described exactly"
          }

          testAsync "literal float change at the 11th digit changes fingerprint" {
              let! before, after =
                  finalFingerprint
                      "module Tiny.C\n\n[<Literal>]\nlet k = 1.00000000001\n"
                      "module Tiny.C\n\n[<Literal>]\nlet k = 1.00000000002\n"
                      "k"

              Expect.notEqual after before "literal values are described exactly"
          }

          testAsync "inline IL is described by its instructions" {
              let text op = $"module Tiny.C\n\nlet inline f (x: int) : int = (# \"%s{op}\" x 1 : int #)\n"
              let! before, after = finalFingerprint (text "add") (text "sub") "f"
              Expect.isFalse (before.StartsWith "?") "describable"
              Expect.notEqual after before "a different instruction"
          }

          testAsync "inline IL with a type token is never equal" {
              let text = "module Tiny.C\n\nlet inline asString (x: obj) : obj = (# \"isinst [System.Runtime]System.String\" x : obj #)\n"
              let! checkedTiny = checkC text
              let c = checkedTiny.File "C.fs"
              let again = DeclarationChecker.FinishFile(c.Start, c.Results)
              let fingerprint = fingerprintOf c.FileResult "asString"
              Expect.isTrue (fingerprint.StartsWith "?") $"not describable: %s{fingerprint}"
              Expect.notEqual (fingerprintOf again "asString") fingerprint "counts as changed every time"
          } ]
