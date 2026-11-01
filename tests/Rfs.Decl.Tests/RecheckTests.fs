module Rfs.Decl.Tests.RecheckTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private fourDecls =
    """module Tiny.C

let one = Tiny.B.two - 1

let two = one + 1

let three = two + 1

let four: string = three
"""

// Declaration 2's body edited on its own line: same type, so the old results of 3 and 4 stay valid, and an incomplete match.
let private fourDeclsEdited = fourDecls.Replace("let two = one + 1", "let two = match one with 1 -> one + 1")

let private inNamespace =
    """namespace Tiny

module One =
    let one = B.two - 1

type Two() =
    member _.Value = One.one + 1

module Three =
    let three = Two().Value + 1

module Four =
    let four: string = Three.three
"""

let private inNamespaceEdited =
    inNamespace.Replace("member _.Value = One.one + 1", "member _.Value = match One.one with 1 -> 2")

let private twoFragments =
    """namespace Tiny.X

module C =
    let three = Tiny.B.two + 1

namespace Tiny.Y

module D =
    let four: string = Tiny.X.C.three + 1
"""

let private twoFragmentsEdited =
    twoFragments.Replace("let four: string = Tiny.X.C.three + 1", "let four = match Tiny.X.C.three with 3 -> 4")

let private heavySecond =
    """module Tiny.C

let one = Tiny.B.two - 1

let two = [ for i in 1 .. 10 -> (one + i, string i) ] |> List.map (fun (n, s) -> n + s.Length) |> List.sum

let three = two + 1

let four: string = string three
"""

let private duplicateLet =
    """module Tiny.C

let x = 1

let x = 2
"""

let private globalDuplicates =
    """namespace global

type T = A

namespace global

type T = B

namespace global

module M =
    let a = 1

namespace global

module M =
    let b = 2
"""

let private augmented =
    """module Tiny.C

type T() =
    member _.A = 1

type T with
    member _.B = 2

let three = 3
"""

type private Fixture =
    { Spec: ProjectSpec
      Context: ProjectContext
      C: CheckedFile }

let private checkTiny (cText: string) =
    async {
        let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
        let files = declByDecl ctx spec (sources spec (Map.ofList [ "C.fs", cText ]))

        return
            { Spec = spec
              Context = ctx
              C = files |> List.find (fun f -> f.File.EndsWith "C.fs") }
    }

let private check (c: CheckedFile) (envBefore: DeclEnv) (decl: ParsedDeclaration) =
    DeclarationChecker.CheckDeclaration(c.Start, envBefore, decl, CancellationToken.None)

let private diagnosticTexts (diagnostics: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) =
    [ for d in diagnostics -> d.ErrorNumber, d.Range.StartLine, d.Range.StartColumn, d.Message ]

/// The names of the values of a module in the file's assembly signature, in order.
let private moduleValues (fileResult: FileResult) (path: string list) =
    let fullName = String.concat "." path

    let rec all (entities: seq<FSharpEntity>) =
        entities |> Seq.collect (fun e -> Seq.append [ e ] (all e.NestedEntities))

    let entity =
        all fileResult.CheckResults.PartialAssemblySignature.Entities
        |> Seq.find (fun e -> e.FullName = fullName)

    [ for v in entity.MembersFunctionsAndValues -> v.DisplayName ]

/// Checks the project with C.fs replaced, and expects the same diagnostics for C.fs as a full check.
let private matchesFileCheck (cText: string) =
    async {
        let! fixture = checkTiny cText
        let texts = sources fixture.Spec (Map.ofList [ "C.fs", cText ])
        let! expected = oracle fixture.Spec texts fixture.C.File
        let diff = Diagnostics.compare (toLsp expected) (toLsp fixture.C.FileResult.Diagnostics)
        Expect.isEmpty diff $"decl-by-decl diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"
        Expect.isNonEmpty expected "the file check reports"
        return fixture
    }

/// The names of the members of the entity that declares the value with the given name.
let private declaringEntityMembers (fileResult: FileResult) (name: string) =
    let definition =
        fileResult.CheckResults.GetAllUsesOfAllSymbolsInFile()
        |> Seq.find (fun u -> u.IsFromDefinition && u.Symbol.DisplayName = name && (u.Symbol :? FSharpMemberOrFunctionOrValue))

    let value = definition.Symbol :?> FSharpMemberOrFunctionOrValue
    [ for v in value.DeclaringEntity.Value.MembersFunctionsAndValues -> v.DisplayName ]

/// Checks C.fs, then re-checks only declaration `index` (not the first) of the edited text against the old environment after the
/// declaration before it, and finishes the file with the old results of the others.
let private recheckOne (original: string) (edited: string) (index: int) =
    async {
        let! fixture = checkTiny original
        let c = fixture.C
        let editedParse = DeclarationChecker.Parse(fixture.Context, c.File, edited)
        let editedDecls = DeclarationChecker.Split editedParse
        Expect.equal editedDecls.Length c.Declarations.Length "the edit keeps the declarations"

        let rechecked = check c c.Results[index - 1].EnvAfter editedDecls[index]
        let results = c.Results |> Array.mapi (fun i r -> if i = index then rechecked else r)
        let fileResult = DeclarationChecker.FinishFile(c.Start, results, editedParse)

        let texts = sources fixture.Spec (Map.ofList [ "C.fs", edited ])
        let! expected = oracle fixture.Spec texts c.File
        let diff = Diagnostics.compare (toLsp expected) (toLsp fileResult.Diagnostics)
        Expect.isEmpty diff $"re-checked file diagnostics equal a fresh full check:\n%s{String.concat Environment.NewLine diff}"
        Expect.isTrue (rechecked.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 25)) "the edit's warning belongs to the re-checked declaration"
        return c, rechecked, fileResult
    }

[<Tests>]
let tests =
    testList
        "Recheck"
        [ testAsync "recheck one declaration matches full check" {
              let! c, rechecked, fileResult = recheckOne fourDecls fourDeclsEdited 1
              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "the old result of four keeps its type error"
              Expect.equal (moduleValues fileResult [ "Tiny"; "C" ]) [ "one"; "two"; "three"; "four" ] "the module is built from the deltas, in order"
              Expect.isFalse (rechecked.IsSameDelta c.Results[1]) "the re-check contributes new objects"
              Expect.isTrue (c.Results[2].IsSameDelta c.Results[2]) "a result is its own delta"
          }

          testAsync "recheck one declaration in a namespace matches full check" {
              // The rows are One, one, Two, Three, three, Four, four: nested modules are split into their declarations
              let! _, _, fileResult = recheckOne inNamespace inNamespaceEdited 2
              Expect.equal (moduleValues fileResult [ "Tiny"; "Three" ]) [ "three" ] "later modules are kept"
          }

          testAsync "recheck one namespace fragment matches full check" {
              let! _, _, fileResult = recheckOne twoFragments twoFragmentsEdited 1
              Expect.equal (moduleValues fileResult [ "Tiny"; "X"; "C" ]) [ "three" ] "the earlier fragment is kept"
              Expect.equal (moduleValues fileResult [ "Tiny"; "Y"; "D" ]) [ "four" ] "the re-checked fragment"
          }

          testAsync "finishing again builds the same file from the same results" {
              let! fixture = checkTiny fourDecls
              let c = fixture.C
              let again = DeclarationChecker.FinishFile(c.Start, c.Results, c.Start.Parse)
              Expect.equal (diagnosticTexts again.Diagnostics) (diagnosticTexts c.FileResult.Diagnostics) "same diagnostics"
              Expect.equal (moduleValues again [ "Tiny"; "C" ]) [ "one"; "two"; "three"; "four" ] "no declaration is added twice"
              Expect.equal (moduleValues c.FileResult [ "Tiny"; "C" ]) [ "one"; "two"; "three"; "four" ] "the first finish is unchanged"
          }

          testAsync "an earlier result is unchanged by later checks" {
              let! fixture = checkTiny fourDecls
              let c = fixture.C
              let first = check c c.Results[0].EnvAfter c.Declarations[1]
              let third = check c first.EnvAfter c.Declarations[2]
              let _fourth = check c third.EnvAfter c.Declarations[3]
              let again = check c c.Results[0].EnvAfter c.Declarations[1]
              Expect.equal again.Fingerprint first.Fingerprint "same fingerprint from the same environment"
              Expect.equal again.Fingerprint c.Results[1].Fingerprint "same fingerprint as the first full check"
              Expect.equal (diagnosticTexts again.Diagnostics) (diagnosticTexts first.Diagnostics) "same diagnostics"
              Expect.equal third.Fingerprint c.Results[2].Fingerprint "a later declaration checked after a re-check"

              let finished = DeclarationChecker.FinishFile(c.Start, [| c.Results[0]; again; third; _fourth |], c.Start.Parse)
              Expect.equal (moduleValues finished [ "Tiny"; "C" ]) [ "one"; "two"; "three"; "four" ] "one value per declaration"
          }

          testAsync "a duplicate module-level let matches file check" {
              let! fixture = matchesFileCheck duplicateLet
              Expect.isTrue (fixture.C.FileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 37)) "duplicate definition"
          }

          testAsync "duplicate types and modules in namespace global fragments match file check" {
              let! fixture = matchesFileCheck globalDuplicates
              Expect.equal fixture.C.Results.Length 4 "each fragment is one declaration"
          }

          testAsync "a module value's declaring entity lists it" {
              let! fixture = checkTiny fourDecls
              let c = fixture.C
              Expect.equal (declaringEntityMembers c.FileResult "three") [ "one"; "two"; "three"; "four" ] "after the full check"

              let finished = DeclarationChecker.FinishFile(c.Start, c.Results, c.Start.Parse)
              Expect.equal (declaringEntityMembers finished "three") [ "one"; "two"; "three"; "four" ] "after finishing again"
          }

          // R11: re-checking an augmentation alone adds its members to T again; Task 13 makes it re-check T too.
          ptestCase "re-checking an augmentation alone leaves its type with one copy of its members" <| fun () ->
              async {
                  let! fixture = checkTiny augmented
                  let c = fixture.C
                  let rechecked = check c c.Results[0].EnvAfter c.Declarations[1]
                  let finished = DeclarationChecker.FinishFile(c.Start, [| c.Results[0]; rechecked; c.Results[2] |], c.Start.Parse)

                  Expect.equal
                      (moduleValues finished [ "Tiny"; "C"; "T" ] |> List.filter ((=) "B"))
                      [ "B" ]
                      "one B"
              }
              |> Async.RunSynchronously

          testAsync "results don't leak" {
              let! fixture = checkTiny heavySecond
              let c = fixture.C
              let envBefore = c.Results[0].EnvAfter
              let expected = c.Results[1]

              for _ in 1..10 do
                  use barrier = new Barrier(2)

                  let run () =
                      Task.Run(fun () ->
                          barrier.SignalAndWait()
                          check c envBefore c.Declarations[1])

                  let t1, t2 = run (), run ()
                  let! r1 = Async.AwaitTask t1
                  let! r2 = Async.AwaitTask t2
                  Expect.equal r1.Fingerprint r2.Fingerprint "parallel checks give equal fingerprints"
                  Expect.equal r1.Fingerprint expected.Fingerprint "and equal the sequential check"
                  Expect.equal (diagnosticTexts r1.Diagnostics) (diagnosticTexts expected.Diagnostics) "same diagnostics"
                  Expect.equal (diagnosticTexts r2.Diagnostics) (diagnosticTexts expected.Diagnostics) "same diagnostics"
                  Expect.isFalse (r1.IsSameDelta r2) "each check has its own objects"

              let finished = DeclarationChecker.FinishFile(c.Start, c.Results, c.Start.Parse)
              Expect.equal (diagnosticTexts finished.Diagnostics) (diagnosticTexts c.FileResult.Diagnostics) "the file is unchanged"
              Expect.equal (moduleValues finished [ "Tiny"; "C" ]) [ "one"; "two"; "three"; "four" ] "no stray values"
          } ]
