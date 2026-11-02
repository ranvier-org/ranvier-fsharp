module Rfs.Decl.Tests.D1Tests

open System
open System.IO
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

// The graph (Rfs.Checker) cuts off on the fingerprint a check leaves and relies on: a final fingerprint other than the
// provisional one means DependsOnLaterDeclarations. These pin that premise for the shapes that broke it.

let private depsDir = Path.Combine(Path.GetDirectoryName tinyDir, "Deps")
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")
let private depsText (name: string) = File.ReadAllText(Path.Combine(depsDir, name))

type private Checked =
    { Spec: ProjectSpec
      Context: ProjectContext
      Texts: Map<string, string>
      Files: CheckedFile list }

    member this.File(name: string) = this.Files |> List.find (fun f -> Path.GetFileName f.File = name)

    member this.Result(file: string, name: string) =
        (this.File file).Results |> Array.find (fun r -> r.Key.Name = name)

    member this.Final(file: string, name: string) =
        (this.File file).FileResult.Fingerprints[this.Result(file, name).Key]

/// The project checked one declaration at a time, the oracle (a full check) sequential as an editor's.
let private check (project: string) (overrides: (string * string) list) =
    async {
        let! spec = ProjectSpec.load project |> Async.AwaitTask
        let spec = ProjectSpec.sequential spec
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
        let texts = sources spec (Map.ofList overrides)

        return
            { Spec = spec
              Context = ctx
              Texts = texts
              Files = declByDecl ctx spec texts }
    }

let private expectOracle (c: Checked) =
    async {
        for f in c.Files do
            let! expected = oracle c.Spec c.Texts f.File
            let diff = Diagnostics.compare (toLsp expected) (toLsp f.FileResult.Diagnostics)
            Expect.isEmpty diff $"%s{Path.GetFileName f.File}: diagnostics equal a full check:\n%s{String.concat Environment.NewLine diff}"
    }

/// The declarations whose final fingerprint differs from the provisional one while they do not depend on later ones.
let private premiseBreaks (c: Checked) =
    [ for f in c.Files do
          for r in f.Results do
              if f.FileResult.Fingerprints[r.Key] <> r.Fingerprint && not r.DependsOnLaterDeclarations then
                  $"%s{Path.GetFileName f.File} %s{r.Key.Name}" ]

let private expectPremise (c: Checked) =
    let breaks = premiseBreaks c
    Expect.isEmpty breaks $"final fingerprint = provisional, or the result depends on later declarations: %A{breaks}"

let private dependsOnLater (c: Checked) =
    [ for f in c.Files do
          for r in f.Results do
              if r.DependsOnLaterDeclarations then
                  $"%s{Path.GetFileName f.File} %s{r.Key.Name}" ]

let private types =
    """module Tiny.C

type Shape =
    | Circle of radius: int
    | Square of side: int

type R = { X: int }

[<Struct>]
type S = { Y: int }

exception E of int

type K() =
    override _.ToString() = "k"

type I =
    abstract M: int -> int

type J() =
    interface I with
        member _.M x = x

type V() =
    abstract N: int
    default _.N = 1

[<CustomEquality; NoComparison>]
type U =
    | U of int
    override this.Equals other = obj.ReferenceEquals(this, other)
    override _.GetHashCode() = 0

let after = 1
"""

let private shape (attribute: string) (field: string) =
    $"module Tiny.C\n\n%s{attribute}type Shape =\n    | Circle of radius: %s{field}\n    | Square of side: int\n\nlet after = 1\n"

[<Tests>]
let tests =
    testList
        "D1"
        [ testList
              "generated and overriding members"
              [ testAsync "the Deps fixture keeps the premise, and only r depends on later declarations" {
                    let! c = check depsProject []
                    do! expectOracle c
                    expectPremise c
                    Expect.equal (dependsOnLater c) [ "D.fs r" ] "r = ref [] alone leaves its type open"
                    Expect.equal (c.Final("A.fs", "Shape")) (c.Result("A.fs", "Shape").Fingerprint) "Shape's comparison members survive the finish"
                }

                testAsync "types with generated, overriding, interface and default members keep their fingerprints" {
                    let! c = check tinyProject [ "C.fs", types ]
                    do! expectOracle c
                    expectPremise c
                    Expect.equal (dependsOnLater c) [] "nothing is left open"
                }

                testAsync "[<NoComparison>] changes a union's fingerprint, provisional and final" {
                    let! plain = check tinyProject [ "C.fs", shape "" "int" ]
                    let! noComparison = check tinyProject [ "C.fs", shape "[<NoComparison>]\n" "int" ]
                    do! expectOracle noComparison

                    Expect.notEqual
                        (noComparison.Result("C.fs", "Shape").Fingerprint)
                        (plain.Result("C.fs", "Shape").Fingerprint)
                        "provisional"

                    Expect.notEqual (noComparison.Final("C.fs", "Shape")) (plain.Final("C.fs", "Shape")) "final"
                }

                testAsync "a field type without comparison changes a union's fingerprint, provisional and final" {
                    let! plain = check tinyProject [ "C.fs", shape "" "int" ]
                    let! func = check tinyProject [ "C.fs", shape "" "(int -> int)" ]
                    do! expectOracle func
                    expectPremise func
                    Expect.notEqual (func.Result("C.fs", "Shape").Fingerprint) (plain.Result("C.fs", "Shape").Fingerprint) "provisional"
                    Expect.notEqual (func.Final("C.fs", "Shape")) (plain.Final("C.fs", "Shape")) "final"
                }

                testAsync "a second finish of the same results reports what the first did" {
                    let! c = check tinyProject [ "C.fs", types ]
                    let f = c.File "C.fs"
                    let parse = DeclarationChecker.Parse(c.Context, f.File, c.Texts[f.File])
                    let again = DeclarationChecker.FinishFile(f.Start, f.Results, parse)
                    let diff = Diagnostics.compare (toLsp f.FileResult.Diagnostics) (toLsp again.Diagnostics)
                    Expect.isEmpty diff $"same diagnostics:\n%s{String.concat Environment.NewLine diff}"

                    for r in f.Results do
                        Expect.equal again.Fingerprints[r.Key] f.FileResult.Fingerprints[r.Key] $"%s{r.Key.Name}'s final fingerprint"
                } ]

          testList
              "defaulted inference variables"
              [ testAsync "counted (Deps B.fs, an operand int64) depends on later declarations" {
                    let b = (depsText "B.fs").Replace("(xs: int list) : int =", "(xs: int list) : int64 =")
                    let! c = check depsProject [ "B.fs", b ]
                    do! expectOracle c
                    expectPremise c
                    Expect.notEqual (c.Final("B.fs", "counted")) (c.Result("B.fs", "counted").Fingerprint) "the finish defaults it"
                    Expect.isTrue (c.Result("B.fs", "counted").DependsOnLaterDeclarations) "a later declaration can solve it first"
                }

                testAsync "a later use decides counted's type, which the default would" {
                    let b = (depsText "B.fs").Replace("(xs: int list) : int =", "(xs: int list) : int64 =")
                    let! alone = check depsProject [ "B.fs", b ]
                    let! used = check depsProject [ "B.fs", b + "\nlet z : int64 = counted\n" ]
                    do! expectOracle used
                    expectPremise used
                    Expect.notEqual (used.Final("B.fs", "counted")) (alone.Final("B.fs", "counted")) "int64 when used so, int by default"
                }

                testAsync "areaSum (Deps C.fs, operands that do not add) depends on later declarations" {
                    let text = (depsText "C.fs").Replace("area (Square 2)", "describe 2")
                    let! c = check depsProject [ "C.fs", text ]
                    do! expectOracle c
                    expectPremise c
                    Expect.isTrue (c.Result("C.fs", "areaSum").DependsOnLaterDeclarations) "a later declaration can solve it first"
                }

                testAsync "a member whose return type a later use decides depends on later declarations" {
                    let text = "module Tiny.C\n\ntype T() =\n    member _.M() = 1L + 1\n\nlet t : int64 = T().M()\n"
                    let! c = check tinyProject [ "C.fs", text ]
                    do! expectOracle c
                    expectPremise c
                    Expect.isTrue (c.Result("C.fs", "T").DependsOnLaterDeclarations) "t solves M's return type in place"
                }

                testAsync "an inline body whose inference variable the finish solves depends on later declarations" {
                    let text = "module Tiny.C\n\nlet inline k () = ignore (1L + 1); 5\n\nlet after = 1\n"
                    let! c = check tinyProject [ "C.fs", text ]
                    do! expectOracle c
                    expectPremise c
                } ] ]
