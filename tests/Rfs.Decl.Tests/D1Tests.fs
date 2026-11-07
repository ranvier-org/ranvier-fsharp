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

let private ordinary =
    """module Tiny.C

let map f xs = List.map f xs

let inline add x y = x + y

let inline callM (x: ^a) = (^a: (member M: int) x)

type Box<'T>(v: 'T) =
    member _.V = v
    member inline _.Map f = f v

type Pair<'T> = { First: 'T; Second: 'T }

type Tree<'T> =
    | Leaf
    | Node of Tree<'T> * 'T * Tree<'T>

let empty = []

type Counter() =
    member val X = 0 with get, set
    override _.ToString() = "counter"

type Queue<'T>(items: 'T list) =
    member _.Head = List.head items

let inline head (q: Queue<'T>) = q.Head
"""

/// Where two fingerprints first differ, with the parts before it.
let private firstDifference (provisional: string) (final: string) =
    let a, b = provisional.Split '|', final.Split '|'
    let i = Seq.init (min a.Length b.Length) id |> Seq.tryFind (fun i -> a[i] <> b[i]) |> Option.defaultValue (min a.Length b.Length)
    let part (parts: string[]) = if i < parts.Length then parts[i] else "(end)"
    let before = String.concat "|" a[max 0 (i - 8) .. i - 1]
    $"part %d{i} after '%s{before}': provisional '%s{part a}', final '%s{part b}'"

/// Every declaration of the file has its check's fingerprint after the finish.
let private expectFinalIsProvisional (c: Checked) (file: string) =
    for r in (c.File file).Results do
        let final = c.Final(file, r.Key.Name)

        if final <> r.Fingerprint then
            failtest $"%s{file} %s{r.Key.Name}: final = provisional, but %s{firstDifference r.Fingerprint final}"

/// Tiny with B.fsi and B.fs replaced; both keep `two`, which C.fs uses.
let private withSignature (fsi: string) (fs: string) =
    check tinyProject [ "B.fsi", "module Tiny.B\n\nval two: int\n" + fsi; "B.fs", "module Tiny.B\n\nlet two = Tiny.A.one + 1\n" + fs ]

/// What checking B.fs against B.fsi changes in a declaration is in its fingerprint from its check on.
let private conforms (name: string) (fsi: string) (fs: string) =
    testAsync name {
        let! c = withSignature fsi fs
        do! expectOracle c
        expectFinalIsProvisional c "B.fs"
        expectPremise c
    }

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
                    expectPremise noComparison

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
                    let! expected = oracle c.Spec c.Texts f.File
                    let diff = Diagnostics.compare (toLsp expected) (toLsp again.Diagnostics)
                    Expect.isEmpty diff $"the second finish equals a full check:\n%s{String.concat Environment.NewLine diff}"

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
                    Expect.isTrue (c.Result("C.fs", "k").DependsOnLaterDeclarations) "the finish's default changes k's inline body"
                }

                testAsync "a member val whose type a later use can decide depends on later declarations" {
                    let text = "module Tiny.C\n\ntype T() =\n    member val X = ResizeArray() with get\n\nlet after = 1\n"
                    let! c = check tinyProject [ "C.fs", text ]
                    do! expectOracle c
                    expectPremise c
                    // The variable stays in the environment, so after leaves it open too, as rows after an unsolved
                    // `let r = ref []` do
                    Expect.equal (dependsOnLater c) [ "C.fs T"; "C.fs after" ] "X's element type is open: a later T().X.Add 1 decides it"
                } ]

          // Each shape broke the premise on the corpora: FinishFile checks the file against its signature file, which
          // gives the implementation's items arities, argument names and attributes from the signature
          testList
              "signature conformance"
              [ conforms
                    "a value bound to a lambda takes its arity from a signature without arguments (List.moreThanOne)"
                    "val moreThanOne: ('a list -> bool)\nval after: int\n"
                    "\nlet moreThanOne =\n    function\n    | []\n    | [ _ ] -> false\n    | _ -> true\n\nlet after = 1\n"

                conforms
                    "a partial active pattern bound to a lambda (|Val|_|)"
                    "val (|Val|_|): (int option -> int option)\nval after: int\n"
                    "\nlet (|Val|_|) =\n    function\n    | Some i when i > 0 -> Some i\n    | _ -> None\n\nlet after = 1\n"

                conforms
                    "a partial active pattern with an argument returning a union case (|StaticParameter|_|)"
                    "val (|Wrap|_|): _x: obj -> ('a -> 'a option)\nval after: int\n"
                    "\nlet (|Wrap|_|) (_x: obj) = Some\n\nlet after = 1\n"

                conforms
                    "a partial active pattern with an argument (|Val|_|) (x: obj)"
                    "val (|Val|_|): x: obj -> int option\nval after: int\n"
                    "\nlet (|Val|_|) (x: obj) =\n    match x with\n    | :? int as i -> Some i\n    | _ -> None\n\nlet after = 1\n"

                conforms
                    "an interface rec group takes [<Interface>] from its signature (IAdaptiveValue)"
                    "\n[<Interface>]\ntype IA =\n    abstract M: IB -> int\n\nand [<Interface>] IB =\n    abstract N: IA -> int\n\nval after: int\n"
                    "\ntype IA =\n    abstract M: IB -> int\n\nand IB =\n    abstract N: IA -> int\n\nlet after = 1\n"

                conforms
                    "a class takes [<Class>] from its signature (ChangeableValue, ReadOnlySpanExtensions)"
                    "\n[<Sealed; Class>]\ntype K =\n    new: unit -> K\n    member X: int\n\nval after: int\n"
                    "\n[<Sealed>]\ntype K() =\n    member _.X = 1\n\nlet after = 1\n"

                conforms
                    "a member takes an argument name from its signature (BatchRecalculateDirty)"
                    "\ntype T =\n    new: unit -> T\n    member M: t: int -> int\n\nval after: int\n"
                    "\ntype T() =\n    member _.M(_: int) = 1\n\nlet after = 1\n"

                testAsync "a namespace fragment's module takes an arity from its signature (UntypedAstUtils)" {
                    let fsi =
                        "namespace Tiny\n\nmodule B =\n    val two: int\n    val moreThanOne: ('a list -> bool)\n\nnamespace Other\n\nmodule M =\n    val x: int\n"

                    let fs =
                        "namespace Tiny\n\nmodule B =\n    let two = Tiny.A.one + 1\n\n    let moreThanOne =\n        function\n        | []\n        | [ _ ] -> false\n        | _ -> true\n\nnamespace Other\n\nmodule M =\n    let x = 1\n"

                    let! c = check tinyProject [ "B.fsi", fsi; "B.fs", fs ]
                    do! expectOracle c
                    expectFinalIsProvisional c "B.fs"
                    expectPremise c
                } ]

          // What else of the finish broke the premise on the corpora: passes over the whole file that change a
          // declaration no other declaration takes part in
          testList
              "whole-file passes"
              [ testAsync "a solved unit of measure is normalized as the finish's defaults find it (normalizePathI)" {
                    let text =
                        "module Tiny.C\n\n[<Measure>]\ntype LocalPath\n\nlet inline normalizeI (x: int) : int<LocalPath> = LanguagePrimitives.Int32WithMeasure<LocalPath>(x + 1)\n\nlet inline inferred (x: int) = LanguagePrimitives.Int32WithMeasure<LocalPath> x\n\nlet after = 1\n"

                    let! c = check tinyProject [ "C.fs", text ]
                    do! expectOracle c
                    expectFinalIsProvisional c "C.fs"
                    expectPremise c
                }

                testAsync "an attribute's inference variable takes the finish's default (HashSet's DebuggerTypeProxy)" {
                    let text =
                        "module Tiny.C\n\n[<System.Diagnostics.DebuggerTypeProxy(typedefof<Proxy<_>>)>]\ntype Box<'T>(x: 'T) =\n    member _.X = x\n\nand internal Proxy<'T>(b: Box<'T>) =\n    member _.B = b\n\nlet after = 1\n"

                    let! c = check tinyProject [ "C.fs", text ]
                    do! expectOracle c
                    expectFinalIsProvisional c "C.fs"
                    expectPremise c
                } ]

          // The declared 'T of a generic class stays in the environment after it, rigid: it once flagged the class and
          // every later declaration of its file
          testAsync "ordinary generic, inline and SRTP code depends on nothing later" {
              let! c = check tinyProject [ "C.fs", ordinary ]
              do! expectOracle c
              expectPremise c
              Expect.equal (dependsOnLater c) [] "no declaration is flagged"

              for r in (c.File "C.fs").Results do
                  Expect.equal (c.Final("C.fs", r.Key.Name)) r.Fingerprint $"%s{r.Key.Name}: final = provisional"
          } ]
