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

/// An edit after the finish, as the graph makes one: the declarations of `edited` whose text differs from `c`'s are
/// checked again on c's FileStart, in the environment c's results leave before them. Each re-check equals a fresh
/// check of the edited text. With `seen` (the edit changes what the declarations after it see, as the conformance
/// check overwrites an argument name, an arity or an attribute) its fingerprint differs from the one before the edit,
/// so they are checked again; without, it is the same. The last declaration, checked again unedited, keeps its
/// fingerprint.
let private expectEditRechecksWith (seen: bool) (c: Checked) (edited: Checked) (file: string) =
    let before, after = c.File file, edited.File file
    let parse = DeclarationChecker.Parse(c.Context, before.File, edited.Texts[before.File])
    let decls = DeclarationChecker.Split parse
    Expect.equal decls.Length before.Declarations.Length "the edit keeps the declarations"
    let envBefore i = if i = 0 then before.Env0 else before.Results[i - 1].EnvAfter
    let mutable changed = 0

    for i in 0 .. decls.Length - 1 do
        if decls[i].Text <> before.Declarations[i].Text then
            changed <- changed + 1
            let again = DeclarationChecker.CheckDeclaration(before.Start, envBefore i, decls[i], Threading.CancellationToken.None)
            let name = decls[i].Key.Name
            if seen then
                Expect.notEqual again.Fingerprint before.Results[i].Fingerprint $"%s{name}: the edit changes the cutoff for what follows"
            else
                Expect.equal again.Fingerprint before.Results[i].Fingerprint $"%s{name}: the edit keeps the cutoff for what follows"

            Expect.equal again.Fingerprint after.Results[i].Fingerprint $"%s{name}: checked again after the finish = checked fresh"
            let diff = Diagnostics.compare (toLsp after.Results[i].Diagnostics) (toLsp again.Diagnostics)
            Expect.isEmpty diff $"%s{name}: the same diagnostics:\n%s{String.concat Environment.NewLine diff}"

    Expect.isGreaterThan changed 0 "the edit changes a declaration"
    let last = decls.Length - 1
    let again = DeclarationChecker.CheckDeclaration(before.Start, envBefore last, before.Declarations[last], Threading.CancellationToken.None)
    Expect.equal again.Fingerprint before.Results[last].Fingerprint "the last declaration checked again keeps its fingerprint"

let private expectEditRechecks = expectEditRechecksWith true

/// What checking B.fs against B.fsi changes in a declaration is in its fingerprint from its check on; and `edited`,
/// which changes only what the conformance check overwrites, is checked again as expectEditRechecksWith `seen` says.
let private conformsWith (seen: bool) (name: string) (fsi: string) (fs: string) (edited: string) =
    testList
        name
        [ testAsync "checked cold" {
              let! c = withSignature fsi fs
              do! expectOracle c
              expectFinalIsProvisional c "B.fs"
              expectPremise c
          }

          testAsync "edited and checked again" {
              let! c = withSignature fsi fs
              let! e = withSignature fsi edited
              do! expectOracle e
              expectFinalIsProvisional e "B.fs"
              expectEditRechecksWith seen c e "B.fs"
          } ]

let private conforms = conformsWith true

/// What InteractiveChecker's check of the file gives, for its queries.
let private referenceResults (c: Checked) (file: string) =
    async {
        let checker = FSharp.Compiler.SourceCodeServices.InteractiveChecker.Create(ProjectSpec.toFcsOptions c.Spec)
        let sourceReader f = let t = c.Texts[f] in hash t, lazy t
        let path = (c.File file).File
        let! _, results, _ = checker.ParseAndCheckFileInProject(c.Spec.ProjectFile, Array.ofList c.Spec.SourceFiles, sourceReader, path)
        return results
    }

/// A flat module of `n` values, in B.fs with B.fsi (`signed`) or in C.fs without a signature: the time to check it
/// cold by declaration, and the time to check its last declaration again (the best of 20).
let private flatModule (n: int) (signed: bool) =
    async {
        let values = String.concat "\n" [ for i in 1..n -> $"let v%d{i} (x: int) = x + %d{i}" ]

        let overrides =
            if signed then
                [ "B.fsi", "module Tiny.B\n\nval two: int\n" + String.concat "\n" [ for i in 1..n -> $"val v%d{i}: x: int -> int" ] + "\n"
                  "B.fs", "module Tiny.B\n\nlet two = Tiny.A.one + 1\n" + values + "\n" ]
            else
                [ "C.fs", "module Tiny.C\n\nlet three = Tiny.B.two + 1\n" + values + "\n" ]

        let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
        let spec = ProjectSpec.sequential spec
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
        let texts = sources spec (Map.ofList overrides)
        let target = if signed then "B.fs" else "C.fs"
        let mutable before = []
        let mutable timed = 0.0, 0.0

        for file in spec.SourceFiles do
            let parse = DeclarationChecker.Parse(ctx, file, texts[file])

            if file.EndsWith ".fsi" then
                before <- before @ [ DeclarationChecker.CheckSignatureFile(ctx, before, parse) ]
            else
                let start, env0 = DeclarationChecker.BeginFile(ctx, before, parse)
                let decls = DeclarationChecker.Split parse
                let clock = Diagnostics.Stopwatch.StartNew()

                let results =
                    decls
                    |> Array.mapFold
                        (fun env decl ->
                            let r = DeclarationChecker.CheckDeclaration(start, env, decl, Threading.CancellationToken.None)
                            r, r.EnvAfter)
                        env0
                    |> fst

                let cold = clock.Elapsed.TotalMilliseconds

                if Path.GetFileName file = target then
                    let last = decls.Length - 1

                    let again =
                        [ for _ in 1..20 ->
                              let clock = Diagnostics.Stopwatch.StartNew()
                              DeclarationChecker.CheckDeclaration(start, results[last - 1].EnvAfter, decls[last], Threading.CancellationToken.None) |> ignore
                              clock.Elapsed.TotalMilliseconds ]
                        |> List.min

                    timed <- cold, again

                before <- before @ [ (DeclarationChecker.FinishFile(start, results, parse)).Signature ]

        return timed
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
              "windows"
              [ testAsync "a window's variables close at their solver, and the close print is the final fingerprint" {
                    let! c = check tinyProject []
                    let file = c.Spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "A.fs")
                    let text = "module Tiny.A\n\nlet r = ref []\n\nlet s = ref []\n\nlet link () = r.Value <- s.Value\n\nlet fin () = s.Value <- [ 1 ]\n\nlet z = 1\n"
                    let parse = DeclarationChecker.Parse(c.Context, file, text)
                    let start, env0 = DeclarationChecker.BeginFile(c.Context, [], parse)
                    let ct = System.Threading.CancellationToken.None

                    let results, _ =
                        DeclarationChecker.Split parse
                        |> Array.mapFold
                            (fun env decl ->
                                let result = DeclarationChecker.CheckDeclaration(start, env, decl, ct)
                                result, DeclarationChecker.ExtendEnv(env, result))
                            env0

                    for r in results do
                        Expect.equal (r.DescribeOpenTypars().Length > 0) r.DependsOnLaterDeclarations $"%s{r.Key.Name}: open variables exactly when flagged"

                    let upTo (name: string) =
                        let last = results |> Array.findIndex (fun r -> r.Key.Name = name)
                        results[0..last] |> Array.filter _.DependsOnLaterDeclarations

                    // The checks above are done, so everything is solved: what they watch is what stayed open
                    Expect.isFalse (DeclarationChecker.WindowOpen(start, upTo "fin", ct)) "fin solves s, and through it r"
                    let flagged = results |> Array.filter _.DependsOnLaterDeclarations
                    Expect.isNonEmpty flagged "r and s are open at their checks"
                    let prints = DeclarationChecker.PrintsNow(start, flagged)
                    let finished = DeclarationChecker.FinishFile(start, results, parse)

                    for r, print in Array.zip flagged prints do
                        Expect.equal finished.Fingerprints[r.Key] print $"%s{r.Key.Name}: the final fingerprint is the close print"

                    Expect.isFalse (DeclarationChecker.WindowOpen(start, flagged, ct)) "still closed with the finish undone"
                }

                testAsync "the finish's defaults do not close a window" {
                    let! c = check tinyProject []
                    let file = c.Spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "A.fs")
                    let parse = DeclarationChecker.Parse(c.Context, file, "module Tiny.A\n\nlet r = ref []\n\nlet z = 1\n")
                    let start, env0 = DeclarationChecker.BeginFile(c.Context, [], parse)
                    let ct = System.Threading.CancellationToken.None

                    let results, _ =
                        DeclarationChecker.Split parse
                        |> Array.mapFold
                            (fun env decl ->
                                let result = DeclarationChecker.CheckDeclaration(start, env, decl, ct)
                                result, DeclarationChecker.ExtendEnv(env, result))
                            env0

                    let flagged = results |> Array.filter _.DependsOnLaterDeclarations
                    Expect.isTrue (DeclarationChecker.WindowOpen(start, flagged, ct)) "nothing solved r"
                    DeclarationChecker.FinishFile(start, results, parse) |> ignore
                    Expect.isTrue (DeclarationChecker.WindowOpen(start, flagged, ct)) "and the finish's defaults do not count"
                } ]

          testList
              "signature conformance"
              [ conforms
                    "a value bound to a lambda takes its arity from a signature without arguments (List.moreThanOne)"
                    "val moreThanOne: ('a list -> bool)\nval after: int\n"
                    "\nlet moreThanOne =\n    function\n    | []\n    | [ _ ] -> false\n    | _ -> true\n\nlet after = 1\n"
                    "\nlet moreThanOne l =\n    match l with\n    | []\n    | [ _ ] -> false\n    | _ -> true\n\nlet after = 1\n"

                conforms
                    "a partial active pattern bound to a lambda (|Val|_|)"
                    "val (|Val|_|): (int option -> int option)\nval after: int\n"
                    "\nlet (|Val|_|) =\n    function\n    | Some i when i > 0 -> Some i\n    | _ -> None\n\nlet after = 1\n"
                    "\nlet (|Val|_|) x =\n    match x with\n    | Some i when i > 0 -> Some i\n    | _ -> None\n\nlet after = 1\n"

                conforms
                    "a partial active pattern with an argument returning a union case (|StaticParameter|_|)"
                    "val (|Wrap|_|): _x: obj -> ('a -> 'a option)\nval after: int\n"
                    "\nlet (|Wrap|_|) (_x: obj) = Some\n\nlet after = 1\n"
                    "\nlet (|Wrap|_|) (_x: obj) y = Some y\n\nlet after = 1\n"

                conforms
                    "a partial active pattern with an argument (|Val|_|) (x: obj)"
                    "val (|Val|_|): x: obj -> int option\nval after: int\n"
                    "\nlet (|Val|_|) (x: obj) =\n    match x with\n    | :? int as i -> Some i\n    | _ -> None\n\nlet after = 1\n"
                    "\nlet (|Val|_|) (y: obj) =\n    match y with\n    | :? int as i -> Some i\n    | _ -> None\n\nlet after = 1\n"

                conforms
                    "an interface rec group takes [<Interface>] from its signature (IAdaptiveValue)"
                    "\n[<Interface>]\ntype IA =\n    abstract M: IB -> int\n\nand [<Interface>] IB =\n    abstract N: IA -> int\n\nval after: int\n"
                    "\ntype IA =\n    abstract M: IB -> int\n\nand IB =\n    abstract N: IA -> int\n\nlet after = 1\n"
                    "\n[<Interface>]\ntype IA =\n    abstract M: IB -> int\n\nand IB =\n    abstract N: IA -> int\n\nlet after = 1\n"

                conforms
                    "a class takes [<Class>] from its signature (ChangeableValue, ReadOnlySpanExtensions)"
                    "\n[<Sealed; Class>]\ntype K =\n    new: unit -> K\n    member X: int\n\nval after: int\n"
                    "\n[<Sealed>]\ntype K() =\n    member _.X = 1\n\nlet after = 1\n"
                    "\n[<Sealed; Class>]\ntype K() =\n    member _.X = 1\n\nlet after = 1\n"

                conforms
                    "a member takes an argument name from its signature (BatchRecalculateDirty)"
                    "\ntype T =\n    new: unit -> T\n    member M: t: int -> int\n\nval after: int\n"
                    "\ntype T() =\n    member _.M(_: int) = 1\n\nlet after = 1\n"
                    "\ntype T() =\n    member _.M(t: int) = t\n\nlet after = 1\n"

                // What the declarations after it report can show a type parameter by the name its user wrote (an
                // overload list does), so the name is part of what they see
                conforms
                    "a type parameter takes its name from its signature"
                    "val g: x: 'a -> 'a\nval after: int\n"
                    "\nlet g (x: 'b) = x\n\nlet after = 1\n"
                    "\nlet g (x: 'a) = x\n\nlet after = 1\n"

                // C2 of the Task 6 review: the finish's conformance is undone by the next check of the file, and its
                // check results, read after that, apply it again
                // I1 of the Task 6 re-review: building the results applies the conformance again, and must not land
                // between a re-check's undo of the finish and its check
                testAsync "building a finish's check results waits for a re-check that undid the finish" {
                    let fsi = "\ntype T =\n    new: unit -> T\n    member M: t: int -> int\n\nval use1: int\n"
                    let fs = "\ntype T() =\n    member _.M(_: int) = 1\n\nlet use1 = T().M(t = 1)\n"
                    let! c = withSignature fsi fs
                    let f = c.File "B.fs"
                    let last = f.Declarations.Length - 1
                    let cold = f.Results[last]
                    Expect.isNonEmpty cold.Diagnostics "useM's FS0495 against the unconformed T"
                    use inGap = new Threading.ManualResetEventSlim()
                    use built = new Threading.ManualResetEventSlim()
                    let mutable recheck = Unchecked.defaultof<DeclarationResult>

                    let checker =
                        Threading.Thread(fun () ->
                            recheck <- DeclarationChecker.CheckDeclaration(f.Start, f.Results[last - 1].EnvAfter, f.Declarations[last], Threading.CancellationToken.None))

                    // Only on this test's re-check: other tests check Tiny's B.fs meanwhile. Once the results are built
                    // the check goes on; when the build has to wait for the check, it goes on after a while.
                    DeclarationChecker.AfterUndoHook <-
                        fun _ ->
                            if Threading.Thread.CurrentThread.ManagedThreadId = checker.ManagedThreadId then
                                inGap.Set()
                                built.Wait(TimeSpan.FromSeconds 2.) |> ignore

                    try
                        checker.Start()
                        Expect.isTrue (inGap.Wait(TimeSpan.FromSeconds 60.)) "the re-check undid the finish"

                        let reader =
                            Threading.Tasks.Task.Run(fun () ->
                                f.FileResult.CheckResults |> ignore
                                built.Set())

                        checker.Join()
                        do! reader |> Async.AwaitTask
                    finally
                        DeclarationChecker.AfterUndoHook <- ignore

                    Expect.equal recheck.Fingerprint cold.Fingerprint "the re-check sees T unconformed, as the cold check did"
                    let diff = Diagnostics.compare (toLsp cold.Diagnostics) (toLsp recheck.Diagnostics)
                    Expect.isEmpty diff $"the re-check keeps FS0495:\n%s{String.concat Environment.NewLine diff}"
                }

                testAsync "a finish's check results, read after a later check, show the implementation conformed" {
                    let fsi = "\ntype T =\n    new: unit -> T\n    /// From the signature\n    member M: t: int -> int\n\nval use1: int\nval after: int\n"
                    let fs = "\ntype T() =\n    member _.M(_: int) = 1\n\nlet use1 = T().M(1)\n\nlet after = use1 + 1\n"
                    let! c = withSignature fsi fs
                    let f = c.File "B.fs"
                    let last = f.Declarations.Length - 1
                    DeclarationChecker.CheckDeclaration(f.Start, f.Results[last - 1].EnvAfter, f.Declarations[last], Threading.CancellationToken.None) |> ignore
                    let! expected = referenceResults c "B.fs"
                    let text = c.Texts[f.File]
                    expectLikeReference 2 text expected f.FileResult.CheckResults "B.fs, read after a later check"
                    let lines = text.Split '\n'
                    let line = lines |> Array.findIndex (fun l -> l.StartsWith "let use1")
                    let column = lines[line].IndexOf "M(" + 1

                    let location (results: FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults) =
                        sprintf "%A" (results.GetDeclarationLocation(line + 1, column, lines[line], [ "M" ], false))

                    Expect.stringContains (location expected) "DeclFound" "the reference finds M's declaration"
                    Expect.equal (location f.FileResult.CheckResults) (location expected) "go to M's declaration"

                    let tip (results: FSharp.Compiler.CodeAnalysis.FSharpCheckFileResults) =
                        tooltipText (results.GetToolTip(line + 1, column, lines[line], [ "M" ], FSharp.Compiler.Tokenization.FSharpTokenTag.Identifier))

                    Expect.stringContains (tip expected) "t: int" "the reference shows the argument name the signature gives"
                    Expect.equal (tip f.FileResult.CheckResults) (tip expected) "M's tooltip"
                }

                testAsync "a namespace fragment's module takes an arity from its signature (UntypedAstUtils)" {
                    let fsi =
                        "namespace Tiny\n\nmodule B =\n    val two: int\n    val moreThanOne: ('a list -> bool)\n\nnamespace Other\n\nmodule M =\n    val x: int\n"

                    let fs =
                        "namespace Tiny\n\nmodule B =\n    let two = Tiny.A.one + 1\n\n    let moreThanOne =\n        function\n        | []\n        | [ _ ] -> false\n        | _ -> true\n\nnamespace Other\n\nmodule M =\n    let x = 1\n"

                    let! c = check tinyProject [ "B.fsi", fsi; "B.fs", fs ]
                    do! expectOracle c
                    expectFinalIsProvisional c "B.fs"
                    expectPremise c
                    let edited = fs.Replace("let moreThanOne =\n        function", "let moreThanOne l =\n        match l with")
                    let! e = check tinyProject [ "B.fsi", fsi; "B.fs", edited ]
                    do! expectOracle e
                    expectEditRechecks c e "B.fs"
                } ]

          // I1 of the Task 6 review: the conformance check at each declaration's check is proportional to the
          // declaration, not to the module it is in
          testAsync "checking a flat module with a signature file costs about what it does without one" {
              // Each warmed up, then three times, alternating, keeping the fastest run: a busy machine slows a run,
              // not all of them
              let! _ = flatModule 2000 true
              let! _ = flatModule 2000 false
              let signed = ResizeArray()
              let plain = ResizeArray()

              for _ in 1..3 do
                  let! s = flatModule 2000 true
                  signed.Add s
                  let! p = flatModule 2000 false
                  plain.Add p

              let best (runs: ResizeArray<float * float>) = (runs |> Seq.map fst |> Seq.min), (runs |> Seq.map snd |> Seq.min)
              let signedCold, signedAgain = best signed
              let plainCold, plainAgain = best plain
              printfn $"2000 values: cold %.0f{signedCold} ms with B.fsi, %.0f{plainCold} ms without; again %.3f{signedAgain} ms, %.3f{plainAgain} ms"
              Expect.isLessThanOrEqual signedCold (1.5 * plainCold) $"cold by declaration: %.0f{signedCold} ms with a signature, %.0f{plainCold} ms without"
              Expect.isLessThanOrEqual signedAgain (2.0 * plainAgain) $"a declaration checked again: %.3f{signedAgain} ms with a signature, %.3f{plainAgain} ms without"
          }

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
