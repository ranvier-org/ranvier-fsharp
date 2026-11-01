module Rfs.Decl.Tests.GuardTests

open System
open System.Threading
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

type private Tiny = { Spec: ProjectSpec; Context: ProjectContext }

let private tiny =
    lazy
        (async {
            let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
            let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
            return { Spec = spec; Context = ctx }
         }
         |> Async.StartAsTask)

/// C.fs: `body` after a module header, or `body` alone when it starts with its own namespace.
let private textOf (body: string) =
    if body.StartsWith "namespace" then body else "module Tiny.C\n\n" + body

/// C.fs checked one declaration at a time.
let private checkC (body: string) =
    async {
        let! t = tiny.Value |> Async.AwaitTask
        let files = declByDecl t.Context t.Spec (sources t.Spec (Map.ofList [ "C.fs", textOf body ]))
        return t, files |> List.find (fun f -> f.File.EndsWith "C.fs")
    }

let private rowOf (c: CheckedFile) (name: string) =
    c.Declarations |> Array.findIndex (fun d -> d.Key.Name = name)

/// The environment a fresh check gives row `i`, assembled from the file's setup env and the current results before it.
let private assembledEnv (c: CheckedFile) (results: DeclarationResult[]) (i: int) =
    results[.. i - 1] |> Array.fold (fun env result -> DeclarationChecker.ExtendEnv(env, result)) c.Env0

/// Re-checks `rows` of `decls` in ascending order, each in its assembled environment; the other results are kept.
let private recheck (c: CheckedFile) (decls: ParsedDeclaration[]) (results: DeclarationResult[]) (rows: int list) =
    let results = Array.copy results

    for i in List.sort rows do
        results[i] <- DeclarationChecker.CheckDeclaration(c.Start, assembledEnv c results i, decls[i], CancellationToken.None)

    results

let private parseC (t: Tiny) (c: CheckedFile) (body: string) =
    let parse = DeclarationChecker.Parse(t.Context, c.File, textOf body)
    let decls = DeclarationChecker.Split parse
    let keys (ds: ParsedDeclaration[]) = [ for d in ds -> d.Key ]
    Expect.equal (keys decls) (keys c.Declarations) "the edit keeps the keys"
    parse, decls

let private oracleDiff (t: Tiny) (body: string) (file: string) (fileResult: FileResult) =
    async {
        let! expected = oracle t.Spec (sources t.Spec (Map.ofList [ "C.fs", textOf body ])) file
        return expected, Diagnostics.compare (toLsp expected) (toLsp fileResult.Diagnostics)
    }

let private expectOracle (t: Tiny) (body: string) (file: string) (fileResult: FileResult) (what: string) =
    async {
        let! _, diff = oracleDiff t body file fileResult
        Expect.isEmpty diff $"%s{what}: diagnostics equal a full check:\n%s{String.concat Environment.NewLine diff}"
    }

let private hasError (number: int) (fileResult: FileResult) =
    fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = number)

[<Tests>]
let freeTyparTests =
    testList
        "Guard.FreeTypars"
        [ testAsync "ref [] leaves free typars" {
              let! _, c = checkC "let r = ref []\n"
              Expect.isTrue c.Results[0].LeavesFreeTypars "'_a list ref"
              let! _, c = checkC "let r = ref [1]\n"
              Expect.isFalse c.Results[0].LeavesFreeTypars "int list ref"
          }

          testAsync "free-typar decl re-check reproduces later solution" {
              let original = "let r = ref []\n\nlet f () = r.Value <- [1]\n"
              let edited = original.Replace("[1]", "[\"a\"]")
              let! t, c = checkC original
              do! expectOracle t original c.File c.FileResult "the original"
              Expect.isTrue c.Results[0].LeavesFreeTypars "r leaves its element type free"
              Expect.isTrue c.Results[0].DependsOnLaterDeclarations "so r depends on every later declaration"
              let parse, decls = parseC t c edited

              // f alone, after the old r: the old f solved r's element type to int, and nothing undid that.
              let alone = DeclarationChecker.CheckDeclaration(c.Start, c.Results[0].EnvAfter, decls[1], CancellationToken.None)
              let aloneFinished = DeclarationChecker.FinishFile(c.Start, [| c.Results[0]; alone |], parse)
              let! _, diff = oracleDiff t edited c.File aloneFinished
              Expect.isNonEmpty diff "re-checking f alone differs from a full check"

              // The guard: r leaves free typars, so a change after it re-checks r and everything after it.
              let guarded = recheck c decls c.Results [ 0; 1 ]
              let finished = DeclarationChecker.FinishFile(c.Start, guarded, parse)
              do! expectOracle t edited c.File finished "r and f re-checked"
          } ]

[<Tests>]
let unusedValueTests =
    testList
        "Guard.UnusedValues"
        [ testAsync "with --warnon:1182 a later use removed re-checks a value the signature leaves out" {
              let! t = tiny.Value |> Async.AwaitTask
              let spec = { t.Spec with OtherOptions = t.Spec.OtherOptions @ [ "--warnon:1182" ] }
              let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
              let original = "module Tiny.B\n\nlet two = Tiny.A.one + 1\n\nlet hidden = 1\n\nlet user = hidden + 1\n"
              let edited = original.Replace("let user = hidden + 1\n", "")
              let b = declByDecl ctx spec (sources spec (Map.ofList [ "B.fs", original ])) |> List.find (fun f -> f.File.EndsWith "B.fs")
              let hidden = b.Results[1]
              Expect.isFalse hidden.LeavesFreeTypars "no free typars"
              Expect.isTrue hidden.DependsOnLaterDeclarations "FS1182 is on, B.fs has a signature and hidden is a module value"

              let parse = DeclarationChecker.Parse(ctx, b.File, edited)
              let decls = DeclarationChecker.Split parse
              let! expected = oracle spec (sources spec (Map.ofList [ "B.fs", edited ])) b.File
              Expect.isTrue (expected |> Array.exists (fun d -> d.ErrorNumber = 1182)) "hidden is unused"

              let kept = DeclarationChecker.FinishFile(b.Start, [| b.Results[0]; hidden |], parse)
              Expect.isNonEmpty (Diagnostics.compare (toLsp expected) (toLsp kept.Diagnostics)) "keeping hidden's result misses the warning"

              let rechecked = recheck b decls b.Results[.. 1] [ 0; 1 ]
              let finished = DeclarationChecker.FinishFile(b.Start, rechecked, parse)
              let diff = Diagnostics.compare (toLsp expected) (toLsp finished.Diagnostics)
              Expect.isEmpty diff $"re-checked from the first such result:\n%s{String.concat Environment.NewLine diff}"
          } ]

[<Tests>]
let augmentationTests =
    testList
        "Guard.Augmentation"
        [ testAsync "augmentation names target" {
              let body =
                  "type T() =\n    member _.A = 1\n\ntype U() = class end\n\ntype T with\n    member _.B = 2\n\ntype U with\n    member _.C = 3\nand T with\n    member _.D = 4\n\nlet x = 1\n"

              let! t, c = checkC body
              do! expectOracle t body c.File c.FileResult "the file"
              let content i = DeclarationChecker.Content c.Declarations[i]
              Expect.equal (content 2).Augments [| "T" |] "the augmentation"
              Expect.equal (content 3).Augments [| "T"; "U" |] "a group augments every type in it"
              Expect.equal (content 0).Augments [||] "the type itself"
              Expect.equal (content 4).Augments [||] "a value"
          } ]

/// R13: B reaches type T only through a value. `edited` changes `target` and keeps every line. The rows in `byName`
/// re-check because they name the target, as the graph's name index finds; `user` then re-checks only if a declaration
/// in its TypedUses has a changed final fingerprint. The finish must equal a full check of the edited text.
let private typedDependency (original: string) (edited: string) (target: string) (byName: string list) (user: string) =
    async {
        let! t, c = checkC original
        do! expectOracle t original c.File c.FileResult "the original"
        let userRow = rowOf c user
        let typedUses = c.Results[userRow].TypedUses
        let targetRef: DeclarationRef = { File = c.File; Key = Some c.Declarations[rowOf c target].Key }
        Expect.contains typedUses targetRef $"%s{user}'s typed uses hold %s{target}"

        let parse, decls = parseC t c edited
        let byNameResults = recheck c decls c.Results [ for name in target :: byName -> rowOf c name ]
        let byNameFinished = DeclarationChecker.FinishFile(c.Start, byNameResults, parse)

        let changed (dependency: DeclarationRef) =
            match dependency.Key with
            | Some key when dependency.File = c.File -> byNameFinished.Fingerprints[key] <> c.FileResult.Fingerprints[key]
            | _ -> false

        Expect.isTrue (typedUses |> Array.exists changed) $"a typed use of %s{user} changed its fingerprint, so it re-checks"

        let finished = DeclarationChecker.FinishFile(c.Start, recheck c decls byNameResults [ userRow ], parse)
        do! expectOracle t edited c.File finished $"%s{user} re-checked"

        // Why the edge is needed: the old result of the user, kept, does not give the full check's diagnostics.
        let! _, staleDiff = oracleDiff t edited c.File byNameFinished
        Expect.isNonEmpty staleDiff $"keeping the old %s{user} differs from a full check"
    }

[<Tests>]
let typedDependencyTests =
    testList
        "Guard.TypedUses"
        [ testAsync "NoEquality added to T" {
              let original = "type T = { X: int }\n\nlet c = { X = 1 }\n\nlet b = c = c\n\nlet z = 0\n"
              do! typedDependency original (original.Replace("type T =", "[<NoEquality; NoComparison>] type T =")) "T" [ "c" ] "b"
          }

          testAsync "an interface removed from T" {
              let original =
                  "type T() =\n    interface System.IDisposable with member _.Dispose() = ()\n\nlet c = new T()\n\nlet b = (c :> System.IDisposable)\n\nlet z = 0\n"

              let edited = original.Replace("interface System.IDisposable with member _.Dispose() = ()", "member _.Dispose() = ()")
              do! typedDependency original edited "T" [ "c" ] "b"
          }

          testAsync "op_Implicit removed" {
              let original =
                  "type T =\n    | T of int\n    static member op_Implicit(t: T) : int = match t with T n -> n\n\nlet c = T 1\n\ntype K =\n    static member Take(x: int) = x\n\nlet b = K.Take c\n\nlet z = 0\n"

              do! typedDependency original (original.Replace("op_Implicit", "Other")) "T" [ "c" ] "b"
          }

          testAsync "Item changed" {
              let original = "type T() =\n    member _.Item with get (i: int) = i\n\nlet c = T()\n\nlet b = c[0] + 1\n\nlet z = 0\n"
              do! typedDependency original (original.Replace("= i\n", "= string i\n")) "T" [ "c" ] "b"
          }

          testAsync "an SRTP member removed" {
              let original =
                  "type T() =\n    member _.M = 1\n\nlet c = T()\n\nlet inline callM (x: ^a) = (^a: (member M: int) x)\n\nlet b = callM c\n\nlet z = 0\n"

              do! typedDependency original (original.Replace("member _.M = 1", "member _.N = 1")) "T" [ "c" ] "b"
          }

          testAsync "AllowNullLiteral added" {
              let original = "type T() = class end\n\nlet c = T()\n\nlet b = isNull c\n\nlet z = 0\n"
              do! typedDependency original (original.Replace("type T()", "[<AllowNullLiteral>] type T()")) "T" [ "c" ] "b"
          }

          testAsync "error recovery keeps the typed use" {
              let original = "type T() =\n    member _.M = 1\n\nlet c = T()\n\nlet b = c[0]\n\nlet z = 0\n"
              do! typedDependency original (original.Replace("member _.M = 1", "member _.Item with get (i: int) = i")) "T" [ "c" ] "b"
          }

          // Through T's field type: B's typed uses are closed over the types a type is made of.
          testAsync "transitive structural" {
              let original = "type U = { N: int }\n\ntype T = { U: U }\n\nlet t = { U = { N = 1 } }\n\nlet b = t = t\n\nlet z = 0\n"
              do! typedDependency original (original.Replace("type U =", "[<NoEquality; NoComparison>] type U =")) "U" [ "T"; "t" ] "b"
          }

          // T's own fingerprint does not show what its base type implements
          testAsync "an interface removed from T's base type" {
              let original =
                  "type U() =\n    interface System.IDisposable with member _.Dispose() = ()\n\ntype T() =\n    inherit U()\n\nlet c = new T()\n\nlet b = (c :> System.IDisposable)\n\nlet z = 0\n"

              let edited = original.Replace("interface System.IDisposable with member _.Dispose() = ()", "member _.Dispose() = ()")
              do! typedDependency original edited "U" [ "T"; "c" ] "b"
          } ]

/// Every row of `body` checked again in the environment folded with ExtendEnv from the results before it, none
/// chained: the finish equals the full check in diagnostics, in what every name resolves to, and in fingerprints.
let private foldedMatchesFullCheck (body: string) =
    async {
        let! t, c = checkC body
        do! expectOracle t body c.File c.FileResult "the full check"

        let again =
            Array.init c.Results.Length (fun i ->
                DeclarationChecker.CheckDeclaration(c.Start, assembledEnv c c.Results i, c.Declarations[i], CancellationToken.None))

        let finished = DeclarationChecker.FinishFile(c.Start, again, c.Start.Parse)
        do! expectOracle t body c.File finished "assembled environments"

        let uses (fileResult: FileResult) =
            [ for u in fileResult.CheckResults.GetAllUsesOfAllSymbolsInFile() -> u.Symbol.FullName, string u.Range, u.IsFromDefinition ]
            |> List.sort

        Expect.equal (uses finished) (uses c.FileResult) "every name resolves as in the full check"

        let fingerprints (fileResult: FileResult) =
            [ for KeyValue(key, fingerprint) in fileResult.Fingerprints -> key, fingerprint ] |> List.sortBy fst

        Expect.equal (fingerprints finished) (fingerprints c.FileResult) "same fingerprints"
        return c
    }

[<Tests>]
let assembledEnvTests =
    testList
        "Guard.ExtendEnv"
        [ testAsync "an edited value reaches a later user through an unrelated declaration" {
              let original = "let g = 1\n\nlet d = 0\n\nlet e = g + 1\n"
              let edited = original.Replace("let g = 1", "let g = \"s\"")
              let! t, c = checkC original
              let parse, decls = parseC t c edited
              let g' = DeclarationChecker.CheckDeclaration(c.Start, c.Env0, decls[0], CancellationToken.None)

              // d's old EnvAfter holds the old int g: e checked there misses the type error.
              let stale = DeclarationChecker.CheckDeclaration(c.Start, c.Results[1].EnvAfter, decls[2], CancellationToken.None)
              let! _, staleDiff = oracleDiff t edited c.File (DeclarationChecker.FinishFile(c.Start, [| g'; c.Results[1]; stale |], parse))
              Expect.isNonEmpty staleDiff "e against d's stale environment differs from a full check"

              let envE = DeclarationChecker.ExtendEnv(DeclarationChecker.ExtendEnv(c.Env0, g'), c.Results[1])
              let e' = DeclarationChecker.CheckDeclaration(c.Start, envE, decls[2], CancellationToken.None)
              let finished = DeclarationChecker.FinishFile(c.Start, [| g'; c.Results[1]; e' |], parse)
              do! expectOracle t edited c.File finished "g and e re-checked"
              Expect.isTrue (e'.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "FS0001 at e"
          }

          testAsync "the same through a nested module" {
              let original = "module M =\n    let g = 1\n\nlet d = 0\n\nlet e = M.g + 1\n"
              let edited = original.Replace("let g = 1", "let g = \"s\"")
              let! t, c = checkC original
              let parse, decls = parseC t c edited
              let results = recheck c decls c.Results [ rowOf c "g"; rowOf c "e" ]
              let finished = DeclarationChecker.FinishFile(c.Start, results, parse)
              do! expectOracle t edited c.File finished "g and e re-checked"
              Expect.isTrue (hasError 1 finished) "FS0001 at e"
          }

          testAsync "folding ExtendEnv over the results gives the full check's name resolution" {
              let body =
                  "open System\n\nmodule M =\n    let g = 1\n    type Inner = { I: int }\n\nopen M\n\ntype T = { X: int }\n\nlet v = g + Math.Max(1, 2) + { X = 1 }.X\n\nlet w = { I = v }\n\nlet s: string = w.I\n"

              let! c = foldedMatchesFullCheck body
              Expect.isTrue (hasError 1 c.FileResult) "s has a type error"
          }

          testAsync "the same over namespace fragments" {
              let body =
                  "namespace Tiny.X\n\nmodule C =\n    let three = Tiny.B.two + 1\n\nnamespace Tiny.Y\n\nopen Tiny.X\n\nmodule D =\n    let four: string = C.three + 1\n"

              let! c = foldedMatchesFullCheck body
              Expect.isTrue (hasError 1 c.FileResult) "four has a type error"
          }

          // A namespace global fragment leaves what it opened and defined in the environment after it
          testAsync "the same over namespace global fragments" {
              let body =
                  "namespace global\n\nopen System\n\ntype G = { V: int }\n\nmodule N =\n    let n = 1\n\nnamespace global\n\nmodule M =\n    let m: string = Math.Max({ V = 1 }.V, N.n)\n"

              let! c = foldedMatchesFullCheck body
              Expect.equal [ for d in c.FileResult.Diagnostics -> d.ErrorNumber ] [ 193 ] "only m's type error: Math, G and N resolve"
          } ]
