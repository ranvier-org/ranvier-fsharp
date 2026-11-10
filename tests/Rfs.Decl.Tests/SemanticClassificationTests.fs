module Rfs.Decl.Tests.SemanticClassificationTests

#nowarn "57"

open System
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Rfs.Decl.Tests.Common
open Rfs.Projects

let private depsDir = Path.Combine(Path.GetDirectoryName tinyDir, "Deps")
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")

let private normalize (items: SemanticClassificationItem[]) =
    items
    |> Array.map (fun item ->
        let r = item.Range
        r.StartLine, r.StartColumn, r.EndLine, r.EndColumn, item.Type)
    |> Array.sort
    |> Array.distinct

let private oracleClassification (spec: ProjectSpec) (texts: Map<string, string>) (file: string) (range: range option) =
    async {
        let docSource =
            DocumentSource.Custom(fun path ->
                async {
                    return
                        texts
                        |> Map.tryPick (fun k v ->
                            if String.Equals(Path.GetFullPath k, Path.GetFullPath path, StringComparison.OrdinalIgnoreCase) then
                                Some(SourceText.ofString v)
                            else
                                None)
                })

        let checker = FSharpChecker.Create(documentSource = docSource)
        let options = ProjectSpec.toFcsOptions spec
        let! _, answer = checker.ParseAndCheckFileInProject(file, 0, SourceText.ofString texts[file], options)

        match answer with
        | FSharpCheckFileAnswer.Succeeded checkResults -> return checkResults.GetSemanticClassification(range)
        | FSharpCheckFileAnswer.Aborted -> return failtest $"FSharpChecker aborted on %s{file}"
    }
    |> Async.RunSynchronously

[<Tests>]
let tests =
    testSequenced
    <| testList
        "SemanticClassification"
        [ testCase "CheckFile and FinishFile match InteractiveChecker for printf, interpolated format specifiers, and sub-ranges"
          <| fun () ->
              let spec = ProjectSpec.load depsProject |> Async.AwaitTask |> Async.RunSynchronously |> ProjectSpec.sequential
              let a = spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "A.fs")

              let aText =
                  """module Deps.A

let baseValue = 10

let formatted (name: string) (score: float) =
    let msg1 = sprintf "hello %s: %d / %0.2f %%" name baseValue score
    let msg2 = $"interpolated %04d{baseValue} and %s{name} ({score})"
    msg1 + msg2
"""

              let texts = sources spec (Map.ofList [ "A.fs", aText ])
              let expectedAll = oracleClassification spec texts a None |> normalize

              Expect.isTrue
                  (expectedAll |> Array.exists (fun (_, _, _, _, ty) -> ty = SemanticClassificationType.Printf))
                  "oracle classification must include Printf items"

              let subRange =
                  Range.mkRange a (Position.mkPos 5 0) (Position.mkPos 7 80)

              let expectedSub = oracleClassification spec texts a (Some subRange) |> normalize

              // 1. Whole-file CheckFile parity + non-retention of CheckResults
              let ctxWhole =
                  DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
                  |> Async.RunSynchronously

              let parseA = DeclarationChecker.Parse(ctxWhole, a, aText)
              let wholeResult = DeclarationChecker.CheckFile(ctxWhole, [], parseA)
              Expect.equal ctxWhole.CheckResultsBuilt 0 "CheckFile itself builds no CheckResults"

              let actualWhole1 = wholeResult.GetSemanticClassification() |> normalize
              Expect.equal ctxWhole.CheckResultsBuilt 1 "first GetSemanticClassification uses CheckResultsNotKept"
              let actualWhole2 = wholeResult.GetSemanticClassification(subRange) |> normalize
              Expect.equal ctxWhole.CheckResultsBuilt 2 "second GetSemanticClassification builds transiently without retaining CheckResults"

              Expect.equal actualWhole1 expectedAll "CheckFile full semantic classification matches InteractiveChecker"
              Expect.equal actualWhole2 expectedSub "CheckFile ranged semantic classification matches InteractiveChecker"

              // Once CheckResults is explicitly forced, GetSemanticClassification reuses it without rebuilding
              let _ = wholeResult.CheckResults
              Expect.equal ctxWhole.CheckResultsBuilt 3 "reading CheckResults builds once"
              let actualWhole3 = wholeResult.GetSemanticClassification() |> normalize
              Expect.equal ctxWhole.CheckResultsBuilt 3 "GetSemanticClassification reuses already-created CheckResults"
              Expect.equal actualWhole3 expectedAll "classification after CheckResults matches InteractiveChecker"

              // 2. Initial declaration-by-declaration FinishFile parity + lock guard
              let ctxDecl =
                  DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
                  |> Async.RunSynchronously

              let checkedFiles = declByDecl ctxDecl spec texts
              let checkedA = checkedFiles |> List.find (fun f -> f.File = a)
              let actualFinish = checkedA.FileResult.GetSemanticClassification() |> normalize
              let actualFinishSub = checkedA.FileResult.GetSemanticClassification(subRange) |> normalize

              Expect.equal ctxDecl.CheckResultsBuilt 0 "FinishFile GetSemanticClassification never calls buildCheckResults"
              Expect.equal actualFinish expectedAll "FinishFile full semantic classification matches InteractiveChecker"
              Expect.equal actualFinishSub expectedSub "FinishFile ranged semantic classification matches InteractiveChecker"

              Expect.throwsT<InvalidOperationException>
                  (fun () ->
                      ctxDecl.HoldingCheckLock(a, fun () -> checkedA.FileResult.GetSemanticClassification() |> ignore))
                  "FinishFile GetSemanticClassification enforces m2 lock order via beforeRead"

          testCase "edited and shifted declarations after BeginFile preserve exact printf format specifier ranges"
          <| fun () ->
              let spec = ProjectSpec.load depsProject |> Async.AwaitTask |> Async.RunSynchronously |> ProjectSpec.sequential
              let a = spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "A.fs")

              let initialText =
                  """module Deps.A

let baseValue = 10

let midFormatted (x: int) =
    sprintf "orig %d" x

let tailFormatted (y: string) =
    $"tail %s{y} %04d{baseValue}"
"""

              let ctx =
                  DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
                  |> Async.RunSynchronously

              let parse0 = DeclarationChecker.Parse(ctx, a, initialText)
              let start, env0 = DeclarationChecker.BeginFile(ctx, [], parse0)
              let decls0 = DeclarationChecker.Split parse0

              let results0 =
                  decls0
                  |> Array.mapFold
                      (fun env d ->
                          let r = DeclarationChecker.CheckDeclaration(start, env, d, CancellationToken.None)
                          r, DeclarationChecker.ExtendEnv(env, r))
                      env0
                  |> fst

              let _ = DeclarationChecker.FinishFile(start, results0, parse0)

              // Edit declaration 1 (changing line count & columns so start.Parse has completely different offsets!)
              // and shift declaration 2 down without re-typechecking it.
              let editedText =
                  """module Deps.A

let baseValue = 10

let midFormatted (x: int) =
    let prefix = "new-line-shifted"
    let first = sprintf "edited-prefix %s and %06d %%" prefix x
    let second = $"interp %x{x}"
    first + second

let tailFormatted (y: string) =
    $"tail %s{y} %04d{baseValue}"
"""

              let parse1 = DeclarationChecker.Parse(ctx, a, editedText)
              let decls1 = DeclarationChecker.Split parse1
              Expect.equal decls1.Length 3 "still three declarations"

              let envAfter0 = DeclarationChecker.ExtendEnv(env0, results0[0])
              let rMid = DeclarationChecker.CheckDeclaration(start, envAfter0, decls1[1], CancellationToken.None)

              let rTail =
                  match DeclarationChecker.Shift(results0[2], decls1[2]) with
                  | ValueSome shifted -> shifted
                  | ValueNone -> failtest "expected tailFormatted declaration to shift cleanly"

              let finished1 = DeclarationChecker.FinishFile(start, [| results0[0]; rMid; rTail |], parse1)
              let actualEdited = finished1.GetSemanticClassification() |> normalize

              let texts1 = sources spec (Map.ofList [ "A.fs", editedText ])
              let expectedEdited = oracleClassification spec texts1 a None |> normalize

              let printfRanges items =
                  items |> Array.filter (fun (_, _, _, _, ty) -> ty = SemanticClassificationType.Printf)

              Expect.isNonEmpty (printfRanges expectedEdited) "oracle has printf classifications in edited file"
              Expect.equal (printfRanges actualEdited) (printfRanges expectedEdited) "edited + shifted printf ranges match InteractiveChecker"
              Expect.equal actualEdited expectedEdited "full classification after edit + shift matches InteractiveChecker"

          testCase "FinishFile tail sink classifications (defaulted SRTP trait constraints) match InteractiveChecker"
          <| fun () ->
              let spec = ProjectSpec.load depsProject |> Async.AwaitTask |> Async.RunSynchronously |> ProjectSpec.sequential
              let a = spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "A.fs")

              // `uncalledAdd` leaves `+` and `abs` on unconstrained typars during CheckDeclaration;
              // only CheckImplFileTail's type-defaulting solves them to `int` and records Operator / Function in FinishFile's tail sink!
              let aText =
                  """module Deps.A

let baseValue = 10

let uncalledDefaulted =
    fun x -> abs x + x

let inline callTrait (target: ^T) =
    (^T : (member Compute : int -> string) (target, baseValue))

type Runner() =
    member _.Compute(n: int) = sprintf "run=%d" n

let invoked = callTrait (Runner())
"""

              let texts = sources spec (Map.ofList [ "A.fs", aText ])
              let expected = oracleClassification spec texts a None |> normalize

              let ctx =
                  DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
                  |> Async.RunSynchronously

              let checkedFiles = declByDecl ctx spec texts
              let checkedA = checkedFiles |> List.find (fun f -> f.File = a)
              let actual = checkedA.FileResult.GetSemanticClassification() |> normalize

              Expect.isTrue
                  (expected |> Array.exists (fun (sl, _, _, _, ty) -> sl = 6 && ty = SemanticClassificationType.Operator))
                  "oracle classifies defaulted `+` on line 6 as Operator"

              Expect.equal actual expected "FinishFile includes tail sink classifications matching InteractiveChecker"

          testCase "signature file FileSignature.SemanticClassification matches InteractiveChecker and impl signature stays empty"
          <| fun () ->
              let spec = ProjectSpec.load depsProject |> Async.AwaitTask |> Async.RunSynchronously |> ProjectSpec.sequential
              let eSig = spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "E.fsi")
              let eImpl = spec.SourceFiles |> List.find (fun f -> Path.GetFileName f = "E.fs")

              let eSigText =
                  """module Deps.E

[<System.Obsolete("legacy %d")>]
type Widget =
    | Sprocket of count: int
    member Format: Printf.StringFormat<int -> string> -> string

val user: int
val compute: unit -> int
val unrelated: int
"""

              let eImplText =
                  """module Deps.E

[<System.Obsolete("legacy %d")>]
type Widget =
    | Sprocket of count: int
    member _.Format(fmt: Printf.StringFormat<int -> string>) = sprintf fmt 42

let user = 2
let compute () = 6
let unrelated = 7
"""

              let texts = sources spec (Map.ofList [ "E.fsi", eSigText; "E.fs", eImplText ])
              let expectedSig = oracleClassification spec texts eSig None |> normalize
              Expect.isNonEmpty expectedSig "oracle produces semantic classifications for .fsi signature file"

              let ctx =
                  DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
                  |> Async.RunSynchronously

              let mutable before = []
              let mutable actualSig: FileSignature option = None
              let mutable actualImplSig: FileSignature option = None

              for f in spec.SourceFiles do
                  let p = DeclarationChecker.Parse(ctx, f, texts[f])

                  if f.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                      let s = DeclarationChecker.CheckSignatureFile(ctx, before, p)
                      if f = eSig then actualSig <- Some s
                      before <- before @ [ s ]
                  else
                      let r = DeclarationChecker.CheckFile(ctx, before, p)
                      if f = eImpl then actualImplSig <- Some r.Signature
                      before <- before @ [ r.Signature ]

              Expect.equal
                  (actualSig.Value.SemanticClassification |> normalize)
                  expectedSig
                  "FileSignature.SemanticClassification matches InteractiveChecker on .fsi"

              Expect.isEmpty
                  actualImplSig.Value.SemanticClassification
                  "implementation FileSignature.SemanticClassification is empty" ]
