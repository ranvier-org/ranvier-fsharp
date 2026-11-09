module Rfs.Decl.Tests.SymbolKeyTests

open System.Collections.Generic
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private fixture (name: string) (project: string) =
    let path = Path.Combine(Path.GetDirectoryName tinyDir, name, project)
    Rfs.Harness.Fixtures.build path
    (ProjectSpec.load path).GetAwaiter().GetResult()

let private contextOf (spec: ProjectSpec) =
    DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.RunSynchronously

let private fileNamed (files: CheckedFile list) (name: string) =
    files |> List.find (fun f -> Path.GetFileName f.File = name)

let private usesOf (check: FSharpCheckFileResults) (name: string) =
    check.GetAllUsesOfAllSymbolsInFile() |> Seq.filter (fun u -> u.Symbol.DisplayName = name) |> List.ofSeq

let private keyOf (u: FSharpSymbolUse) =
    match DeclarationChecker.SymbolKey u.Symbol with
    | Some k -> k
    | None -> failtest $"no key for %s{u.Symbol.DisplayName}"

let private keys (file: CheckedFile) (name: string) =
    let uses = usesOf file.FileResult.CheckResults name
    Expect.isNonEmpty uses $"%s{name} is used in %s{Path.GetFileName file.File}"
    uses |> List.map keyOf |> List.distinct

let private definitionKeys (file: CheckedFile) (name: string) =
    usesOf file.FileResult.CheckResults name |> List.filter _.IsFromDefinition |> List.map keyOf

let private deps = lazy fixture "Deps" "Deps.fsproj"
let private sigConform = lazy fixture "SigConform" "SigConform.fsproj"

let private checkAll (spec: ProjectSpec) (overrides: Map<string, string>) =
    declByDecl (contextOf spec) spec (sources spec overrides)

let private withSignature = "module Deps.E\n\nval user: int\nval compute: unit -> int\nval unrelated: int\n"
let private rest = "let user = 0\nlet compute () = 0\nlet unrelated = 0\n"

let private overloads =
    "module Deps.E\n\ntype T() =\n    member _.M(x: int) = x\n    member _.M(x: string) = x\n    member _.M(x: 'a list) = x\n    static member M(x: float) = x\n\nlet a = T().M 1\nlet b = T().M \"s\"\n"
    + rest

let private inE (text: string) =
    fileNamed (checkAll deps.Value (Map [ "E.fs", text; "E.fsi", withSignature ])) "E.fs"

[<Tests>]
let tests =
    testSequenced
    <| testList
        "SymbolKey"
        [ test "key equal across copies" {
              let spec = deps.Value
              let ctx = contextOf spec
              let texts = sources spec Map.empty
              let files = declByDecl ctx spec texts
              let a = fileNamed files "A.fs"
              let d = fileNamed files "D.fs"
              let first = keys a "baseValue"
              Expect.equal first.Length 1 "one key for its definition and its uses in the file"
              Expect.equal (keys d "baseValue") first "the copy in a later file's signature"
              // A body edit: the first file finishes again on a new check, and the later file keeps its objects
              let parse = DeclarationChecker.Parse(ctx, a.File, texts[a.File].Replace("baseValue = 10", "baseValue = 11"))
              let declarations = DeclarationChecker.Split parse
              let row = declarations |> Array.findIndex (fun x -> x.Text.Contains "baseValue = 11")
              let env = if row = 0 then a.Env0 else a.Results[row - 1].EnvAfter
              let results = Array.copy a.Results
              results[row] <- DeclarationChecker.CheckDeclaration(a.Start, env, declarations[row], CancellationToken.None)
              let again = DeclarationChecker.FinishFile(a.Start, results, parse)
              Expect.equal (usesOf again.CheckResults "baseValue" |> List.map keyOf |> List.distinct) first "the new check of it"
              Expect.equal (keys d "baseValue") first "the older objects a kept row holds"
          }

          test "a type, a case, a field and a value have keys of their own kind" {
              let a = fileNamed (checkAll deps.Value Map.empty) "A.fs"
              let found = [ "Shape"; "Circle"; "radius"; "scale" ] |> List.collect (definitionKeys a)
              Expect.equal found.Length 4 $"one definition each: %A{found}"
              Expect.equal (List.distinct found).Length 4 "each its own"
              Expect.stringContains found[0] "|type|" "a type"
              Expect.stringContains found[1] "|case|" "a union case"
              Expect.stringContains found[2] "|field|" "a field of it"
              Expect.stringContains found[3] "|value|" "a value"
              Expect.stringStarts found[3] "Deps|" "it starts with the defining assembly"
          }

          test "a local has no key" {
              let a = fileNamed (checkAll deps.Value Map.empty) "A.fs"
              let keyOfFirst (name: string) = DeclarationChecker.SymbolKey (usesOf a.FileResult.CheckResults name).Head.Symbol
              Expect.isNone (keyOfFirst "r") "a pattern's value"
              Expect.isNone (keyOfFirst "x") "a parameter"
          }

          test "overloads differ, and each keeps its key when one is added before it" {
              let before = definitionKeys (inE overloads) "M"
              Expect.equal (List.distinct before).Length 4 "four overloads, four keys"
              let added = "    member _.M(x: char) = x\n    member _.M(x: int) = x\n"
              let after = definitionKeys (inE (overloads.Replace("    member _.M(x: int) = x\n", added))) "M"
              Expect.equal (List.distinct after).Length 5 "five overloads"

              for key in before do
                  Expect.contains after key "an index would have shifted: the same text names the same overload"
          }

          test "a member and the interface implementations of its name differ" {
              let text =
                  "module Deps.E\n\ntype IOne =\n    abstract Same: int\n\ntype ITwo =\n    abstract Same: int\n\ntype Both() =\n    member _.Same = 0\n\n    interface IOne with\n        member _.Same = 1\n\n    interface ITwo with\n        member _.Same = 2\n\n"
                  + rest

              let found = usesOf (inE text).FileResult.CheckResults "Same" |> List.filter _.IsFromDefinition |> List.map keyOf
              Expect.equal (List.distinct found).Length 5 $"two slots and three implementations: %A{found}"
          }

          test "the use of an overload has the key of that overload's definition" {
              let e = inE overloads
              let check = e.FileResult.CheckResults
              let lines = overloads.Split '\n'

              let at (line: int) =
                  let column = lines[line - 1].IndexOf ".M" + 2
                  (check.GetSymbolUseAtLocation(line, column, lines[line - 1], [ "M" ])).Value

              let definitions = definitionKeys e "M"
              Expect.equal (keyOf (at 9)) definitions[0] "M 1 is M(int)"
              Expect.equal (keyOf (at 10)) definitions[1] "M \"s\" is M(string)"
          }

          test "a signature's copy has the key, whatever order and spelling it uses" {
              let spec = sigConform.Value

              let source =
                  "module SigConform.A\n\ntype A = int\n\ntype T() =\n    member _.M(t: A) = t\n    member _.M(t: string) = t\n    member _.G(x: 'first) = x\n    override _.ToString() = \"\"\n    abstract V: int -> int\n    default _.V x = x\n\nlet f (x: int) y = x + y\n\nlet g (x: 'a) = x\n\nlet useM = T().M(t = 1)\n\nlet useF = f 1 2\n\nlet useG = g 3\n"

              let signature =
                  "module SigConform.A\n\ntype A = int\n\ntype T =\n    new: unit -> T\n    member G: x: 'other -> 'other\n    member M: t: string -> string\n    member M: t: int -> int\n    override ToString: unit -> string\n    abstract V: int -> int\n    default V: x: int -> int\n\nval f: x: int -> (int -> int)\n\nval g: x: 'b -> 'b\n\nval useM: int\n\nval useF: int\n\nval useG: int\n"

              let ctx = contextOf spec
              let texts = sources spec (Map [ "A.fs", source; "A.fsi", signature ])
              let sigFile = spec.SourceFiles |> List.find (fun f -> f.EndsWith "A.fsi")
              let checkedSignature = DeclarationChecker.CheckSignatureFile(ctx, [], DeclarationChecker.Parse(ctx, sigFile, texts[sigFile]))
              let a = fileNamed (declByDecl ctx spec texts) "A.fs"

              let slot, implementation =
                  usesOf a.FileResult.CheckResults "V"
                  |> List.filter _.IsFromDefinition
                  |> List.partition (fun u -> (u.Symbol :?> FSharpMemberOrFunctionOrValue).IsDispatchSlot)

              let lineOf (u: FSharpSymbolUse) =
                  (DeclarationChecker.SignatureLocation(ctx, checkedSignature, keyOf u, false, CancellationToken.None)).Value.StartLine

              Expect.equal (lineOf slot.Head) 11 "abstract V to its own line"
              Expect.equal (lineOf implementation.Head) 12 "default V to its own line"

              for name in [ "M"; "G"; "f"; "g"; "T"; "ToString"; "V" ] do
                  for key in definitionKeys a name do
                      match DeclarationChecker.SignatureLocation(ctx, checkedSignature, key, false, CancellationToken.None) with
                      | Some r -> Expect.equal r.FileName sigFile $"%s{name}: found in the signature by %s{key}"
                      | None -> failtest $"%s{name}: the signature has nothing for %s{key}"

              let declared =
                  [ for u in usesOf a.FileResult.CheckResults "M" |> List.filter _.IsFromDefinition -> u.Symbol.SignatureLocation.Value.StartLine ]

              let found =
                  [ for key in definitionKeys a "M" -> (DeclarationChecker.SignatureLocation(ctx, checkedSignature, key, false, CancellationToken.None)).Value.StartLine ]

              Expect.equal found declared "each overload to its own line, though the signature lists them in another order"
              Expect.isNone (DeclarationChecker.SignatureLocation(ctx, checkedSignature, "SigConform|value|SigConform.A|absent", false, CancellationToken.None)) "nothing by a stale key"
          }

          test "a property's getter, setter and uses have one key" {
              let text =
                  "module Deps.E\n\ntype P() =\n    member _.V with get () = 1 and set (x: int) = ()\n\nlet read = P().V\nlet write = P().V <- 2\n"
                  + rest

              // The token of the setter says so, and looks up the same definitions
              let all = usesOf (inE text).FileResult.CheckResults "V" |> List.map (fun u -> (keyOf u).Replace("|setter", "")) |> List.distinct
              Expect.equal all.Length 1 $"one key: %A{all}"
          }

          test "overloads that differ by class type parameter or anonymous record field differ" {
              let text =
                  "module Deps.E\n\ntype T<'a, 'b>() =\n    member _.M(x: 'a) = 1\n    member _.M(x: 'b) = 2\n    member _.N(x: {| A: int |}) = 1\n    member _.N(x: {| B: int |}) = 2\n\n"
                  + rest

              let e = inE text
              Expect.equal (definitionKeys e "M" |> List.distinct).Length 2 "'a and 'b"
              Expect.equal (definitionKeys e "N" |> List.distinct).Length 2 "field names"
          }

          test "a union's IsCase has its case's key" {
              let text =
                  "module Deps.D\n\nopen Deps.A\n\nlet start = 1\nlet isCircle = (Circle 1).IsCircle\nlet r = 0\nlet fill () = ()\nlet size () = 0\nlet after = 42\n"

              let files = checkAll deps.Value (Map [ "D.fs", text ])
              Expect.equal (keys (fileNamed files "D.fs") "IsCircle") (definitionKeys (fileNamed files "A.fs") "Circle") "IsCircle goes to the case"
          }

          test "an active pattern's case has its value's key" {
              let text =
                  "module Deps.E\n\nlet (|Even|Odd|) n = if n % 2 = 0 then Even else Odd\n\nlet f x = match x with Even -> 1 | Odd -> 2\n"
                  + rest

              let e = inE text
              let value = definitionKeys e "(|Even|Odd|)"
              Expect.equal value.Length 1 "the value is defined once"
              let inLaterRow = usesOf e.FileResult.CheckResults "Even" |> List.filter (fun u -> u.Range.StartLine = 5)
              Expect.equal (inLaterRow |> List.map keyOf) value "its case in a later row"
          }

          test "a project's in-memory copy has the key of the definition" {
              let multi = Path.Combine(Path.GetDirectoryName tinyDir, "Multi")
              let appProject = Path.Combine(multi, "App", "App.fsproj")
              Rfs.Harness.Fixtures.build appProject
              let load (p: string) = (ProjectSpec.load p).GetAwaiter().GetResult()
              let lib = load (Path.Combine(multi, "Lib", "Lib.fsproj"))
              let app = load appProject
              let libCtx = contextOf lib
              let libFiles = declByDecl libCtx lib (sources lib Map.empty)
              let texts = sources lib Map.empty

              // Every file's signature in file order, as the assembly wants it
              let signatures =
                  (([], []), lib.SourceFiles)
                  ||> List.fold (fun (before: FileSignature list, results: FileResult list) file ->
                      let parse = DeclarationChecker.Parse(libCtx, file, texts[file])

                      if file.EndsWith ".fsi" then
                          before @ [ DeclarationChecker.CheckSignatureFile(libCtx, before, parse) ], results
                      else
                          let result = DeclarationChecker.CheckFile(libCtx, before, parse)
                          before @ [ result.Signature ], results @ [ result ])

              let assembly = DeclarationChecker.ProjectAssembly(libCtx, snd signatures, fst signatures)
              let references = Dictionary<string, ProjectAssembly>()
              references[(ProjectSpec.referencePath app lib).Value] <- assembly
              let appCtx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions app, references) |> Async.RunSynchronously
              let inApp = fileNamed (declByDecl appCtx app (sources app Map.empty)) "Use.fs"
              let api = fileNamed libFiles "Lib.fs"
              Expect.equal (keys inApp "f") (definitionKeys api "f") "a value"
              Expect.equal (keys inApp "shapeOf") (definitionKeys api "shapeOf") "another"
              Expect.stringStarts (keys inApp "f").Head "Lib|" "Lib's, from the file that uses it in App"
          } ]
