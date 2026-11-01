module Rfs.Decl.Tests.ContentTests

open System
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

type private Tiny =
    { Spec: ProjectSpec
      Context: ProjectContext
      CFile: string }

let private tiny =
    lazy
        (async {
            let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
            let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)

            return
                { Spec = spec
                  Context = ctx
                  CFile = spec.SourceFiles |> List.find (fun f -> f.EndsWith "C.fs") }
         }
         |> Async.StartAsTask)

/// A context of its own: the #nowarn scopes a parse records are the project's, by file, so a test that parses C.fs
/// with a #nowarn must not share a context with tests that parse other versions of C.fs at the same time.
let private ownTiny () =
    async {
        let! t = tiny.Value |> Async.AwaitTask
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions t.Spec)
        return { t with Context = ctx }
    }

let private split (source: string) =
    async {
        let! t = tiny.Value |> Async.AwaitTask
        return DeclarationChecker.Split(DeclarationChecker.Parse(t.Context, t.CFile, source))
    }

let private keys (decls: ParsedDeclaration[]) =
    [ for d in decls -> d.Key.ModulePath, d.Key.Kind, d.Key.Name, d.Key.Ordinal ]

let private fiveDecls =
    """module Tiny.C

let a = 1

type T = { F: int }

module M =
    let b = a

open System

let c = M.b
"""

let private nested =
    """module Tiny.C

module M =
    let a = Tiny.B.two + 1

    module Inner =
        let e = a + 1

    let b = a + Inner.e

let c: string = M.b

module N =
    let d = M.a + M.Inner.e
"""

// b's body edited on its own line: same type, and an incomplete match.
let private nestedEdited = nested.Replace("let b = a + Inner.e", "let b = match a with 1 -> Inner.e")

let private codePrinter =
    @"C:\Users\shaya\RiderProjects\ranvier-fsharp-corpus\large\src\Fantomas.Core\CodePrinter.fs"

let private diagnosticsDiff spec (texts: Map<string, string>) file (actual: FSharp.Compiler.Diagnostics.FSharpDiagnostic[]) =
    async {
        let! expected = oracle spec texts file
        return expected, Diagnostics.compare (toLsp expected) (toLsp actual)
    }

[<Tests>]
let splitTests =
    testList
        "Content.Split"
        [ testAsync "let rec ... and is one RecGroup" {
              let! decls = split "module Tiny.C\n\nlet rec a = b\nand b = 1\n"
              Expect.equal (keys decls) [ [ "Tiny"; "C" ], DeclarationKind.RecGroup, "a,b", 0 ] "one row"
          }

          testAsync "type ... and is one RecGroup" {
              let! decls = split "module Tiny.C\n\ntype A = B of B option\nand B = A of A\n"
              Expect.equal (keys decls) [ [ "Tiny"; "C" ], DeclarationKind.RecGroup, "A", 0 ] "one row"
          }

          testAsync "a nested module rec is one row" {
              let! decls = split "module Tiny.C\n\nmodule rec M =\n    let a () = b ()\n    let b () = a ()\n\nlet c = 1\n"

              Expect.equal
                  (keys decls)
                  [ [ "Tiny"; "C" ], DeclarationKind.RecGroup, "M", 0; [ "Tiny"; "C" ], DeclarationKind.Value, "c", 0 ]
                  "the module and the value after it"
          }

          testAsync "a module rec file is one row" {
              let! decls = split "module rec Tiny.C\n\nlet a () = b ()\n\nlet b () = a ()\n"
              Expect.equal (decls |> Array.map (fun d -> d.Key.Kind)) [| DeclarationKind.RecGroup |] "one row"
          }

          testAsync "a namespace rec file is one row" {
              let! decls = split "namespace rec Tiny\n\ntype A() = member _.B = B()\n\ntype B() = member _.A = A()\n"
              Expect.equal (decls |> Array.map (fun d -> d.Key.Kind)) [| DeclarationKind.RecGroup |] "one row"
          }

          testAsync "a nested module is split into its declarations" {
              let! decls = split nested
              let c = [ "Tiny"; "C" ]
              let m = c @ [ "M" ]

              Expect.equal
                  (keys decls)
                  [ c, DeclarationKind.Module, "M", 0
                    m, DeclarationKind.Value, "a", 0
                    m, DeclarationKind.Module, "Inner", 0
                    m @ [ "Inner" ], DeclarationKind.Value, "e", 0
                    m, DeclarationKind.Value, "b", 0
                    c, DeclarationKind.Value, "c", 0
                    c, DeclarationKind.Module, "N", 0
                    c @ [ "N" ], DeclarationKind.Value, "d", 0 ]
                  "the module, then its declarations under its path"

              Expect.isTrue (decls[1].Text.StartsWith "let a") "an inner declaration's text is its own"
          }

          testAsync "inserting a declaration at the top keeps the other keys" {
              let! before = split fiveDecls
              let! after = split (fiveDecls.Replace("module Tiny.C\n\n", "module Tiny.C\n\nlet z = 0\n\n"))
              Expect.equal (keys after |> List.tail) (keys before) "the same keys after the new first one"
              Expect.equal after[0].Key.Name "z" "the new declaration"
          }

          testAsync "two lets of one name in a row get ordinals 0 and 1" {
              let! decls = split "module Tiny.C\n\nlet x = 1\n\nlet x = 2\n"
              Expect.equal [ for d in decls -> d.Key.Name, d.Key.Ordinal ] [ "x", 0; "x", 1 ] "ordinals"
          }

          testAsync "an augmentation has its own kind" {
              let! decls = split "module Tiny.C\n\ntype T() =\n    member _.A = 1\n\ntype T with\n    member _.B = 2\n"
              Expect.equal [ for d in decls -> d.Key.Kind, d.Key.Name ] [ DeclarationKind.Type, "T"; DeclarationKind.Augmentation, "T" ] "kinds"
          }

          testAsync "the big corpus file is one RecGroup row" {
              if not (File.Exists codePrinter) then
                  skiptest $"corpus file absent: %s{codePrinter} (run build/fetch-corpus.ps1)"

              let! decls = split (File.ReadAllText codePrinter)
              Expect.equal (decls |> Array.map (fun d -> d.Key.Kind)) [| DeclarationKind.RecGroup |] "the whole module rec file"
          }

          testAsync "a file with nested modules split into declarations checks and finishes as a full check" {
              let! t = tiny.Value |> Async.AwaitTask
              let texts = sources t.Spec (Map.ofList [ "C.fs", nested ])
              let c = declByDecl t.Context t.Spec texts |> List.find (fun f -> f.File.EndsWith "C.fs")
              Expect.equal c.Results.Length 8 "one result per row"
              let! expected, diff = diagnosticsDiff t.Spec texts c.File c.FileResult.Diagnostics
              Expect.isEmpty diff $"decl-by-decl diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"
              Expect.isTrue (expected |> Array.exists (fun d -> d.ErrorNumber = 1)) "the file check reports c's type error"
              let c' = c.Results |> Array.find (fun r -> r.Key.Name = "c")
              Expect.isTrue (c'.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "the type error belongs to c"
          }

          testAsync "re-checking one nested declaration alone finishes as a full check" {
              let! t = tiny.Value |> Async.AwaitTask
              let c = declByDecl t.Context t.Spec (sources t.Spec (Map.ofList [ "C.fs", nested ])) |> List.find (fun f -> f.File.EndsWith "C.fs")
              let editedDecls = DeclarationChecker.Split(DeclarationChecker.Parse(t.Context, c.File, nestedEdited))
              Expect.equal (keys editedDecls) (keys c.Declarations) "the edit keeps the keys"
              let index = editedDecls |> Array.findIndex (fun d -> d.Key.Name = "b")
              let rechecked = DeclarationChecker.CheckDeclaration(c.Start, c.Results[index - 1].EnvAfter, editedDecls[index], CancellationToken.None)
              Expect.isTrue (rechecked.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 25)) "the edit's warning belongs to b"

              let results = c.Results |> Array.mapi (fun i r -> if i = index then rechecked else r)
              let fileResult = DeclarationChecker.FinishFile(c.Start, results)
              let texts = sources t.Spec (Map.ofList [ "C.fs", nestedEdited ])
              let! _, diff = diagnosticsDiff t.Spec texts c.File fileResult.Diagnostics
              Expect.isEmpty diff $"re-checked file diagnostics equal a fresh full check:\n%s{String.concat Environment.NewLine diff}"

              let names =
                  fileResult.CheckResults.PartialAssemblySignature.Entities
                  |> Seq.collect (fun e -> e.NestedEntities)
                  |> Seq.filter (fun e -> e.DisplayName = "M")
                  |> Seq.collect (fun e -> e.MembersFunctionsAndValues)
                  |> Seq.map (fun v -> v.DisplayName)
                  |> List.ofSeq

              Expect.equal names [ "a"; "b" ] "M is built from its declarations' deltas"
          } ]

let private content (body: string) (pick: ParsedDeclaration -> bool) =
    async {
        let! decls = split ("module Tiny.C\n\n" + body)

        match decls |> Array.tryFind pick with
        | Some decl -> return DeclarationChecker.Content decl
        | None -> return failwithf "no declaration picked from %A" (keys decls)
    }

let private named name (d: ParsedDeclaration) = d.Key.Name = name
let private ofKind kind (d: ParsedDeclaration) = d.Key.Kind = kind

let private none =
    { Defines = [||]
      Uses = [||]
      MemberUses = [||]
      FieldOrCaseUses = [||]
      HasUnresolvable = false
      AffectsAllLater = false }

/// (test name, declarations after `module Tiny.C`, which row, expected content)
let private exact =
    [ "x.Foo is a member use",
      "let f (x: string) = x.Length",
      named "f",
      { none with
          Defines = [| "f" |]
          Uses = [| "Length"; "string"; "x" |]
          MemberUses = [| "Length" |]
          FieldOrCaseUses = [| "Length" |] }

      "a member of an expression is a member use",
      "let n = (string 1).Length",
      named "n",
      { none with
          Defines = [| "n" |]
          Uses = [| "string" |]
          MemberUses = [| "Length" |]
          FieldOrCaseUses = [| "Length" |] }

      "a record label is a field use",
      "let r = { Name = \"a\" }",
      named "r",
      { none with
          Defines = [| "r" |]
          Uses = [| "Name" |]
          FieldOrCaseUses = [| "Name" |] }

      "union cases in patterns are case uses",
      "let g x = match x with Some y -> y | None -> 0",
      named "g",
      { none with
          Defines = [| "g" |]
          Uses = [| "None"; "Some"; "x"; "y" |]
          FieldOrCaseUses = [| "None"; "Some" |] }

      "open affects all later",
      "open System.IO",
      ofKind DeclarationKind.Open,
      { none with
          Uses = [| "IO"; "System" |]
          AffectsAllLater = true }

      "an AutoOpen module affects all later",
      "[<AutoOpen>]\nmodule Helpers =\n    let h = 1",
      named "Helpers",
      { none with
          Defines = [| "Helpers" |]
          Uses = [| "AutoOpen"; "AutoOpenAttribute" |]
          AffectsAllLater = true }

      "a plain module defines its name only",
      "module Plain =\n    let p = List.length [ 1 ]",
      named "Plain",
      { none with Defines = [| "Plain" |] }

      "a type abbreviation affects all later",
      "type Id = int",
      named "Id",
      { none with
          Defines = [| "Id" |]
          Uses = [| "int" |]
          AffectsAllLater = true }

      "a module abbreviation affects all later",
      "module L = List",
      named "L",
      { none with
          Defines = [| "L" |]
          Uses = [| "List" |]
          AffectsAllLater = true }

      "a module-level do affects all later",
      "do printfn \"hi\"",
      ofKind DeclarationKind.Do,
      { none with
          Uses = [| "printfn" |]
          AffectsAllLater = true }

      "an attribute declaration affects all later",
      "[<assembly: System.Reflection.AssemblyTitle(\"t\")>]\ndo ()",
      ofKind DeclarationKind.Attribute,
      { none with
          Uses = [| "AssemblyTitle"; "AssemblyTitleAttribute"; "Reflection"; "System" |]
          AffectsAllLater = true }

      "the ? operator is unresolvable",
      "let d (x: obj) = x?Name",
      named "d",
      { none with
          Defines = [| "d" |]
          Uses = [| "Name"; "obj"; "x" |]
          HasUnresolvable = true }

      "a union defines its cases",
      "type U = A | B of int",
      named "U",
      { none with
          Defines = [| "A"; "B"; "U" |]
          Uses = [| "int" |] }

      "a record defines its fields",
      "type R = { F: int; G: string }",
      named "R",
      { none with
          Defines = [| "F"; "G"; "R" |]
          Uses = [| "int"; "string" |] }

      "an exception defines its name",
      "exception Oops of string",
      named "Oops",
      { none with
          Defines = [| "Oops" |]
          Uses = [| "string" |] } ]

[<Tests>]
let contentTests =
    testList
        "Content"
        [ yield!
              [ for name, body, pick, expected in exact ->
                    testAsync name {
                        let! actual = content body pick
                        Expect.equal actual expected "content"
                    } ]

          testAsync "an augmentation uses its type" {
              let! decls = split "module Tiny.C\n\ntype T() =\n    member _.A = 1\n\ntype T with\n    member _.B = 2\n"
              let augmentation = decls[1]
              Expect.equal augmentation.Key.Kind DeclarationKind.Augmentation "kind"
              let c = DeclarationChecker.Content augmentation
              Expect.contains c.Uses "T" "uses the augmented type"
              Expect.equal c.Defines [| "B" |] "defines its members, not the type"
          }

          testAsync "a class defines its members" {
              let! c =
                  content
                      "type K() =\n    member _.M = 1\n    static member S() = 2\n    abstract X: int\n    default _.X = 3\n    member val P = 0 with get, set"
                      (named "K")

              Expect.equal c.Defines [| "K"; "M"; "P"; "S"; "X" |] "the type and its members"
          }

          testAsync "an active pattern defines its cases" {
              let! c = content "let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd" (ofKind DeclarationKind.Value)
              Expect.containsAll c.Defines [ "Even"; "Odd" ] "the cases"
          }

          testAsync "a custom operation defines its operation name" {
              let! c = content "type QB() =\n    [<CustomOperation(\"pick\")>]\n    member _.Pick(xs: int list) = xs" (named "QB")
              Expect.containsAll c.Defines [ "Pick"; "pick" ] "the member and its operation"
              Expect.isFalse c.AffectsAllLater "the operation name is known"
          }

          testAsync "a custom operation named by a non-literal affects all later" {
              let! c = content "type QB() =\n    [<CustomOperation(Names.pick)>]\n    member _.Pick(xs: int list) = xs" (named "QB")
              Expect.isTrue c.AffectsAllLater "its operation name is unknown"
          }

          testAsync "a computation expression uses its builder's methods" {
              let! c = content "let w = async { let! x = async { return 1 } in return x }" (named "w")
              Expect.contains c.Uses "async" "the builder"
              Expect.containsAll c.MemberUses [ "Bind"; "Return"; "Delay" ] "the builder methods"
              Expect.isFalse c.HasUnresolvable "attributable"
          }

          testAsync "an attribute uses its name with and without the suffix" {
              let! c = content "[<Obsolete>]\nlet o = 1" (named "o")
              Expect.containsAll c.Uses [ "Obsolete"; "ObsoleteAttribute" ] "both spellings"
          }

          testAsync "a qualified use names every segment" {
              let! c = content "let q = Tiny.B.two" (named "q")
              Expect.containsAll c.Uses [ "Tiny"; "B"; "two" ] "every segment"
              Expect.containsAll c.MemberUses [ "B"; "two" ] "every segment after the first"
          }

          testAsync "a nested declaration's content is its own" {
              let! decls = split nested
              let b = decls |> Array.find (named "b")
              let c = DeclarationChecker.Content b
              Expect.equal c.Defines [| "b" |] "defines b"
              Expect.containsAll c.Uses [ "a"; "Inner"; "e" ] "uses a and Inner.e"

              let m = DeclarationChecker.Content(decls |> Array.find (named "M"))
              Expect.equal m.Uses [||] "the module row has none of its declarations' uses"
          }

          testAsync "indexing lists the indexer members" {
              let! c = content "let i (t: int[]) = t[0]" (named "i")
              Expect.containsAll c.MemberUses [ "Item"; "GetReverseIndex" ] "t[0]"
          }

          testAsync "assigning through an index lists the indexer members" {
              let! c = content "let s (t: int[]) = t[0] <- 1" (named "s")
              Expect.containsAll c.MemberUses [ "Item"; "SetSlice" ] "t[0] <- v"
          }

          testAsync "slicing lists the slice members" {
              let! c = content "let sl (t: int[]) = t[1..2]" (named "sl")
              Expect.containsAll c.MemberUses [ "GetSlice"; "SetSlice" ] "t[1..2]"
          }

          testAsync "return! and yield! list the Final builder methods" {
              let! r = content "let r = b { return! 1 }" (named "r")
              Expect.containsAll r.MemberUses [ "ReturnFrom"; "ReturnFromFinal" ] "return!"
              let! y = content "let y = b { yield! xs }" (named "y")
              Expect.containsAll y.MemberUses [ "YieldFrom"; "YieldFromFinal" ] "yield!"
          } ]

/// The rows of a nested-module file checked, then some rows re-checked in order (each from the EnvAfter of the row
/// before it, re-checked or not), and the file finished with the old results of the others.
let private recheckRows (original: string) (edited: string) (rechecked: int list) =
    async {
        let! t = tiny.Value |> Async.AwaitTask
        let c = declByDecl t.Context t.Spec (sources t.Spec (Map.ofList [ "C.fs", original ])) |> List.find (fun f -> f.File.EndsWith "C.fs")
        let editedDecls = DeclarationChecker.Split(DeclarationChecker.Parse(t.Context, c.File, edited))
        Expect.equal (keys editedDecls) (keys c.Declarations) "the edit keeps the keys"
        let results = Array.copy c.Results

        for i in rechecked do
            let envBefore = if i = 0 then c.Env0 else results[i - 1].EnvAfter
            results[i] <- DeclarationChecker.CheckDeclaration(c.Start, envBefore, editedDecls[i], CancellationToken.None)

        let fileResult = DeclarationChecker.FinishFile(c.Start, results)
        let texts = sources t.Spec (Map.ofList [ "C.fs", edited ])
        let! expected, diff = diagnosticsDiff t.Spec texts c.File fileResult.Diagnostics
        Expect.isEmpty diff $"re-checked file diagnostics equal a fresh full check:\n%s{String.concat Environment.NewLine diff}"
        return expected, c, editedDecls, fileResult
    }

let private suffixed =
    """module Tiny.C

module X =
    let a = 1

let c: string = X.a
"""

let private signatureAfterModule =
    """module Tiny.C

module M =
    let a = Tiny.B.two + 1

let c = M.a

module N =
    let d: string = c
"""

let private nowarnInTheMiddle =
    """module Tiny.C

let f x = match x with 1 -> 2

#nowarn "25"

let g x = match x with 1 -> 2
"""

[<Tests>]
let headerTests =
    testList
        "Content.Headers"
        [ testAsync "each declaration names the header it is directly inside" {
              let! decls = split nested
              let within name = (decls |> Array.find (named name)).Within |> Option.map _.Name
              Expect.equal (within "M") None "M is in no module"
              Expect.equal (within "a") (Some "M") "a is in M"
              Expect.equal (within "Inner") (Some "M") "Inner is in M"
              Expect.equal (within "e") (Some "Inner") "e is in Inner"
              Expect.equal (within "c") None "c is after M"
          }

          testAsync "re-checking a header and every declaration under it matches the file check" {
              let edited = nested.Replace("module M =", "module internal M =")
              let! expected, c, editedDecls, _ = recheckRows nested edited [ 0; 1; 2; 3; 4 ]
              Expect.isFalse (editedDecls[0].IsUnchangedFrom c.Declarations[0]) "the header changed"
              Expect.isTrue (editedDecls[1].IsUnchangedFrom c.Declarations[1]) "a did not"
              Expect.isNonEmpty expected "the file check reports c's type error"
          }

          testAsync "a sibling type after a module changes the module's header inputs" {
              let edited = suffixed + "\ntype X = { F: int }\n"
              let! before = split suffixed
              let! after = split edited
              Expect.equal after[0].Text before[0].Text "the header's text is the same"
              Expect.notEqual after[0].HeaderInputs before[0].HeaderInputs "its inputs are not"
              Expect.isFalse (after[0].IsUnchangedFrom before[0]) "so it counts as changed"
              Expect.isTrue (after[1].IsUnchangedFrom before[1]) "a is unchanged"
          }

          testAsync "re-checking a header and its declarations after a sibling type is added matches the file check" {
              let! t = tiny.Value |> Async.AwaitTask
              let c = declByDecl t.Context t.Spec (sources t.Spec (Map.ofList [ "C.fs", suffixed ])) |> List.find (fun f -> f.File.EndsWith "C.fs")
              let edited = suffixed + "\ntype X = { F: int }\n"
              let editedDecls = DeclarationChecker.Split(DeclarationChecker.Parse(t.Context, c.File, edited))
              Expect.equal (keys editedDecls |> List.take 3) (keys c.Declarations) "the same rows, then the type"

              // The header and its declaration again, then c, which uses X by name and whose environment holds the
              // module, then the new type
              let check envBefore i = DeclarationChecker.CheckDeclaration(c.Start, envBefore, editedDecls[i], CancellationToken.None)
              let header = check c.Env0 0
              let a = check header.EnvAfter 1
              let c' = check a.EnvAfter 2
              let typeX = check c'.EnvAfter 3
              let fileResult = DeclarationChecker.FinishFile(c.Start, [| header; a; c'; typeX |])

              let texts = sources t.Spec (Map.ofList [ "C.fs", edited ])
              let! expected, diff = diagnosticsDiff t.Spec texts c.File fileResult.Diagnostics
              Expect.isEmpty diff $"diagnostics equal a fresh full check:\n%s{String.concat Environment.NewLine diff}"
              Expect.isNonEmpty expected "the file check reports c's type error"

              let x =
                  fileResult.CheckResults.PartialAssemblySignature.Entities
                  |> Seq.collect (fun e -> e.NestedEntities)
                  |> Seq.find (fun e -> e.IsFSharpModule && e.DisplayName = "X")

              Expect.equal x.CompiledName "XModule" "the module now has the Module suffix"
          }

          testAsync "re-checking only the first declaration after a nested module matches the file check" {
              let edited = nested.Replace("let c: string = M.b", "let c: string = string (match M.b with 1 -> 2)")
              let! _, _, _, fileResult = recheckRows nested edited [ 5 ]
              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 25)) "the edit's warning"
          }

          testAsync "re-checking a declaration whose signature changed after a nested module, and its dependents, matches the file check" {
              let edited = signatureAfterModule.Replace("let c = M.a", "let c = string M.a")
              // c's dependent d is in N: N's header is checked again too, for the environment d is checked in
              let! expected, c, _, fileResult = recheckRows signatureAfterModule edited [ 2; 3; 4 ]
              Expect.isTrue (c.FileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "before, d had a type error"
              Expect.isEmpty expected "after, the file check reports nothing"
              Expect.notEqual fileResult.Fingerprints[c.Declarations[2].Key] c.FileResult.Fingerprints[c.Declarations[2].Key] "c's signature changed"
          }

          testAsync "a #nowarn applies from its line on, as in the file check" {
              let! t = ownTiny ()
              let texts = sources t.Spec (Map.ofList [ "C.fs", nowarnInTheMiddle ])
              let c = declByDecl t.Context t.Spec texts |> List.find (fun f -> f.File.EndsWith "C.fs")
              let! expected, diff = diagnosticsDiff t.Spec texts c.File c.FileResult.Diagnostics
              Expect.isEmpty diff $"diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"
              Expect.equal [ for d in expected -> d.ErrorNumber, d.Range.StartLine ] [ 25, 3 ] "only the warning before it"
              Expect.equal [ for d in c.Declarations -> d.Key.Name ] [ "f"; "g" ] "a #nowarn is not a declaration"
          }

          testAsync "adding a #nowarn applies to old results when finishing again" {
              let! t = ownTiny ()
              // The #nowarn replaces the blank line between f and g, so no declaration moves or changes
              let original = nowarnInTheMiddle.Replace("#nowarn \"25\"\n\n", "")
              let edited = nowarnInTheMiddle.Replace("\n\n#nowarn \"25\"\n\n", "\n#nowarn \"25\"\n")
              let c = declByDecl t.Context t.Spec (sources t.Spec (Map.ofList [ "C.fs", original ])) |> List.find (fun f -> f.File.EndsWith "C.fs")
              Expect.equal (c.FileResult.Diagnostics |> Array.filter (fun d -> d.ErrorNumber = 25)).Length 2 "both warn before"

              let editedDecls = DeclarationChecker.Split(DeclarationChecker.Parse(t.Context, c.File, edited))
              Expect.isTrue (Array.forall2 (fun (a: ParsedDeclaration) b -> a.IsUnchangedFrom b) editedDecls c.Declarations) "no declaration changed"
              let fileResult = DeclarationChecker.FinishFile(c.Start, c.Results)
              let texts = sources t.Spec (Map.ofList [ "C.fs", edited ])
              let! expected, diff = diagnosticsDiff t.Spec texts c.File fileResult.Diagnostics
              Expect.isEmpty diff $"diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"
              Expect.equal expected.Length 1 "only f warns"
          } ]
