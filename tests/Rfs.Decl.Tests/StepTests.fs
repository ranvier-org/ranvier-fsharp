module Rfs.Decl.Tests.StepTests

open System
open System.IO
open Expecto
open Rfs.Decl.Tests.Common
open FSharp.Compiler.Declarations
open FSharp.Compiler.Diagnostics
open Rfs.Lsp
open Rfs.Projects

let private equalsFileCheck (overrides: Map<string, string>) =
    async {
        let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
        let texts = sources spec overrides
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)

        let results =
            [ for checkedFile in declByDecl ctx spec texts -> checkedFile.File, (checkedFile.Results, checkedFile.FileResult) ]

        // B.fs has a signature file: the finish pass still runs the file's conformance check, so it is compared too.
        for file, (declResults, fileResult) in results do
            let name = Path.GetFileName file
            Expect.isNonEmpty declResults $"%s{name} has declarations"
            let! expected = oracle spec texts file
            let diff = Diagnostics.compare (toLsp expected) (toLsp fileResult.Diagnostics)
            Expect.isEmpty diff $"%s{name}: decl-by-decl diagnostics equal the file check:\n%s{String.concat Environment.NewLine diff}"

        return results
    }

let private manyDecls =
    """module Tiny.C

open System

let three = Tiny.B.two + 1

type Point = { X: int; Y: int }

let p = { X = 1; Y = "two" }

let describe (x: int) =
    match x with
    | 1 -> "one"

module Inner =
    let four = three + 1

exception Oops of string

let five = Inner.four + String.length "five"
"""

let private inNamespace =
    """namespace Tiny

open System

module C =
    let three = B.two + 1

type T() =
    member _.Three: string = C.three
"""

let private twoFragments =
    """namespace Tiny.X

module C =
    let three = Tiny.B.two + 1

namespace Tiny.Y

module D =
    let four: string = Tiny.X.C.three + 1
"""

let private valueRestriction =
    """module Tiny.C

let three = Tiny.B.two + 1

let r = ref []
"""

let private split (source: string) =
    async {
        let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
        let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
        let file = spec.SourceFiles |> List.find (fun f -> f.EndsWith "C.fs")
        return DeclarationChecker.Split(DeclarationChecker.Parse(ctx, file, source))
    }

[<Tests>]
let splitTests =
    testList
        "Split"
        [ testAsync "a same-length edit inside a declaration changes its syntax hash only" {
              let! before = split "module Tiny.C\n\nlet a = 1\n\nlet b x = x + 1\n"
              let! after = split "module Tiny.C\n\nlet a = 1\n\nlet b y = y + 2\n"
              Expect.equal before[0].Range after[0].Range "same range for a"
              Expect.equal before[1].Range after[1].Range "same range for b"
              Expect.equal before[0].SyntaxHash after[0].SyntaxHash "a is unchanged"
              Expect.notEqual before[1].SyntaxHash after[1].SyntaxHash "b was edited"
          }

          testAsync "ordinals count only earlier declarations with the same key" {
              let! decls = split "module Tiny.C\n\nopen System\n\nlet x = 1\n\nopen System\n\ndo ()\n\nlet y = 2\n\ndo ()\n"

              Expect.equal
                  [ for d in decls -> d.Key.Kind, d.Key.Name, d.Key.Ordinal ]
                  [ DeclarationKind.Open, "System", 0
                    DeclarationKind.Value, "x", 0
                    DeclarationKind.Open, "System", 1
                    DeclarationKind.Do, "", 0
                    DeclarationKind.Value, "y", 0
                    DeclarationKind.Do, "", 1 ]
                  "ordinals"
          } ]

[<Tests>]
let tests =
    testList
        "Step"
        [ testAsync "decl-by-decl equals file check" {
              let! results = equalsFileCheck Map.empty
              Expect.equal (results |> List.map (fst >> Path.GetFileName)) [ "A.fs"; "B.fs"; "C.fs" ] "implementation files"

              for file, (_, fileResult) in results do
                  Expect.isEmpty fileResult.Diagnostics $"{Path.GetFileName file} has no diagnostics"
          }

          testAsync "decl-by-decl equals file check with errors in several declarations" {
              let! results = equalsFileCheck (Map.ofList [ "C.fs", manyDecls ])
              let declResults, fileResult = results |> List.find (fst >> fun f -> f.EndsWith "C.fs") |> snd

              Expect.equal
                  [ for r in declResults -> r.Key.Kind, r.Key.Name ]
                  [ DeclarationKind.Open, "System"
                    DeclarationKind.Value, "three"
                    DeclarationKind.Type, "Point"
                    DeclarationKind.Value, "p"
                    DeclarationKind.Value, "describe"
                    DeclarationKind.Module, "Inner"
                    DeclarationKind.Exception, "Oops"
                    DeclarationKind.Value, "five" ]
                  "top-level declarations"

              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "type error"
              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 25)) "incomplete match"

              let p = declResults |> Array.find (fun r -> r.Key.Name = "p")
              Expect.isTrue (p.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "the type error belongs to p"
          }

          testAsync "decl-by-decl equals file check in a namespace" {
              let! results = equalsFileCheck (Map.ofList [ "C.fs", inNamespace ])
              let declResults, fileResult = results |> List.find (fst >> fun f -> f.EndsWith "C.fs") |> snd

              Expect.equal
                  [ for r in declResults -> r.Key.ModulePath, r.Key.Kind, r.Key.Name ]
                  [ [ "Tiny" ], DeclarationKind.Open, "System"
                    [ "Tiny" ], DeclarationKind.Module, "C"
                    [ "Tiny" ], DeclarationKind.Type, "T" ]
                  "namespace declarations"

              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "type error in T"
          }

          testAsync "decl-by-decl equals file check with two namespace fragments" {
              let! results = equalsFileCheck (Map.ofList [ "C.fs", twoFragments ])
              let declResults, fileResult = results |> List.find (fst >> fun f -> f.EndsWith "C.fs") |> snd

              Expect.equal
                  [ for r in declResults -> r.Key.Kind, r.Key.Name ]
                  [ DeclarationKind.Module, "Tiny.X"; DeclarationKind.Module, "Tiny.Y" ]
                  "each fragment is one declaration"

              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 1)) "type error in D"
          }

          testAsync "decl-by-decl equals file check when the finish pass reports" {
              let! results = equalsFileCheck (Map.ofList [ "C.fs", valueRestriction ])
              let _, fileResult = results |> List.find (fst >> fun f -> f.EndsWith "C.fs") |> snd
              Expect.isTrue (fileResult.Diagnostics |> Array.exists (fun d -> d.ErrorNumber = 30)) "value restriction"
          } ]
