module Rfs.Decl.Tests.FinishBugTests

open System
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

/// C.fs checked one declaration at a time, compared with a full check of the same text.
let private expectOracle (text: string) =
    async {
        let! t = tiny.Value |> Async.AwaitTask
        let texts = sources t.Spec (Map.ofList [ "C.fs", text ])
        let file = t.Spec.SourceFiles |> List.find (fun f -> f.EndsWith "C.fs")
        let! expected = oracle t.Spec texts file
        let c = declByDecl t.Context t.Spec texts |> List.find (fun f -> f.File = file)
        let diff = Diagnostics.compare (toLsp expected) (toLsp c.FileResult.Diagnostics)
        Expect.isNonEmpty expected "the full check reports something"
        Expect.isEmpty diff $"diagnostics equal a full check:\n%s{String.concat Environment.NewLine diff}"
    }

[<Tests>]
let finishTests =
    testList
        "Finish"
        [ testAsync "messages show the types defaults gave" {
              do! expectOracle "module Tiny.C\n\nlet r = ref []\n\nlet fill () = r.Value <- [ r ]\n"
          }

          testAsync "an error applying a default is reported, not raised" {
              do!
                  expectOracle
                      "module Tiny.C\n\nlet scale (x: int) : int = x * 2\n\nlet area (r: int) : int = 3 * scale * r\n"
          } ]
