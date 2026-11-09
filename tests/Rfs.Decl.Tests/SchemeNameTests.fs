module Rfs.Decl.Tests.SchemeNameTests

open Expecto
open FSharp.Compiler.Declarations
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private text =
    "module Tiny.C\n\nlet f (x: #seq<'a>) = x\n\nlet inline add (x: ^T) y = x + y\n\nlet g x y = x, y\n\nlet other = 1\n"

/// What later files see of a finish: its signature's links, and the names of its type parameters as CheckFile's
[<Tests>]
let tests =
    testList
        "SchemeNames"
        [ testAsync "a finish's signature holds its own links and CheckFile's names, through later finishes" {
              let! spec = ProjectSpec.load tinyProject |> Async.AwaitTask
              let! ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
              let texts = sources spec (Map.ofList [ "C.fs", text ])
              let file = spec.SourceFiles |> List.find (fun f -> f.EndsWith "C.fs")
              let checkedFiles = declByDecl ctx spec texts
              let c = checkedFiles |> List.find (fun f -> f.File = file)
              let before =
                  ([], spec.SourceFiles |> List.takeWhile (fun f -> f <> file))
                  ||> List.fold (fun before f ->
                      let earlier = DeclarationChecker.Parse(ctx, f, texts[f])

                      if f.EndsWith ".fsi" then
                          before @ [ DeclarationChecker.CheckSignatureFile(ctx, before, earlier) ]
                      else
                          before @ [ DeclarationChecker.CheckFile(ctx, before, earlier).Signature ])

              let parse = DeclarationChecker.Parse(ctx, file, text)
              let whole = (DeclarationChecker.CheckFile(ctx, before, parse)).Signature
              Expect.isEmpty (whole.OutOfScope()) "CheckFile's signature holds its own links"
              let first = c.FileResult.Signature
              Expect.equal first.Fingerprint whole.Fingerprint "FinishFile's print is CheckFile's"
              Expect.isEmpty (first.OutOfScope()) "FinishFile's signature holds its own links"
              Expect.sequenceEqual (first.TyparNames()) (whole.TyparNames()) "the names are CheckFile's"

              for finish in 2..4 do
                  let next = DeclarationChecker.FinishFile(c.Start, c.Results, parse)
                  Expect.isTrue (obj.ReferenceEquals(next.Signature, first)) "nothing changed: the same signature is kept"
                  Expect.isEmpty (first.OutOfScope()) $"after finish %d{finish}: its links"
                  Expect.sequenceEqual (first.TyparNames()) (whole.TyparNames()) $"after finish %d{finish}: its names"
          } ]
