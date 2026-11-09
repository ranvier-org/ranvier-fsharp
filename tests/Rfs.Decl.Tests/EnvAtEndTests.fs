module Rfs.Decl.Tests.EnvAtEndTests

open System
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.EditorServices
open Rfs.Lsp
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private depsDir = Path.Combine(Path.GetDirectoryName tinyDir, "Deps")
let private depsProject = Path.Combine(depsDir, "Deps.fsproj")
let private a = Path.Combine(depsDir, "A.fs")

let private load () =
    async {
        let! spec = ProjectSpec.load depsProject |> Async.AwaitTask
        let spec = ProjectSpec.sequential spec
        return! DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec)
    }

/// Checks every declaration in order, each in the environment the ones before it leave
let private checkAll (start: FileStart) (env0: DeclEnv) (decls: ParsedDeclaration[]) =
    let mutable env = env0

    [| for d in decls do
           let r = DeclarationChecker.CheckDeclaration(start, env, d, CancellationToken.None)
           env <- DeclarationChecker.ExtendEnv(env, r)
           r |]

let private namesAtColumn (check: FSharpCheckFileResults) parse (text: string) (line: int) (column: int) =
    let lineStr = text.Split('\n')[line - 1]
    let info = check.GetDeclarationListInfo(Some parse, line, lineStr, QuickParse.GetPartialLongNameEx(lineStr, column - 1))
    info.Items |> Array.map _.NameInList |> Set.ofArray

let private namesAt check parse (text: string) (line: int) =
    namesAtColumn check parse text line (text.Split('\n')[line - 1]).Length

let private before = "module Deps.A\n\nlet g = 1\n\nlet h = 2\n\nlet kept =\n    let inner = 3\n    inner\n\n"
let private after = before.Replace("let g =", "let g2 =")
let private endLine = after.Split('\n').Length

[<Tests>]
let tests =
    testSequenced
    <| testList
        "EnvAtEnd"
        [ testAsync "end env holds the current names" {
              let! ctx = load ()
              let parse0 = DeclarationChecker.Parse(ctx, a, before)
              let start, env0 = DeclarationChecker.BeginFile(ctx, [], parse0)
              let results = checkAll start env0 (DeclarationChecker.Split parse0)
              let parse1 = DeclarationChecker.Parse(ctx, a, after)
              let row1 = (DeclarationChecker.Split parse1)[0]
              let renamed = DeclarationChecker.CheckDeclaration(start, env0, row1, CancellationToken.None)
              let current = [| renamed; results[1]; results[2] |]
              let finished = DeclarationChecker.FinishFile(start, current, parse1)
              // Past the last row a full check answers from what that row captured, so the published results do too
              Expect.contains (namesAt finished.CheckResults parse1 after endLine) "g" "the kept last row's captures"

              // A request on the stale file goes through a view with the last row checked again
              let env = DeclarationChecker.ExtendEnvAdHoc(DeclarationChecker.ExtendEnvAdHoc(env0, renamed), results[1])
              let adHoc = DeclarationChecker.CheckDeclarationAdHoc(start, env, (DeclarationChecker.Split parse1)[2], CancellationToken.None)
              let names = namesAt (DeclarationChecker.ViewResults(start, [| renamed; results[1]; adHoc |], parse1)) parse1 after endLine
              Expect.contains names "g2" "the current name"
              Expect.isFalse (names.Contains "g") "not the name it replaced"
          }

          testAsync "where no row captured anything the end environment holds the current names" {
              let top = "// top\n"
              let! ctx = load ()
              let parse0 = DeclarationChecker.Parse(ctx, a, top + before)
              let start, env0 = DeclarationChecker.BeginFile(ctx, [], parse0)
              let results = checkAll start env0 (DeclarationChecker.Split parse0)
              let parse1 = DeclarationChecker.Parse(ctx, a, top + after)
              let row1 = (DeclarationChecker.Split parse1)[0]
              let renamed = DeclarationChecker.CheckDeclaration(start, env0, row1, CancellationToken.None)
              let finished = DeclarationChecker.FinishFile(start, [| renamed; results[1]; results[2] |], parse1)
              let names = namesAtColumn finished.CheckResults parse1 (top + after) 1 0
              Expect.contains names "g2" "the row checked again"
              Expect.contains names "kept" "a kept row"
              Expect.isFalse (names.Contains "g") "the name it replaced"
          }

          testAsync "a view with an ad hoc result in a kept row's place holds the current names there" {
              let! ctx = load ()
              let parse0 = DeclarationChecker.Parse(ctx, a, before)
              let start, env0 = DeclarationChecker.BeginFile(ctx, [], parse0)
              let results = checkAll start env0 (DeclarationChecker.Split parse0)
              let parse1 = DeclarationChecker.Parse(ctx, a, after)
              let decls = DeclarationChecker.Split parse1
              let renamed = DeclarationChecker.CheckDeclaration(start, env0, decls[0], CancellationToken.None)
              let current = [| renamed; results[1]; results[2] |]
              let finished = DeclarationChecker.FinishFile(start, current, parse1)
              let keptLine = decls[2].Range.EndLine
              let published = namesAtColumn finished.CheckResults parse1 after keptLine 4
              Expect.contains published "g" "the kept row's own environment is as it was"

              let env = DeclarationChecker.ExtendEnvAdHoc(DeclarationChecker.ExtendEnvAdHoc(env0, renamed), results[1])
              let adHoc = DeclarationChecker.CheckDeclarationAdHoc(start, env, decls[2], CancellationToken.None)
              let view = DeclarationChecker.ViewResults(start, [| renamed; results[1]; adHoc |], parse1)
              let names = namesAtColumn view parse1 after keptLine 4
              Expect.contains names "g2" "the current name"
              Expect.isFalse (names.Contains "g") "not the name it replaced"
              Expect.equal (namesAtColumn finished.CheckResults parse1 after keptLine 4) published "the published results read as they did"
          }

          testAsync "an ad hoc check leaves what the finish solved solved" {
              let text = "module Deps.A\n\nlet items = ResizeArray()\n\nlet kept = 3\n"
              let! ctx = load ()
              let parse = DeclarationChecker.Parse(ctx, a, text)
              let start, env0 = DeclarationChecker.BeginFile(ctx, [], parse)
              let decls = DeclarationChecker.Split parse
              let results = checkAll start env0 decls
              let finished = DeclarationChecker.FinishFile(start, results, parse)

              // Check results cache tooltips by line, column, line text and width: each read has a width of its own
              let mutable width = 80

              let tip () =
                  width <- width + 1

                  let (ToolTipText elements) =
                      finished.CheckResults.GetToolTip(3, 9, "let items = ResizeArray()", [ "items" ], FSharp.Compiler.Tokenization.FSharpTokenTag.Identifier, width)

                  [ for e in elements do
                        match e with
                        | ToolTipElement.Group items -> yield! items |> List.map _.MainDescription.Text
                        | _ -> () ]

              let solved = tip ()
              Expect.stringContains (String.concat "" solved) "ResizeArray<obj>" "the finish's default"
              let env = DeclarationChecker.ExtendEnvAdHoc(env0, results[0])
              DeclarationChecker.CheckDeclarationAdHoc(start, env, decls[1], CancellationToken.None) |> ignore
              Expect.equal (tip ()) solved "the ad hoc check undid nothing"
              DeclarationChecker.CheckDeclaration(start, env, decls[1], CancellationToken.None) |> ignore
              Expect.notEqual (tip ()) solved "a check undoes the finish: why an ad hoc check is not one"
          }

          testAsync "reading the check results holding the file's check lock raises, and later reads work" {
              let! ctx = load ()
              let parse = DeclarationChecker.Parse(ctx, a, before)
              let start, env0 = DeclarationChecker.BeginFile(ctx, [], parse)
              let results = checkAll start env0 (DeclarationChecker.Split parse)
              let finished = DeclarationChecker.FinishFile(start, results, parse)

              ctx.HoldingCheckLock(
                  a,
                  fun () -> Expect.throwsT<InvalidOperationException> (fun () -> finished.CheckResults |> ignore) "m2 guard"
              )

              Expect.contains (namesAt finished.CheckResults parse before endLine) "h" "the refused read left no exception behind"
              ctx.HoldingCheckLock(a, fun () -> finished.CheckResults |> ignore)
          } ]
