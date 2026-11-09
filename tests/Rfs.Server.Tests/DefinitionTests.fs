module Rfs.Server.Tests.DefinitionTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open Rfs.Harness
open Rfs.Lsp
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests.PublishTests

let private fixtures = Path.GetDirectoryName depsDir
let private d = file "D.fs"
let private sigProject = Path.Combine(fixtures, "SigConform", "SigConform.fsproj")
let private sigA = Path.Combine(fixtures, "SigConform", "A.fs")
let private sigAi = Path.Combine(fixtures, "SigConform", "A.fsi")
let private sigB = Path.Combine(fixtures, "SigConform", "B.fs")
let private multiDir = Path.Combine(fixtures, "Multi")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private libProject = Path.Combine(multiDir, "Lib", "Lib.fsproj")
let private useFs = Path.Combine(multiDir, "App", "Use.fs")
let private libFs = Path.Combine(multiDir, "Lib", "Lib.fs")
let private signedFs = Path.Combine(multiDir, "Lib", "Signed.fs")
let private signedFsi = Path.Combine(multiDir, "Lib", "Signed.fsi")

/// A location as the tests compare it: the file, and the 0-based start and end
type private Answer = (string * int * int * int * int) option

/// The 0-based line and character of the `nth` (from 0) occurrence of `needle` in `text`, `inside` characters into it
let private position (text: string) (needle: string) (nth: int) (inside: int) =
    let mutable index = -1

    for _ in 0..nth do
        index <- text.IndexOf(needle, index + 1, StringComparison.Ordinal)

    Expect.isGreaterThanOrEqual index 0 $"%s{needle} occurs %d{nth + 1} times"
    let before = text.Substring(0, index)
    before.Split('\n').Length - 1, index - (before.LastIndexOf '\n' + 1) + inside

let private atPosition (path: string) ((line, character): int * int) : JsonNode =
    let at = JsonObject()
    at["line"] <- JsonValue.Create line
    at["character"] <- JsonValue.Create character
    let document = JsonObject()
    document["uri"] <- JsonValue.Create(Paths.toUri path)
    let parameters = JsonObject()
    parameters["textDocument"] <- document
    parameters["position"] <- at
    parameters

let private definition (s: Session) (path: string) (at: int * int) : Task<Answer> =
    task {
        let! result, _ = s.Lsp.Request("textDocument/definition", atPosition path at) |> within

        match result with
        | null -> return None
        | r ->
            let range = r["range"]
            let point (name: string) (part: string) = range[name].[part].GetValue<int>()

            return
                Some(
                    (Paths.ofUri (r["uri"].GetValue<string>())).ToLowerInvariant(),
                    point "start" "line",
                    point "start" "character",
                    point "end" "line",
                    point "end" "character"
                )
    }

/// A from-scratch check of `project` with the session's texts: what FSAC's preference asks FCS for at a position.
let private fcsOf (s: Session) (project: string) : Task<string -> int * int -> Answer> =
    task {
        let! spec = ProjectSpec.load project
        let text (f: string) =
            try
                s.Lsp.Text f
            with :? System.Collections.Generic.KeyNotFoundException ->
                File.ReadAllText f

        let texts = spec.SourceFiles |> List.map (fun f -> f, text f) |> Map.ofList
        let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
        let reader (f: string) = (let t = texts[f] in hash t, lazy t)
        let checks = System.Collections.Generic.Dictionary<string, _>()

        return
            fun (path: string) ((line, character): int * int) ->
                let check =
                    match checks.TryGetValue path with
                    | true, c -> c
                    | _ ->
                        let _, c, _ =
                            checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, reader, path)
                            |> Async.RunSynchronously

                        checks[path] <- c
                        c

                match Hover.lookupOf texts[path] line character with
                | None -> None
                | Some(lineStr, names, endColumn) ->
                    match check.GetDeclarationLocation(line + 1, endColumn, lineStr, names, preferFlag = true) with
                    | FindDeclResult.DeclFound r when List.exists (fun f -> String.Equals(f, r.FileName, StringComparison.OrdinalIgnoreCase)) spec.SourceFiles ->
                        Some(r.FileName.ToLowerInvariant(), r.StartLine - 1, r.StartColumn, r.EndLine - 1, r.EndColumn)
                    | _ -> None
    }

let private start (root: string) (project: string) (opened: string list) =
    task {
        Fixtures.build project
        let! (s: Session) = startSessionWith root [ project ] true ignore
        do! openAll s opened
        return s
    }

let private modeOf (s: Session) (path: string) =
    task {
        do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
        let graph = (s.Server.Workspace.ProjectOf path).Value
        return! graph.WhenPromoted(path, CancellationToken.None) |> within
    }

/// Waits until the file is checked by declaration for its latest text: a kept row is what the tests are about. The
/// server promotes an open file after its first publish, so until then it says FileLevel.
let private promoted (s: Session) (path: string) =
    task {
        let deadline = DateTime.UtcNow.AddSeconds 60.
        let! (first: Rfs.Checker.FileMode) = modeOf s path
        let mutable mode = first

        while mode <> Rfs.Checker.FileMode.Declarations && DateTime.UtcNow < deadline do
            do! Task.Delay 25
            let! (next: Rfs.Checker.FileMode) = modeOf s path
            mode <- next

        Expect.equal mode Rfs.Checker.FileMode.Declarations $"%s{Path.GetFileName path} is checked by declaration"
    }

let private whole (s: Session) (path: string) (version: int) (text: string) =
    s.Lsp.DidChange(path, version, replace s path (s.Lsp.Text path) text)

/// Every position answers what FCS does, and FCS answers something at each
let private expectLikeFcs (s: Session) (project: string) (positions: (string * string * int * int) list) (what: string) =
    task {
        let! fcs = fcsOf s project

        for path, needle, nth, inside in positions do
            let at = position (s.Lsp.Text path) needle nth inside
            let expected = fcs path at
            Expect.isSome expected $"%s{what}: FCS finds %s{needle} (%d{nth}) in %s{Path.GetFileName path}"
            let! actual = definition s path at
            Expect.equal actual expected $"%s{what}: %s{needle} (%d{nth}) in %s{Path.GetFileName path} equals FCS"
    }

let private battery =
    "\n\ntype Rec = { Field: int; Other: string }\n\ntype Un =\n    | CaseA of named: int\n    | CaseB\n\ntype Cls(x: int) =\n    new() = Cls(0)\n    member _.Meth(a: int) = a + x\n    member _.Meth(a: string) = a\n    member _.Prop\n        with get () = x\n        and set (v: int) = ()\n    member val Auto = 1 with get, set\n    static member Stat = 1\n\nexception Boom of int\n\ntype Abbr = int\n\ntype IFace =\n    abstract Do: unit -> int\n\nmodule Nest =\n    let inner = 1\n\n    type InNest = | I\n\nlet (|Even|Odd|) n = if n % 2 = 0 then Even else Odd\n\nlet (|Small|_|) n = if n < 1 then Some n else None\n\nlet inline (+++) a b = a + b\n\ntype System.String with\n    member s.Ext = s.Length\n\ntype IOne =\n    abstract Same: int\n\ntype ITwo =\n    abstract Same: int\n\ntype Both() =\n    member _.Same = 0\n\n    interface IOne with\n        member _.Same = 1\n\n    interface ITwo with\n        member _.Same = 2\n"

let private batteryUses =
    "\n\nlet u1 = { Field = 1; Other = \"\" }\nlet u2 = u1.Field\nlet u3 = CaseA 1\nlet u4 = match u3 with CaseA(named = n) -> n | CaseB -> 0\nlet u5 = Cls()\nlet u6 = Cls(1)\nlet u7 = u5.Meth 1\nlet u8 = u5.Meth \"s\"\nlet u9 = u5.Prop\nlet u10 = u5.Prop <- 2\nlet u11 = u5.Auto\nlet u12 = Cls.Stat\nlet u13 = Boom 1\nlet u14: Abbr = 1\nlet u15 (i: IFace) = i.Do()\nlet u16 = Nest.inner\nlet u17 = Nest.I\nlet u18 = match 1 with Even -> 1 | Odd -> 2\nlet u19 = match 1 with Small x -> x | _ -> 2\nlet u20 = 1 +++ 2\nlet u21 = \"s\".Ext\nlet u22 (c: Cls) = c\nlet u23 (x: Rec) (y: Un) = x, y\nlet u24 = Deps.A.baseValue\nlet local = (let inner = 1 in inner + 1)\n"

let private batteryPositions =
    [ d, "{ Field", 0, 2
      d, "Other =", 0, 1
      d, "u1.Field", 0, 4
      d, "CaseA 1", 0, 1
      d, "CaseA(named", 0, 1
      d, "named =", 0, 1
      d, "CaseB", 0, 1
      d, "Cls()", 0, 1
      d, "Cls(1)", 0, 1
      d, "u5.Meth 1", 0, 4
      d, "u5.Meth \"s\"", 0, 4
      d, "u5.Prop\n", 0, 4
      d, "u5.Prop <-", 0, 4
      d, "u5.Auto", 0, 4
      d, "Cls.Stat", 0, 1
      d, "Cls.Stat", 0, 5
      d, "Boom 1", 0, 1
      d, "Abbr = 1", 0, 1
      d, "IFace)", 0, 1
      d, "i.Do", 0, 2
      d, "Nest.inner", 0, 1
      d, "Nest.inner", 0, 6
      d, "Nest.I\n", 0, 5
      d, "Even ->", 0, 1
      d, "Odd ->", 0, 1
      d, "Small x", 0, 1
      d, "+++ 2", 0, 1
      d, ".Ext", 0, 2
      d, "c: Cls", 0, 4
      d, "x: Rec", 0, 4
      d, "y: Un", 0, 4
      d, "A.baseValue", 0, 0
      d, "A.baseValue", 0, 4
      d, "inner + 1", 0, 1
      a, "Rec =", 0, 1
      a, "Field:", 0, 1
      a, "CaseA of", 0, 1
      a, "Cls(x", 0, 1
      a, "x: int) =\n    new", 0, 0
      a, "Meth(a: int", 0, 1
      a, "Meth(a: string", 0, 1
      a, "Prop\n", 0, 1
      a, "Auto =", 0, 1
      a, "Boom of", 0, 1
      a, "Abbr =", 0, 1
      a, "Nest =", 0, 1
      a, "(|Even|Odd|) n", 0, 3
      a, "Ext =", 0, 1
      a, "Same = 0", 0, 1
      a, "Same = 1", 0, 1
      a, "Same = 2", 0, 1 ]

let private original (path: string) = File.ReadAllText path

let private signedAbbreviation (gap: string) =
    $"module SigConform.A\n\ntype A =%s{gap} int\n\nlet user: A = Unchecked.defaultof<A>\n\ntype T() =\n    member _.M(t: int) = t\n\nlet f (x: int) y = x + y\n\nlet g (x: 'a) = x\n\nlet useM = T().M(t = 1)\n\nlet useF = f 1 2\n\nlet useG = g 3\n"

let private signedAbbreviationFsi =
    "module SigConform.A\n\ntype A = int\n\nval user: A\n\ntype T =\n    new: unit -> T\n    member M: t: int -> int\n\nval f: x: int -> (int -> int)\n\nval g: x: 'a -> 'a\n\nval useM: int\n\nval useF: int\n\nval useG: int\n"

let private sigPositions = [ sigA, "f 1 2", 0, 0; sigA, "M(t = 1)", 0, 0; sigA, "g 3", 0, 0; sigB, "useM", 0, 1; sigB, "M(t = 2)", 0, 0; sigB, "T()", 0, 0 ]

let private overloaded (member': string) =
    (original a) + $"\ntype Over() =\n%s{member'}    member _.Pick(a: int) = a\n    member _.Pick(a: string) = a\n"

/// Holds the file's lock with a read until the gate opens. Returns once it holds it.
let private holdingRead (s: Session) (path: string) (gate: ManualResetEventSlim) =
    task {
        let entered = new ManualResetEventSlim(false)
        let graph = (s.Server.Workspace.ProjectOf path).Value

        let held =
            graph.ReadFile(
                path,
                (fun _ ->
                    entered.Set()
                    gate.Wait(TimeSpan.FromSeconds 60.) |> ignore),
                CancellationToken.None
            )

        let! _ = Task.Run(fun () -> entered.Wait(TimeSpan.FromSeconds 60.)) |> within
        return held
    }

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Definition"
        [ testTask "definition equals FCS for each kind of symbol" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! whole s a 1 (original a + battery)
              do! whole s d 1 (original d + batteryUses)
              do! promoted s a
              do! promoted s d
              do! expectLikeFcs s depsProject batteryPositions "as checked"
          }

          testTask "definition across files after a blank line insert in the target" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! whole s a 1 (original a + battery)
              do! whole s d 1 (original d + batteryUses)
              do! promoted s a
              do! promoted s d
              let at = position (s.Lsp.Text d) "baseValue + 1" 0 1
              let! (before: Answer) = definition s d at
              let line = fst (position (s.Lsp.Text a) "let baseValue" 0 0)
              Expect.equal (before |> Option.map (fun (_, l, _, _, _) -> l)) (Some line) "where it is"
              do! s.Lsp.DidChange(a, 2, replace s a "let baseValue" "\nlet baseValue")
              do! promoted s a
              do! promoted s d
              let! (moved: Answer) = definition s d at
              Expect.equal (moved |> Option.map (fun (_, l, _, _, _) -> l)) (Some(line + 1)) "the new line"
              do! expectLikeFcs s depsProject batteryPositions "after the blank line"
              do! s.Lsp.DidChange(a, 3, replace s a "\nlet baseValue" "\n\n\nlet baseValue")
              do! promoted s d
              do! expectLikeFcs s depsProject batteryPositions "after two more"
          }

          testTask "external symbol returns empty" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! promoted s b
              let! (fromFsharpCore: Answer) = definition s b (position (s.Lsp.Text b) "List.length" 0 6)
              Expect.isNone fromFsharpCore "FSharp.Core"
              do! whole s d 1 (original d + "\nlet now = System.DateTime.Now\n")
              do! promoted s d
              let! (fromIl: Answer) = definition s d (position (s.Lsp.Text d) "DateTime" 0 1)
              Expect.isNone fromIl "an IL type"
              let! (blank: Answer) = definition s d (position (s.Lsp.Text d) "\n\nlet r" 0 0)
              Expect.isNone blank "no symbol"
          }

          testTask "a kept user of a re-checked value of a signed file finds the signature" {
              let! (s: Session) = start (Path.GetDirectoryName sigProject) sigProject [ sigAi; sigA; sigB ]
              use _ = disposing s
              do! promoted s sigA
              do! s.Lsp.DidChange(sigA, 1, replace s sigA "let f (x: int) y" "let f (x:  int) y")
              do! promoted s sigA
              do! s.Lsp.DidChange(sigA, 2, replace s sigA "let useG = g 3" "let useG = g 3\n\nlet z = 1")
              do! promoted s sigA
              do! expectLikeFcs s sigProject sigPositions "after f's re-check"
          }

          testTask "a blank line in a signature file moves what later files find" {
              let! (s: Session) = start (Path.GetDirectoryName sigProject) sigProject [ sigAi; sigA; sigB ]
              use _ = disposing s
              do! promoted s sigA
              do! expectLikeFcs s sigProject sigPositions "as checked"
              do! s.Lsp.DidChange(sigAi, 1, replace s sigAi "type T =" "\ntype T =")
              do! promoted s sigA
              do! expectLikeFcs s sigProject sigPositions "after the blank line"
          }

          testTask "kept users of an abbreviation find it, signed" {
              let! (s: Session) = start (Path.GetDirectoryName sigProject) sigProject [ sigAi; sigA; sigB ]
              use _ = disposing s
              do! whole s sigAi 1 signedAbbreviationFsi
              do! whole s sigA 1 (signedAbbreviation "")
              do! promoted s sigA
              do! s.Lsp.DidChange(sigA, 2, replace s sigA "type A = int" "type A =  int")
              do! promoted s sigA
              do! expectLikeFcs s sigProject [ sigA, "A = Unchecked", 0, 0; sigA, "<A>", 0, 1; sigA, "A =  int", 0, 0 ] "the same target re-checked"
          }

          testTask "kept users of a value or an abbreviation whose name moved find it, unsigned" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              let source (abbreviation: string) (value: string) =
                  original d + $"\n%s{abbreviation} = int\n\nlet held: Ab = 1\n\n%s{value} = 1\n\nlet usesV = valueHere + 1\n"

              do! whole s d 1 (source "type Ab" "let valueHere")
              do! promoted s d
              let positions = [ d, "Ab = 1", 0, 0; d, "valueHere + 1", 0, 1 ]
              do! expectLikeFcs s depsProject positions "as checked"
              do! whole s d 2 (source "type  Ab" "let  valueHere")
              do! promoted s d
              do! expectLikeFcs s depsProject positions "after the names moved along their lines"
          }

          testTask "definition into a referenced project" {
              let! (s: Session) = start multiDir appProject [ useFs; libFs; signedFsi; signedFs ]
              use _ = disposing s
              do! promoted s useFs
              do! promoted s libFs
              let at = position (s.Lsp.Text useFs) "f 1" 0 0
              let inLib () = position (s.Lsp.Text libFs) "let f (x" 0 4

              let expectAtDefinition (what: string) =
                  task {
                      let! fcs = fcsOf s libProject
                      let expected = fcs libFs (inLib ())
                      Expect.isSome expected "FCS finds f in Lib"
                      let! (actual: Answer) = definition s useFs at
                      Expect.equal actual expected what
                  }

              do! expectAtDefinition "from App to Lib"
              do! s.Lsp.DidChange(libFs, 1, replace s libFs "let f (x" "\n\nlet f (x")
              do! promoted s libFs
              do! promoted s useFs
              do! expectAtDefinition "Lib's new line: App's assembly did not change"
          }

          testTask "definition into a referenced project's signature file" {
              let! (s: Session) = start multiDir appProject [ useFs; libFs; signedFsi; signedFs ]
              use _ = disposing s
              do! whole s useFs 1 (s.Lsp.Text useFs + "\nlet viaSignature = Lib.Signed.signed\n")
              do! promoted s useFs
              let at = position (s.Lsp.Text useFs) ".signed" 0 2

              let expectAtDefinition (what: string) =
                  task {
                      let! fcs = fcsOf s libProject
                      let expected = fcs signedFs (position (s.Lsp.Text signedFs) "let signed" 0 4)
                      Expect.isSome expected "FCS finds signed in Lib"
                      let! (actual: Answer) = definition s useFs at
                      Expect.equal actual expected what
                  }

              do! expectAtDefinition "to the signature"
              do! s.Lsp.DidChange(signedFsi, 1, replace s signedFsi "val signed" "\n\nval signed")
              do! promoted s useFs
              do! expectAtDefinition "its new line"
          }

          testTask "a target held by a read makes the request wait, holding nothing of its own file" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! promoted s a
              do! promoted s d
              let at = position (s.Lsp.Text d) "baseValue + 1" 0 1
              let gate = new ManualResetEventSlim(false)
              let! held = holdingRead s a gate

              try
                  let pending = definition s d at
                  do! Task.Delay 500
                  Expect.isFalse pending.IsCompleted "it waits for the target"
                  // Takes the lock of the request's own file: this would hang if the request kept it while it waits
                  let! hover, _ = s.Lsp.Request("textDocument/hover", atPosition d at) |> within
                  Expect.isNotNull hover "the request's own file answers meanwhile"
                  Expect.isFalse pending.IsCompleted "still waiting"
                  gate.Set()
                  let! _ = held |> within
                  let! (answer: Answer) = pending
                  Expect.isSome answer "and it answers"
              finally
                  gate.Set()
          }

          testTask "a target edited while the request waits answers for the new text" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! promoted s a
              do! promoted s d
              let at = position (s.Lsp.Text d) "baseValue + 1" 0 1
              let gate = new ManualResetEventSlim(false)
              let! held = holdingRead s a gate

              try
                  let pending = definition s d at
                  do! Task.Delay 500
                  Expect.isFalse pending.IsCompleted "it waits for the target"
                  let line = fst (position (s.Lsp.Text a) "let baseValue" 0 0)
                  do! s.Lsp.DidChange(a, 1, replace s a "let baseValue" "\nlet baseValue")
                  do! Task.Delay 200
                  gate.Set()
                  let! _ = held |> within
                  let! (answer: Answer) = pending
                  Expect.equal (answer |> Option.map (fun (_, l, _, _, _) -> l)) (Some(line + 1)) "the line the edit left"
                  do! expectLikeFcs s depsProject [ d, "baseValue + 1", 0, 1 ] "and FCS agrees"
              finally
                  gate.Set()
          }

          testTask "a key gone stale finds the right overload, or nothing" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! whole s a 1 (overloaded "")
              do! whole s d 1 (original d + "\nlet o = Over().Pick \"s\"\n")
              do! promoted s a
              do! promoted s d
              let at = position (s.Lsp.Text d) "Pick" 0 1
              do! expectLikeFcs s depsProject [ d, "Pick", 0, 1 ] "as checked"

              let whileWaiting (version: int) (text: string) =
                  task {
                      let gate = new ManualResetEventSlim(false)
                      let! held = holdingRead s a gate

                      try
                          // The request looks the key up in D, then waits for A, whose text then changes under the key
                          let pending = definition s d at
                          do! Task.Delay 500
                          Expect.isFalse pending.IsCompleted "it waits for the target"
                          do! whole s a version text
                          do! Task.Delay 200
                          gate.Set()
                          let! _ = held |> within
                          return! pending
                      finally
                          gate.Set()
                  }

              let! (shifted: Answer) = whileWaiting 2 (overloaded "    member _.Pick(a: char) = a\n")
              let line = fst (position (s.Lsp.Text a) "Pick(a: string" 0 0)
              Expect.equal (shifted |> Option.map (fun (_, l, _, _, _) -> l)) (Some line) "an overload added before it: the same overload at its line, where an index would give the one before"
              do! promoted s d
              // The overload it names goes: nothing, not its neighbour
              let! (gone: Answer) = whileWaiting 3 (s.Lsp.Text a |> fun t -> t.Replace("    member _.Pick(a: string) = a\n", ""))
              Expect.isNone gone "the key finds nothing"
          }

          testTask "a file-level file answers: module rec and fragments with an augmentation" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! whole s a 1 ((original a).Replace("module Deps.A", "module rec Deps.A"))
              let fragment = "\nnamespace Deps.Extra\n\ntype X() =\n    member _.One = 1\n\ntype X with\n    member _.Two = 2\n"
              do! whole s c 1 (original c + fragment)
              do! whole s d 1 (original d + "\nlet viaX = Deps.Extra.X().Two + 1\n")
              let! (modeA: Rfs.Checker.FileMode) = modeOf s a
              Expect.equal modeA Rfs.Checker.FileMode.FileLevel "module rec is one declaration"
              do! promoted s d
              let positions = [ d, "baseValue + 1", 0, 1; d, "X().Two", 0, 0; d, "Two +", 0, 1 ]
              do! expectLikeFcs s depsProject positions "as checked"
              do! s.Lsp.DidChange(a, 2, replace s a "let baseValue" "\nlet baseValue")
              do! s.Lsp.DidChange(c, 2, replace s c "namespace Deps.Extra" "\n\nnamespace Deps.Extra")
              do! promoted s d
              do! expectLikeFcs s depsProject positions "after blank lines in them"
          }

          testTask "a project going away answers nothing" {
              let! (s: Session) = start depsDir depsProject [ a; b; c; d; file "E.fsi"; file "E.fs" ]
              use _ = disposing s
              do! promoted s a
              do! promoted s d
              let at = position (s.Lsp.Text d) "baseValue + 1" 0 1
              let gate = new ManualResetEventSlim(false)
              let! _held = holdingRead s a gate
              let pending = definition s d at
              do! Task.Delay 500
              Expect.isFalse pending.IsCompleted "it waits for the target"
              let reload = s.Server.Workspace.Reload depsProject
              let! (answer: Answer) = pending
              gate.Set()
              do! reload
              Expect.isNone answer "the graph it waited on went away"
              do! s.Server.Publisher.Barrier().WaitAsync(TimeSpan.FromSeconds 120.)
              (s.Server :> IDisposable).Dispose()
              let! (afterShutdown: Answer) = definition s d at
              Expect.isNone afterShutdown "after shutdown"
          } ]
