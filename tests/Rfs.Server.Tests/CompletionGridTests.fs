module Rfs.Server.Tests.CompletionGridTests

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open Rfs.Checker
open Rfs.Projects
open Rfs.Server
open Rfs.Server.Tests.PublishTests

/// Item names at every position of a text, as FCS lists them from scratch, unedited and after the scenario's edits. This
/// is what hand-picked positions missed for two rounds: which row answers a position depends on what every row captured.
let private wait (t: Task<'a>) = t.WaitAsync(TimeSpan.FromSeconds 110.)

let private namesOf (items: FSharp.Compiler.EditorServices.DeclarationListItem[]) =
    items |> Array.map _.NameInList |> Set.ofArray

let private trunc (s: Set<string>) = s |> Set.toList |> List.truncate 6

/// The differences of the last text's list from FCS's, at every position, with how many positions there were
let private grid (signature: string option) (texts: string list) =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-grid-"

        try
            let path = Path.Combine(dir.FullName, "D.fs")
            let sigPath = Path.Combine(dir.FullName, "D.fsi")
            File.WriteAllText(path, texts.Head)
            signature |> Option.iter (fun t -> File.WriteAllText(sigPath, t))
            let! deps = ProjectSpec.load depsProject

            let spec =
                { ProjectSpec.sequential deps with
                    ProjectFile = Path.Combine(dir.FullName, "P.fsproj")
                    SourceFiles = (match signature with Some _ -> [ sigPath; path ] | None -> [ path ]) }

            use host = GraphHost.Start "grid"
            let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec)
            graph.Promote path
            let! _ = graph.WhenPromoted(path, CancellationToken.None) |> wait

            for t in texts do
                graph.SetText(path, t) |> ignore
                let! _ = graph.AllDiagnostics() |> wait
                let! _ = graph.WhenPromoted(path, CancellationToken.None) |> wait
                ()

            let text = List.last texts

            let map =
                Map.ofList
                    [ yield path, text
                      match signature with
                      | Some t -> yield sigPath, t
                      | None -> () ]

            let checker = InteractiveChecker.Create(ProjectSpec.toFcsOptions spec)
            let reader (f: string) = (let t = map[f] in hash t, lazy t)

            let! parse, check, _ =
                checker.ParseAndCheckFileInProject(spec.ProjectFile, Array.ofList spec.SourceFiles, reader, path)
                |> Async.StartAsTask

            let lines = text.Split '\n'
            let differences = ResizeArray<string>()
            let mutable total = 0

            for l in 0 .. lines.Length - 1 do
                for c in 0 .. lines[l].Length do
                    total <- total + 1
                    let! ours = graph.ReadFile(path, (fun v -> namesOf (Completion.itemsAt v l c)), CancellationToken.None) |> wait
                    let reference = namesOf (Completion.itemsOf check parse text l c)

                    if ours <> reference then
                        differences.Add(
                            sprintf "(%d,%d) [%s|%s] only ours %A, only FCS %A" l c (lines[l].Substring(0, c)) (lines[l].Substring c) (trunc (ours - reference)) (trunc (reference - ours))
                        )

            graph.Dispose()
            return total, List.ofSeq differences
        finally
            try
                dir.Delete true
            with :? IOException | :? UnauthorizedAccessException ->
                ()
    }

let private expectNone (name: string) signature texts =
    task {
        let! (total: int), (differences: string list) = grid signature texts
        Expect.isGreaterThan total 0 "positions"
        let shown = differences |> List.truncate 8 |> String.concat "\n  "
        Expect.isTrue differences.IsEmpty $"%s{name}: %d{differences.Length} of %d{total} positions differ from FCS:\n  %s{shown}"
    }

let private keptText = "module Deps.D\n\nlet oldName = 1\n\nlet kept =\n    let inner = 3\n    inner\n\nlet other = kept + 1\n"
let private scenarios : (string * string option * string list * string list) list =
  let ren (a: string) (b: string) (t: string) = [ t; t.Replace(a, b) ]
  [ "plain lets, rename first", None, ren "oldName" "newName" keptText, []
    "plain lets, rename, edit middle row", None, [ keptText; keptText.Replace("oldName","newName"); keptText.Replace("oldName","newName").Replace("inner = 3","inner = 5") ], []
    "gaps with comments", None, ren "oldName" "newName" "// top\nmodule Deps.D\n\n// c1\nlet oldName = 1\n\n// c2\n\nlet kept =\n    let inner = 3\n    inner\n    // inside after\n\n// c3\n\nlet other = kept + 1\n\n// end\n", []
    "split module", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nmodule Inner =\n    let a = 1\n    let b =\n        let q = 2\n        q\n    let c = 3\n\nlet last = 4\n", []
    "nested split modules", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nmodule A =\n    let a0 = 0\n    module B =\n        let b0 = 1\n\n        let b1 = 2\n    let a1 = 3\n    module C =\n        let c0 = 1\n\nlet last = 4\n", []
    "rename inside a split module", None, ren "oldIn" "newIn" "module Deps.D\n\nmodule Inner =\n    let oldIn = 1\n    let b = 2\n\n    let c =\n        let q = 2\n        q\n\nlet last = 4\n", []
    "edit a module name", None, ren "module Inner" "module Inner2" "module Deps.D\n\nlet z = 0\n\nmodule Inner =\n    let a = 1\n    let b = 2\n\nlet last = 4\n", []
    "attributes on rows", None, ren "oldName" "newName" "module Deps.D\nopen System\n\nlet oldName = 1\n\n[<Obsolete(\"a\")>]\nlet att = 1\n\n[<Literal>]\nlet K = 1\n\n[<Sealed>]\ntype T() =\n    member _.A = 1\n\nlet last = 4\n", []
    "attribute on a row right after the edit, inside a module", None, ren "oldName" "newName" "module Deps.D\nopen System\n\nmodule M =\n    let oldName = 1\n    [<Obsolete(\"a\")>]\n    let att = 1\n    let z = 2\n", []
    "open in the middle", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nopen System\n\nlet u = DateTime.Now\n\nopen System.IO\n", []
    "namespace fragments", None, ren "module M1" "module M1b" "namespace Deps.N1\n\nmodule M1 =\n    let y = 1\n\nnamespace Deps.N1\n\nmodule M2 =\n    let z = 2\n\n    let w = 3\n", []
    "namespace with rows", None, ren "type Old" "type Neu" "namespace Deps.N1\n\ntype Old = int\n\ntype Kept() =\n    member _.A = 1\n\nmodule M =\n    let z = 2\n", []
    "last module rec", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nlet mid = 2\n\nmodule rec Inner =\n    let i = 1\n    let j = i\n", []
    "record field added: row after has a nested let", None, ren "{ F: int }" "{ F: int; G: int }" "module Deps.D\n\ntype R = { F: int }\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n", []
    "record field added: a gap after the nested row", None, ren "{ F: int }" "{ F: int; G: int }" "module Deps.D\n\ntype R = { F: int }\n\nlet c =\n    let q = 2\n    q\n\n", []
    "record field removed", None, ren "{ F: int; G: int }" "{ F: int }" "module Deps.D\n\ntype R = { F: int; G: int }\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n", []
    "record renamed, kept rows", None, ren "type R " "type R2 " "module Deps.D\n\ntype R = { F: int }\n\nlet m = 1\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n", []
    "record added, row after", None, [ "module Deps.D\n\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n"; "module Deps.D\n\ntype R = { F: int }\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n" ], []
    "extension renamed, dots after", None, ren "Ext1" "Ext2" "module Deps.D\n\ntype System.String with\n    member s.Ext1 = 1\n\nlet a = \"x\".Length\n\nlet b = \"x\".Length\n\nlet c (t: string) = t.Length\n", []
    "extension renamed with a row between", None, ren "Ext1" "Ext2" "module Deps.D\n\ntype System.String with\n    member s.Ext1 = 1\n\nlet between = 1\n\nlet a = \"x\".Length\n\nlet t = \"y\"\n\nlet c =\n    t.Length\n", []
    "augmentation segment", None, ren "let earlier" "let earlier2" "module Deps.D\n\nlet earlier = 1\n\ntype T() =\n    member x.M(o: T) = o.ToString()\n\nlet t = T()\n\nlet u = t.M(t)\n\ntype T with\n    member _.B = 2\n\nlet v = T().M(t)\n", []
    "window segment", None, ren "let earlier" "let earlier2" "module Deps.D\n\nlet earlier = 1\n\nlet r = ref []\n\nlet look () = r.Value.Length\n\nlet s = r.Value.Length\n\nlet solve () = r.Value <- [ 1 ]\n\nlet after = r.Value.Head\n", []
    "fresh row just before a stale window member", None, ren "let earlier" "let earlier2" "module Deps.D\n\nlet earlier = (ref []).Value.Length\n\nlet last = 1\n", []
    "leader with a dotted body", None, ren "let earlier" "let earlier2" "module Deps.D\n\nlet earlier = 1\n\nlet r = (ref []).Value\n\nlet r2 = (let w = ref [] in w).Value.Length\n", []
    "augmentation, edit the row just above the type", None, ren "let earlier = 1" "let earlier = 2" "module Deps.D\n\nlet earlier = 1\n\ntype T() =\n    static member S = T()\n    member x.M = T.S.ToString()\n\ntype T with\n    member _.B = 2\n", []
    "a let with no nested env, dot on a type augmented later", None, ren "let earlier = 1" "let earlier = 2" "module Deps.D\n\nlet earlier = 1\n\ntype T() =\n    member _.A = 1\n\nlet t = T()\n\nlet z = t.ToString()\n\ntype T with\n    member _.B = 2\n", []
    "body edit, print the same", None, ren "member _.P = 1" "member _.P = 2" "module Deps.D\n\ntype A() =\n    member _.P = 1\n\nlet a = A()\n\nlet b = a.P\n", [ "P" ]
    "member doc edit", None, ren "/// doc one" "/// doc two" "module Deps.D\n\ntype A() =\n    /// doc one\n    member _.P = 1\n\nlet a = A()\n\nlet b = a.P\n\nlet c =\n    let d = a\n    d.P\n", [ "P" ]
    "val doc edit", None, ren "/// doc one" "/// doc two" "module Deps.D\n\n/// doc one\nlet f = 1\n\nlet m = 2\n\nlet c =\n    let d = f\n    d\n", [ "f" ]
    "signed: names and docs in a stale row", Some "module Deps.D\n\n/// Adds one\nval f: x: int -> int\n\ntype T =\n    new: unit -> T\n    /// Sig doc of M\n    member M: x: int -> int\n    member N: int\n\nval last: int\n", ren "let other = 1" "let other2 = 1" "module Deps.D\n\nlet f y = y + 1\n\nlet other = 1\n\ntype T() =\n    member _.M(y: int) = y\n    member this.N = this.M(1)\n\nlet last =\n    let t = T()\n    t.M(y = 1) + f 1\n", [ "f"; "M"; "x"; "y" ]
    "signed: a stale segment", Some "module Deps.D\n\n/// Adds one\nval f: x: int -> int\n\n/// Sig doc of T\ntype T =\n    new: unit -> T\n    /// Sig doc of M\n    member M: x: int -> int\n    member N: int\n    member B: int\n\nval last: int\n", ren "let other = 1" "let other2 = 1" "module Deps.D\n\nlet f y = y + 1\n\nlet other = 1\n\ntype T() =\n    member _.M(y: int) = y\n    member this.N = this.M(y = 1)\n\ntype T with\n    member this.B = this.M(y = 2)\n\nlet last = T().M(y = 1) + f 1\n", [ "f"; "M"; "T"; "x"; "y" ] ]

let private scenarios2 : (string * string option * string list * string list) list =
  let ren (a: string) (b: string) (t: string) = [ t; t.Replace(a, b) ]
  [ "a row appended after a kept last row", None, [ "module Deps.D\n\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n"; "module Deps.D\n\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n\nlet e =\n    let r = 5\n    r\n" ], []
    "a row appended inside a split module", None, [ "module Deps.D\n\nmodule M =\n    let p = 1\n\nlet z = 1\n"; "module Deps.D\n\nmodule M =\n    let p = 1\n\n    let c = 2\n\nlet z = 1\n" ], []
    "the last row removed", None, [ "module Deps.D\n\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n"; "module Deps.D\n\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n" ], []
    "do row after a nested module", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nmodule Inner =\n    let a = 1\n\nSystem.Console.WriteLine(Inner.a.ToString())\n\nlet z = 1\n", []
    "open and type after a nested module", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nmodule Inner =\n    let a = 1\n\nopen System\n\nmodule Inner2 =\n    let b = 2\n\ntype U() =\n    member _.A = 1\n", []
    "a nested module last, gap and end", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nmodule Inner =\n    let a = 1\n\n    let b =\n        let q = 2\n        q\n\n", []
    "a row in the module, typing at a nested let after a nested module", None, ren "oldName" "newName" "module Deps.D\n\nlet oldName = 1\n\nmodule Inner =\n    let a = 1\n\nlet after =\n    let q = 1\n    q\n", []
    "signed: the row that answers is the documented one", Some "module Deps.D\n\nval z: int\n\n/// Adds one\nval f: x: int -> int\n\nval g: int\n\nval h: int\n", ren "let z = 1" "let z = 2" "module Deps.D\n\nlet z = 1\n\nlet f y = y + 1\n\nlet g = 1\n\nlet h =\n    let inner = f 1\n    inner\n", [ "f"; "g" ]
    "signed: the row that answers is the documented one, renamed name above", Some "module Deps.D\n\n/// Adds one\nval f: x: int -> int\n\nval h: int\n", ren "let priv = 1" "let priv2 = 1" "module Deps.D\n\nlet priv = 1\n\nlet f y = y + 1\n\nlet h = 1\n", [ "f" ]
    "signed: record and union docs", Some "module Deps.D\n\nval z: int\n\n/// Sig R\ntype R =\n    {\n        /// Sig F\n        F: int\n    }\n\n/// Sig U\ntype U =\n    /// Sig C\n    | C of int\n\nval h: R\n", ren "let priv = 1" "let priv2 = 1" "module Deps.D\n\nlet priv = 1\nlet z = 1\n\ntype R = { F: int }\n\ntype U =\n    | C of int\n\nlet h = { F = 1 }\n", [ "R"; "F"; "U"; "C" ]
    "record row inserted above a row with locals", None, [ "module Deps.D\n\nlet p = 1\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n"; "module Deps.D\n\nlet p = 1\n\ntype R = { F: int }\n\nlet c =\n    let q = 2\n    q\n\nlet d = 4\n" ], []
    "record row edited mid file, functions below", None, ren "{ F: int }" "{ F: int; G: int }" "module Deps.D\n\nlet p = 1\n\ntype R = { F: int }\n\nlet f (arg: int) =\n    let loc = arg + 1\n    loc\n\nlet g (arg2: int) =\n    let loc2 = arg2 + 1\n    loc2\n", []
    "open added above a row with locals", None, [ "module Deps.D\n\nlet c =\n    let q = 2\n    q\n"; "module Deps.D\n\nopen FSharp.Core.CompilerServices\n\nlet c =\n    let q = 2\n    q\n" ], []
    "defaults, in the row and from a leader", None, ren "let earlier" "let earlier2" "module Deps.D\n\nlet earlier = 1\n\nlet y =\n    let z = ref None\n    z.Value.Value.ToString()\n\nlet w = ref None\n\nlet y2 = w.Value.Value.ToString()\n", []
    "insert a comment line, shift only", None, [ keptText; keptText.Replace("let kept", "// a line\nlet kept") ], []
    "insert line and rename", None, [ keptText; keptText.Replace("let oldName", "// a line\nlet newName") ], []
    "line delete", None, [ "module Deps.D\n\nlet oldName = 1\n\n// gone\n\nlet kept =\n    let inner = 3\n    inner\n\nlet other = kept + 1\n"; "module Deps.D\n\nlet newName = 1\n\nlet kept =\n    let inner = 3\n    inner\n\nlet other = kept + 1\n" ], [] ]

/// C3: a row's scope depends on what follows it, so a row kept since it was last has captured a scope that has moved.
/// Orders of two or more edits, each with a row that was last when it was checked, then rows inserted around it.
let private editOrders : (string * string option * string list * string list) list =
  let head = "module Deps.D\n\nlet f (x: int) = x + 1\n\n"
  let rows = "let first = f 1\n\nlet second = \"two\"\n\nlet third = first\n"
  let tail = "\nlet g2 = third + 1\n"
  let above = "let inserted = \"s\"\n\n"
  let locals = "let third =\n    let loc = first\n    loc\n"
  let lastHasLocals = rows.Replace("let third = first\n", locals)
  [ "append, then insert above", None, [ head + rows; head + rows + tail; head + above + rows + tail ], []
    "insert above, then append", None, [ head + rows; head + above + rows; head + above + rows + tail ], []
    "append, then insert below it", None, [ head + rows; head + rows + tail; head + rows + tail + "\nlet below = 1\n" ], []
    "append twice, then insert above", None, [ head + rows; head + rows + tail; head + rows + tail + "\nlet g3 = g2\n"; head + above + rows + tail + "\nlet g3 = g2\n" ], []
    "insert above twice after an append", None, [ head + rows; head + rows + tail; head + above + rows + tail; head + above + "let again = 2\n\n" + rows + tail ], []
    "append, insert above, then edit a body", None, [ head + rows; head + rows + tail; head + above + rows + tail; head + above + rows.Replace("f 1", "f 2") + tail ], []
    "append, insert above, then edit the appended row", None, [ head + rows; head + rows + tail; head + above + rows + tail; head + above + rows + tail.Replace("+ 1", "+ 2") ], []
    "append, edit a body, then insert above", None, [ head + rows; head + rows + tail; head + rows.Replace("f 1", "f 2") + tail; head + above + rows.Replace("f 1", "f 2") + tail ], []
    "the last row has locals: append, then insert above", None, [ head + lastHasLocals; head + lastHasLocals + tail; head + above + lastHasLocals + tail ], []
    "the last row has a gap after it: append, then insert above", None, [ head + rows + "\n\n"; head + rows + "\n\n" + tail + "\n\n"; head + above + rows + "\n\n" + tail + "\n\n" ], []
    "a comment line above after an append", None, [ head + rows; head + rows + tail; head + "// a line\n" + rows + tail ], []
    // N1: the scope a kept row captured ends where the module ended when it was checked
    "edit the end of the last row, then insert above", None, [ head + rows; head + rows.Replace("= first\n", "= first + 0\n"); head + above + rows.Replace("= first\n", "= first + 0\n") ], []
    "edit the end of the last row, then a comment line above", None, [ head + rows; head + rows.Replace("= first\n", "= first + 0\n"); head + "// a line\n" + rows.Replace("= first\n", "= first + 0\n") ], []
    "append, then insert above twice", None, [ head + rows; head + rows + tail; head + "// a line\n" + rows + tail; head + "// a line\n// two\n" + rows + tail ], []
    "append, then remove the appended row, then insert above", None, [ head + rows; head + rows + tail; head + rows; head + above + rows ], []
    "inside a module: append, then insert above", None,
    [ "module Deps.D\n\nmodule M =\n    let a = 1\n\n    let b = a\n\nlet z = 1\n"
      "module Deps.D\n\nmodule M =\n    let a = 1\n\n    let b = a\n\n    let c = b\n\nlet z = 1\n"
      "module Deps.D\n\nmodule M =\n    let inserted = 0\n\n    let a = 1\n\n    let b = a\n\n    let c = b\n\nlet z = 1\n" ], [] ]

/// Every order of two kinds of edit, as texts built from the rows and the lines between them.
let private editPairs : (string * string option * string list * string list) list =
  let render (top: string list, rows: string list) = "module Deps.D\n\n" + String.concat "\n" (top @ rows) + "\n"
  let start = [], [ "let f (x: int) = x + 1\n"; "let first = f 1\n"; "let second = \"two\"\n"; "let third = first\n" ]

  let ops : (string * ((string list * string list) -> (string list * string list))) list =
    [ "append", (fun (t, r) -> t, r @ [ $"let added%d{r.Length} = 1\n" ])
      "remove last", (fun (t, r) -> t, List.truncate (r.Length - 1) r)
      "row above", (fun (t, r) -> t, $"let above%d{r.Length} = \"s\"\n" :: r)
      "comment above", (fun (t, r) -> "// a line\n" :: t, r)
      "comment between", (fun (t, r) -> t, List.insertAt 2 "// between\n" r)
      "body", (fun (t, r) -> t, r |> List.map (fun x -> x.Replace("f 1", "f 2").Replace("f 2", "f 3"))) ]

  [ for name1, op1 in ops do
        for name2, op2 in ops do
            let one = op1 start
            let two = op2 one
            $"%s{name1}, then %s{name2}", None, [ render start; render one; render two ], [] ]

let private all = scenarios @ scenarios2 @ editOrders @ editPairs

/// The declarations checked, and the rows shifted, by the last of `texts`, each settled in turn
let private lastEditRuns (texts: string list) =
    task {
        let dir = Directory.CreateTempSubdirectory "rfs-n1-"

        try
            let path = Path.Combine(dir.FullName, "D.fs")
            File.WriteAllText(path, texts.Head)
            let! deps = ProjectSpec.load depsProject

            let spec =
                { ProjectSpec.sequential deps with
                    ProjectFile = Path.Combine(dir.FullName, "P.fsproj")
                    SourceFiles = [ path ] }

            use host = GraphHost.Start "n1"
            let! (graph: ProjectGraph) = ProjectGraph.Create(host, spec)
            graph.Promote path
            let! _ = graph.WhenPromoted(path, CancellationToken.None) |> wait
            let ran = System.Collections.Concurrent.ConcurrentQueue<string>()
            use _sub = graph.Ran |> Observable.subscribe (fun (k: FSharp.Compiler.Declarations.DeclarationKey, _) -> ran.Enqueue k.Name)
            let mutable shifted = 0L

            for t in texts do
                ran.Clear()
                shifted <- graph.ShiftedRows
                graph.SetText(path, t) |> ignore
                let! _ = graph.AllDiagnostics() |> wait
                let! _ = graph.WhenPromoted(path, CancellationToken.None) |> wait
                ()

            let result = List.ofSeq ran |> List.distinct |> List.sort, graph.ShiftedRows - shifted
            graph.Dispose()
            return result
        finally
            try
                dir.Delete true
            with :? IOException | :? UnauthorizedAccessException ->
                ()
    }

let private n1Head = "module Deps.D\n\nlet f (x: int) = x + 1\n\n"
let private n1Rows = "let first = f 1\n\nlet second = \"two\"\n\nlet third = first\n"

[<Tests>]
let tests =
    testList
        "CompletionGrid"
        [ testList
              "after the edits"
              [ for name, signature, texts, _ in all ->
                    testTask name { do! expectNone name signature texts } ]
          testList
              "unedited"
              [ for name, signature, texts, _ in all ->
                    testTask name { do! expectNone name signature [ List.last texts ] } ]
          // N1: rows kept since an edit at the end of the file are shifted by a line inserted above them, not checked again
          testList
              "rows shifted after an edit at the end"
              [ testTask "edit the last row's end, then insert a row near the top" {
                    let edited = n1Rows.Replace("= first\n", "= first + 0\n")
                    let! ran, shifted = lastEditRuns [ n1Head + n1Rows; n1Head + edited; "module Deps.D\n\nlet inserted = \"s\"\n\n" + n1Head.Substring 15 + edited ]
                    Expect.equal ran [ "inserted" ] "only the inserted row is checked"
                    Expect.equal shifted 4L "f, first, second and third are shifted"
                }
                testTask "append a row, then insert a comment line near the top" {
                    let tail = "\nlet g2 = third + 1\n"
                    let! ran, shifted = lastEditRuns [ n1Head + n1Rows; n1Head + n1Rows + tail; n1Head + "// a line\n" + n1Rows + tail ]
                    Expect.equal ran [] "nothing is checked"
                    Expect.equal shifted 4L "first, second, third and g2 are shifted"
                } ] ]
