module Rfs.Checker.Tests.RecheckTests

open System
open System.Threading.Tasks
open Expecto
open Rfs.Checker
open Rfs.Checker.Tests.Deps

let private d = fileOf "D.fs"

let private modes = [ "by declaration from the start", Declarations; "promoted from file level", FileLevel ]

/// `body` once per way a file comes to be checked by declaration, with D.fs promoted.
let private inBothModes (name: string) (body: Session -> Task) =
    testList
        name
        [ for label, mode in modes ->
              testTask label {
                  let! s = startWith mode
                  use _ = (s :> IAsyncDisposable)
                  s.Graph.Promote d
                  do! body s
              } ]

let private lines (rows: string list) = "module Deps.D\n\n" + String.Join("\n\n", rows) + "\n"

let private x1 (literal: int) (k: string) =
    lines [ $"type T() =\n    member _.A = %d{literal}"; "let useT (t: T) = t.A"; k ]

let private x9 (member': string) =
    lines [ "type T() =\n    member _.A = 1"; "let f (x: int) = x"; $"type T with\n    %s{member'}"; "let y = f (T())" ]

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Recheck"
        [ inBothModes "a user re-checked after its type's body changed sees one entity" (fun s ->
              task {
                  let! r = edit s d (x1 1 "let k = useT (T())")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (x1 2 "let k = useT (T())")
                  Expect.equal (r.NamesIn d) [ "T"; "k"; "useT" ] "T and the rows holding its entity"
                  do! expectOracle s r.Diagnostics "T's body edited"
                  let! r = edit s d (x1 2 "let k = useT (T()) + 0")
                  Expect.equal (r.NamesIn d) [ "k" ] "only k"
                  do! expectOracle s r.Diagnostics "k edited after T's body"
              })

          inBothModes "a type's users through a value re-check with it" (fun s ->
              task {
                  let source (literal: int) (e: string) =
                      lines [ $"type T() =
    member _.A = %d{literal}"; "let a = T()"; "let b = a"; "let c (x: T) = x"; e ]

                  let! r = edit s d (source 1 "let e = c b")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source 2 "let e = c b")
                  Expect.equal (r.NamesIn d) [ "T"; "a"; "b"; "c"; "e" ] "every row holding T's entity, b only through a's type"
                  do! expectOracle s r.Diagnostics "T's body edited"
                  let! r = edit s d (source 2 "let e = c (c b)")
                  Expect.equal (r.NamesIn d) [ "e" ] "only e"
                  do! expectOracle s r.Diagnostics "e edited"
              })

          inBothModes "a subclass's users re-check when its base does" (fun s ->
              task {
                  let source (literal: int) (w: string) =
                      lines [ $"type T() =
    member _.A = %d{literal}"; "type U() =
    inherit T()"; "let mk () = U()"; "let takesT (t: T) = t.A"; w ]

                  let! r = edit s d (source 1 "let w = takesT (mk ())")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source 2 "let w = takesT (mk ())")
                  do! expectOracle s r.Diagnostics "T's body edited"
                  let! r = edit s d (source 2 "let w = takesT (mk ()) + 0")
                  do! expectOracle s r.Diagnostics "w edited"
              })

          testTask "a kept value of a signed file holding the old entity conforms" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let e = fileOf "E.fs"
              let sigFile = fileOf "E.fsi"

              let source (literal: int) =
                  $"module Deps.E\n\ntype T() =\n    member _.A = %d{literal}\n\nlet useT (t: T) = t.A\n\nlet user = 1\n\nlet compute () = 1\n\nlet unrelated = 7\n"

              let! _ = edit s e (source 1)
              let! r = edit s sigFile "module Deps.E\n\ntype T =\n    new: unit -> T\n    member A: int\n\nval useT: T -> int\n\nval user: int\n\nval compute: unit -> int\n\nval unrelated: int\n"
              do! expectOracle s r.Diagnostics "the source"
              let! r = edit s e (source 2)
              do! expectOracle s r.Diagnostics "T's body edited, useT kept"
              let! r = edit s e (source 3)
              do! expectOracle s r.Diagnostics "T's body edited again"
          }

          testTask "a later file set up from a kept user's signature sees one entity" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let later = fileOf "E.fs"
              let! r = edit s d (x1 1 "let k = 0")
              do! expectOracle s r.Diagnostics "the source"
              let! r = edit s later (s.Texts[later] + "\nlet viaD = Deps.D.useT (Deps.D.T())\n")
              do! expectOracle s r.Diagnostics "a later file uses both"
              let! r = edit s d (x1 2 "let k = 0")
              do! expectOracle s r.Diagnostics "T's body edited"
              let! r = edit s d (x1 2 "let k = 0\n\nlet added = 1")
              do! expectOracle s r.Diagnostics "D.fs's signature changed, so the later file begins again"
          }

          inBothModes "an unrelated row is kept when a type re-checks" (fun s ->
              task {
                  let source (literal: int) = x1 literal "let k = useT (T())" + "
let z = 1
"
                  let! _ = edit s d (source 1)
                  let! r = edit s d (source 2)
                  Expect.isFalse (List.contains "z" (r.NamesIn d)) "z holds nothing of T"
                  do! expectOracle s r.Diagnostics "T's body edited"
              })

          // Named holds every declaration defining a member of a name after a dot; this row holds no entity of U
          inBothModes "a row sharing only a member's name with a type is kept when it re-checks" (fun s ->
              task {
                  let source (literal: int) =
                      lines [ $"type U() =
    member _.Length = %d{literal}"; "let n = \"abc\".Length" ]

                  let! _ = edit s d (source 1)
                  let! r = edit s d (source 2)
                  Expect.equal (r.NamesIn d) [ "U" ] "only U"
                  do! expectOracle s r.Diagnostics "U's body edited"
              })

          inBothModes "a value's body edit keeps its users" (fun s ->
              task {
                  let source (literal: int) =
                      lines [ $"let rec f (x: int) : int = if x > 0 then f (x - 1) else %d{literal}"; "let g = f 1" ]

                  let! _ = edit s d (source 1)
                  let! r = edit s d (source 2)
                  Expect.equal (r.NamesIn d) [ "f" ] "a value defines no entity"
                  do! expectOracle s r.Diagnostics "f's body edited"
              })

          inBothModes "an augmentation's op_Implicit reaches a later row" (fun s ->
              task {
                  let! r = edit s d (x9 "member _.Other = 1")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (x9 "static member op_Implicit(t: T) : int = 1")
                  do! expectOracle s r.Diagnostics "op_Implicit added"
              }) ]
