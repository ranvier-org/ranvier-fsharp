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

/// Re-checks after editing `type A = int` to `string`, and back, with the rows `rest` gives over it.
let private abbreviationBattery (name: string) (abbreviation: string -> string) (rest: string list) =
    inBothModes name (fun s ->
        task {
            let source (target: string) = lines (abbreviation target :: rest)
            let! r = edit s d (source "int")
            do! expectOracle s r.Diagnostics "int"
            let! r = edit s d (source "string")
            do! expectOracle s r.Diagnostics "string"
            let! r = edit s d (source "int")
            do! expectOracle s r.Diagnostics "int again"
            let! r = edit s d (source "int64")
            do! expectOracle s r.Diagnostics "int64"
        })

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

          testTask "an edit every later row depends on stays linear" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let n = 400

              let rows =
                  String.Join("\n\n", [ for i in 1..n -> $"let v%d{i} (x: int) = (x + %d{i}) * List.length [ x; x ]" ])

              let measure (first: string) (second: string) =
                  task {
                      let! _ = edit s d first
                      let! _ = edit s d second
                      let! _ = edit s d first
                      let extends = s.Graph.EnvExtends
                      let allocated = GC.GetTotalAllocatedBytes true
                      let! r = edit s d second
                      Expect.isGreaterThanOrEqual r.Delta (int64 n) "every row re-checks"
                      return s.Graph.EnvExtends - extends, GC.GetTotalAllocatedBytes true - allocated
                  }

              let openOf (name: string) = $"module Deps.D\n\nopen %s{name}\n\n%s{rows}\n"
              let! singleExtends, singleAllocated = measure (openOf "System") (openOf "System.IO")
              let segmentOf (w: int) = $"module Deps.D\n\nlet r = ref []\n\nlet w = %d{w}\n\n%s{rows}\n"
              let! _, segmentAllocated = measure (segmentOf 1) (segmentOf 2)
              printfn $"single rows: %d{singleExtends} ExtendEnv, %d{singleAllocated / 1048576L} MB; segment: %d{segmentAllocated / 1048576L} MB"
              Expect.isLessThanOrEqual singleExtends (int64 (2 * n)) "environments are assembled in file order"
              Expect.isLessThan singleAllocated (3L * segmentAllocated) "the same rows cost about what a segment does"
          }

          inBothModes "a row after the solver re-checks alone" (fun s ->
              task {
                  let source (after: int) =
                      lines [ "let r = ref []"; "let fill () = r.Value <- [ 1 ]"; "let size () = 1"; $"let after = %d{after}" ]

                  let! r = edit s d (source 42)
                  do! expectOracle s r.Diagnostics "the source"
                  Expect.equal (s.Graph.SegmentRows d) 2 "r and fill"
                  let! r = edit s d (source 43)
                  Expect.equal (r.NamesIn d) [ "after" ] "only after"
                  do! expectOracle s r.Diagnostics "after edited"
              })

          inBothModes "a line insert after the window shifts" (fun s ->
              task {
                  let source (gap: string) =
                      "module Deps.D\n\nlet r = ref []\n\nlet fill () = r.Value <- [ 1 ]\n\n" + gap + "let after = 42\n"

                  let! _ = edit s d (source "")
                  let! r = edit s d (source "\n")
                  Expect.equal r.Delta 0L "nothing re-checks"
                  do! expectOracle s r.Diagnostics "line inserted"
              })

          inBothModes "three windows in one file are separate" (fun s ->
              task {
                  let source (second: int) =
                      lines
                          [ "let r1 = ref []"
                            "let fill1 () = r1.Value <- [ 1 ]"
                            "let r2 = ref []"
                            $"let fill2 () = r2.Value <- [ %d{second} ]"
                            "let r3 = ref []"
                            "let fill3 () = r3.Value <- [ 3 ]" ]

                  let! r = edit s d (source 1)
                  do! expectOracle s r.Diagnostics "the source"
                  Expect.equal (s.Graph.SegmentRows d) 6 "two rows each"
                  let! r = edit s d (source 2)
                  Expect.equal (r.NamesIn d) [ "fill2"; "r2" ] "only the second"
                  do! expectOracle s r.Diagnostics "the second edited"
              })

          inBothModes "a row after the window sees a different solution" (fun s ->
              task {
                  let source (value: string) =
                      lines [ "let r = ref []"; $"let fill () = r.Value <- [ %s{value} ]"; "let n = r.Value.Head + 1" ]

                  let! r = edit s d (source "1")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "\"s\"")
                  Expect.contains (r.NamesIn d) "n" "n re-checks"
                  do! expectOracle s r.Diagnostics "solved to string"
              })

          inBothModes "a user through a window row sees it" (fun s ->
              task {
                  let source (value: string) =
                      lines
                          [ "let r = ref []"
                            "let g () = r.Value"
                            $"let fill () = r.Value <- [ %s{value} ]"
                            "let h = g ()"
                            "let j = h.Head + 1" ]

                  let! r = edit s d (source "1")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "\"s\"")
                  Expect.isTrue (Set.isSubset (set [ "h"; "j" ]) (set (r.NamesIn d))) "h and j re-check, by g's close print"
                  do! expectOracle s r.Diagnostics "solved to string"
              })

          inBothModes "the solver moves earlier" (fun s ->
              task {
                  let source (m: string) =
                      lines [ "let r = ref []"; m; "let n = r.Value.Length"; "let solve () = r.Value <- [ 1 ]"; "let z = 1" ]

                  let! r = edit s d (source "let m = 1")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "let m = (r.Value <- [ 1 ])")
                  do! expectOracle s r.Diagnostics "m solves it"
                  let! r = edit s d (source "let m = 1")
                  do! expectOracle s r.Diagnostics "solve solves it again"
              })

          inBothModes "the solver moves later" (fun s ->
              task {
                  let source (solve: string) (k: string) =
                      lines [ "let r = ref []"; "let m = r.Value.Length"; solve; k; "let tail = 1" ]

                  let! r = edit s d (source "let solve () = r.Value <- [ 1 ]" "let k = 1")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "let solve () = ()" "let k = (r.Value <- [ 1 ])")
                  do! expectOracle s r.Diagnostics "k solves it"
                  let! r = edit s d (source "let solve () = r.Value <- [ 1 ]" "let k = 1")
                  do! expectOracle s r.Diagnostics "solve solves it again"
              })

          inBothModes "the solver is deleted" (fun s ->
              task {
                  let! r = edit s d (lines [ "let r = ref []"; "let solve () = r.Value <- [ 1 ]"; "let z = 1" ])
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (lines [ "let r = ref []"; "let z = 1" ])
                  do! expectOracle s r.Diagnostics "solver gone"
                  Expect.isTrue (r.Diagnostics[d] |> Array.exists (fun x -> x.ErrorNumber = 30)) "FS0030 at r"
              })

          inBothModes "solved to another open variable stays open" (fun s ->
              task {
                  let source (z: int) (after: int) =
                      lines
                          [ "let r = ref []"
                            "let s = ref []"
                            "let link () = r.Value <- s.Value"
                            $"let z = %d{z}"
                            "let fin () = s.Value <- [ 1 ]"
                            $"let after = %d{after}" ]

                  let! r = edit s d (source 1 1)
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source 1 2)
                  Expect.equal (r.NamesIn d) [ "after" ] "after is past fin"
                  do! expectOracle s r.Diagnostics "after edited"
                  let! r = edit s d (source 2 2)
                  Expect.equal (r.NamesIn d) [ "fin"; "link"; "r"; "s"; "z" ] "z is in the window, which link's r keeps open"
                  do! expectOracle s r.Diagnostics "z edited"
              })

          inBothModes "a nested module's variable keeps the window open" (fun s ->
              task {
                  let source (after: int) =
                      lines [ "module M =\n    let r = ref []"; $"let after = %d{after}"; "let use1 () = M.r.Value <- [ 1 ]"; "let z = 2" ]

                  let! r = edit s d (source 1)
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source 2)
                  Expect.contains (r.NamesIn d) "use1" "the window runs to its solver"
                  Expect.isFalse (List.contains "z" (r.NamesIn d)) "z is past it"
                  do! expectOracle s r.Diagnostics "after edited"
              })

          inBothModes "a member's variable closes at its use" (fun s ->
              task {
                  let source (literal: string) =
                      lines [ "type T() =\n    member _.M() = 1L + 1"; $"let t: %s{literal} = T().M()"; "let z = 1" ]

                  let! r = edit s d (source "int64")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "int")
                  do! expectOracle s r.Diagnostics "the use changes"
                  let! r = edit s d (source "int64")
                  do! expectOracle s r.Diagnostics "and back"
              })

          inBothModes "a window's type's users re-check" (fun s ->
              task {
                  let source (value: string) =
                      lines
                          [ "let r = ref []"
                            "type S() =\n    member _.R = r"
                            $"let fill () = r.Value <- [ %s{value} ]"
                            "let u (s: S) = s.R"
                            "let z = 1" ]

                  let! r = edit s d (source "1")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "2")
                  Expect.contains (r.NamesIn d) "u" "u holds S's entity"
                  Expect.isFalse (List.contains "z" (r.NamesIn d)) "z does not"
                  do! expectOracle s r.Diagnostics "fill edited"
              })

          testTask "a broken close print re-forms the segment to the end of the file" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let source (after: int) = lines [ "let r = ref []"; "let fill () = r.Value <- [ 1 ]"; "let size () = 1"; $"let after = %d{after}" ]
              let mutable broken = true

              s.Graph.OnClosePrint(fun k print ->
                  if broken && k.Name = "r" then print + " broken" else print)

              let! (r: Edit) = edit s d (source 1)
              Expect.isGreaterThan s.Graph.PremiseBreaks 0L "the finish finds the print moved"
              Expect.equal (s.Graph.SegmentRows d) 4 "r leads to the end of the file"
              let! diff = oracleDiff s r.Diagnostics
              Expect.isEmpty diff "the diagnostics equal a full check"
              broken <- false
              let! (r: Edit) = edit s d (source 2)
              Expect.equal (r.NamesIn d) [ "after"; "fill"; "r"; "size" ] "the whole segment re-checks"
              let! diff = oracleDiff s r.Diagnostics
              Expect.isEmpty diff "the diagnostics equal a full check"
          }

          // Known gap: a re-check of a value keeps its users on the old value, which conformance does not reach, so a use
          // in a kept row shows no signature location. No diagnostic depends on it. Carried to the query tasks.
          ptestTask "a kept user of a re-checked value of a signed file shows its signature location" {
              let project = IO.Path.Combine(IO.Path.GetDirectoryName depsDir, "SigConform", "SigConform.fsproj")
              let! s = startOn project Declarations
              use _ = (s :> IAsyncDisposable)
              let a = s.Spec.SourceFiles |> List.find (fun f -> IO.Path.GetFileName f = "A.fs")
              let! _ = edit s a (s.Texts[a].Replace("let f (x: int) y", "let f (x:  int) y"))
              let! _ = edit s a (s.Texts[a].TrimEnd('\n') + "\n\nlet z = 1\n")
              do! expectLocations s a "after f's re-check"
          }

          inBothModes "a window that never closes is today's segment" (fun s ->
              task {
                  let source (last: int) = lines [ "let r = ref []"; "let a = 1"; $"let last = %d{last}" ]
                  let! r = edit s d (source 1)
                  do! expectOracle s r.Diagnostics "the source"
                  Expect.equal (s.Graph.SegmentRows d) 3 "everything from r"
                  let! r = edit s d (source 2)
                  Expect.equal (r.NamesIn d) [ "a"; "last"; "r" ] "everything from r"
                  do! expectOracle s r.Diagnostics "last edited"
              })

          abbreviationBattery "abbreviation: a value chain" (sprintf "type A = %s") [ "let x: A = Unchecked.defaultof<A>"; "let y = x"; "let z = y + y" ]
          abbreviationBattery "abbreviation: a function result" (sprintf "type A = %s") [ "let g () : A = Unchecked.defaultof<A>"; "g () |> ignore"; "let h = g () + g ()" ]
          abbreviationBattery "abbreviation: member lookup through it" (sprintf "type A = %s") [ "let x: A = Unchecked.defaultof<A>"; "let n = x.ToString().Length"; "let m = x.Length" ]
          abbreviationBattery "abbreviation of an abbreviation" (sprintf "type A = %s") [ "type B = A"; "let x: B = Unchecked.defaultof<B>"; "let y = x + x" ]
          abbreviationBattery "abbreviation: generic" (fun t -> $"type L<'x> = list<'x * %s{t}>") [ "let e: L<int> = []"; "let h = e.Head"; "let s = snd h + snd h" ]
          abbreviationBattery "abbreviation: an attribute through it" (fun t -> "type Attr = System." + (if t = "int" then "ObsoleteAttribute" elif t = "string" then "SerializableAttribute" else "FlagsAttribute")) [ "[<Attr>]\ntype U() = class end"; "let u = U()" ]
          abbreviationBattery "abbreviation: a union through it" (fun t -> $"type Un = | Case of %s{t}\n\ntype A = Un") [ "let a = A.Case Unchecked.defaultof<_>"; "let f (x: A) = match x with Case v -> v"; "let b = f a" ]
          abbreviationBattery "abbreviation: a measure" (fun t -> "[<Measure>] type m\n\n[<Measure>] type q = " + (if t = "int" then "m" elif t = "string" then "m m" else "m m m")) [ "let x = 1.0<q>"; "let y: float<m> = x" ]
          abbreviationBattery "abbreviation: a measure attribute by another name" (fun t -> "type MA = MeasureAttribute\n\n[<MA>] type m\n\n[<MA>] type q = " + (if t = "int" then "m" elif t = "string" then "m m" else "m m m")) [ "let x = 1.0<q>"; "let y: float<m> = x" ]
          abbreviationBattery "abbreviation shadowing an earlier type's name" (fun t -> $"type T() =\n    member _.A = 1\n\nlet early = T()\n\ntype T = %s{t}") [ "let t: T = Unchecked.defaultof<T>"; "let n = early.A"; "let z = t + t" ]
          abbreviationBattery "abbreviation of a project type" (fun t -> $"type R = {{ V: %s{t} }}\n\ntype A = R") [ "let mk (v: A) = v"; "let w = (mk { V = Unchecked.defaultof<_> }).V"; "let j = w" ]

          testTask "abbreviation named in a signature file" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let e = fileOf "E.fs"
              let sigFile = fileOf "E.fsi"
              let source (target: string) =
                  $"module Deps.E\n\ntype A = %s{target}\n\nlet user: A = Unchecked.defaultof<A>\n\nlet compute () = 1\n\nlet unrelated = 7\n"

              let! _ = edit s e (source "int")
              let! (r: Edit) = edit s sigFile "module Deps.E\n\ntype A = int\n\nval user: A\n\nval compute: unit -> int\n\nval unrelated: int\n"
              do! expectOracle s r.Diagnostics "the source"
              let! (r: Edit) = edit s e (source "string")
              do! expectOracle s r.Diagnostics "the implementation's target differs"
              let! (r: Edit) = edit s e (source "int")
              do! expectOracle s r.Diagnostics "and agrees again"
              let! (r: Edit) = edit s e (source "int ")
              do! expectOracle s r.Diagnostics "the same target re-checked"
          }

          inBothModes "a unit of measure re-checked gives its users one unit" (fun s ->
              task {
                  let source (gap: string) (y: string) =
                      lines [ $"[<Measure>] type m%s{gap}"; "let x = 1.0<m>"; y ]

                  let! r = edit s d (source "" "let y: float<m> = x")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "  " "let y: float<m> = x")
                  do! expectOracle s r.Diagnostics "the unit re-checked"
                  let! r = edit s d (source "  " "let y: float<m> = x + x")
                  do! expectOracle s r.Diagnostics "a row meets the unit again"
              })

          testTask "rows re-checking with kept rows between them stay linear" {
              let! s = startWith Declarations
              use _ = (s :> IAsyncDisposable)
              let n = 400

              let rows =
                  String.Join(
                      "\n\n",
                      [ for i in 1..n ->
                            if i % 2 = 0 then $"let v%d{i} (x: A) = x" else $"let v%d{i} (x: int) = (x + %d{i}) * List.length [ x; x ]" ]
                  )

              let source (target: string) = $"module Deps.D\n\ntype A = %s{target}\n\n%s{rows}\n"
              let! _ = edit s d (source "int")
              let! _ = edit s d (source "int64")
              let extends = s.Graph.EnvExtends
              let! (r: Edit) = edit s d (source "string")
              Expect.isGreaterThanOrEqual r.Delta (int64 (n / 2)) "the rows naming A re-check"
              Expect.isLessThan r.Delta (int64 n) "the others are kept"
              Expect.isLessThanOrEqual (s.Graph.EnvExtends - extends) (int64 (2 * n)) "environments are assembled in file order"
          }

          inBothModes "an abbreviation edit re-checks only its users" (fun s ->
              task {
                  let source (target: string) =
                      lines [ $"type A = %s{target}"; "let x: A = Unchecked.defaultof<A>"; "let y = x"; "let unrelated = 1" ]

                  let! r = edit s d (source "int")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "int64")
                  Expect.isFalse (List.contains "unrelated" (r.NamesIn d)) "a row that holds no A is kept"
                  Expect.contains (r.NamesIn d) "y" "y's type is A"
                  do! expectOracle s r.Diagnostics "the target changed"
              })

          inBothModes "an unchanged-print abbreviation re-check keeps later rows" (fun s ->
              task {
                  let source (gap: string) =
                      lines [ $"type A = int%s{gap}"; "let x: A = Unchecked.defaultof<A>"; "let y = x + 1" ]

                  let! r = edit s d (source "")
                  do! expectOracle s r.Diagnostics "the source"
                  let! r = edit s d (source "  ")
                  Expect.isFalse (List.contains "y" (r.NamesIn d)) "A's print is the same, and A's entity is looked through"
                  do! expectOracle s r.Diagnostics "same print"
                  let! r = edit s d (lines [ "type A = int  "; "let x: A = Unchecked.defaultof<A>"; "let y = x + 2" ])
                  do! expectOracle s r.Diagnostics "a row mixing the old A and the new"
              })

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
