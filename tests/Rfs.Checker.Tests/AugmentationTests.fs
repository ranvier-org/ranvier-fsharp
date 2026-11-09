/// A kept type with an augmentation: rows that see the augmentation only through the environment, or a type left holding
/// a deleted one. Each case states the diagnostic a stale row would change, as well as comparing with the oracle.
module Rfs.Checker.Tests.AugmentationTests

open System
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.Diagnostics
open Rfs.Checker
open Rfs.Checker.Tests.Deps

let private d = fileOf "D.fs"
let private modes = [ "by declaration from the start", Declarations; "promoted from file level", FileLevel ]

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

let private errors (r: Edit) =
    r.Diagnostics[d]
    |> Array.filter (fun e -> e.Severity = FSharpDiagnosticSeverity.Error)
    |> Array.map _.ErrorNumber
    |> Array.sort
    |> List.ofArray

/// Edits D.fs and expects the oracle's diagnostics, and exactly these errors.
let private step (s: Session) (what: string) (text: string) (codes: int list) : Task =
    task {
        let! r = edit s d text
        Expect.equal (errors r) codes $"%s{what}: the errors"
        do! expectOracle s r.Diagnostics what
    }

let private staticClass = "[<Extension; AbstractClass; Sealed>]\ntype T =\n    static member Seed = 1"
let private namesExtension = "open System.Runtime.CompilerServices"

/// What a row reads of an extension member it does not name the type of, and the error a fresh check gives it.
let private shapes =
    [ "an extension method", [], "let y = (1).F()", "[<Extension>] static member F(x: int) = x", 39
      "an extension GetEnumerator",
      [ "type U() =\n    member _.V = 1" ],
      "let y () = for x in U() do ignore x",
      "[<Extension>] static member GetEnumerator(u: U) = ([ 1 ] :> seq<int>).GetEnumerator()",
      693
      "a computation expression's extension Return",
      [ "type B() =\n    member _.Zero() = 0"; "let b = B()" ],
      "let y = b { return 1 }",
      "[<Extension>] static member Return(b: B, x: int) = x",
      708
      "an extension GetSlice",
      [ "type U() =\n    member _.V = 1"; "let u = U()" ],
      "let y = u.[1..2]",
      "[<Extension>] static member GetSlice(b: U, l: int option, h: int option) = 1",
      39 ]

/// Every way the environment before the row comes to be assembled again.
let private foldBattery (position: string) (source: int -> string -> string) (row: string) (code: int) (s: Session) : Task =
    task {
        let quiet = "let y = 1"
        let blank (text: string) = text.Replace("module Deps.D\n", "module Deps.D\n\n")
        do! step s "the source" (source 1 quiet) []
        do! step s $"%s{position}: the row alone" (source 1 row) [ code ]
        do! step s "back" (source 1 quiet) []
        do! step s $"%s{position}: two edits in one text" (source 2 row) [ code ]
        do! step s "back" (source 2 quiet) []
        do! step s "an edit before the type" (source 3 quiet) []
        do! step s $"%s{position}: then the row" (source 3 row) [ code ]
        do! step s "back" (source 3 quiet) []
        do! step s "a blank line at the top" (blank (source 3 quiet)) []
        do! step s $"%s{position}: then the row, moved" (blank (source 3 row)) [ code ]
    }

let private foldTests =
    [ for name, setup, row, aug, code in shapes do
          let augmentation = $"type T with\n    %s{aug}"
          let rows (a: int) = [ namesExtension; $"let a = %d{a}" ] @ setup @ [ staticClass ]

          inBothModes $"%s{name}, a row after the augmentation" (foldBattery "after" (fun a y -> lines (rows a @ [ augmentation; y ])) row code)

          inBothModes
              $"%s{name}, a row between the type and the augmentation"
              (foldBattery "between" (fun a y -> lines (rows a @ [ y; augmentation; "let tail = 1" ])) row code) ]

let private openType (a: int) (between: string) (afterwards: string) =
    lines [ $"let a = %d{a}"; "let S = \"s\""; "type T() =\n    member _.A = 1"; "open type T"; between; "type T with\n    static member S = 1"; afterwards ]

[<Tests>]
let tests =
    testSequenced
    <| testList
        "Augmentation"
        [ yield! foldTests

          inBothModes "a replayed open type does not see a later augmentation" (fun s ->
              task {
                  do! step s "the source" (openType 1 "let y = 1" "let z = 1") []
                  do! step s "S is the value, two edits at once" (openType 2 "let y = 1" "let z: string = S") []
                  do! step s "and is not an int" (openType 2 "let y = 1" "let z: int = S") [ 1 ]
                  do! step s "an edit before" (openType 3 "let y = 1" "let z: int = S") [ 1 ]
                  do! step s "between, it is the value too" (openType 4 "let y: int = S" "let z = 1") [ 1 ]
              })

          inBothModes "an open type after the augmentation still sees it when replayed" (fun s ->
              task {
                  let source (a: int) (z: string) =
                      lines [ $"let a = %d{a}"; "let S = \"s\""; "type T() =\n    member _.A = 1"; "type T with\n    static member S = 1"; "open type T"; z ]

                  do! step s "the source" (source 1 "let z = 1") []
                  do! step s "S is the member, two edits at once" (source 2 "let z: int = S") []
                  do! step s "and is not a string" (source 2 "let z: string = S") [ 193 ]
                  do! step s "an edit before" (source 3 "let z: string = S") [ 193 ]
              })

          inBothModes "a member of the augmentation stays visible after the environment is assembled again" (fun s ->
              task {
                  let source (a: int) (z: string) =
                      lines [ $"let a = %d{a}"; "type T() =\n    member _.A = 1"; "type T with\n    member _.B = 2"; z ]

                  do! step s "the source" (source 1 "let z = 1") []
                  do! step s "B, two edits at once" (source 2 "let z = T().B") []
                  do! step s "and C is not there" (source 3 "let z = T().C") [ 39 ]
              })

          inBothModes "a deleted augmentation leaves the kept type" (fun s ->
              task {
                  let t = "type T() =\n    member _.A = 1"
                  let aug = "type T with\n    member _.B = 2"
                  do! step s "the source" (lines [ t; aug; "let z = T().B" ]) []
                  do! step s "deleted" (lines [ t; "let z = T().B" ]) [ 39 ]
                  do! step s "added again" (lines [ t; aug; "let z = T().B" ]) []
                  do! step s "deleted again" (lines [ t; "let z = T().B" ]) [ 39 ]
              })

          inBothModes "a deleted augmentation, with a row not naming the type" (fun s ->
              task {
                  let source (rows: string list) = lines ([ "type T() =\n    member _.A = 1" ] @ rows @ [ "let v = T()"; "let z = v.B" ])
                  do! step s "the source" (source [ "type T with\n    member _.B = 2" ]) []
                  do! step s "deleted" (source []) [ 39 ]
              })

          inBothModes "one of two augmentations deleted, and a row between them" (fun s ->
              task {
                  let t = "type T() =\n    member _.A = 1"
                  let first = "type T with\n    member _.B = 2"
                  let second = "type T with\n    member _.C = 3"
                  do! step s "the source" (lines [ t; first; "let y = T().B"; second; "let z = T().C" ]) []
                  do! step s "the second deleted" (lines [ t; first; "let y = T().B"; "let z = T().C" ]) [ 39 ]
                  do! step s "the first deleted" (lines [ t; "let y = T().B"; "let z = T().C" ]) [ 39; 39 ]
              })

          inBothModes "a deleted augmentation with the type's module opened after it" (fun s ->
              task {
                  let source (aug: string) =
                      lines
                          [ namesExtension
                            "module X =\n    [<Extension; AbstractClass; Sealed>]\n    type T =\n        static member Seed = 1" + aug
                            "open X"
                            "let z = (1).F()" ]

                  let aug = "\n\n    type T with\n        [<Extension>] static member F(x: int) = x"
                  do! step s "the source" (source aug) []
                  do! step s "deleted" (source "") [ 39 ]
                  do! step s "added again" (source aug) []
              })

          inBothModes "an edited augmentation with a kept row reached by an open" (fun s ->
              task {
                  let source (m: string) =
                      lines [ namesExtension; staticClass; $"type T with\n    [<Extension>] static member %s{m}(x: int) = x"; "open type T"; "let z = (1).F()" ]

                  do! step s "the source" (source "F") []
                  do! step s "renamed" (source "G") [ 39 ]
                  do! step s "back" (source "F") []
              })

          inBothModes "a conversion in the augmentation reaches a row that names only the overloads" (fun s ->
              task {
                  let source (a: int) (aug: string list) (y: string) =
                      lines (
                          [ $"let a = %d{a}"; "type T() =\n    member _.A = 1"; "type K =\n    static member F(x: string) = 1\n    static member F(x: T) = 2" ]
                          @ aug
                          @ [ y ]
                      )

                  let aug = [ "type T with\n    static member op_Implicit(x: int) = T()" ]
                  let y = "let y = K.F(1)"
                  do! step s "with the conversion" (source 1 aug y) []
                  do! step s "deleted" (source 1 [] y) [ 41 ]
                  do! step s "added again" (source 1 aug y) []
                  do! step s "an edit before" (source 2 aug y) []
              })

          inBothModes "a type in a namespace, with a row in a module after it" (fun s ->
              task {
                  let source (a: int) (z: string) =
                      $"namespace Deps.N\n\n%s{namesExtension}\n\ntype A() =\n    member _.V = %d{a}\n\n%s{staticClass}\n\ntype T with\n    [<Extension>] static member F(x: int) = x\n\nmodule Rows =\n    let z = %s{z}\n"

                  do! step s "the source" (source 1 "1") []
                  do! step s "both edits" (source 2 "(1).F()") [ 39 ]
              })

          // The review's N1, which main passes because a row between re-checks with the type: a guard
          inBothModes "N1: both edits in one text, a row between" (fun s ->
              task {
                  let source (a: int) (y: string) =
                      lines [ namesExtension; $"let a = %d{a}"; staticClass; y; "type T with\n    [<Extension>] static member F(x: int) = x"; "let tail = 1" ]

                  do! step s "the source" (source 1 "let y = 1") []
                  do! step s "both" (source 2 "let y = (1).F()") [ 39 ]
              }) ]

let private fragment (name: string) (body: string) = $"namespace %s{name}\n\n%s{body}"
let private fragments (name: string) (bodies: string list) = String.Join("\n", bodies |> List.map (fragment name))

let private tType = "type T() =\n    member _.A = 1\n"
let private tAugmented (body: string) = $"type T with\n    member _.%s{body}\n"
let private reader = "module R =\n    let z = T().B\n"

/// A file of several fragments with an augmentation anywhere inside them stays at file level: the tie between a type
/// and its augmentation is read from one module's declarations, and a fragment is one row.
[<Tests>]
let fragmentTests =
    testSequenced
    <| testList
        "AugmentationFragments"
        [ for ns in [ "global"; "N" ] do
              // In a named namespace an augmentation in another fragment is FS0644 whatever the graph does, and more
              // follows from it: the oracle says what
              let step (s: Session) (what: string) (text: string) (codes: int list) : Task =
                  if ns = "global" then
                      step s what text codes
                  else
                      task {
                          let! r = edit s d text
                          do! expectOracle s r.Diagnostics what
                      }

              let source = fragments ns [ tType; tAugmented "B = 2"; reader ]

              inBothModes $"%s{ns}: the augmentation deleted" (fun s ->
                  task {
                      do! step s "the source" source []
                      do! step s "deleted" (fragments ns [ tType; "module Q =\n    let q = 1\n"; reader ]) [ 39 ]
                  })

              inBothModes $"%s{ns}: its member renamed" (fun s ->
                  task {
                      do! step s "the source" source []
                      do! step s "renamed" (fragments ns [ tType; tAugmented "C = 2"; reader ]) [ 39 ]
                  })

              inBothModes $"%s{ns}: its member's value edited" (fun s ->
                  task {
                      do! step s "the source" source []
                      do! step s "= 3" (fragments ns [ tType; tAugmented "B = 3"; reader ]) []
                      do! step s "= 4" (fragments ns [ tType; tAugmented "B = 4"; reader ]) []
                  })

              let earlier (v: int) = fragment "Z" $"type A() =\n    member _.V = %d{v}\n"
              let rows (row: string) = fragment ns $"module R =\n    %s{row}\n"

              inBothModes $"%s{ns}: a fragment with the type and its extension, replayed" (fun s ->
                  let held = fragment ns $"%s{namesExtension}\n\n%s{staticClass}\n\ntype T with\n    [<Extension>] static member F(x: int) = x\n"
                  let text (v: int) (row: string) = String.Join("\n", [ earlier v; held; rows row ])

                  task {
                      do! step s "the source" (text 1 "let z = 1") []
                      do! step s "both" (text 2 "let z = (1).F()") [ 39 ]
                      do! step s "the earlier fragment" (text 3 "let z = (1).F()") [ 39 ]
                  })

              inBothModes $"%s{ns}: a fragment opening the type before its augmentation, replayed" (fun s ->
                  let held = fragment ns "module H =\n    let S = \"s\"\n\nopen H\n\ntype T() =\n    member _.A = 1\n\nopen type T\n\ntype T with\n    static member S = 1\n"
                  let text (v: int) (row: string) = String.Join("\n", [ earlier v; held; rows row ])

                  task {
                      do! step s "the source" (text 1 "let z = 1") []
                      do! step s "both" (text 2 "let z: string = S") []
                      do! step s "the earlier fragment" (text 3 "let z: string = S") []
                  })

              inBothModes $"%s{ns}: an augmentation in a nested module keeps the file whole" (fun s ->
                  task {
                      do! step s "the source" (fragments ns [ tType; "module M =\n    type U() =\n        member _.A = 1\n\n    type U with\n        member _.B = 2\n"; "module R =\n    let z = M.U().B\n" ]) []
                      let! mode = s.Graph.Mode d
                      Expect.equal mode FileLevel "at file level"
                  })

              inBothModes $"%s{ns}: several fragments with no augmentation check by declaration" (fun s ->
                  let text (v: int) = fragments ns [ $"type T() =\n    member _.A = %d{v}\n"; "type V() =\n    member _.B = 2\n"; "module R =\n    let z = V().B + 1\n" ]

                  task {
                      do! step s "the source" (text 1) []
                      let! mode = s.Graph.Mode d
                      Expect.equal mode Declarations "by declaration"
                      let checks = s.Graph.FileLevelChecks
                      do! step s "a body" (text 2) []
                      Expect.equal (s.Graph.FileLevelChecks - checks) 0L "no file is checked whole"
                  }) ]

/// A file as the fuzz sees it: two types with augmentations whose members come and go, rows that read those members
/// without naming the type, before, between and after, and opens of the type. An empty member list is no augmentation.
type private Model =
    { Literal: int
      Blank: bool
      OpenBefore: bool
      OpenAfter: bool
      Between: string list
      After: string list
      Extensions: string list
      Instance: string list }

let private extensionMembers =
    [| "[<Extension>] static member F(x: int) = x"
       "[<Extension>] static member F(x: string) = x"
       "[<Extension>] static member H(x: int) = string x"
       "[<Extension>] static member GetEnumerator(u: U) = ([ 1 ] :> seq<int>).GetEnumerator()"
       "[<Extension>] static member Return(b: B, x: int) = x"
       "[<Extension>] static member GetSlice(b: U, l: int option, h: int option) = 1"
       "static member S = 1"
       "static member Q(x: int) = x" |]

let private instanceMembers =
    [| "member _.B = 2"
       "member _.B = \"b\""
       "static member op_Implicit(x: int) = P()"
       "static member op_Implicit(x: string) = P()"
       "static member (+) (a: P, b: int) = a" |]

/// None of these names T or P, though the last ones reach P through a value or an overload.
let private rowsReading =
    [| "let r{0} = (1).F()"
       "let r{0} = (\"s\").F()"
       "let r{0} = (1).H()"
       "let r{0} () = for x in U() do ignore x"
       "let r{0} = b { return 1 }"
       "let r{0} = u.[1..2]"
       "let r{0}: string = S"
       "let r{0}: int = S"
       "let r{0} = Q 1"
       "let r{0} = v.B"
       "let r{0} = v + 1"
       "let r{0} = K.F(1)"
       "let r{0} = K.F(\"s\")"
       "let r{0}: int = K.F(1)"
       "let r{0} = 1" |]

let private render (m: Model) =
    let augmentation (name: string) (members: string list) =
        match members with
        | [] -> []
        | _ -> [ $"type %s{name} with\n" + String.Join("\n", members |> List.map (fun x -> "    " + x)) ]

    let text =
        lines (
            [ namesExtension
              $"let a = %d{m.Literal}"
              "let S = \"s\""
              "type U() =\n    member _.V = 1"
              "let u = U()"
              "type B() =\n    member _.Zero() = 0"
              "let b = B()"
              staticClass
              "type P() =\n    member _.A = 1"
              "let v = P()"
              "type K =\n    static member F(x: int64) = 1\n    static member F(x: P) = 2" ]
            @ (if m.OpenBefore then [ "open type T" ] else [])
            @ m.Between
            @ augmentation "T" m.Extensions
            @ augmentation "P" m.Instance
            @ (if m.OpenAfter then [ "open type T" ] else [])
            @ m.After
        )

    if m.Blank then text.Replace("module Deps.D\n", "module Deps.D\n\n") else text

/// The same model as several fragments of one namespace: the types, the augmentations, then the rows.
let private renderFragments (name: string) (m: Model) =
    let augmentation (type': string) (members: string list) =
        match members with
        | [] -> ""
        | _ -> $"type %s{type'} with\n" + String.Join("\n", members |> List.map (fun x -> "    " + x)) + "\n"

    let moduleOf (moduleName: string) (rows: string list) =
        match rows with
        | [] -> ""
        | _ -> $"[<AutoOpen>]\nmodule %s{moduleName} =\n" + String.Join("\n", rows |> List.map (fun r -> "    " + r)) + "\n"

    let types =
        String.Join(
            "\n",
            [ namesExtension
              "type U() =\n    member _.V = 1"
              "type B() =\n    member _.Zero() = 0"
              staticClass
              "type P() =\n    member _.A = 1"
              "type K =\n    static member F(x: int64) = 1\n    static member F(x: P) = 2"
              moduleOf "Values" [ $"let a = %d{m.Literal}"; "let S = \"s\""; "let u = U()"; "let b = B()"; "let v = P()" ]
              (if m.OpenBefore then "open type T" else "")
              moduleOf "Between" m.Between ]
        )

    let augmentations = namesExtension + "\n\n" + augmentation "T" m.Extensions + "\n" + augmentation "P" m.Instance

    let rows =
        (if m.OpenAfter then "open type T\n\n" else "") + moduleOf "After" m.After

    let text = fragments name [ types; augmentations; rows ]
    if m.Blank then "\n" + text else text

let private toggle (item: string) (items: string list) =
    if List.contains item items then List.except [ item ] items else items @ [ item ]

let private change (random: Random) (counter: int ref) (m: Model) : Model * string =
    let pickOf (items: string[]) = items[random.Next items.Length]

    let row () =
        counter.Value <- counter.Value + 1
        (pickOf rowsReading).Replace("{0}", string counter.Value)

    let dropOrAdd (rows: string list) =
        if not rows.IsEmpty && (random.Next 2 = 0 || rows.Length >= 4) then
            List.removeAt (random.Next rows.Length) rows
        else
            rows @ [ row () ]

    match random.Next 14 with
    | 0
    | 1 -> { m with Literal = m.Literal + 1 }, "the literal before the types"
    | 2 -> { m with Blank = not m.Blank }, "a blank line at the top"
    | 3 -> { m with OpenBefore = not m.OpenBefore }, "open type T before its augmentation"
    | 4 -> { m with OpenAfter = not m.OpenAfter }, "open type T after its augmentation"
    | 5 -> { m with Between = dropOrAdd m.Between }, "a row between"
    | 6
    | 7 -> { m with After = dropOrAdd m.After }, "a row after"
    | 8
    | 9 ->
        let item = pickOf extensionMembers
        { m with Extensions = toggle item m.Extensions }, $"T's member `%s{item}`"
    | 10
    | 11 ->
        let item = pickOf instanceMembers
        { m with Instance = toggle item m.Instance }, $"P's member `%s{item}`"
    | 12 -> { m with Extensions = [] }, "T's augmentation deleted"
    | _ -> { m with Instance = [] }, "P's augmentation deleted"

let private runFuzzOf (rendered: Model -> string) (mode: FileMode) (seed: int) (steps: int) : Task =
    task {
        let! s = startWith mode
        use _ = (s :> IAsyncDisposable)
        s.Graph.Promote d
        let random = Random seed
        let counter = ref 0

        let mutable model =
            { Literal = 1
              Blank = false
              OpenBefore = false
              OpenAfter = false
              Between = []
              After = [ "let z = 1" ]
              Extensions = [ extensionMembers[0] ]
              Instance = [ instanceMembers[0] ] }

        let! _ = edit s d (rendered model)

        for i in 1..steps do
            let m1, what1 = change random counter model
            // Now and then two edits arrive in one text
            let m2, what2 = if random.Next 3 = 0 then change random counter m1 else m1, ""
            model <- m2
            let text = rendered model
            let! r = edit s d text
            let! diff = oracleDiff s r.Diagnostics

            if not diff.IsEmpty then
                failtest $"%A{mode} seed %d{seed}, step %d{i}: %s{what1}; %s{what2}\n%s{String.concat Environment.NewLine diff}\n%s{text}"

            let! fingerprints = fingerprintDiff s

            if not fingerprints.IsEmpty then
                failtest $"%A{mode} seed %d{seed}, step %d{i}: %s{what1}; %s{what2}\n%s{String.concat Environment.NewLine fingerprints}\n%s{text}"
    }

let private runFuzz = runFuzzOf render

[<Tests>]
let fragmentFuzzTests =
    testSequenced
    <| testList
        "OracleFuzzAugmentationFragments"
        [ for mode in [ Declarations; FileLevel ] do
              for name in [ "global"; "N" ] do
                  for seed in [ 4242; 7919 ] ->
                      testTask $"%A{mode} %s{name} seed %d{seed}: 80 edits of a file of fragments equal the oracle after each" {
                          do! runFuzzOf (renderFragments name) mode seed 80
                      } ]

[<Tests>]
let fuzzTests =
    testSequenced
    <| testList
        "OracleFuzzAugmentation"
        [ for mode in [ Declarations; FileLevel ] do
              // 4242 reaches a deleted augmentation early; 7919 and 33 reach the repeated fold, at about edits 144 and 86
              for seed in [ 4242; 7919; 33 ] ->
                  testTask $"%A{mode} seed %d{seed}: 200 edits of augmentations equal the oracle after each" { do! runFuzz mode seed 200 } ]
