module Rfs.Server.Tests.ReferencesGridTexts

open Rfs.Server.Tests.TokenGrid

let private rep (a: string) (b: string) (t: string) =
    if not (t.Contains a) then failwithf "no %s" a
    t.Replace(a, b)

let augA = "module Proj.A\n\ntype C = { V: int }\n\nlet filler = 1\n\ntype C with\n    member x.M = x.V\n\nlet u (c: C) = c.M\n"
let extB = "module Proj.B\n\nopen Proj.A\n\ntype C with\n    member x.E = x.V + 1\n\nlet w (c: C) = c.E\n"
let augSig = "module Proj.A\n\ntype C =\n    { V: int }\n    member M: int\n\nval filler: int\n\nval u: c: C -> int\n"

let scenarios : Scenario list =
    [ { Name = "augmentation in its file"
        Files = [ "A.fs", augA ]
        Steps =
          [ "blank line above the type", [ "A.fs", rep "type C = " "\n\ntype C = " augA ]
            "then a re-check of the augmentation", [ "A.fs", rep "x.V" "x.V + 1" (rep "type C = " "\n\ntype C = " augA) ]
            "then a blank line above the augmentation", [ "A.fs", rep "type C with" "\ntype C with" (rep "x.V" "x.V + 1" (rep "type C = " "\n\ntype C = " augA)) ]
            "re-check of the type only", [ "A.fs", rep "{ V: int }" "{ V: int (* c *) }" augA ] ] }
      { Name = "extension in a later file"
        Files = [ "A.fs", augA; "B.fs", extB ]
        Steps =
          [ "blank line in A", [ "A.fs", rep "type C = " "\n\ntype C = " augA ]
            "body edit in B's extension", [ "B.fs", rep "x.V + 1" "x.V + 2" extB ]
            "blank line above in B", [ "B.fs", rep "type C with" "\n\ntype C with" extB ] ] }
      { Name = "augmentation, signed"
        Files = [ "A.fsi", augSig; "A.fs", augA ]
        Steps =
          [ "blank line in the signature", [ "A.fsi", rep "type C =" "\n\ntype C =" augSig ]
            "re-check of the augmentation", [ "A.fs", rep "x.V" "x.V + 1" augA ]
            "blank line in the signature again", [ "A.fsi", rep "type C =" "\n\n\n\ntype C =" augSig ] ] }
      { Name = "interface properties"
        Files =
          [ "A.fs", "module Proj.A\n\ntype I =\n    abstract P: int\n    abstract Q: int with get, set\n    abstract W: int with set\n    abstract M: int -> int\n\ntype T() =\n    let mutable q = 0\n    interface I with\n        member _.P = 1\n        member _.Q with get () = q and set (v: int) = q <- v\n        member _.W with set (v: int) = q <- v\n        member _.M x = x\n\nlet i = T() :> I\nlet a = i.P\nlet b = i.Q\ni.Q <- 3\ni.W <- 4\nlet m = i.M 1\n\ntype B() =\n    abstract R: int\n    default _.R = 1\n\ntype D() =\n    inherit B()\n    override _.R = 2\n\nlet d = D().R\nlet e = B().R\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet c = i.P + i.Q\ni.Q <- 5\nlet o = { new I with\n            member _.P = 2\n            member _.Q with get () = 3 and set (_: int) = ()\n            member _.W with set (_: int) = ()\n            member _.M x = x }\nlet f = o.P\n" ]
        Steps = [] }
      { Name = "constructors"
        Files =
          [ "A.fs", "module Proj.A\n\ntype C(x: int) =\n    new() = C(0)\n    new(s: string) = C(s.Length)\n    member _.X = x\n\ntype S(n: int) =\n    member _.N = n\n\ntype One() =\n    member _.K = 1\n\n[<Struct>]\ntype V =\n    val I: int\n    new(i: int) = { I = i }\n\nlet a = C()\nlet b = C(1)\nlet c = C(\"s\")\nlet t: C = a\nlet n = new C(2)\nlet s = S(1)\nlet one = One()\nlet v = V()\nlet v2 = V(1)\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\ntype D() =\n    inherit C(1)\n\nlet d = C()\nlet s2 = S(3)\nlet one2 = One()\nlet o = { new C(4) with\n            member _.ToString() = \"\" }\n" ]
        Steps = [] }
      { Name = "named arguments, across files"
        Files =
          [ "A.fs", "module Proj.A\n\ntype T(p: int) =\n    member val Prop = p with get, set\n    static member M(name: int, ?opt: int) = name + defaultArg opt 0\n    member _.I(other: int) = other\n\nlet here = T.M(name = 1)\nlet there = T(p = 1).I(other = 2)\n\ntype U = A of field: int | B of int\n\nlet pa (A(field = f) | B f) = f\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet use1 = T.M(name = 1, opt = 2)\nlet use2 = T(p = 3, Prop = 4)\nlet use3 = A(field = 1)\n" ]
        Steps = [] }
      { Name = "generic parameters and others"
        Files =
          [ "A.fs", "module Proj.A\n\ntype G<'t>(v: 't) =\n    member _.V: 't = v\n    static member Make<'u>(u: 'u) = u\n\nlet inline add< ^a when ^a: (static member (+): ^a * ^a -> ^a)> (x: ^a) (y: ^a) = x + y\n\nexception Bad of string\n\ntype E = One = 1 | Two = 2\n\ntype Del = delegate of int -> int\n\n[<Measure>] type m\n\nlet k = 1<m>\nlet r = Bad \"x\"\nlet e = E.One\n\nmodule Inner =\n    let z = 1\n\nmodule Inner2 =\n    let z = 2\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet g = G<int>(1).V\nlet h = G<string>.Make<int>(1)\nlet i = add 1 2\nlet j = (try 1 with Bad s -> s.Length)\nlet l = E.Two\nlet q = 2<m>\nlet z1 = Inner.z\nlet z2 = Inner2.z\n" ]
        Steps = [] } ]


/// Lib's type, and App extending it: a blank line in Lib leaves App's copy of the type at an old line
let libExtended =
    let lib = "module Proj.Lib\n\ntype C = { V: int }\n\nlet mk = { V = 1 }\n"

    { Name = "App extends Lib's type"
      Files = [ "Lib.fs", lib ]
      Steps = [ "a blank line above the type", [ "Lib.fs", rep "type C = " "\n\ntype C = " lib ] ] }

let appExtending = "module Proj.App\n\nopen Proj.Lib\n\ntype C with\n    member x.E = x.V\n\nlet w (c: C) = c.E\n"


/// What reaches a symbol with another spelling or no text, each asked at every token against FCS. The names of the second
/// file are never written in it: the filter would lose them if it applied (see `spelledName`).
let noTextShapes : Scenario list =
    [ { Name = "shape: computation expressions"
        Files =
          [ "A.fs", "module Proj.A\n\ntype Builder() =\n    member _.Bind(x: int, f: int -> int) = f x\n    member _.Return(x: int) = x\n    member _.ReturnFrom(x: int) = x\n    member _.Yield(x: int) = x\n    member _.YieldFrom(x: int) = x\n    member _.Zero() = 0\n    member _.Delay(f: unit -> int) = f ()\n    member _.Run(x: int) = x\n    member _.Combine(a: int, b: int) = a + b\n    member _.For(xs: int list, f: int -> int) = List.sumBy f xs\n    member _.While(g: unit -> bool, f: unit -> int) = if g () then f () else 0\n    member _.TryWith(f: unit -> int, h: exn -> int) = try f () with e -> h e\n    member _.TryFinally(f: unit -> int, h: unit -> unit) = try f () finally h ()\n    member _.Using(d: System.IDisposable, f: System.IDisposable -> int) = f d\n    [<CustomOperation(\"twice\")>]\n    member _.Double(x: int) = x * 2\n\nlet make = Builder()\n\nlet res () = { new System.IDisposable with member _.Dispose() = () }\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet a = make { let! x = 1\n               return x }\nlet b = make { return! 2 }\nlet c = make { yield 3 }\nlet d = make { yield! 4 }\nlet e = make { for i in [ 1 ] do\n                   yield i }\nlet f = make { if true then return 1 }\nlet g = make { while false do return 1 }\nlet h = make { try return 1 with _ -> return 2 }\nlet i = make { try return 1 finally () }\nlet j = make { use q = res ()\n               return 1 }\nlet k = make { return 1\n               return 2 }\nlet l = make { yield 1\n               twice }\n" ]
        Steps = [] }
      { Name = "shape: use, for, indexers, slices, conversions, dynamic, operators"
        Files =
          [ "A.fs", "module Proj.A\n\ntype Res() =\n    interface System.IDisposable with\n        member _.Dispose() = ()\n\ntype Walker() =\n    let mutable n = 0\n    member _.MoveNext() = (n <- n + 1; n < 2)\n    member _.Current = n\n\ntype Walk() =\n    member _.GetEnumerator() = Walker()\n\ntype Box(v: int) =\n    member _.Item\n        with get (i: int) = v + i\n        and set (i: int) (x: int) = ()\n    member _.GetSlice(a: int option, b: int option) = v\n    member _.SetSlice(a: int option, b: int option, x: int) = ()\n    member _.Value = v\n    static member op_Implicit(b: Box) : int = b.Value\n    static member op_Explicit(b: Box) : float = 1.0\n    static member (+)(a: Box, b: Box) = a\n    static member (~-)(a: Box) = a\n\nlet (+.) a b = a + b + 1\nlet (?) (b: Box) (name: string) = name\nlet (?<-) (b: Box) (name: string) (v: int) = ()\nlet mk = Box 1\nlet res () = new Res()\nlet walk = Walk()\n\ntype O =\n    static member M(?x: int) = x\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet u () =\n    use r = res ()\n    1\nlet w = [ for x in walk -> x ]\nlet i1 = mk[0]\nmk[0] <- 1\nlet s1 = mk[1..2]\nmk[1..2] <- 3\nlet n: int = mk\nlet e = float mk\nlet p = mk + mk\nlet m = -mk\nlet d = mk?name\nmk?name <- 1\nlet q = 1 +. 2\nlet arr = [| 1; 2 |]\nlet sl = arr[0..1]\nlet o1 = O.M()\nlet o2 = O.M(1)\n" ]
        Steps = [] }
      { Name = "shape: attributes, constructors, setters, active patterns"
        Files =
          [ "A.fs", "module Proj.A\n\ntype FooAttribute() =\n    inherit System.Attribute()\n\ntype C(x: int) =\n    member _.X = x\n\ntype S() =\n    member val P = 1 with get, set\n\nlet (|Even|Odd|) n = if n % 2 = 0 then Even else Odd\nlet (|Pos|_|) n = if n > 0 then Some n else None\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\n[<Foo>]\nlet f = 1\nlet c = C 1\nlet s = S(P = 2)\ns.P <- 3\nlet t n = match n with Even -> 1 | Odd -> 2\nlet v n = match n with Pos x -> x | _ -> 0\n" ]
        Steps = [] }
      { Name = "shape: abbreviations, module aliases, backticks, open type"
        Files =
          [ "A.fs", "module Proj.A\n\nmodule Long =\n    module Name =\n        let z = 1\n        type Target() =\n            member _.K = 1\n        type Rec = { F: int }\n\ntype Alias = Long.Name.Target\ntype RecAlias = Long.Name.Rec\nmodule Short = Long.Name\n\nlet ``two words`` = 1\nlet plain = 2\n\ntype St =\n    static member Make() = 1\n    static member Val = 2\n\ntype Cases = CaseA | CaseB\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet b: Alias = Unchecked.defaultof<_>\nlet c = Short.z\nlet r: RecAlias = { F = 1 }\nlet t = ``two words``\nlet u = ``plain``\n"
            "C.fs", "module Proj.C\n\nopen type Proj.A.St\nopen type Proj.A.Cases\n\nlet a = Make()\nlet b = Val\nlet c = CaseA\n" ]
        Steps = [] }
      { Name = "shape: generated, extensions, slots"
        Files =
          [ "A.fs", "module Proj.A\n\ntype R = { X: int }\n\ntype U =\n    | One of int\n    | Two of y: int\n\nexception E of string\n\ntype T() =\n    member _.K = 1\n\ntype T with\n    member _.Ext = 1\n\ntype System.String with\n    member s.Shout = s\n\nmodule More =\n    type T with\n        member _.Opt = 2\n\ntype I =\n    abstract M: int -> int\n\ntype Base() =\n    abstract V: int\n    default _.V = 1\n\ntype D() =\n    inherit Base()\n    override _.V = 2\n\nlet impl = { new I with member _.M n = n }\nlet aR = { X = 1 }\nlet aU = One 1\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\nopen Proj.A.More\n\nlet a = aU.IsOne\nlet b = aU.Tag\nlet c = aR = aR\nlet d = compare aR aR\nlet e = aR.Equals aR\nlet f = T().Ext\nlet g = \"\".Shout\nlet h = T().Opt\nlet i = impl.M 1\nlet j = D().V\nlet k = (D() :> Base).V\nlet l = Two(y = 1)\n" ]
        Steps = [] } ]

/// Differences from FCS that 9a6a312 has too, found by these shapes and not fixed here: each with how many tokens differ.
/// - A constructor through an abbreviation: `Alias()` is a use of `Target` by key, where FCS says of `Alias`.
/// - A namespace named only in the file's module header: that file's use of it isn't found.
let knownDifferences : (Scenario * int) list =
    [ { Name = "known: a constructor through an abbreviation"
        Files =
          [ "A.fs", "module Proj.A\n\nmodule Long =\n    type Target() =\n        member _.K = 1\n\ntype Alias = Long.Target\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet a = Alias()\nlet b: Alias = a\n" ]
        Steps = [] },
      5
      { Name = "known: a namespace only in a module header"
        Files =
          [ "A.fs", "module Proj.A\n\nlet shared = 1\n"
            "B.fs", "module Proj.B\n\nopen Proj.A\n\nlet a = shared\n"
            "C.fs", "module Proj.C\n\nlet unrelated = 5\n" ]
        Steps = [] },
      4 ]

/// What the filter applies to, in files with and without the name, and a value a literal's suffix calls with no text
let writtenByName : Scenario list =
    let a = "module Proj.A\n\ntype R = { Alpha: int; Beta: int }\ntype U =\n    | Uno\n    | Dos of int\n\nlet shared = 1\nlet other = 2\nlet rcd = { Alpha = 1; Beta = 2 }\n\n[<Literal>]\nlet Lit = 3\n\nlet inline gen x = x\n\nmodule NumericLiteralQ =\n    let FromZero () = 0\n    let FromOne () = 1\n    let FromInt32 (n: int) = n\n    let FromInt64 (n: int64) = int n\n    let FromString (s: string) = s.Length\n"
    let b = "module Proj.B\n\nopen Proj.A\n\nlet a = shared + rcd.Alpha\nlet b = { rcd with Beta = 3 }\nlet c = Dos 1\nlet d = function Uno -> 1 | Dos n -> n\nlet e x = match x with Lit -> 1 | _ -> 0\nlet f = gen 1\nlet s = $\"{shared}\"\nlet n = nameof other\n"
    let c = "module Proj.C\n\nopen Proj.A\n\nlet unrelated = 5\n"
    let d = "module Proj.D\n\nopen Proj.A\n\nlet z = 0Q\nlet o = 1Q\nlet n = 5Q\nlet big = 9999999999Q\nlet huge = 99999999999999999999999Q\n"

    [ { Name = "shape: written by name"
        Files = [ "A.fs", a; "B.fs", b; "C.fs", c; "D.fs", d ]
        Steps =
          [ "a use in a file that had none", [ "C.fs", "module Proj.C\n\nopen Proj.A\n\nlet unrelated = shared\n" ]
            "and gone again", [ "C.fs", c ]
            "a blank line above the definitions", [ "A.fs", rep "let shared" "\n\nlet shared" a ] ] }
      { Name = "shape: written by name, signed"
        Files =
          [ "A.fsi", "module Proj.A\n\ntype R = { Alpha: int }\ntype U = Uno | Dos of int\nval shared: int\n"
            "A.fs", "module Proj.A\n\ntype R = { Alpha: int }\ntype U = Uno | Dos of int\nlet shared = 1\n"
            "B.fs", b |> rep "rcd.Alpha" "1" |> rep "let b = { rcd with Beta = 3 }\n" "" |> rep "let e x = match x with Lit -> 1 | _ -> 0\n" "" |> rep "let f = gen 1\n" "" |> rep "let n = nameof other\n" ""
            "C.fs", c ]
        Steps = [] } ]

