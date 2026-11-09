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
