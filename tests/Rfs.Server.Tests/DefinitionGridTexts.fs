/// The grid's projects: every kind of symbol, signed and unsigned, a later file, and overloads and shapes that can tie.
module Rfs.Server.Tests.DefinitionGridTexts

open Rfs.Server.Tests.TokenGrid

let aHead = "module Proj.A\n\nopen System\n\n"

let aBody =
    String.concat "\n" [
      "let value = 1"
      ""
      "let func (x: int) (y: int) = x + y"
      ""
      "let generic (x: 'a) = x"
      ""
      "let (+++) a b = a + b"
      ""
      "let (|Even|Odd|) n = if n % 2 = 0 then Even else Odd"
      ""
      "let (|Small|_|) n = if n < 1 then Some n else None"
      ""
      "let private hidden = 2"
      ""
      "[<Literal>]"
      "let Lit = 3"
      ""
      "type Abbr = int"
      ""
      "type Rec = { Field: int; mutable Other: string }"
      ""
      "type Un ="
      "    | CaseA of named: int"
      "    | CaseB"
      "    | CaseC of int * string"
      ""
      "let CaseB = 42"
      ""
      "type Same = { S: int }"
      ""
      "module Same ="
      "    let make () = { S = 1 }"
      ""
      "type Gen() ="
      "    member _.A = 1"
      ""
      "type Gen<'a>() ="
      "    member _.A = 2"
      ""
      "type Cls(x: int) ="
      "    let field = x"
      "    new() = Cls(0)"
      "    member _.Meth(a: int) = a + field"
      "    member _.Meth(a: string) = a"
      "    member _.Meth(a: 't list) = a"
      "    member _.Ga<'t>(a: int) = a"
      "    member _.Ga<'t, 'u>(a: int) = a"
      "    member _.Prop"
      "        with get () = x"
      "        and set (v: int) = ()"
      "    member val Auto = 1 with get, set"
      "    static member Stat = 1"
      "    static member Stat2(a: int) = a"
      "    member _.Stat2(a: string) = a"
      "    member _.Named(first: int, second: string) = first"
      "    member _.Item"
      "        with get (i: int) = i"
      "        and set (i: int) (v: int) = ()"
      "    member _.ROnly = 1"
      "    member _.WOnly"
      "        with set (v: int) = ()"
      "    static member (+)(a: Cls, b: Cls) = a"
      ""
      "type Cls with"
      "    member c.Intrinsic = 1"
      ""
      "type IFace ="
      "    abstract Do: unit -> int"
      "    abstract P: int with get, set"
      ""
      "type IOne ="
      "    abstract Same: int"
      ""
      "type ITwo ="
      "    abstract Same: int"
      ""
      "type IGen<'t> ="
      "    abstract Get: 't -> int"
      ""
      "type Both() ="
      "    member _.Same = 0"
      ""
      "    interface IOne with"
      "        member _.Same = 1"
      ""
      "    interface ITwo with"
      "        member _.Same = 2"
      ""
      "    interface IGen<int> with"
      "        member _.Get x = x"
      ""
      "    interface IGen<string> with"
      "        member _.Get x = 0"
      ""
      "    interface IFace with"
      "        member _.Do() = 1"
      ""
      "        member _.P"
      "            with get () = 1"
      "            and set (v: int) = ()"
      ""
      "type Base() ="
      "    abstract Over: int -> int"
      "    default _.Over x = x"
      "    override _.ToString() = \"b\""
      ""
      "type Derived() ="
      "    inherit Base()"
      "    override _.Over x = x + 1"
      "    override _.ToString() = \"d\""
      ""
      "[<Struct>]"
      "type St ="
      "    val V: int"
      "    new(v: int) = { V = v }"
      ""
      "type En ="
      "    | One = 1"
      "    | Two = 2"
      ""
      "type Del = delegate of int -> int"
      ""
      "type Ev() ="
      "    let e = Event<int>()"
      "    [<CLIEvent>]"
      "    member _.Fired = e.Publish"
      "    member _.Raise() = e.Trigger 1"
      ""
      "exception Boom of int"
      ""
      "exception Bang of msg: string"
      ""
      "[<Measure>]"
      "type m"
      ""
      "module Nest ="
      "    let inner = 1"
      ""
      "    type InNest = | I"
      ""
      "    module Deep ="
      "        let deep = 2"
      ""
      "module Exts ="
      "    type Cls with"
      "        member c.ExtM(a: int) = a"
      ""
      "    type String with"
      "        member s.Ext = s.Length"
      ""
      "module Priv2 ="
      "    type private T = { P: int }"
      "    let private p = { P = 1 }"
      "    let q = p.P"
      ""
      "type MyAttribute() ="
      "    inherit Attribute()"
      ""
      "[<My>]"
      "let withAttr = 1"
      ""
      "type B() ="
      "    member _.Return x = x"
      "    member _.Bind(x, f) = f x"
      ""
      "let b = B()"
      ""
      "let ce = b { let! a = 1 in return a }"
      ""
      "let viaLocal ="
      "    let inner = value"
      "    let lam = fun z -> z + inner + func 1 2"
      "    lam (generic 3)"
      ""
      "let objectOne = { new IOne with member _.Same = 1 }"
      ""
      "let objectTwo = { new IOne with member _.Same = 2 }"
      ""
      "let useInSameFile (c: Cls) (u: Un) (r: Rec) ="
      "    c.Meth 1 |> ignore"
      "    c.Prop <- c.Prop + c.Auto"
      "    c.Item(1) <- c[2]"
      "    (match u with CaseA(named = n) -> n | Un.CaseB -> 0 | CaseC _ -> 1) + r.Field"
      ""
    ]

let aSig =
    String.concat "\n" [
      "module Proj.A"
      ""
      "open System"
      ""
      "val value: int"
      ""
      "val func: x: int -> y: int -> int"
      ""
      "val generic: x: 'a -> 'a"
      ""
      "val (+++): a: int -> b: int -> int"
      ""
      "val (|Even|Odd|): n: int -> Choice<unit, unit>"
      ""
      "val (|Small|_|): n: int -> int option"
      ""
      "[<Literal>]"
      "val Lit: int = 3"
      ""
      "type Abbr = int"
      ""
      "type Rec = { Field: int; mutable Other: string }"
      ""
      "type Un ="
      "    | CaseA of named: int"
      "    | CaseB"
      "    | CaseC of int * string"
      ""
      "val CaseB: int"
      ""
      "type Same = { S: int }"
      ""
      "module Same ="
      "    val make: unit -> Same"
      ""
      "type Gen ="
      "    new: unit -> Gen"
      "    member A: int"
      ""
      "type Gen<'a> ="
      "    new: unit -> Gen<'a>"
      "    member A: int"
      ""
      "type Cls ="
      "    new: unit -> Cls"
      "    new: x: int -> Cls"
      "    member Meth: a: 't list -> 't list"
      "    member Meth: a: string -> string"
      "    member Meth: a: int -> int"
      "    member Ga<'t> : a: int -> int"
      "    member Ga<'t, 'u> : a: int -> int"
      "    member Prop: int with get, set"
      "    member Auto: int with get, set"
      "    static member Stat: int"
      "    static member Stat2: a: int -> int"
      "    member Stat2: a: string -> string"
      "    member Named: first: int * second: string -> int"
      "    member Item: i: int -> int with get, set"
      "    member ROnly: int"
      "    member WOnly: int with set"
      "    static member (+): a: Cls * b: Cls -> Cls"
      "    member Intrinsic: int"
      ""
      "type IFace ="
      "    abstract Do: unit -> int"
      "    abstract P: int with get, set"
      ""
      "type IOne ="
      "    abstract Same: int"
      ""
      "type ITwo ="
      "    abstract Same: int"
      ""
      "type IGen<'t> ="
      "    abstract Get: 't -> int"
      ""
      "type Both ="
      "    new: unit -> Both"
      "    member Same: int"
      "    interface IOne"
      "    interface ITwo"
      "    interface IGen<int>"
      "    interface IGen<string>"
      "    interface IFace"
      ""
      "type Base ="
      "    new: unit -> Base"
      "    abstract Over: int -> int"
      "    default Over: x: int -> int"
      "    override ToString: unit -> string"
      ""
      "type Derived ="
      "    inherit Base"
      "    new: unit -> Derived"
      "    override Over: x: int -> int"
      "    override ToString: unit -> string"
      ""
      "[<Struct>]"
      "type St ="
      "    val V: int"
      "    new: v: int -> St"
      ""
      "type En ="
      "    | One = 1"
      "    | Two = 2"
      ""
      "type Del = delegate of int -> int"
      ""
      "type Ev ="
      "    new: unit -> Ev"
      "    [<CLIEvent>]"
      "    member Fired: IEvent<int>"
      "    member Raise: unit -> unit"
      ""
      "exception Boom of int"
      ""
      "exception Bang of msg: string"
      ""
      "[<Measure>]"
      "type m"
      ""
      "module Nest ="
      "    val inner: int"
      ""
      "    type InNest = | I"
      ""
      "    module Deep ="
      "        val deep: int"
      ""
      "module Exts ="
      "    type Cls with"
      "        member ExtM: a: int -> int"
      ""
      "    type String with"
      "        member Ext: int"
      ""
      "module Priv2 ="
      "    val q: int"
      ""
      "type MyAttribute ="
      "    inherit Attribute"
      "    new: unit -> MyAttribute"
      ""
      "[<My>]"
      "val withAttr: int"
      ""
      "type B ="
      "    new: unit -> B"
      "    member Return: x: 'a -> 'a"
      "    member Bind: x: 'a * f: ('a -> 'b) -> 'b"
      ""
      "val b: B"
      ""
      "val ce: int"
      ""
      "val viaLocal: int"
      ""
      "val objectOne: IOne"
      ""
      "val objectTwo: IOne"
      ""
      "val useInSameFile: c: Cls -> u: Un -> r: Rec -> int"
      ""
    ]

let uses =
    String.concat "\n" [
      "module Proj.U"
      ""
      "open Proj.A"
      "open Proj.A.Exts"
      ""
      "let u1 = { Field = 1; Other = \"\" }"
      "let u2 = u1.Field"
      "let u2b = { u1 with Other = \"x\" }"
      "u1.Other <- \"y\""
      "let u3 = CaseA 1"
      "let u3b = Un.CaseB"
      "let u3c = CaseB"
      "let u3d = CaseC(1, \"\")"
      "let u4 = match u3 with CaseA(named = n) -> n | Un.CaseB -> 0 | CaseC(_, _) -> 1"
      "let u5 = Cls()"
      "let u6 = Cls(1)"
      "let u6b = Cls(x = 1)"
      "let u7 = u5.Meth 1"
      "let u8 = u5.Meth \"s\""
      "let u8b = u5.Meth [ 1 ]"
      "let u8c = u5.Ga<int> 1"
      "let u8d = u5.Ga<int, string> 1"
      "let u9 = u5.Prop"
      "u5.Prop <- 2"
      "let u11 = u5.Auto"
      "u5.Auto <- 3"
      "let u12 = Cls.Stat"
      "let u12b = Cls.Stat2 1"
      "let u12c = u5.Stat2 \"s\""
      "let u12d = u5.Named(first = 1, second = \"\")"
      "let u12e = u5[1]"
      "u5[1] <- 2"
      "let u12f = u5.ROnly"
      "u5.WOnly <- 1"
      "let u12g = u5 + u5"
      "let u12h = u5.Intrinsic"
      "let u12i = u5.ExtM 1"
      "let u12j = \"s\".Ext"
      "let u13 = Boom 1"
      "let u13b = Bang \"s\""
      "let u13c = match u13b with Bang(msg = t) -> t | _ -> \"\""
      "let u14: Abbr = 1"
      "let u15 (i: IFace) = i.Do() + i.P"
      "let u15b (i: IOne) (j: ITwo) (g: IGen<int>) = i.Same + j.Same + g.Get 1"
      "let u15c = Both().Same"
      "let u15d = (Both() :> IOne).Same"
      "let u16 = Nest.inner"
      "let u16b = Nest.Deep.deep"
      "let u17 = Nest.I"
      "let u17b: Nest.InNest = Nest.I"
      "let u18 = match 1 with Even -> 1 | Odd -> 2"
      "let u19 = match 1 with Small x -> x | _ -> 2"
      "let u20 = 1 +++ 2"
      "let u21 = func 1 2 + value + Lit"
      "let u22 = generic \"s\""
      "let u23 = (Derived()).Over 1"
      "let u24 = (Base()).Over 1"
      "let u24b = (Derived()).ToString()"
      "let u25 = St(1).V"
      "let u25b = St()"
      "let u26 = En.One"
      "let u27 = Del(fun x -> x)"
      "let u28 = Ev().Fired"
      "let u29 = 1.0<m>"
      "let u30: Same = Same.make ()"
      "let u31 = Gen().A"
      "let u32 = Gen<int>().A"
      "let u33 = Priv2.q"
      "[<My>]"
      "let u34 = withAttr"
      "let u35 = b.Return 1"
      "let u36 = ce"
      "let u37 = { new IFace with"
      "              member _.Do() = 1"
      "              member _.P with get () = 1 and set (v: int) = () }"
      "let u38 = viaLocal + objectOne.Same + objectTwo.Same"
      "let u39 (c: Cls) (u: Un) (r: Rec) = useInSameFile c u r"
      "let u40 = Proj.A.value"
      "let u41 (x: Rec) (y: Un) (z: Cls) (w: Same) = x, y, z, w"
      "let u42 = u1.Equals u1"
      "let u43 = u3.IsCaseA"
      "let u44 = typeof<Rec>"
      ""
    ]

let lastFile = "module Proj.Z\n\nlet z1 = Proj.A.value\n\nlet z2 = Proj.U.u1\n\nlet z3 = Proj.A.Cls().Meth 1\n\nlet z4 = z2.Field\n"

let rep (a: string) (b: string) (t: string) =
    if not (t.Contains a) then failwith $"no %s{a}"
    let i = t.IndexOf a
    t.Substring(0, i) + b + t.Substring(i + a.Length)

let aText = aHead + aBody
let aSteps (a0: string) =
    let s1 = rep "let value = 1" "\nlet value = 1" a0
    let s2 = rep "let func (x: int)" "let func (x:  int)" s1
    let s3 = rep "type Abbr = int" "type Abbr =  int" s2
    let s4 = rep "let  value" "let  value" (rep "let value = 1" "let  value = 1" s3)
    let s5 = rep "type Abbr =  int" "type  Abbr = int" s4
    let s6 = rep "let  value = 1" "let  value = 2" s5
    let s7 = rep "type  Abbr = int" "type Between = int\n\ntype  Abbr = int" s6
    let s8 = rep "    member _.Meth(a: int)" "    member _.Meth(a: float) = a\n    member _.Meth(a: int)" s7
    let s9 = rep "    member _.Meth(a: float) = a\n" "" s8
    let s10 = rep "    module Deep =" "\n    module Deep =" s9
    let s11 = rep "    let inner = 1" "\n    let inner = 1" s10
    let s12 = rep "type Base() =" "// a comment\n\ntype Base() =" s11
    [ "blank line at top of A", s1
      "func re-checked, same print", s2
      "abbreviation re-checked, same print", s3
      "value's name moved", s4
      "abbreviation's name moved", s5
      "value's body", s6
      "a type between", s7
      "an overload before", s8
      "that overload gone", s9
      "blank line above a nested module", s10
      "blank line inside a nested module", s11
      "comment above a type", s12 ]

let unsigned =
    { Name = "unsigned battery"
      Files = [ "A.fs", aText; "U.fs", uses; "Z.fs", lastFile ]
      Steps = aSteps aText |> List.map (fun (n, t) -> n, [ "A.fs", t ])
      }

let signed =
    { Name = "signed battery"
      Files = [ "A.fsi", aSig; "A.fs", aText; "U.fs", uses; "Z.fs", lastFile ]
      Steps =
        (aSteps aText |> List.map (fun (n, t) -> n, [ "A.fs", t ]))
        @ [ "blank line at top of the fsi", [ "A.fsi", rep "val value" "\nval value" aSig ]
            "blank line mid fsi", [ "A.fsi", rep "type Rec" "\n\ntype Rec" (rep "val value" "\nval value" aSig) ]
            "blank line above a nested module of the fsi", [ "A.fsi", rep "module Nest" "\n\nmodule Nest" (rep "type Rec" "\n\ntype Rec" (rep "val value" "\nval value" aSig)) ]
            "an overload reordered in the fsi",
                [ "A.fsi", (rep "module Nest" "\n\nmodule Nest" (rep "type Rec" "\n\ntype Rec" (rep "val value" "\nval value" aSig))).Replace("    member Meth: a: string -> string\n    member Meth: a: int -> int\n", "    member Meth: a: int -> int\n    member Meth: a: string -> string\n") ] ]
      }

let small name (a: string) (u: string) (steps: (string * string) list) =
    { Name = name
      Files = [ "A.fs", a; "U.fs", u ]
      Steps = steps |> List.map (fun (n, t) -> n, [ "A.fs", t ])
      }

let smallSigned name (si: string) (a: string) (u: string) (steps: (string * (string * string) list) list) =
    { Name = name
      Files = [ "A.fsi", si; "A.fs", a; "U.fs", u ]
      Steps = steps
      }

let blankAbove (needle: string) (t: string) = rep needle ("\n" + needle) t

let collisions =
  [ // Two overloads with one key: class type parameters
    (let a = "module Proj.A\n\ntype T<'a, 'b>() =\n    member _.M(x: 'a) = 1\n    member _.M(x: 'b) = 2\n"
     small "overloads by class type parameter" a "module Proj.U\nlet t = Proj.A.T<int, string>()\nlet one = t.M 1\nlet two = t.M \"s\"\n"
        [ "blank above the type", blankAbove "type T" a ])
    // Only the return type differs
    (let a = "module Proj.A\n\ntype T() =\n    member _.M(x: int) : int = 1\n    member _.M(x: int) : string = \"\"\n"
     small "overloads by return type" a "module Proj.U\nlet one: int = Proj.A.T().M 1\nlet two: string = Proj.A.T().M 1\n"
        [ "blank above the type", blankAbove "type T" a ])
    // An anonymous record prints as _
    (let a = "module Proj.A\n\ntype T() =\n    member _.M(x: {| A: int |}) = 1\n    member _.M(x: {| B: int |}) = 2\n"
     small "overloads by anonymous record" a "module Proj.U\nlet one = Proj.A.T().M {| A = 1 |}\nlet two = Proj.A.T().M {| B = 1 |}\n"
        [ "blank above the type", blankAbove "type T" a ])
    (let a = "module Proj.A\n\ntype T() =\n    member _.M<'t when 't: struct>(x: 't) = 1\n    member _.M<'t when 't: not struct>(x: 't) = 2\n"
     small "overloads by constraint" a "module Proj.U\nlet one = Proj.A.T().M 1\nlet two = Proj.A.T().M \"s\"\n"
        [ "blank above the type", blankAbove "type T" a ])
    (let a = "module Proj.A\n\ntype T() =\n    member _.M(x: float<'u>) = 1\n    member _.M(x: float) = 2\n    member _.N(x: int[]) = 1\n    member _.N(x: int[,]) = 2\n    member _.O(x: byref<int>) = 1\n    member _.O(x: int) = 2\n    member _.P(x: (int * int)) = 1\n    member _.P(x: int, y: int) = 2\n    member _.Q(x: int -> int) = 1\n    member _.Q(x: int -> string) = 2\n    member _.R(x: struct (int * int)) = 1\n    member _.R(x: int * int) = 2\n    member _.S([<System.ParamArray>] x: int[]) = 1\n    member _.S(x: int) = 2\n    member _.V(?x: int) = 1\n    member _.V(x: string) = 2\n"
     small "overloads by kinds of type" a "module Proj.U\nlet t = Proj.A.T()\nlet a = t.N [| 1 |]\nlet b = t.N (Array2D.zeroCreate 1 1)\nlet c = t.O 1\nlet d = t.P(1, 2)\nlet e = t.Q(fun x -> x)\nlet f = t.R(struct (1, 2))\nlet g = t.S(1)\nlet h = t.V(\"s\")\n"
        [ "blank above the type", blankAbove "type T" a ])
    // Extension against intrinsic, and twice in a module
    (let a = "module Proj.A\n\ntype T() =\n    member _.M(x: int) = 1\n\nmodule E1 =\n    type T with\n        member _.M(x: int) = 2\n        member _.N = 1\n\nmodule E2 =\n    type T with\n        member _.M(x: int) = 3\n        member _.N = 2\n"
     small "extensions against an intrinsic" a "module Proj.U\nopen Proj.A\nlet one = T().M 1\nopen E1\nlet two = T().M 1\nlet n1 = T().N\nopen E2\nlet three = T().M 1\nlet n2 = T().N\n"
        [ "blank above the type", blankAbove "type T" a; "blank above E2", blankAbove "module E2" (blankAbove "type T" a) ])
    // Two extensions in a module
    (let a = "module Proj.A\n\ntype T() =\n    member _.Base = 1\n\ntype T with\n    member _.Twice(x: int) = 2\n\nmodule E =\n    type T with\n        member _.Twice(x: string) = 3\n"
     small "extension augmentations" a "module Proj.U\nopen Proj.A\nlet one = T().Twice 1\nopen E\nlet two = T().Twice \"s\"\n" [ "blank", blankAbove "type T" a ])
    // A duplicate value, which does not compile: the file with errors
    (let a = "module Proj.A\n\nlet x = 1\nlet x = 2\nlet y = x\n"
     small "a duplicate value" a "module Proj.U\nlet z = Proj.A.x\n" [ "blank above", blankAbove "let x = 1" a ])
    // Shadowing inside: local, nested let, in a module
    (let a = "module Proj.A\n\nlet f () =\n    let x = 1\n    let x = x + 1\n    x\n\nmodule M =\n    let x = 1\n\nmodule N =\n    let x = M.x\n\nlet x = N.x\n"
     small "shadowing" a "module Proj.U\nopen Proj.A\nlet a = x\nlet b = M.x\nlet c = N.x\nopen N\nlet d = x\n" [ "blank above", blankAbove "let f ()" a ])
    (let a = "module Proj.A\n\ntype X = { F: int }\n\nmodule X =\n    let make () = { F = 1 }\n    let F = 1\n\ntype Y = | Z of int\n\nlet Z (x: int) = x\n\nmodule W =\n    type private P = { A: int }\n    let useP = { A = 1 }.A\n\nmodule V =\n    type private P = { A: int }\n    let useP = { A = 1 }.A\n"
     small "a type and a module, a case and a function, nested private types" a "module Proj.U\nopen Proj.A\nlet a: X = X.make ()\nlet b = X.F\nlet c = Z 1\nlet d = Y.Z 1\nlet e = W.useP + V.useP\n" [ "blank above", blankAbove "type X" a ])
    // Duplicate type name across two namespace fragments, private
    { Name = "private types of one name across files"
      Files = [ "A.fs", "namespace Proj\n\ntype private P = { A: int }\n\nmodule MA =\n    let a = { A = 1 }.A\n"
                "B.fs", "namespace Proj\n\ntype private P = { B: int }\n\nmodule MB =\n    let b = { B = 1 }.B\n"
                "U.fs", "module Proj.U\nlet z = Proj.MA.a + Proj.MB.b\n" ]
      Steps = [ "blank", [ "A.fs", "namespace Proj\n\n\ntype private P = { A: int }\n\nmodule MA =\n    let a = { A = 1 }.A\n" ] ]
      }
    // Signed: what a signature file spells differently from its implementation
    (let si = "module Proj.A\n\ntype T =\n    new: unit -> T\n    member M: x: int -> int\n    override ToString: unit -> string\n\nval user: T\n"
     let a = "module Proj.A\n\ntype T() =\n    member _.M(x) = x\n    override _.ToString() = \"\"\n\nlet user = T()\nlet z = user.M 1\nlet y = user.ToString()\n"
     smallSigned "a member more generic than its signature, an override" (si) a "module Proj.U\nlet b = Proj.A.user.M 1\nlet c = Proj.A.user.ToString()\n"
        [ "blank in impl", [ "A.fs", blankAbove "type T" a ]; "blank in sig", [ "A.fsi", blankAbove "type T" si ] ])
    (let si = "module Proj.A\n\ntype B =\n    new: unit -> B\n    abstract V: int -> int\n    default V: x: int -> int\n\ntype D =\n    inherit B\n    new: unit -> D\n    override V: x: int -> int\n\ntype I =\n    abstract W: int\n\ntype J =\n    new: unit -> J\n    interface I\n\nval i: J\n"
     let a = "module Proj.A\n\ntype B() =\n    abstract V: int -> int\n    default _.V x = x\n\ntype D() =\n    inherit B()\n    override _.V x = x + 1\n\ntype I =\n    abstract W: int\n\ntype J() =\n    interface I with\n        member _.W = 1\n\nlet i = J()\nlet k = D().V 1\nlet l = B().V 1\nlet m = (i :> I).W\n"
     smallSigned "signed: abstract, default, override, interface" si a "module Proj.U\nlet k = Proj.A.D().V 1\nlet l = Proj.A.B().V 1\nlet m = (Proj.A.i :> Proj.A.I).W\n"
        [ "blank in impl", [ "A.fs", blankAbove "type B" a ]; "blank in sig", [ "A.fsi", blankAbove "type B" si ] ])
    (let si = "module Proj.A\n\ntype A1 = int\n\ntype T =\n    new: unit -> T\n    member M: x: A1 -> A1\n    member M: x: string -> string\n    member G: x: 'q -> 'q\n    member U: unit -> int\n    member V: int * int -> int\n    member W: x: (int * int) -> int\n    member C: x: int * ?y: int -> int\n\nval f: A1 -> A1\nval g: x: int * y: int -> int\nval h: (int -> int)\n"
     let a = "module Proj.A\n\ntype A1 = int\n\ntype T() =\n    member _.M(x: int) = x\n    member _.M(x: string) = x\n    member _.G(x: 'z) = x\n    member _.U() = 1\n    member _.V(a: int, b: int) = a\n    member _.W(x: int * int) = fst x\n    member _.C(x: int, ?y: int) = x\n\nlet f x = x + 1\nlet g (x, y) = x + y\nlet h x = x + 1\nlet use1 = T().M 1 + f 1 + T().U() + T().V(1, 2) + T().W((1, 2)) + T().C(1)\n"
     smallSigned "signed: spellings" si a "module Proj.U\nopen Proj.A\nlet z = T().M 1 + T().U() + T().V(1, 2) + T().W((1, 2)) + T().C(1) + T().G 1\n"
        [ "blank in impl", [ "A.fs", blankAbove "type T" a ]; "blank in sig", [ "A.fsi", blankAbove "type T" si ] ])
    // Named-argument parameters, keyless, across files
    (let a = "module Proj.A\n\ntype T() =\n    member _.M(first: int) = first\n    static member S(second: int) = second\n\ntype U = | C of field: int\n\nlet f (x: int) = x\n"
     small "named arguments" a "module Proj.U\nopen Proj.A\nlet a = T().M(first = 1)\nlet b = T.S(second = 1)\nlet c = C(field = 1)\n"
        [ "blank above", blankAbove "type T" a; "name moved", rep "M(first: int)" "M( first: int)" (blankAbove "type T" a); "parameter renamed", rep "first: int) = first" "other: int) = other" (blankAbove "type T" a) ])
    (let a = "module Proj.A\n\ntype A1 = int\n\ntype L<'a> = 'a list\n\ntype T() =\n    member _.M(x: A1) = x\n    member _.M(x: string) = x\n    member _.N(x: L<int>) = x\n    member _.N(x: L<string>) = x\n\nlet use1 = T().M 1\n"
     small "reprint abbreviation" a "module Proj.U\nopen Proj.A\nlet a = T().M 1\nlet b = T().M \"s\"\nlet c = T().N [ 1 ]\nlet d = T().N [ \"s\" ]\nlet e: A1 = 1\n"
        [ "same print", rep "type A1 = int" "type A1 =  int" a
          "same print and blank", blankAbove "type T" (rep "type A1 = int" "type A1 =  int" a)
          "name moved", blankAbove "type T" (rep "type A1 = int" "type  A1 = int" a)
          "target changed", blankAbove "type T" (rep "member _.M(x: string) = x" "member _.M(x: int) = x" (rep "type A1 = int" "type A1 = string" a)) ])
    (let si = "module Proj.A\n\ntype T =\n    new: unit -> T\n    member P: int with set\n    member P: int with get\n    member Q: int with get, set\n"
     let a = "module Proj.A\n\ntype T() =\n    member _.P\n        with get () = 1\n        and set (v: int) = ()\n    member _.Q\n        with get () = 1\n        and set (v: int) = ()\n\nlet use1 = T().P\nT().P <- 1\n"
     smallSigned "signed: a property's accessors on two lines" si a "module Proj.U\nlet a = Proj.A.T().P\nProj.A.T().P <- 1\nlet b = Proj.A.T().Q\n"
        [ "blank in sig", [ "A.fsi", blankAbove "type T" si ] ])
    // Generic parameters and type-level lets
    (let a = "module Proj.A\n\ntype T<'x>() =\n    let held: 'x list = []\n    member _.M(a: 'x) = held\n\nlet f<'y> (a: 'y) = a\n"
     small "generic parameters" a "module Proj.U\nlet a = Proj.A.T<int>().M 1\nlet b<'q> (x: 'q) = Proj.A.f<'q> x\n" [ "blank above", blankAbove "type T" a ]) ]


/// A signature pairs with its implementation by module name: here the file names differ
let unmatched =
    let si = "module N.M\n\nval v: int\n\nval f: x: int -> int\n\ntype T =\n    new: unit -> T\n    member P: int\n    member Q: x: int -> int\n\nval user: int\n"
    let a = "module N.M\n\nlet v = 1\n\nlet f (x: int) = x + v\n\nlet hidden = 2\n\ntype T() =\n    member _.P = 1\n    member _.Q(x: int) = x + hidden\n\nlet user = f v + T().P + T().Q 1\n"
    let u = "module U\n\nlet a = N.M.v + N.M.f 1\n\nlet b = N.M.T().P + N.M.T().Q 2\n"

    { Name = "a signature of another file name"
      Files = [ "Sig.fsi", si; "Impl.fs", a; "U.fs", u ]
      Steps =
        [ "blank line in the implementation", [ "Impl.fs", blankAbove "let v" a ]
          "value re-checked, same print", [ "Impl.fs", rep "let f (x: int)" "let f (x:  int)" (blankAbove "let v" a) ]
          // A second finish: the kept user's old value loses the signature its first conformance gave it
          "a row added, and a kept user of the value", [ "Impl.fs", rep "let f (x: int)" "let f (x:  int)" (blankAbove "let v" a) + "\nlet z = 1\n" ]
          "blank line in the signature", [ "Sig.fsi", blankAbove "val v" si ]
          "blank line in both", [ "Impl.fs", blankAbove "type T" (blankAbove "let v" a); "Sig.fsi", blankAbove "type T" (blankAbove "val v" si) ] ] }

/// The same file names, and modules that don't pair, as the SigUnmatched fixture has it. It doesn't compile.
let unpaired =
    let si = "module A\n\nval x: int\n"
    let a = "module Ab\n\nlet f = 1\n\nlet g = f + 1\n"
    let u = "module Z\n\nlet z = Ab.f + Ab.g\n"

    { Name = "a signature whose module pairs with nothing"
      Files = [ "X.fsi", si; "X.fs", a; "Z.fs", u ]
      Steps =
        [ "blank line in the implementation", [ "X.fs", blankAbove "let f" a ]
          "blank line in the signature", [ "X.fsi", blankAbove "val x" si ] ] }

/// What `conformed` answers: an implementation more generic than its signature
let conformed =
    let si = "module Proj.A\n\ntype T =\n    new: unit -> T\n    member One: x: int -> int\n    member Two: x: int -> int\n    member Two: x: int * y: int -> int\n    member Three: x: int -> int\n    member Three: x: string -> string\n    member Pr: int -> int with get\n\nval id2: x: int -> int\n\nval user: int\n"
    let a = "module Proj.A\n\ntype T() =\n    member _.One(x) = x\n    member _.Hid(x) = x\n    member _.Two(x) = x\n    member _.Two(x: int, y: int) = x + y\n    member _.Three(x) = x\n    member _.Three(x: string) = x\n    member _.Pr with get (i: int) = i\n\nlet id2 x = x\n\nlet user = T().One 1 + T().Two 1 + T().Two(1, 2) + T().Three 1 + T().Hid 1 + id2 1 + T().Pr(1)\n"
    let u = "module Proj.U\n\nopen Proj.A\n\nlet z = T().One 1 + T().Two 1 + T().Two(1, 2) + T().Three 1 + T().Three \"s\" + id2 1\n"

    { Name = "implementations more generic than their signature"
      Files = [ "A.fsi", si; "A.fs", a; "U.fs", u ]
      Steps =
        [ "blank line in the implementation", [ "A.fs", blankAbove "type T" a ]
          "a re-check of a member, same print", [ "A.fs", rep "member _.One(x) = x" "member _.One(x) =  x" (blankAbove "type T" a) ]
          "blank line in the signature", [ "A.fsi", blankAbove "type T" si ]
          "overloads reordered in the signature", [ "A.fsi", (blankAbove "type T" si).Replace("    member Two: x: int -> int\n    member Two: x: int * y: int -> int\n", "    member Two: x: int * y: int -> int\n    member Two: x: int -> int\n") ] ] }

let batteries = [ unsigned; signed ]

let libUnsigned =
    { Name = "Lib, unsigned"
      Files = [ "A.fs", aText ]
      Steps = aSteps aText |> List.truncate 2 |> List.map (fun (n, t) -> n, [ "A.fs", t ]) }

let libSigned =
    { Name = "Lib, signed"
      Files = [ "A.fsi", aSig; "A.fs", aText ]
      Steps =
        [ "blank line in the implementation", [ "A.fs", rep "let value = 1" "\nlet value = 1" aText ]
          "blank line in the signature", [ "A.fsi", rep "val value" "\nval value" aSig ] ] }

let all = batteries @ [ unmatched; unpaired; conformed ] @ collisions
