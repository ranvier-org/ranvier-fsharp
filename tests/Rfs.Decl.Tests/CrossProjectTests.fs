module Rfs.Decl.Tests.CrossProjectTests

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Expecto
open FSharp.Compiler.Declarations
open Rfs.Projects
open Rfs.Decl.Tests.Common

let private multiDir = Path.Combine(Path.GetDirectoryName tinyDir, "Multi")
let private libProject = Path.Combine(multiDir, "Lib", "Lib.fsproj")
let private appProject = Path.Combine(multiDir, "App", "App.fsproj")
let private libFile = Path.Combine(multiDir, "Lib", "Lib.fs")
let private signedFile = Path.Combine(multiDir, "Lib", "Signed.fs")
let private useFile = Path.Combine(multiDir, "App", "Use.fs")

let private load (project: string) =
    Rfs.Harness.Fixtures.build appProject
    (ProjectSpec.load project).GetAwaiter().GetResult()

/// Checks every file whole in file order: each file's result, when it is an implementation file, and every signature
let private checkAll (ctx: ProjectContext) (spec: ProjectSpec) (overrides: Map<string, string>) =
    let texts = sources spec overrides

    ((([], []), spec.SourceFiles)
     ||> List.fold (fun (results, before) file ->
         let parse = DeclarationChecker.Parse(ctx, file, texts[file])

         if file.EndsWith ".fsi" then
             results, before @ [ DeclarationChecker.CheckSignatureFile(ctx, before, parse) ]
         else
             let result = DeclarationChecker.CheckFile(ctx, before, parse)
             results @ [ result ], before @ [ result.Signature ]))

let private assemblyOf (spec: ProjectSpec) (overrides: Map<string, string>) =
    let ctx = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions spec) |> Async.RunSynchronously
    let results, signatures = checkAll ctx spec overrides
    DeclarationChecker.ProjectAssembly(ctx, results, signatures)

let private diagnosticsOf (ctx: ProjectContext) (spec: ProjectSpec) (overrides: Map<string, string>) =
    let results, _ = checkAll ctx spec overrides
    results |> List.collect (fun r -> r.Diagnostics |> List.ofArray) |> List.map (fun d -> d.ErrorNumber, d.StartLine, d.Message)

let private appWith (assembly: ProjectAssembly) =
    let app = load appProject
    let lib = load libProject
    let references = Dictionary<string, ProjectAssembly>()
    references[(ProjectSpec.referencePath app lib).Value] <- assembly
    DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions app, references) |> Async.RunSynchronously

let private libText () = File.ReadAllText libFile

[<Tests>]
let tests =
    testSequenced
    <| testList
        "CrossProject"
        [ test "in-memory reference equals built dll" {
              let lib, app = load libProject, load appProject
              let onDisk = DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions app) |> Async.RunSynchronously
              let inMemory = appWith (assemblyOf lib Map.empty)
              // Errors in App that read Lib: each message names what it found there
              let bad = Map [ "Use.fs", "module App.Use\n\nopen Lib.Api\n\nlet a: string = f 1\n\nlet b: string = g 1\n\nlet c: int = shapeOf ()\n\nlet d = nope\n" ]
              let expected = diagnosticsOf onDisk app bad
              Expect.isGreaterThanOrEqual expected.Length 4 "the text reports its errors"
              Expect.equal (diagnosticsOf inMemory app bad) expected "the same diagnostics"
              Expect.equal (diagnosticsOf inMemory app Map.empty) (diagnosticsOf onDisk app Map.empty) "and for the text on disk"
          }

          test "in-memory reference reads the assembly, not the file" {
              let lib, app = load libProject, load appProject
              let edited = (libText ()).Replace("let f (x: int) : int = x + 1", "let f (x: int) : string = string x")
              let inMemory = appWith (assemblyOf lib (Map [ "Lib.fs", edited ]))
              let diagnostics = diagnosticsOf inMemory app Map.empty
              Expect.exists diagnostics (fun (n, line, _) -> n = 1 && line = 7) $"y + 1 no longer adds up: %A{diagnostics}"
          }

          test "internals visible in memory" {
              let lib, app = load libProject, load appProject
              let inMemory = appWith (assemblyOf lib Map.empty)
              Expect.isEmpty (diagnosticsOf inMemory app Map.empty) "App reads g"

              let closed = (libText ()).Replace("""[<assembly: InternalsVisibleTo("App")>]""", """[<assembly: InternalsVisibleTo("Other")>]""")
              let hidden = appWith (assemblyOf lib (Map [ "Lib.fs", closed ]))
              let diagnostics = diagnosticsOf hidden app Map.empty
              Expect.exists diagnostics (fun (_, line, _) -> line = 9) $"g is out of reach: %A{diagnostics}"
          }

          test "the fingerprint keeps a body edit and sees a signature and an attribute" {
              let lib = load libProject
              let fingerprint overrides = (assemblyOf lib overrides).Fingerprint
              let text = libText ()
              let first = fingerprint Map.empty
              Expect.equal (fingerprint Map.empty) first "the same check of the same text"
              Expect.equal (fingerprint (Map [ "Lib.fs", text.Replace("x + 1", "x + 3") ])) first "a body"
              Expect.notEqual (fingerprint (Map [ "Lib.fs", text.Replace("x + 1", "string x").Replace(": int = string", ": string = string") ])) first "a return type"
              Expect.notEqual (fingerprint (Map [ "Lib.fs", text.Replace("\"App\"", "\"Other\"") ])) first "InternalsVisibleTo"
          }

          test "an assembly attribute in a file with a signature file changes the fingerprint and App" {
              let lib, app = load libProject, load appProject
              let first = (assemblyOf lib Map.empty).Fingerprint
              let signed = File.ReadAllText signedFile

              let opened =
                  signed.Replace(
                      "let signed = 1",
                      "open System.Runtime.CompilerServices\n\n[<assembly: InternalsVisibleTo(\"Other\")>]\ndo ()\n\nlet signed = 1"
                  )

              let withAttribute = assemblyOf lib (Map [ "Signed.fs", opened ])
              Expect.notEqual withAttribute.Fingerprint first "its print leaves the attribute out; the assembly's does not"

              let closed = (libText ()).Replace("""[<assembly: InternalsVisibleTo("App")>]""", "")
              let reopened = signed.Replace("let signed = 1", "open System.Runtime.CompilerServices\n\n[<assembly: InternalsVisibleTo(\"App\")>]\ndo ()\n\nlet signed = 1")
              let viaSigned = appWith (assemblyOf lib (Map [ "Lib.fs", closed; "Signed.fs", reopened ]))
              Expect.isEmpty (diagnosticsOf viaSigned app Map.empty) "the attribute of the signed file reaches App"
          }

          test "the fingerprint sees everything App's diagnostics read" {
              let lib = load libProject
              let text = libText ()
              let fingerprint (extra: string) = (assemblyOf lib (Map [ "Lib.fs", text + "\n" + extra + "\n" ])).Fingerprint

              for name, before, after in
                  [ "a union case", "type U =\n    | A\n", "type U =\n    | A\n    | B\n"
                    "the order of cases", "type U =\n    | A\n    | B\n", "type U =\n    | B\n    | A\n"
                    "a member in an augmentation", "type U = { X: int }\ntype U with\n    member _.P = 1\n", "type U = { X: int }\ntype U with\n    member _.Q = 1\n"
                    "an attribute's argument", "[<System.Obsolete(\"a\")>]\nlet u = 1\n", "[<System.Obsolete(\"b\")>]\nlet u = 1\n"
                    "a literal", "[<Literal>]\nlet U = 1\n", "[<Literal>]\nlet U = 2\n"
                    "a default parameter", "type U =\n    static member M([<System.Runtime.InteropServices.Optional; System.Runtime.InteropServices.DefaultParameterValue(1)>] x: int) = x\n", "type U =\n    static member M([<System.Runtime.InteropServices.Optional; System.Runtime.InteropServices.DefaultParameterValue(2)>] x: int) = x\n"
                    "EqualityConditionalOn", "[<CustomEquality; NoComparison>]\ntype U<'T> =\n    { V: int }\n    override _.Equals _ = true\n    override _.GetHashCode() = 0\n", "[<CustomEquality; NoComparison>]\ntype U<[<EqualityConditionalOn>] 'T> =\n    { V: int }\n    override _.Equals _ = true\n    override _.GetHashCode() = 0\n"
                    "explicit type parameters", "let u<'T> (x: 'T) = x\n", "let u (x: 'T) = x\n"
                    "a type parameter's name", "let u (x: 'A) = x\n", "let u (x: 'B) = x\n"
                    "prefix display", "type U<'a> = { V: 'a }\n", "type 'a U = { V: 'a }\n"
                    "a struct record", "type U = { V: int }\n", "[<Struct>]\ntype U = { V: int }\n"
                    "a self-referential constructor", "type U() =\n    member _.P = 1\n", "type U() as this =\n    member _.P = this.GetHashCode()\n" ] do
                  Expect.notEqual (fingerprint after) (fingerprint before) name

              // Generalized from inference, with no name of the user's: checking it again gives the same print
              Expect.equal (fingerprint "let i x = x\n") (fingerprint "let i x = id x\n") "an inferred type parameter has no name to see"
          }

          test "a pickling failure is an error, not a missing reference" {
              let lib, app = load libProject, load appProject
              let broken = DeclarationChecker.FailingProjectAssembly "the pickle failed"
              let references = Dictionary<string, ProjectAssembly>()
              references[(ProjectSpec.referencePath app lib).Value] <- broken

              Expect.throws
                  (fun () -> DeclarationChecker.CreateProject(ProjectSpec.toFcsOptions app, references) |> Async.RunSynchronously |> ignore)
                  "CreateProject fails where its caller sees it"
          } ]
