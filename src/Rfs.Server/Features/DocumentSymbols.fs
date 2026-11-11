module Rfs.Server.DocumentSymbols

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspPos = Ionide.LanguageServerProtocol.Types.Position
type private LspRange = Ionide.LanguageServerProtocol.Types.Range

/// Matches FSAC's `LspHelpers.glyphToSymbolKindGenerator`.
let private glyphToSymbolKind (glyph: FSharpGlyph) : SymbolKind =
    match glyph with
    | FSharpGlyph.Class -> SymbolKind.Class
    | FSharpGlyph.Constant -> SymbolKind.Constant
    | FSharpGlyph.Delegate -> SymbolKind.Function
    | FSharpGlyph.Enum -> SymbolKind.Enum
    | FSharpGlyph.EnumMember -> SymbolKind.EnumMember
    | FSharpGlyph.Event -> SymbolKind.Event
    | FSharpGlyph.Exception -> SymbolKind.Class
    | FSharpGlyph.Field -> SymbolKind.Field
    | FSharpGlyph.Interface -> SymbolKind.Interface
    | FSharpGlyph.Method
    | FSharpGlyph.OverridenMethod
    | FSharpGlyph.ExtensionMethod -> SymbolKind.Method
    | FSharpGlyph.Module
    | FSharpGlyph.NameSpace -> SymbolKind.Module
    | FSharpGlyph.Property -> SymbolKind.Property
    | FSharpGlyph.Struct -> SymbolKind.Struct
    | FSharpGlyph.Typedef
    | FSharpGlyph.Type
    | FSharpGlyph.Union -> SymbolKind.Class
    | FSharpGlyph.Variable -> SymbolKind.Variable
    | FSharpGlyph.Error
    | _ -> SymbolKind.Variable

let inline private toLspRange (r: range) : LspRange =
    { Start =
        { Line = uint32 (max 0 (r.StartLine - 1))
          Character = uint32 (max 0 r.StartColumn) }
      End =
        { Line = uint32 (max 0 (r.EndLine - 1))
          Character = uint32 (max 0 r.EndColumn) } }

let inline private posLeq (a: LspPos) (b: LspPos) =
    a.Line < b.Line || (a.Line = b.Line && a.Character <= b.Character)

let inline private unionLspRanges (a: LspRange) (b: LspRange) : LspRange =
    { Start = if posLeq a.Start b.Start then a.Start else b.Start
      End = if posLeq a.End b.End then b.End else a.End }

let private mapItem (decl: NavigationItem) (children: DocumentSymbol[]) : DocumentSymbol option =
    if String.IsNullOrWhiteSpace decl.LogicalName || Range.equals decl.Range Range.range0 then
        None
    else
        let selectionRange = toLspRange decl.Range

        // LSP 3.17 invariant: `Range` MUST strictly enclose `SelectionRange` and all `Children` ranges.
        // In FCS `ServiceNavigation.fs`, attributed types (`[<Struct>] type T`) and modules can have `Range` starting
        // before `BodyRange`, while fieldless union cases / exceptions can have `BodyRange = Range.range0`.
        let mutable fullRange =
            if Range.equals decl.BodyRange Range.range0 then
                selectionRange
            else
                toLspRange (Range.unionRanges decl.Range decl.BodyRange)

        for child in children do
            fullRange <- unionLspRanges fullRange child.Range

        Some
            { Name = decl.LogicalName
              Detail = None
              Kind = glyphToSymbolKind decl.Glyph
              Tags = None
              Deprecated = None
              Range = fullRange
              SelectionRange = selectionRange
              Children = if Array.isEmpty children then None else Some children }

let internal compute (parseTree: ParsedInput) : DocumentSymbol[] =
    StackGuard.onLargeStack (fun () ->
        let nav = Navigation.getNavigation parseTree

        [| for top in nav.Declarations do
               let nested = top.Nested |> Array.choose (fun item -> mapItem item [||])

               if String.IsNullOrWhiteSpace top.Declaration.LogicalName || Range.equals top.Declaration.Range Range.range0 then
                   yield! nested
               else
                   match mapItem top.Declaration nested with
                   | Some sym -> yield sym
                   | None -> yield! nested |])

/// Pure-syntax hierarchical document symbols (`U2.C2`) for `.fs` and `.fsi` via `ProjectGraph.LatestParse` (never waits on typechecking).
let hierarchical (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task<U2<SymbolInformation[], DocumentSymbol[]> option> =
    task {
        let! textView = graph.LatestParse(path, ct)
        return Some(U2.C2(compute textView.Parse.ParseTree))
    }
