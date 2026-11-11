module Rfs.Server.WorkspaceSymbols

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspRange = Ionide.LanguageServerProtocol.Types.Range

let private maxResults = 1000

/// Matches FSAC's `LspHelpers.glyphToSymbolKindGenerator` (same mapping as `DocumentSymbols.fs`).
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

let inline private isSeparator (c: char) =
    c = '_' || c = '.' || c = ' ' || c = '`'

let inline private isWordStart (s: string) (i: int) =
    i = 0 || Char.IsUpper s[i] || isSeparator s[i - 1]

let inline private isHump (s: string) (i: int) =
    Char.IsUpper s[i] || (i > 0 && isSeparator s[i - 1])

/// CamelCase / PascalCase subsequence matcher (e.g. `"gSC"` -> `"GetSemanticClassification"`,
/// `"mC"` / `"mkCust"` -> `"makeCustomer"`, while rejecting non-hump `'c'` in `"makeconstant"`
/// and mid-word `'m'` in `"someConstant"`).
let internal isCamelCaseMatch (pattern: string) (candidate: string) : bool =
    let pLen = pattern.Length
    let cLen = candidate.Length

    if pLen < 2 || cLen < pLen then
        false
    else
        let mutable hasUpper = false

        for i in 0 .. pLen - 1 do
            if Char.IsUpper pattern[i] then
                hasUpper <- true

        if not hasUpper then
            false
        else
            let failed = Array.zeroCreate<bool> (pLen * cLen)

            let rec step (pi: int) (ci: int) : bool =
                if pi = pLen then
                    true
                elif ci >= cLen || (cLen - ci) < (pLen - pi) then
                    false
                else
                    let idx = pi * cLen + ci

                    if failed[idx] then
                        false
                    else
                        let pc = pattern[pi]
                        let cc = candidate[ci]

                        let charMatches =
                            if pi = 0 then
                                isWordStart candidate ci && Char.ToUpperInvariant pc = Char.ToUpperInvariant cc
                            elif Char.IsUpper pc then
                                isHump candidate ci && Char.ToUpperInvariant pc = Char.ToUpperInvariant cc
                            else
                                Char.ToLowerInvariant pc = Char.ToLowerInvariant cc

                        let matched = (charMatches && step (pi + 1) (ci + 1)) || step pi (ci + 1)

                        if not matched then
                            failed[idx] <- true

                        matched

            step 0 0

let internal matchesQuery (trimmedQuery: string) (name: string) (containerOpt: string option) : bool =
    if trimmedQuery.Length = 0 then
        true
    elif name.IndexOf(trimmedQuery, StringComparison.OrdinalIgnoreCase) >= 0 then
        true
    elif
        trimmedQuery.IndexOf('.') >= 0
        && (match containerOpt with
            | Some c -> (c + "." + name).IndexOf(trimmedQuery, StringComparison.OrdinalIgnoreCase) >= 0
            | None -> false)
    then
        true
    else
        isCamelCaseMatch trimmedQuery name

let private extractSymbols
    (query: string)
    (projectFiles: (ProjectGraph * (string * TextView)[])[])
    (ct: CancellationToken)
    : WorkspaceSymbol[] =
    let trimmed = if isNull query then "" else query.Trim()

    StackGuard.onLargeStack (fun () ->
        let seenFiles = HashSet<string>(StringComparer.OrdinalIgnoreCase)
        let results = ResizeArray<WorkspaceSymbol>()

        let tryAdd (uri: string) (containerOpt: string option) (decl: NavigationItem) =
            if
                results.Count < maxResults
                && not (String.IsNullOrWhiteSpace decl.LogicalName)
                && not (Range.equals decl.Range Range.range0)
                && matchesQuery trimmed decl.LogicalName containerOpt
            then
                results.Add
                    { Name = decl.LogicalName
                      Kind = glyphToSymbolKind decl.Glyph
                      Tags = None
                      ContainerName = containerOpt
                      Location =
                        U2.C1
                            { Uri = uri
                              Range = toLspRange decl.Range }
                      Data = None }

        for _, views in projectFiles do
            for path, view in views do
                ct.ThrowIfCancellationRequested()

                if results.Count < maxResults then
                    let normPath =
                        try
                            Path.GetFullPath path
                        with _ ->
                            path

                    if seenFiles.Add normPath then
                        let uri = Paths.toUri path
                        let nav = Navigation.getNavigation view.Parse.ParseTree

                        for top in nav.Declarations do
                            if results.Count < maxResults then
                                tryAdd uri None top.Declaration

                                let containerOpt =
                                    if String.IsNullOrWhiteSpace top.Declaration.LogicalName then
                                        None
                                    else
                                        Some top.Declaration.LogicalName

                                for nested in top.Nested do
                                    if results.Count < maxResults then
                                        tryAdd uri containerOpt nested

        results.ToArray())

let internal searchOnHost
    (host: GraphHost)
    (projectsNow: unit -> ProjectGraph list)
    (loadingNow: unit -> bool)
    (filesNow: ProjectGraph -> string list)
    (query: string)
    (ct: CancellationToken)
    : Task<U2<SymbolInformation[], WorkspaceSymbol[]> option> =
    task {
        let mutable answer: WorkspaceSymbol[] option = None

        while answer.IsNone do
            ct.ThrowIfCancellationRequested()

            let! firstGraphOpt =
                host.WhenSettled(
                    (fun () ->
                        if loadingNow () then
                            ValueNone
                        else
                            match projectsNow () with
                            | [] -> ValueSome None
                            | g :: _ -> ValueSome(Some g)),
                    ct
                )

            match firstGraphOpt with
            | None ->
                answer <- Some [||]
            | Some hostGraph ->
                let scope () =
                    if loadingNow () then
                        struct (true, [||])
                    else
                        let ps = projectsNow ()
                        struct (false, [| for g in ps -> g, filesNow g |])

                match! hostGraph.LatestParsesInOneTurn(scope, ct) with
                | ValueSome read ->
                    answer <- Some(extractSymbols query read ct)
                | ValueNone ->
                    // A project in scope was disposed concurrently by a reload; loop to pick up the current graph set.
                    ()

        return Some(U2.C2 answer.Value)
    }

let search
    (host: GraphHost)
    (projectsNow: unit -> ProjectGraph list)
    (loadingNow: unit -> bool)
    (filesNow: ProjectGraph -> string list)
    (query: string)
    (ct: CancellationToken)
    : Task<U2<SymbolInformation[], WorkspaceSymbol[]> option> =
    searchOnHost host projectsNow loadingNow filesNow query ct

let resolve (sym: WorkspaceSymbol) : WorkspaceSymbol = sym
