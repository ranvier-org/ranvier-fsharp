namespace Rfs.Server

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Go to definition, ported from FSAC's <c>TryFindIdentifierDeclaration</c> (ParseAndCheckResults.fs:102) and
/// <c>findDeclaration</c> (Commands.fs): the signature file when the symbol has one, as there.
/// </summary>
/// <remarks>
/// What the symbol under the cursor says tells only which definition it is, and which file to look in: a location it
/// holds can be stale (ProjectGraph.CurrentLocation), so the answer is read from that file now. A symbol with no key
/// answers only inside the row that uses it, else nothing (see `at`). Not ported: decompiling and sourcelink (an external
/// definition answers nothing), <c>#load</c>, and moving from a signature to its implementation (a signature file has no
/// result to ask, and answers nothing).
/// <para>
/// Where FCS answers and this answers nothing, never something stale, in the DefinitionGrid's batteries (tokens across
/// states, unsigned of 11581 and signed of 15149): a signed function's or member's parameter (0, 1071), a named
/// argument (39, 51), a namespace outside its own file's header (104, 136). Also a cursor in a
/// signature file, and in a file that doesn't compile a key two definitions share.
/// </para>
/// </remarks>
module Definition =
    [<NoComparison>]
    type private Found =
        | Nowhere
        /// The location is the symbol's own
        | Local of range
        /// `asSignature`: read as a file after the signature would, so a signature file's answer isn't asked of the
        /// implementation, which doesn't conform a namespace
        | Keyed of key: string * landed: string * asSignature: bool

    /// Whether the location is the definition under the cursor: this row's, so current. FCS answers it for a property
    /// even when a signature lists the property, as what sits on its name didn't conform.
    let private isOwnDefinition (check: FSharpCheckFileResults) (line: int) (endColumn: int) (range: range) =
        range.EndLine = line + 1
        && range.EndColumn = endColumn
        && check.GetAllUsesOfAllSymbolsInFile()
           |> Seq.exists (fun u -> u.IsFromDefinition && Range.equals u.Range.EndRange range.EndRange && u.FileName = range.FileName)

    let private find (check: FSharpCheckFileResults) (line: int) (lineStr: string, names: string list, endColumn: int) : Found =
        match check.GetDeclarationLocation(line + 1, endColumn, lineStr, names, preferFlag = true) with
        | FindDeclResult.DeclFound range when not (range.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)) ->
            match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
            // A named argument's parameter is another row's, or another file's, and has no key to find it by
            | Some u when (u.Symbol :? FSharpParameter) -> Nowhere
            // A namespace has no one definition: only the token in a file's own header answers
            | Some u when (match u.Symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false) ->
                let isHeader =
                    check.GetAllUsesOfAllSymbolsInFile()
                    |> Seq.exists (fun d ->
                        d.IsFromDefinition
                        && d.Range.EndLine = line + 1
                        && d.Range.EndColumn = endColumn
                        && (match d.Symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false))

                match DeclarationChecker.SymbolKey u.Symbol with
                | Some key when isHeader ->
                    // FCS answers the signature this file conformed to, which pairs with it by module name. By declaration
                    // the header's own symbol doesn't say so, but what the file defines does.
                    let signature =
                        check.GetAllUsesOfAllSymbolsInFile()
                        |> Seq.tryPick (fun d ->
                            match d.IsFromDefinition, d.Symbol.SignatureLocation with
                            | true, Some r when r.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) -> Some r.FileName
                            | _ -> None)

                    Keyed(key, defaultArg signature range.FileName, true)
                | _ -> Nowhere
            | _ when isOwnDefinition check line endColumn range -> Local range
            | u ->
                match u |> Option.bind (fun u -> DeclarationChecker.SymbolKey u.Symbol) with
                | Some key -> Keyed(key, range.FileName, false)
                | None -> Local range
        | _ -> Nowhere

    /// The cursor's answer is a request node on the file (ProjectGraph.ReadFile), which runs again on a new text. The
    /// file's reading is over before another file's starts: no read waits for a file while it holds one.
    let at (ownersOf: string -> ProjectGraph list) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<range option> =
        task {
            // Only the check is asked under the lock
            let! found =
                graph.ReadFile(
                    path,
                    (fun view -> Hover.lookupOf view.Text line character),
                    (fun view lookup ->
                        match lookup with
                        | Some lookup -> find view.Result.CheckResults line lookup
                        | None -> Nowhere),
                    ct
                )

            match found with
            | Nowhere -> return None
            | Local range ->
                // In the row that uses it, which the file's result keeps current. Anywhere else, as a signed function's
                // parameter lands in its signature, nothing keeps that location current, and there is no key to look it up.
                let here = String.Equals(Path.GetFullPath range.FileName, Path.GetFullPath path, StringComparison.OrdinalIgnoreCase)
                return (if here && not (range.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then Some range else None)
            | Keyed(key, landed, asSignature) ->
                let implementation = if landed.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then Path.ChangeExtension(landed, ".fs") else landed
                let owners = ownersOf implementation @ ownersOf (Path.ChangeExtension(landed, ".fsi")) |> List.distinct
                let mutable answer = None

                // A file two projects hold: only the one whose assembly the key names defines it
                for owner in owners do
                    if answer.IsNone then
                        let! current = owner.CurrentLocation((if asSignature then landed else path), landed, key, ct)
                        answer <- current

                return answer
        }
