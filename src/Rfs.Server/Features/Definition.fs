namespace Rfs.Server

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Go to definition, ported from FSAC's <c>TryFindIdentifierDeclaration</c> (ParseAndCheckResults.fs:102) and
/// <c>findDeclaration</c> (Commands.fs): the signature file when the symbol has one, as there.
/// </summary>
/// <remarks>
/// What the symbol under the cursor says tells only which definition it is, and which file to look in: a location it
/// holds can be stale (ProjectGraph.CurrentLocation), so the answer is read from that file now. A local has no key, and
/// is in the row that uses it, which the file's result keeps current. Not ported: decompiling and sourcelink (an external
/// definition answers nothing), <c>#load</c>, and moving from a signature to its implementation (a signature file has no
/// result to ask, and answers nothing).
/// </remarks>
module Definition =
    [<NoComparison>]
    type private Found =
        | Nowhere
        /// The location is the symbol's own
        | Local of range
        | Keyed of key: string * location: range

    let private find (check: FSharpCheckFileResults) (line: int) (lineStr: string, names: string list, endColumn: int) : Found =
        match check.GetDeclarationLocation(line + 1, endColumn, lineStr, names, preferFlag = true) with
        | FindDeclResult.DeclFound range when not (range.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)) ->
            match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) |> Option.bind (fun u -> DeclarationChecker.SymbolKey u.Symbol) with
            | Some key -> Keyed(key, range)
            | None -> Local range
        | _ -> Nowhere

    /// The file's reading is over before another file's starts: no read waits for a file while it holds one.
    let at (workspace: Workspace) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<range option> =
        task {
            let mutable looked = None
            let mutable found = ValueNone

            // Only the check is asked inside the read; a read that finds another text than the name was looked up in looks it up again
            while found.IsNone do
                let current = looked

                let! answer =
                    graph.ReadFile(
                        path,
                        (fun view ->
                            match current with
                            | Some(text, lookup) when obj.ReferenceEquals(text, view.Text) -> Choice1Of2(find view.Result.CheckResults line lookup)
                            | _ -> Choice2Of2 view.Text),
                        ct
                    )

                match answer with
                | Choice1Of2 f -> found <- ValueSome f
                | Choice2Of2 text ->
                    match Hover.lookupOf text line character with
                    | Some lookup -> looked <- Some(text, lookup)
                    | None -> found <- ValueSome Nowhere

            match found.Value with
            | Nowhere -> return None
            | Local range -> return (if workspace.OwnersOf range.FileName |> List.isEmpty then None else Some range)
            | Keyed(key, location) ->
                let landed = location.FileName
                let implementation = if landed.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then Path.ChangeExtension(landed, ".fs") else landed
                let owners = workspace.OwnersOf implementation @ workspace.OwnersOf(Path.ChangeExtension(landed, ".fsi")) |> List.distinct
                let mutable answer = None

                // A file two projects hold: only the one whose assembly the key names defines it
                for owner in owners do
                    if answer.IsNone then
                        let! location = owner.CurrentLocation(path, location, key, ct)
                        answer <- location

                return answer
        }
