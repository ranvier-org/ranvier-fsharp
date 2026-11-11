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
/// answers only inside the row that uses it, else nothing (see `at`). In a signature (<c>.fsi</c>) file, a symbol
/// declared there jumps to its paired <c>.fs</c> implementation when one exists, and a symbol referenced there jumps
/// to its workspace definition. Not ported: decompiling and sourcelink (an external definition answers nothing), and
/// <c>#load</c>.
/// <para>
/// Where FCS answers and this answers nothing, never something stale, in the DefinitionGrid's batteries (tokens across
/// states, unsigned of 11581 and signed of 15149): a signed function's or member's parameter (0, 1071), a named
/// argument (39, 51), a namespace outside its own file's header (104, 136). Also in a file that doesn't compile a key
/// two definitions share.
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
    /// file's reading is over before another file's starts: no read waits for a file while it holds one. With it, what
    /// an answer from the last state needs: the place asked at, and the files the answer was read from at their versions.
    let private atIn
        (record: (unit -> ProjectGraph list) option)
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        =
        task {
            // The loaded projects, read before the answer's turns: one loaded during them refuses, never the other way
            let! loaded =
                match record with
                | Some projects ->
                    task {
                        let! now = graph.FilesNow([||], (fun () -> false), (fun () -> false), projects, ct)
                        return now.Projects
                    }
                | None -> Task.FromResult []

            // Only the check is asked under the lock
            let! struct (found, view, lookup) =
                graph.ReadFile(
                    path,
                    (fun view -> Hover.lookupOf view.Text line character),
                    (fun view lookup ->
                        let inputs = struct (view.Version, view.Inputs, view.Generation)

                        match lookup with
                        | Some l -> struct (find view.Result.CheckResults line l, inputs, lookup)
                        | None -> struct (Nowhere, inputs, lookup)),
                    record.IsSome,
                    ct
                )

            let struct (version, inputs, generation) = view
            let files = graph.FilesOf

            // The asking file's result is of its text and of every file above it, at these versions
            let asking = graph, generation, Array.init inputs.Length (fun j -> struct (files[j], inputs[j]))

            let entry (parts: (ProjectGraph * int64 * struct (string * int64)[]) list) (answer: range option) =
                match lookup, LastState.merge (asking :: parts) with
                | Some l, Some graphs when record.IsSome ->
                    let e: LastState.Entry<range option> =
                        { Graphs = graphs
                          Projects = LastState.projectsOf loaded
                          Answer = answer }
                    ValueSome(struct (LastState.keyOf path version line l, e))
                | _ -> ValueNone

            match found with
            | Nowhere -> return struct (None, entry [] None)
            | Local range ->
                // In the row that uses it, which the file's result keeps current. Anywhere else, as a signed function's
                // parameter lands in its signature, nothing keeps that location current, and there is no key to look it up.
                let here = String.Equals(Path.GetFullPath range.FileName, Path.GetFullPath path, StringComparison.OrdinalIgnoreCase)
                let answer = if here && not (range.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then Some range else None
                return struct (answer, entry [] answer)
            | Keyed(key, landed, asSignature) ->
                let implementation = if landed.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then Path.ChangeExtension(landed, ".fs") else landed
                let owners = ownersOf implementation @ ownersOf (Path.ChangeExtension(landed, ".fsi")) |> List.distinct
                let mutable answer = None
                let mutable from = []

                // A file two projects hold: only the one whose assembly the key names defines it
                for owner in owners do
                    if answer.IsNone then
                        let! struct (current, at, read, generation) = owner.CurrentLocationAt((if asSignature then landed else path), landed, key, ct)
                        answer <- current

                        match current with
                        | Some r when at >= 0L ->
                            let ownerFiles = owner.FilesOf
                            // The range's file at the version its index was read at, and every file up to the last read
                            from <- [ owner, generation, Array.init read.Length (fun j -> struct (ownerFiles[j], read[j])) ]
                        | _ -> from <- []

                // Nothing found in a project read: which files decided that, no turn here says
                return struct (answer, (if answer.IsSome || owners.IsEmpty then entry from answer else ValueNone))
        }

    [<NoComparison>]
    type private SigFound =
        | SigNowhere
        | SigDecl of key: string * ownRange: range
        | SigLocalDecl of range
        | SigRef of key: string * landed: string

    let internal pickSignatureUse
        (line: int)
        (character: int)
        (endColOpt: int option)
        (sigUses: (struct (range * bool * FSharpSymbol))[])
        : struct (range * bool * FSharpSymbol) option =
        let cursorPos = Position.mkPos (line + 1) character

        let containing =
            sigUses
            |> Array.filter (fun (struct (r, _, _)) -> Range.rangeContainsPos r cursorPos)

        if containing.Length = 0 then
            None
        else
            let paramPenalty (s: FSharpSymbol) = if s :? FSharpParameter then 1 else 0

            match endColOpt with
            | Some endCol ->
                containing
                |> Array.filter (fun (struct (r, _, _)) -> r.EndLine = line + 1 && r.EndColumn = endCol)
                |> Array.sortBy (fun (struct (r, _, s)) -> paramPenalty s, r.EndLine - r.StartLine, r.EndColumn - r.StartColumn)
                |> Array.tryHead
                |> Option.orElseWith (fun () ->
                    containing
                    |> Array.sortBy (fun (struct (r, _, s)) -> paramPenalty s, r.EndLine - r.StartLine, r.EndColumn - r.StartColumn)
                    |> Array.tryHead)
            | None ->
                containing
                |> Array.sortBy (fun (struct (r, _, s)) -> paramPenalty s, r.EndLine - r.StartLine, r.EndColumn - r.StartColumn)
                |> Array.tryHead

    let private atSignature
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<range option> =
        task {
            let! found =
                graph.ReadSignature(
                    path,
                    (fun view -> Hover.lookupOf view.Text line character),
                    (fun _ lookup _ sigUses ->
                        let endColOpt = lookup |> Option.map (fun (_, _, ec) -> ec)

                        match pickSignatureUse line character endColOpt sigUses with
                        | None -> SigNowhere
                        | Some(struct (_, _, symbol)) when (symbol :? FSharpParameter) -> SigNowhere
                        | Some(struct (_, isDef, symbol)) when (match symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false) && not isDef ->
                            SigNowhere
                        | Some(struct (ownRange, true, symbol)) ->
                            match DeclarationChecker.SymbolKey symbol with
                            | Some key -> SigDecl(key, ownRange)
                            | None -> SigLocalDecl ownRange
                        | Some(struct (_, false, symbol)) ->
                            match DeclarationChecker.SymbolKey symbol, symbol.SignatureLocation |> Option.orElse symbol.DeclarationLocation with
                            | Some key, Some loc when not (loc.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)) ->
                                SigRef(key, loc.FileName)
                            | _ -> SigNowhere),
                    ct
                )

            match found with
            | SigNowhere -> return None
            | SigLocalDecl ownRange -> return Some ownRange
            | SigDecl(key, ownRange) ->
                let implPath = Path.ChangeExtension(path, ".fs")
                let owners = (ownersOf implPath @ ownersOf path @ [ graph ]) |> List.distinct
                let mutable answer = None

                for owner in owners do
                    if answer.IsNone then
                        let! implLoc = owner.ImplementationLocation(path, key, ct)
                        answer <- implLoc

                return answer |> Option.orElse (Some ownRange)
            | SigRef(key, landed) ->
                let implementation =
                    if landed.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                        Path.ChangeExtension(landed, ".fs")
                    else
                        landed

                let owners = (ownersOf implementation @ ownersOf (Path.ChangeExtension(landed, ".fsi"))) |> List.distinct
                let mutable answer = None

                for owner in owners do
                    if answer.IsNone then
                        let! current = owner.CurrentLocation(path, landed, key, ct)
                        answer <- current

                return answer
        }

    let at (ownersOf: string -> ProjectGraph list) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<range option> =
        if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
            atSignature ownersOf graph path line character ct
        else
            task {
                let! struct (found, _) = atIn None ownersOf graph path line character ct
                return found
            }

    /// The settled answers definition gave, for answers from the last state
    type LastAnswers = LastState.Cache<range option>

    /// <summary>
    /// The definition's location from the last state while anything is pending (LastState.answer): the settled answer
    /// asked at the place the cursor had, mapped to the latest text, or a refusal. Reads no check.
    /// </summary>
    let internal fromLastState
        (answers: LastAnswers)
        (shapes: Hover.LastAnswers)
        (busy: unit -> bool)
        (pending: unit -> bool)
        (projects: unit -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<LastState.Outcome<range option>> =
        let remap (remap: LastState.Remap) (answer: range option) =
            match answer with
            | None -> Ok None
            | Some r -> remap r |> Result.map Some

        LastState.answer answers shapes busy pending projects graph path line character remap ct

    /// <summary>
    /// The definition's location: the settled answer, given to `answers` too, or one from the last state (fromLastState)
    /// while the settled one hasn't completed. Only for a client that shows the server's progress.
    /// </summary>
    let answer
        (answers: LastAnswers)
        (shapes: Hover.LastAnswers)
        (busy: unit -> bool)
        (pending: unit -> bool)
        (whenShown: unit -> Task)
        (projects: unit -> ProjectGraph list)
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<range option> =
        LastState.race
            whenShown
            (fun ct ->
                task {
                    let! struct (found, entry) = atIn (Some projects) ownersOf graph path line character ct

                    match entry with
                    | ValueSome(struct (key, e)) -> answers.Add(key, e)
                    | ValueNone -> ()

                    return found
                })
            (fromLastState answers shapes busy pending projects graph path line character)
            ct
