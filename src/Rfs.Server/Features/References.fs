namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Find references: the uses with the symbol's key in every implementation file of the project that defines it and of the
/// loaded projects that reference it, or of every loaded project when none defines it. Each file is read as it stands,
/// checked by declaration or whole; asking promotes nothing.
/// </summary>
/// <remarks>
/// <para>
/// Ranges: a file's published result is current, kept rows included: shifting a row moves its uses, and App's own uses are
/// App's. What a use's symbol says of its location is not read for an answer. A signature file's uses are its latest
/// check's index (ISignatureFile.Uses, read in the same turn), so a blank line in it moves them.
/// </para>
/// <para>
/// A symbol with no key (a local, a parameter) answers its uses in the asking file's result, in one read, so a parameter's
/// named uses in other files aren't found. A named argument answers the same in its method's file, and nothing elsewhere:
/// that file's result has neither its definition nor its other uses.
/// </para>
/// <para>
/// Consistency: every file's index, the project set, the loading state and the name filter's texts are read in one
/// graph turn (ProjectGraph.ReadInOneTurn), with every file settled on its latest text: one state, across projects. A turn
/// with any of them pending, or with the workspace loading, isn't answered from; the request waits for a later one, with
/// no time limit, until a turn with everything settled or the client's cancel. So typing holds an answer up until it
/// pauses. A reload under way is pending too, and the projects read are those of the turn, the asking file's as it is
/// then: a request waiting through a reload answers from after it, never a mix. A project that is disposed and not
/// loaded again (a scope that still names it) answers Moved: its nodes never change again. The handler gives LSP's
/// ContentModified, and Ionide's bundled client then shows no results and doesn't ask again.
/// </para>
/// <para>
/// Each file's uses are a lookup in its node's index (FileUses), built once per result and shared by every request. For a
/// symbol a loaded project defines and that is always written by name (SymbolUses.spelledName), a file whose latest text
/// in that turn doesn't have the name isn't read at all: it has no uses from any inputs. Every other symbol reads every
/// file. Signature files are always read: their checks are made anyway.
/// </para>
/// <para>
/// Where FCS answers otherwise, counted in the ReferencesGrid's limit list: a key two definitions share answers nothing;
/// a parameter's name in a signature isn't found, as it has no key; a member more
/// generic than its signature has a key each side of it, and answers nothing; a parameter's named uses in other files
/// aren't found; a slot's property with both accessors and what implements it are told apart otherwise than FCS does; a
/// call of an overloaded constructor doesn't add that overload's `new`. A file linked into two projects finds the asking
/// project's copy only, as the key names the assembly. A cursor in a signature file answers nothing.
/// </para>
/// </remarks>
module References =
    /// What the workspace says about the projects, passed in so a test can run on graphs alone
    [<NoComparison; NoEquality>]
    type Scope =
        { OwnersOf: string -> ProjectGraph list
          ReferencingProjects: ProjectGraph -> ProjectGraph list
          Projects: unit -> ProjectGraph list
          FilesOf: ProjectGraph -> string list
          /// A load or reload is under way: the projects listed may lack those it hides
          IsLoading: unit -> bool }

    /// A use, and whether it is a definition
    type Use = (struct (range * bool))

    [<RequireQualifiedAccess; NoComparison>]
    type Answer =
        | Found of Use list
        /// A project went away, or the workspace is loading
        | Moved

    [<NoComparison; NoEquality>]
    type private Asked =
        | Nothing
        /// No key: the row's own uses, read from the file's result
        | InFile of Use list
        /// `spelled`: the name every use of it is written with in its file, when that can be shown
        | Keyed of key: string * landed: string option * spelled: string option

    let private isSignature (file: string) = file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

    let internal keyOf (symbol: FSharpSymbol) : string option = SymbolUses.keyOf symbol

    let internal keysIn (check: FSharpCheckFileResults) (ct: CancellationToken) : FSharpSymbolUse -> string option = SymbolUses.keysIn check ct

    let private ask (check: FSharpCheckFileResults) (line: int) (lineStr: string, names: string list, endColumn: int) (ct: CancellationToken) : Asked =
        match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
        | None -> Nothing
        | Some u ->
            match keysIn check ct u with
            | Some key ->
                let landed = u.Symbol.SignatureLocation |> Option.orElse u.Symbol.DeclarationLocation |> Option.map _.FileName
                // The token asked at is one use: one written otherwise says the kind's rule doesn't hold here
                let spelled = SymbolUses.spelledName u.Symbol |> Option.filter (fun name -> List.tryLast names = Some name)
                Keyed(key, landed, spelled)
            // Its method is another file's: this file's result knows only the argument
            | None when (match u.Symbol with
                         | :? FSharpParameter as p -> not (String.Equals(p.DeclarationLocation.FileName, u.FileName, StringComparison.OrdinalIgnoreCase))
                         | _ -> false) ->
                Nothing
            | None -> InFile [ for d in check.GetUsesOfSymbolInFile(u.Symbol, cancellationToken = ct) -> struct (d.Range, d.IsFromDefinition) ]

    /// The symbol under the cursor: a request node on the file (ProjectGraph.ReadFile), with only the check asked under
    /// the lock.
    let private asked (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) =
        graph.ReadFile(
            path,
            (fun view -> Hover.lookupOf view.Text line character),
            (fun view lookup ->
                match lookup with
                | Some l -> struct (ask view.Result.CheckResults line l ct, view.Version, lookup)
                | None -> struct (Nothing, view.Version, lookup)),
            ct
        )

    let private setterSuffix = "|setter"

    let private withoutSetter (key: string) =
        if key.EndsWith setterSuffix then key.Substring(0, key.Length - setterSuffix.Length) else key

    /// Whether a definition with a key is the only one of that key.
    let internal definesItsKey (symbol: FSharpSymbol) = SymbolUses.definesItsKey symbol

    /// What a file's uses say of a key and of its setter's, the same symbol's two tokens: each hit with whether its key is
    /// the setter's, and where each definition here says it is, with the same. The location can be stale: it tells
    /// definitions apart and names a signature.
    let private hitsOf (key: string) (uses: FileUses) =
        let plain = withoutSetter key
        let own = uses.Of plain
        let setter = uses.Of(plain + setterSuffix)

        Array.append
            (own.Hits |> Array.map (fun (struct (r, d)) -> struct (r, d, false)))
            (setter.Hits |> Array.map (fun (struct (r, d)) -> struct (r, d, true))),
        Array.append (own.Definitions |> Array.map (fun r -> struct (r, false))) (setter.Definitions |> Array.map (fun r -> struct (r, true)))

    /// Whether more than one definition has this key, which only a file that doesn't compile, or private types of one
    /// name in two files, can have: a property's accessors are one definition, and every fragment declares its namespace.
    /// Across files only the location's file counts: `type C with` in another file or project holds a kept copy of C, whose
    /// location names the right file at an old line.
    let private isTied (key: string) (definitionsByFile: (struct (range * bool))[] list) =
        match key.Split('|')[1] with
        | "property"
        | "namespace"
        | "anonymous" -> false
        | _ ->
            let inOneFile (definitions: (struct (range * bool))[]) =
                (definitions |> Seq.map (fun (struct (r, _)) -> r) |> Seq.distinct |> Seq.length) > 1

            List.exists inOneFile definitionsByFile
            || (definitionsByFile
                |> Seq.concat
                |> Seq.map (fun (struct (r, _)) -> r.FileName)
                |> Seq.distinct
                |> Seq.length) > 1

    /// The answer, and each file the turn read at its version (empty for Moved)
    let private everywhere
        (scope: Scope)
        (graph: ProjectGraph)
        (path: string)
        (key: string)
        (landed: string option)
        (spelled: string option)
        (ct: CancellationToken)
        : Task<struct (Answer * LastState.FileAt[])> =
        task {
            let definersNow () =
                match landed with
                | Some file -> scope.OwnersOf file
                | None -> []

            // Asked again at each turn the read tries, on the graph thread: the answer is the projects' at that turn
            // The definers are the scope's Extra: the answer reads them, so they must be the agreeing turn's
            let turnScope () : TurnScope<ProjectGraph list> =
                if scope.IsLoading() then
                    { Loading = true; Projects = [||]; Name = None; Extra = [] }
                else
                    let definers = definersNow ()

                    let projects =
                        match definers with
                        // No loaded project defines it: every project can use it
                        | [] -> scope.Projects()
                        | definers -> definers @ (definers |> List.collect scope.ReferencingProjects)
                        // The asking file's project as it is now: a reload replaces the graph the request started on
                        |> fun found -> (match scope.OwnersOf path with | g :: _ -> g | [] -> graph) :: found
                        |> List.distinct

                    { Loading = false
                      Projects = [| for p in projects -> p, scope.FilesOf p |]
                      // FSharp.Core's are reached from no text as well (SymbolUses.spelledName)
                      Name = if definers.IsEmpty then None else spelled
                      Extra = definers }

            match! graph.ReadInOneTurn(turnScope, ct) with
            | TurnRead.Gone -> return struct (Answer.Moved, [||])
            | TurnRead.Read(definers, projects) ->
                let files = projects |> Seq.collect snd |> List.ofSeq

                let read =
                    [| for g, turnFiles in projects do
                           for f in turnFiles do
                               match f with
                               | TurnFile.Indexed(p, v, _)
                               | TurnFile.Signature(p, v, _)
                               | TurnFile.Skipped(p, v) -> LastState.fileAt g p v |]

                let found =

                    let kept =
                        [ for f in files do
                              match f with
                              | TurnFile.Indexed(_, _, uses) -> yield hitsOf key uses
                              | _ -> () ]

                    let signatureHits =
                        [ for f in files do
                              match f with
                              | TurnFile.Signature(_, _, uses) -> yield! fst (hitsOf key uses)
                              | _ -> () ]

                    let definitions = kept |> List.collect (snd >> List.ofArray)
                    let isMember = (match key.Split('|')[1] with "member" | "property" -> true | _ -> false)

                    // A member more generic than its signature has a key each side of it. From a use after the signature its
                    // definer shows no definition; from the implementation, a signature it says it is in has no such key.
                    let moreGeneric =
                        isMember
                        && (if definitions.IsEmpty then
                                not definers.IsEmpty
                            else
                                definitions |> List.exists (fun (struct (r, _)) -> isSignature r.FileName)
                                && not (signatureHits |> List.exists (fun (struct (_, definition, _)) -> definition)))

                    if moreGeneric || isTied key (kept |> List.map snd) then
                        Answer.Found []
                    else
                        // A setter's own token is its own symbol, unless it is all the property has: then it is the property
                        let wantsSetter = key.EndsWith setterSuffix
                        let onlySetter = definitions |> List.forall (fun (struct (_, setter)) -> setter)

                        let hits =
                            [ for hits, _ in kept do
                                  for struct (r, definition, setter) in hits do
                                      if onlySetter || setter = wantsSetter then
                                          yield struct (r, definition) ]

                        let inSignatures =
                            [ for struct (r, definition, setter) in signatureHits do
                                  if onlySetter || setter = wantsSetter then
                                      yield struct (r, definition) ]

                        Answer.Found(List.distinct (hits @ inSignatures))

                return struct (found, read)
        }

    /// Every use of the symbol at the position, and the turn's files when the answer can be given again from them
    let private findIn (scope: Scope) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) =
        task {
            try
                match! asked graph path line character ct with
                | struct (Nothing, _, _) -> return struct (Answer.Found [], ValueNone)
                | struct (InFile uses, _, _) -> return struct (Answer.Found uses, ValueNone)
                | struct (Keyed(key, landed, spelled), version, lookup) ->
                    let! struct (found, read) = everywhere scope graph path key landed spelled ct

                    // The asking file in the turn must be at the text the symbol was asked of
                    let asking =
                        read
                        |> Array.tryFindIndex (fun f ->
                            String.Equals(Path.GetFullPath f.Path, Path.GetFullPath path, StringComparison.OrdinalIgnoreCase)
                            && (match f.Graph.TryGetTarget() with
                                | true, g -> obj.ReferenceEquals(g, graph)
                                | _ -> false))

                    match found, asking, lookup with
                    | Answer.Found _, Some i, Some lookup when read[i].Version = version ->
                        let files = Array.append [| read[i] |] (read |> Array.removeAt i)
                        let entry: LastState.Entry<Answer> = { Files = files; Answer = found }
                        return struct (found, ValueSome(struct (LastState.keyOf path version line lookup, entry)))
                    | _ -> return struct (found, ValueNone)
            with
            | :? OperationCanceledException when not ct.IsCancellationRequested -> return struct (Answer.Moved, ValueNone)
            // A file's #line directives stayed another parse's for as long as building its index waits for them
            | :? SupersededLineDirectivesException when not ct.IsCancellationRequested -> return struct (Answer.Moved, ValueNone)
        }

    /// Every use of the symbol at the position.
    let find (scope: Scope) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<Answer> =
        task {
            let! struct (found, _) = findIn scope graph path line character ct
            return found
        }

    /// The settled answers references gave, for answers from the last state
    type LastAnswers = LastState.Cache<Answer>

    let private remapUses (remap: LastState.Remap) (answer: Answer) : Result<Answer, Refusal> =
        match answer with
        | Answer.Moved -> Error Refusal.Moved
        | Answer.Found uses ->
            let mapped = uses |> List.map (fun (struct (r, d)) -> remap r |> Result.map (fun r -> struct (r, d)))

            match mapped |> List.tryPick (function Error e -> Some e | Ok _ -> None) with
            | Some refusal -> Error refusal
            | None -> Ok(Answer.Found(mapped |> List.choose (function Ok u -> Some u | Error _ -> None)))

    /// <summary>
    /// The uses from the last state while anything is pending (LastState.answer): the settled answer asked at the place
    /// the cursor had, each use mapped to the latest texts, or a refusal. Reads no check.
    /// </summary>
    let internal fromLastState
        (answers: LastAnswers)
        (shapes: Hover.LastAnswers)
        (busy: unit -> bool)
        (pending: unit -> bool)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<LastState.Outcome<Answer>> =
        LastState.answer answers shapes busy pending graph path line character remapUses ct

    /// <summary>
    /// Every use of the symbol at the position: the settled answer, given to `answers` too, or one from the last state
    /// (fromLastState) while the settled one hasn't completed. Only for a client that shows the server's progress, which
    /// is what marks such an answer.
    /// </summary>
    let answer
        (answers: LastAnswers)
        (shapes: Hover.LastAnswers)
        (busy: unit -> bool)
        (pending: unit -> bool)
        (scope: Scope)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<Answer> =
        LastState.race
            (fun ct ->
                task {
                    let! struct (found, entry) = findIn scope graph path line character ct

                    match entry with
                    | ValueSome(struct (key, e)) -> answers.Add(key, e)
                    | ValueNone -> ()

                    return found
                })
            (fromLastState answers shapes busy pending graph path line character)
            ct
