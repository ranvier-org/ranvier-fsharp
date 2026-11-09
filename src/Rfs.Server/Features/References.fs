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
/// check's (ProjectGraph.ReadSignature), so a blank line in it moves them.
/// </para>
/// <para>
/// A symbol with no key (a local, a parameter) answers its uses in the asking file's result, in one read, so a parameter's
/// named uses in other files aren't found. A named argument answers the same in its method's file, and nothing elsewhere:
/// that file's result has neither its definition nor its other uses.
/// </para>
/// <para>
/// Consistency: each file is read alone, holding nothing while another is waited for. The files are read until a whole
/// pass finds, for every file, the same result object as the pass before it, or the same uses from the same inputs: the
/// same context and signatures (FileView.Upstream), at the same text version or the next one. A file's uses are a function
/// of those and its text. So between its two reads each file's inputs went through at most one edit of its own text, both
/// of its texts having these uses: at the moment between the passes each file stood at one of them, and the answer is
/// that moment's. A signature file holds still when its check is the same object. An edit that moves no use and changes no
/// signature a file reads doesn't hold the answer up. More than one edit of a file between its reads does, as nothing was
/// read of the texts between; so does a changed signature a file reads, even when its uses come out the same, as they
/// could differ in a state between that no read saw. That rule rests on FileSig.Next keeping a signature's object only
/// while nothing a later file reads of it changed; it is shown on one three-file schedule and in the re-review's model.
/// Where edits keep this from happening in a few passes, or a project goes away, or the workspace is loading at either end
/// of the request, or the projects to read are others by its end (a reload hides the project and those that reference it
/// from the snapshot), the answer is Moved, never a mix or a partial list.
/// The handler gives LSP's ContentModified, and Ionide's bundled client then shows no results and doesn't ask again: to
/// the user it looks like no references.
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
          IsLoading: unit -> bool
          /// Runs before each implementation file's read. For tests
          BeforeRead: string -> Task }

    /// A use, and whether it is a definition
    type Use = (struct (range * bool))

    [<RequireQualifiedAccess; NoComparison>]
    type Answer =
        | Found of Use list
        /// No moment was seen at which what the files say held still, or a project went away or is loading
        | Moved

    [<NoComparison; NoEquality>]
    type private Asked =
        | Nothing
        /// No key: the row's own uses, read from the file's result
        | InFile of Use list
        | Keyed of key: string * landed: string option

    let private isSignature (file: string) = file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)

    /// What FCS counts as one symbol, as far as a key can say it: a constructor's uses are its type's; what a key sends to
    /// its case or type (a union's `IsCase`, a generated member) is a symbol of its own; a pattern's cases are told apart.
    let internal keyOf (symbol: FSharpSymbol) : string option =
        try
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as v when v.IsConstructor ->
                v.DeclaringEntity |> Option.bind (fun e -> DeclarationChecker.SymbolKey e) |> Option.orElse (DeclarationChecker.SymbolKey symbol)
            | :? FSharpMemberOrFunctionOrValue as v ->
                DeclarationChecker.SymbolKey symbol
                |> Option.map (fun key ->
                    match key.Split('|')[1] with
                    | "case"
                    | "type" -> key + "|" + v.LogicalName
                    | _ -> key)
            | :? FSharpActivePatternCase as c -> DeclarationChecker.SymbolKey symbol |> Option.map (fun key -> key + "|" + c.Name)
            // One symbol by its names and place wherever it is written, with no definition of its own
            | :? FSharpField as f when f.IsAnonRecordField ->
                let details, _, index = f.AnonRecordFieldDetails
                let names = String.concat "," details.SortedFieldNames
                Some $"%s{symbol.Assembly.SimpleName}|anonymous|%s{names}|%d{index}"
            | _ -> DeclarationChecker.SymbolKey symbol
        with :? InvalidOperationException ->
            None

    /// Keys of a file's uses. A pattern's case inside its own definition names no value: it is the case of the last pattern
    /// of its name defined before it in the file.
    let internal keysIn (check: FSharpCheckFileResults) (ct: CancellationToken) : FSharpSymbolUse -> string option =
        let patterns =
            lazy
                (check.GetAllUsesOfAllSymbolsInFile ct
                 |> Seq.choose (fun u ->
                     match u.Symbol with
                     | :? FSharpMemberOrFunctionOrValue as v when u.IsFromDefinition && v.IsActivePattern ->
                         DeclarationChecker.SymbolKey u.Symbol |> Option.map (fun key -> u.Range, v.LogicalName.Split('|'), key)
                     | _ -> None)
                 |> Seq.toArray)

        fun (u: FSharpSymbolUse) ->
            match keyOf u.Symbol, u.Symbol with
            // The `new` of a constructor is that constructor's, where its calls are its type's
            | _, (:? FSharpMemberOrFunctionOrValue as v) when u.IsFromDefinition && v.IsConstructor -> DeclarationChecker.SymbolKey u.Symbol
            | None, (:? FSharpActivePatternCase as c) ->
                patterns.Value
                |> Array.filter (fun (r, names, _) -> Array.contains c.Name names && Position.posGeq u.Range.Start r.Start)
                |> Array.tryLast
                |> Option.map (fun (_, _, key) -> key + "|" + c.Name)
            | key, _ -> key

    let private ask (check: FSharpCheckFileResults) (line: int) (lineStr: string, names: string list, endColumn: int) (ct: CancellationToken) : Asked =
        match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
        | None -> Nothing
        | Some u ->
            match keysIn check ct u with
            | Some key ->
                let landed = u.Symbol.SignatureLocation |> Option.orElse u.Symbol.DeclarationLocation |> Option.map _.FileName
                Keyed(key, landed)
            // Its method is another file's: this file's result knows only the argument
            | None when (match u.Symbol with
                         | :? FSharpParameter as p -> not (String.Equals(p.DeclarationLocation.FileName, u.FileName, StringComparison.OrdinalIgnoreCase))
                         | _ -> false) ->
                Nothing
            | None -> InFile [ for d in check.GetUsesOfSymbolInFile(u.Symbol, cancellationToken = ct) -> struct (d.Range, d.IsFromDefinition) ]

    /// The symbol under the cursor. Only the check is asked inside the read; a read that finds another text than the name
    /// was looked up in looks it up again.
    let private asked (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<Asked> =
        task {
            let mutable looked = None
            let mutable found = ValueNone

            while found.IsNone do
                let current = looked

                let! answer =
                    graph.ReadFile(
                        path,
                        (fun view ->
                            match current with
                            | Some(text, lookup) when obj.ReferenceEquals(text, view.Text) -> Choice1Of2(ask view.Result.CheckResults line lookup ct)
                            | _ -> Choice2Of2 view.Text),
                        ct
                    )

                match answer with
                | Choice1Of2 a -> found <- ValueSome a
                | Choice2Of2 text ->
                    match Hover.lookupOf text line character with
                    | Some lookup -> looked <- Some(text, lookup)
                    | None -> found <- ValueSome Nothing

            return found.Value
        }

    [<NoComparison; NoEquality>]
    type private Scanned =
        { /// The result it was read from: the same object has the same uses
          Result: obj
          /// The text version it was read at
          Version: int64
          /// What the result read besides the text
          Upstream: obj[]
          /// With whether its key is the setter's
          Hits: (struct (range * bool * bool))[]
          /// Where each definition here says it is, with whether its key is the setter's. The location can be stale: it
          /// tells definitions apart and names a signature
          Definitions: (struct (range * bool))[] }

    let private setterSuffix = "|setter"

    let private withoutSetter (key: string) =
        if key.EndsWith setterSuffix then key.Substring(0, key.Length - setterSuffix.Length) else key

    /// Whether a definition with a key is the only one of that key. A type has its constructors' key, and many of them; a
    /// pattern's case says where its use is, not where it is defined.
    let internal definesItsKey (symbol: FSharpSymbol) =
        match symbol with
        | :? FSharpActivePatternCase -> false
        | :? FSharpMemberOrFunctionOrValue as v -> not v.IsConstructor
        | _ -> true

    let private scan (key: string) (view: FileView) (ct: CancellationToken) : Scanned =
        let check = view.Result.CheckResults
        let keys = keysIn check ct
        let plain = withoutSetter key

        let found =
            check.GetAllUsesOfAllSymbolsInFile ct
            |> Seq.choose (fun u ->
                match keys u with
                | Some k when k = plain -> Some(u, false)
                | Some k when k = plain + setterSuffix -> Some(u, true)
                | _ -> None)
            |> Seq.toArray

        { Result = view.Result
          Version = view.Version
          Upstream = view.Upstream
          Hits = found |> Array.map (fun (u, setter) -> struct (u.Range, u.IsFromDefinition, setter))
          Definitions =
            found
            |> Array.filter (fun (u, _) -> u.IsFromDefinition && definesItsKey u.Symbol)
            |> Array.choose (fun (u, setter) ->
                u.Symbol.SignatureLocation |> Option.orElse u.Symbol.DeclarationLocation |> Option.map (fun r -> struct (r, setter))) }

    [<NoComparison; NoEquality>]
    type private ScannedSignature =
        { /// The check it was read from: the same object has the same uses
          Signature: obj
          Hits: (struct (range * bool * bool))[] }

    let private scanSignature (key: string) (signature: FileSignature) (uses: (struct (range * bool * FSharpSymbol))[]) (ct: CancellationToken) : ScannedSignature =
        let plain = withoutSetter key

        // As in an implementation file (keysIn): a pattern's case names no value in the signature that lists the pattern
        let patterns =
            uses
            |> Array.choose (fun (struct (r, definition, symbol)) ->
                match symbol with
                | :? FSharpMemberOrFunctionOrValue as v when definition && v.IsActivePattern ->
                    DeclarationChecker.SymbolKey symbol |> Option.map (fun k -> r, v.LogicalName.Split('|'), k)
                | _ -> None)

        { Signature = signature
          Hits =
            uses
            |> Array.choose (fun (struct (r, definition, symbol)) ->
                ct.ThrowIfCancellationRequested()

                let own =
                    match symbol with
                    | :? FSharpMemberOrFunctionOrValue as v when definition && v.IsConstructor -> DeclarationChecker.SymbolKey symbol
                    | :? FSharpActivePatternCase as c when (keyOf symbol).IsNone ->
                        patterns
                        |> Array.filter (fun (at, names, _) -> Array.contains c.Name names && Position.posGeq r.Start at.Start)
                        |> Array.tryLast
                        |> Option.map (fun (_, _, k) -> k + "|" + c.Name)
                    | _ -> keyOf symbol

                match own with
                | Some k when k = plain -> Some(struct (r, definition, false))
                | Some k when k = plain + setterSuffix -> Some(struct (r, definition, true))
                | _ -> None) }

    /// How many times every file is read before the answer is Moved. Two when nothing moves.
    let private maximumPasses = 5

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

    /// Whether the same uses from the same inputs, at the same or the next text version: see the remarks
    let private holdsStill (before: Scanned) (now: Scanned) =
        let sameUse (struct (a: range, b: bool, c: bool)) (struct (x: range, y: bool, z: bool)) = Range.equals a x && b = y && c = z
        let sameDefinition (struct (a: range, b: bool)) (struct (x: range, y: bool)) = Range.equals a x && b = y

        obj.ReferenceEquals(before.Result, now.Result)
        || (now.Version - before.Version <= 1L
            && before.Upstream.Length = now.Upstream.Length
            && Array.forall2 (fun (a: obj) (b: obj) -> obj.ReferenceEquals(a, b)) before.Upstream now.Upstream
            && before.Hits.Length = now.Hits.Length
            && before.Definitions.Length = now.Definitions.Length
            && Array.forall2 sameUse before.Hits now.Hits
            && Array.forall2 sameDefinition before.Definitions now.Definitions)

    let private sameFile (a: string) (b: string) =
        String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

    let private everywhere (scope: Scope) (graph: ProjectGraph) (key: string) (landed: string option) (ct: CancellationToken) : Task<Answer> =
        task {
            let wasLoading = scope.IsLoading()

            let definersNow () =
                match landed with
                | Some file -> scope.OwnersOf file
                | None -> []

            let projectsNow () =
                match definersNow () with
                // No loaded project defines it: every project can use it
                | [] -> scope.Projects()
                | definers -> definers @ (definers |> List.collect scope.ReferencingProjects)
                |> fun found -> graph :: found
                |> List.distinct
                |> Array.ofList

            let definers = definersNow ()
            let projects = projectsNow ()
            let gone = HashSet<int>()
            let mutable last = Dictionary<struct (int * string), Scanned>()
            let mutable lastSignatures = Dictionary<struct (int * string), ScannedSignature>()
            let mutable settled = false
            let mutable passes = 0

            while not wasLoading && not settled && passes < maximumPasses do
                passes <- passes + 1
                let previous = last
                let current = Dictionary<struct (int * string), Scanned>()
                let filesOf = projects |> Array.map scope.FilesOf

                for p in 0 .. projects.Length - 1 do
                    for file in filesOf[p] do
                        if not (isSignature file) && not (gone.Contains p) then
                            ct.ThrowIfCancellationRequested()

                            try
                                do! scope.BeforeRead file

                                let! scanned =
                                    projects[p].ReadFile(
                                        file,
                                        (fun view ->
                                            match previous.TryGetValue(struct (p, file)) with
                                            | true, s when obj.ReferenceEquals(s.Result, view.Result) -> s
                                            | _ -> scan key view ct),
                                        ct
                                    )

                                current[struct (p, file)] <- scanned
                            with :? OperationCanceledException when not ct.IsCancellationRequested ->
                                gone.Add p |> ignore

                let currentSignatures = Dictionary<struct (int * string), ScannedSignature>()

                for p in 0 .. projects.Length - 1 do
                    for file in filesOf[p] do
                        if isSignature file && not (gone.Contains p) then
                            ct.ThrowIfCancellationRequested()

                            try
                                let! scanned =
                                    projects[p].ReadSignature(
                                        file,
                                        (fun signature uses ->
                                            match lastSignatures.TryGetValue(struct (p, file)) with
                                            | true, s when obj.ReferenceEquals(s.Signature, signature) -> s
                                            | _ -> scanSignature key signature uses.Value ct),
                                        ct
                                    )

                                currentSignatures[struct (p, file)] <- scanned
                            with :? OperationCanceledException when not ct.IsCancellationRequested ->
                                gone.Add p |> ignore

                settled <-
                    passes > 1
                    && currentSignatures.Count = lastSignatures.Count
                    && currentSignatures
                       |> Seq.forall (fun (KeyValue(k, s)) ->
                           match lastSignatures.TryGetValue k with
                           | true, before -> obj.ReferenceEquals(before.Signature, s.Signature)
                           | _ -> false)
                    && current.Count = previous.Count
                    && current
                       |> Seq.forall (fun (KeyValue(k, s)) ->
                           match previous.TryGetValue k with
                           | true, before -> holdsStill before s
                           | _ -> false)

                last <- current
                lastSignatures <- currentSignatures

            let sameProjects () =
                let now = projectsNow ()
                now.Length = projects.Length && Array.forall2 (fun (a: ProjectGraph) (b: ProjectGraph) -> obj.ReferenceEquals(a, b)) now projects

            if not settled || gone.Count > 0 || scope.IsLoading() || not (sameProjects ()) then
                return Answer.Moved
            else
                let kept = List.ofSeq last.Values
                let definitions = kept |> List.collect (fun s -> List.ofArray s.Definitions)
                let isMember = (match key.Split('|')[1] with "member" | "property" -> true | _ -> false)
                let signatureHits = lastSignatures.Values |> Seq.collect _.Hits |> List.ofSeq

                // A member more generic than its signature has a key each side of it. From a use after the signature its
                // definer shows no definition; from the implementation, a signature it says it is in has no such key.
                let moreGeneric =
                    isMember
                    && (if definitions.IsEmpty then
                            not definers.IsEmpty
                        else
                            definitions |> List.exists (fun (struct (r, _)) -> isSignature r.FileName)
                            && not (signatureHits |> List.exists (fun (struct (_, definition, _)) -> definition)))

                if moreGeneric || isTied key (kept |> List.map _.Definitions) then
                    return Answer.Found []
                else
                    // A setter's own token is its own symbol, unless it is all the property has: then it is the property
                    let wantsSetter = key.EndsWith setterSuffix
                    let onlySetter = definitions |> List.forall (fun (struct (_, setter)) -> setter)

                    let hits =
                        [ for s in kept do
                              for struct (r, definition, setter) in s.Hits do
                                  if onlySetter || setter = wantsSetter then
                                      yield struct (r, definition) ]

                    let inSignatures =
                        [ for struct (r, definition, setter) in signatureHits do
                              if onlySetter || setter = wantsSetter then
                                  yield struct (r, definition) ]

                    return Answer.Found(List.distinct (hits @ inSignatures))
        }

    /// Every use of the symbol at the position.
    let find (scope: Scope) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<Answer> =
        task {
            try
                match! asked graph path line character ct with
                | Nothing -> return Answer.Found []
                | InFile uses -> return Answer.Found uses
                | Keyed(key, landed) -> return! everywhere scope graph key landed ct
            with
            | :? OperationCanceledException when not ct.IsCancellationRequested -> return Answer.Moved
            // A newer parse replaced the file's #line directives while its results were built: an edit got there first
            | :? SupersededLineDirectivesException when not ct.IsCancellationRequested -> return Answer.Moved
        }
