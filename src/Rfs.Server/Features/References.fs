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
/// App's. What a use's symbol says of its location is not read for an answer. A signature file has no result: its
/// declaration comes from ProjectGraph.CurrentLocation.
/// </para>
/// <para>
/// A symbol with no key (a local, a parameter) answers its uses in the asking file's result, in one read. A named argument
/// the same, so its parameter's uses in other files aren't found.
/// </para>
/// <para>
/// Consistency: each file is read alone, holding nothing while another is waited for. The files are read until a whole
/// pass finds every file on the result the pass before it read, and every signature's declaration where it was. Each
/// file stayed on that result between its two reads, and the first pass ended before the second began, so at that moment
/// all of them stood where the answer says. Where edits keep that from happening in a few passes the answer is Moved, never
/// a mix: the handler gives the client LSP's ContentModified.
/// </para>
/// <para>
/// Where FCS answers otherwise, counted in the ReferencesGrid's limit list: a key two definitions share answers nothing;
/// a use in a signature file other than its declaration isn't found, as CheckSignatureFile has no sink; a member more
/// generic than its signature has two keys, and finds one side; a named argument and its parameter find each other only in
/// one file; a slot's property and what implements it are one key; a call of an overloaded constructor doesn't add that
/// overload's `new`. A cursor in a signature file answers nothing.
/// </para>
/// </remarks>
module References =
    /// What the workspace says about the projects, passed in so a test can run on graphs alone
    [<NoComparison; NoEquality>]
    type Scope =
        { OwnersOf: string -> ProjectGraph list
          ReferencingProjects: ProjectGraph -> ProjectGraph list
          Projects: unit -> ProjectGraph list
          FilesOf: ProjectGraph -> string list }

    /// A use, and whether it is a definition
    type Use = (struct (range * bool))

    [<RequireQualifiedAccess; NoComparison>]
    type Answer =
        | Found of Use list
        /// Edits kept arriving: no moment was seen at which every file stood still
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

    let private scan (key: string) (result: obj) (check: FSharpCheckFileResults) (ct: CancellationToken) : Scanned =
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

        { Result = result
          Hits = found |> Array.map (fun (u, setter) -> struct (u.Range, u.IsFromDefinition, setter))
          Definitions =
            found
            |> Array.filter (fun (u, _) -> u.IsFromDefinition && definesItsKey u.Symbol)
            |> Array.choose (fun (u, setter) ->
                u.Symbol.SignatureLocation |> Option.orElse u.Symbol.DeclarationLocation |> Option.map (fun r -> struct (r, setter))) }

    /// How many times every file is read before the answer is Moved. Two when nothing moves.
    let private maximumPasses = 5

    /// Whether more than one definition has this key, which only a file that doesn't compile, or private types of one
    /// name in two files, can have: a property's accessors are one definition, and every fragment declares its namespace.
    let private isTied (key: string) (definitions: range seq) =
        match key.Split('|')[1] with
        | "property"
        | "namespace"
        | "anonymous" -> false
        | _ -> (definitions |> Seq.distinct |> Seq.length) > 1

    let private sameFile (a: string) (b: string) =
        String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)

    let private everywhere (scope: Scope) (graph: ProjectGraph) (key: string) (landed: string option) (ct: CancellationToken) : Task<Answer> =
        task {
            let definers =
                match landed with
                | Some file -> scope.OwnersOf file
                | None -> []

            let projects =
                match definers with
                // No loaded project defines it: every project can use it
                | [] -> scope.Projects()
                | _ -> definers @ (definers |> List.collect scope.ReferencingProjects)
                |> fun found -> graph :: found
                |> List.distinct
                |> Array.ofList

            let gone = HashSet<int>()
            let mutable last = Dictionary<struct (int * string), Scanned>()
            let mutable lastSignatures = Set.empty<string>
            let mutable signatureUses = []
            let mutable settled = false
            let mutable passes = 0

            while not settled && passes < maximumPasses do
                passes <- passes + 1
                let previous = last
                let current = Dictionary<struct (int * string), Scanned>()

                for p in 0 .. projects.Length - 1 do
                    for file in scope.FilesOf projects[p] do
                        if not (isSignature file) && not (gone.Contains p) then
                            ct.ThrowIfCancellationRequested()

                            try
                                let! scanned =
                                    projects[p].ReadFile(
                                        file,
                                        (fun view ->
                                            match previous.TryGetValue(struct (p, file)) with
                                            | true, s when obj.ReferenceEquals(s.Result, view.Result) -> s
                                            | _ -> scan key view.Result view.Result.CheckResults ct),
                                        ct
                                    )

                                current[struct (p, file)] <- scanned
                            with :? OperationCanceledException when not ct.IsCancellationRequested ->
                                gone.Add p |> ignore

                // A signature's declaration, read from its latest check: nothing else holds its current range
                let wanted =
                    [ for KeyValue(struct (p, file), s) in current do
                          for struct (d, _) in s.Definitions do
                              if isSignature d.FileName then
                                  yield p, file, d.FileName ]
                    |> List.distinctBy (fun (p, _, signature) -> p, signature)

                let landedSignature =
                    match landed with
                    | Some file when isSignature file && not (wanted |> List.exists (fun (_, _, s) -> sameFile s file)) ->
                        [ for p in 0 .. projects.Length - 1 do
                              if scope.FilesOf projects[p] |> List.exists (sameFile file) then
                                  yield p, file, file ]
                    | _ -> []

                let signatures = ResizeArray<Use>()

                for p, asking, signature in wanted @ landedSignature do
                    if not (gone.Contains p) then
                        ct.ThrowIfCancellationRequested()

                        try
                            let! location = projects[p].CurrentLocation(asking, signature, key, ct)

                            match location with
                            | Some r when isSignature r.FileName -> signatures.Add(struct (r, true))
                            | _ -> ()
                        with :? OperationCanceledException when not ct.IsCancellationRequested ->
                            gone.Add p |> ignore

                let now = signatures |> Seq.map (fun (struct (r, _)) -> r.ToString()) |> Set.ofSeq

                settled <-
                    passes > 1
                    && now = lastSignatures
                    && current.Count = previous.Count
                    && current
                       |> Seq.forall (fun (KeyValue(k, s)) ->
                           match previous.TryGetValue k with
                           | true, before -> obj.ReferenceEquals(before.Result, s.Result)
                           | _ -> false)

                last <- current
                lastSignatures <- now
                signatureUses <- List.ofSeq signatures

            if not settled then
                return Answer.Moved
            else
                let kept =
                    [ for KeyValue(struct (p, _), s) in last do
                          if not (gone.Contains p) then
                              yield s ]

                let definitions = kept |> List.collect (fun s -> List.ofArray s.Definitions)

                if isTied key (definitions |> Seq.map (fun (struct (r, _)) -> r)) then
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

                    return Answer.Found(List.distinct (hits @ signatureUses))
        }

    /// Every use of the symbol at the position.
    let find (scope: Scope) (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<Answer> =
        task {
            match! asked graph path line character ct with
            | Nothing -> return Answer.Found []
            | InFile uses -> return Answer.Found uses
            | Keyed(key, landed) -> return! everywhere scope graph key landed ct
        }
