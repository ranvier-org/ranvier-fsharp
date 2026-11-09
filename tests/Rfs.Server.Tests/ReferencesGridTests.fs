module Rfs.Server.Tests.ReferencesGridTests

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Server
open Rfs.Server.Tests.TokenGrid

/// How one token compares with FCS
type private Outcome =
    | Same
    | Limit of string
    | Different of string

let private showRange (file: string) (r: range) =
    sprintf "%s %d,%d-%d,%d" (Path.GetFileName file) r.StartLine r.StartColumn r.EndLine r.EndColumn

let private show (uses: Set<string>) = String.Join("; ", uses)

let private oursOf (answer: References.Answer) =
    match answer with
    | References.Answer.Moved -> None
    | References.Answer.Found uses -> Some(uses |> List.map (fun (struct (r, d)) -> showRange r.FileName r, d))

/// One use of what FCS answers
type private FcsUse =
    { Range: string
      IsDefinition: bool
      InSignature: bool
      InOtherFile: bool
      IsConstructor: bool
      /// The keys of what stands at this range in its own file: FCS gives a use the symbol it was asked about
      Keys: Set<string> }

/// Why ours may differ from FCS at a token, the grid's limit list:
/// - "tie": the key has two definitions, and the answer is nothing;
/// - "use in a signature": a signature file is checked with no sink, so all that is known of it is what it defines. A
///   pattern's case there is one of these, as the signature defines the pattern and not the case, and a constructor's
///   `new`, which FCS counts as a use of its type there;
/// - "constructor's new": FCS at a call of an overloaded constructor adds that overload's `new`;
/// - "namespace": a namespace in a signature file, whose header says so with no definition to look up, and likewise a
///   type another assembly defines, extended in a signature file;
/// - "signed parameter": the parameter's name in the signature, for a parameter with no key;
/// - "named argument": a parameter used by name in another file than the one asked, as nothing ties the two by key;
/// - "generic overload": a member more generic than its signature has another key than the signature's, which later files
///   use, so the uses on the other side of the signature are not found;
/// - "interface property": a slot's property and what implements it share a key, where FCS tells `i.P` from the slot's
///   accessors, so ours has `i.P` at the implementation's name, or lacks the implementation's name at the slot.
let private limitOf (asked: FSharpSymbolUse) (key: string option) (tied: bool) (ourAll: Set<string>) (expected: FcsUse list) (extra: bool) =
    let missing = expected |> List.filter (fun u -> not (ourAll.Contains u.Range))

    let isExternal =
        match asked.Symbol with
        | :? FSharpEntity as e -> e.IsNamespace || not (e.DeclarationLocation.FileName.Contains "rfs-token-grid-")
        | _ -> false

    let declared = ourAll |> Set.exists (fun at -> at.Contains ".fsi ")
    let isSlotProperty = key |> Option.exists (fun k -> k.Contains "|property|" && k.Contains "|slot|")
    let isOtherKey (u: FcsUse) = key |> Option.exists (fun k -> not (u.Keys.Contains k))

    if isSlotProperty then
        Some "interface property"
    elif extra || missing.IsEmpty then
        None
    elif tied && ourAll.IsEmpty then
        Some "tie"
    elif key.IsSome && (asked.Symbol :? FSharpMemberOrFunctionOrValue) && missing |> List.exists isOtherKey then
        Some "generic overload"
    // What a signature says of a symbol it declares, past the declaration: an augmentation's header counts as a definition there
    elif key.IsSome && missing |> List.forall (fun u -> u.InSignature && (not u.IsDefinition || u.IsConstructor || declared || (asked.Symbol :? FSharpActivePatternCase))) then
        Some "use in a signature"
    elif key.IsSome && missing |> List.forall (fun u -> u.IsConstructor || (u.InSignature && declared)) then
        Some "constructor's new"
    elif key.IsSome && isExternal && missing |> List.forall _.InSignature then
        Some "namespace"
    elif key.IsNone && missing |> List.forall _.InSignature then
        Some "signed parameter"
    elif key.IsNone && missing |> List.forall (fun u -> u.InSignature || u.InOtherFile) then
        Some "named argument"
    else
        None

/// The keys two definitions share, in FCS's own check
let private tiesOf (checks: FSharpCheckFileResults seq) =
    let definitions =
        checks
        |> Seq.collect (fun c -> c.GetAllUsesOfAllSymbolsInFile())
        |> Seq.filter (fun u -> u.IsFromDefinition && References.definesItsKey u.Symbol)
        |> Seq.choose (fun u -> References.keyOf u.Symbol |> Option.map (fun k -> k, u.Symbol.SignatureLocation))
        |> Seq.distinct
        |> Seq.groupBy fst

    set [ for k, group in definitions do if Seq.length group > 1 then yield k ]

/// Every token of every implementation file of one state, against the union of FCS's uses over the files
let private compareState (state: State) : Task<Outcome list> =
    task {
        let outcomes = ResizeArray<Outcome>()
        let checks = Dictionary<string, FSharpCheckFileResults>()

        for file in state.Files do
            let! check = state.Reference file
            checks[file] <- check

        let ties = tiesOf checks.Values
        let keysAt = Dictionary<string, Set<string>>()
        let constructors = HashSet<string>()

        for KeyValue(f, check) in checks do
            for u in check.GetAllUsesOfAllSymbolsInFile() do
                match u.Symbol with
                | :? FSharpMemberOrFunctionOrValue as v when v.IsConstructor -> constructors.Add(showRange f u.Range) |> ignore
                | _ -> ()

                match References.keyOf u.Symbol with
                | Some k ->
                    let at = showRange f u.Range
                    keysAt[at] <- (match keysAt.TryGetValue at with | true, s -> s.Add k | _ -> Set.singleton k)
                | None -> ()

        let scope: References.Scope =
            { OwnersOf = state.OwnersOf
              ReferencingProjects = fun _ -> []
              Projects = fun () -> [ state.Graph ]
              FilesOf = fun _ -> state.Files }

        for file in state.Implementations do
            for token in tokensOf (state.TextOf file) do
                let! actual = References.find scope state.Graph file token.Line token.Character CancellationToken.None
                let where = $"%s{state.Scenario} / %s{state.Step}: %s{Path.GetFileName file} %d{token.Line + 1},%d{token.EndColumn} %A{token.Names} [%s{token.LineText.Trim()}]"

                match checks[file].GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names), oursOf actual with
                | _, None -> outcomes.Add(Different $"%s{where}: Moved with nothing moving")
                | None, Some [] -> outcomes.Add Same
                | None, Some ours -> outcomes.Add(Different $"%s{where}: FCS has no symbol, ours %A{ours}")
                | Some asked, Some ours ->
                    let expected =
                        [ for f in state.Files do
                              for u in checks[f].GetUsesOfSymbolInFile asked.Symbol do
                                  { Range = showRange f u.Range
                                    IsDefinition = u.IsFromDefinition
                                    InSignature = f.EndsWith ".fsi"
                                    InOtherFile = not (String.Equals(f, file, StringComparison.OrdinalIgnoreCase))
                                    IsConstructor = constructors.Contains(showRange f u.Range)
                                    Keys = (match keysAt.TryGetValue(showRange f u.Range) with | true, k -> k | _ -> Set.empty) } ]

                    let all (uses: (string * bool) list) = uses |> List.map fst |> Set.ofList
                    let notDeclared (uses: (string * bool) list) = uses |> List.filter (snd >> not) |> all
                    let ourAll = all ours
                    let fcsAll = expected |> List.map _.Range |> Set.ofList
                    let ourUses = notDeclared ours
                    let fcsUses = expected |> List.filter (fun u -> not u.IsDefinition) |> List.map _.Range |> Set.ofList
                    let key = References.keyOf asked.Symbol
                    let tied = key |> Option.exists ties.Contains

                    if ourAll = fcsAll && ourUses = fcsUses then
                        outcomes.Add Same
                    else
                        let extra = not ((ourAll - fcsAll).IsEmpty && (ourUses - fcsUses).IsEmpty)

                        match limitOf asked key tied ourAll expected extra with
                        | Some limit -> outcomes.Add(Limit limit)
                        | None ->
                            outcomes.Add(Different $"%s{where} {asked.Symbol.GetType().Name}: ours only %s{show (ourAll - fcsAll)} | FCS only %s{show (fcsAll - ourAll)} | undeclared ours only %s{show (ourUses - fcsUses)} | FCS only %s{show (fcsUses - ourUses)}")

        return List.ofSeq outcomes
    }

let private expectGrid (promote: bool) (scenario: Scenario) =
    task {
        let! outcomes = run promote scenario compareState
        Expect.isGreaterThan outcomes.Length 0 "tokens"
        let different = outcomes |> List.choose (function Different d -> Some d | _ -> None)
        let limits = outcomes |> List.choose (function Limit l -> Some l | _ -> None) |> List.countBy id
        let shown = different |> List.truncate 25 |> String.concat "\n  "
        printfn "references grid %s (%s): %d tokens, %d differ, limits %A" scenario.Name (if promote then "by declaration" else "file level") outcomes.Length different.Length limits
        Expect.isTrue different.IsEmpty $"%d{different.Length} of %d{outcomes.Length} tokens differ from FCS:\n  %s{shown}"
    }

/// App on Lib in memory, at every token of both. FCS can't check App on Lib's unbuilt text, so what stands for it is the
/// key: the uses in Lib are what FCS on Lib has with that key, at Lib's current text, and the uses in App what App's
/// result has with that key. A signature file's part is left to the grids above. A token with no key stays in its file.
let private expectAcrossProjects (lib: Scenario) (appText: string) =
    task {
        let! outcomes =
            runReferencing lib appText (fun state app appFile ->
                task {
                    let checks = Dictionary<string, FSharpCheckFileResults>()

                    for f in state.Files do
                        let! check = state.Reference f
                        checks[f] <- check

                    let usesOf = Dictionary<string, HashSet<string>>()

                    let addUse (key: string) (at: string) =
                        match usesOf.TryGetValue key with
                        | true, found -> found.Add at |> ignore
                        | _ -> usesOf[key] <- HashSet [ at ]

                    for f in state.Implementations do
                        let keys = References.keysIn checks[f] CancellationToken.None

                        for u in checks[f].GetAllUsesOfAllSymbolsInFile() do
                            keys u |> Option.iter (fun k -> addUse k (showRange f u.Range))

                    let! (appUses: (string * string) list) =
                        app.ReadFile(
                            appFile,
                            (fun view ->
                                let keys = References.keysIn view.Result.CheckResults CancellationToken.None

                                [ for u in view.Result.CheckResults.GetAllUsesOfAllSymbolsInFile() do
                                      match keys u with
                                      | Some k -> yield k, showRange appFile u.Range
                                      | None -> () ]),
                            CancellationToken.None
                        )

                    for k, at in appUses do
                        addUse k at

                    let ties = tiesOf checks.Values

                    let scope: References.Scope =
                        { OwnersOf = state.OwnersOf
                          ReferencingProjects = fun g -> if obj.ReferenceEquals(g, state.Graph) then [ app ] else []
                          Projects = fun () -> [ state.Graph; app ]
                          FilesOf = fun g -> if obj.ReferenceEquals(g, state.Graph) then state.Files else [ appFile ] }

                    let expectedOf (key: string) =
                        match usesOf.TryGetValue key with
                        | true, found -> Set.ofSeq found
                        | _ -> Set.empty

                    let found = ResizeArray<Outcome>()

                    let compare (graph: Rfs.Checker.ProjectGraph) (file: string) (token: Token) (key: string option) =
                        task {
                            let! actual = References.find scope graph file token.Line token.Character CancellationToken.None
                            let where = $"%s{lib.Name} / %s{state.Step}: %s{Path.GetFileName file} %d{token.Line + 1},%d{token.EndColumn} %A{token.Names} [%s{token.LineText.Trim()}]"

                            match oursOf actual, key with
                            | None, _ -> found.Add(Different $"%s{where}: Moved with nothing moving")
                            | Some ours, None ->
                                let here = showRange file Range.range0
                                let name = here.Substring(0, here.IndexOf ' ')

                                if ours |> List.forall (fun (at, _) -> at.StartsWith(name + " ")) then
                                    found.Add Same
                                else
                                    found.Add(Different $"%s{where}: no key, and ours %A{ours}")
                            | Some ours, Some k ->
                                let ourAll = ours |> List.map fst |> List.filter (fun at -> not (at.Contains ".fsi ")) |> Set.ofList
                                let expected = expectedOf k
                                // The setter's own token is one symbol with its property when it is all the property has
                                let plain = if k.EndsWith "|setter" then k.Substring(0, k.Length - 7) else k
                                let either = Set.union (expectedOf plain) (expectedOf (plain + "|setter"))

                                if ourAll.IsEmpty && ties.Contains k then
                                    found.Add(Limit "tie")
                                // Here both assemblies have one name. Elsewhere the key's assembly tells two projects' namespaces apart
                                elif k.Split('|')[1] = "namespace" && Set.isSubset ourAll expected then
                                    found.Add(Limit "namespace")
                                elif Set.isSubset expected ourAll && Set.isSubset ourAll either then
                                    found.Add Same
                                else
                                    found.Add(Different $"%s{where}: ours only %s{show (ourAll - either)} | expected only %s{show (expected - ourAll)}")
                        }

                    for token in tokensOf appText do
                        let! (key: string option) =
                            app.ReadFile(
                                appFile,
                                (fun view ->
                                    let check = view.Result.CheckResults

                                    check.GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names)
                                    |> Option.bind (References.keysIn check CancellationToken.None)),
                                CancellationToken.None
                            )

                        do! compare app appFile token key

                    for file in state.Implementations do
                        for token in tokensOf (state.TextOf file) do
                            let check = checks[file]

                            let key =
                                check.GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names)
                                |> Option.bind (References.keysIn check CancellationToken.None)

                            do! compare state.Graph file token key

                    return List.ofSeq found
                })

        let different = outcomes |> List.choose (function Different d -> Some d | _ -> None)
        let limits = outcomes |> List.choose (function Limit l -> Some l | _ -> None) |> List.countBy id
        printfn "references grid %s, App on Lib: %d tokens, %d differ, limits %A" lib.Name outcomes.Length different.Length limits
        let shown = different |> List.truncate 25 |> String.concat "\n  "
        Expect.isTrue different.IsEmpty $"%d{different.Length} of %d{outcomes.Length} tokens differ:\n  %s{shown}"
    }

[<Tests>]
let tests =
    testList
        "ReferencesGrid"
        [ testTask "App on Lib, unsigned" { do! expectAcrossProjects DefinitionGridTexts.libUnsigned DefinitionGridTexts.uses }
          testTask "App on Lib, signed" { do! expectAcrossProjects DefinitionGridTexts.libSigned DefinitionGridTexts.uses }
          testList "by declaration" [ for s in DefinitionGridTexts.all -> testTask s.Name { do! expectGrid true s } ]
          testList "file level" [ for s in DefinitionGridTexts.batteries -> testTask s.Name { do! expectGrid false s } ] ]
