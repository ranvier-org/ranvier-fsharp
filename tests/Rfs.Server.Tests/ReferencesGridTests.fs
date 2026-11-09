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
open Rfs.Checker
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

/// One use of what FCS answers
type private FcsUse =
    { Range: string
      IsDefinition: bool
      InSignature: bool
      InOtherFile: bool
      /// The text at the range
      Text: string
      /// A named argument's parameter
      IsArgument: bool
      /// The keys of what stands at this range in its own file: FCS gives a use the symbol it was asked about
      Keys: Set<string> }

let private plain (key: string) =
    if key.EndsWith "|setter" then key.Substring(0, key.Length - 7) else key

let private kindOf (key: string) = key.Split('|')[1]

/// Why ours may differ from FCS at a token, the grid's limit list. Each excuses exactly its own difference, as the review's
/// mutations of the answers showed a looser one hides whole classes of bug. The definition flag is compared wherever both
/// have a range, at every limit.
/// - "tie": the key has two definitions, and the answer is nothing;
/// - "generic overload": a member more generic than its signature has a key each side of it, and the answer is nothing;
/// - "generic overload's sibling": an overload exact in the signature, beside one more generic than it: FCS counts the
///   two as one, and ours has the exact one's only;
/// - "pattern's case": a case asked in its pattern's own definition: ours has the case's name in the signature, which
///   FCS has only from a use of the case;
/// - "constructor's new": at a call of an overloaded constructor, FCS adds that overload's `new`;
/// - "signed parameter": the parameter's name in the signature, for a parameter with no key;
/// - "named argument": a parameter asked at its definition lacks its named uses in other files; a named argument lacks
///   the other files' uses, and asked in another file than its method's answers nothing;
/// - "interface property": at a slot with both accessors, whose token is its setter, FCS adds the names that implement it;
///   at such a name, ours adds the uses of the property.
let private limitOf
    (asked: FSharpSymbolUse)
    (key: string option)
    (tied: bool)
    (ours: (string * bool) list)
    (expected: FcsUse list)
    (keysAt: Dictionary<string, Set<string>>)
    (definedBothSides: HashSet<string>)
    (ourText: string -> string)
    =
    let ourAll = ours |> List.map fst |> Set.ofList
    let missing = expected |> List.filter (fun u -> not (ourAll.Contains u.Range))
    let surplus = ourAll - (expected |> List.map _.Range |> Set.ofList)
    let name = asked.Symbol.DisplayName
    let ourDefinitions = ours |> List.filter snd |> List.map fst |> Set.ofList
    let flagsAgree = expected |> List.forall (fun u -> not (ourAll.Contains u.Range) || u.IsDefinition = ourDefinitions.Contains u.Range)
    let isConstructor = (match asked.Symbol with :? FSharpMemberOrFunctionOrValue as v -> v.IsConstructor | _ -> false)
    let isPattern = asked.Symbol :? FSharpActivePatternCase

    let isSlotProperty = key |> Option.exists (fun k -> kindOf k = "property" && k.Contains "|slot|")

    if not flagsAgree then
        None
    elif isSlotProperty then
        let k = key.Value

        if ourAll.IsEmpty then
            None
        elif k.EndsWith "|setter" then
            if surplus.IsEmpty && not missing.IsEmpty && missing |> List.forall (fun u -> not u.IsDefinition && u.Text = name && u.Keys.Contains(plain k)) then
                Some "interface property"
            else
                None
        else
            let isUseOfProperty (at: string) = (match keysAt.TryGetValue at with | true, keys -> keys.Contains k | _ -> false)

            if missing.IsEmpty && not surplus.IsEmpty && Set.forall isUseOfProperty surplus then
                Some "interface property"
            else
                None
    elif isPattern && missing.IsEmpty && surplus |> Set.forall (fun at -> at.Contains ".fsi " && ourText at = name) then
        Some "pattern's case"
    elif not surplus.IsEmpty || missing.IsEmpty then
        None
    elif tied && ourAll.IsEmpty && key |> Option.exists (fun k -> not (List.contains (kindOf k) [ "property"; "namespace"; "anonymous" ])) then
        Some "tie"
    // What the server tells by: the key isn't defined on both sides of the signature. A setter's suffix isn't another key
    elif key.IsSome
         && ourAll.IsEmpty
         && not isConstructor
         && (asked.Symbol :? FSharpMemberOrFunctionOrValue)
         && not (definedBothSides.Contains(plain key.Value))
         && missing |> List.exists (fun u -> not u.Keys.IsEmpty && not (u.Keys |> Set.map plain |> Set.contains (plain key.Value))) then
        Some "generic overload"
    elif key.IsSome
         && not ourAll.IsEmpty
         && not isConstructor
         && (asked.Symbol :? FSharpMemberOrFunctionOrValue)
         && missing |> List.exists (fun u -> not u.InSignature && not u.Keys.IsEmpty && not (u.Keys.Contains key.Value))
         && missing |> List.forall (fun u -> u.InSignature || (not u.Keys.IsEmpty && not (u.Keys.Contains key.Value))) then
        Some "generic overload's sibling"
    elif isConstructor && key.IsSome && missing |> List.forall (fun u -> u.IsDefinition && u.Text = "new") then
        Some "constructor's new"
    elif key.IsNone && not isPattern && missing |> List.forall _.InSignature then
        Some "signed parameter"
    elif key.IsNone && (asked.Symbol :? FSharpParameter) then
        let inOtherFile = not (String.Equals((asked.Symbol :?> FSharpParameter).DeclarationLocation.FileName, asked.FileName, StringComparison.OrdinalIgnoreCase))

        if (ourAll.IsEmpty && inOtherFile) || (not ourAll.IsEmpty && missing |> List.forall (fun u -> u.InSignature || u.InOtherFile)) then
            Some "named argument"
        else
            None
    elif key.IsNone && (asked.Symbol :? FSharpMemberOrFunctionOrValue) && not ourAll.IsEmpty && missing |> List.forall (fun u -> u.InSignature || (u.InOtherFile && u.IsArgument)) then
        Some "named argument"
    else
        None

/// The keys two definitions share, in FCS's own check, as `isTied` exempts them
let private tiesOf (checks: FSharpCheckFileResults seq) =
    let definitions =
        checks
        |> Seq.collect (fun c -> c.GetAllUsesOfAllSymbolsInFile())
        |> Seq.filter (fun u -> u.IsFromDefinition && References.definesItsKey u.Symbol)
        |> Seq.choose (fun u -> References.keyOf u.Symbol |> Option.map (fun k -> k, u.Symbol.SignatureLocation))
        |> Seq.distinct
        |> Seq.groupBy fst

    set
        [ for k, group in definitions do
              if Seq.length group > 1 && not (List.contains (kindOf k) [ "property"; "namespace"; "anonymous" ]) then
                  yield k ]

/// What a test does to the answers before they are compared, knowing where constructors stand
type private Mutation = HashSet<string> -> (string * bool) list -> (string * bool) list

/// Every token of the asking files, against the union of FCS's uses over `files`
let private compareTokens
    (mutate: Mutation)
    (scenario: string)
    (step: string)
    (scope: References.Scope)
    (checks: Dictionary<string, FSharpCheckFileResults>)
    (files: string list)
    (textOf: string -> string)
    (asking: (ProjectGraph * string) list)
    : Task<Outcome list> =
    task {
        let outcomes = ResizeArray<Outcome>()
        let ties = tiesOf checks.Values
        let keysAt = Dictionary<string, Set<string>>()
        let constructors = HashSet<string>()
        let arguments = HashSet<string>()
        let definedInSignature = HashSet<string>()
        let definedInImplementation = HashSet<string>()

        for f in files do
            let check = checks[f]
            let keys = References.keysIn check CancellationToken.None

            for u in check.GetAllUsesOfAllSymbolsInFile() do
                let at = showRange f u.Range

                if u.IsFromDefinition then
                    keys u |> Option.iter (fun k -> (if f.EndsWith ".fsi" then definedInSignature else definedInImplementation).Add(plain k) |> ignore)

                match u.Symbol with
                | :? FSharpMemberOrFunctionOrValue as v when v.IsConstructor -> constructors.Add at |> ignore
                | :? FSharpParameter -> arguments.Add at |> ignore
                | _ -> ()

                match keys u with
                | Some k -> keysAt[at] <- (match keysAt.TryGetValue at with | true, s -> s.Add k | _ -> Set.singleton k)
                | None -> ()

        let textAt (f: string) (r: range) =
            let lines = (textOf f).Split '\n'

            if r.StartLine = r.EndLine && r.StartLine - 1 < lines.Length && r.EndColumn <= lines[r.StartLine - 1].Length then
                lines[r.StartLine - 1].Substring(r.StartColumn, r.EndColumn - r.StartColumn)
            else
                "?"

        let definedBothSides = HashSet<string>(definedInSignature)
        definedBothSides.IntersectWith definedInImplementation

        for graph, file in asking do
            let keys = References.keysIn checks[file] CancellationToken.None

            for token in tokensOf (textOf file) do
                let! actual = References.find scope graph file token.Line token.Character CancellationToken.None
                let where = $"%s{scenario} / %s{step}: %s{Path.GetFileName file} %d{token.Line + 1},%d{token.EndColumn} %A{token.Names} [%s{token.LineText.Trim()}]"

                let ourTexts = Dictionary<string, string>()

                let answered =
                    match actual with
                    | References.Answer.Moved -> None
                    | References.Answer.Found uses ->
                        for struct (r, _) in uses do
                            match files |> List.tryFind (fun f -> Path.GetFileName f = Path.GetFileName r.FileName) with
                            | Some f -> ourTexts[showRange r.FileName r] <- textAt f r
                            | None -> ()

                        Some(uses |> List.map (fun (struct (r, d)) -> showRange r.FileName r, d) |> mutate constructors)

                match checks[file].GetSymbolUseAtLocation(token.Line + 1, token.EndColumn, token.LineText, token.Names), answered with
                | _, None -> outcomes.Add(Different $"%s{where}: Moved with nothing moving")
                | None, Some [] -> outcomes.Add Same
                | None, Some ours -> outcomes.Add(Different $"%s{where}: FCS has no symbol, ours %A{ours}")
                | Some asked, Some ours ->
                    let expected =
                        [ for f in files do
                              for u in checks[f].GetUsesOfSymbolInFile asked.Symbol do
                                  let at = showRange f u.Range

                                  { Range = at
                                    IsDefinition = u.IsFromDefinition
                                    InSignature = f.EndsWith ".fsi"
                                    InOtherFile = not (String.Equals(f, file, StringComparison.OrdinalIgnoreCase))
                                    Text = textAt f u.Range
                                    IsArgument = arguments.Contains at
                                    Keys = (match keysAt.TryGetValue at with | true, k -> k | _ -> Set.empty) } ]

                    let all (uses: (string * bool) list) = uses |> List.map fst |> Set.ofList
                    let notDeclared (uses: (string * bool) list) = uses |> List.filter (snd >> not) |> all
                    let ourAll = all ours
                    let fcsAll = expected |> List.map _.Range |> Set.ofList
                    let ourUses = notDeclared ours
                    let fcsUses = expected |> List.filter (fun u -> not u.IsDefinition) |> List.map _.Range |> Set.ofList
                    let key = keys asked
                    let tied = key |> Option.exists ties.Contains

                    if ourAll = fcsAll && ourUses = fcsUses then
                        outcomes.Add Same
                    else
                        match limitOf asked key tied ours expected keysAt definedBothSides (fun at -> match ourTexts.TryGetValue at with | true, t -> t | _ -> "") with
                        | Some limit -> outcomes.Add(Limit limit)
                        | None ->
                            outcomes.Add(Different $"%s{where} {asked.Symbol.GetType().Name}: ours only %s{show (ourAll - fcsAll)} | FCS only %s{show (fcsAll - ourAll)} | undeclared ours only %s{show (ourUses - fcsUses)} | FCS only %s{show (fcsUses - ourUses)}")

        return List.ofSeq outcomes
    }

let private compareState (mutate: Mutation) (state: State) : Task<Outcome list> =
    task {
        let checks = Dictionary<string, FSharpCheckFileResults>()

        for file in state.Files do
            let! check = state.Reference file
            checks[file] <- check

        let scope: References.Scope =
            { OwnersOf = state.OwnersOf
              ReferencingProjects = fun _ -> []
              Projects = fun () -> [ state.Graph ]
              FilesOf = fun _ -> state.Files
              IsLoading = fun () -> false
              BeforeRead = fun _ -> Task.CompletedTask }

        return! compareTokens mutate state.Scenario state.Step scope checks state.Files state.TextOf [ for f in state.Implementations -> state.Graph, f ]
    }

let private summarize (what: string) (outcomes: Outcome list) =
    let different = outcomes |> List.choose (function Different d -> Some d | _ -> None)
    let limits = outcomes |> List.choose (function Limit l -> Some l | _ -> None) |> List.countBy id
    let shown = different |> List.truncate 25 |> String.concat "\n  "
    printfn "references grid %s: %d tokens, %d differ, limits %A" what outcomes.Length different.Length limits
    Expect.isGreaterThan outcomes.Length 0 "tokens"
    Expect.isTrue different.IsEmpty $"%d{different.Length} of %d{outcomes.Length} tokens differ from FCS:\n  %s{shown}"

let private expectGrid (promote: bool) (scenario: Scenario) =
    task {
        let! outcomes = run promote scenario (compareState (fun _ ours -> ours))
        summarize $"""%s{scenario.Name} (%s{if promote then "by declaration" else "file level"})""" outcomes
    }

/// App on Lib in memory, at every token of both, against FCS checking them as one project at the same texts: FCS can't
/// check App on Lib's unbuilt text as two.
let private expectAcrossProjects (lib: Scenario) (appText: string) =
    task {
        let! outcomes =
            runReferencing lib appText (fun state app appFile ->
                task {
                    let files = state.Files @ [ appFile ]
                    let checks = Dictionary<string, FSharpCheckFileResults>()

                    for f in files do
                        let! check = state.Combined f
                        checks[f] <- check

                    let scope: References.Scope =
                        { OwnersOf = state.OwnersOf
                          ReferencingProjects = fun g -> if obj.ReferenceEquals(g, state.Graph) then [ app ] else []
                          Projects = fun () -> [ state.Graph; app ]
                          FilesOf = fun g -> if obj.ReferenceEquals(g, state.Graph) then state.Files else [ appFile ]
                          IsLoading = fun () -> false
                          BeforeRead = fun _ -> Task.CompletedTask }

                    let asking = [ for f in state.Implementations -> state.Graph, f ] @ [ app, appFile ]
                    return! compareTokens (fun _ ours -> ours) lib.Name state.Step scope checks files state.TextOf asking
                })

        summarize $"%s{lib.Name}, App on Lib" outcomes
    }

let private filterShapes = ReferencesGridTexts.noTextShapes @ ReferencesGridTexts.writtenByName
let private allScenarios = DefinitionGridTexts.all @ ReferencesGridTexts.scenarios @ filterShapes

/// The uses, in FCS's own check of every file, of a key `guard` names a symbol of, in a file whose text hasn't that name.
/// This is what a name filter would lose. Every use counts here, not only those a token can ask for.
let private unwritten (guard: FSharpSymbol -> string option) (state: State) : Task<string list> =
    task {
        let names = Dictionary<string, string>()
        let uses = ResizeArray<string * string * FSharpSymbolUse>()

        for file in state.Files do
            let! check = state.Reference file
            let keys = References.keysIn check CancellationToken.None

            for u in check.GetAllUsesOfAllSymbolsInFile() do
                match keys u with
                | Some key ->
                    uses.Add(file, plain key, u)

                    match guard u.Symbol with
                    | Some name -> names[plain key] <- name
                    | None -> ()
                | None -> ()

        return
            [ for file, key, u in uses do
                  match names.TryGetValue key with
                  | true, name when not ((state.TextOf file).Contains(name, StringComparison.Ordinal)) ->
                      yield $"%s{state.Step}: %s{Path.GetFileName file} %d{u.Range.StartLine},%d{u.Range.StartColumn} %s{key} isn't written `%s{name}`"
                  | _ -> () ]
    }

[<Tests>]
let tests =
    testList
        "ReferencesGrid"
        [ testTask "App on Lib, unsigned" { do! expectAcrossProjects DefinitionGridTexts.libUnsigned DefinitionGridTexts.uses }
          testTask "App on Lib, signed" { do! expectAcrossProjects DefinitionGridTexts.libSigned DefinitionGridTexts.uses }
          testTask "App extends Lib's type" { do! expectAcrossProjects ReferencesGridTexts.libExtended ReferencesGridTexts.appExtending }

          testTask "the grid sees every constructor call dropped" {
              let scenario = ReferencesGridTexts.scenarios |> List.find (fun s -> s.Name = "constructors")
              let dropCalls: Mutation = fun constructors ours -> ours |> List.filter (fun (at, _) -> not (constructors.Contains at))
              let! (outcomes: Outcome list) = run true scenario (compareState dropCalls)
              let different = outcomes |> List.filter (function Different _ -> true | _ -> false)
              Expect.isGreaterThan different.Length 10 "no limit may excuse them"
          }

          testTask "the grid sees real answers emptied" {
              let emptied: Mutation = fun _ _ -> []
              let names = [ "implementations more generic than their signature"; "a member more generic than its signature, an override"; "signed: a property's accessors on two lines"; "constructors"; "interface properties" ]

              let excused =
                  allScenarios
                  |> List.filter (fun s -> List.contains s.Name names)
                  |> List.map (fun s ->
                      task {
                          let! (real: Outcome list) = run true s (compareState (fun _ ours -> ours))
                          let! (mutated: Outcome list) = run true s (compareState emptied)
                          let count (outcomes: Outcome list) = outcomes |> List.filter (function Limit "generic overload" -> true | _ -> false) |> List.length
                          return s.Name, count real, count mutated
                      })

              for t in excused do
                  let! (name: string), (real: int), (mutated: int) = t
                  Expect.equal mutated real $"%s{name}: emptying real answers adds none to `generic overload`"
          }

          testTask "every use of a symbol the filter applies to has its name in its file" {
              for s in allScenarios @ List.map fst ReferencesGridTexts.knownDifferences do
                  let! (lost: string list) = run false s (unwritten SymbolUses.spelledName)
                  Expect.isEmpty lost $"%s{s.Name}"
          }

          testTask "the shapes reach symbols with no text, so a filter on every symbol would lose uses" {
              for s in ReferencesGridTexts.noTextShapes do
                  let! (lost: string list) = run false s (unwritten (fun symbol -> Some symbol.DisplayName))
                  printfn "unguarded, %s loses %d: %s" s.Name lost.Length (String.Join("; ", lost |> List.truncate 60))
                  Expect.isNonEmpty lost $"%s{s.Name}"
          }

          testList
              "known differences"
              [ for s, count in ReferencesGridTexts.knownDifferences do
                    for promote in [ true; false ] ->
                        testTask $"""%s{s.Name} (%s{if promote then "by declaration" else "file level"})""" {
                            let! (outcomes: Outcome list) = run promote s (compareState (fun _ ours -> ours))
                            let different = outcomes |> List.filter (function Different _ -> true | _ -> false)
                            Expect.equal different.Length count "as at 9a6a312: fixing one should change this number"
                        } ]

          testList "by declaration" [ for s in allScenarios -> testTask s.Name { do! expectGrid true s } ]
          testList "file level" [ for s in DefinitionGridTexts.batteries @ ReferencesGridTexts.scenarios @ filterShapes -> testTask s.Name { do! expectGrid false s } ] ]
