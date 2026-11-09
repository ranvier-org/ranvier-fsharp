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
/// - "use in a signature": a signature file is checked with no sink, so all that is known of it is what it defines. A
///   pattern's case there is one of these, as the signature defines the pattern and not the case, and a `new` there, and
///   what a signature that we found the declaration in counts as defining past it, as an augmentation's header;
/// - "constructor's new": at a call of an overloaded constructor, FCS adds that overload's `new`;
/// - "namespace": a namespace in a signature file's header, or a type of another assembly extended in a signature file,
///   each a definition with nothing to look up. Across projects also: see `crossProject`;
/// - "signed parameter": the parameter's name in the signature, for a parameter with no key;
/// - "named argument": a parameter asked at its definition lacks its named uses in other files; a named argument lacks
///   the other files' uses, and asked in another file than its method's answers nothing;
/// - "interface property": at a slot with both accessors, whose token is its setter, FCS adds the names that implement it;
///   at such a name, ours adds the uses of the property.
/// `crossProject`: FCS checks both projects as one, whose namespace is one: asked in App, a namespace of the same name is
/// another assembly's for ours, and it has App's uses, the token itself at least, so an empty answer isn't excused.
let private limitOf
    (crossProject: bool)
    (asked: FSharpSymbolUse)
    (key: string option)
    (tied: bool)
    (ours: (string * bool) list)
    (expected: FcsUse list)
    (keysAt: Dictionary<string, Set<string>>)
    =
    let ourAll = ours |> List.map fst |> Set.ofList
    let missing = expected |> List.filter (fun u -> not (ourAll.Contains u.Range))
    let surplus = ourAll - (expected |> List.map _.Range |> Set.ofList)
    let name = asked.Symbol.DisplayName
    let declared = ourAll |> Set.exists (fun at -> at.Contains ".fsi ")
    let ourDefinitions = ours |> List.filter snd |> List.map fst |> Set.ofList
    let flagsAgree = expected |> List.forall (fun u -> not (ourAll.Contains u.Range) || u.IsDefinition = ourDefinitions.Contains u.Range)
    let isConstructor = (match asked.Symbol with :? FSharpMemberOrFunctionOrValue as v -> v.IsConstructor | _ -> false)
    let isNamespace = (match asked.Symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false)
    let isPattern = asked.Symbol :? FSharpActivePatternCase

    let isExternal =
        match asked.Symbol with
        | :? FSharpEntity as e -> e.IsNamespace || not (e.DeclarationLocation.FileName.Contains "rfs-token-grid-")
        | _ -> false

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
    elif not surplus.IsEmpty || missing.IsEmpty then
        None
    elif tied && ourAll.IsEmpty && key |> Option.exists (fun k -> not (List.contains (kindOf k) [ "property"; "namespace"; "anonymous" ])) then
        Some "tie"
    elif key.IsSome
         && ourAll.IsEmpty
         && (asked.Symbol :? FSharpMemberOrFunctionOrValue)
         && missing |> List.exists (fun u -> not u.Keys.IsEmpty && not (u.Keys.Contains key.Value)) then
        Some "generic overload"
    elif key.IsSome
         && not ourAll.IsEmpty
         && not isConstructor
         && (asked.Symbol :? FSharpMemberOrFunctionOrValue)
         && missing |> List.exists (fun u -> not u.InSignature && not u.Keys.IsEmpty && not (u.Keys.Contains key.Value))
         && missing |> List.forall (fun u -> u.InSignature || (not u.Keys.IsEmpty && not (u.Keys.Contains key.Value))) then
        Some "generic overload's sibling"
    elif key.IsSome && missing |> List.forall (fun u -> u.InSignature && (not u.IsDefinition || u.Text = "new" || declared || isPattern)) then
        Some "use in a signature"
    elif isConstructor && key.IsSome && missing |> List.forall (fun u -> (u.IsDefinition && u.Text = "new") || (u.InSignature && declared)) then
        Some "constructor's new"
    elif key.IsSome && isExternal && missing |> List.forall (fun u -> u.InSignature && u.IsDefinition) then
        Some "namespace"
    elif crossProject && isNamespace && not ourAll.IsEmpty then
        Some "namespace"
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
    (crossProject: bool)
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

        for f in files do
            let check = checks[f]
            let keys = References.keysIn check CancellationToken.None

            for u in check.GetAllUsesOfAllSymbolsInFile() do
                let at = showRange f u.Range

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

        for graph, file in asking do
            let keys = References.keysIn checks[file] CancellationToken.None

            for token in tokensOf (textOf file) do
                let! actual = References.find scope graph file token.Line token.Character CancellationToken.None
                let where = $"%s{scenario} / %s{step}: %s{Path.GetFileName file} %d{token.Line + 1},%d{token.EndColumn} %A{token.Names} [%s{token.LineText.Trim()}]"

                let answered =
                    match actual with
                    | References.Answer.Moved -> None
                    | References.Answer.Found uses -> Some(uses |> List.map (fun (struct (r, d)) -> showRange r.FileName r, d) |> mutate constructors)

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
                        match limitOf crossProject asked key tied ours expected keysAt with
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
              IsLoading = fun () -> false }

        return! compareTokens false mutate state.Scenario state.Step scope checks state.Files state.TextOf [ for f in state.Implementations -> state.Graph, f ]
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
                          IsLoading = fun () -> false }

                    let asking = [ for f in state.Implementations -> state.Graph, f ] @ [ app, appFile ]
                    return! compareTokens true (fun _ ours -> ours) lib.Name state.Step scope checks files state.TextOf asking
                })

        summarize $"%s{lib.Name}, App on Lib" outcomes
    }

let private allScenarios = DefinitionGridTexts.all @ ReferencesGridTexts.scenarios

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

          testList "by declaration" [ for s in allScenarios -> testTask s.Name { do! expectGrid true s } ]
          testList "file level" [ for s in DefinitionGridTexts.batteries @ ReferencesGridTexts.scenarios -> testTask s.Name { do! expectGrid false s } ] ]
