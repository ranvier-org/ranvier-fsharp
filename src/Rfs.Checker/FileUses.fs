namespace Rfs.Checker

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text

module internal ReadSupport =
    /// Takes the lock exclusively. A grant that arrives after `ct` ended the wait is released at once.
    let acquireExclusive (l: AsyncReaderWriterLock) (ct: CancellationToken) : Task<IDisposable> =
        task {
            let grant = l.WriteAsync()

            try
                return! grant.WaitAsync ct
            with :? OperationCanceledException ->
                grant.ContinueWith(
                    (fun (t: Task<IDisposable>) -> if t.IsCompletedSuccessfully then t.Result.Dispose()),
                    TaskScheduler.Default
                )
                |> ignore

                return raise (OperationCanceledException ct)
        }

/// What a file says of one symbol key.
[<NoComparison; NoEquality>]
type KeyUses =
    { /// Each use's range, with whether it is a definition
      Hits: (struct (range * bool))[]
      /// Where each definition here that is the only one of its key says it is. The location can be stale: it tells
      /// definitions apart and names a signature
      Definitions: range[] }

/// One definition in a file by its raw key (DeclarationChecker.SymbolKey), with what DeclarationChecker's location lookups
/// read of its symbol.
[<Struct; NoComparison; NoEquality>]
type DefinitionSite =
    { /// The raw key, which only a signature's lookup reads (the exact match among what conformed); null in an
      /// implementation's table, whose entries are by the key already
      Key: string
      /// The symbol's signature location
      Location: range voption
      IsGetter: bool
      IsSetter: bool }

/// DeclarationChecker's location lookups over a file's definitions as plain data: DefinitionLocation's for an
/// implementation, SignatureLocation's for a signature. The key handling follows the fork's SymbolKeys (service_decl.fs).
module internal DefinitionSites =
    let private setterSuffix = "|setter"

    let withoutSetter (key: string) =
        if key.EndsWith setterSuffix then key.Substring(0, key.Length - setterSuffix.Length) else key

    /// What a signature's definition shares with an implementation that conformed to it: a member's types can differ
    let conformance (key: string) =
        let key = withoutSetter key

        match key.Split '|' with
        | [| assembly; kind; parent; apparent; name; isInstance; _typars; role; types |] ->
            let arguments = if types = "" then 0 else types.Split(';').Length
            String.concat "|" [ assembly; kind; parent; apparent; name; isInstance; role; string arguments ]
        | _ -> key

    /// A property's getter's location first, or its setter's for the setter's key; two other locations answer nothing
    let locationOf (key: string) (sites: DefinitionSite seq) : range option =
        let wanted (s: DefinitionSite) = if key.EndsWith setterSuffix then s.IsSetter else s.IsGetter

        let locations =
            sites
            |> Seq.sortBy (fun s -> if wanted s then 0 else 1)
            |> Seq.choose (fun s -> match s.Location with ValueSome r -> Some r | ValueNone -> None)
            |> Seq.distinct
            |> Seq.toList

        match locations with
        | [ only ] -> Some only
        | first :: _ when key.Split('|')[1] = "property" -> Some first
        | _ -> None

    let siteOf (key: string) (symbol: FSharpSymbol) : DefinitionSite =
        let getter, setter =
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as v -> v.IsPropertyGetterMethod, v.IsPropertySetterMethod
            | _ -> false, false

        { Key = key
          Location =
            match symbol.SignatureLocation with
            | Some r -> ValueSome r
            | None -> ValueNone
          IsGetter = getter
          IsSetter = setter }

    /// The sites by `by` of each one's key, keeping the key in each site only when `keepKey`
    let group (by: string -> string) (keepKey: bool) (sites: DefinitionSite seq) : Dictionary<string, DefinitionSite[]> =
        let d = Dictionary<string, DefinitionSite[]>()

        for key, g in sites |> Seq.groupBy (fun s -> by s.Key) do
            d[key] <- g |> Seq.map (fun s -> if keepKey then s else { s with Key = null }) |> Array.ofSeq

        d

/// <summary>
/// Every symbol use of one check of a file, by key, and its definitions by raw key. Immutable and free of typed-tree
/// objects, so it needs no lock and keeps no check alive.
/// </summary>
[<Sealed>]
type FileUses
    internal
    (
        source: WeakReference,
        byKey: Dictionary<string, KeyUses>,
        definitions: Dictionary<string, DefinitionSite[]>,
        conforming: Dictionary<string, DefinitionSite[]>
    ) =
    static let nothing = { Hits = [||]; Definitions = [||] }

    let sitesIn (d: Dictionary<string, DefinitionSite[]>) (key: string) =
        match d.TryGetValue key with
        | true, sites -> sites
        | _ -> [||]

    /// The result or signature this was made from, while something else holds it. Weak: holding it would keep the check
    member internal _.Source = source

    /// What the file says of exactly this key
    member _.Of(key: string) : KeyUses =
        match byKey.TryGetValue key with
        | true, uses -> uses
        | _ -> nothing

    /// How many keys it holds
    member _.Keys = byKey.Count

    /// An implementation file's answer to DeclarationChecker.DefinitionLocation for the raw key, from the check indexed
    member _.DefinitionOf(key: string) : range option =
        DefinitionSites.locationOf key (sitesIn definitions (DefinitionSites.withoutSetter key))

    /// A signature file's answer to DeclarationChecker.SignatureLocation for the raw key, from the check indexed
    member _.SignatureDefinitionOf(key: string, conformed: bool) : range option =
        let defined = sitesIn conforming (DefinitionSites.conformance key)
        let exact = defined |> Array.filter (fun s -> DefinitionSites.withoutSetter s.Key = DefinitionSites.withoutSetter key)

        if exact.Length = 0 && conformed then
            DefinitionSites.locationOf key defined
        else
            DefinitionSites.locationOf key exact

    /// An estimate of the bytes it holds (entries, key strings, arrays, ranges), for measuring: the uses by key, then the
    /// definitions by raw key
    member internal _.Footprint: struct (int64 * int64) =
        // A range is two int64s; arrays hold structs inline
        let rangeBytes = 16L
        let text (s: string) = 22L + 2L * int64 s.Length
        let entry = 24L
        let mutable keyed = 0L

        for KeyValue(k, u) in byKey do
            keyed <- keyed + entry + text k + 32L + int64 u.Hits.Length * (rangeBytes + 8L) + 24L + int64 u.Definitions.Length * rangeBytes + 24L

        let mutable raw = 0L

        for d in [ definitions; conforming ] do
            for KeyValue(k, sites) in d do
                raw <- raw + entry + text k + 24L

                for site in sites do
                    raw <- raw + 8L + (8L + rangeBytes) + 8L + (if isNull site.Key then 0L else text site.Key)

        struct (keyed, raw)


/// Symbol keys as references count them, and the index of a file's uses by them.
module SymbolUses =
    /// What FCS counts as one symbol, as far as a key can say it: a constructor's uses are its type's; what a key sends to
    /// its case or type (a union's `IsCase`, a generated member) is a symbol of its own; a pattern's cases are told apart.
    let keyOf (symbol: FSharpSymbol) : string option =
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
    let keysIn (check: FSharpCheckFileResults) (ct: CancellationToken) : FSharpSymbolUse -> string option =
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

    /// Whether a definition with a key is the only one of that key. A type has its constructors' key, and many of them; a
    /// pattern's case says where its use is, not where it is defined.
    let definesItsKey (symbol: FSharpSymbol) =
        match symbol with
        | :? FSharpActivePatternCase -> false
        | :? FSharpMemberOrFunctionOrValue as v -> not v.IsConstructor
        | _ -> true

    let private plainIdentifier (name: string) =
        name.Length > 0
        && (Char.IsLetter name[0] || name[0] = '_')
        && name |> Seq.forall (fun c -> Char.IsLetterOrDigit c || c = '_' || c = '\'')
        && not (name.StartsWith "op_")

    /// <summary>
    /// The name every use of the symbol has in the text of its file, for the kinds of symbol that can be shown to be
    /// written by name; None for every other. A whitelist of kinds: what isn't listed scans every file.
    /// </summary>
    /// <remarks>
    /// Left out, each for reaching a symbol with no such text (ReferencesGridTexts has a shape of each): members, as the
    /// compiler looks up `Bind`, `Dispose`, `GetEnumerator`, `Item`, `GetSlice`, `op_Implicit` and so on by name, and a
    /// custom operation has another spelling; types, modules and namespaces, through abbreviations, `FooAttribute` and
    /// constructors; active patterns; anything not a plain identifier, as an operator is spelled otherwise than its name; a
    /// value of a `NumericLiteralQ` module, which `1Q` calls with no text. That guard is by the declaring module's name, and a
    /// module abbreviation (`module NumericLiteralZ = Other`) gets past it, as the compiler resolves the module by its name
    /// in the environment: it is safe only because FCS records the module at the literal and not the value. The shape
    /// "written by name" pins that. A case's generated `IsCase` has a key of its own.
    /// FSharp.Core's values and cases (`Some` for an optional argument, `GetArraySlice` for a slice) are reached from no
    /// text too: `References` applies this only to what a loaded project defines.
    /// </remarks>
    let spelledName (symbol: FSharpSymbol) : string option =
        let name =
            match symbol with
            | :? FSharpUnionCase as c -> Some c.Name
            | :? FSharpField as f when
                not f.IsAnonRecordField
                && not f.IsCompilerGenerated
                && f.DeclaringUnionCase.IsNone
                && f.DeclaringEntity |> Option.exists _.IsFSharpRecord
                ->
                Some f.Name
            | :? FSharpMemberOrFunctionOrValue as v when
                v.IsModuleValueOrMember
                && not v.IsMember
                && not v.IsExtensionMember
                && not v.IsActivePattern
                && not v.IsCompilerGenerated
                && v.DeclaringEntity |> Option.exists (fun e -> e.IsFSharpModule && not (e.LogicalName.StartsWith "NumericLiteral"))
                ->
                Some v.LogicalName
            | _ -> None

        name |> Option.filter plainIdentifier

    let private build
        (source: obj)
        (definitionSites: Dictionary<string, DefinitionSite[]>)
        (conforming: Dictionary<string, DefinitionSite[]>)
        (uses: seq<struct (string * range * bool * range voption)>)
        =
        let hits = Dictionary<string, List<struct (range * bool)>>()
        let definitions = Dictionary<string, List<range>>()

        for struct (key, r, isDefinition, location) in uses do
            match hits.TryGetValue key with
            | true, l -> l.Add(struct (r, isDefinition))
            | _ -> hits[key] <- List [ struct (r, isDefinition) ]

            match location with
            | ValueSome at ->
                match definitions.TryGetValue key with
                | true, l -> l.Add at
                | _ -> definitions[key] <- List [ at ]
            | ValueNone -> ()

        let byKey = Dictionary<string, KeyUses>(hits.Count)

        for KeyValue(key, l) in hits do
            byKey[key] <-
                { Hits = l.ToArray()
                  Definitions =
                    match definitions.TryGetValue key with
                    | true, d -> d.ToArray()
                    | _ -> [||] }

        FileUses(WeakReference source, byKey, definitionSites, conforming)

    let private noSites = Dictionary<string, DefinitionSite[]>()

    /// Each definition's site by its raw key, constructors included. A union case's or exception's fields too: the
    /// compiler reports a definition only for a named one, and an unnamed one (`Item1`, `Data0`) can be named in a use.
    let private sitesOf (definitions: seq<FSharpSymbol>) : DefinitionSite list =
        let withFields (symbol: FSharpSymbol) : FSharpSymbol seq =
            seq {
                yield symbol

                match symbol with
                | :? FSharpUnionCase as c -> yield! c.Fields |> Seq.cast<FSharpSymbol>
                | :? FSharpEntity as e when e.IsFSharpExceptionDeclaration -> yield! e.FSharpFields |> Seq.cast<FSharpSymbol>
                | _ -> ()
            }

        [ for symbol in definitions |> Seq.collect withFields do
              match DeclarationChecker.SymbolKey symbol with
              | Some key -> DefinitionSites.siteOf key symbol
              | None -> () ]

    /// Indexes an implementation file's check.
    let internal ofCheck (source: obj) (check: FSharpCheckFileResults) (ct: CancellationToken) : FileUses =
        let keys = keysIn check ct
        let all = check.GetAllUsesOfAllSymbolsInFile ct |> Seq.toArray

        let definitions =
            all |> Seq.filter _.IsFromDefinition |> Seq.map _.Symbol |> sitesOf |> DefinitionSites.group DefinitionSites.withoutSetter false

        all
        |> Seq.choose (fun u ->
            keys u
            |> Option.map (fun key ->
                let location =
                    if u.IsFromDefinition && definesItsKey u.Symbol then
                        match u.Symbol.SignatureLocation |> Option.orElse u.Symbol.DeclarationLocation with
                        | Some at -> ValueSome at
                        | None -> ValueNone
                    else
                        ValueNone

                struct (key, u.Range, u.IsFromDefinition, location)))
        |> build source definitions noSites

    /// Indexes what a signature file's check resolved. Its definitions are kept by raw key for SignatureLocation, and have
    /// no locations to tell apart in the uses.
    let internal ofSignature (source: obj) (uses: (struct (range * bool * FSharpSymbol))[]) (ct: CancellationToken) : FileUses =
        // What SignatureLocation walks is the file's own module type. A module's enclosing namespaces are in it, with no
        // definition use; a `type System.String with` augmentation's use is a definition of what another file defines.
        let enclosing (symbol: FSharpSymbol) : FSharpSymbol seq =
            seq {
                yield symbol

                match symbol with
                | :? FSharpEntity as e ->
                    let mutable parent = e.DeclaringEntity

                    while parent |> Option.exists _.IsNamespace do
                        yield parent.Value :> FSharpSymbol
                        parent <- parent.Value.DeclaringEntity
                | _ -> ()
            }

        let file =
            uses |> Array.tryPick (fun (struct (r, definition, _)) -> if definition then Some r.FileName else None)

        let conforming =
            uses
            |> Seq.choose (fun (struct (_, definition, symbol)) -> if definition then Some symbol else None)
            |> Seq.collect enclosing
            |> sitesOf
            |> List.filter (fun s ->
                match s.Location, file with
                | ValueSome r, Some f -> String.Equals(r.FileName, f, StringComparison.OrdinalIgnoreCase)
                | _ -> true)
            |> DefinitionSites.group DefinitionSites.conformance true

        // As in an implementation file (keysIn): a pattern's case names no value in the signature that lists the pattern
        let patterns =
            uses
            |> Array.choose (fun (struct (r, definition, symbol)) ->
                match symbol with
                | :? FSharpMemberOrFunctionOrValue as v when definition && v.IsActivePattern ->
                    DeclarationChecker.SymbolKey symbol |> Option.map (fun k -> r, v.LogicalName.Split('|'), k)
                | _ -> None)

        uses
        |> Seq.choose (fun (struct (r, definition, symbol)) ->
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

            own |> Option.map (fun key -> struct (key, r, definition, ValueNone)))
        |> build source noSites conforming
