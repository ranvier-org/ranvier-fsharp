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

/// <summary>
/// Every symbol use of one check of a file, by key. Immutable and free of typed-tree objects, so it needs no lock and keeps
/// no check alive.
/// </summary>
[<Sealed>]
type FileUses internal (source: WeakReference, byKey: Dictionary<string, KeyUses>) =
    static let nothing = { Hits = [||]; Definitions = [||] }

    /// The result or signature this was made from, while something else holds it. Weak: holding it would keep the check
    member internal _.Source = source

    /// What the file says of exactly this key
    member _.Of(key: string) : KeyUses =
        match byKey.TryGetValue key with
        | true, uses -> uses
        | _ -> nothing

    /// How many keys it holds
    member _.Keys = byKey.Count


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

    let private build (source: obj) (uses: seq<struct (string * range * bool * range voption)>) =
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

        FileUses(WeakReference source, byKey)

    /// Indexes an implementation file's check.
    let internal ofCheck (source: obj) (check: FSharpCheckFileResults) (ct: CancellationToken) : FileUses =
        let keys = keysIn check ct

        check.GetAllUsesOfAllSymbolsInFile ct
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
        |> build source

    /// Indexes what a signature file's check resolved. It has no definitions to tell apart.
    let internal ofSignature (source: obj) (uses: (struct (range * bool * FSharpSymbol))[]) (ct: CancellationToken) : FileUses =
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
        |> build source
