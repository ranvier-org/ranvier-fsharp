namespace Rfs.Checker

open System
open System.Collections.Generic
open FSharp.Compiler.Declarations

/// Every key of a file, in declaration order.
[<Sealed>]
type Order(keys: DeclarationKey[]) =
    let positions = Dictionary<DeclarationKey, int>(HashIdentity.Structural)

    do keys |> Array.iteri (fun i key -> positions[key] <- i)

    member _.Keys = keys

    member _.TryPosition(key: DeclarationKey) =
        match positions.TryGetValue key with
        | true, position -> ValueSome position
        | _ -> ValueNone

    member _.Contains(key: DeclarationKey) = positions.ContainsKey key

    /// Equal when the keys are, in the same order.
    static member Comparer =
        { new IEqualityComparer<Order> with
            member _.Equals(a, b) = a.Keys = b.Keys
            member _.GetHashCode o = o.Keys.Length }

/// What a file's declarations define, from their syntax.
type FileIndex =
    { /// Each defined name, with the declarations defining it, in file order
      Definers: Map<string, DeclarationKey list>
      /// The declarations every later declaration depends on, in file order
      AffectsAllLater: DeclarationKey list
      /// Each augmentation, with the earlier declarations the names it augments may resolve to
      Augmentations: (DeclarationKey * DeclarationKey list) list }

/// Each name defined by the implementation files of a project, with the declarations defining it.
type ProjectIndex = Map<string, DeclarationRef list>

/// How a declaration is checked: on its own, or as part of a segment its first declaration checks in order.
[<RequireQualifiedAccess>]
type Role =
    | Single
    /// Checks the declarations of the span, itself first, one after the other, whenever any of them would re-check
    | Leader of span: DeclarationKey[]
    | Member of leader: DeclarationKey

/// What a declaration's check reads of the rest of its file.
type Dependencies =
    { /// Earlier declarations of the file defining a name it uses (all of them when a name is unresolvable): a change
      /// to one's fingerprint re-checks it
      Named: DeclarationKey[]
      /// Earlier declarations whose re-check re-checks it, with no fingerprint cutoff: the module header it is in, and
      /// every earlier declaration that affects all later ones (an open has no fingerprint of what it opens)
      Identity: DeclarationKey[]
      /// Earlier declarations of the file its latest published result depends on through types (TypedUses): a change
      /// to one's fingerprint re-checks it. The check also reads its own latest result's, which can be newer.
      Typed: DeclarationKey[]
      /// Declarations of earlier files defining a name it uses. Cross-file dependencies are per file in v1 (R16): the
      /// file begins again when an earlier file's signature changes, so these only record what a finer rule would read.
      CrossFile: DeclarationRef[] }

module NameIndex =

    let private add (name: string) (key: DeclarationKey) (definers: Map<string, DeclarationKey list>) =
        definers |> Map.change name (fun keys -> Some(key :: Option.defaultValue [] keys))

    /// The last segment of a name as written: `M.T` augments a type named T, wherever M resolves.
    let private lastSegment (name: string) =
        match name.LastIndexOf '.' with
        | -1 -> name
        | i -> name.Substring(i + 1)

    /// The index of a file from its declarations' contents, in declaration order. An augmentation's names resolve to
    /// every earlier declaration defining their last segment, which over-approximates when the name is ambiguous.
    let ofFile (declarations: (DeclarationKey * DeclarationContent)[]) : FileIndex =
        let mutable definers = Map.empty
        let affectsAllLater = ResizeArray()
        let augmentations = ResizeArray()

        for key, content in declarations do
            if content.Augments.Length > 0 then
                let targets =
                    content.Augments
                    |> Seq.collect (fun name -> definers |> Map.tryFind (lastSegment name) |> Option.defaultValue [])
                    |> Seq.distinct
                    |> List.ofSeq

                augmentations.Add((key, targets))

            if content.AffectsAllLater then
                affectsAllLater.Add key

            for name in content.Defines do
                definers <- add name key definers

        { Definers = definers |> Map.map (fun _ keys -> List.rev keys)
          AffectsAllLater = List.ofSeq affectsAllLater
          Augmentations = List.ofSeq augmentations }

    let ofProject (files: (string * FileIndex) list) : ProjectIndex =
        (Map.empty, files)
        ||> List.fold (fun acc (file, index) ->
            (acc, index.Definers)
            ||> Map.fold (fun acc name keys ->
                let refs = [ for key in keys -> { File = file; Key = Some key } ]
                acc |> Map.change name (fun known -> Some(Option.defaultValue [] known @ refs))))

    /// The keys of `TypedUses` that are declarations of `file` other than `self`, sorted.
    let typedKeys (file: string) (self: DeclarationKey) (uses: DeclarationRef[]) =
        uses
        |> Array.choose (fun r ->
            match r.Key with
            | Some key when key <> self && String.Equals(r.File, file, StringComparison.OrdinalIgnoreCase) -> Some key
            | _ -> None)
        |> Array.distinct
        |> Array.sort

    let dependencies
        (key: DeclarationKey)
        (within: DeclarationKey option)
        (content: DeclarationContent)
        (order: Order)
        (index: FileIndex)
        (typed: DeclarationKey[])
        (earlierFiles: ProjectIndex)
        : Dependencies =
        let position =
            match order.TryPosition key with
            | ValueSome p -> p
            | ValueNone -> invalidOp $"%A{key} is not a declaration of the file"

        let earlier (k: DeclarationKey) =
            match order.TryPosition k with
            | ValueSome p -> p < position
            | ValueNone -> false

        let names =
            Seq.concat [ content.Uses; content.MemberUses; content.FieldOrCaseUses ]
            |> Seq.distinct
            |> Array.ofSeq

        let augmented =
            index.Augmentations
            |> List.tryFind (fun (augmentation, _) -> augmentation = key)
            |> Option.map snd
            |> Option.defaultValue []

        let named =
            if content.HasUnresolvable then
                Array.sub order.Keys 0 position
            else
                Seq.append (names |> Seq.collect (fun n -> index.Definers |> Map.tryFind n |> Option.defaultValue [])) augmented
                |> Seq.filter earlier
                |> Seq.distinct
                |> Array.ofSeq
            |> Array.sort

        let identity =
            Seq.append (Option.toList within) (index.AffectsAllLater |> List.filter earlier)
            |> Seq.distinct
            |> Array.ofSeq
            |> Array.sort

        { Named = named
          Identity = identity
          Typed = typed |> Array.filter earlier
          CrossFile =
            names
            |> Seq.collect (fun n -> earlierFiles |> Map.tryFind n |> Option.defaultValue [])
            |> Seq.distinct
            |> Array.ofSeq
            |> Array.sort }

    /// The role of every declaration. A segment starts at the first declaration whose result depends on later ones
    /// (R12) and runs to the end of the file, or at a type an augmentation of the file may extend (R11) and runs to its
    /// last augmentation; a segment that reaches the start of another one takes it in.
    let roles (order: Order) (index: FileIndex) (dependsOnLater: DeclarationKey option) : Map<DeclarationKey, Role> =
        let keys = order.Keys
        let count = keys.Length
        let lastAugmentation = Dictionary<int, int>()

        for augmentation, targets in index.Augmentations do
            match order.TryPosition augmentation with
            | ValueSome a ->
                for target in targets do
                    match order.TryPosition target with
                    | ValueSome t when t < a ->
                        lastAugmentation[t] <-
                            match lastAugmentation.TryGetValue t with
                            | true, known -> max known a
                            | _ -> a
                    | _ -> ()
            | ValueNone -> ()

        let leader =
            dependsOnLater |> Option.bind (fun k -> order.TryPosition k |> ValueOption.toOption)

        let roles = Dictionary<DeclarationKey, Role>(HashIdentity.Structural)
        let mutable i = 0

        while i < count do
            let last =
                if leader = Some i then
                    count - 1
                else
                    match lastAugmentation.TryGetValue i with
                    | true, a ->
                        let mutable last = a
                        let mutable j = i + 1

                        while j <= last do
                            if leader = Some j then
                                last <- count - 1

                            match lastAugmentation.TryGetValue j with
                            | true, b -> last <- max last b
                            | _ -> ()

                            j <- j + 1

                        last
                    | _ -> i

            if last = i then
                roles[keys[i]] <- Role.Single
            else
                let span = Array.sub keys i (last - i + 1)
                roles[keys[i]] <- Role.Leader span

                for k in span.[1..] do
                    roles[k] <- Role.Member keys[i]

            i <- last + 1

        roles |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
