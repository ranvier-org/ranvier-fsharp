namespace Rfs.Server

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Go to type definition (<c>textDocument/typeDefinition</c>) for both <c>.fs</c> and <c>.fsi</c> files.
/// Unwraps type abbreviations (<c>IsAbbreviation</c> / <c>IsFSharpAbbreviation</c>) to the underlying workspace entity
/// when available and resolves the settled range via <c>ProjectGraph.CurrentLocation</c>.
/// </summary>
module TypeDefinition =

    let rec private entitiesFromType (depth: int) (ty: FSharpType) : FSharpEntity list =
        if depth > 16 then
            []
        else
            let isAbbr = try ty.IsAbbreviation with _ -> false

            if isAbbr then
                let unwrapped =
                    (try Some ty.AbbreviatedType with _ -> None)
                    |> Option.map (entitiesFromType (depth + 1))
                    |> Option.defaultValue []

                let selfEnt =
                    if (try ty.HasTypeDefinition with _ -> false) then
                        try [ ty.TypeDefinition ] with _ -> []
                    else
                        []

                unwrapped @ selfEnt
            else
                let stripped = try ty.StripAbbreviations() with _ -> ty

                if (try stripped.HasTypeDefinition with _ -> false) then
                    try [ stripped.TypeDefinition ] with _ -> []
                elif (try ty.HasTypeDefinition with _ -> false) then
                    try [ ty.TypeDefinition ] with _ -> []
                else
                    []

    let rec private entitiesFromEntity (depth: int) (e: FSharpEntity) : FSharpEntity list =
        if depth > 16 then
            [ e ]
        elif (try e.IsNamespace || e.IsFSharpModule with _ -> false) then
            []
        elif (try e.IsFSharpAbbreviation with _ -> false) then
            let unwrapped =
                (try Some e.AbbreviatedType with _ -> None)
                |> Option.map (entitiesFromType (depth + 1))
                |> Option.defaultValue []

            unwrapped @ [ e ]
        else
            [ e ]

    let private candidateTargets (symbol: FSharpSymbol) : (string * string) list =
        let entities =
            match symbol with
            | :? FSharpEntity as e -> entitiesFromEntity 0 e
            | :? FSharpField as f ->
                (try Some f.FieldType with _ -> None)
                |> Option.map (entitiesFromType 0)
                |> Option.defaultValue []
            | :? FSharpUnionCase as c ->
                (try Some c.ReturnType with _ -> None)
                |> Option.map (entitiesFromType 0)
                |> Option.defaultValue []
            | :? FSharpParameter as p ->
                (try Some p.Type with _ -> None)
                |> Option.map (entitiesFromType 0)
                |> Option.defaultValue []
            | :? FSharpActivePatternCase as apc ->
                (try Some apc.Group.OverallType with _ -> None)
                |> Option.map (entitiesFromType 0)
                |> Option.defaultValue []
            | :? FSharpMemberOrFunctionOrValue as v ->
                if v.IsConstructor then
                    v.DeclaringEntity
                    |> Option.map (entitiesFromEntity 0)
                    |> Option.defaultValue []
                else
                    let fullTyOpt = try Some v.FullType with _ -> None

                    let fromFull =
                        fullTyOpt
                        |> Option.map (entitiesFromType 0)
                        |> Option.defaultValue []

                    let isCallable =
                        (try v.CurriedParameterGroups.Count > 0 with _ -> false)
                        || (fullTyOpt |> Option.exists (fun t -> try t.IsFunctionType with _ -> false))

                    let fromReturn =
                        if isCallable then
                            (try Some v.ReturnParameter.Type with _ -> None)
                            |> Option.map (entitiesFromType 0)
                            |> Option.defaultValue []
                        else
                            []

                    fromFull @ fromReturn
            | _ -> []

        entities
        |> List.choose (fun e ->
            match DeclarationChecker.SymbolKey e with
            | None -> None
            | Some key ->
                match e.SignatureLocation |> Option.orElse (e :> FSharpSymbol).DeclarationLocation with
                | Some loc when not (loc.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)) ->
                    Some(key, loc.FileName)
                | _ -> None)
        |> List.distinct

    let at
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<range option> =
        task {
            let! candidates =
                if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                    graph.ReadSignature(
                        path,
                        (fun view -> Hover.lookupOf view.Text line character),
                        (fun _ lookup _ sigUses ->
                            let endColOpt = lookup |> Option.map (fun (_, _, ec) -> ec)

                            match Definition.pickSignatureUse line character endColOpt sigUses with
                            | Some(struct (_, _, symbol)) -> candidateTargets symbol
                            | None -> []),
                        ct
                    )
                else
                    graph.ReadFile(
                        path,
                        (fun view -> Hover.lookupOf view.Text line character),
                        (fun view lookup ->
                            match lookup with
                            | None -> []
                            | Some(lineStr, names, endColumn) ->
                                let check =
                                    if view.Whole then
                                        view.Result.CheckResultsNotKept
                                    else
                                        view.CheckResultsAt(line + 1, endColumn)

                                match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
                                | Some u -> candidateTargets u.Symbol
                                | None -> []),
                        ct
                    )

            let mutable answer = None

            for key, landed in candidates do
                if answer.IsNone then
                    let implementation =
                        if landed.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                            Path.ChangeExtension(landed, ".fs")
                        else
                            landed

                    let owners = (ownersOf implementation @ ownersOf (Path.ChangeExtension(landed, ".fsi"))) |> List.distinct

                    for owner in owners do
                        if answer.IsNone then
                            let! current = owner.CurrentLocation(path, landed, key, ct)
                            answer <- current

            return answer
        }
