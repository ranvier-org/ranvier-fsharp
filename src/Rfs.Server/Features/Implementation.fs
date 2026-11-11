namespace Rfs.Server

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Declarations
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Go to implementation (<c>textDocument/implementation</c>) for both <c>.fs</c> and <c>.fsi</c> files.
/// Reads across the defining project and referencing projects in one single consistent graph turn
/// (<c>ProjectGraph.ReadInOneTurn</c>), collecting concrete subtype, object-expression, and dispatch-slot
/// implementations (<c>FileUses.ImplementationsOf</c>) plus the paired <c>.fs</c> implementation when asked on an
/// <c>.fsi</c> signature declaration.
/// </summary>
module Implementation =

    let private setterSuffix = "|setter"

    let private withoutSetter (key: string) =
        if key.EndsWith(setterSuffix, StringComparison.Ordinal) then
            key.Substring(0, key.Length - setterSuffix.Length)
        else
            key

    let private sameFile (a: string) (b: string) =
        try
            String.Equals(Path.GetFullPath a, Path.GetFullPath b, StringComparison.OrdinalIgnoreCase)
        with _ ->
            String.Equals(a, b, StringComparison.OrdinalIgnoreCase)

    [<NoComparison; NoEquality>]
    type private TargetQuery =
        { TargetKeys: string list
          FsiDeclRawKey: string option
          Landed: string option }

    let find
        (scope: References.Scope)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<Result<range[], unit>> =
        task {
            try
                let! queryOpt =
                    if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                        graph.ReadSignature(
                            path,
                            (fun view -> Hover.lookupOf view.Text line character),
                            (fun _ lookup _ sigUses ->
                                let endColOpt = lookup |> Option.map (fun (_, _, ec) -> ec)

                                match Definition.pickSignatureUse line character endColOpt sigUses with
                                | None -> None
                                | Some(struct (_, isDef, symbol)) ->
                                    let targetKeys =
                                        SymbolUses.keyOf symbol
                                        |> Option.map (fun k -> [ withoutSetter k ])
                                        |> Option.defaultValue []

                                    let rawDeclKey =
                                        if isDef then DeclarationChecker.SymbolKey symbol else None

                                    let landed =
                                        symbol.SignatureLocation
                                        |> Option.orElse symbol.DeclarationLocation
                                        |> Option.map _.FileName
                                        |> Option.orElse (Some path)

                                    if targetKeys.IsEmpty && rawDeclKey.IsNone then
                                        None
                                    else
                                        Some
                                            { TargetKeys = targetKeys
                                              FsiDeclRawKey = rawDeclKey
                                              Landed = landed }),
                            ct
                        )
                    else
                        graph.ReadFile(
                            path,
                            (fun view -> Hover.lookupOf view.Text line character),
                            (fun view lookup ->
                                match lookup with
                                | None -> None
                                | Some(lineStr, names, endColumn) ->
                                    let check = view.Result.CheckResults

                                    match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
                                    | None -> None
                                    | Some u ->
                                        let keys = SymbolUses.keysIn check ct

                                        let primaryKeys =
                                            keys u
                                            |> Option.map (fun k -> [ withoutSetter k ])
                                            |> Option.defaultValue []

                                        let extraSlotKeys =
                                            match u.Symbol with
                                            | :? FSharpMemberOrFunctionOrValue as mfv when mfv.IsOverrideOrExplicitInterfaceImplementation ->
                                                [ for d in check.GetAllUsesOfAllSymbolsInFile ct do
                                                      if d.IsFromDispatchSlotImplementation && Range.equals d.Range u.Range then
                                                          match keys d with
                                                          | Some sk -> yield withoutSetter sk
                                                          | None -> () ]
                                            | _ -> []

                                        let allKeys = (primaryKeys @ extraSlotKeys) |> List.distinct

                                        let landed =
                                            u.Symbol.SignatureLocation
                                            |> Option.orElse u.Symbol.DeclarationLocation
                                            |> Option.map _.FileName

                                        if allKeys.IsEmpty then
                                            None
                                        else
                                            Some
                                                { TargetKeys = allKeys
                                                  FsiDeclRawKey = None
                                                  Landed = landed }),
                            ct
                        )

                match queryOpt with
                | None -> return Ok [||]
                | Some query ->
                    let definersNow () =
                        match query.Landed with
                        | Some file ->
                            let impl =
                                if file.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                                    Path.ChangeExtension(file, ".fs")
                                else
                                    file

                            (scope.OwnersOf file @ scope.OwnersOf impl) |> List.distinct
                        | None -> []

                    let turnScope () : TurnScope<unit> =
                        if scope.IsLoading() then
                            { Loading = true
                              Projects = [||]
                              Name = None
                              Extra = () }
                        else
                            let definers = definersNow ()

                            let projects =
                                match definers with
                                | [] -> scope.Projects()
                                | ds -> ds @ (ds |> List.collect scope.ReferencingProjects)
                                |> fun found -> (match scope.OwnersOf path with g :: _ -> g | [] -> graph) :: found
                                |> List.distinct

                            { Loading = false
                              Projects = [| for p in projects -> p, scope.FilesOf p |]
                              Name = None
                              Extra = () }

                    match! graph.ReadInOneTurn(turnScope, ct) with
                    | TurnRead.Gone -> return Error()
                    | TurnRead.Read((), projects) ->
                        let pairedImplPath = Path.ChangeExtension(path, ".fs")
                        let seen = HashSet<struct (string * int * int * int * int)>()
                        let results = ResizeArray<range>()

                        let addRange (r: range) =
                            let key =
                                struct (Path.GetFullPath(r.FileName).ToUpperInvariant(), r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)

                            if seen.Add key then
                                results.Add r

                        for _, turnFiles in projects do
                            for tf in turnFiles do
                                match tf with
                                | TurnFile.Indexed(filePath, _, uses) ->
                                    for tk in query.TargetKeys do
                                        for r in uses.ImplementationsOf tk do
                                            addRange r

                                    match query.FsiDeclRawKey with
                                    | Some rk ->
                                        let isPairedFs =
                                            sameFile filePath pairedImplPath
                                            || (uses.DefinitionOf rk |> Option.exists (fun sr -> sameFile sr.FileName path))

                                        if isPairedFs then
                                            match uses.ImplementationDefinitionOf(rk, true) with
                                            | Some implRange -> addRange implRange
                                            | None -> ()
                                    | None -> ()
                                | _ -> ()

                        let arr = results.ToArray()

                        Array.sortInPlaceBy
                            (fun (r: range) ->
                                Path.GetFullPath(r.FileName).ToUpperInvariant(), r.StartLine, r.StartColumn, r.EndLine, r.EndColumn)
                            arr

                        return Ok arr
            with
            | :? OperationCanceledException when not ct.IsCancellationRequested -> return Error()
            | :? SupersededLineDirectivesException when not ct.IsCancellationRequested -> return Error()
        }
