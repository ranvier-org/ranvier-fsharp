namespace Rfs.Server

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Declarations
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Rfs.Checker

/// <summary>
/// Go to declaration (<c>textDocument/declaration</c>): prefers the <c>.fsi</c> signature declaration when one exists
/// in the owning project (even when asked from inside the paired <c>.fs</c> implementation), falling back to the
/// primary <c>.fs</c> definition otherwise.
/// </summary>
module Declaration =

    [<NoComparison>]
    type private FoundDecl =
        | DeclNowhere
        | DeclLocal of range
        | DeclKeyed of key: string * landed: string

    let private probeAskingPath =
        Path.Combine(Path.GetTempPath(), "__rfs_declaration_probe__.fs")

    let private findInFs
        (check: FSharpCheckFileResults)
        (line: int)
        (lineStr: string, names: string list, endColumn: int)
        (ct: CancellationToken)
        : FoundDecl =
        match check.GetDeclarationLocation(line + 1, endColumn, lineStr, names, preferFlag = true) with
        | FindDeclResult.DeclFound range when not (range.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)) ->
            match check.GetSymbolUseAtLocation(line + 1, endColumn, lineStr, names) with
            | Some u when (u.Symbol :? FSharpParameter) -> DeclNowhere
            | Some u when (match u.Symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false) ->
                let isHeader =
                    check.GetAllUsesOfAllSymbolsInFile ct
                    |> Seq.exists (fun d ->
                        d.IsFromDefinition
                        && d.Range.EndLine = line + 1
                        && d.Range.EndColumn = endColumn
                        && (match d.Symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false))

                match DeclarationChecker.SymbolKey u.Symbol with
                | Some key when isHeader ->
                    let signature =
                        check.GetAllUsesOfAllSymbolsInFile ct
                        |> Seq.tryPick (fun d ->
                            match d.IsFromDefinition, d.Symbol.SignatureLocation with
                            | true, Some r when r.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) -> Some r.FileName
                            | _ -> None)

                    DeclKeyed(key, defaultArg signature range.FileName)
                | _ -> DeclNowhere
            | Some u ->
                match DeclarationChecker.SymbolKey u.Symbol with
                | Some key ->
                    let sigLocOpt =
                        u.Symbol.SignatureLocation
                        |> Option.filter (fun r -> r.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase))
                        |> Option.map _.FileName
                        |> Option.orElseWith (fun () ->
                            if String.Equals(Path.GetFullPath range.FileName, Path.GetFullPath u.FileName, StringComparison.OrdinalIgnoreCase) then
                                check.GetAllUsesOfAllSymbolsInFile ct
                                |> Seq.tryPick (fun d ->
                                    match d.IsFromDefinition, d.Symbol.SignatureLocation with
                                    | true, Some r when r.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) -> Some r.FileName
                                    | _ -> None)
                            else
                                None)

                    let landed =
                        sigLocOpt
                        |> Option.orElse (u.Symbol.SignatureLocation |> Option.map _.FileName)
                        |> Option.orElse (u.Symbol.DeclarationLocation |> Option.map _.FileName)
                        |> Option.defaultValue range.FileName

                    DeclKeyed(key, landed)
                | None -> DeclLocal range
            | None -> DeclLocal range
        | _ -> DeclNowhere

    let private resolveKeyed
        (ownersOf: string -> ProjectGraph list)
        (key: string)
        (landed: string)
        (ct: CancellationToken)
        : Task<range option> =
        task {
            let implementation =
                if landed.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
                    Path.ChangeExtension(landed, ".fs")
                else
                    landed

            let owners = (ownersOf implementation @ ownersOf (Path.ChangeExtension(landed, ".fsi"))) |> List.distinct
            let mutable answer = None

            for owner in owners do
                if answer.IsNone then
                    let! current = owner.CurrentLocation(probeAskingPath, landed, key, ct)
                    answer <- current

            return answer
        }

    let at
        (ownersOf: string -> ProjectGraph list)
        (graph: ProjectGraph)
        (path: string)
        (line: int)
        (character: int)
        (ct: CancellationToken)
        : Task<range option> =
        if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
            task {
                let! found =
                    graph.ReadSignature(
                        path,
                        (fun view -> Hover.lookupOf view.Text line character),
                        (fun _ lookup _ sigUses ->
                            let endColOpt = lookup |> Option.map (fun (_, _, ec) -> ec)

                            match Definition.pickSignatureUse line character endColOpt sigUses with
                            | None -> DeclNowhere
                            | Some(struct (_, _, symbol)) when (symbol :? FSharpParameter) -> DeclNowhere
                            | Some(struct (_, isDef, symbol)) when (match symbol with :? FSharpEntity as e -> e.IsNamespace | _ -> false) && not isDef ->
                                DeclNowhere
                            | Some(struct (ownRange, true, _)) -> DeclLocal ownRange
                            | Some(struct (_, false, symbol)) ->
                                match DeclarationChecker.SymbolKey symbol, symbol.SignatureLocation |> Option.orElse symbol.DeclarationLocation with
                                | Some key, Some loc when not (loc.FileName.EndsWith(Range.rangeStartup.FileName, StringComparison.Ordinal)) ->
                                    DeclKeyed(key, loc.FileName)
                                | _ -> DeclNowhere),
                        ct
                    )

                match found with
                | DeclNowhere -> return None
                | DeclLocal ownRange -> return Some ownRange
                | DeclKeyed(key, landed) -> return! resolveKeyed ownersOf key landed ct
            }
        else
            task {
                let! found =
                    graph.ReadFile(
                        path,
                        (fun view -> Hover.lookupOf view.Text line character),
                        (fun view lookup ->
                            match lookup with
                            | Some l -> findInFs view.Result.CheckResults line l ct
                            | None -> DeclNowhere),
                        ct
                    )

                match found with
                | DeclNowhere -> return None
                | DeclLocal range ->
                    let here = String.Equals(Path.GetFullPath range.FileName, Path.GetFullPath path, StringComparison.OrdinalIgnoreCase)
                    return if here && not (range.FileName.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase)) then Some range else None
                | DeclKeyed(key, landed) -> return! resolveKeyed ownersOf key landed ct
            }
