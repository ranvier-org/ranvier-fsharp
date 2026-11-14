namespace Rfs.Agent

open System
open FSharp.Compiler.Tokenization
open Rfs.Server

[<NoComparison; NoEquality>]
type internal ResolvedTarget =
    { Line0: int
      Char0: int
      Line1: int
      Col1: int
      Names: string list
      EndCol: int }

module internal PositionResolver =

    let private normalizeSymbol (raw: string) : string =
        let trimmed = raw.Trim()

        let noBackticks (s: string) =
            if s.Length > 4 && s.StartsWith("``", StringComparison.Ordinal) && s.EndsWith("``", StringComparison.Ordinal) then
                s.Substring(2, s.Length - 4)
            else
                s

        // If user passes a dotted name like `Hover.lookupOf`, look for the final member token `lookupOf`
        let dotIdx = trimmed.LastIndexOf '.'

        let leaf =
            if dotIdx >= 0 && dotIdx + 1 < trimmed.Length && trimmed <> "." && trimmed <> ".." then
                trimmed.Substring(dotIdx + 1)
            else
                trimmed

        noBackticks leaf

    let private modifierKeywords =
        set
            [ "let"; "member"; "static"; "inline"; "private"; "internal"; "public"; "rec"; "mutable"; "val"; "type"
              "module"; "namespace"; "open"; "and"; "override"; "abstract"; "default"; "new"; "if"; "then"; "else"
              "match"; "with"; "fun"; "function"; "do"; "return"; "yield"; "for"; "in"; "while" ]

    [<NoComparison>]
    type private LineCandidate =
        { Line0: int
          Char0: int
          Names: string list
          EndCol: int }

    let private scanLine (text: string) (lines: string[]) (line0: int) : LineCandidate list =
        if line0 < 0 || line0 >= lines.Length then
            []
        else
            let rawLine = lines[line0].TrimEnd '\r'
            let found = ResizeArray<LineCandidate>()
            let seenEnds = System.Collections.Generic.HashSet<int>()

            for c in 0 .. rawLine.Length - 1 do
                if not (Char.IsWhiteSpace rawLine[c]) then
                    match Hover.lookupOf text line0 c with
                    | Some(_, names, endCol) when seenEnds.Add endCol ->
                        found.Add { Line0 = line0; Char0 = c; Names = names; EndCol = endCol }
                    | _ -> ()

            List.ofSeq found

    let private toResolved (c: LineCandidate) =
        { Line0 = c.Line0
          Char0 = c.Char0
          Line1 = c.Line0 + 1
          Col1 = c.Char0 + 1
          Names = c.Names
          EndCol = c.EndCol }

    let resolve (text: string) (line1Based: int) (col1BasedOpt: int option) (symbolOpt: string option) : Result<ResolvedTarget, string> =
        if String.IsNullOrEmpty text then
            Error "File is empty."
        else
            let lines = text.Split '\n'

            if line1Based < 1 || line1Based > lines.Length then
                Error $"Line {line1Based} is out of range (file has {lines.Length} lines)."
            else
                let targetLine0 = line1Based - 1

                let tryCol (line0: int) (col0: int) =
                    if col0 >= 0 then
                        Hover.lookupOf text line0 col0
                        |> Option.map (fun (_, names, endCol) ->
                            toResolved { Line0 = line0; Char0 = col0; Names = names; EndCol = endCol })
                    else
                        None

                match symbolOpt |> Option.filter (fun s -> not (String.IsNullOrWhiteSpace s)) with
                | Some rawSym ->
                    let targetName = normalizeSymbol rawSym
                    let offsets = [ 0; -1; 1; -2; 2; -3; 3; -4; 4; -5; 5 ]

                    let matchesOn (cmp: StringComparison) =
                        offsets
                        |> List.tryPick (fun d ->
                            let l0 = targetLine0 + d

                            let hits =
                                scanLine text lines l0
                                |> List.filter (fun cand ->
                                    match List.tryLast cand.Names with
                                    | Some leaf -> String.Equals(leaf, targetName, cmp)
                                    | None -> false)

                            match hits with
                            | [] -> None
                            | [ single ] -> Some single
                            | many ->
                                match col1BasedOpt with
                                | Some c1 ->
                                    let targetC0 = max 0 (c1 - 1)
                                    Some(many |> List.minBy (fun h -> Math.Abs(h.Char0 - targetC0)))
                                | None -> Some many.Head)

                    match matchesOn StringComparison.Ordinal |> Option.orElseWith (fun () -> matchesOn StringComparison.OrdinalIgnoreCase) with
                    | Some hit -> Ok(toResolved hit)
                    | None ->
                        let near =
                            [ targetLine0 - 1; targetLine0; targetLine0 + 1 ]
                            |> List.collect (fun l0 ->
                                scanLine text lines l0
                                |> List.map (fun h -> $"""{l0 + 1}:{h.Char0 + 1}:{String.Join(".", h.Names)}"""))

                        let hint = if near.IsEmpty then "(none found)" else String.Join(", ", List.truncate 12 near)
                        Error $"Symbol '{rawSym}' not found near line {line1Based}. Nearby symbols: {hint}"

                | None ->
                    match col1BasedOpt with
                    | Some col1 ->
                        let c0 = max 0 (col1 - 1)

                        match tryCol targetLine0 c0
                              |> Option.orElseWith (fun () -> tryCol targetLine0 (c0 + 1))
                              |> Option.orElseWith (fun () -> tryCol targetLine0 (max 0 (c0 - 1))) with
                        | Some res -> Ok res
                        | None ->
                            let onLine =
                                scanLine text lines targetLine0
                                |> List.map (fun h -> $"""{h.Char0 + 1}:{String.Join(".", h.Names)}""")

                            let hint = if onLine.IsEmpty then "no identifiers on line" else String.Join(", ", List.truncate 10 onLine)
                            Error $"No symbol at {line1Based}:{col1}. Columns on line {line1Based}: {hint}"
                    | None ->
                        let candidates = scanLine text lines targetLine0

                        let picked =
                            candidates
                            |> List.tryFind (fun h ->
                                match List.tryLast h.Names with
                                | Some leaf -> not (modifierKeywords.Contains leaf)
                                | None -> false)
                            |> Option.orElse (List.tryHead candidates)

                        match picked with
                        | Some hit -> Ok(toResolved hit)
                        | None -> Error $"No symbol found on line {line1Based}."
