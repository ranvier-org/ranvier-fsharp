namespace Rfs.Projects

open System
open System.Collections.Generic
open System.IO

module Glob =

    type EvaluatedItem =
        { FullPath: string
          RecursiveDir: string
          Metadata: (string * string) list }

    type ItemOp =
        | Include of includeExpr: string * excludeExpr: string option * metadata: (string * string) list
        | Remove of removeExpr: string
        | Update of updateExpr: string * metadataUpdates: (string * string) list

    let defaultCaseSensitive () : bool = OperatingSystem.IsLinux()

    let inline private toSlash (p: string) = p.Replace('\\', '/')

    let private unescapeMsBuild (s: string) : string =
        if s.IndexOf('%') < 0 then
            s
        else
            s
                .Replace("%3B", ";", StringComparison.OrdinalIgnoreCase)
                .Replace("%2A", "*", StringComparison.OrdinalIgnoreCase)
                .Replace("%3F", "?", StringComparison.OrdinalIgnoreCase)

    let splitPatterns (expr: string) : string list =
        if String.IsNullOrWhiteSpace expr then
            []
        else
            [ for raw in expr.Split(';') do
                  let trimmed = raw.Trim()

                  if trimmed.Length > 0 then
                      yield unescapeMsBuild trimmed ]

    let private collapseIntraStars (seg: string) : string =
        if seg.IndexOf("**", StringComparison.Ordinal) < 0 then
            seg
        else
            let sb = Text.StringBuilder(seg.Length)
            let mutable prevStar = false

            for i = 0 to seg.Length - 1 do
                let c = seg[i]

                if c = '*' then
                    if not prevStar then
                        sb.Append('*') |> ignore
                        prevStar <- true
                else
                    prevStar <- false
                    sb.Append(c) |> ignore

            sb.ToString()

    /// Matches a pattern with `*` and `?` (no `**`) against `text`, where neither `*` nor `?` may ever match `'/'`.
    let private matchNoSlash (caseSensitive: bool) (pat: string) (text: string) : bool =
        let cmp = if caseSensitive then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
        let mutable pi = 0
        let mutable ti = 0
        let mutable starPi = -1
        let mutable starTi = -1
        let mutable failed = false

        while ti < text.Length && not failed do
            let tc = text[ti]

            if pi < pat.Length && pat[pi] = '?' && tc <> '/' then
                pi <- pi + 1
                ti <- ti + 1
            elif pi < pat.Length && pat[pi] = '*' then
                starPi <- pi
                starTi <- ti
                pi <- pi + 1
            elif pi < pat.Length && MemoryExtensions.Equals(text.AsSpan(ti, 1), pat.AsSpan(pi, 1), cmp) then
                pi <- pi + 1
                ti <- ti + 1
            elif starPi >= 0 && text[starTi] <> '/' then
                pi <- starPi + 1
                starTi <- starTi + 1
                ti <- starTi
            else
                failed <- true

        if failed then
            false
        else
            while pi < pat.Length && pat[pi] = '*' do
                pi <- pi + 1

            pi = pat.Length

    type private ParsedPattern =
        | LiteralPath of fullPath: string
        | WildcardPath of searchRoot: string * patSegs: string[] * hasDoubleStar: bool * flatNoDoubleStar: string

    let private parseSinglePattern (baseDir: string) (pat: string) : ParsedPattern =
        let norm = toSlash (pat.Trim())
        let firstWild = norm.IndexOfAny([| '*'; '?' |])

        if firstWild < 0 then
            let osRel = norm.Replace('/', Path.DirectorySeparatorChar)
            LiteralPath(Path.GetFullPath(Path.Combine(baseDir, osRel)))
        else
            let slashBefore = if firstWild = 0 then -1 else norm.LastIndexOf('/', firstWild - 1)

            let searchRoot, remRaw =
                if slashBefore < 0 then
                    Path.GetFullPath baseDir, norm
                else
                    let prefixRaw = norm.Substring(0, slashBefore)

                    let prefixFixed =
                        if prefixRaw = "" then "/"
                        elif prefixRaw.Length = 2 && prefixRaw[1] = ':' then prefixRaw + "/"
                        else prefixRaw

                    let rootFull =
                        Path.GetFullPath(Path.Combine(baseDir, prefixFixed.Replace('/', Path.DirectorySeparatorChar)))

                    rootFull, norm.Substring(slashBefore + 1)

            let rawSegs = remRaw.Split('/', StringSplitOptions.RemoveEmptyEntries)
            let collapsed = ResizeArray<string>(rawSegs.Length + 1)

            for seg in rawSegs do
                if seg = "**" then
                    if collapsed.Count = 0 || collapsed[collapsed.Count - 1] <> "**" then
                        collapsed.Add "**"
                else
                    collapsed.Add(collapseIntraStars seg)

            if collapsed.Count > 0 && collapsed[collapsed.Count - 1] = "**" then
                collapsed.Add "*"

            let segs = collapsed.ToArray()
            let hasDS = segs |> Array.contains "**"
            let flat = if hasDS then "" else String.Join("/", segs)
            WildcardPath(searchRoot, segs, hasDS, flat)

    let private tryRelativeSlash (caseSensitive: bool) (rootFull: string) (candFull: string) : string voption =
        let cmp = if caseSensitive then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
        let rootSlash = (toSlash rootFull).TrimEnd('/')
        let candSlash = toSlash candFull

        if rootSlash.Length = 0 then
            let rel = candSlash.TrimStart('/')
            if rel.Length = 0 then ValueNone else ValueSome rel
        elif
            candSlash.Length > rootSlash.Length + 1
            && candSlash.StartsWith(rootSlash, cmp)
            && candSlash[rootSlash.Length] = '/'
        then
            ValueSome(candSlash.Substring(rootSlash.Length + 1))
        else
            ValueNone

    let private matchParsed (caseSensitive: bool) (parsed: ParsedPattern) (candidateFull: string) : string voption =
        let cmp = if caseSensitive then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase

        match parsed with
        | LiteralPath full ->
            if String.Equals(toSlash full, toSlash candidateFull, cmp) then
                ValueSome ""
            else
                ValueNone
        | WildcardPath(searchRoot, patSegs, hasDS, flat) ->
            if patSegs.Length = 0 then
                ValueNone
            else
                match tryRelativeSlash caseSensitive searchRoot candidateFull with
                | ValueNone -> ValueNone
                | ValueSome relSlash ->
                    if not hasDS then
                        if matchNoSlash caseSensitive flat relSlash then ValueSome "" else ValueNone
                    else
                        let pathSegs = relSlash.Split('/', StringSplitOptions.RemoveEmptyEntries)
                        let pLen = patSegs.Length
                        let sLen = pathSegs.Length

                        if sLen = 0 then
                            ValueNone
                        else
                            let memo = Array2D.zeroCreate<sbyte> (pLen + 1) (sLen + 1)

                            let rec canMatch (pi: int) (si: int) : bool =
                                let cached = memo[pi, si]

                                if cached <> 0y then
                                    cached > 0y
                                else
                                    let ok =
                                        if pi = pLen && si = sLen then
                                            true
                                        elif pi = pLen then
                                            false
                                        elif patSegs[pi] = "**" then
                                            canMatch (pi + 1) si || (si < sLen - 1 && canMatch pi (si + 1))
                                        else
                                            si < sLen
                                            && matchNoSlash caseSensitive patSegs[pi] pathSegs[si]
                                            && canMatch (pi + 1) (si + 1)

                                    memo[pi, si] <- if ok then 1y else -1y
                                    ok

                            if not (canMatch 0 0) then
                                ValueNone
                            else
                                let recSb = Text.StringBuilder()
                                let mutable pi = 0
                                let mutable si = 0

                                while pi < pLen do
                                    if patSegs[pi] = "**" then
                                        if canMatch (pi + 1) si then
                                            pi <- pi + 1
                                        else
                                            recSb.Append(pathSegs[si]).Append('/') |> ignore
                                            si <- si + 1
                                    else
                                        pi <- pi + 1
                                        si <- si + 1

                                ValueSome(recSb.ToString())

    let matchPattern (caseSensitive: bool) (baseDir: string) (pattern: string) (candidateFullPath: string) : string voption =
        let baseFull = Path.GetFullPath baseDir
        let candFull = Path.GetFullPath candidateFullPath
        let parsed = parseSinglePattern baseFull pattern
        matchParsed caseSensitive parsed candFull

    let private sortWildcardMatches (matches: ResizeArray<string * string>) : (string * string) list =
        let arr = matches.ToArray()

        Array.sortInPlaceWith
            (fun (a, _) (b, _) ->
                let sa = toSlash a
                let sb = toSlash b
                let c = StringComparer.OrdinalIgnoreCase.Compare(sa, sb)
                if c <> 0 then c else StringComparer.Ordinal.Compare(sa, sb))
            arr

        List.ofArray arr

    let private buildExcludePred (caseSensitive: bool) (baseFull: string) (excludeExpr: string option) =
        match excludeExpr with
        | None -> (fun (_: string) -> false), [||]
        | Some expr when String.IsNullOrWhiteSpace expr -> (fun (_: string) -> false), [||]
        | Some expr ->
            let parsedExcludes =
                splitPatterns expr
                |> List.map (parseSinglePattern baseFull)
                |> Array.ofList

            let isExcluded (candFull: string) =
                parsedExcludes |> Array.exists (fun ep -> (matchParsed caseSensitive ep candFull).IsSome)

            isExcluded, parsedExcludes

    /// Checks whether an entire directory subtree is unconditionally excluded (e.g. `bin/**`, `obj/**`, `**/.git/**`)
    /// so disk enumeration skips walking into `.git`, `node_modules`, `bin`, `obj`, etc.
    let private isDirectorySubtreeExcluded (caseSensitive: bool) (parsedExcludes: ParsedPattern[]) (dirFull: string) : bool =
        let cmp = if caseSensitive then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
        let dirSlash = (toSlash dirFull).TrimEnd('/')
        let dirLeaf = Path.GetFileName dirFull

        parsedExcludes
        |> Array.exists (function
            | LiteralPath _ -> false
            | WildcardPath(exRoot, exSegs, _, _) ->
                let exRootSlash = (toSlash exRoot).TrimEnd('/')

                let strictlyInsideExRoot =
                    dirSlash.Length > exRootSlash.Length + 1
                    && dirSlash.StartsWith(exRootSlash, cmp)
                    && dirSlash[exRootSlash.Length] = '/'

                let atOrInsideExRoot =
                    String.Equals(dirSlash, exRootSlash, cmp) || strictlyInsideExRoot

                if not atOrInsideExRoot then
                    false
                elif exSegs.Length = 2 && exSegs[0] = "**" && exSegs[1] = "*" then
                    true
                elif
                    strictlyInsideExRoot
                    && exSegs.Length = 4
                    && exSegs[0] = "**"
                    && exSegs[2] = "**"
                    && exSegs[3] = "*"
                    && matchNoSlash caseSensitive exSegs[1] dirLeaf
                then
                    true
                else
                    false)

    let expandCandidates
        (caseSensitive: bool)
        (baseDir: string)
        (includeExpr: string)
        (excludeExpr: string option)
        (candidates: string seq)
        : (string * string) list =
        let baseFull = Path.GetFullPath baseDir
        let normCandidates = [| for c in candidates -> Path.GetFullPath c |]
        let isExcluded, _ = buildExcludePred caseSensitive baseFull excludeExpr
        let comparer = if caseSensitive then StringComparer.Ordinal else StringComparer.OrdinalIgnoreCase
        let seen = HashSet<string>(comparer)
        let results = ResizeArray<string * string>()

        for subPat in splitPatterns includeExpr do
            let parsed = parseSinglePattern baseFull subPat

            match parsed with
            | LiteralPath _ ->
                for cand in normCandidates do
                    match matchParsed caseSensitive parsed cand with
                    | ValueSome recDir when not (isExcluded cand) && seen.Add(toSlash cand) ->
                        results.Add((cand, recDir))
                    | _ -> ()
            | WildcardPath _ ->
                let subMatches = ResizeArray<string * string>()

                for cand in normCandidates do
                    match matchParsed caseSensitive parsed cand with
                    | ValueSome recDir when not (isExcluded cand) -> subMatches.Add((cand, recDir))
                    | _ -> ()

                for full, recDir in sortWildcardMatches subMatches do
                    if seen.Add(toSlash full) then
                        results.Add((full, recDir))

        List.ofSeq results

    let private enumeratePatternFiles
        (caseSensitive: bool)
        (parsedExcludes: ParsedPattern[])
        (searchRoot: string)
        (patSegs: string[])
        (hasDS: bool)
        : ResizeArray<string> =
        let found = ResizeArray<string>()

        if Directory.Exists searchRoot && patSegs.Length > 0 then
            let rec walk (dir: string) (depth: int) =
                try
                    if hasDS || depth = patSegs.Length - 1 then
                        for f in Directory.GetFiles dir do
                            found.Add(Path.GetFullPath f)

                    if hasDS || depth < patSegs.Length - 1 then
                        for sub in Directory.GetDirectories dir do
                            let subFull = Path.GetFullPath sub

                            if not (isDirectorySubtreeExcluded caseSensitive parsedExcludes subFull) then
                                if hasDS then
                                    walk subFull (depth + 1)
                                else
                                    let leaf = Path.GetFileName subFull

                                    if matchNoSlash caseSensitive patSegs[depth] leaf then
                                        walk subFull (depth + 1)
                with
                | :? UnauthorizedAccessException
                | :? DirectoryNotFoundException
                | :? IOException -> ()

            if not (isDirectorySubtreeExcluded caseSensitive parsedExcludes searchRoot) then
                walk searchRoot 0

        found

    let expandDisk
        (caseSensitive: bool)
        (baseDir: string)
        (includeExpr: string)
        (excludeExpr: string option)
        : (string * string) list =
        let baseFull = Path.GetFullPath baseDir
        let isExcluded, parsedExcludes = buildExcludePred caseSensitive baseFull excludeExpr
        let comparer = if caseSensitive then StringComparer.Ordinal else StringComparer.OrdinalIgnoreCase
        let seen = HashSet<string>(comparer)
        let results = ResizeArray<string * string>()

        for subPat in splitPatterns includeExpr do
            let parsed = parseSinglePattern baseFull subPat

            match parsed with
            | LiteralPath full ->
                if File.Exists full && not (isExcluded full) && seen.Add(toSlash full) then
                    results.Add((full, ""))
            | WildcardPath(searchRoot, patSegs, hasDS, _) ->
                let diskFiles = enumeratePatternFiles caseSensitive parsedExcludes searchRoot patSegs hasDS
                let subMatches = ResizeArray<string * string>()

                for cand in diskFiles do
                    match matchParsed caseSensitive parsed cand with
                    | ValueSome recDir when not (isExcluded cand) -> subMatches.Add((cand, recDir))
                    | _ -> ()

                for full, recDir in sortWildcardMatches subMatches do
                    if seen.Add(toSlash full) then
                        results.Add((full, recDir))

        List.ofSeq results

    let mergeMetadata (existing: (string * string) list) (updates: (string * string) list) : (string * string) list =
        if updates.IsEmpty then
            existing
        else
            let mutable current = existing

            for uk, uv in updates do
                let mutable replaced = false

                current <-
                    [ for ek, ev in current ->
                          if String.Equals(ek, uk, StringComparison.OrdinalIgnoreCase) then
                              replaced <- true
                              uk, uv
                          else
                              ek, ev ]

                if not replaced then
                    current <- current @ [ uk, uv ]

            current

    let matchExpressions (caseSensitive: bool) (baseDir: string) (expr: string) (candidateFullPath: string) : (bool * string voption) =
        let baseFull = Path.GetFullPath baseDir
        let candFull = Path.GetFullPath candidateFullPath
        let mutable matched = false
        let mutable doubleStarRecDir = ValueNone

        for subPat in splitPatterns expr do
            let parsed = parseSinglePattern baseFull subPat

            match matchParsed caseSensitive parsed candFull with
            | ValueSome recDir ->
                matched <- true

                match parsed with
                | WildcardPath(_, _, true, _) -> doubleStarRecDir <- ValueSome recDir
                | _ -> ()
            | ValueNone -> ()

        matched, doubleStarRecDir

    let applyOpWith
        (caseSensitive: bool)
        (baseDir: string)
        (expandInclude: string -> string option -> (string * string) list)
        (current: EvaluatedItem list)
        (op: ItemOp)
        : EvaluatedItem list * (string * string) list =
        let comparer = if caseSensitive then StringComparer.Ordinal else StringComparer.OrdinalIgnoreCase
        let items = ResizeArray<EvaluatedItem>(current)
        let included = ResizeArray<string * string>()

        match op with
        | Include(incExpr, excOpt, meta) ->
            let expanded = expandInclude incExpr excOpt

            for full, recDir in expanded do
                included.Add((full, recDir))
                let key = toSlash full

                let existingIdx =
                    items |> Seq.tryFindIndex (fun existing -> comparer.Equals(toSlash existing.FullPath, key))

                match existingIdx with
                | None ->
                    items.Add
                        { FullPath = full
                          RecursiveDir = recDir
                          Metadata = meta }
                | Some _ -> ()
        | Remove remExpr ->
            let kept = ResizeArray<EvaluatedItem>(items.Count)

            for existing in items do
                let matched, _ = matchExpressions caseSensitive baseDir remExpr existing.FullPath

                if not matched then
                    kept.Add existing

            items.Clear()
            items.AddRange kept
        | Update(updExpr, metaUpdates) ->
            for i = 0 to items.Count - 1 do
                let existing = items[i]
                let matched, dsRecDir = matchExpressions caseSensitive baseDir updExpr existing.FullPath

                if matched then
                    let nextRecDir =
                        match dsRecDir with
                        | ValueSome r -> r
                        | ValueNone -> existing.RecursiveDir

                    items[i] <-
                        { existing with
                            RecursiveDir = nextRecDir
                            Metadata = mergeMetadata existing.Metadata metaUpdates }

        List.ofSeq items, List.ofSeq included

    let applyOpDisk
        (caseSensitive: bool)
        (baseDir: string)
        (current: EvaluatedItem list)
        (op: ItemOp)
        : EvaluatedItem list * (string * string) list =
        applyOpWith caseSensitive baseDir (fun inc exc -> expandDisk caseSensitive baseDir inc exc) current op

    let private evaluateWith
        (caseSensitive: bool)
        (baseDir: string)
        (expandInclude: string -> string option -> (string * string) list)
        (ops: ItemOp seq)
        : EvaluatedItem list =
        let mutable current = []

        for op in ops do
            let next, _ = applyOpWith caseSensitive baseDir expandInclude current op
            current <- next

        current

    let evaluateCandidates
        (caseSensitive: bool)
        (baseDir: string)
        (candidates: string seq)
        (ops: ItemOp seq)
        : EvaluatedItem list =
        evaluateWith caseSensitive baseDir (fun inc exc -> expandCandidates caseSensitive baseDir inc exc candidates) ops

    let evaluateDisk (caseSensitive: bool) (baseDir: string) (ops: ItemOp seq) : EvaluatedItem list =
        evaluateWith caseSensitive baseDir (fun inc exc -> expandDisk caseSensitive baseDir inc exc) ops
