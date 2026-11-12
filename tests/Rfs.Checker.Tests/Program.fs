module Rfs.Checker.Tests.Program

open System
open System.Reflection
open Expecto

let private filterTree (inFuzz: bool -> bool) (keepLeaf: int -> bool) (root: Test) : Test =
    let mutable leafIndex = 0

    let rec loop (underFuzz: bool) (t: Test) : Test option =
        match t with
        | Test.Sequenced(seqMethod, inner) ->
            loop underFuzz inner |> Option.map (fun kept -> Test.Sequenced(seqMethod, kept))
        | TestLabel(label, inner, focus) ->
            let nowFuzz = underFuzz || label.StartsWith("OracleFuzz", StringComparison.Ordinal)
            loop nowFuzz inner |> Option.map (fun kept -> TestLabel(label, kept, focus))
        | TestList(items, focus) ->
            let kept = items |> List.choose (loop underFuzz)
            if kept.IsEmpty then None else Some(TestList(kept, focus))
        | TestCase _ ->
            if not (inFuzz underFuzz) then
                None
            else
                let idx = leafIndex
                leafIndex <- leafIndex + 1
                if keepLeaf idx then Some t else None

    loop false root |> Option.defaultValue (TestList([], FocusState.Normal))

[<EntryPoint>]
let main argv =
    let rec parseArgs (rest: string list) (noFuzz: bool) (shard: (int * int) option) (forward: string list) =
        match rest with
        | [] -> noFuzz, shard, List.rev forward
        | "--no-fuzz" :: tail -> parseArgs tail true shard forward
        | "--fuzz-shard" :: spec :: tail ->
            match spec.Split '/' with
            | [| kStr; nStr |] ->
                let k, n = int kStr, int nStr

                if k < 1 || n < 1 || k > n then
                    failwithf "Invalid --fuzz-shard '%s' (expected k/n with 1 <= k <= n)" spec

                parseArgs tail noFuzz (Some(k, n)) forward
            | _ -> failwithf "Invalid --fuzz-shard '%s' (expected k/n)" spec
        | arg :: tail -> parseArgs tail noFuzz shard (arg :: forward)

    let noFuzz, shard, expectoArgs = parseArgs ( Array.toList argv ) false None []

    if not noFuzz && shard.IsNone then
        runTestsInAssemblyWithCLIArgs [] argv
    else
        let root =
            Impl.testFromAssembly (Assembly.GetExecutingAssembly())
            |> Option.defaultValue (TestList([], FocusState.Normal))

        let filtered =
            match noFuzz, shard with
            | true, _ -> filterTree not (fun _ -> true) root
            | false, Some(k, n) -> filterTree id (fun idx -> idx % n = k - 1) root
            | false, None -> root

        runTestsWithCLIArgs [] (List.toArray expectoArgs) filtered
