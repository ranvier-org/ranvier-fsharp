module Rfs.Server.Folding

open System.Runtime.ExceptionServices
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.EditorServices
open FSharp.Compiler.Syntax
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

let private onLargeStack (f: unit -> 'T) : 'T =
    let result = ref ValueNone
    let failure = ref null

    let thread =
        Thread(
            (fun () ->
                try
                    result.Value <- ValueSome(f ())
                with ex ->
                    failure.Value <- ExceptionDispatchInfo.Capture ex),
            GraphHost.StackSize,
            IsBackground = true
        )

    thread.Start()
    thread.Join()

    if not (isNull failure.Value) then
        failure.Value.Throw()

    result.Value.Value

/// Matches FSAC's `LspHelpers.Structure.scopeToKind`: Open -> Imports, Comment/XmlDocComment -> Comment, all other scopes -> None.
let private scopeToKind (scope: Structure.Scope) : string option =
    match scope with
    | Structure.Scope.Open -> Some FoldingRangeKind.Imports
    | Structure.Scope.Comment
    | Structure.Scope.XmlDocComment -> Some FoldingRangeKind.Comment
    | Structure.Scope.Namespace
    | Structure.Scope.Module
    | Structure.Scope.Type
    | Structure.Scope.Member
    | Structure.Scope.LetOrUse
    | Structure.Scope.Val
    | Structure.Scope.ComputationExpr
    | Structure.Scope.IfThenElse
    | Structure.Scope.ThenInIfThenElse
    | Structure.Scope.ElseInIfThenElse
    | Structure.Scope.TryWith
    | Structure.Scope.TryInTryWith
    | Structure.Scope.WithInTryWith
    | Structure.Scope.TryFinally
    | Structure.Scope.TryInTryFinally
    | Structure.Scope.FinallyInTryFinally
    | Structure.Scope.ArrayOrList
    | Structure.Scope.ObjExpr
    | Structure.Scope.For
    | Structure.Scope.While
    | Structure.Scope.Match
    | Structure.Scope.MatchBang
    | Structure.Scope.MatchLambda
    | Structure.Scope.MatchClause
    | Structure.Scope.Lambda
    | Structure.Scope.Quote
    | Structure.Scope.Record
    | Structure.Scope.SpecialFunc
    | Structure.Scope.Do
    | Structure.Scope.New
    | Structure.Scope.Attribute
    | Structure.Scope.Interface
    | Structure.Scope.HashDirective
    | Structure.Scope.LetOrUseBang
    | Structure.Scope.TypeExtension
    | Structure.Scope.YieldOrReturn
    | Structure.Scope.YieldOrReturnBang
    | Structure.Scope.Tuple
    | Structure.Scope.UnionCase
    | Structure.Scope.EnumCase
    | Structure.Scope.RecordField
    | Structure.Scope.RecordDefn
    | Structure.Scope.UnionDefn -> None

let private toFoldingRange (item: Structure.ScopeRange) : FoldingRange =
    let r = item.CollapseRange

    { StartLine = uint32 (max 0 (r.StartLine - 1))
      StartCharacter = Some(uint32 (max 0 r.StartColumn))
      EndLine = uint32 (max 0 (r.EndLine - 1))
      EndCharacter = Some(uint32 (max 0 r.EndColumn))
      Kind = scopeToKind item.Scope
      CollapsedText = None }

let internal compute (text: string) (parseTree: ParsedInput) : FoldingRange[] =
    onLargeStack (fun () ->
        let lines = text.Split('\n') |> Array.map (fun s -> s.TrimEnd('\r'))

        Structure.getOutliningRanges lines parseTree
        |> Seq.map toFoldingRange
        |> Seq.distinct
        |> Seq.toArray)

/// Pure-syntax folding ranges for `.fs` and `.fsi` via `ProjectGraph.LatestParse` (never waits on typechecking).
let ranges (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task<FoldingRange[] option> =
    task {
        let! textView = graph.LatestParse(path, ct)
        return Some(compute textView.Text textView.Parse.ParseTree)
    }
