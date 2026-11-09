namespace Rfs.Server

open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Declarations
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

/// <summary>The parameters of <c>rfs/why</c>: a position in an implementation file.</summary>
type WhyParams =
    { TextDocument: TextDocumentIdentifier
      Position: Position }

/// <summary>What <c>rfs/why</c> answers for the declaration containing the position.</summary>
/// <remarks>
/// <c>Chain</c> is Ranvier's rendering of the cause chain of the declaration's last check, and is empty in an untraced
/// build, where <c>Traced</c> is false.
/// </remarks>
type WhyAnswer =
    { Traced: bool
      Declaration: string
      Chain: string }

/// <summary>
/// <c>rfs/why</c>: why the declaration at a position was checked again, from the graph's trace. Ranvier's own, so only a
/// build with RANVIER_TRACE (Debug by default) has one.
/// </summary>
/// <remarks>
/// Nothing comes back (JSON null) for a position in no declaration, in a signature file, or in a file the project does not
/// hold, and, in a traced build, for a declaration with no node to ask: the file is checked whole, or the declaration is
/// not one of its rows yet, or its check has not run. An untraced build answers every declaration with
/// <c>Traced = false</c> and an empty chain.
/// </remarks>
module Why =
    /// The key of the smallest declaration of the file's current parse that contains the 0-based line and character: a
    /// request node on the file (ProjectGraph.ReadFile), run again on a new text
    let private declarationAt (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<DeclarationKey option> =
        graph.ReadFile(
            path,
            (fun view ->
                let before (l1, c1) (l2, c2) = l1 < l2 || (l1 = l2 && c1 <= c2)

                DeclarationChecker.Split view.Parse
                |> Array.filter (fun d ->
                    let r = d.Range
                    before (r.StartLine, r.StartColumn) (line + 1, character) && before (line + 1, character) (r.EndLine, r.EndColumn))
                |> Array.sortBy (fun d -> d.Range.EndLine - d.Range.StartLine)
                |> Array.tryHead
                |> Option.map _.Key),
            ct
        )

    let at (graph: ProjectGraph) (path: string) (line: int) (character: int) (ct: CancellationToken) : Task<WhyAnswer option> =
        task {
            match! declarationAt graph path line character ct with
            | None -> return None
            | Some key ->
                let declaration = ProjectGraph.DescribeKey key

                if not ProjectGraph.Traced then
                    return Some { Traced = false; Declaration = declaration; Chain = "" }
                else
                    let! chain = graph.Why(path, key)

                    return
                        chain
                        |> Option.map (fun c -> { Traced = true; Declaration = declaration; Chain = c })
        }
