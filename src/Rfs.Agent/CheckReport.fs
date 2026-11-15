namespace Rfs.Agent

open System
open System.Text.RegularExpressions

/// The result of `check`: its text, and the counts that decide whether it passed.
[<NoComparison; NoEquality>]
type internal CheckOutcome =
    { Text: string
      Errors: int
      Warnings: int
      CheckedProjects: int
      /// Projects of the workspace that failed to load. Above zero, the diagnostics cover only part of the workspace.
      FailedProjects: int }

/// The cause of a project load failure, extracted from the MSBuild log that carries it.
module internal LoadFailure =

    let private maxLength = 400

    let private clip (s: string) =
        let t = s.Trim()
        if t.Length > maxLength then t.Substring(0, maxLength - 3) + "..." else t

    let private collapse (s: string) = Regex.Replace(s, @"\s+", " ")

    // `error NETSDK1045: text [C:\dir\App.fsproj]`, as MSBuild prints an error on the console
    let private codedError =
        Regex(@"error\s+[A-Z]+\d+\s*:\s.*?(?=\s\[[^\]]*proj\]|$)", RegexOptions.Compiled)

    // A task that failed in a detailed log, which states its message without an error code
    let private failedTask = Regex(@"Done executing task ""(?<task>\w+)"" -- FAILED", RegexOptions.Compiled)

    /// One line of at most 400 characters: the first coded MSBuild error, else the message of the first failed task,
    /// else the start of `reason`.
    let summarize (reason: string) : string =
        let flat = collapse reason
        let coded = codedError.Match flat

        if coded.Success then
            clip coded.Value
        else
            let failed = failedTask.Match flat

            let taskMessage =
                if failed.Success then
                    let opening = "Task \"" + failed.Groups["task"].Value + "\""
                    let start = flat.LastIndexOf(opening, failed.Index, StringComparison.Ordinal)

                    if start >= 0 then
                        Some(flat.Substring(start + opening.Length, failed.Index - start - opening.Length))
                    else
                        None
                else
                    None

            match taskMessage with
            | Some message when not (String.IsNullOrWhiteSpace message) -> clip message
            | _ -> clip flat

/// The last line of a `check` report and the exit code that goes with it.
module internal CheckSummary =

    /// `N errors, M warnings (…)` when every project loaded. With `failedProjects` above zero the line starts with
    /// `incomplete:`, so a reader of the last line alone cannot take it for a pass.
    let line
        (errors: int)
        (warnings: int)
        (projects: int)
        (files: int)
        (elapsedMs: int64)
        (declarationsRechecked: int64)
        (filesRechecked: int64)
        (failedProjects: int)
        : string =
        let detail =
            $"checked %d{projects} project(s), %d{files} file(s) in %d{elapsedMs}ms; rechecked %d{declarationsRechecked} declaration(s) and %d{filesRechecked} file(s), the rest unchanged"

        if failedProjects > 0 then
            $"incomplete: %d{failedProjects} project(s) failed to load; %d{errors} errors, %d{warnings} warnings in the rest (%s{detail})"
        else
            $"%d{errors} errors, %d{warnings} warnings (%s{detail})"

    /// 1 when a project failed to load, whatever the flags; 1 for errors under `--fail-on-error`; otherwise 0.
    let exitCode (failOnError: bool) (errors: int) (failedProjects: int) : int =
        if failedProjects > 0 then 1
        elif failOnError && errors > 0 then 1
        else 0
