namespace Rfs.Projects

open FSharp.Compiler.Diagnostics
open Rfs.Lsp

module Diagnostics =

    let private severity =
        function
        | FSharpDiagnosticSeverity.Error -> 1
        | FSharpDiagnosticSeverity.Warning -> 2
        | FSharpDiagnosticSeverity.Info -> 3
        | FSharpDiagnosticSeverity.Hidden -> 4

    let private file (name: string) =
        // project-level diagnostics carry no real file name
        try
            LspClient.NormalizePath name
        with _ ->
            name

    /// FCS lines are 1-based, LSP lines 0-based; columns are 0-based UTF-16 in both.
    let ofFcs (d: FSharpDiagnostic) : Diagnostic =
        { File = file d.FileName
          Range =
            { Start = { Line = max 0 (d.StartLine - 1); Character = d.StartColumn }
              End = { Line = max 0 (d.EndLine - 1); Character = d.EndColumn } }
          Severity = severity d.Severity
          Code = $"FS%04d{d.ErrorNumber}"
          Message = d.Message
          Source = "F# Compiler" }
