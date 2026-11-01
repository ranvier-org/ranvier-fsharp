namespace Rfs.Projects

open FSharp.Compiler.Diagnostics
open Rfs.Lsp

module FcsDiagnostics =

    let private severity =
        function
        | FSharpDiagnosticSeverity.Error -> 1
        | FSharpDiagnosticSeverity.Warning -> 2
        | FSharpDiagnosticSeverity.Info -> 3
        | FSharpDiagnosticSeverity.Hidden -> 4

    /// FCS lines are 1-based, LSP lines 0-based; columns are 0-based UTF-16 in both.
    let ofFcs (d: FSharpDiagnostic) : Diagnostic =
        { File =
            // GetFullPath rejects an empty name
            if System.String.IsNullOrEmpty d.FileName then
                ""
            else
                LspClient.NormalizePath d.FileName
          Range =
            { Start = { Line = max 0 (d.StartLine - 1); Character = d.StartColumn }
              End = { Line = max 0 (d.EndLine - 1); Character = d.EndColumn } }
          Severity = severity d.Severity
          Code = $"FS%04d{d.ErrorNumber}"
          Message = d.Message
          Source = "F# Compiler" }
