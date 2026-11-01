namespace Rfs.Lsp

open System
open System.Text.Json.Nodes

module Diagnostics =

    /// Analyzer and linter diagnostics are not the oracle's business: only compiler diagnostics are compared.
    let isCompiler (d: Diagnostic) =
        d.Source = "F# Compiler"
        || (d.Code.Length > 2
            && d.Code.StartsWith("FS", StringComparison.Ordinal)
            && Seq.forall Char.IsAsciiDigit (d.Code.Substring 2))

    let private normalise (file: string) =
        try
            Protocol.normalizePath file
        with _ ->
            file

    /// FSAC sends the compiler's number as an integer, which the client reads as "1"; `ofFcs` spells it FS0001.
    let private normaliseCode (code: string) =
        match Int32.TryParse code with
        | true, n when code |> Seq.forall Char.IsAsciiDigit -> $"FS%04d{n}"
        | _ -> code

    /// FSAC leaves the severity out for Hidden diagnostics, which the client reads as 0; Hidden is 4.
    let private normaliseSeverity (severity: int) = if severity = 0 then 4 else severity

    /// FCS separates the paragraphs of a message with U+001D; FSAC turns it into the platform's newline.
    let private normaliseMessage (message: string) =
        message.Replace("\u001d", "\n").Replace("\r\n", "\n")

    /// Source is not part of the key: it names the server that produced the diagnostic.
    let private key (d: Diagnostic) =
        $"%s{normalise d.File} %d{d.Range.Start.Line}:%d{d.Range.Start.Character}-%d{d.Range.End.Line}:%d{d.Range.End.Character} sev=%d{normaliseSeverity d.Severity} %s{normaliseCode d.Code} %s{normaliseMessage d.Message}"

    /// One line per difference, "missing: ..." or "unexpected: ..."; empty means equal. Duplicates count.
    let compare (expected: Diagnostic list) (actual: Diagnostic list) : string list =
        let counts items =
            items |> List.filter isCompiler |> List.countBy key |> Map.ofList

        let e, a = counts expected, counts actual

        let surplus (label: string) (left: Map<string, int>) (right: Map<string, int>) =
            [ for KeyValue(k, n) in left do
                  let extra = n - (right |> Map.tryFind k |> Option.defaultValue 0)

                  for _ in 1..extra -> $"%s{label}: %s{k}" ]

        surplus "missing" e a @ surplus "unexpected" a e

    /// LSP's diagnostic shape plus a `file`, so a list of them can travel without a publishDiagnostics envelope.
    let toJson (d: Diagnostic) : JsonNode =
        Protocol.obj' [ "file", Protocol.str d.File
                        "range", Protocol.rangeToJson d.Range :> JsonNode
                        "severity", JsonValue.Create d.Severity :> JsonNode
                        "code", Protocol.str d.Code
                        "message", Protocol.str d.Message
                        "source", Protocol.str d.Source ]
        :> JsonNode

    let ofJson (node: JsonNode) : Diagnostic =
        Protocol.diagnostic (node["file"].GetValue<string>()) node
