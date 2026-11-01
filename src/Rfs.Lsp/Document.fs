namespace Rfs.Lsp

/// A zero-based position; Character counts UTF-16 code units, like a .NET string index.
type Position = { Line: int; Character: int }

type Range = { Start: Position; End: Position }

type Edit = { Range: Range; Text: string }

module Document =
    let private lineStart (text: string) (line: int) =
        let mutable offset = 0
        let mutable current = 0

        while current < line && offset < text.Length do
            match text.IndexOf('\n', offset) with
            | -1 -> offset <- text.Length
            | newline -> offset <- newline + 1

            current <- current + 1

        offset

    /// Lines end at '\n'; a '\r' right before it belongs to the terminator, so a column never lands after it.
    let private offsetOf (text: string) (position: Position) =
        let start = lineStart text position.Line

        let contentEnd =
            match text.IndexOf('\n', start) with
            | -1 -> text.Length
            | newline when newline > start && text[newline - 1] = '\r' -> newline - 1
            | newline -> newline

        min (start + max 0 position.Character) contentEnd

    let apply (text: string) (edit: Edit) : string =
        let first = offsetOf text edit.Range.Start
        let last = max first (offsetOf text edit.Range.End)
        text.Substring(0, first) + edit.Text + text.Substring last
