module Rfs.Lsp.Tests.DocumentTests

open Expecto
open Rfs.Lsp

let private edit (sl, sc) (el, ec) text : Edit =
    { Range =
        { Start = { Line = sl; Character = sc }
          End = { Line = el; Character = ec } }
      Text = text }

[<Tests>]
let tests =
    testList
        "Document"
        [ test "apply insert in CRLF file" {
              Expect.equal (Document.apply "a\r\nb" (edit (1, 0) (1, 0) "X")) "a\r\nXb" "inserted at the start of line 1"
          }

          test "apply keeps the CR out of the line length" {
              // a column past the end of line 0 clamps before the \r
              Expect.equal (Document.apply "a\r\nb" (edit (0, 9) (0, 9) "X")) "aX\r\nb" "clamped before CRLF"
          }

          test "apply uses UTF-16 columns" {
              // the surrogate pair counts as 2, so column 3 is after the space
              Expect.equal (Document.apply "𝑥 = 1" (edit (0, 3) (0, 3) "X")) "𝑥 X= 1" "after the space"
          }

          test "apply replace across lines" {
              Expect.equal (Document.apply "one\ntwo\nthree" (edit (0, 2) (2, 2) "-")) "on-ree" "joined"
          }

          test "apply delete a line" {
              Expect.equal (Document.apply "a\nb\nc" (edit (1, 0) (2, 0) "")) "a\nc" "line removed"
          }

          test "apply past the end appends" {
              Expect.equal (Document.apply "a\n" (edit (1, 0) (1, 0) "b")) "a\nb" "empty last line"
              Expect.equal (Document.apply "a" (edit (5, 0) (5, 0) "b")) "ab" "clamped to the end"
          } ]
