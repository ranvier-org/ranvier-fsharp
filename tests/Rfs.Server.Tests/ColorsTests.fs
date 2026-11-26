module Rfs.Server.Tests.ColorsTests

open System
open System.IO
open System.Text.Json.Nodes
open System.Threading.Tasks
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Lsp
open Rfs.Server

type private Range = Ionide.LanguageServerProtocol.Types.Range

let private within (task: Task<'T>) = task.WaitAsync(TimeSpan.FromSeconds 60.)
let private withinUnit (task: Task) = task.WaitAsync(TimeSpan.FromSeconds 60.)

let private parseChecker = lazy (FSharpChecker.Create(keepAssemblyContents = false))

let private parse (source: string) =
    let opts = { FSharpParsingOptions.Default with SourceFiles = [| "C.fs" |] }
    (parseChecker.Value.ParseFile("C.fs", SourceText.ofString source, opts) |> Async.RunSynchronously).ParseTree

let private slice (source: string) (r: Range) =
    let lines = source.Split '\n'
    let first, last = int r.Start.Line, int r.End.Line

    if first = last then
        lines[first].Substring(int r.Start.Character, int r.End.Character - int r.Start.Character)
    else
        String.concat
            "\n"
            [ for l in first..last ->
                  if l = first then lines[l].Substring(int r.Start.Character)
                  elif l = last then lines[l].Substring(0, int r.End.Character)
                  else lines[l] ]

let private toByte (d: decimal) = int (Math.Round(float d * 255.0))
let private bytes (c: Color) = toByte c.Red, toByte c.Green, toByte c.Blue, toByte c.Alpha

let private source (body: string list) = String.concat "\n" ("module C" :: "" :: body)

let private on = ColorsConfig.Default

let private colours (config: ColorsConfig) (text: string) =
    Colors.find config text (parse text)
    |> Array.map (fun i -> slice text i.Range, bytes i.Color)
    |> Array.toList

let private spans config text = colours config text |> List.map fst

let private present (config: ColorsConfig) (text: string) (span: string) (r, g, b, a) =
    let tree = parse text

    let info =
        Colors.find config text tree
        |> Array.tryFind (fun i -> slice text i.Range = span)
        |> Option.defaultWith (fun () -> failwithf "no colour found at %s" span)

    let color =
        { Red = decimal r / 255m
          Green = decimal g / 255m
          Blue = decimal b / 255m
          Alpha = decimal a / 255m }

    Colors.presentations config text tree info.Range color
    |> Array.map (fun p ->
        match p.TextEdit with
        | Some e -> e.NewText
        | None -> p.Label)
    |> Array.toList

[<Tests>]
let detectionTests =
    testList
        "Colors.Detection"
        [ testCase "hex strings with 3, 4, 6 and 8 digits are colours; other strings are not" (fun () ->
              let text =
                  source
                      [ "let a = \"#fbbf24\""
                        "let b = \"#ABC\""
                        "let c = \"#11223380\""
                        "let d = \"fbbf24\""
                        "let e = \"#12345\""
                        "let f = \"see #fbbf24 here\""
                        "let g = @\"#00ff00\"" ]

              Expect.equal
                  (colours on text)
                  [ "#fbbf24", (251, 191, 36, 255)
                    "#ABC", (0xAA, 0xBB, 0xCC, 255)
                    "#11223380", (0x11, 0x22, 0x33, 0x80)
                    "#00ff00", (0, 255, 0, 255) ]
                  "only whole-string hex colours are found, and the range covers the colour inside the quotes"

              Expect.equal
                  (colours { on with HexByteOrder = HexByteOrder.Argb } text
                   |> List.find (fun (s, _) -> s = "#11223380"))
                  ("#11223380", (0x22, 0x33, 0x80, 0x11))
                  "argb byte order reads the first byte of 8 digits as alpha")

          testCase "stringsWithoutHash admits 6 and 8 bare digits, never 3 or 4" (fun () ->
              let text =
                  source
                      [ "let a = \"fbbf24\""
                        "let b = \"11223380\""
                        "let c = \"cafe\""
                        "let d = \"bad\""
                        "let e = \"#abc\"" ]

              Expect.equal (spans on text) [ "#abc" ] "off by default"

              Expect.equal
                  (colours { on with StringsWithoutHash = true } text)
                  [ "fbbf24", (251, 191, 36, 255)
                    "11223380", (0x11, 0x22, 0x33, 0x80)
                    "#abc", (0xAA, 0xBB, 0xCC, 255) ]
                  "bare words of 3 or 4 hex letters stay plain strings")

          testCase "hex integers: 6 digits anywhere, 8 digits under a colour name" (fun () ->
              let text =
                  source
                      [ "let a = 0xfbbf24"
                        "let accentColour = 0xFBBF2480u"
                        "let c = 0xffff"
                        "let d = 16777215"
                        "let e = 0xFB_BF_24"
                        "let f = 0x00ff00L"
                        "let crcPolynomial = 0xEDB88320u"
                        "let mask = 0xFFFF_FFFFu" ]

              Expect.equal
                  (colours on text)
                  [ "0xfbbf24", (251, 191, 36, 255)
                    "0xFBBF2480u", (0xFB, 0xBF, 0x24, 0x80)
                    "0xFB_BF_24", (251, 191, 36, 255)
                    "0x00ff00L", (0, 255, 0, 255) ]
                  "decimal integers, other widths, and 8 digits without a colour name are not colours"

              Expect.equal
                  (colours { on with HexByteOrder = HexByteOrder.Argb } text |> List.item 1)
                  ("0xFBBF2480u", (0xBF, 0x24, 0x80, 0xFB))
                  "argb byte order applies to hex integers")

          testCase "a name containing argb or rgba overrides the byte order setting" (fun () ->
              let text =
                  source
                      [ "let argbAccent = 0x80FBBF24"
                        "let c = Color.FromArgb(0x80FBBF24)"
                        "let rgbaAccent = 0xFBBF2480" ]

              Expect.equal
                  (colours on text)
                  [ "0x80FBBF24", (0xFB, 0xBF, 0x24, 0x80)
                    "0x80FBBF24", (0xFB, 0xBF, 0x24, 0x80)
                    "0xFBBF2480", (0xFB, 0xBF, 0x24, 0x80) ]
                  "argb names read alpha first under the rgba default"

              Expect.equal
                  (colours { on with HexByteOrder = HexByteOrder.Argb } text |> List.last)
                  ("0xFBBF2480", (0xFB, 0xBF, 0x24, 0x80))
                  "rgba names read alpha last under the argb setting")

          testCase "named colour members under a Color, Colors, Colour or Palette qualifier" (fun () ->
              let text =
                  source
                      [ "let a = Palette.red"
                        "let b = Colors.RebeccaPurple"
                        "let c = Css.Color.rebeccapurple"
                        "let d = Colour.darkgrey"
                        "let e = Color.activeBorder"
                        "let f = Foo.red"
                        "let g = Colors.Transparent" ]

              Expect.equal
                  (colours on text)
                  [ "Palette.red", (255, 0, 0, 255)
                    "Colors.RebeccaPurple", (0x66, 0x33, 0x99, 255)
                    "Css.Color.rebeccapurple", (0x66, 0x33, 0x99, 255)
                    "Colour.darkgrey", (0xA9, 0xA9, 0xA9, 255)
                    "Colors.Transparent", (255, 255, 255, 0) ]
                  "system colours and other qualifiers are not colours")

          testCase "records and anonymous records with R G B (A) or Red Green Blue (Alpha) fields" (fun () ->
              let text =
                  source
                      [ "let a = { R = 251uy; G = 191uy; B = 36uy }"
                        "let b = {| Red = 0.5; Green = 0.25; Blue = 1.0; Alpha = 0.5 |}"
                        "let c = { R = 1; X = 2; B = 3 }"
                        "let d = { B = 128.0; G = 64.0; R = 255.0 }"
                        "let e = { a with R = 1uy }"
                        "let f = {"
                        "    R = 0.2f"
                        "    G = 0.4f"
                        "    B = 0.6f"
                        "    A = 1.0f }" ]

              Expect.equal
                  (colours on text)
                  [ "{ R = 251uy; G = 191uy; B = 36uy }", (251, 191, 36, 255)
                    "{| Red = 0.5; Green = 0.25; Blue = 1.0; Alpha = 0.5 |}", (128, 64, 255, 128)
                    "{ B = 128.0; G = 64.0; R = 255.0 }", (255, 64, 128, 255)
                    "{\n    R = 0.2f\n    G = 0.4f\n    B = 0.6f\n    A = 1.0f }", (51, 102, 153, 255) ]
                  "floats above 1 are byte scale, copy-and-update records are skipped")

          testCase "field names match in any case, in records, anonymous records and struct anonymous records" (fun () ->
              let text =
                  source
                      [ "let a = { r = 251uy; g = 191uy; b = 36uy }"
                        "let b = {| RED = 255; green = 0; Blue = 0; ALPHA = 128 |}"
                        "let c = struct {| R = 0.2; G = 0.4; B = 0.6 |}"
                        "let d = {| Name = \"accent\"; Color = (251, 191, 36) |}"
                        "let ACCENT_RGB = (1, 2, 3)" ]

              Expect.equal
                  (colours on text)
                  [ "{ r = 251uy; g = 191uy; b = 36uy }", (251, 191, 36, 255)
                    "{| RED = 255; green = 0; Blue = 0; ALPHA = 128 |}", (255, 0, 0, 128)
                    "struct {| R = 0.2; G = 0.4; B = 0.6 |}", (51, 102, 153, 255)
                    "(251, 191, 36)", (251, 191, 36, 255)
                    "(1, 2, 3)", (1, 2, 3, 255) ]
                  "lowercase, uppercase and mixed-case fields and names are all recognised")

          testCase "floatScale: auto, normalized or byte" (fun () ->
              let text =
                  source
                      [ "let a = { R = 255.0; G = 64.0; B = 128.0 }"
                        "let b = RgbaColor(1.0f, 1.0f, 1.0f, 1.0f)"
                        "let c = { R = 255; G = 0.8; B = 0 }" ]

              Expect.equal
                  (colours on text)
                  [ "{ R = 255.0; G = 64.0; B = 128.0 }", (255, 64, 128, 255)
                    "RgbaColor(1.0f, 1.0f, 1.0f, 1.0f)", (255, 255, 255, 255)
                    "{ R = 255; G = 0.8; B = 0 }", (255, 204, 0, 255) ]
                  "auto: floats at most 1 are normalised, larger ones byte scale"

              Expect.equal
                  (colours { on with FloatScale = FloatScale.Normalized } text)
                  [ "RgbaColor(1.0f, 1.0f, 1.0f, 1.0f)", (255, 255, 255, 255)
                    "{ R = 255; G = 0.8; B = 0 }", (255, 204, 0, 255) ]
                  "normalized: a float above 1 is not a channel"

              Expect.equal
                  (colours { on with FloatScale = FloatScale.Byte } text)
                  [ "{ R = 255.0; G = 64.0; B = 128.0 }", (255, 64, 128, 255)
                    "RgbaColor(1.0f, 1.0f, 1.0f, 1.0f)", (1, 1, 1, 1)
                    "{ R = 255; G = 0.8; B = 0 }", (255, 1, 0, 255) ]
                  "byte: every float is 0-255, integers are unaffected")

          testCase "floatScale is read from FSharp.colors.floatScale" (fun () ->
              let read (json: string) =
                  (FSharpConfig.UpdateFromJson FSharpConfig.Default (JToken.Parse json)).Colors.FloatScale

              Expect.equal FSharpConfig.Default.Colors.FloatScale FloatScale.Auto "auto by default"
              Expect.equal (read """{"FSharp":{"colors":{"floatScale":"Byte"}}}""") FloatScale.Byte "byte"
              Expect.equal (read """{"FSharp":{"colors":{"floatScale":"normalised"}}}""") FloatScale.Normalized "normalised"
              Expect.equal (read """{"FSharp":{"colors":{"floatScale":"normalized"}}}""") FloatScale.Normalized "normalized"
              Expect.equal (read """{"FSharp":{"colors":{"floatScale":"other"}}}""") FloatScale.Auto "unknown keeps the current")

          testCase "constructor and function calls whose name says colour or rgb" (fun () ->
              let text =
                  source
                      [ "let a = RgbaColor(0.2f, 0.4f, 0.6f, 1.0f)"
                        "let b = new RgbaColor(0.2f, 0.4f, 0.6f, 0.5f)"
                        "let c = Color.FromArgb(128, 255, 0, 0)"
                        "let d = RgbaColor.fromRgb24 251uy 191uy 36uy"
                        "let e = RgbaColor.create 0.2f 0.4f 0.6f 1.0f"
                        "let f = Vector4(1.0f, 0.5f, 0.2f, 1.0f)"
                        "let g = Color.FromRgb(300, 0, 0)" ]

              Expect.equal
                  (colours on text)
                  [ "RgbaColor(0.2f, 0.4f, 0.6f, 1.0f)", (51, 102, 153, 255)
                    "new RgbaColor(0.2f, 0.4f, 0.6f, 0.5f)", (51, 102, 153, 128)
                    "Color.FromArgb(128, 255, 0, 0)", (255, 0, 0, 128)
                    "RgbaColor.fromRgb24 251uy 191uy 36uy", (251, 191, 36, 255)
                    "RgbaColor.create 0.2f 0.4f 0.6f 1.0f", (51, 102, 153, 255) ]
                  "one colour per call, argb names put alpha first, out-of-range channels are rejected")

          testCase "tuples bound to, or passed as, a name that says colour or rgb" (fun () ->
              let text =
                  source
                      [ "let accentColor = (251, 191, 36)"
                        "let rgb = 0.2, 0.4, 0.6"
                        "let colours = [ (255, 0, 0); (0, 255, 0, 128) ]"
                        "let size = (251, 191, 36)"
                        "let swatch = { Name = \"x\"; Color = struct (0, 0, 255) }"
                        "let tooBig = { Name = \"x\"; Color = (300, 0, 0) }"
                        "let p = paint (color = (10, 20, 30))" ]

              Expect.equal
                  (colours on text)
                  [ "(251, 191, 36)", (251, 191, 36, 255)
                    "0.2, 0.4, 0.6", (51, 102, 153, 255)
                    "(255, 0, 0)", (255, 0, 0, 255)
                    "(0, 255, 0, 128)", (0, 255, 0, 128)
                    "struct (0, 0, 255)", (0, 0, 255, 255)
                    "(10, 20, 30)", (10, 20, 30, 255) ]
                  "names without colour or rgb are not colours")

          testCase "each setting turns its source off, and enabled=false turns all off" (fun () ->
              let text =
                  source
                      [ "let a = \"#fbbf24\""
                        "let b = 0xfbbf24"
                        "let c = Palette.red"
                        "let d = { R = 1uy; G = 2uy; B = 3uy }"
                        "let e = RgbaColor(0.2f, 0.4f, 0.6f, 1.0f)"
                        "let accentColor = (1, 2, 3)" ]

              Expect.hasLength (spans on text) 6 "all sources on by default"
              Expect.isEmpty (spans { on with Enabled = false } text) "enabled=false"
              Expect.isFalse (spans { on with Strings = false } text |> List.contains "#fbbf24") "strings off"
              Expect.isFalse (spans { on with HexIntegers = false } text |> List.contains "0xfbbf24") "hex integers off"
              Expect.isFalse (spans { on with Members = false } text |> List.contains "Palette.red") "members off"
              Expect.isFalse (spans { on with Records = false } text |> List.exists (fun s -> s.StartsWith "{")) "records off"

              Expect.isFalse
                  (spans { on with Constructors = false } text |> List.exists (fun s -> s.StartsWith "RgbaColor"))
                  "constructors off"

              Expect.isFalse (spans { on with Tuples = false } text |> List.contains "(1, 2, 3)") "tuples off")

          testCase "FSharp.colors settings are read case-insensitively and merge over defaults" (fun () ->
              let config =
                  FSharpConfig.UpdateFromJson
                      FSharpConfig.Default
                      (JToken.Parse """{"FSharp":{"colors":{"enabled":false,"hexByteOrder":"ARGB","tuples":false}}}""")

              Expect.equal
                  config.Colors
                  { ColorsConfig.Default with
                      Enabled = false
                      HexByteOrder = HexByteOrder.Argb
                      Tuples = false }
                  "only the given settings change"

              Expect.equal FSharpConfig.Default.Colors.HexByteOrder HexByteOrder.Rgba "rgba is the default byte order") ]

[<Tests>]
let presentationTests =
    testList
        "Colors.Presentation"
        [ testCase "hex strings keep their case and gain alpha digits only when alpha is below 1" (fun () ->
              let text = source [ "let a = \"#fbbf24\""; "let b = \"#FBBF24\""; "let c = \"#abc\"" ]
              Expect.equal (present on text "#fbbf24" (255, 0, 0, 255)) [ "#ff0000" ] "lowercase, opaque"
              Expect.equal (present on text "#FBBF24" (255, 0, 0, 255)) [ "#FF0000" ] "uppercase kept"
              Expect.equal (present on text "#abc" (0x11, 0x22, 0x33, 255)) [ "#112233" ] "short form widens"
              Expect.equal (present on text "#fbbf24" (255, 0, 0, 128)) [ "#ff000080" ] "rgba alpha last"

              Expect.equal
                  (present { on with HexByteOrder = HexByteOrder.Argb } text "#fbbf24" (255, 0, 0, 128))
                  [ "#80ff0000" ]
                  "argb alpha first")

          testCase "a string without the # stays without it" (fun () ->
              let bare = { on with StringsWithoutHash = true }
              let text = source [ "let a = \"fbbf24\""; "let b = \"FBBF2480\"" ]
              Expect.equal (present bare text "fbbf24" (255, 0, 0, 255)) [ "ff0000" ] "opaque"
              Expect.equal (present bare text "fbbf24" (255, 0, 0, 128)) [ "ff000080" ] "alpha added, still no #"
              Expect.equal (present bare text "FBBF2480" (1, 2, 3, 4)) [ "01020304" ] "eight digits")

          testCase "hex integers keep their prefix, case and suffix" (fun () ->
              let text = source [ "let a = 0xfbbf24"; "let tintRgba = 0XFBBF2480u" ]
              Expect.equal (present on text "0xfbbf24" (255, 0, 0, 255)) [ "0xff0000" ] "six digits stay six"
              Expect.equal (present on text "0xfbbf24" (255, 0, 0, 128)) [ "0xff000080" ] "alpha widens to eight"
              Expect.equal (present on text "0XFBBF2480u" (1, 2, 3, 4)) [ "0X01020304u" ] "eight digits keep suffix")

          testCase "channel literals keep their type suffix and scale" (fun () ->
              let text =
                  source
                      [ "let a = { R = 251uy; G = 191uy; B = 36uy }"
                        "let b = {| Red = 0.5; Green = 0.25; Blue = 1.0; Alpha = 0.5 |}"
                        "let c = RgbaColor(0.2f, 0.4f, 0.6f, 1.0f)"
                        "let d = RgbaColor.fromRgb24 251uy 191uy 36uy"
                        "let e = Color.FromArgb(128, 255, 0, 0)"
                        "let accentColor = (251, 191, 36)"
                        "let f = { R = 255.0; G = 64.0; B = 0x80 }" ]

              Expect.equal
                  (present on text "{ R = 251uy; G = 191uy; B = 36uy }" (1, 2, 3, 255))
                  [ "{ R = 1uy; G = 2uy; B = 3uy }" ]
                  "byte record"

              Expect.equal
                  (present on text "{| Red = 0.5; Green = 0.25; Blue = 1.0; Alpha = 0.5 |}" (255, 0, 51, 255))
                  [ "{| Red = 1.0; Green = 0.0; Blue = 0.2; Alpha = 1.0 |}" ]
                  "normalised float record"

              Expect.equal
                  (present on text "RgbaColor(0.2f, 0.4f, 0.6f, 1.0f)" (255, 128, 0, 255))
                  [ "RgbaColor(1.0f, 0.502f, 0.0f, 1.0f)" ]
                  "float32 constructor"

              Expect.equal
                  (present on text "RgbaColor.fromRgb24 251uy 191uy 36uy" (1, 2, 3, 255))
                  [ "RgbaColor.fromRgb24 1uy 2uy 3uy" ]
                  "curried call"

              Expect.equal
                  (present on text "Color.FromArgb(128, 255, 0, 0)" (0, 0, 255, 255))
                  [ "Color.FromArgb(255, 0, 0, 255)" ]
                  "argb call writes alpha first"

              Expect.equal (present on text "(251, 191, 36)" (0, 0, 0, 255)) [ "(0, 0, 0)" ] "tuple"

              Expect.equal
                  (present on text "{ R = 255.0; G = 64.0; B = 0x80 }" (10, 20, 255, 255))
                  [ "{ R = 10.0; G = 20.0; B = 0xFF }" ]
                  "byte-scale floats stay whole, hex channels stay hex")

          testCase "floatScale=byte writes floats back on the 0-255 scale" (fun () ->
              let text = source [ "let a = { R = 0.5; G = 0.5; B = 0.5 }" ]

              Expect.equal
                  (present { on with FloatScale = FloatScale.Byte } text "{ R = 0.5; G = 0.5; B = 0.5 }" (10, 20, 30, 255))
                  [ "{ R = 10.0; G = 20.0; B = 30.0 }" ]
                  "byte scale"

              Expect.equal
                  (present on text "{ R = 0.5; G = 0.5; B = 0.5 }" (255, 0, 51, 255))
                  [ "{ R = 1.0; G = 0.0; B = 0.2 }" ]
                  "auto keeps these normalised")

          testCase "named members snap to the nearest named colour in the member's casing" (fun () ->
              let text = source [ "let a = Palette.red"; "let b = Colors.Red" ]
              Expect.equal (present on text "Palette.red" (0xFF, 0x64, 0x47, 255)) [ "Palette.tomato" ] "lowercase"
              Expect.equal (present on text "Colors.Red" (0xFF, 0x64, 0x47, 255)) [ "Colors.Tomato" ] "PascalCase") ]

[<Tests>]
let hoverTests =
    let rgbaOf (bytes: int list) = bytes |> List.map (fun b -> float b / 255.0) |> List.toArray

    testList
        "Colors.Hover"
        [ testCase "the colour literal under the cursor" (fun () ->
              let text = source [ "let a = \"#fbbf24\""; "let b = 1" ]
              let tree = parse text
              let at line character = Colors.literalAt on text tree line character |> Option.map (Array.map (fun v -> int (Math.Round(v * 255.0))))
              Expect.equal (at 2 10) (Some [| 251; 191; 36; 255 |]) "inside the literal"
              Expect.equal (at 3 8) None "elsewhere")

          testCase "the colour a declaration binds: one colour as the body, a literal, or a one-argument call" (fun () ->
              let lines =
                  [ "let accent = \"#fbbf24\""
                    "let gas = hex \"#e3c58a\""
                    "let rgb = { R = 251uy; G = 191uy; B = 36uy }"
                    "let accentRgb = (251, 191, 36)"
                    "let planets = [ \"#a8a29e\"; \"#fbbf24\" ]"
                    "let label = \"accent\""
                    "let typed: string = \"#60a5fa\"" ]

              let text = source lines
              let tree = parse text

              let bound (name: string) =
                  let line = lines |> List.findIndex (fun l -> l.StartsWith("let " + name))
                  let start = Position.mkPos (line + 3) 4
                  let declaration = Range.mkRange "C.fs" start (Position.mkPos (line + 3) (4 + name.Length))
                  Colors.boundAt on text tree declaration |> Option.map (Array.map (fun v -> int (Math.Round(v * 255.0))))

              Expect.equal (bound "accent") (Some [| 251; 191; 36; 255 |]) "string literal"
              Expect.equal (bound "gas") (Some [| 0xE3; 0xC5; 0x8A; 255 |]) "one-argument call"
              Expect.equal (bound "rgb") (Some [| 251; 191; 36; 255 |]) "record"
              Expect.equal (bound "accentRgb") (Some [| 251; 191; 36; 255 |]) "tuple"
              Expect.equal (bound "planets") None "several colours bind none"
              Expect.equal (bound "label") None "no colour"
              Expect.equal (bound "typed") (Some [| 96; 165; 250; 255 |]) "type annotation")

          testCase "the swatch is a span only for clients that render one" (fun () ->
              Expect.equal
                  (Colors.swatchMarkdown true (rgbaOf [ 251; 191; 36; 255 ]))
                  "<span style=\"color:#fbbf24;background-color:#fbbf24;border-radius:3px;\">████</span> `#fbbf24` rgb(251, 191, 36)"
                  "html"

              Expect.equal (Colors.swatchMarkdown false (rgbaOf [ 251; 191; 36; 128 ])) "`#fbbf2480` rgba(251, 191, 36, 0.5)" "plain")

          testCase "FSharp.colors.hover turns hover swatches off" (fun () ->
              let config =
                  FSharpConfig.UpdateFromJson FSharpConfig.Default (JToken.Parse """{"FSharp":{"colors":{"hover":false}}}""")

              Expect.isFalse config.Colors.Hover "hover off"
              Expect.isTrue FSharpConfig.Default.Colors.Hover "on by default") ]

let private copyDeps () : string * IDisposable =
    let fixturesRoot = Path.GetDirectoryName PublishTests.depsDir
    let repoRoot = Path.GetDirectoryName(Path.GetDirectoryName fixturesRoot)
    let root = Path.Combine(Path.GetTempPath(), "rfs-colors-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    let target = Path.Combine(root, "Deps")
    Directory.CreateDirectory target |> ignore
    File.Copy(Path.Combine(repoRoot, "global.json"), Path.Combine(root, "global.json"))

    for f in Directory.GetFiles PublishTests.depsDir do
        File.Copy(f, Path.Combine(target, Path.GetFileName f))

    let cleanup =
        { new IDisposable with
            member _.Dispose() =
                try
                    if Directory.Exists root then
                        Directory.Delete(root, true)
                with _ ->
                    () }

    target, cleanup

let private loadProject (lsp: LspClient) (projPath: string) : Task =
    task {
        let doc = JsonObject()
        doc["Uri"] <- JsonValue.Create(Paths.toUri projPath)
        let docs = JsonArray()
        docs.Add doc
        let p = JsonObject()
        p["TextDocuments"] <- docs
        let! _ = lsp.Request("fsharp/workspaceLoad", p) |> within
        ()
    }

let private documentColors (lsp: LspClient) (path: string) : Task<JsonArray> =
    task {
        let p = JsonObject()
        let doc = JsonObject()
        doc["uri"] <- JsonValue.Create(Paths.toUri path)
        p["textDocument"] <- doc
        let! reply, _ = lsp.Request("textDocument/documentColor", p) |> within
        return reply.AsArray()
    }

let private runLspTest () =
    task {
              let targetDir, cleanup = copyDeps ()
              use _ = cleanup
              let projPath = Path.Combine(targetDir, "Deps.fsproj")
              let dPath = Path.Combine(targetDir, "D.fs")
              let! lsp, owner = TestServer.start ()
              use _ = owner

              let initParams =
                  JsonNode.Parse
                      $"""{{"processId":null,"rootUri":%s{JsonValue.Create(Paths.toUri targetDir).ToJsonString()},"capabilities":{{"general":{{"markdown":{{"parser":"marked","allowedTags":["span"]}}}}}}}}"""

              let! initReply, _ = lsp.Request("initialize", initParams) |> within
              do! lsp.Notify("initialized", JsonObject()) |> withinUnit
              Expect.isTrue (initReply["capabilities"].["colorProvider"].GetValue<bool>()) "colorProvider must be true"

              // VS Code asks for colours once, when the file opens, often before the workspace has loaded.
              let text =
                  String.concat "\n" [ "module Deps.D"; ""; "let accent = \"#fbbf24\""; "let accentColor = (1, 2, 3)"; "let useIt = accent" ]
              do! lsp.DidOpen(dPath, text) |> withinUnit
              let! beforeLoad = documentColors lsp dPath
              Expect.equal beforeLoad.Count 2 "an open file no loaded project holds is read from its open text"

              do! loadProject lsp projPath
              let! colors = documentColors lsp dPath
              Expect.equal colors.Count 2 "both colours are reported"
              let first = colors[0]
              Expect.equal (first["range"].["start"].["line"].GetValue<int>()) 2 "first colour line"
              Expect.equal (first["range"].["start"].["character"].GetValue<int>()) 14 "first colour starts after the quote"
              Expect.floatClose Accuracy.medium (first["color"].["red"].GetValue<float>()) (251.0 / 255.0) "red channel"

              let presentationParams = JsonObject()
              let doc = JsonObject()
              doc["uri"] <- JsonValue.Create(Paths.toUri dPath)
              presentationParams["textDocument"] <- doc
              presentationParams["range"] <- first["range"].DeepClone()
              presentationParams["color"] <- JsonNode.Parse """{"red":1,"green":0,"blue":0,"alpha":1}"""
              let! presented, _ = lsp.Request("textDocument/colorPresentation", presentationParams) |> within
              let edit = presented.AsArray()[0]
              Expect.equal (edit["textEdit"].["newText"].GetValue<string>()) "#ff0000" "hex presentation"

              let hoverParams = JsonObject()
              let hoverDoc = JsonObject()
              hoverDoc["uri"] <- JsonValue.Create(Paths.toUri dPath)
              hoverParams["textDocument"] <- hoverDoc
              hoverParams["position"] <- JsonNode.Parse """{"line":4,"character":13}"""
              let! hover, _ = lsp.Request("textDocument/hover", hoverParams) |> within
              let shown = hover["contents"].["value"].GetValue<string>()

              Expect.stringContains
                  shown
                  "<span style=\"color:#fbbf24;background-color:#fbbf24;border-radius:3px;\">"
                  "hovering a use of a value bound to a colour shows its swatch"

              do! lsp.Configure(JsonNode.Parse """{"FSharp":{"colors":{"enabled":false}}}""")

              let sw = Diagnostics.Stopwatch.StartNew()
              let mutable remaining = colors.Count

              while remaining > 0 && sw.Elapsed < TimeSpan.FromSeconds 10. do
                  let! now = documentColors lsp dPath
                  remaining <- now.Count

                  if remaining > 0 then
                      do! Task.Delay 25

              Expect.equal remaining 0 "colors.enabled=false reports no colours"
    }

[<Tests>]
let lspTests =
    testSequenced
    <| testList
        "Colors.Lsp"
        [ testTask "colorProvider is advertised, documentColor and colorPresentation answer, colors.enabled=false empties" {
              do! runLspTest ()
          } ]
