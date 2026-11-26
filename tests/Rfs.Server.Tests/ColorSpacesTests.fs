module Rfs.Server.Tests.ColorSpacesTests

open System
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Newtonsoft.Json.Linq
open Rfs.Server
open Rfs.Server.ColorSpaces

type private Range = Ionide.LanguageServerProtocol.Types.Range

let private parseChecker = lazy (FSharpChecker.Create(keepAssemblyContents = false))

let private parse (text: string) =
    let opts = { FSharpParsingOptions.Default with SourceFiles = [| "C.fs" |] }
    (parseChecker.Value.ParseFile("C.fs", SourceText.ofString text, opts) |> Async.RunSynchronously).ParseTree

let private slice (text: string) (r: Range) =
    let lines = text.Split '\n'
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

let private toByte (v: float) = int (Math.Round(v * 255.0))
let private bytes (c: Color) = toByte (float c.Red), toByte (float c.Green), toByte (float c.Blue), toByte (float c.Alpha)
let private source (body: string list) = String.concat "\n" ("module C" :: "" :: body)
let private on = ColorsConfig.Default

let private colours config text =
    Colors.find config text (parse text) |> Array.map (fun i -> slice text i.Range, bytes i.Color) |> Array.toList

let private close (r1, g1, b1, a1) (r2, g2, b2, a2) =
    abs (r1 - r2) <= 2 && abs (g1 - g2) <= 2 && abs (b1 - b2) <= 2 && abs (a1 - a2) <= 2

let private expectColours config text (expected: (string * (int * int * int * int)) list) message =
    let found = colours config text
    Expect.equal (found |> List.map fst) (expected |> List.map fst) $"%s{message}: spans"

    for (span, actual), (_, wanted) in List.zip found expected do
        Expect.isTrue (close actual wanted) $"%s{message}: %s{span} is %A{actual}, expected about %A{wanted}"

let private rgbOf space (v: float list) = toRgb space (List.toArray v) |> Array.map toByte |> Array.toList

/// Applies the presentation for `span` and reads the colour back from the rewritten text.
let private roundTrip config (text: string) (span: string) (r, g, b, a) =
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

    let edit = (Colors.presentations config text tree info.Range color |> Array.head).TextEdit.Value
    let lines = text.Split '\n'
    let line = int info.Range.Start.Line
    let before = lines[line].Substring(0, int info.Range.Start.Character)
    let after = lines[int info.Range.End.Line].Substring(int info.Range.End.Character)
    let rewritten = Array.concat [ lines[.. line - 1]; [| before + edit.NewText + after |]; lines[int info.Range.End.Line + 1 ..] ] |> String.concat "\n"

    let back =
        colours config rewritten |> List.tryFind (fun (s, _) -> s = edit.NewText) |> Option.map snd

    edit.NewText, back

[<Tests>]
let tests =
    testList
        "ColorSpaces"
        [ testCase "a space counts only as a whole word, and the first colour word in a name decides" (fun () ->
              let cases =
                  [ "Color.FromHsl", Some(Declared.Space(Space.Hsl, [ 0; 1; 2 ]))
                    "SKColor.FromHsv", Some(Declared.Space(Space.Hsv, [ 0; 1; 2 ]))
                    "HSVToRGB", Some(Declared.Space(Space.Hsv, [ 0; 1; 2 ]))
                    "RgbToHsl", Some(Declared.Rgb false)
                    "HclColor", Some(Declared.Space(Space.Lch, [ 2; 1; 0 ]))
                    "OkLch", Some(Declared.Space(Space.OkLch, [ 0; 1; 2 ]))
                    "oklab", Some(Declared.Space(Space.OkLab, [ 0; 1; 2 ]))
                    "fromLab", Some(Declared.Space(Space.Lab, [ 0; 1; 2 ]))
                    "hsba", Some(Declared.Space(Space.Hsv, [ 0; 1; 2 ]))
                    "labelColor", Some(Declared.Rgb false)
                    "batchClient", None
                    "Color.FromArgb", Some(Declared.Rgb true)
                    "accentColour", Some(Declared.Rgb false)
                    "colorXyz", None ]

              for name, expected in cases do
                  Expect.equal (declaredBy name) expected name)

          testCase "conversions match known values" (fun () ->
              Expect.equal (rgbOf Space.Hsl [ 210.0; 0.5; 0.6 ]) [ 102; 153; 204 ] "hsl(210, 50%, 60%) is #6699cc"
              Expect.equal (rgbOf Space.Hsv [ 210.0; 0.5; 0.8 ]) [ 102; 153; 204 ] "hsv(210, 50%, 80%) is #6699cc"
              Expect.equal (rgbOf Space.Cmyk [ 0.0; 1.0; 1.0; 0.0 ]) [ 255; 0; 0 ] "cmyk red"
              Expect.equal (rgbOf Space.OkLab [ 1.0; 0.0; 0.0 ]) [ 255; 255; 255 ] "oklab white"

              let red (space: Space) (v: float list) =
                  match rgbOf space v with
                  | [ r; g; b ] -> close (r, g, b, 255) (255, 0, 0, 255)
                  | _ -> false

              Expect.isTrue (red Space.Lab [ 54.29; 80.81; 69.89 ]) "lab red (D50)"
              Expect.isTrue (red Space.Lch [ 54.29; 106.84; 40.85 ]) "lch red (D50)"
              Expect.isTrue (red Space.OkLch [ 0.628; 0.2577; 29.23 ]) "oklch red"

              for space in [ Space.Hsl; Space.Hsv; Space.Lab; Space.Lch; Space.OkLab; Space.OkLch; Space.Cmyk ] do
                  let back = ofRgb space [| 0.4; 0.6; 0.8 |] |> toRgb space |> Array.map toByte |> Array.toList
                  Expect.equal back [ 102; 153; 204 ] $"%A{space} round trip")

          testCase "records whose fields spell a space, in any case" (fun () ->
              let text =
                  source
                      [ "let a = { H = 210.0; S = 0.5; L = 0.6 }"
                        "let b = { Hue = 210.0; Saturation = 50.0; Lightness = 60.0 }"
                        "let c = { Lightness = 54.29; A = 80.81; B = 69.89; Alpha = 1.0 }"
                        "let d = { Hue = 40.85; Chroma = 106.84; Lightness = 54.29; Alpha = 0.5 }"
                        "let e = { cyan = 0.0; magenta = 1.0; yellow = 1.0; black = 0.0 }"
                        "let f = { L = 50.0; A = -20.0; B = 30.0 }"
                        "let g = { H = 210.0; S = 0.5; B = 0.8 }"
                        "let accentOklch = { L = 0.628; C = 0.2577; H = 29.23 }" ]

              let f = rgbOf Space.Lab [ 50.0; -20.0; 30.0 ]

              expectColours
                  on
                  text
                  [ "{ H = 210.0; S = 0.5; L = 0.6 }", (102, 153, 204, 255)
                    "{ Hue = 210.0; Saturation = 50.0; Lightness = 60.0 }", (102, 153, 204, 255)
                    "{ Lightness = 54.29; A = 80.81; B = 69.89; Alpha = 1.0 }", (255, 0, 0, 255)
                    "{ Hue = 40.85; Chroma = 106.84; Lightness = 54.29; Alpha = 0.5 }", (255, 0, 0, 128)
                    "{ cyan = 0.0; magenta = 1.0; yellow = 1.0; black = 0.0 }", (255, 0, 0, 255)
                    "{ L = 50.0; A = -20.0; B = 30.0 }", (f[0], f[1], f[2], 255)
                    "{ H = 210.0; S = 0.5; B = 0.8 }", (102, 153, 204, 255)
                    "{ L = 0.628; C = 0.2577; H = 29.23 }", (255, 0, 0, 255) ]
                  "Ranvier.D5's LabColor and HclColor shapes, HSB, CMYK, negative Lab channels and an ok name")

          testCase "calls and tuples whose names say the space, with channels in the name's letter order" (fun () ->
              let text =
                  source
                      [ "let a = SKColor.FromHsl(210f, 50f, 60f)"
                        "let b = Color.HSVToRGB(0.5833f, 0.5f, 0.8f)"
                        "let c = oklch 0.628 0.2577 29.23"
                        "let accentHsl = (210, 50, 60)"
                        "let hclAccent = (40.85, 106.84, 54.29)"
                        "let d = hsla 210.0 0.5 0.6 0.5"
                        "let e = batchClient(1, 2, 3)"
                        "let f = Color.FromHsl(210.0, 150.0, 60.0)"
                        "let g = cmyk 0.0 1.0 1.0 0.0" ]

              expectColours
                  on
                  text
                  [ "SKColor.FromHsl(210f, 50f, 60f)", (102, 153, 204, 255)
                    "Color.HSVToRGB(0.5833f, 0.5f, 0.8f)", (102, 153, 204, 255)
                    "oklch 0.628 0.2577 29.23", (255, 0, 0, 255)
                    "(210, 50, 60)", (102, 153, 204, 255)
                    "(40.85, 106.84, 54.29)", (255, 0, 0, 255)
                    "hsla 210.0 0.5 0.6 0.5", (102, 153, 204, 128)
                    "cmyk 0.0 1.0 1.0 0.0", (255, 0, 0, 255) ]
                  "Unity's hue in turns, hcl in H C L order, alpha after the channels; batchClient and 150% saturation are not colours")

          testCase "spaces and hueUnit settings" (fun () ->
              let text = source [ "let a = Color.HSVToRGB(0.5833f, 0.5f, 0.8f)"; "let b = { H = 210.0; S = 0.5; L = 0.6 }"; "let c = \"#fbbf24\"" ]
              Expect.equal (colours { on with Spaces = false } text |> List.map fst) [ "#fbbf24" ] "spaces off leaves RGB"

              let degrees = colours { on with HueUnit = HueUnit.Degrees } text |> List.head |> snd
              Expect.isFalse (close degrees (102, 153, 204, 255)) "a hue of 0.5833 degrees is not 210 degrees"

              let read (json: string) = (FSharpConfig.UpdateFromJson FSharpConfig.Default (JToken.Parse json)).Colors
              let config = read """{"FSharp":{"colors":{"spaces":false,"hueUnit":"Turns"}}}"""
              Expect.isFalse config.Spaces "spaces"
              Expect.equal config.HueUnit HueUnit.Turns "hueUnit"
              Expect.equal FSharpConfig.Default.Colors.HueUnit HueUnit.Auto "auto by default")

          testCase "a picked colour is written back in the literal's space, scale and suffix" (fun () ->
              let text =
                  source
                      [ "let a = SKColor.FromHsl(210f, 50f, 60f)"
                        "let b = { H = 210.0; S = 0.5; L = 0.6 }"
                        "let c = Color.HSVToRGB(0.5833f, 0.5f, 0.8f)"
                        "let accentHsl = (210, 50, 60)" ]

              let presented span rgba = fst (roundTrip on text span rgba)
              Expect.equal (presented "SKColor.FromHsl(210f, 50f, 60f)" (255, 0, 0, 255)) "SKColor.FromHsl(0.0f, 100.0f, 50.0f)" "percent floats"
              Expect.equal (presented "{ H = 210.0; S = 0.5; L = 0.6 }" (128, 128, 128, 255)) "{ H = 210.0; S = 0.0; L = 0.502 }" "a grey keeps its hue"
              Expect.equal (presented "Color.HSVToRGB(0.5833f, 0.5f, 0.8f)" (255, 0, 0, 255)) "Color.HSVToRGB(0.0f, 1.0f, 1.0f)" "turns"
              Expect.equal (presented "(210, 50, 60)" (255, 0, 0, 255)) "(0, 100, 50)" "integer percentages")

          testCase "Lab, LCH, OKLCH and CMYK round trip through the picker" (fun () ->
              let text =
                  source
                      [ "let a = { Lightness = 54.29; A = 80.81; B = 69.89; Alpha = 1.0 }"
                        "let b = { Hue = 40.85; Chroma = 106.84; Lightness = 54.29; Alpha = 1.0 }"
                        "let c = oklch 0.628 0.2577 29.23"
                        "let d = cmyk 0.0 1.0 1.0 0.0"
                        "let e = { L = 50.0; A = -20.0; B = 30.0 }" ]

              for span in
                  [ "{ Lightness = 54.29; A = 80.81; B = 69.89; Alpha = 1.0 }"
                    "{ Hue = 40.85; Chroma = 106.84; Lightness = 54.29; Alpha = 1.0 }"
                    "oklch 0.628 0.2577 29.23"
                    "cmyk 0.0 1.0 1.0 0.0"
                    "{ L = 50.0; A = -20.0; B = 30.0 }" ] do
                  let written, back = roundTrip on text span (40, 90, 200, 255)
                  Expect.isSome back $"%s{written} is still a colour"
                  Expect.isTrue (close back.Value (40, 90, 200, 255)) $"%s{written} reads back as %A{back.Value}") ]
