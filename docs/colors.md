---
title: Colour swatches
description: How ranvierautocomplete finds colours in F# source, in RGB and seven other colour spaces, and writes a picked colour back.
---

# Colour swatches (`ranvierautocomplete`)

`ranvierautocomplete` answers `textDocument/documentColor` and `textDocument/colorPresentation`. VS Code draws a swatch beside each colour the server reports and opens its colour picker on a click, and the server writes the picked colour back in the literal's own spelling. Hovering a colour, or a value bound to one, shows it in the tooltip.

Colours are read from the parse tree alone, by names and literal shapes, not by types. Swatches therefore appear while a project is still loading, and in an open file that no loaded project holds.

Swatches need a client that implements `textDocument/documentColor`. The hover preview works in any client.

## Settings

Ionide sends these as `FSharp.colors.*`. Other clients send the same keys under `FSharp.Colors`, or a top-level `Colors`, in `initializationOptions` or `workspace/didChangeConfiguration`. Keys match in any case.

| Setting | Default | What it reads |
| --- | --- | --- |
| `enabled` | `true` | Turns every colour feature on or off |
| `strings` | `true` | `"#rgb"`, `"#rgba"`, `"#rrggbb"` and `"#rrggbbaa"` strings |
| `stringsWithoutHash` | `false` | Also `"rrggbb"` and `"rrggbbaa"`; a picked colour keeps the string without `#` |
| `hexIntegers` | `true` | `0xfbbf24` anywhere; an 8-digit one only when bound or passed to a name containing `color`, `colour` or `rgb` |
| `hexByteOrder` | `"rgba"` | Where alpha sits in 8-digit hex and 4-digit strings: `"rgba"` (CSS, `RgbaColor.fromHex`) or `"argb"` (.NET `Color.FromArgb`, WPF, Skia) |
| `members` | `true` | Named colours under a `Color`, `Colors`, `Colour`, `Colours` or `Palette` qualifier |
| `records` | `true` | Records whose fields are R, G, B or Red, Green, Blue, with optional alpha |
| `constructors` | `true` | Calls with 3 or 4 numeric literals to a name containing `color`, `colour` or `rgb` |
| `tuples` | `true` | Tuples of 3 or 4 numeric literals bound to, or passed as, such a name |
| `floatScale` | `"auto"` | RGB float channels: `"auto"` (`[0, 1]` when every channel is at most 1, else 0–255), `"normalized"` or `"byte"` |
| `hover` | `true` | A colour in the hover of a colour literal, and of any use of a value bound to one |
| `spaces` | `true` | HSL, HSV/HSB, Lab, LCH/HCL, OKLab, OKLCH and CMYK, from names and record fields |
| `hueUnit` | `"auto"` | Hue literals: `"auto"`, `"degrees"` or `"turns"` (see [Channel values](#channel-values)) |

In VS Code, `"editor.colorDecorators": false` hides the swatches without turning off the server's colour features.

## RGB

| Source | Examples |
| --- | --- |
| Hex string | `"#fbbf24"`, `"#fbbf2480"` |
| Hex integer | `0xfbbf24`, `let argbAccent = 0x80FBBF24` |
| Named member | `Colors.RebeccaPurple`, `Palette.red`, `Css.Color.rebeccapurple` |
| Record | `{ R = 251uy; G = 191uy; B = 36uy }`, `{| Red = 0.5; Green = 0.2; Blue = 0.1; Alpha = 1.0 |}` |
| Constructor | `RgbaColor(0.2f, 0.4f, 0.6f, 1.0f)`, `Color.FromArgb(128, 255, 0, 0)` |
| Tuple | `let accentColor = (251, 191, 36)` |

- Integer channels are always 0–255. `floatScale` decides float channels, and a picked colour is written back on the same scale.
- Record fields match in any case (`r`, `R`, `Red`, `RED`), in records, anonymous records and `struct {| ... |}`.
- A name containing `argb` or `rgba` (`Color.FromArgb`, `argbAccent`) overrides `hexByteOrder` for the literal it names.
- A picked colour keeps the literal's spelling: case, digit count, `#` or not, `uy` and `f` suffixes, and byte or `[0, 1]` scale.

## Other colour spaces

With `spaces` on, the server reads seven more spaces when a name or a record's fields say so. The picker converts a picked colour back into that space.

| Space | Name words | Record fields, any case | Channel ranges |
| --- | --- | --- | --- |
| HSL | `hsl`, `hsla` | `H`/`Hue`, `S`/`Sat`/`Saturation`, `L`/`Light`/`Lightness` | hue; S and L 0–1 or 0–100 |
| HSV, HSB | `hsv`, `hsva`, `hsb`, `hsba` | `H`/`Hue`, `S`/`Sat`/`Saturation`, `V`/`Value`/`B`/`Brightness` | hue; S and V 0–1 or 0–100 |
| CIE Lab | `lab` | `L`/`Lightness`/`Luminance`, `A`, `B` | L 0–100; a and b −160 to 160 |
| CIE LCH | `lch`, `hcl` | `L`/`Lightness`/`Luminance`, `C`/`Chroma`, `H`/`Hue` | L 0–100; C 0–230; hue |
| OKLab | `oklab`, or `Ok` then `Lab` | Lab's fields, under a name with the word `ok` | L 0–1 or 0–100; a and b −0.5 to 0.5 |
| OKLCH | `oklch`, or `Ok` then `Lch` | LCH's fields, under a name with the word `ok` | L 0–1 or 0–100; C 0–0.5; hue |
| CMYK | `cmyk` | `C`/`Cyan`, `M`/`Magenta`, `Y`/`Yellow`, `K`/`Black`/`Key` | each 0–1 or 0–100 |

Alpha is optional. In a record it is an `A` or `Alpha` field; in Lab and OKLab, where `A` is a channel, only `Alpha` counts. In a call or tuple, alpha is one more argument after the channels. An integer alpha is 0–255, a float alpha at most 1 is 0–1, and a larger float alpha is 0–255.

```fsharp
let a = SKColor.FromHsl(210f, 50f, 60f)                                  // HSL in percentages: #6699cc
let b = Color.HSVToRGB(0.5833f, 0.5f, 0.8f)                              // Unity's HSV, hue in turns: #6699cc
let c = oklch 0.628 0.2577 29.23                                         // OKLCH: red
let d = hsla 210.0 0.5 0.6 0.5                                           // alpha after the channels
let accentHsl = (210, 50, 60)                                            // a tuple named for its space: #6699cc
let hclAccent = (40.85, 106.84, 54.29)                                   // hcl: channels in H, C, L order
let e = { Lightness = 54.29; A = 80.81; B = 69.89; Alpha = 1.0 }         // Ranvier.D5's LabColor: red
let f = { Hue = 40.85; Chroma = 106.84; Lightness = 54.29; Alpha = 0.5 } // Ranvier.D5's HclColor: red at 50%
let g = { H = 210.0; S = 0.5; B = 0.8 }                                  // HSB: #6699cc
let h = { cyan = 0.0; magenta = 1.0; yellow = 1.0; black = 0.0 }         // CMYK: red
let accentOklch = { L = 0.628; C = 0.2577; H = 29.23 }                   // "ok" in the name makes LCH OKLCH

let x = batchClient(1, 2, 3)              // not a colour: "hcl" inside a word is no space name
let y = Color.FromHsl(210.0, 150.0, 60.0) // not a colour: 150% saturation is out of range
```

### Names

- A space name counts only as a whole word of a name: `SKColor.FromHsl`, `Color.HSVToRGB`, `HclColor`, `OkLch`. So `batchClient` is not HCL, and `labelColor` is RGB, not Lab.
- The first colour word decides: `HSVToRGB` takes HSV, `RgbToHsl` takes RGB.
- Channels follow the name's letters: `hcl` is H, C, L, and `lch` is L, C, H.
- A name with a space the server does not convert (`hwb`, `xyz`, `yuv`, `ycbcr`, `luv`) is not read as a colour, even with `color` in it (`colorXyz`).
- Records match by their fields alone, so `{ H = 210.0; S = 0.5; L = 0.6 }` needs no name. The word `ok` in the name around a Lab or LCH record makes it OKLab or OKLCH.

### Channel values

- Fraction channels are 0–1, unless one of them is above 1: then all of them are percentages. The fraction channels are HSL and HSV saturation, lightness and value; OKLab and OKLCH lightness; and every CMYK channel. In `SKColor.FromHsl(210f, 50f, 60f)`, saturation is 50% and lightness 60%.
- A hue is in degrees, with one exception under `hueUnit: "auto"`: an HSL or HSV hue at most 1, among fraction channels at most 1, is in turns, as in Unity's `Color.HSVToRGB`. `"degrees"` and `"turns"` fix the unit for every hue, LCH and OKLCH included.
- A literal outside its space's range is not a colour.
- Lab and LCH are CIE Lab under D50, as in CSS Color 4 and Ranvier.D5. OKLab and OKLCH use Björn Ottosson's matrices. A colour outside sRGB is clipped for its swatch only.

### Writing back

The server converts a picked colour into the literal's space and writes each channel on the literal's scale, keeping its suffix. Picking red:

| Before | After |
| --- | --- |
| `SKColor.FromHsl(210f, 50f, 60f)` | `SKColor.FromHsl(0.0f, 100.0f, 50.0f)` |
| `Color.HSVToRGB(0.5833f, 0.5f, 0.8f)` | `Color.HSVToRGB(0.0f, 1.0f, 1.0f)` |
| `let accentHsl = (210, 50, 60)` | `let accentHsl = (0, 100, 50)` |

A grey has no hue, so picking one keeps the hue literal as it was. Picking grey on `{ H = 210.0; S = 0.5; L = 0.6 }` writes `{ H = 210.0; S = 0.0; L = 0.502 }`.

## Hover

Hovering a colour literal, or any use of a value whose declaration binds a colour (`let accent = "#fbbf24"`, then `accent` anywhere), adds the colour to the tooltip. A client that allows `<span>` in markdown (`general.markdown.allowedTags`, which Ionide sets) gets a swatch. Any other client gets the colour's hex and `rgb(...)` as text.

A declaration binds a colour when its body is a colour, a literal holding one, or a one-argument call such as `hex "#e3c58a"`.
