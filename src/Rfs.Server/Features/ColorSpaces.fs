/// Colour spaces other than RGB that a name or a record's fields declare, and their conversions to and from sRGB in
/// `[0, 1]`. Lab and LCH (HCL) are CIE Lab under D50, as in CSS Color 4 and Ranvier.D5's `ColorSpace`; OKLab and OKLCH
/// use Björn Ottosson's matrices.
module Rfs.Server.ColorSpaces

open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
type Space =
    | Hsl
    | Hsv
    | Lab
    | Lch
    | OkLab
    | OkLch
    | Cmyk

/// The space's channel letters, in canonical order.
let letters (space: Space) =
    match space with
    | Space.Hsl -> "hsl"
    | Space.Hsv -> "hsv"
    | Space.Lab
    | Space.OkLab -> "lab"
    | Space.Lch
    | Space.OkLch -> "lch"
    | Space.Cmyk -> "cmyk"

/// What a name says about the colour it takes or holds.
[<RequireQualifiedAccess>]
type Declared =
    | Rgb of alphaFirst: bool
    /// A space, and the canonical channel index of each argument in the order the name spells them.
    | Space of Space * order: int list

type private Token =
    | RgbToken of alphaFirst: bool
    | SpaceToken of Space * spelled: string
    /// A space this does not convert (xyz, yuv, ...): the name is not RGB either.
    | Other

/// Space names count only as whole words (`fromHsl`, `HSVToRGB`, `OkLch`), so `batchClient` is not HCL and
/// `labelColor` is not Lab.
let private spaceWords =
    dict
        [ "oklch", SpaceToken(Space.OkLch, "lch")
          "oklab", SpaceToken(Space.OkLab, "lab")
          "hsla", SpaceToken(Space.Hsl, "hsl")
          "hsl", SpaceToken(Space.Hsl, "hsl")
          "hsva", SpaceToken(Space.Hsv, "hsv")
          "hsv", SpaceToken(Space.Hsv, "hsv")
          "hsba", SpaceToken(Space.Hsv, "hsb")
          "hsb", SpaceToken(Space.Hsv, "hsb")
          "hcl", SpaceToken(Space.Lch, "hcl")
          "lch", SpaceToken(Space.Lch, "lch")
          "lab", SpaceToken(Space.Lab, "lab")
          "cmyk", SpaceToken(Space.Cmyk, "cmyk")
          "hwb", Other
          "xyz", Other
          "yuv", Other
          "ycbcr", Other
          "luv", Other ]

let private wordPattern = Regex("[A-Z]+(?=[A-Z][a-z])|[A-Z]?[a-z]+|[A-Z]+|[0-9]+", RegexOptions.Compiled)

/// Each argument's canonical channel index, from the letters the name spells (`hcl` in LCH is 2, 1, 0).
let private orderOf (space: Space) (spelled: string) =
    let canonical = letters space

    [ for c in spelled -> canonical.IndexOf(if space = Space.Hsv && c = 'b' then 'v' else c) ]

/// The first colour word in a name decides: `HSVToRGB` takes HSV, `RgbToHsl` takes RGB. RGB words (rgb, rgba, argb)
/// match anywhere, as before; a name with no space word but color or colour is RGB.
let declaredBy (name: string) : Declared option =
    let lower = name.ToLowerInvariant()
    let words = [ for m in wordPattern.Matches name -> m.Index, m.Value.ToLowerInvariant() ]

    let spaceHits =
        [ for i in 0 .. words.Length - 1 do
              let at, word = words[i]

              match spaceWords.TryGetValue word with
              | true, token -> yield at, word.Length, token
              | _ when word = "ok" && i + 1 < words.Length ->
                  match spaceWords.TryGetValue("ok" + snd words[i + 1]) with
                  | true, token -> yield at, 2 + (snd words[i + 1]).Length, token
                  | _ -> ()
              | _ -> () ]

    let rgbHits =
        [ for token, alphaFirst in [ "argb", true; "rgba", false; "rgb", false ] do
              match lower.IndexOf(token, StringComparison.Ordinal) with
              | -1 -> ()
              | at -> yield at, token.Length, RgbToken alphaFirst ]

    match spaceHits @ rgbHits |> List.sortBy (fun (at, length, _) -> at, -length) with
    | (_, _, RgbToken alphaFirst) :: _ -> Some(Declared.Rgb alphaFirst)
    | (_, _, SpaceToken(space, spelled)) :: _ -> Some(Declared.Space(space, orderOf space spelled))
    | (_, _, Other) :: _ -> None
    | [] when lower.Contains "color" || lower.Contains "colour" -> Some(Declared.Rgb false)
    | [] -> None

/// The OK variant of Lab or LCH, when a name around the record says so (`let accentOklab = { L = ...; A = ...; B = ... }`).
let okVariant (name: string option) (space: Space) =
    let ok =
        name
        |> Option.exists (fun n -> [ for m in wordPattern.Matches n -> m.Value.ToLowerInvariant() ] |> List.exists (fun w -> w = "ok" || w = "oklab" || w = "oklch"))

    match space with
    | Space.Lab when ok -> Space.OkLab
    | Space.Lch when ok -> Space.OkLch
    | other -> other

let private fieldNames =
    [ Space.Hsl, [ [ "h"; "hue" ]; [ "s"; "sat"; "saturation" ]; [ "l"; "light"; "lightness" ] ], [ "a"; "alpha" ]
      Space.Hsv, [ [ "h"; "hue" ]; [ "s"; "sat"; "saturation" ]; [ "v"; "value"; "b"; "brightness" ] ], [ "a"; "alpha" ]
      Space.Lch, [ [ "l"; "lightness"; "luminance" ]; [ "c"; "chroma" ]; [ "h"; "hue" ] ], [ "a"; "alpha" ]
      Space.Lab, [ [ "l"; "lightness"; "luminance" ]; [ "a" ]; [ "b" ] ], [ "alpha" ]
      Space.Cmyk, [ [ "c"; "cyan" ]; [ "m"; "magenta" ]; [ "y"; "yellow" ]; [ "k"; "black"; "key" ] ], [ "a"; "alpha" ] ]

/// The space a record's field names spell, in any case, and each field's canonical channel index (-1 for alpha).
let ofFields (names: string list) : (Space * int list) option =
    fieldNames
    |> List.tryPick (fun (space, channels, alphas) ->
        let indexOf (name: string) =
            let n = name.ToLowerInvariant()

            match channels |> List.tryFindIndex (List.contains n) with
            | Some i -> Some i
            | None when List.contains n alphas -> Some -1
            | None -> None

        let indices = names |> List.map indexOf

        if indices |> List.forall Option.isSome then
            let found = indices |> List.choose id
            let used = found |> List.filter (fun i -> i >= 0) |> List.sort
            let alphaCount = found |> List.filter (fun i -> i < 0) |> List.length

            if used = [ 0 .. channels.Length - 1 ] && alphaCount <= 1 then Some(space, found) else None
        else
            None)

// --- Channels ----------------------------------------------------------------------------------------------------

/// The hue channel's canonical index, or -1.
let hueIndex (space: Space) =
    match space with
    | Space.Hsl
    | Space.Hsv -> 0
    | Space.Lch
    | Space.OkLch -> 2
    | _ -> -1

/// Channels whose canonical value is a fraction in `[0, 1]`, written as a fraction or a percentage.
let private fractions (space: Space) =
    match space with
    | Space.Hsl
    | Space.Hsv -> [ 1; 2 ]
    | Space.Cmyk -> [ 0; 1; 2; 3 ]
    | Space.OkLab
    | Space.OkLch -> [ 0 ]
    | Space.Lab
    | Space.Lch -> []

/// For each channel, literal = canonical * scale. A fraction channel above 1 makes them all percentages; a hue at most
/// 1 among fraction channels at most 1 is in turns (Unity's `HSVToRGB`), unless `hueUnit` says otherwise.
let scales (hueUnit: HueUnit) (space: Space) (literal: float[]) : float[] =
    let fractionIndices = fractions space
    let percent = fractionIndices |> List.exists (fun i -> literal[i] > 1.0)
    let hue = hueIndex space

    let turns =
        match hueUnit with
        | HueUnit.Turns -> true
        | HueUnit.Degrees -> false
        | HueUnit.Auto -> (space = Space.Hsl || space = Space.Hsv) && literal[hue] <= 1.0 && not percent

    Array.init literal.Length (fun i ->
        if i = hue then (if turns then 1.0 / 360.0 else 1.0)
        elif List.contains i fractionIndices then (if percent then 100.0 else 1.0)
        else 1.0)

/// How a float channel is written back.
let formats (space: Space) (scales: float[]) : string[] =
    Array.init scales.Length (fun i ->
        if i = hueIndex space then (if scales[i] < 1.0 then "0.0###" else "0.0#")
        elif scales[i] = 100.0 then "0.0#"
        elif List.contains i (fractions space) then "0.0##"
        else
            match space with
            | Space.OkLab
            | Space.OkLch -> "0.0###"
            | _ -> "0.0#")

let valid (space: Space) (v: float[]) =
    let within lo hi (x: float) = x >= lo && x <= hi
    let finite (x: float) = not (Double.IsNaN x || Double.IsInfinity x)

    match space with
    | Space.Hsl
    | Space.Hsv -> finite v[0] && within 0.0 1.0 v[1] && within 0.0 1.0 v[2]
    | Space.Lab -> within 0.0 100.0 v[0] && within -160.0 160.0 v[1] && within -160.0 160.0 v[2]
    | Space.Lch -> within 0.0 100.0 v[0] && within 0.0 230.0 v[1] && finite v[2]
    | Space.OkLab -> within 0.0 1.0 v[0] && within -0.5 0.5 v[1] && within -0.5 0.5 v[2]
    | Space.OkLch -> within 0.0 1.0 v[0] && within 0.0 0.5 v[1] && finite v[2]
    | Space.Cmyk -> v |> Array.forall (within 0.0 1.0)

/// A colour without hue (grey): writing one back keeps the literal's hue rather than jumping to 0.
let achromatic (space: Space) (v: float[]) =
    match space with
    | Space.Hsl
    | Space.Hsv -> v[1] < 1e-4
    | Space.Lch -> v[1] < 0.01
    | Space.OkLch -> v[1] < 1e-4
    | _ -> false

// --- Conversions ---------------------------------------------------------------------------------------------------

let private toLinear (x: float) = if x <= 0.04045 then x / 12.92 else Math.Pow((x + 0.055) / 1.055, 2.4)
let private toGamma (x: float) = if x <= 0.0031308 then 12.92 * x else 1.055 * Math.Pow(x, 1.0 / 2.4) - 0.055
let private wrap (degrees: float) = ((degrees % 360.0) + 360.0) % 360.0

let private ofHue (h: float) (c: float) (m: float) =
    let hp = wrap h / 60.0
    let x = c * (1.0 - abs ((hp % 2.0) - 1.0))

    let r, g, b =
        if hp < 1.0 then c, x, 0.0
        elif hp < 2.0 then x, c, 0.0
        elif hp < 3.0 then 0.0, c, x
        elif hp < 4.0 then 0.0, x, c
        elif hp < 5.0 then x, 0.0, c
        else c, 0.0, x

    [| r + m; g + m; b + m |]

let private hueOf (r: float) (g: float) (b: float) =
    let hi, lo = max r (max g b), min r (min g b)
    let d = hi - lo

    if d = 0.0 then 0.0
    elif hi = r then wrap (60.0 * ((g - b) / d))
    elif hi = g then wrap (60.0 * ((b - r) / d + 2.0))
    else wrap (60.0 * ((r - g) / d + 4.0))

/// Ranvier.D5 `ColorSpace.fromLab` (D50).
let private ofLab (l: float) (a: float) (b: float) =
    let inverse t = if t > 6.0 / 29.0 then t * t * t else (108.0 / 841.0) * (t - 4.0 / 29.0)
    let y0 = (l + 16.0) / 116.0
    let x = 0.96422 * inverse (y0 + a / 500.0)
    let z = 0.82521 * inverse (y0 - b / 200.0)
    let y = inverse y0

    [| toGamma (3.1338561 * x - 1.6168667 * y - 0.4906146 * z)
       toGamma (-0.9787684 * x + 1.9161415 * y + 0.0334540 * z)
       toGamma (0.0719453 * x - 0.2289914 * y + 1.4052427 * z) |]

/// Ranvier.D5 `ColorSpace.toLab` (D50).
let private toLab (rgb: float[]) =
    let f x = if x > 216.0 / 24389.0 then Math.Pow(x, 1.0 / 3.0) else x / (108.0 / 841.0) + 4.0 / 29.0
    let r, g, b = toLinear rgb[0], toLinear rgb[1], toLinear rgb[2]
    let y = f (0.2225045 * r + 0.7168786 * g + 0.0606169 * b)

    let x, z =
        if r = g && g = b then
            y, y
        else
            f ((0.4360747 * r + 0.3850649 * g + 0.1430804 * b) / 0.96422),
            f ((0.0139322 * r + 0.0971045 * g + 0.7141733 * b) / 0.82521)

    [| 116.0 * y - 16.0; 500.0 * (x - y); 200.0 * (y - z) |]

let private ofOkLab (l: float) (a: float) (b: float) =
    let l' = l + 0.3963377774 * a + 0.2158037573 * b
    let m' = l - 0.1055613458 * a - 0.0638541728 * b
    let s' = l - 0.0894841775 * a - 1.2914855480 * b
    let l3, m3, s3 = l' * l' * l', m' * m' * m', s' * s' * s'

    [| toGamma (4.0767416621 * l3 - 3.3077115913 * m3 + 0.2309699292 * s3)
       toGamma (-1.2684380046 * l3 + 2.6097574011 * m3 - 0.3413193965 * s3)
       toGamma (-0.0041960863 * l3 - 0.7034186147 * m3 + 1.7076147010 * s3) |]

let private toOkLab (rgb: float[]) =
    let r, g, b = toLinear rgb[0], toLinear rgb[1], toLinear rgb[2]
    let l = Math.Cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
    let m = Math.Cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
    let s = Math.Cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)

    [| 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s
       1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s
       0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s |]

let private polar (lab: float[]) =
    let chroma = sqrt (lab[1] * lab[1] + lab[2] * lab[2])
    [| lab[0]; chroma; (if chroma < 1e-9 then 0.0 else wrap (atan2 lab[2] lab[1] * 180.0 / Math.PI)) |]

let private cartesian (lch: float[]) =
    let angle = wrap lch[2] * Math.PI / 180.0
    lch[0], cos angle * lch[1], sin angle * lch[1]

/// sRGB channels in `[0, 1]` (outside it when the colour is out of gamut) from canonical values.
let toRgb (space: Space) (v: float[]) : float[] =
    match space with
    | Space.Hsl ->
        let c = (1.0 - abs (2.0 * v[2] - 1.0)) * v[1]
        ofHue v[0] c (v[2] - c / 2.0)
    | Space.Hsv ->
        let c = v[2] * v[1]
        ofHue v[0] c (v[2] - c)
    | Space.Lab -> ofLab v[0] v[1] v[2]
    | Space.Lch -> cartesian v |||> ofLab
    | Space.OkLab -> ofOkLab v[0] v[1] v[2]
    | Space.OkLch -> cartesian v |||> ofOkLab
    | Space.Cmyk -> [| (1.0 - v[0]) * (1.0 - v[3]); (1.0 - v[1]) * (1.0 - v[3]); (1.0 - v[2]) * (1.0 - v[3]) |]

/// Canonical values from sRGB channels in `[0, 1]`.
let ofRgb (space: Space) (rgb: float[]) : float[] =
    let r, g, b = rgb[0], rgb[1], rgb[2]
    let hi, lo = max r (max g b), min r (min g b)

    match space with
    | Space.Hsl ->
        let l = (hi + lo) / 2.0
        let d = hi - lo
        let s = if d = 0.0 then 0.0 else d / (1.0 - abs (2.0 * l - 1.0))
        [| hueOf r g b; s; l |]
    | Space.Hsv -> [| hueOf r g b; (if hi = 0.0 then 0.0 else (hi - lo) / hi); hi |]
    | Space.Lab -> toLab rgb
    | Space.Lch -> toLab rgb |> polar
    | Space.OkLab -> toOkLab rgb
    | Space.OkLch -> toOkLab rgb |> polar
    | Space.Cmyk ->
        let k = 1.0 - hi

        if k >= 1.0 then
            [| 0.0; 0.0; 0.0; 1.0 |]
        else
            [| (1.0 - r - k) / (1.0 - k); (1.0 - g - k) / (1.0 - k); (1.0 - b - k) / (1.0 - k); k |]
