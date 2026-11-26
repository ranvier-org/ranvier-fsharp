/// `textDocument/documentColor` and `textDocument/colorPresentation` from the parse tree alone (never waits on
/// typechecking): hex strings and integers, named colour members, R G B (A) records, colour constructor calls and
/// colour-named tuples, each behind its `FSharp.colors` setting.
module Rfs.Server.Colors

open System
open System.Drawing
open System.Globalization
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Ionide.LanguageServerProtocol.Types
open Rfs.Checker

type private LspRange = Ionide.LanguageServerProtocol.Types.Range
type private LspColor = Ionide.LanguageServerProtocol.Types.Color

/// The file's text, addressed by FCS line (1-based) and column.
type private Source(text: string) =
    let starts =
        [| yield 0
           for i in 0 .. text.Length - 1 do
               if text[i] = '\n' then
                   yield i + 1 |]

    member _.Offset(line: int, column: int) =
        if line < 1 || line > starts.Length then text.Length else min text.Length (starts[line - 1] + column)

    member this.Slice(r: range) =
        let a = this.Offset(r.StartLine, r.StartColumn)
        let b = this.Offset(r.EndLine, r.EndColumn)
        if b > a then text.Substring(a, b - a) else ""

    /// The text of `outer` with each `(inner, replacement)` span substituted.
    member this.Splice(outer: range, edits: (range * string) list) =
        let sb = Text.StringBuilder()
        let mutable at = this.Offset(outer.StartLine, outer.StartColumn)

        for inner, replacement in edits |> List.sortBy (fun (r, _) -> r.StartLine, r.StartColumn) do
            let a = this.Offset(inner.StartLine, inner.StartColumn)
            sb.Append(text, at, max 0 (a - at)).Append(replacement) |> ignore
            at <- this.Offset(inner.EndLine, inner.EndColumn)

        let b = this.Offset(outer.EndLine, outer.EndColumn)
        sb.Append(text, at, max 0 (b - at)).ToString()

/// A colour found in source: where it is, its channels in `[0, 1]` (r, g, b, a), and how to respell it for a picked
/// colour as replacements of spans inside `Range`.
type private Found =
    { Range: range
      Rgba: float[]
      Respell: float[] -> (range * string) list }

let private toByte (v: float) = int (Math.Round(Math.Clamp(v, 0.0, 1.0) * 255.0))

// --- Hex digits ------------------------------------------------------------------------------------------------

let private hexString = Regex("^(#?)([0-9a-fA-F]+)$", RegexOptions.Compiled)
let private hexInteger = Regex("^(0[xX])([0-9a-fA-F_]+)([uUyYsSlLnN]*)$", RegexOptions.Compiled)

/// Channels in `[0, 1]` (r, g, b, a) from 3, 4, 6 or 8 hex digits.
let private ofDigits (order: HexByteOrder) (digits: string) : float[] =
    let wide =
        if digits.Length <= 4 then
            String(digits.ToCharArray() |> Array.collect (fun c -> [| c; c |]))
        else
            digits

    let bytes =
        [| for i in 0..2 .. wide.Length - 1 -> float (Convert.ToByte(wide.Substring(i, 2), 16)) / 255.0 |]

    match bytes with
    | [| r; g; b |] -> [| r; g; b; 1.0 |]
    | [| a; r; g; b |] when order = HexByteOrder.Argb -> [| r; g; b; a |]
    | [| r; g; b; a |] -> [| r; g; b; a |]
    | _ -> [| 0.0; 0.0; 0.0; 1.0 |]

/// Hex digits for `rgba`: 6 when opaque and the original had no alpha, else 8 in `order`.
let private toDigits (order: HexByteOrder) (hadAlpha: bool) (upper: bool) (rgba: float[]) =
    let r, g, b, a = toByte rgba[0], toByte rgba[1], toByte rgba[2], toByte rgba[3]
    let f = if upper then "X2" else "x2"
    let hex (v: int) = v.ToString(f, CultureInfo.InvariantCulture)

    if not hadAlpha && a = 255 then hex r + hex g + hex b
    elif order = HexByteOrder.Argb then hex a + hex r + hex g + hex b
    else hex r + hex g + hex b + hex a

let private hasUpper (s: string) = s |> Seq.exists (fun c -> c >= 'A' && c <= 'F')
let private hasLower (s: string) = s |> Seq.exists (fun c -> c >= 'a' && c <= 'f')

// --- Names -----------------------------------------------------------------------------------------------------

let private otherSpace = Regex("(hs[lvb]|lch|oklab|lab$|cmyk|xyz|yuv)", RegexOptions.Compiled)

/// A name that says colour (color, colour, rgb) and not another colour space (hsl, lab, cmyk, ...).
let private colourName (name: string) =
    let n = name.ToLowerInvariant()
    (n.Contains "color" || n.Contains "colour" || n.Contains "rgb") && not (otherSpace.IsMatch n)

/// The byte order a name asks for, if it names one.
let private orderOf (name: string) =
    let n = name.ToLowerInvariant()
    if n.Contains "argb" then Some HexByteOrder.Argb
    elif n.Contains "rgba" then Some HexByteOrder.Rgba
    else None

let private qualifiers = set [ "color"; "colors"; "colour"; "colours"; "palette" ]

/// The CSS/X11 named colours: .NET's known colours without the OS-dependent system colours.
let private namedColours =
    lazy
        [| for known in Enum.GetValues<KnownColor>() do
               let c = Color.FromKnownColor known

               if not c.IsSystemColor then
                   known.ToString(), [| float c.R / 255.0; float c.G / 255.0; float c.B / 255.0; float c.A / 255.0 |] |]

let private namedLookup =
    lazy (namedColours.Value |> dict |> fun d -> Collections.Generic.Dictionary(d, StringComparer.OrdinalIgnoreCase))

let private tryNamed (name: string) =
    let key = Regex.Replace(name, "grey", "gray", RegexOptions.IgnoreCase)

    match namedLookup.Value.TryGetValue key with
    | true, rgba -> Some rgba
    | _ -> None

/// The named colour closest to `rgba`, spelled in the casing of `like`.
let private nearestNamed (like: string) (rgba: float[]) =
    let name, _ =
        namedColours.Value
        |> Array.minBy (fun (_, c) -> Array.fold2 (fun acc x y -> acc + (x - y) * (x - y)) 0.0 c rgba)

    if like = like.ToLowerInvariant() then name.ToLowerInvariant()
    elif Char.IsUpper like[0] then name
    else string (Char.ToLowerInvariant name[0]) + name.Substring 1

// --- Numeric channel literals ----------------------------------------------------------------------------------

type private Channel =
    { Range: range
      Value: float
      IsFloat: bool
      Text: string
      /// 1 for a normalised channel, 255 for a byte-scale one.
      Scale: float }

let private trailingLetters = Regex("[a-zA-Z]*$", RegexOptions.Compiled)

let private literal (source: Source) (e: SynExpr) : Channel option =
    match e with
    | SynExpr.Const(c, r) ->
        let number =
            match c with
            | SynConst.Byte v -> Some(float v, false)
            | SynConst.SByte v -> Some(float v, false)
            | SynConst.Int16 v -> Some(float v, false)
            | SynConst.UInt16 v -> Some(float v, false)
            | SynConst.Int32 v -> Some(float v, false)
            | SynConst.UInt32 v -> Some(float v, false)
            | SynConst.Int64 v -> Some(float v, false)
            | SynConst.UInt64 v -> Some(float v, false)
            | SynConst.Double v -> Some(v, true)
            | SynConst.Single v -> Some(float v, true)
            | SynConst.Decimal v -> Some(float v, true)
            | _ -> None

        number
        |> Option.map (fun (value, isFloat) ->
            { Range = r
              Value = value
              IsFloat = isFloat
              Text = source.Slice r
              Scale = 255.0 })
    | _ -> None

/// Channels in r, g, b (, a) order with their scale: integers are bytes; floats follow `mode`, where `Auto` makes them
/// normalised when every float colour channel is at most 1. `None` when a channel is out of range.
let private scaled (mode: FloatScale) (channels: Channel list) : Channel list option =
    let auto (cs: Channel list) =
        if cs |> List.forall (fun c -> not c.IsFloat || c.Value <= 1.0) then 1.0 else 255.0

    let floatScale (i: int) (c: Channel) =
        match mode with
        | FloatScale.Normalized -> 1.0
        | FloatScale.Byte -> 255.0
        | FloatScale.Auto when i < 3 -> auto (List.truncate 3 channels)
        | FloatScale.Auto -> auto [ c ]

    let withScale =
        channels
        |> List.mapi (fun i c -> { c with Scale = if c.IsFloat then floatScale i c else 255.0 })

    if withScale |> List.forall (fun c -> c.Value >= 0.0 && c.Value <= c.Scale) then Some withScale else None

let private render (c: Channel) (v: float) =
    let value = Math.Clamp(v, 0.0, 1.0) * c.Scale

    if c.IsFloat then
        let body =
            if c.Scale = 1.0 then
                Math.Round(value, 3).ToString("0.0##", CultureInfo.InvariantCulture)
            else
                Math.Round(value).ToString("0", CultureInfo.InvariantCulture) + ".0"

        body + trailingLetters.Match(c.Text).Value
    else
        let n = int (Math.Round value)
        let m = hexInteger.Match c.Text

        if m.Success then
            let digits = m.Groups[2].Value
            let f = if hasLower digits then "x2" else "X2"
            m.Groups[1].Value + n.ToString(f, CultureInfo.InvariantCulture) + m.Groups[3].Value
        else
            n.ToString(CultureInfo.InvariantCulture) + trailingLetters.Match(c.Text).Value

/// A colour from channel literals already in r, g, b (, a) order.
let private ofChannels (mode: FloatScale) (outer: range) (channels: Channel list) : Found option =
    scaled mode channels
    |> Option.map (fun chans ->
        let values = chans |> List.map (fun c -> c.Value / c.Scale)

        { Range = outer
          Rgba =
            match values with
            | [ r; g; b ] -> [| r; g; b; 1.0 |]
            | _ -> List.toArray values
          Respell = fun picked -> chans |> List.mapi (fun i c -> c.Range, render c picked[i]) })

/// Channel literals in source order, reordered to r, g, b, a when the name puts alpha first.
let private ofArguments (mode: FloatScale) (source: Source) (outer: range) (alphaFirst: bool) (args: SynExpr list) =
    let channels = args |> List.choose (literal source)

    if channels.Length <> args.Length || (args.Length <> 3 && args.Length <> 4) then
        None
    else
        match channels with
        | [ a; r; g; b ] when alphaFirst -> ofChannels mode outer [ r; g; b; a ]
        | _ -> ofChannels mode outer channels

// --- Syntax context --------------------------------------------------------------------------------------------

type private Context =
    /// A binding, record field or named argument.
    | Named of string
    /// An argument of a call: the callee's name, the call, and whether the argument is the call's whole argument.
    | Call of name: string * call: range * direct: bool

let rec private headName (e: SynExpr) : string option =
    match e with
    | SynExpr.Ident id -> Some id.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) -> Some(ids |> List.map (fun i -> i.idText) |> String.concat ".")
    | SynExpr.TypeApp(expr = inner) -> headName inner
    | SynExpr.DotGet(expr = inner; longDotId = SynLongIdent(id = ids)) ->
        headName inner
        |> Option.map (fun h -> h + "." + (ids |> List.map (fun i -> i.idText) |> String.concat "."))
    | _ -> None

let rec private typeName (t: SynType) : string option =
    match t with
    | SynType.LongIdent(SynLongIdent(id = ids)) -> Some(ids |> List.map (fun i -> i.idText) |> String.concat ".")
    | SynType.App(typeName = inner) -> typeName inner
    | _ -> None

let rec private patName (p: SynPat) : string option =
    match p with
    | SynPat.Named(ident = SynIdent(id, _)) -> Some id.idText
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) -> ids |> List.tryLast |> Option.map (fun i -> i.idText)
    | SynPat.Typed(pat = inner)
    | SynPat.Paren(pat = inner)
    | SynPat.Attrib(pat = inner) -> patName inner
    | _ -> None

let private isEquality (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> id.idText = "op_Equality"
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ id ])) -> id.idText = "op_Equality"
    | _ -> false

let private contains (outer: range) (inner: range) = Range.rangeContainsRange outer inner

let private allOrNone (xs: 'T option list) = if List.forall Option.isSome xs then Some(List.choose id xs) else None

/// A record expression's field names and values; `None` when it spreads another record.
let private recordFields (fields: SynExprRecordFieldOrSpread list) : (string * SynExpr option) list option =
    fields
    |> List.map (function
        | SynExprRecordFieldOrSpread.Field(SynExprRecordField(fieldName = (SynLongIdent(id = ids), _); expr = value), _) ->
            ids |> List.tryLast |> Option.map (fun i -> i.idText, value)
        | SynExprRecordFieldOrSpread.Spread _ -> None)
    |> allOrNone

/// An anonymous record expression's field names and values; `None` when it spreads another record.
let private anonFields (fields: SynExprAnonRecordFieldOrSpread list) : (string * SynExpr option) list option =
    fields
    |> List.map (function
        | SynExprAnonRecordFieldOrSpread.Field(SynExprAnonRecordField(fieldName = SynLongIdent(id = ids); expr = value), _) ->
            ids |> List.tryLast |> Option.map (fun i -> i.idText, Some value)
        | SynExprAnonRecordFieldOrSpread.Spread _ -> None)
    |> allOrNone

let private fieldHolding (child: range) (fields: (string * SynExpr option) list option) =
    fields
    |> Option.bind (
        List.tryPick (fun (name, value) ->
            match value with
            | Some v when contains v.Range child -> Some(Named name)
            | _ -> None)
    )

/// What names the expression at `child`: walks out through parentheses, type annotations and list or array
/// literals to a binding, a record field, a named argument or a call.
let rec private contextOf (child: range) (direct: bool) (path: SyntaxVisitorPath) : Context option =
    match path with
    | SyntaxNode.SynExpr e :: rest ->
        match e with
        | SynExpr.Paren _ -> contextOf e.Range direct rest
        | SynExpr.Typed _
        | SynExpr.ArrayOrList _
        | SynExpr.ArrayOrListComputed _
        | SynExpr.Sequential _ -> contextOf e.Range false rest
        | SynExpr.Record(recordFields = fields) -> fieldHolding child (recordFields fields)
        | SynExpr.AnonRecd(recordFields = fields) -> fieldHolding child (anonFields fields)
        | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; funcExpr = op; argExpr = SynExpr.Ident name); argExpr = value) when
            isEquality op && contains value.Range child
            ->
            Some(Named name.idText)
        | SynExpr.App(isInfix = false; funcExpr = f; argExpr = arg) when contains arg.Range child ->
            headName f |> Option.map (fun n -> Call(n, e.Range, direct))
        | SynExpr.New(targetType = t; expr = arg) when contains arg.Range child ->
            typeName t |> Option.map (fun n -> Call(n, e.Range, direct))
        | _ -> None
    | SyntaxNode.SynBinding(SynBinding(headPat = pat)) :: _ -> patName pat |> Option.map Named
    | _ -> None

let private contextName (context: Context option) =
    match context with
    | Some(Named n)
    | Some(Call(n, _, _)) -> Some n
    | None -> None

// --- Detection -------------------------------------------------------------------------------------------------

let private channelIndex (field: string) =
    match field.ToLowerInvariant() with
    | "r"
    | "red" -> Some 0
    | "g"
    | "green" -> Some 1
    | "b"
    | "blue" -> Some 2
    | "a"
    | "alpha" -> Some 3
    | _ -> None

let private ofRecord (mode: FloatScale) (source: Source) (outer: range) (fields: (string * SynExpr option) list) : Found option =
    if fields.Length <> 3 && fields.Length <> 4 then
        None
    else
        let indexed =
            fields
            |> List.choose (fun (name, value) ->
                match channelIndex name, value |> Option.bind (literal source) with
                | Some i, Some c -> Some(i, c)
                | _ -> None)

        let indices = indexed |> List.map fst |> List.sort

        if indices = [ 0 .. fields.Length - 1 ] then
            indexed |> List.sortBy fst |> List.map snd |> ofChannels mode outer
        else
            None

/// `app` is the function part of the application above it (an inner step of a curried call).
let private isCalleeOfParent (app: SynExpr) (path: SyntaxVisitorPath) =
    match path with
    | SyntaxNode.SynExpr(SynExpr.App(funcExpr = f)) :: _ -> Range.equals f.Range app.Range
    | _ -> false

let rec private spine (e: SynExpr) (args: SynExpr list) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = f; argExpr = a) -> spine f (a :: args)
    | head -> head, args

/// `"#rgb"`, `"#rgba"`, `"#rrggbb"` and `"#rrggbbaa"`; with `StringsWithoutHash`, also 6 or 8 digits without the `#`
/// (3 and 4 are left out there: too many words are short hex).
let private ofHexString (config: ColorsConfig) (source: Source) (path: SyntaxVisitorPath) (value: string) (r: range) =
    let m = hexString.Match value
    let prefix = if m.Success then m.Groups[1].Value else ""
    let digits = if m.Success then m.Groups[2].Value else ""

    let admitted =
        match prefix, digits.Length with
        | "#", (3 | 4 | 6 | 8) -> true
        | "", (6 | 8) -> config.StringsWithoutHash
        | _ -> false

    if r.StartLine <> r.EndLine || not admitted then
        None
    else
        let literalText = source.Slice r
        let at = literalText.IndexOf(value, StringComparison.Ordinal)

        if at < 0 then
            None
        else
            let start = Position.mkPos r.StartLine (r.StartColumn + at)
            let span = Range.mkRange r.FileName start (Position.mkPos r.StartLine (start.Column + value.Length))

            let order =
                contextOf r true path |> contextName |> Option.bind orderOf |> Option.defaultValue config.HexByteOrder

            let hadAlpha = digits.Length = 4 || digits.Length = 8

            Some
                { Range = span
                  Rgba = ofDigits order digits
                  Respell = fun picked -> [ span, prefix + toDigits order hadAlpha (hasUpper digits) picked ] }

let private ofHexInteger (config: ColorsConfig) (source: Source) (path: SyntaxVisitorPath) (r: range) =
    let m = hexInteger.Match(source.Slice r)

    if not m.Success then
        None
    else
        let digits = m.Groups[2].Value.Replace("_", "")
        let name = contextOf r true path |> contextName

        // 8 digits are as often a hash or a mask (0xEDB88320u, 0xFFFF_FFFFu), so they need a colour name around them.
        if digits.Length <> 6 && not (digits.Length = 8 && name |> Option.exists colourName) then
            None
        else
            let order = name |> Option.bind orderOf |> Option.defaultValue config.HexByteOrder

            let upper = not (hasLower digits)

            Some
                { Range = r
                  Rgba = ofDigits order digits
                  Respell =
                    fun picked ->
                        [ r, m.Groups[1].Value + toDigits order (digits.Length = 8) upper picked + m.Groups[3].Value ] }

let private ofMember (ids: Ident list) (r: range) =
    match List.rev ids with
    | name :: qualifier :: _ when qualifiers.Contains(qualifier.idText.ToLowerInvariant()) ->
        tryNamed name.idText
        |> Option.map (fun rgba ->
            { Range = r
              Rgba = rgba
              Respell = fun picked -> [ name.idRange, nearestNamed name.idText picked ] })
    | _ -> None

let private ofTuple (config: ColorsConfig) (source: Source) (path: SyntaxVisitorPath) (exprs: SynExpr list) (r: range) =
    let outer =
        match path with
        | SyntaxNode.SynExpr(SynExpr.Paren(range = p)) :: _ -> p
        | _ -> r

    match contextOf r true path with
    | Some(Call(name, call, true)) when config.Constructors && colourName name ->
        ofArguments config.FloatScale source call (orderOf name = Some HexByteOrder.Argb) exprs
    | Some(Call(name, _, false))
    | Some(Named name) when config.Tuples && colourName name ->
        ofArguments config.FloatScale source outer (orderOf name = Some HexByteOrder.Argb) exprs
    | _ -> None

let private visit (config: ColorsConfig) (source: Source) (path: SyntaxVisitorPath) (e: SynExpr) : Found option =
    match e with
    | SynExpr.Const(SynConst.String(text = value), r) when config.Strings -> ofHexString config source path value r
    | SynExpr.Const(_, r) when config.HexIntegers -> ofHexInteger config source path r
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids); range = r) when config.Members -> ofMember ids r
    | SynExpr.Record(copyInfo = None; recordFields = fields; range = r) when config.Records ->
        recordFields fields |> Option.bind (ofRecord config.FloatScale source r)
    | SynExpr.AnonRecd(copyInfo = None; recordFields = fields; range = r) when config.Records ->
        anonFields fields |> Option.bind (ofRecord config.FloatScale source r)
    | SynExpr.Tuple(exprs = exprs; range = r) when exprs.Length = 3 || exprs.Length = 4 -> ofTuple config source path exprs r
    | SynExpr.App(isInfix = false; range = r) when config.Constructors && not (isCalleeOfParent e path) ->
        match spine e [] with
        | head, args when args.Length = 3 || args.Length = 4 ->
            match headName head with
            | Some name when colourName name -> ofArguments config.FloatScale source r (orderOf name = Some HexByteOrder.Argb) args
            | _ -> None
        | _ -> None
    | _ -> None

/// Every colour in the file, outermost first where two overlap, in source order.
let private collect (config: ColorsConfig) (text: string) (tree: ParsedInput) : Found list =
    if not config.Enabled then
        []
    else
        StackGuard.onLargeStack (fun () ->
            let source = Source text

            let found =
                ([], tree)
                ||> ParsedInput.fold (fun acc path node ->
                    match node with
                    | SyntaxNode.SynExpr e ->
                        match visit config source path e with
                        | Some f -> f :: acc
                        | None -> acc
                    | _ -> acc)

            let found: Found list = found

            found
            |> List.filter (fun f ->
                not (found |> List.exists (fun o -> not (Range.equals o.Range f.Range) && contains o.Range f.Range)))
            |> List.distinctBy (fun f -> f.Range)
            |> List.sortBy (fun f -> f.Range.StartLine, f.Range.StartColumn))

let private toLsp (r: range) : LspRange =
    { Start =
        { Line = uint32 (max 0 (r.StartLine - 1))
          Character = uint32 (max 0 r.StartColumn) }
      End =
        { Line = uint32 (max 0 (r.EndLine - 1))
          Character = uint32 (max 0 r.EndColumn) } }

let internal find (config: ColorsConfig) (text: string) (tree: ParsedInput) : ColorInformation[] =
    collect config text tree
    |> List.map (fun f ->
        { Range = toLsp f.Range
          Color =
            { Red = decimal f.Rgba[0]
              Green = decimal f.Rgba[1]
              Blue = decimal f.Rgba[2]
              Alpha = decimal f.Rgba[3] } })
    |> List.toArray

let private newlines = Regex(@"\s*\r?\n\s*", RegexOptions.Compiled)

let internal presentations
    (config: ColorsConfig)
    (text: string)
    (tree: ParsedInput)
    (range: LspRange)
    (color: LspColor)
    : ColorPresentation[] =
    let found = collect config text tree

    let starts (r: LspRange) (p: Ionide.LanguageServerProtocol.Types.Position) =
        (r.Start.Line, r.Start.Character) <= (p.Line, p.Character)
        && (p.Line, p.Character) <= (r.End.Line, r.End.Character)

    let target =
        found
        |> List.tryFind (fun f -> toLsp f.Range = range)
        |> Option.orElse (found |> List.tryFind (fun f -> starts (toLsp f.Range) range.Start))

    match target with
    | None -> [||]
    | Some f ->
        let picked =
            [| float color.Red; float color.Green; float color.Blue; float color.Alpha |]
            |> Array.map (fun v -> Math.Clamp(v, 0.0, 1.0))

        let newText = (Source text).Splice(f.Range, f.Respell picked)

        [| { Label = newlines.Replace(newText, " ")
             TextEdit = Some { Range = toLsp f.Range; NewText = newText }
             AdditionalTextEdits = None } |]

// --- Hover swatches --------------------------------------------------------------------------------------------

/// The colour of the literal at a 0-based position, if one is there.
let internal literalAt (config: ColorsConfig) (text: string) (tree: ParsedInput) (line: int) (character: int) =
    let at = Position.mkPos (line + 1) character
    collect config text tree |> List.tryFind (fun f -> Range.rangeContainsPos f.Range at) |> Option.map _.Rgba

let rec private unwrap (e: SynExpr) =
    match e with
    | SynExpr.Paren(expr = inner)
    | SynExpr.Typed(expr = inner) -> unwrap inner
    | _ -> e

/// The colour the binding declared at `declaration` binds: its body is a colour, a literal holding one, or a call with
/// one argument holding one (`hex "#e3c58a"`). A body with several colours binds none.
let internal boundAt (config: ColorsConfig) (text: string) (tree: ParsedInput) (declaration: range) : float[] option =
    let found = collect config text tree

    let bodies =
        StackGuard.onLargeStack (fun () ->
            ([], tree)
            ||> ParsedInput.fold (fun acc _ node ->
                match node with
                | SyntaxNode.SynBinding(SynBinding(headPat = pat; expr = body)) when
                    Range.rangeContainsPos pat.Range declaration.Start
                    ->
                    body :: acc
                | _ -> acc))

    bodies
    |> List.tryPick (fun body ->
        let inner = unwrap body

        match found |> List.filter (fun f -> contains body.Range f.Range) with
        | [ f ] ->
            let direct =
                Range.equals f.Range body.Range
                || Range.equals f.Range inner.Range
                || (match inner with
                    | SynExpr.Const _ -> true
                    | SynExpr.App _ -> (spine inner [] |> snd |> List.length) = 1
                    | _ -> false)

            if direct then Some f.Rgba else None
        | _ -> None)

/// A hover line for a colour: a swatch when the client renders `<span>` in markdown, then its hex and rgb(a).
let internal swatchMarkdown (html: bool) (rgba: float[]) =
    let r, g, b, a = toByte rgba[0], toByte rgba[1], toByte rgba[2], toByte rgba[3]
    let opaque = sprintf "#%02x%02x%02x" r g b
    let hex = if a = 255 then opaque else opaque + sprintf "%02x" a

    let channels =
        if a = 255 then
            sprintf "rgb(%d, %d, %d)" r g b
        else
            sprintf "rgba(%d, %d, %d, %s)" r g b (Math.Round(float a / 255.0, 2).ToString(CultureInfo.InvariantCulture))

    // VS Code keeps a span's style only as color, background-color and border-radius, in that order, without spaces.
    let swatch =
        if html then
            sprintf "<span style=\"color:%s;background-color:%s;border-radius:3px;\">████</span> " hex hex
        else
            ""

    sprintf "%s`%s` %s" swatch hex channels

/// The swatch a hover adds: the colour literal under the cursor, else the colour the hovered value's declaration binds,
/// found through go-to-definition (which waits on the file's check; the caller bounds that wait).
let hoverSwatch
    (config: ColorsConfig)
    (html: bool)
    (ownersOf: string -> ProjectGraph list)
    (graph: ProjectGraph)
    (path: string)
    (line: int)
    (character: int)
    (ct: CancellationToken)
    : Task<string option> =
    task {
        if not (config.Enabled && config.Hover) then
            return None
        else
            let! view = graph.LatestParse(path, ct)

            match literalAt config view.Text view.Parse.ParseTree line character with
            | Some rgba -> return Some(swatchMarkdown html rgba)
            | None ->
                let! declaration = Definition.at ownersOf graph path line character ct

                match declaration |> Option.map (fun d -> d, ownersOf d.FileName) with
                | Some(d, owner :: _) ->
                    let! declared = owner.LatestParse(d.FileName, ct)
                    return boundAt config declared.Text declared.Parse.ParseTree d |> Option.map (swatchMarkdown html)
                | _ -> return None
    }

/// Parses for a file that no loaded project holds (one opened while the workspace loads, or a loose script), without
/// a project's defines: colours need only the syntax.
let private looseChecker = lazy (FSharpChecker.Create(keepAssemblyContents = false))

let private parseLoose (path: string) (text: string) : ParsedInput =
    StackGuard.onLargeStack (fun () ->
        let options = { FSharpParsingOptions.Default with SourceFiles = [| path |] }
        (looseChecker.Value.ParseFile(path, SourceText.ofString text, options) |> Async.RunSynchronously).ParseTree)

let internal looseDocument (config: ColorsConfig) (path: string) (text: string) : ColorInformation[] =
    if config.Enabled then find config text (parseLoose path text) else [||]

let internal loosePresentation (config: ColorsConfig) (path: string) (text: string) (range: LspRange) (color: LspColor) =
    if config.Enabled then presentations config text (parseLoose path text) range color else [||]

let document (config: ColorsConfig) (graph: ProjectGraph) (path: string) (ct: CancellationToken) : Task<ColorInformation[]> =
    task {
        if not config.Enabled then
            return [||]
        else
            let! view = graph.LatestParse(path, ct)
            return find config view.Text view.Parse.ParseTree
    }

let presentation
    (config: ColorsConfig)
    (graph: ProjectGraph)
    (path: string)
    (range: LspRange)
    (color: LspColor)
    (ct: CancellationToken)
    : Task<ColorPresentation[]> =
    task {
        if not config.Enabled then
            return [||]
        else
            let! view = graph.LatestParse(path, ct)
            return presentations config view.Text view.Parse.ParseTree range color
    }
