namespace Rfs.Server

open System
open System.IO
open System.Text.RegularExpressions

module Paths =
    let private driveLetter = Regex(@"^[A-Za-z]:", RegexOptions.Compiled)

    /// The full local path of a `file:` URI: percent escapes decoded, the drive letter uppercased. Anything that is not
    /// a `file:` URI is taken as a path already.
    let ofUri (uri: string) : string =
        let path =
            if uri.StartsWith("file:", StringComparison.OrdinalIgnoreCase) then
                // `file:///c%3A/x` and `file:///C:/x` both name C:\x; `file:///home/x` names /home/x
                let afterScheme = uri.Substring(uri.IndexOf ':' + 1).TrimStart('/')
                let decoded = Uri.UnescapeDataString afterScheme

                if driveLetter.IsMatch decoded then
                    string (Char.ToUpperInvariant decoded[0]) + decoded.Substring 1
                else
                    "/" + decoded
            else
                uri

        Path.GetFullPath path

    /// The `file:///` URI of a path, each segment escaped, the drive letter left as it is.
    let toUri (path: string) : string =
        let full = Path.GetFullPath path
        let segments = full.Replace('\\', '/').Split('/')

        let escaped =
            segments
            |> Array.mapi (fun i s -> if i = 0 && driveLetter.IsMatch s then s else Uri.EscapeDataString s)

        let joined = String.Join("/", escaped)
        if joined.StartsWith "/" then "file://" + joined else "file:///" + joined
