module Rfs.Server.Program

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Threading
open Rfs.Server

type internal RunOptions =
    { Verbose: bool
      AttachDebugger: bool
      WaitForDebugger: bool
      WorkspaceDiagnostics: bool
      LogFile: string option }

    static member Default =
        { Verbose = false
          AttachDebugger = false
          WaitForDebugger = false
          WorkspaceDiagnostics = false
          LogFile = None }

type internal CliAction =
    | PrintVersion
    | PrintHelp
    | RunServer of RunOptions * unrecognized: string list

let internal informationalVersion () : string =
    let asm = typeof<RunOptions>.Assembly

    match Attribute.GetCustomAttribute(asm, typeof<AssemblyInformationalVersionAttribute>) with
    | :? AssemblyInformationalVersionAttribute as attr when not (String.IsNullOrWhiteSpace attr.InformationalVersion) ->
        attr.InformationalVersion
    | _ ->
        match asm.GetName().Version with
        | null -> RanvierServer.Version
        | v -> v.ToString()

let internal helpText () : string =
    String.Join(
        Environment.NewLine,
        [ $"ranvierautocomplete {informationalVersion ()}"
          "Demand-driven incremental F# language server powered by Ranvier (LSP over stdio)."
          ""
          "Usage:"
          "  ranvierautocomplete [options]"
          ""
          "Options:"
          "  --version                      Print version information and exit"
          "  -h, --help                     Show help and usage information and exit"
          "  -v, --verbose, --debug         Enable verbose diagnostic logging on stderr"
          "  -l, --logfile, --log-file <f>  Mirror diagnostic logs to the specified file"
          "  --workspace-diagnostics        Publish closed-file diagnostics across loaded projects"
          "  --attach-debugger              Launch the system debugger on startup"
          "  --wait-for-debugger            Wait for an external debugger to attach on startup"
          ""
          "FSAC compatibility flags (--adaptive-lsp-server-enabled, --use-fcs-transparent-compiler,"
          "--project-graph-enabled, --state-directory, --otel-exporter-enabled, --log-level, --filter)"
          "are accepted for drop-in compatibility with Ionide and other LSP clients." ]
    )

let private isFlag (s: string) = s.StartsWith("-", StringComparison.Ordinal)

let private trySplitEq (arg: string) =
    let idx = arg.IndexOf('=')

    if idx > 0 then
        Some(arg.Substring(0, idx), arg.Substring(idx + 1))
    else
        None

let private isOptionalBoolLiteral (s: string) =
    String.Equals(s, "true", StringComparison.OrdinalIgnoreCase)
    || String.Equals(s, "false", StringComparison.OrdinalIgnoreCase)

let internal parseArgs (argv: string array) : CliAction =
    if isNull argv || argv.Length = 0 then
        RunServer(RunOptions.Default, [])
    elif argv |> Array.exists (fun a -> a = "--version") then
        // Note: in FSAC/Ionide, `-v` means `--verbose`, NOT `--version`.
        PrintVersion
    elif argv |> Array.exists (fun a -> a = "-h" || a = "--help" || a = "-?") then
        PrintHelp
    else
        let mutable opts = RunOptions.Default
        let unrecognized = Collections.Generic.List<string>()
        let mutable i = 0

        let consumeOptionalBool () =
            if i + 1 < argv.Length && isOptionalBoolLiteral argv[i + 1] then
                i <- i + 1

        let consumeOptionalBoolValue (defaultVal: bool) =
            if i + 1 < argv.Length && isOptionalBoolLiteral argv[i + 1] then
                i <- i + 1
                not (String.Equals(argv[i], "false", StringComparison.OrdinalIgnoreCase))
            else
                defaultVal

        let consumeValue () : string option =
            if i + 1 < argv.Length && not (isFlag argv[i + 1]) then
                i <- i + 1
                Some argv[i]
            else
                None

        while i < argv.Length do
            let raw = argv[i]

            match trySplitEq raw with
            | Some(flag, value) ->
                match flag with
                | "-l"
                | "--logfile"
                | "--log-file" ->
                    if not (String.IsNullOrWhiteSpace value) then
                        opts <- { opts with LogFile = Some value }
                | "-v"
                | "--verbose"
                | "--debug" ->
                    opts <- { opts with Verbose = not (String.Equals(value, "false", StringComparison.OrdinalIgnoreCase)) }
                | "--attach-debugger" ->
                    opts <- { opts with AttachDebugger = not (String.Equals(value, "false", StringComparison.OrdinalIgnoreCase)) }
                | "--wait-for-debugger"
                | "--attachdebugger" ->
                    opts <- { opts with WaitForDebugger = not (String.Equals(value, "false", StringComparison.OrdinalIgnoreCase)) }
                | "--workspace-diagnostics" ->
                    opts <- { opts with WorkspaceDiagnostics = not (String.Equals(value, "false", StringComparison.OrdinalIgnoreCase)) }
                | "--state-directory"
                | "--log-level"
                | "--filter"
                | "--log-filter"
                | "--project-graph-enabled"
                | "--adaptive-lsp-server-enabled"
                | "--otel-exporter-enabled"
                | "--use-fcs-transparent-compiler"
                | "--background-service-enabled" -> ()
                | _ -> unrecognized.Add raw
            | None ->
                match raw with
                | "-v"
                | "--verbose"
                | "--debug" ->
                    consumeOptionalBool ()
                    opts <- { opts with Verbose = true }
                | "--attach-debugger" ->
                    consumeOptionalBool ()
                    opts <- { opts with AttachDebugger = true }
                | "--wait-for-debugger"
                | "--attachdebugger" ->
                    consumeOptionalBool ()
                    opts <- { opts with WaitForDebugger = true }
                | "--workspace-diagnostics" ->
                    opts <- { opts with WorkspaceDiagnostics = consumeOptionalBoolValue true }
                | "-l"
                | "--logfile"
                | "--log-file" ->
                    match consumeValue () with
                    | Some path when not (String.IsNullOrWhiteSpace path) ->
                        opts <- { opts with LogFile = Some path }
                    | _ -> ()
                | "--state-directory"
                | "--log-level" ->
                    consumeValue () |> ignore
                | "--filter"
                | "--log-filter" ->
                    while i + 1 < argv.Length && not (isFlag argv[i + 1]) do
                        i <- i + 1
                | "--project-graph-enabled"
                | "--adaptive-lsp-server-enabled"
                | "--otel-exporter-enabled"
                | "--use-fcs-transparent-compiler"
                | "--background-service-enabled"
                | "--stdio" ->
                    consumeOptionalBool ()
                | _ -> unrecognized.Add raw

            i <- i + 1

        RunServer(opts, List.ofSeq unrecognized)

type private TeeTextWriter(first: TextWriter, second: TextWriter) =
    inherit TextWriter()
    override _.Encoding = first.Encoding

    override _.Write(value: char) =
        first.Write value
        second.Write value

    override _.Write(value: string) =
        first.Write value
        second.Write value

    override _.WriteLine(value: string) =
        first.WriteLine value
        second.WriteLine value

    override _.Flush() =
        first.Flush()
        second.Flush()

    override _.Dispose(disposing: bool) =
        if disposing then
            try
                second.Flush()
                second.Dispose()
            with _ ->
                ()

        base.Dispose disposing

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | PrintVersion ->
        Console.Out.WriteLine($"ranvierautocomplete {informationalVersion ()}")
        0
    | PrintHelp ->
        Console.Out.WriteLine(helpText ())
        0
    | RunServer(opts, unrecognized) ->
        use input = Console.OpenStandardInput()
        use output = Console.OpenStandardOutput()

        // A library writing to Console.Out would corrupt the protocol on stdout; logs go to stderr.
        Console.SetOut TextWriter.Null

        let originalErr = Console.Error

        use logTee =
            match opts.LogFile with
            | None -> { new IDisposable with member _.Dispose() = () }
            | Some logPath ->
                try
                    let full = Path.GetFullPath logPath

                    match Path.GetDirectoryName full with
                    | null
                    | "" -> ()
                    | dir -> Directory.CreateDirectory dir |> ignore

                    let fs = new FileStream(full, FileMode.Append, FileAccess.Write, FileShare.ReadWrite)
                    let sw = new StreamWriter(fs, AutoFlush = true)
                    let tee = new TeeTextWriter(originalErr, sw)
                    Console.SetError(TextWriter.Synchronized tee)

                    { new IDisposable with
                        member _.Dispose() =
                            Console.SetError originalErr
                            tee.Dispose() }
                with ex ->
                    eprintfn "[ranvierautocomplete] could not open log file '%s': %s" logPath ex.Message
                    { new IDisposable with member _.Dispose() = () }

        if not unrecognized.IsEmpty then
            eprintfn "[ranvierautocomplete] ignoring unrecognized CLI arguments: %s" (String.Join(", ", unrecognized))

        if opts.AttachDebugger then
            Debugger.Launch() |> ignore

        if opts.WaitForDebugger then
            eprintfn "[ranvierautocomplete] waiting for debugger to attach (PID %d)..." Environment.ProcessId

            while not Debugger.IsAttached do
                Thread.Sleep 100

        if opts.Verbose then
            eprintfn "[ranvierautocomplete] starting %s (PID %d)" (informationalVersion ()) Environment.ProcessId

        Server.runWith
            (fun server ->
                if opts.WorkspaceDiagnostics then
                    server.EnableWorkspaceDiagnostics())
            input
            output
