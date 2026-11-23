namespace Rfs.Agent

open System
open System.Threading
open System.Threading.Tasks
open Ranvier
open Ranvier.Keyboard
open Ranvier.Terminal
open Ranvier.Terminal.Css
open Ranvier.Terminal.View
open Ranvier.Terminal.View.Dsl

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type internal PendingAction =
    | Stop of MonitorRow
    | StopFlagged of MonitorRow array

/// What the monitor runs on. The interactive app uses the real sampler and actions; tests substitute fakes.
[<NoComparison; NoEquality>]
type internal MonitorServices =
    { Take: unit -> MonitorSnapshot
      AutoClean: MonitorSnapshot -> MonitorEvent list
      Stop: MonitorRow -> Task<MonitorEvent>
      /// Extra detail lines for the selected row: a daemon's counters and a log tail. Runs off the UI thread.
      Describe: MonitorSnapshot -> MonitorRow -> Task<string array> }

[<NoComparison; NoEquality>]
type internal MonitorModel =
    { Services: MonitorServices
      Revision: Signal<int>
      Size: Signal<struct (int * int)>
      SelectedKey: Signal<string option>
      Order: TableOrder<MonitorRow, string>
      Events: ResizeArray<MonitorEvent>
      Cancellation: CancellationTokenSource
      mutable Snapshot: MonitorSnapshot
      mutable RowCache: (MonitorRow array * string * MonitorRow array) option
      mutable Table: Table<MonitorRow> option
      mutable TableFirst: int
      mutable Width: int
      mutable Height: int
      mutable AutoClean: bool
      mutable IntervalMs: int
      mutable Paused: bool
      mutable Elapsed: float
      mutable NextSample: float
      mutable Sampling: Task<MonitorSnapshot * MonitorEvent list> option
      mutable Acting: Task<MonitorEvent list> option
      mutable Filter: string
      mutable Filtering: bool
      mutable Confirm: PendingAction option
      mutable Help: bool
      mutable Quit: bool
      mutable Dirty: bool
      mutable Detail: (string * string array) option
      mutable Describing: (string * Task<string array>) option
      mutable NextDescribe: float
      mutable Error: string }

module internal MonitorModel =

    let minIntervalMs = 250
    let maxIntervalMs = 10000

    let private severity (row: MonitorRow) =
        match row.Verdict with
        | Verdict.Cleanup _ -> 2
        | Verdict.Concern _ -> 1
        | Verdict.Healthy -> 0

    let private byThen (first: MonitorRow -> MonitorRow -> int) (second: MonitorRow -> MonitorRow -> int) a b =
        match first a b with
        | 0 -> second a b
        | c -> c

    let private memory (a: MonitorRow) (b: MonitorRow) = compare a.WorkingSet b.WorkingSet

    let private sorts: TableSort<MonitorRow> array =
        [| { Id = "state"; Compare = byThen (fun a b -> compare (severity a) (severity b)) memory; Descending = true }
           { Id = "mem"; Compare = memory; Descending = true }
           { Id = "cpu"; Compare = byThen (fun a b -> compare a.CpuPercent b.CpuPercent) memory; Descending = true }
           { Id = "kind"; Compare = byThen (fun a b -> compare a.Kind.Label b.Kind.Label) memory; Descending = false }
           { Id = "pid"; Compare = (fun (a: MonitorRow) (b: MonitorRow) -> compare a.Pid b.Pid); Descending = false } |]

    let create (services: MonitorServices) (autoClean: bool) (intervalMs: int) (struct (width, height) as size) =
        { Services = services
          Revision = createSignal 0
          Size = createSignal size
          SelectedKey = createSignal None
          Order =
            Table.sortable (fun (row: MonitorRow) -> row.Key) sorts "state"
            |> Table.withSortIndicators "↑" "↓"
          Events = ResizeArray()
          Cancellation = new CancellationTokenSource()
          Snapshot = MonitorSnapshot.Empty
          RowCache = None
          Table = None
          TableFirst = 0
          Width = width
          Height = height
          AutoClean = autoClean
          IntervalMs = Math.Clamp(intervalMs, minIntervalMs, maxIntervalMs)
          Paused = false
          Elapsed = 0.
          NextSample = 0.
          Sampling = None
          Acting = None
          Filter = ""
          Filtering = false
          Confirm = None
          Help = false
          Quit = false
          Dirty = true
          Detail = None
          Describing = None
          NextDescribe = 0.
          Error = "" }

    let log (event: MonitorEvent) (model: MonitorModel) =
        model.Events.Add event

        if model.Events.Count > 200 then
            model.Events.RemoveRange(0, model.Events.Count - 200)

        model.Dirty <- true

    let rows (model: MonitorModel) =
        match model.RowCache with
        | Some(source, filter, result) when obj.ReferenceEquals(source, model.Snapshot.Rows) && filter = model.Filter -> result
        | _ ->
            let query = model.Filter

            let result =
                if query = "" then
                    model.Snapshot.Rows
                else
                    model.Snapshot.Rows
                    |> Array.filter (fun row ->
                        let has (text: string) = text.Contains(query, StringComparison.OrdinalIgnoreCase)

                        has row.Kind.Label
                        || has (string row.Pid)
                        || has (MonitorFormat.state model.Snapshot.TakenUtc row)
                        || has (MonitorFormat.parent row.Parent)
                        || (row.Workspace |> Option.exists has)
                        || (row.CommandLine |> Option.exists has))

            model.RowCache <- Some(model.Snapshot.Rows, query, result)
            result

    let ordered (model: MonitorModel) = Table.order (rows model) model.Order

    let selected (model: MonitorModel) =
        match model.SelectedKey.Peek with
        | Some key -> rows model |> Array.tryFind (fun row -> row.Key = key)
        | None -> None

    let flagged (model: MonitorModel) =
        model.Snapshot.Rows |> Array.filter MonitorRules.isFlagged

    let setInterval (milliseconds: int) (model: MonitorModel) =
        model.IntervalMs <- Math.Clamp(milliseconds, minIntervalMs, maxIntervalMs)
        model.NextSample <- min model.NextSample (model.Elapsed + float model.IntervalMs / 1000.)
        model.Dirty <- true

    let private run (pending: PendingAction) (model: MonitorModel) =
        let targets =
            match pending with
            | PendingAction.Stop row -> [| row |]
            | PendingAction.StopFlagged rows -> rows

        let stop = model.Services.Stop

        model.Acting <-
            Some(
                Task.Run<MonitorEvent list>(
                    Func<Task<MonitorEvent list>>(fun () ->
                        task {
                            let results = ResizeArray<MonitorEvent>()

                            for row in targets do
                                try
                                    let! e = stop row
                                    results.Add e
                                with ex ->
                                    results.Add(MonitorEvent.Now($"stopping %s{row.Kind.Label} %d{row.Pid} failed: %s{ex.Message}", true))

                            return List.ofSeq results
                        })
                )
            )

    let describePending (pending: PendingAction) =
        match pending with
        | PendingAction.Stop row ->
            let target =
                match row.Workspace with
                | Some _ -> $"%s{row.Kind.Label} %d{row.Pid} (%s{MonitorFormat.workspace row})"
                | None -> $"%s{row.Kind.Label} %d{row.Pid}"

            if row.Liveness = Liveness.Running then $"Stop %s{target}?" else $"Delete the record of %s{target}?"
        | PendingAction.StopFlagged rows -> $"Stop all %d{rows.Length} flagged processes?"

    let handle (event: InputEvent) (model: MonitorModel) : bool =
        let forward event =
            match model.Table with
            | Some table -> table.Root.OnEvent event
            | None -> false

        let mutable handled = true

        match event with
        | Key key when not key.Released && (key.Modifiers = Modifiers.None || (key.Key = Key.Character && key.Modifiers = Modifiers.Shift)) ->
            match model.Confirm with
            | Some pending ->
                model.Confirm <- None

                if key.Key = Key.Character && key.Scalar <= 127 && Char.ToLowerInvariant(char key.Scalar) = 'y' then
                    run pending model
            | None when model.Filtering ->
                match key.LogicalKey with
                | LogicalKey.Ui Escape ->
                    model.Filtering <- false
                    model.Filter <- ""
                | LogicalKey.Whitespace Enter -> model.Filtering <- false
                | LogicalKey.Editing EditingKey.Backspace ->
                    if model.Filter.Length > 0 then
                        let starts = Globalization.StringInfo.ParseCombiningCharacters model.Filter
                        model.Filter <- model.Filter.Substring(0, starts[starts.Length - 1])
                | LogicalKey.Character _
                | LogicalKey.Whitespace Space when Text.Rune.IsValid key.Scalar && key.Scalar >= 32 && model.Filter.Length < 128 ->
                    model.Filter <- model.Filter + Text.Rune(key.Scalar).ToString()
                | _ -> ()
            | None when model.Help -> model.Help <- false
            | None ->
                match key.LogicalKey with
                | LogicalKey.Ui Escape -> model.Filter <- ""
                | LogicalKey.Character _
                | LogicalKey.Whitespace Space when not key.Paste && key.Scalar <= 127 ->
                    match Char.ToLowerInvariant(char key.Scalar) with
                    | 'q' -> model.Quit <- true
                    | 'k' ->
                        match selected model with
                        | Some row -> model.Confirm <- Some(PendingAction.Stop row)
                        | None -> handled <- false
                    | 'c' ->
                        match flagged model with
                        | [||] -> log (MonitorEvent.Now("nothing is flagged", false)) model
                        | rows -> model.Confirm <- Some(PendingAction.StopFlagged rows)
                    | 'a' ->
                        model.AutoClean <- not model.AutoClean
                        log (MonitorEvent.Now((if model.AutoClean then "auto-clean on" else "auto-clean off"), false)) model
                    | '/' -> model.Filtering <- true
                    | '+'
                    | '=' -> setInterval (model.IntervalMs + 250) model
                    | '-'
                    | '_' -> setInterval (model.IntervalMs - 250) model
                    | 'p'
                    | ' ' -> model.Paused <- not model.Paused
                    | 'u' -> model.NextSample <- model.Elapsed
                    | '?'
                    | 'h' -> model.Help <- true
                    | _ -> handled <- forward event
                | _ -> handled <- forward event
        | Resize(struct (width, height)) ->
            model.Width <- max 1 width
            model.Height <- max 1 height
        | _ -> handled <- false

        if handled then
            model.Dirty <- true

        handled

    let private startSample (model: MonitorModel) =
        let services = model.Services
        let autoClean = model.AutoClean

        model.Sampling <-
            Some(
                Task.Run<MonitorSnapshot * MonitorEvent list>(fun () ->
                    let first = services.Take()

                    if autoClean then
                        match services.AutoClean first with
                        | [] -> first, []
                        | events -> services.Take(), events
                    else
                        first, [])
            )

    let private startDescribe (model: MonitorModel) =
        match selected model with
        | Some row ->
            let services = model.Services
            let snapshot = model.Snapshot
            model.Describing <- Some(row.Key, Task.Run<string array>(Func<Task<string array>>(fun () -> services.Describe snapshot row)))
        | None -> model.Detail <- None

    /// Advances the clock, picks up finished background work and starts the next sample. Never blocks.
    let tick (seconds: float) (model: MonitorModel) =
        model.Elapsed <- model.Elapsed + seconds

        match model.Sampling with
        | Some work when work.IsCompleted ->
            model.Sampling <- None

            if work.IsCompletedSuccessfully then
                let snapshot, events = work.Result
                model.Snapshot <- snapshot
                model.Error <- ""

                for e in events do
                    log e model
            else
                model.Error <- work.Exception.GetBaseException().Message

            model.Dirty <- true
        | _ -> ()

        match model.Acting with
        | Some work when work.IsCompleted ->
            model.Acting <- None

            if work.IsCompletedSuccessfully then
                for e in work.Result do
                    log e model
            else
                log (MonitorEvent.Now(work.Exception.GetBaseException().Message, true)) model

            model.NextSample <- model.Elapsed
        | _ -> ()

        match model.Describing with
        | Some(key, work) when work.IsCompleted ->
            model.Describing <- None

            if work.IsCompletedSuccessfully then
                model.Detail <- Some(key, work.Result)
                model.Dirty <- true
        | _ -> ()

        if not model.Paused && model.Sampling.IsNone && model.Elapsed >= model.NextSample then
            model.NextSample <- model.Elapsed + float model.IntervalMs / 1000.
            startSample model

        let selectionMoved =
            match model.Detail, model.SelectedKey.Peek with
            | Some(key, _), Some current -> key <> current
            | None, Some _ -> true
            | _ -> false

        if model.Describing.IsNone && (selectionMoved || model.Elapsed >= model.NextDescribe) then
            model.NextDescribe <- model.Elapsed + 2.
            startDescribe model

    let close (model: MonitorModel) =
        model.Cancellation.Cancel()

        for work in [ model.Sampling |> Option.map (fun t -> t :> Task); model.Acting |> Option.map (fun t -> t :> Task) ] do
            match work with
            | Some t -> (try t.Wait 6000 |> ignore with _ -> ())
            | None -> ()

        model.Cancellation.Dispose()

module internal MonitorScreen =

    let background = Rgb 0x1F2124
    let ink = Rgb 0xD7D3C8
    let muted = Rgb 0x8A877C
    let border = Rgb 0x4F5258
    let accent = Rgb 0x7DAEA3
    let green = Rgb 0x8FB573
    let yellow = Rgb 0xE0B05A
    let red = Rgb 0xE76563
    let selection = Rgb 0x343A40

    let private size width height = [ Width(Cells(max 0 width)); Height(Cells(max 0 height)) ]
    let private position x y = Margin { Style.zero with Left = x; Top = y }
    let private bounds x y width height = position x y :: size width height

    let private styled declarations node =
        Node.setStyle declarations node
        node

    let private watch (model: MonitorModel) read () =
        model.Revision.Value |> ignore
        read ()

    let private field x y width color read =
        text read |> styled (Foreground color :: bounds x y width 1)

    let verdictColor (row: MonitorRow) =
        match row.Verdict with
        | Verdict.Cleanup _ -> red
        | Verdict.Concern _ -> yellow
        | Verdict.Healthy -> if row.CpuPercent >= 1. then green else ink

    let header (model: MonitorModel) =
        let running, memoryTotal, cpuTotal, flagged, cleanable = MonitorFormat.summary model.Snapshot

        let mode =
            [ (if model.AutoClean then "auto-clean on" else "auto-clean OFF")
              sprintf "every %.2gs" (float model.IntervalMs / 1000.)
              sprintf "sample %.0f ms" model.Snapshot.SampleMs
              if model.Paused then "PAUSED" ]
            |> String.concat " · "

        sprintf
            " rfs monitor  %d running · %s · CPU %.1f%% · %d flagged · %d to clean   │ %s"
            running
            (MonitorFormat.bytes memoryTotal)
            cpuTotal
            flagged
            cleanable
            mode

    let footer (model: MonitorModel) =
        match model.Confirm with
        | Some pending -> " " + MonitorModel.describePending pending + "  y confirms · any other key cancels"
        | None when model.Filtering -> $" filter: %s{model.Filter}▌   Enter keeps it · Esc clears it"
        | None when model.Error <> "" -> " sampling failed: " + model.Error
        | None -> " ↑↓ select  k stop  c stop flagged  a auto-clean  / filter  s sort  r reverse  +/- interval  p pause  ? help  q quit"

    let private footerColor (model: MonitorModel) =
        if model.Confirm.IsSome then yellow
        elif model.Error <> "" && not model.Filtering then red
        else muted

    let detailLines (model: MonitorModel) : (Color * string) array =
        match MonitorModel.selected model with
        | None -> [| muted, "Select a process with ↑↓." |]
        | Some row ->
            let now = model.Snapshot.TakenUtc

            let idle =
                match row.Kind, row.IdleTimeoutSeconds with
                | MonitorKind.Daemon, Some ttl -> $"idle %s{MonitorFormat.idle now row} of %s{MonitorFormat.duration (TimeSpan.FromSeconds(float ttl))}"
                | _ -> $"idle %s{MonitorFormat.idle now row}"

            let build =
                match row.BinaryStamp with
                | Some stamp when stamp = model.Snapshot.CurrentStamp -> "this rfs build"
                | Some _ -> "another rfs build"
                | None -> if row.Registered then "" else "unregistered"

            let identity =
                if row.Liveness = Liveness.Running then
                    $"%s{row.Kind.Label} %d{row.Pid} · %s{row.ProcessName} · up %s{MonitorFormat.uptime now row} · %s{idle}"
                else
                    let record = row.RecordPath |> Option.orElse row.LeasePath |> Option.defaultValue "?"
                    $"%s{row.Kind.Label} %d{row.Pid} · not running · record %s{record}"

            let basics =
                [| ink, identity
                   verdictColor row, MonitorFormat.state now row
                   muted, MonitorFormat.explain row
                   ink, "workspace " + (row.Workspace |> Option.defaultValue "—")
                   ink,
                   $"parent %s{MonitorFormat.parent row.Parent}"
                   + (if build = "" then "" else " · " + build)
                   if row.Liveness = Liveness.Running then
                       ink,
                       $"cpu %.1f{row.CpuPercent}%% · memory %s{MonitorFormat.bytes row.WorkingSet} (private %s{MonitorFormat.bytes row.PrivateBytes}) · threads %d{row.Threads} · i/o %s{MonitorFormat.rate row.IoRate}"
                   match row.CommandLine with
                   | Some line -> muted, "cmd " + line
                   | None -> () |]

            let extra =
                match model.Detail with
                | Some(key, lines) when key = row.Key -> lines |> Array.map (fun line -> muted, line)
                | _ -> [||]

            Array.append basics extra

    let private eventLines (model: MonitorModel) count =
        let events = model.Events
        let start = max 0 (events.Count - count)

        [| for i in start .. events.Count - 1 do
               let e = events[i]
               (if e.Failed then red else ink), e.TimeUtc.ToLocalTime().ToString("HH:mm:ss") + " " + e.Text |]

    let helpLines =
        [| "rfs monitor watches every rfs daemon, rfs mcp server and ranvierautocomplete process."
           ""
           "Auto-clean (a toggles it) acts only on certain findings, checking each again first:"
           "  · a record or lease whose process exited, or whose PID another process now holds"
           "  · an rfs mcp or ranvierautocomplete whose client exited: nothing can reach it again"
           "  · a temp file left by an interrupted record write"
           "Flagged (yellow) is for you to judge: an older rfs build, a daemon past its idle"
           "timeout, a record not refreshed, a server re-parented to init. k stops one, c all."
           ""
           "Watching costs nothing: the daemon does not count the monitor's requests as use."
           ""
           "↑↓ PgUp PgDn Home End select · s sort · r reverse · click a heading to sort"
           "k stop the selected process (a daemon gets shutdown, then a kill after 5 s)"
           "c stop every flagged process · / filter · +/- interval · p pause · u sample now"
           "Any key closes this help. q quits." |]

    let private columns (inner: int) : TableColumn<MonitorRow> array * string option array =
        let color (read: MonitorRow -> Color) _ row = [ Foreground(read row); Padding { Style.zero with Right = 1 } ]
        let plain = color (fun _ -> ink)
        let dim = color (fun _ -> muted)

        let col sort header width text style : TableColumn<MonitorRow> * string option =
            { Header = header; Width = Cells(max 1 width); Text = text; Style = style }, sort

        let wide = inner >= 106
        let medium = inner >= 84
        let fixedWidth = if wide then 94 elif medium then 78 else 52
        let rest = max 4 (inner - fixedWidth)
        let now () = DateTime.UtcNow

        [| col (Some "kind") "KIND" 7 (fun r -> r.Kind.Label) plain
           col (Some "pid") "PID" 7 (fun r -> string r.Pid) dim
           if medium then col None "PARENT" 20 (fun r -> MonitorFormat.parentShort r.Parent) dim
           col (Some "cpu") "CPU%" 6 MonitorFormat.cpu plain
           col (Some "mem") "MEM" 7 MonitorFormat.memory plain
           if wide then col None "I/O" 9 (fun r -> MonitorFormat.rate r.IoRate) dim
           if medium then col None "IDLE" 7 (fun r -> MonitorFormat.idle (now ()) r) dim
           if wide then col None "UP" 7 (fun r -> MonitorFormat.uptime (now ()) r) dim
           col (Some "state") "STATE" 25 (fun r -> MonitorFormat.state (now ()) r) (color verdictColor)
           col None "WORKSPACE" rest MonitorFormat.workspace dim |]
        |> Array.unzip

    let private processes (model: MonitorModel) x y w h =
        let inner = w - 2
        let visible = max 1 (h - 3)
        let columns, headerIds = columns inner

        let rail =
            Css.tableScrollbar
                { Paint.empty with Foreground = border; Background = background }
                { Paint.empty with Foreground = accent; Background = background }

        let tableBounds overflow = bounds 1 1 (if overflow then w - 1 else w - 2) (h - 2) @ rail

        let readRows () =
            model.Revision.Value |> ignore
            MonitorModel.rows model

        let table =
            Table.bindSortableFrom model.TableFirst columns headerIds [ Foreground accent ] visible readRows model.SelectedKey model.Order (fun navigate ->
                Table.create (tableBounds true) [ Foreground muted ] [ Background selection ] [] (fun selected first ->
                    navigate selected first
                    model.TableFirst <- first
                    model.Dirty <- true))

        table.Root.OnEvent <-
            fun event ->
                match event with
                | Key _ when model.Filtering || model.Confirm.IsSome || model.Help -> false
                | _ -> Table.handle event table

        model.Table <- Some table
        let mutable overlaps = true

        createEffect (fun () ->
            let overflow = (readRows ()).Length > visible

            if overflow <> overlaps then
                overlaps <- overflow
                Node.setStyle (tableBounds overflow) table.Root)

        let caption () =
            let count = (MonitorModel.rows model).Length
            let total = model.Snapshot.Rows.Length
            if model.Filter = "" then $"┤processes %d{total}├" else $"┤processes %d{count} of %d{total} · filter %s{model.Filter}├"

        let sortText () =
            let arrow = if model.Order.Descending then "↓" else "↑"
            $"┤sort %s{model.Order.SortId} %s{arrow}├"

        overlay (bounds x y w h) {
            Styled.panel [] w h border [ 2, accent, watch model caption; max 3 (w - 18), muted, watch model sortText ] [] (fragment [||])
            table.Root

            label "No rfs or ranvierautocomplete processes are running."
            |> bindStyle (watch model (fun () -> Visible((MonitorModel.rows model).Length = 0) :: Foreground muted :: bounds 3 3 (max 1 (w - 6)) 1))
        }

    /// `count` one-cell rows at column `x` of the parent, each bound to a line of `read ()`.
    let private lines (model: MonitorModel) (read: unit -> (Color * string) array) x width count =
        overlay (position x 0 :: size width count) {
            for i in 0 .. count - 1 do
                let line () =
                    let all = read ()
                    if i < all.Length then snd all[i] else ""

                text (watch model line)
                |> bindStyle (watch model (fun () ->
                    let all = read ()
                    let color = if i < all.Length then fst all[i] else ink
                    Foreground color :: bounds 0 i width 1))
        }

    let build (model: MonitorModel) =
        let w, h = model.Width, model.Height

        overlay (Background background :: Foreground ink :: size w h) {
            if w < 60 || h < 14 then
                model.Table <- None
                field 1 1 (w - 2) ink (fun () -> "rfs monitor")
                field 1 3 (w - 2) yellow (fun () -> "Enlarge the terminal to at least 60 × 14.")
                field 1 5 (w - 2) muted (watch model (fun () -> header model))
            else
                let bottom = Math.Clamp(h / 3, 7, 12)
                let tableHeight = h - 2 - bottom
                let detailWidth = w * 60 / 100

                field 0 0 w ink (watch model (fun () -> header model))
                |> bindStyle (watch model (fun () -> Background selection :: Foreground ink :: bounds 0 0 w 1))

                processes model 0 1 w tableHeight

                overlay (bounds 0 (1 + tableHeight) detailWidth bottom) {
                    Styled.panel [] detailWidth bottom border [ 2, accent, (fun () -> "┤details├") ] [] (
                        lines model (fun () -> detailLines model) 1 (detailWidth - 4) (bottom - 2)
                    )
                }

                overlay (bounds detailWidth (1 + tableHeight) (w - detailWidth) bottom) {
                    Styled.panel [] (w - detailWidth) bottom border [ 2, accent, (fun () -> "┤events├") ] [] (
                        lines model (fun () -> eventLines model (bottom - 2)) 1 (w - detailWidth - 4) (bottom - 2)
                    )
                }

                field 0 (h - 1) w muted (watch model (fun () -> footer model))
                |> bindStyle (watch model (fun () -> Foreground(footerColor model) :: bounds 0 (h - 1) w 1))

                let helpWidth = min 92 (w - 4)
                let helpHeight = min (helpLines.Length + 2) (h - 2)

                overlay (bounds ((w - helpWidth) / 2) ((h - helpHeight) / 2) helpWidth helpHeight) {
                    Styled.panel [ Background background ] helpWidth helpHeight accent [ 2, accent, (fun () -> "┤help├") ] [] (
                        lines model (fun () -> helpLines |> Array.map (fun line -> ink, line)) 1 (helpWidth - 4) (helpHeight - 2)
                    )
                }
                |> bindStyle (watch model (fun () -> Visible model.Help :: Background background :: bounds ((w - helpWidth) / 2) ((h - helpHeight) / 2) helpWidth helpHeight))
        }

module internal MonitorApp =

    let private publish (model: MonitorModel) =
        let size = struct (model.Width, model.Height)

        if model.Dirty || model.Size.Peek <> size then
            model.Dirty <- false
            model.Size.Value <- size
            model.Revision.Value <- model.Revision.Peek + 1

    /// The root node. The layout is rebuilt when the terminal size changes; everything else updates in place.
    let root (model: MonitorModel) =
        let content = each (fun () -> [| model.Size.Value |]) (fun _ -> MonitorScreen.build model)
        Node.setStyle [ Width(Flex 1); Height(Flex 1) ] content
        let root = column [ Width(Flex 1); Height(Flex 1); Background MonitorScreen.background ] { content }

        root.OnEvent <-
            fun event ->
                let handled = MonitorModel.handle event model

                if handled then
                    publish model

                handled

        root

    let update (model: MonitorModel) = publish model

    let describeAsync (snapshot: MonitorSnapshot) (row: MonitorRow) : Task<string array> =
        task {
            let lines = ResizeArray<string>()

            match row.Kind, row.PipeName, row.Liveness with
            | MonitorKind.Daemon, Some pipe, Liveness.Running when row.BinaryStamp = Some snapshot.CurrentStamp ->
                match! MonitorActions.daemonInfoAsync pipe 1500 with
                | Ok(data, ms) ->
                    let num (name: string) =
                        match data[name] with
                        | null -> "?"
                        | node -> node.ToJsonString()

                    if JsonUtil.boolOpt data "busy" = Some true then
                        lines.Add(sprintf "daemon answered in %.0f ms; a request holds its engine" ms)
                    elif JsonUtil.boolOpt data "loaded" = Some false then
                        lines.Add(sprintf "daemon answered in %.0f ms; workspace not loaded yet" ms)
                    else
                        lines.Add(
                            sprintf
                                "daemon answered in %.0f ms · %s projects (%s failed) · %s files · cold load %s ms"
                                ms
                                (num "loadedProjects")
                                (num "failedProjects")
                                (num "sourceFiles")
                                (num "coldLoadMs")
                        )
                | Error message -> lines.Add("daemon did not answer: " + message)
            | MonitorKind.Daemon, Some _, Liveness.Running ->
                lines.Add "counters need the daemon to run this rfs build (other builds would count the request as use)"
            | _ -> ()

            match row.LogPath with
            | Some path ->
                lines.Add("log " + path)

                for line in ProcessProbe.tailLines path 6 do
                    lines.Add("  " + line)
            | None -> ()

            return lines.ToArray()
        }

    let services (sampler: MonitorSampler) =
        { Take = sampler.Take
          AutoClean = MonitorActions.autoClean
          Stop = MonitorActions.stopAsync
          Describe = describeAsync }

    let runInteractive (autoClean: bool) (intervalMs: int) (smokeSeconds: float option) : int =
        let sampler = MonitorSampler()
        use stop = new CancellationTokenSource()

        Terminal.runApp Terminal.defaults stop.Token (fun viewport ->
            let model = MonitorModel.create (services sampler) autoClean intervalMs (viewport ())
            onCleanup (fun () -> MonitorModel.close model)
            publish model

            let tick (delta: TimeSpan) =
                MonitorModel.tick delta.TotalSeconds model
                publish model

                if model.Quit || (smokeSeconds |> Option.exists (fun s -> model.Elapsed >= s)) then
                    stop.Cancel()

                Some(TimeSpan.FromMilliseconds 100.)

            root model, tick)

        0

    /// `--once`: two samples half a second apart, so CPU and I/O rates have a baseline; `--clean` applies auto-clean.
    let runOnce (clean: bool) (asJson: bool) : int =
        let sampler = MonitorSampler()
        sampler.Take() |> ignore
        Thread.Sleep 500
        let first = sampler.Take()

        let snapshot, events =
            if clean then
                match MonitorActions.autoClean first with
                | [] -> first, []
                | events -> sampler.Take(), events
            else
                first, []

        if asJson then
            Console.Out.WriteLine((MonitorFormat.json snapshot events).ToJsonString(Text.Json.JsonSerializerOptions(WriteIndented = true)))
        else
            for e in events do
                Console.Out.WriteLine(MonitorFormat.ascii ((if e.Failed then "failed: " else "cleaned: ") + e.Text))

            Console.Out.WriteLine(MonitorFormat.table snapshot)

        0

    /// The rows of a rendered frame as text, wide glyphs counted once.
    let frameLines (frame: Frame) =
        [| for y in 0 .. frame.Height - 1 ->
               let sb = Text.StringBuilder()

               for x in 0 .. frame.Width - 1 do
                   let cell = frame.Cells[y * frame.Width + x]

                   if cell.Width > 0uy then
                       sb.Append(if String.IsNullOrEmpty cell.Glyph then " " else cell.Glyph) |> ignore

               sb.ToString().TrimEnd() |]

    /// `--render=WxH`: one frame of live data drawn as text, with auto-clean off, so the layout can be checked
    /// without a terminal. Written as UTF-8 bytes, because the frame uses box-drawing glyphs.
    let runRender (width: int) (height: int) : int =
        use graph = new Graph()
        let sampler = MonitorSampler()
        let model = graph.Run(fun () -> MonitorModel.create (services sampler) false 1000 (struct (width, height)))
        let view = View.mount graph (fun () -> root model)

        try
            let settle () =
                for _ in 1..4 do
                    graph.Run(fun () ->
                        MonitorModel.tick 0.01 model
                        publish model)

                    for work in
                        [ model.Sampling |> Option.map (fun t -> t :> Task)
                          model.Describing |> Option.map (fun (_, t) -> t :> Task) ] do
                        work |> Option.iter (fun t -> t.Wait 5000 |> ignore)

            settle ()
            Thread.Sleep 500
            graph.Run(fun () -> model.NextSample <- model.Elapsed)
            settle ()
            let frame = Frame.create width height
            Layout.render view frame
            let bytes = Text.Encoding.UTF8.GetBytes(String.Join("\n", frameLines frame) + "\n")
            use stdout = Console.OpenStandardOutput()
            stdout.Write(bytes, 0, bytes.Length)
            0
        finally
            View.dispose view
            MonitorModel.close model

    let usage =
        "rfs tui | rfs monitor | rfs top [--interval-ms <250-10000>] [--no-auto-clean] [--once [--json] [--clean]] [--render=<W>x<H>]"

    let run (args: string list) : int =
        let mutable interval = 1000
        let mutable autoClean = true
        let mutable once = false
        let mutable asJson = false
        let mutable clean = false
        let mutable smoke: float option = None
        let mutable renderSize: (int * int) option = None
        let mutable error: string option = None
        let mutable cur = args

        let parseInterval (v: string) =
            match Int32.TryParse v with
            | true, ms when ms >= MonitorModel.minIntervalMs && ms <= MonitorModel.maxIntervalMs -> interval <- ms
            | _ -> error <- Some $"--interval-ms takes %d{MonitorModel.minIntervalMs} to %d{MonitorModel.maxIntervalMs}, got '%s{v}'."

        while not cur.IsEmpty do
            match cur with
            | "--interval-ms" :: v :: rest ->
                parseInterval v
                cur <- rest
            | (arg: string) :: rest when arg.StartsWith("--interval-ms=", StringComparison.Ordinal) ->
                parseInterval (arg.Substring "--interval-ms=".Length)
                cur <- rest
            | "--no-auto-clean" :: rest ->
                autoClean <- false
                cur <- rest
            | "--once" :: rest ->
                once <- true
                cur <- rest
            | "--json" :: rest ->
                asJson <- true
                once <- true
                cur <- rest
            | "--clean" :: rest ->
                clean <- true
                cur <- rest
            | (arg: string) :: rest when arg.StartsWith("--smoke=", StringComparison.Ordinal) ->
                match Double.TryParse(arg.Substring "--smoke=".Length, Globalization.CultureInfo.InvariantCulture) with
                | true, s when s > 0. -> smoke <- Some s
                | _ -> error <- Some "--smoke takes a number of seconds."

                cur <- rest
            | (arg: string) :: rest when arg.StartsWith("--render=", StringComparison.Ordinal) ->
                match arg.Substring("--render=".Length).Split('x') with
                | [| w; h |] ->
                    match Int32.TryParse w, Int32.TryParse h with
                    | (true, width), (true, height) when width >= 1 && height >= 1 && width <= 1000 && height <= 1000 ->
                        renderSize <- Some(width, height)
                    | _ -> error <- Some "--render takes <width>x<height>, each 1 to 1000."
                | _ -> error <- Some "--render takes <width>x<height>, each 1 to 1000."

                cur <- rest
            | other :: rest ->
                error <- Some $"Unknown option '%s{other}'."
                cur <- rest
            | [] -> ()

        match error with
        | Some message ->
            Console.Error.WriteLine message
            Console.Error.WriteLine("Usage: " + usage)
            2
        | None when clean && not once ->
            Console.Error.WriteLine "--clean applies to --once; the interactive monitor cleans by itself unless --no-auto-clean is given."
            2
        | None when renderSize.IsSome -> runRender (fst renderSize.Value) (snd renderSize.Value)
        | None when once -> runOnce clean asJson
        | None when Console.IsInputRedirected || Console.IsOutputRedirected ->
            Console.Error.WriteLine "rfs monitor needs an interactive terminal; printing one snapshot instead (--once)."
            runOnce false asJson
        | None -> runInteractive autoClean interval smoke
