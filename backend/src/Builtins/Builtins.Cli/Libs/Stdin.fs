/// Standard libraries for reading data from the user via the CLI
module Builtins.Cli.Libs.Stdin

open System

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Effects
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module HE = LibExecution.HostEvents
module Scheduler = LibExecution.Scheduler

open Builtin.Shortcuts

/// Terminal resize, so a full-screen view can repaint without waiting for a keystroke.
///
/// The render loop samples the terminal size at the top of each pass, but the pass blocks on a keypress, so
/// resizing while idle left a stale frame until you pressed something. SIGWINCH sets a flag; `readKeyOrPaste`
/// polls it and reports a keypress nobody handles, which is enough to send the loop round again and repaint
/// at the new size.
module private Resize =
  let mutable private pending = false

  let private registration =
    lazy
      (try
        Some(
          System.Runtime.InteropServices.PosixSignalRegistration.Create(
            System.Runtime.InteropServices.PosixSignal.SIGWINCH,
            fun ctx ->
              // Don't cancel: SIGWINCH has no default action worth suppressing, and cancelling it on a
              // platform that reuses the handler for something else would be rude.
              pending <- true
          )
        )
       with _ ->
         // Windows, or a runtime without POSIX signals. Resize just keeps its old behaviour there.
         None)

  /// Start listening. Idempotent, and safe to call from the read path.
  let arm () : unit =
    registration.Force()
    |> ignore<System.Runtime.InteropServices.PosixSignalRegistration option>

  /// Consume a pending resize, if there is one.
  let takePending () : bool =
    if pending then
      pending <- false
      true
    else
      false


/// How long a gap ends a burst, and how long a burst may run in total.
///
/// A wheel spin sends events a few ms apart, so 4ms of quiet is comfortably a pause without ever waiting on a
/// human. The budget is the backstop. These are two clocks on purpose - one restarted per key, one never
/// restarted: sharing one and restarting it makes the budget unreachable, because a continuous producer keeps
/// resetting the very clock the budget is measured on.
let private quietMs = 4.0
let private burstBudgetMs = 40.0

/// A key read during a burst that turned out not to belong to it, held for the next call.
///
/// Coalescing counts a run of ONE key. A different key mid-burst ends the run and has to go somewhere:
/// dropping it loses real keystrokes (`Down Down Enter` would eat the Enter), and returning it instead of the
/// arrow would report one key's identity with another key's count. One slot is enough, because we stop
/// draining the moment we stash one.
let mutable private pushedBack : ConsoleKeyInfo option = None

/// Reads the next keypress, the whole text of a paste when one is detected, and how many times the key
/// repeated within a coalesced burst (1 for an ordinary keypress).
///
/// Two things queue a flood of input: a paste (a run of printable characters) and a wheel scroll or held-down
/// key (a run of the same control key - in the alternate screen a wheel arrives as arrow escapes). Both drain
/// in one go so the caller renders once, but they come back differently: a paste as text to insert, a run as
/// one key plus its count.
///
/// Which it was is decided AFTER draining, on whether any insertable text came out - not on the first key.
/// Deciding up front destroyed any paste beginning with a newline or tab, which is what you get selecting
/// from the end of a line.
let private readKeyOrPaste () : ConsoleKeyInfo * string option * int =
  // What a single key contributes to pasted text (newlines/tabs preserved).
  let pasteText (k : ConsoleKeyInfo) : string option =
    match k.Key with
    | ConsoleKey.Enter -> Some "\n"
    | ConsoleKey.Tab -> Some "\t"
    | _ ->
      let c = k.KeyChar
      if c = '\u0000' || Char.IsControl c then None else Some(string c)

  // An ordinary printable character. A paste reports one of these as its key so
  // the Dark side inserts it instead of acting on Enter/Tab/etc.
  let isPrintable (k : ConsoleKeyInfo) =
    k.KeyChar <> '\u0000' && not (Char.IsControl k.KeyChar)

  // Redirected stdin (pipe / file / `< /dev/null` / daemon) isn't a TTY, so `Console.ReadKey` throws
  // InvalidOperation_ConsoleReadKeyOnFile mid-loop and crashes a full-screen view with a raw stack trace.
  // Report Escape (every view treats it as "quit") so it exits cleanly instead.
  if Console.IsInputRedirected then
    (ConsoleKeyInfo('\u001b', ConsoleKey.Escape, false, false, false), None, 1)
  else

    Resize.arm ()

    // A key stashed by the previous call's burst comes first, before we look at the terminal again.
    match pushedBack with
    | Some k ->
      pushedBack <- None
      (k, None, 1)
    | None ->

      // Poll rather than blocking outright, so a resize can wake the loop. The interval is short enough to feel
      // instant on a drag-resize and long enough to cost nothing while idle.
      let mutable resized = false
      while not Console.KeyAvailable && not resized do
        if Resize.takePending () then resized <- true else Threading.Thread.Sleep 15

      if resized then
        // A key no view acts on: the loop goes round, re-samples the terminal, and repaints.
        (ConsoleKeyInfo('\u0000', ConsoleKey.NoName, false, false, false), None, 1)
      else

        let first = Console.ReadKey true
        if not Console.KeyAvailable then
          (first, None, 1)
        else
          let sb = System.Text.StringBuilder()
          let append (text : string) : unit =
            sb.Append text |> ignore<System.Text.StringBuilder>
          let burstStart = Diagnostics.Stopwatch.StartNew()
          let sinceLastKey = Diagnostics.Stopwatch.StartNew()
          let sameAsFirst (k : ConsoleKeyInfo) =
            k.Key = first.Key && k.Modifiers = first.Modifiers
          let firstIsPrintable = isPrintable first
          let mutable repeat = 1
          let mutable printableKey = if firstIsPrintable then Some first else None
          pasteText first |> Option.iter append
          let mutable draining = true
          while draining do
            if Console.KeyAvailable then
              let k = Console.ReadKey true
              if sameAsFirst k then
                repeat <- repeat + 1
                pasteText k |> Option.iter append
                if isPrintable k then printableKey <- Some k
                sinceLastKey.Restart()
              elif firstIsPrintable then
                // Mid-paste: a differing key is just the next character, not the end of a run.
                pasteText k |> Option.iter append
                if isPrintable k then printableKey <- Some k
                sinceLastKey.Restart()
              else
                // A different key ends a control-key run. Hold it for the next call rather than dropping it.
                pushedBack <- Some k
                draining <- false
            elif
              sinceLastKey.Elapsed.TotalMilliseconds < quietMs
              && burstStart.Elapsed.TotalMilliseconds < burstBudgetMs
            then
              Threading.Thread.Sleep 1
            else
              draining <- false
          let pasted = sb.ToString()
          if pasted = "" then
            // Nothing insertable came out, so it was a run of control keys: a wheel, or a held-down key.
            (first, None, repeat)
          else
            (Option.defaultValue first printableKey, Some pasted, 1)

/// The Dark shape of one key: `Stdlib.Cli.Stdin.KeyRead`.
let keyReadToDval
  (readKey : ConsoleKeyInfo)
  (pasteText : string option)
  (repeat : int)
  : Dval =
  let altHeld = (readKey.Modifiers &&& ConsoleModifiers.Alt) <> ConsoleModifiers.None
  let shiftHeld =
    (readKey.Modifiers &&& ConsoleModifiers.Shift) <> ConsoleModifiers.None
  let ctrlHeld =
    (readKey.Modifiers &&& ConsoleModifiers.Control) <> ConsoleModifiers.None

  let modifiers =
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.modifiers ())
    let fields =
      [ "alt", DBool altHeld; "shift", DBool shiftHeld; "ctrl", DBool ctrlHeld ]
    DRecord(typeName, typeName, [], Map fields)

  let keyCaseName =
    match readKey.Key with
    | ConsoleKey.Backspace -> "Backspace"
    | ConsoleKey.Tab -> "Tab"
    | ConsoleKey.Clear -> "Clear"
    | ConsoleKey.Enter -> "Enter"
    | ConsoleKey.Pause -> "Pause"
    | ConsoleKey.Escape -> "Escape"
    | ConsoleKey.Spacebar -> "Spacebar"
    | ConsoleKey.PageUp -> "PageUp"
    | ConsoleKey.PageDown -> "PageDown"
    | ConsoleKey.End -> "End"
    | ConsoleKey.Home -> "Home"
    | ConsoleKey.LeftArrow -> "LeftArrow"
    | ConsoleKey.UpArrow -> "UpArrow"
    | ConsoleKey.RightArrow -> "RightArrow"
    | ConsoleKey.DownArrow -> "DownArrow"
    | ConsoleKey.Select -> "Select"
    | ConsoleKey.Print -> "Print"
    | ConsoleKey.Execute -> "Execute"
    | ConsoleKey.PrintScreen -> "PrintScreen"
    | ConsoleKey.Insert -> "Insert"
    | ConsoleKey.Delete -> "Delete"
    | ConsoleKey.Help -> "Help"
    | ConsoleKey.D0 -> "D0"
    | ConsoleKey.D1 -> "D1"
    | ConsoleKey.D2 -> "D2"
    | ConsoleKey.D3 -> "D3"
    | ConsoleKey.D4 -> "D4"
    | ConsoleKey.D5 -> "D5"
    | ConsoleKey.D6 -> "D6"
    | ConsoleKey.D7 -> "D7"
    | ConsoleKey.D8 -> "D8"
    | ConsoleKey.D9 -> "D9"
    | ConsoleKey.A -> "A"
    | ConsoleKey.B -> "B"
    | ConsoleKey.C -> "C"
    | ConsoleKey.D -> "D"
    | ConsoleKey.E -> "E"
    | ConsoleKey.F -> "F"
    | ConsoleKey.G -> "G"
    | ConsoleKey.H -> "H"
    | ConsoleKey.I -> "I"
    | ConsoleKey.J -> "J"
    | ConsoleKey.K -> "K"
    | ConsoleKey.L -> "L"
    | ConsoleKey.M -> "M"
    | ConsoleKey.N -> "N"
    | ConsoleKey.O -> "O"
    | ConsoleKey.P -> "P"
    | ConsoleKey.Q -> "Q"
    | ConsoleKey.R -> "R"
    | ConsoleKey.S -> "S"
    | ConsoleKey.T -> "T"
    | ConsoleKey.U -> "U"
    | ConsoleKey.V -> "V"
    | ConsoleKey.W -> "W"
    | ConsoleKey.X -> "X"
    | ConsoleKey.Y -> "Y"
    | ConsoleKey.Z -> "Z"
    | ConsoleKey.LeftWindows -> "LeftWindows"
    | ConsoleKey.RightWindows -> "RightWindows"
    | ConsoleKey.Applications -> "Applications"
    | ConsoleKey.Sleep -> "Sleep"
    | ConsoleKey.NumPad0 -> "NumPad0"
    | ConsoleKey.NumPad1 -> "NumPad1"
    | ConsoleKey.NumPad2 -> "NumPad2"
    | ConsoleKey.NumPad3 -> "NumPad3"
    | ConsoleKey.NumPad4 -> "NumPad4"
    | ConsoleKey.NumPad5 -> "NumPad5"
    | ConsoleKey.NumPad6 -> "NumPad6"
    | ConsoleKey.NumPad7 -> "NumPad7"
    | ConsoleKey.NumPad8 -> "NumPad8"
    | ConsoleKey.NumPad9 -> "NumPad9"
    | ConsoleKey.Multiply -> "Multiply"
    | ConsoleKey.Add -> "Add"
    | ConsoleKey.Separator -> "Separator"
    | ConsoleKey.Subtract -> "Subtract"
    | ConsoleKey.Decimal -> "Decimal"
    | ConsoleKey.Divide -> "Divide"
    | ConsoleKey.F1 -> "F1"
    | ConsoleKey.F2 -> "F2"
    | ConsoleKey.F3 -> "F3"
    | ConsoleKey.F4 -> "F4"
    | ConsoleKey.F5 -> "F5"
    | ConsoleKey.F6 -> "F6"
    | ConsoleKey.F7 -> "F7"
    | ConsoleKey.F8 -> "F8"
    | ConsoleKey.F9 -> "F9"
    | ConsoleKey.F10 -> "F10"
    | ConsoleKey.F11 -> "F11"
    | ConsoleKey.F12 -> "F12"
    | ConsoleKey.F13 -> "F13"
    | ConsoleKey.F14 -> "F14"
    | ConsoleKey.F15 -> "F15"
    | ConsoleKey.F16 -> "F16"
    | ConsoleKey.F17 -> "F17"
    | ConsoleKey.F18 -> "F18"
    | ConsoleKey.F19 -> "F19"
    | ConsoleKey.F20 -> "F20"
    | ConsoleKey.F21 -> "F21"
    | ConsoleKey.F22 -> "F22"
    | ConsoleKey.F23 -> "F23"
    | ConsoleKey.F24 -> "F24"
    | ConsoleKey.BrowserBack -> "BrowserBack"
    | ConsoleKey.BrowserForward -> "BrowserForward"
    | ConsoleKey.BrowserRefresh -> "BrowserRefresh"
    | ConsoleKey.BrowserStop -> "BrowserStop"
    | ConsoleKey.BrowserSearch -> "BrowserSearch"
    | ConsoleKey.BrowserFavorites -> "BrowserFavorites"
    | ConsoleKey.BrowserHome -> "BrowserHome"
    | ConsoleKey.VolumeMute -> "VolumeMute"
    | ConsoleKey.VolumeDown -> "VolumeDown"
    | ConsoleKey.VolumeUp -> "VolumeUp"
    | ConsoleKey.MediaNext -> "MediaNext"
    | ConsoleKey.MediaPrevious -> "MediaPrevious"
    | ConsoleKey.MediaStop -> "MediaStop"
    | ConsoleKey.MediaPlay -> "MediaPlay"
    | ConsoleKey.LaunchMail -> "LaunchMail"
    | ConsoleKey.LaunchMediaSelect -> "LaunchMediaSelect"
    | ConsoleKey.LaunchApp1 -> "LaunchApp1"
    | ConsoleKey.LaunchApp2 -> "LaunchApp2"
    | ConsoleKey.Oem1 -> "Oem1"
    | ConsoleKey.OemPlus -> "OemPlus"
    | ConsoleKey.OemComma -> "OemComma"
    | ConsoleKey.OemMinus -> "OemMinus"
    | ConsoleKey.OemPeriod -> "OemPeriod"
    | ConsoleKey.Oem2 -> "Oem2"
    | ConsoleKey.Oem3 -> "Oem3"
    | ConsoleKey.Oem4 -> "Oem4"
    | ConsoleKey.Oem5 -> "Oem5"
    | ConsoleKey.Oem6 -> "Oem6"
    | ConsoleKey.Oem7 -> "Oem7"
    | ConsoleKey.Oem8 -> "Oem8"
    | ConsoleKey.Oem102 -> "Oem102"
    | ConsoleKey.Process -> "Process"
    | ConsoleKey.Packet -> "Packet"
    | ConsoleKey.Attention -> "Attention"
    | ConsoleKey.CrSel -> "CrSel"
    | ConsoleKey.ExSel -> "ExSel"
    | ConsoleKey.EraseEndOfFile -> "EraseEndOfFile"
    | ConsoleKey.Play -> "Play"
    | ConsoleKey.Zoom -> "Zoom"
    | ConsoleKey.NoName -> "NoName"
    | ConsoleKey.Pa1 -> "Pa1"
    | ConsoleKey.OemClear -> "OemClear"
    | ConsoleKey.None -> "None"
    // CLEANUP tidy
    | _ -> "None"

  let key =
    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.key ())
    DEnum(typeName, typeName, [], keyCaseName, [])

  // Get character representation based on keyboard layout.
  // For a paste, report the whole pasted run so it's inserted in one go;
  // otherwise only include keyChar for printable characters.
  let keyChar =
    match pasteText with
    | Some text -> DString text
    | None ->
      let ch = readKey.KeyChar
      if System.Char.IsControl(ch) || ch = '\u0000' then
        DString "" // Empty string for control/special keys
      else
        ch |> string |> DString

  let keyRead =
    let typeName =
      FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.keyRead ())
    DRecord(
      typeName,
      typeName,
      [],
      Map
        [ "key", key
          "modifiers", modifiers
          "keyChar", keyChar
          "repeat", Dval.int (bigint repeat) ]
    )

  keyRead


/// Block for one key; Ctrl+C is input while we read.
let private readOneKey () : Dval =
  Console.TreatControlCAsInput <- true
  let readKey, pasteText, repeat = readKeyOrPaste ()
  Console.TreatControlCAsInput <- false
  keyReadToDval readKey pasteText repeat


/// The reader thread's source: installed on the first subscription that wants a key, so a
/// process that never waits on one never starts the thread. Only for a terminal; redirected
/// stdin keeps answering Escape synchronously (see `readKeyOrPaste`).
let private installKeySource () : unit =
  if not Console.IsInputRedirected && HE.sources.readKey.IsNone then
    HE.sources.readKey <- Some readOneKey


/// Read exactly `byteLength` BYTES of input from `reader`, as text.
///
/// The LSP frames each message with a `Content-Length` in bytes, and the header
/// before it is read a line at a time through the same buffered `Console.In`. So
/// the body has to come through that reader too (a raw-stream read would miss
/// whatever it already buffered), and its chars are counted back into the bytes
/// `encoding` decoded them from. Reading chars against a byte count instead read
/// one char past the end per multi-byte char, into the next message's header,
/// and the server took the broken header for the client hanging up.
///
/// Never asks for more chars than could fit in the bytes left, so it cannot
/// over-read. An `Error` when input ends early, or when the length ends inside a
/// character, which means the sender's count was wrong: the text would be mangled
/// and the stream is already out of step.
let readExactlyBytes
  (reader : IO.TextReader)
  (encoding : Text.Encoding)
  (byteLength : int)
  : Result<string, string> =
  let maxBytesPerChar = encoding.GetMaxByteCount 1
  let sb = Text.StringBuilder()
  let buffer = Array.zeroCreate<char> (max 2 (byteLength / maxBytesPerChar + 1))
  let mutable consumed = 0
  let mutable error = None
  while consumed < byteLength && Option.isNone error do
    let want = max 1 ((byteLength - consumed) / maxBytesPerChar)
    let mutable n = reader.Read(buffer, 0, want)
    // A surrogate pair is one character and encodes as a unit; never count half.
    if n > 0 && Char.IsHighSurrogate buffer[n - 1] then
      if reader.Read(buffer, n, 1) = 1 then n <- n + 1
    if n = 0 then
      error <- Some $"input ended after {consumed} of {byteLength} bytes"
    else
      consumed <- consumed + encoding.GetByteCount(buffer, 0, n)
      sb.Append(buffer, 0, n) |> ignore<Text.StringBuilder>
      if consumed > byteLength then
        error <-
          Some
            $"a length of {byteLength} bytes ends inside a character (read {consumed})"
  match error with
  | Some e -> Error e
  | None -> Ok(sb.ToString())


/// One read of stdin, as the stdin reader thread does it: a line, or a byte count read through
/// the same buffered reader (`readExactlyBytes`). The only place either is read once a scheduler
/// is running, so a header line and the body after it come off one reader in order.
let private readStdinFor (request : HE.StdinRequest) : HE.StdinResult =
  match request with
  | HE.StdinRequest.Line ->
    match Console.In.ReadLine() with
    | null -> Ok None
    | line -> Ok(Some line)
  | HE.StdinRequest.Bytes 0 -> Ok(Some "")
  | HE.StdinRequest.Bytes n ->
    readExactlyBytes Console.In Console.InputEncoding n |> Result.map Some

/// Installed on the first wait for stdin, like the key source. Unlike keys it is installed for
/// redirected stdin too: a pipe is exactly what a language server reads.
let private installStdinSource () : unit =
  if HE.sources.readStdin.IsNone then HE.sources.readStdin <- Some readStdinFor

/// Park the calling process on one read of stdin, when there is a scheduler to park under.
/// `None` outside one: the caller reads synchronously, as it always did.
let private parkOnStdin (spec : HE.EventSpec) : Option<Ply<HE.StdinResult>> =
  match Scheduler.Scheduler.Current, Scheduler.Scheduler.CurrentProcess with
  | Some s, Some p ->
    installStdinSource ()
    let wake = s.Subscribe(p, [ spec ])
    Some(
      uply {
        let! ev = wake
        match ev with
        | HE.HostEvent.Stdin(_, result) -> return result
        | other ->
          return
            Exception.raiseInternal
              "a stdin read woke on something that is not stdin"
              [ "event", other ]
      }
    )
  | _ -> None


/// `Stdlib.Host.EventSpec`, as F#.
let private eventSpecOfDval (vm : VMState) (d : Dval) : HE.EventSpec =
  match d with
  | DEnum(_, _, _, "Key", []) -> HE.EventSpec.Key
  | DEnum(_, _, _, "StoreChanged", []) -> HE.EventSpec.StoreChanged
  | DEnum(_, _, _, "Timer", [ DInt64 ms ]) -> HE.EventSpec.Timer ms
  | DEnum(_, _, _, "ExecDone", [ DUuid id ]) -> HE.EventSpec.ExecDone id
  | DEnum(_, _, _, "StdinLine", []) -> HE.EventSpec.StdinLine
  | DEnum(_, _, _, "StdinBytes", [ DInt64 n ]) when
    n >= 0L && n <= int64 Int32.MaxValue
    ->
    HE.EventSpec.StdinBytes(int n)
  | _ ->
    RuntimeError.UncaughtException("hostAwait: not an EventSpec", [ "spec", d ])
    |> raiseRTE vm.threadID


/// `Stdlib.Host.RawEvent`, from what the queue delivered. `Stdlib.Host.await` turns it into an
/// `Event`, describing a store change on the way.
let private eventToDval (vm : VMState) (ev : HE.HostEvent) : Dval =
  let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Host.rawEvent ())
  let case name fields = DEnum(typeName, typeName, [], name, fields)
  match ev with
  | HE.HostEvent.Key k -> case "Key" [ k ]
  | HE.HostEvent.StoreChanged -> case "StoreChanged" []
  | HE.HostEvent.Timer _ -> case "Timer" []
  | HE.HostEvent.ExecDone id -> case "ExecDone" [ DUuid id ]
  | HE.HostEvent.Stdin(_, Ok(Some text)) -> case "Stdin" [ DString text ]
  | HE.HostEvent.Stdin(_, Ok None) -> case "StdinClosed" []
  // Out of step: what comes next cannot be trusted, and a guess would be read as a message.
  | HE.HostEvent.Stdin(_, Error e) ->
    RuntimeError.UncaughtException($"reading stdin: {e}", [])
    |> raiseRTE vm.threadID
  | HE.HostEvent.Completed _
  | HE.HostEvent.Wake ->
    Exception.raiseInternal
      "an internal event reached a Dark subscriber"
      [ "event", ev ]


/// Wait for the first of `specs` without a scheduler: the thread is held, polling, exactly
/// as `readKey` held it. `ExecDone` can never fire here (no scheduler, no processes) and is
/// ignored; a list of only `ExecDone` specs is an internal error rather than a hang.
let private awaitBlockingPolling (specs : HE.EventSpec list) : HE.HostEvent =
  let wantsKey = List.contains HE.EventSpec.Key specs
  let wantsStore = List.contains HE.EventSpec.StoreChanged specs
  let timer =
    specs
    |> List.tryPick (fun spec ->
      match spec with
      | HE.EventSpec.Timer ms -> Some ms
      | _ -> None)
  if not wantsKey && not wantsStore && timer.IsNone then
    Exception.raiseInternal
      "hostAwait outside the scheduler with nothing that can fire"
      []
  let started = Diagnostics.Stopwatch.StartNew()
  let versionAt = HE.sources.storeVersion |> Option.map (fun v -> v ())
  let mutable result = None
  while result.IsNone do
    if
      wantsKey
      && (Console.IsInputRedirected || pushedBack.IsSome || Console.KeyAvailable)
    then
      result <- Some(HE.HostEvent.Key(readOneKey ()))
    else
      match timer with
      | Some ms when started.ElapsedMilliseconds >= ms ->
        result <- Some(HE.HostEvent.Timer 0L)
      | _ ->
        match wantsStore, versionAt, HE.sources.storeVersion with
        | true, Some before, Some version when version () <> before ->
          result <- Some HE.HostEvent.StoreChanged
        | _ -> Threading.Thread.Sleep 15
  Option.get result


/// `awaitBlockingPolling`, except that stdin on its own is a plain blocking read. Mixed with
/// anything else it would need the reader thread, which only a scheduler drains, and nothing
/// outside one asks for that.
let private awaitBlocking (specs : HE.EventSpec list) : HE.HostEvent =
  let isStdin (spec : HE.EventSpec) =
    match spec with
    | HE.EventSpec.StdinLine
    | HE.EventSpec.StdinBytes _ -> true
    | _ -> false
  match specs with
  | [ HE.EventSpec.StdinLine ] ->
    HE.HostEvent.Stdin(HE.StdinRequest.Line, readStdinFor HE.StdinRequest.Line)
  | [ HE.EventSpec.StdinBytes n ] ->
    HE.HostEvent.Stdin(
      HE.StdinRequest.Bytes n,
      readStdinFor (HE.StdinRequest.Bytes n)
    )
  | _ when List.exists isStdin specs ->
    Exception.raiseInternal
      "hostAwait outside the scheduler can wait for stdin only on its own"
      [ "specs", specs ]
  | _ -> awaitBlockingPolling specs


let fns () : List<BuiltInFn> =
  [ { name = fn "stdinReadKey" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.keyRead ())
        TCustomType(NR.ok typeName, [])
      description =
        "Reads one key press: the key, its modifiers, the text of a paste or the repeat count "
        + "of a burst. Under the scheduler the process parks until a key arrives."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          // Under the scheduler the process parks on the event queue and other processes keep
          // running while it waits; the reader thread posts the key. Redirected stdin never
          // parks: `readKeyOrPaste` answers Escape at once.
          installKeySource ()
          match
            Scheduler.Scheduler.Current,
            Scheduler.Scheduler.CurrentProcess,
            HE.sources.readKey
          with
          | Some s, Some p, Some _ ->
            let wake = s.Subscribe(p, [ HE.EventSpec.Key ])
            uply {
              let! ev = wake
              match ev with
              | HE.HostEvent.Key k -> return k
              | other ->
                return
                  Exception.raiseInternal
                    "readKey woke on something that is not a key"
                    [ "event", other ]
            }
          | _ -> Ply(readOneKey ())
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }


    { name = fn "stdinReadLine" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.option TString
      description =
        "Reads a single line from the standard input, or None at its end, so an answer can't be "
        + "confused with nobody answering. Under the scheduler the process parks until the line "
        + "arrives."
      fn =
        (function
        | _, vm, _, [| DUnit |] ->
          let ofResult (result : HE.StdinResult) : Dval =
            match result with
            | Ok(Some line) -> LibExecution.Dval.optionSome KTString (DString line)
            | Ok None -> LibExecution.Dval.optionNone KTString
            | Error e ->
              RuntimeError.UncaughtException($"stdinReadLine: {e}", [])
              |> raiseRTE vm.threadID
          match parkOnStdin HE.EventSpec.StdinLine with
          | Some wait ->
            uply {
              let! result = wait
              return ofResult result
            }
          | None -> Ply(ofResult (readStdinFor HE.StdinRequest.Line))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }


    { name = fn "stdinReadExactly" 0
      typeParams = []
      parameters =
        [ Param.make
            "length"
            TInt
            "The number of BYTES to read, as the input encodes them." ]
      returnType = TString
      description =
        "Reads exactly <param length> bytes from the standard input and returns them as text. "
        + "Raises if input ends first, or if the length ends inside a character."
      fn =
        (function
        | _, vm, _, [| DInt lengthArg |] ->
          // length must fit a native int and be non-negative; both bounds are
          // "out of range" for this parameter, surfaced as a Dark error.
          let length = intToInt32 vm lengthArg
          if length < 0 then
            RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID
          else
            let ofResult (result : HE.StdinResult) : Dval =
              match result with
              | Ok(Some input) -> DString input
              | Ok None ->
                RuntimeError.UncaughtException(
                  $"stdinReadExactly: input ended after 0 of {length} bytes",
                  []
                )
                |> raiseRTE vm.threadID
              | Error e ->
                RuntimeError.UncaughtException($"stdinReadExactly: {e}", [])
                |> raiseRTE vm.threadID
            match parkOnStdin (HE.EventSpec.StdinBytes length) with
            | Some wait ->
              uply {
                let! result = wait
                return ofResult result
              }
            | None -> Ply(ofResult (readStdinFor (HE.StdinRequest.Bytes length)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }


    { name = fn "stdinReadAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Reads all available input from standard input until EOF. Blocks if "
        + "stdin is an interactive TTY with no EOF signal."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let input = System.Console.In.ReadToEnd()
          Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }


    { name = fn "hostAwait" 0
      typeParams = []
      parameters =
        [ Param.make
            "specs"
            (TList(
              TCustomType(
                NR.ok (
                  FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Host.eventSpec ())
                ),
                []
              )
            ))
            "" ]
      returnType =
        TCustomType(
          NR.ok (FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Host.rawEvent ())),
          []
        )
      description =
        "Parks the calling process until the first of the given events happens, and returns "
        + "it. The contract every host loop is written against (docs/processes.md)."
      fn =
        (function
        | _, vm, _, [| DList(_, specs) |] ->
          let specs = specs |> List.map (eventSpecOfDval vm)
          if List.contains HE.EventSpec.Key specs then installKeySource ()
          if
            specs
            |> List.exists (fun spec ->
              match spec with
              | HE.EventSpec.StdinLine
              | HE.EventSpec.StdinBytes _ -> true
              | _ -> false)
          then
            installStdinSource ()
          match Scheduler.Scheduler.Current, Scheduler.Scheduler.CurrentProcess with
          | Some s, Some p ->
            let wake = s.Subscribe(p, specs)
            uply {
              let! ev = wake
              return eventToDval vm ev
            }
          | _ -> Ply(eventToDval vm (awaitBlocking specs))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      // Static, so the union of what any spec could need: a key is stdin, a store change is a
      // package read. A `[Timer 10]` alone still declares both; narrowing per call would need
      // the effect check to run inside the body, which nothing else does today.
      previewable = Impure
      callEffects = set [ Effect.Stdin; Effect.PackageRead ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
