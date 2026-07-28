/// Standard libraries for reading data from the user via the CLI
module Builtins.Cli.Libs.Stdin

open System

open Prelude

open LibExecution.RuntimeTypes
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution

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


/// Reads the next keypress, or the whole text of a paste when one is detected.
///
/// Two things queue a flood of input: a paste (a run of printable characters)
/// and a mouse-wheel scroll (a run of arrow-key escapes, in alternate-screen
/// mode). We keep the paste but collapse the scroll. Printable characters (plus
/// newlines/tabs) are collected and returned as `Some` text, to be inserted in
/// one go; if only control keys were queued, we return just the last key so a
/// scroll renders once instead of flickering.
/// How long a burst may keep draining before we hand back what we have, and how long a gap ends it. A wheel
/// spin sends events a few ms apart; 4ms of quiet is comfortably a pause without waiting on a human. The
/// budget bounds the worst case so a continuous stream can't starve rendering.
let private quietMs = 4.0
let private burstBudgetMs = 40.0

/// Returns the key, the text of a paste when one is detected, and how many times the key repeated within a
/// coalesced burst (1 for an ordinary keypress).
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
      // A burst that starts with a control key is a scroll or a held-down key, not a paste: a wheel in
      // alternate-screen mode arrives as a run of arrow escapes. Coalesce the run and report how many of it
      // were the same key, so the caller applies all the movement in ONE frame.
      //
      // Draining used to throw the count away and return a single key, which is why a flick of the wheel
      // moved a list by exactly one row however far you spun it (measured: burst of 1 -> 1 row, burst of
      // 20 -> 1 row).
      elif not (isPrintable first) then
        let sw = Diagnostics.Stopwatch.StartNew()
        let mutable repeat = 1
        let mutable last = first
        let mutable draining = true
        while draining do
          if Console.KeyAvailable then
            let k = Console.ReadKey true
            last <- k
            if k.Key = first.Key && k.Modifiers = first.Modifiers then repeat <- repeat + 1
          // Nothing waiting: give the burst a moment to continue, since a wheel's events arrive with small
          // gaps. A quiet stretch ends the burst; so does the overall budget, whichever comes first, so a
          // long spin still renders promptly instead of us sitting here reading.
          elif sw.Elapsed.TotalMilliseconds < quietMs then
            Threading.Thread.Sleep 1
          else
            draining <- false
          if sw.Elapsed.TotalMilliseconds >= burstBudgetMs then draining <- false
          // The quiet window is measured from the last key, not from the start of the burst.
          if Console.KeyAvailable then sw.Restart()
        (last, None, repeat)
      else
        let sb = System.Text.StringBuilder()
        let append (text : string) : unit =
          sb.Append text |> ignore<System.Text.StringBuilder>
        let mutable last = first
        let mutable printableKey = if isPrintable first then Some first else None
        pasteText first |> Option.iter append
        while Console.KeyAvailable do
          let k = Console.ReadKey true
          last <- k
          pasteText k |> Option.iter append
          if isPrintable k then printableKey <- Some k
        let pasted = sb.ToString()
        if pasted = "" then
          (last, None, 1)
        else
          (Option.defaultValue last printableKey, Some pasted, 1)

let fns () : List<BuiltInFn> =
  [ { name = fn "stdinReadKey" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.keyRead ())
        TCustomType(NR.ok typeName, [])
      description = "Reads a single line from the standard input."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          // Treat Ctrl+C as input so the Dark code can handle it gracefully
          Console.TreatControlCAsInput <- true
          let readKey, pasteText, repeat = readKeyOrPaste ()
          Console.TreatControlCAsInput <- false

          let altHeld =
            (readKey.Modifiers &&& ConsoleModifiers.Alt) <> ConsoleModifiers.None
          let shiftHeld =
            (readKey.Modifiers &&& ConsoleModifiers.Shift) <> ConsoleModifiers.None
          let ctrlHeld =
            (readKey.Modifiers &&& ConsoleModifiers.Control) <> ConsoleModifiers.None

          let modifiers =
            let typeName =
              FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.modifiers ())
            let fields =
              [ "alt", DBool altHeld
                "shift", DBool shiftHeld
                "ctrl", DBool ctrlHeld ]
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
            let typeName =
              FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.key ())
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

          Ply(keyRead)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdin
      deprecated = NotDeprecated }


    { name = fn "stdinReadLine" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Reads a single line from the standard input."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let input = System.Console.ReadLine()
          if input = null then Ply(DString "") else Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdin
      deprecated = NotDeprecated }


    { name = fn "stdinIsInteractive" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TBool
      description = "Returns whether or not the terminal is 'interactive' (a tty)"
      fn =
        function
        | _, _, _, [ DUnit ] ->
          (not Console.IsInputRedirected || not Console.IsOutputRedirected)
          |> DBool
          |> Ply
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stdinReadExactly" 0
      typeParams = []
      parameters = [ Param.make "length" TInt "The number of characters to read." ]
      returnType = TString
      description = "Reads a specified number of characters from the standard input."
      fn =
        (function
        | _, vm, _, [ DInt lengthArg ] ->
          // length must fit a native int and be non-negative; both bounds are
          // "out of range" for this parameter, surfaced as a Dark error.
          let length = intToInt32 vm lengthArg
          if length < 0 then
            RuntimeError.Ints.OutOfRange |> RuntimeError.Int |> raiseRTE vm.threadID
          else
            let buffer = Array.zeroCreate length
            let bytesRead = System.Console.In.Read(buffer, 0, length)
            let input = System.String(buffer, 0, bytesRead)
            Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdin
      deprecated = NotDeprecated }


    { name = fn "stdinReadAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description =
        "Reads all available input from standard input until EOF.
        Blocks if stdin is an interactive TTY with no EOF signal."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let input = System.Console.In.ReadToEnd()
          Ply(DString input)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdin
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
