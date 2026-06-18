/// Standard libraries for reading data from the user via the CLI
module Builtins.Cli.Libs.Stdin

open System

open Prelude

open LibExecution.RuntimeTypes
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution

open Builtin.Shortcuts

/// Reads the next keypress, or the whole text of a paste when one is detected.
///
/// Two things queue a flood of input: a paste (a run of printable characters)
/// and a mouse-wheel scroll (a run of arrow-key escapes, in alternate-screen
/// mode). We keep the paste but collapse the scroll. Printable characters (plus
/// newlines/tabs) are collected and returned as `Some` text, to be inserted in
/// one go; if only control keys were queued, we return just the last key so a
/// scroll renders once instead of flickering.
let private readKeyOrPaste () : ConsoleKeyInfo * string option =
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

  let first = Console.ReadKey true
  if not Console.KeyAvailable then
    (first, None)
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
      (last, None) // only control keys queued, e.g. a scroll
    else
      (Option.defaultValue last printableKey, Some pasted)

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
          let readKey, pasteText = readKeyOrPaste ()
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
              Map [ "key", key; "modifiers", modifiers; "keyChar", keyChar ]
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
        | _, _, _, [ DInt length ] ->
          let length = int (DarkInt.toBigInt length)
          if length < 0 then
            Exception.raiseInternal "Length must be non-negative" []
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
