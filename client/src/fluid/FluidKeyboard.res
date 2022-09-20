open Tc

type browserPlatform =
  | Mac
  | Linux
  | Windows
  | UnknownPlatform

@val @scope("window")
external jsGetBrowserPlatform: unit => Js.Nullable.t<browserPlatform> = "getBrowserPlatform"

let getBrowserPlatform = (): browserPlatform =>
  jsGetBrowserPlatform() |> Js.Nullable.toOption |> Option.unwrap(~default=UnknownPlatform)

// Character representation that's partially keyboard and partially ascii characters.
// All keyboard keys are represented, but may be split into multiple characters (eg 5
// and percent are the same button, but it's worthwhile knowing which is which).

@ppx.deriving(show)
type rec shortcutHeritage =
  | LegacyShortcut
  | CurrentShortcut

@ppx.deriving(show)
type rec key =
  | Space
  | Left
  | Right
  | Up
  | Down
  | Shift(option<side>)
  | Ctrl(option<side>)
  | Alt
  | Tab
  | ShiftTab
  | CapsLock
  | Escape
  | Enter
  | ShiftEnter
  | PageUp
  | PageDown
  | GoToStartOfLine(maintainSelection)
  | GoToEndOfLine(maintainSelection)
  | GoToStartOfWord(maintainSelection)
  | GoToEndOfWord(maintainSelection)
  | Undo
  | Redo
  | SelectAll
  | CommandPalette(shortcutHeritage)
  | Omnibox
  | Unhandled(string)

@ppx.deriving(show)
and side =
  | LeftHand
  | RightHand

@ppx.deriving(show)
and maintainSelection =
  | KeepSelection
  | DropSelection

let toName = show_key

let fromKeyboardEvent = (key: string, shift: bool, ctrl: bool, meta: bool, alt: bool): key => {
  let isMac = getBrowserPlatform() == Mac
  let osCmdKeyHeld = if isMac {
    meta
  } else {
    ctrl
  }
  let maintainSelection = if shift {
    KeepSelection
  } else {
    DropSelection
  }
  switch key {
  /* ************
   * Shortcuts *
   ************ */
  | "a" if osCmdKeyHeld => SelectAll
  | "a" if ctrl => GoToStartOfLine(maintainSelection)
  | "e" if ctrl => GoToEndOfLine(maintainSelection)
  | "k" if ctrl || meta => Omnibox
  | "y" if !isMac && (ctrl && !shift) =>
    /* CTRL+Y is Windows redo
      but CMD+Y on Mac is the history shortcut in Chrome (since CMD+H is taken for hide)
      See https://support.google.com/chrome/answer/157179?hl=en */
    Redo
  | "Z" | "z" if shift && osCmdKeyHeld => Redo
  | "Z" | "z" if !shift && osCmdKeyHeld => Undo
  | "ArrowLeft" if meta => GoToStartOfLine(maintainSelection)
  | "ArrowLeft" if (isMac && alt) || ctrl =>
    /* Allowing Ctrl on macs because it doesnt override any default mac cursor movements.
     * Default behaivor is desktop switching where the OS swallows the event unless disabled */
    GoToStartOfWord(maintainSelection)
  | "ArrowRight" if meta => GoToEndOfLine(maintainSelection)
  | "ArrowRight" if (isMac && alt) || ctrl =>
    /* Allowing Ctrl on macs because it doesnt override any default mac cursor movements.
     * Default behaivor is desktop switching where the OS swallows the event unless disabled */
    GoToEndOfWord(maintainSelection)
  /* ***********
   * Movement *
   *********** */
  | "PageUp" => PageUp
  | "PageDown" => PageDown
  | "End" if ctrl => GoToStartOfLine(maintainSelection)
  | "End" => GoToEndOfLine(maintainSelection)
  | "Home" if ctrl => GoToEndOfLine(maintainSelection)
  | "Home" => GoToStartOfLine(maintainSelection)
  | "ArrowUp" => Up
  | "ArrowDown" => Down
  | "ArrowLeft" => Left
  | "ArrowRight" => Right
  /* ************
   * Modifiers *
   ************ */
  | "Shift" => Shift(None)
  | "Ctrl" => Ctrl(None)
  | "Alt" => Alt
  | "CapsLock" => CapsLock
  /* *******
   * Misc *
   ******* */
  | "Tab" if shift => ShiftTab
  | "Tab" => Tab
  | "Enter" if shift => ShiftEnter
  | "Enter" => Enter
  | "Escape" => Escape
  | " " => Space
  | "s" if ctrl => CommandPalette(CurrentShortcut)
  | "\\" if ctrl => CommandPalette(CurrentShortcut)
  /* ~*~*~*~ HERE BE DRAGONS ~*~*~*~*
   * alt-x opens command palatte.
   *
   * Since Alt-something is unreliable on different keyboard layouts, We
   * check for Alt-X, Ctrl-s and Ctrl-\ when opening the command palette.
   *
   *
   * On macOS, is key = '≈', which we have to hack in with ReScript JS
   * literals.
   *
   * As a bonus, ReScript doesn't seem to parse this correctly if it's in a
   * match conditional, meaning you still get UTF-8 escape sequences in the JS
   * instead of an unescaped JS literal. So...
   *
   * DON'T DO THIS:
   *   | {js|≈|js} ->
   * or you end up with this in the JS:
   *   case "\xe2\x89\x88" :
   * whereas what you want is this:
   *   case "≈" :
   *
   * An if statement seems to work correctly, so we use that instead.
   *
   * FIXME: This also means it's impossible to type the literal character '≈' in our
   * editor right now, which we probably should fix at some point. This all
   * points to the fact that it may be easier to do shortcuts with Cmd/Ctrl
   * instead of Alt. */
  | "x" if alt => CommandPalette(CurrentShortcut)
  | _ if alt && String.length(key) == 1 =>
    if key == `≈` {
      CommandPalette(CurrentShortcut)
    } else {
      Unhandled(key)
    }
  | _ => Unhandled(key)
  }
}

@ppx.deriving(show)
type rec keyEvent = {
  key: key,
  shiftKey: bool,
  ctrlKey: bool,
  altKey: bool,
  metaKey: bool,
}

@ocaml.doc(" eventToKeyEvent converts the JS KeyboardEvent [evt] into a [keyEvent].
 * Returns (Some keyEvent) or None if a decoding error occurs. ")
let eventToKeyEvent = (evt: Web.Node.event): option<keyEvent> => {
  open Tea.Json.Decoder
  let decoder = map5((rawKey, shiftKey, ctrlKey, altKey, metaKey) => {
    let key = fromKeyboardEvent(rawKey, shiftKey, ctrlKey, metaKey, altKey)
    {key: key, shiftKey: shiftKey, ctrlKey: ctrlKey, altKey: altKey, metaKey: metaKey}
  }, field(
    "key",
    string,
  ), field("shiftKey", bool), field("ctrlKey", bool), field("altKey", bool), field("metaKey", bool))

  decodeEvent(decoder, evt) |> Tea_result.result_to_option
}

@ocaml.doc(" onKeydown converts the JS KeyboardEvent [evt] into a keyEvent, then
  * calls the [tagger] with it if successful.
  *
  * [tagger] is a (keyEvent -> Types.msg). It would be nice to simply return
  * the msg option here, but we cannot reference Types here otherwise we get
  * a dependency cycle. ")
let onKeydown = (tagger, evt: Web.Node.event) =>
  evt
  |> eventToKeyEvent
  |> Option.andThen(~f=x =>
    switch x {
    | {key: Unhandled(_), _} => None
    | kevt =>
      // if we are going to handle the key, then preventDefault
      evt["preventDefault"]()
      Some(tagger(kevt))
    }
  )
