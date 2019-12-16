open Tc

type browserPlatform =
  | Mac
  | Linux
  | Windows
  | UnknownPlatform

external jsGetBrowserPlatform :
  unit -> browserPlatform Js.Nullable.t
  = "getBrowserPlatform"
  [@@bs.val] [@@bs.scope "window"]

let getBrowserPlatform () : browserPlatform =
  jsGetBrowserPlatform ()
  |> Js.Nullable.toOption
  |> Option.withDefault ~default:UnknownPlatform


(* Character representation that's partially keyboard and partially ascii
 * characters. All keyboard keys are represented, but may be split into
 * multiple characters (eg 5 and percent are the same button, but it's
 * worthwhile knowing which is which).
 *)

type key =
  (* Valid ascii characters. Any key that can be converted will be. *)
  | Space
  | ExclamationMark
  | DoubleQuote
  | Hash
  | Dollar
  | Percent
  | Ampersand
  | SingleQuote
  | LeftParens
  | RightParens
  | Multiply
  | Plus
  | Comma
  | Minus
  | Period
  | ForwardSlash
  | Colon
  | SemiColon
  | LessThan
  | Equals
  | GreaterThan
  | QuestionMark
  | At
  | Letter of char
  | Number of char
  | LeftSquareBracket
  | RightSquareBracket
  | Backslash
  | Caret
  | Underscore
  | Backtick
  | LeftCurlyBrace
  | Pipe
  | RightCurlyBrace
  | Tilde
  (* None of these are valid characters *)
  | Left
  | Right
  | Up
  | Down
  | Shift of side option
  | Ctrl of side option
  | Alt
  | Tab
  | ShiftTab
  | CapsLock
  | Escape
  | Enter
  | ShiftEnter
  | Backspace
  | Delete
  | PageUp
  | PageDown
  | End
  | Home
  | Insert
  | PrintScreen
  | PauseBreak
  | Windows
  | Command
  | ChromeSearch
  | NumLock
  | ScrollLock
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Unknown of string
  | GoToStartOfLine
  | GoToEndOfLine
  | DeletePrevWord
  | DeleteNextWord
  | DeleteToStartOfLine
  | DeleteToEndOfLine
  | GoToStartOfWord
  | GoToEndOfWord
  | Undo
  | Redo
  | SelectAll
[@@deriving show]

and side =
  | LeftHand
  | RightHand
[@@deriving show]

let toChar key : char option =
  match key with
  | Space ->
      Some ' '
  | ExclamationMark ->
      Some '!'
  | DoubleQuote ->
      Some '"'
  | Hash ->
      Some '#'
  | Dollar ->
      Some '$'
  | Percent ->
      Some '%'
  | Ampersand ->
      Some '&'
  | SingleQuote ->
      Some '\''
  | LeftParens ->
      Some '('
  | RightParens ->
      Some ')'
  | Multiply ->
      Some '*'
  | Plus ->
      Some '+'
  | Comma ->
      Some ','
  | Minus ->
      Some '-'
  | Period ->
      Some '.'
  | ForwardSlash ->
      Some '/'
  | Colon ->
      Some ':'
  | SemiColon ->
      Some ';'
  | LessThan ->
      Some '<'
  | Equals ->
      Some '='
  | GreaterThan ->
      Some '>'
  | QuestionMark ->
      Some '?'
  | At ->
      Some '@'
  | Letter l ->
      Some l
  | Number n ->
      Some n
  | LeftSquareBracket ->
      Some '['
  | RightSquareBracket ->
      Some ']'
  | Backslash ->
      Some '\\'
  | Caret ->
      Some '^'
  | Underscore ->
      Some '_'
  | Backtick ->
      Some '`'
  | LeftCurlyBrace ->
      Some '{'
  | Pipe ->
      Some '|'
  | RightCurlyBrace ->
      Some '}'
  | Tilde ->
      Some '~'
  | Left
  | Right
  | Up
  | Down
  | Tab
  | Alt
  | ShiftTab
  | Escape
  | CapsLock
  | Enter
  | ShiftEnter
  | Backspace
  | Delete
  | PageUp
  | PageDown
  | End
  | Home
  | Insert
  | PrintScreen
  | PauseBreak
  | Windows
  | Command
  | ChromeSearch
  | NumLock
  | ScrollLock
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Shift _
  | Ctrl _
  | Unknown _
  | GoToStartOfLine
  | GoToEndOfLine
  | DeletePrevWord
  | DeleteNextWord
  | DeleteToStartOfLine
  | DeleteToEndOfLine
  | GoToStartOfWord
  | GoToEndOfWord
  | Undo
  | Redo
  | SelectAll ->
      None


let toName = show_key

let fromChar (c : char) : key =
  match c with
  | ' ' ->
      Space
  | '`' ->
      Backtick
  | '~' ->
      Tilde
  | '!' ->
      ExclamationMark
  | '"' ->
      DoubleQuote
  | '#' ->
      Hash
  | '$' ->
      Dollar
  | '%' ->
      Percent
  | '^' ->
      Caret
  | '&' ->
      Ampersand
  | '\'' ->
      SingleQuote
  | '(' ->
      LeftParens
  | ')' ->
      RightParens
  | '*' ->
      Multiply
  | '+' ->
      Plus
  | ',' ->
      Comma
  | '-' ->
      Minus
  | '_' ->
      Underscore
  | '.' ->
      Period
  | '/' ->
      ForwardSlash
  | '\\' ->
      Backslash
  | '|' ->
      Pipe
  | ':' ->
      Colon
  | ';' ->
      SemiColon
  | '[' ->
      LeftSquareBracket
  | ']' ->
      RightSquareBracket
  | '{' ->
      LeftCurlyBrace
  | '}' ->
      RightCurlyBrace
  | '<' ->
      LessThan
  | '=' ->
      Equals
  | '>' ->
      GreaterThan
  | '?' ->
      QuestionMark
  | '@' ->
      At
  | '0' .. '9' ->
      Number c
  | 'A' .. 'Z' | 'a' .. 'z' ->
      Letter c
  | _ ->
      Unknown (String.fromChar c)


let fromKeyboardEvent
    (key : string) (shift : bool) (ctrl : bool) (meta : bool) (alt : bool) :
    key =
  let isMac = getBrowserPlatform () = Mac in
  let osCmdKeyHeld = if isMac then meta else ctrl in
  let isMacCmdHeld = isMac && meta in
  match key with
  (*************
   * Shortcuts *
   *************)
  | "a" when osCmdKeyHeld ->
      SelectAll
  | "a" when ctrl ->
      GoToStartOfLine
  | "d" when ctrl ->
      Delete
  | "e" when ctrl ->
      GoToEndOfLine
  | "y" when (not isMac) && ctrl && not shift ->
      (* CTRL+Y is Windows redo
      but CMD+Y on Mac is the history shortcut in Chrome (since CMD+H is taken for hide)
      See https://support.google.com/chrome/answer/157179?hl=en *)
      Redo
  | "Z" when shift && osCmdKeyHeld ->
      Redo
  | "z" when osCmdKeyHeld ->
      Undo
  | "Backspace" when isMacCmdHeld ->
      DeleteToStartOfLine
  | "Backspace" when (isMac && alt) || ((not isMac) && ctrl) ->
      DeletePrevWord
  | "Delete" when isMacCmdHeld ->
      DeleteToEndOfLine
  | "Delete" when (isMac && alt) || ((not isMac) && ctrl) ->
      DeleteNextWord
  | "ArrowLeft" when meta ->
      GoToStartOfLine
  | "ArrowLeft" when (isMac && alt) || ctrl ->
      (* Allowing Ctrl on macs because it doesnt override any default mac cursor movements.
       * Default behaivor is desktop switching where the OS swallows the event unless disabled *)
      GoToStartOfWord
  | "ArrowRight" when meta ->
      GoToEndOfLine
  | "ArrowRight" when (isMac && alt) || ctrl ->
      (* Allowing Ctrl on macs because it doesnt override any default mac cursor movements.
       * Default behaivor is desktop switching where the OS swallows the event unless disabled *)
      GoToEndOfWord
  (************
   * Movement *
   ************)
  | "PageUp" ->
      PageUp
  | "PageDown" ->
      PageDown
  | "End" ->
      End
  | "Home" ->
      Home
  | "ArrowUp" ->
      Up
  | "ArrowDown" ->
      Down
  | "ArrowLeft" ->
      Left
  | "ArrowRight" ->
      Right
  (*************
   * Modifiers *
   *************)
  | "Shift" ->
      Shift None
  | "Ctrl" ->
      Ctrl None
  | "Alt" ->
      Alt
  | "CapsLock" ->
      CapsLock
  (********
   * Misc *
   ********)
  | "Backspace" ->
      Backspace
  | "Delete" ->
      Delete
  | "Tab" when shift ->
      ShiftTab
  | "Tab" ->
      Tab
  | "Enter" when shift ->
      ShiftEnter
  | "Enter" ->
      Enter
  | "Escape" ->
      Escape
  (****************
   * Single Chars *
   ****************)
  (*
   * alt-x opens command palatte. On macOS, is key = '≈', which we have to hack
   * in with bucklescript UTF-16 literals because OCaml is terrible.
   * TODO: For now, you cannot type non-ASCII chars, but when you can we can't just
   * magically make this an 'x'. *)
  | {js|≈|js} ->
      Letter 'x'
  | _ when String.length key = 1 ->
      Char.fromString key
      |> Option.map ~f:fromChar
      |> Option.withDefault ~default:(Unknown key)
  | _ ->
      Unknown key


type keyEvent =
  { key : key
  ; shiftKey : bool
  ; ctrlKey : bool
  ; altKey : bool
  ; metaKey : bool }
[@@deriving show]

let keyEvent j =
  let open Json.Decode in
  let shift = field "shiftKey" bool j in
  let ctrl = field "ctrlKey" bool j in
  let alt = field "altKey" bool j in
  let meta = field "metaKey" bool j in
  let key = field "key" string j in
  let parsedKey = fromKeyboardEvent key shift ctrl meta alt in
  { key = parsedKey
  ; shiftKey = shift
  ; ctrlKey = ctrl
  ; altKey = alt
  ; metaKey = meta }


let registerGlobal name key tagger =
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn ev = try Some (tagger (keyEvent (Obj.magic ev))) with _ -> None in
    let handler = Vdom.EventHandlerCallback (key, fn) in
    (* TODO: put on window, not document *)
    let elem = Web_node.document_node in
    let cache = Vdom.eventHandler_Register callbacks elem name handler in
    fun () -> ignore (Vdom.eventHandler_Unregister elem name cache)
  in
  Tea_sub.registration key enableCall


let downs ?(key = "") tagger = registerGlobal "keydown" key tagger
