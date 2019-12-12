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
  | "z" when shift && osCmdKeyHeld ->
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
  (***********
   * Symbols *
   ***********)
  | "!" ->
      ExclamationMark
  | "@" ->
      At
  | "#" ->
      Hash
  | "$" ->
      Dollar
  | "%" ->
      Percent
  | "^" ->
      Caret
  | "&" ->
      Ampersand
  | "*" ->
      Multiply
  | "(" ->
      LeftParens
  | ")" ->
      RightParens
  | "-" ->
      Minus
  | "_" ->
      Underscore
  | "=" ->
      Equals
  | "+" ->
      Plus
  | "/" ->
      ForwardSlash
  | "?" ->
      QuestionMark
  | "|" ->
      Pipe
  | "`" ->
      Backtick
  | "~" ->
      Tilde
  | ";" ->
      SemiColon
  | ":" ->
      Colon
  | "." ->
      Period
  | "<" ->
      LessThan
  | ">" ->
      GreaterThan
  | "'" ->
      SingleQuote
  | "\"" ->
      DoubleQuote
  | "[" ->
      LeftSquareBracket
  | "]" ->
      RightSquareBracket
  | "{" ->
      LeftCurlyBrace
  | "}" ->
      RightCurlyBrace
  (********
   * Text *
   ********)
  | " " ->
      Space
  | _ when String.length key = 1 ->
    ( match Char.fromString key with
    | Some ('a' .. 'z' as c) | Some ('A' .. 'Z' as c) ->
        Letter c
    | Some ('0' .. '9' as n) ->
        Number n
    | Some _ | None ->
        Unknown key )
  | _ ->
      Unknown key


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


let toName (key : key) : string =
  match key with
  | Space ->
      "Space"
  | ExclamationMark ->
      "ExclamationMark"
  | DoubleQuote ->
      "DoubleQuote"
  | Hash ->
      "Hash"
  | Dollar ->
      "Dollar"
  | Percent ->
      "Percent"
  | Ampersand ->
      "Ampersand"
  | SingleQuote ->
      "SingleQuote"
  | LeftParens ->
      "LeftParens"
  | RightParens ->
      "RightParens"
  | Multiply ->
      "Multiply"
  | Plus ->
      "Plus"
  | Comma ->
      "Comma"
  | Minus ->
      "Minus"
  | Period ->
      "Period"
  | ForwardSlash ->
      "ForwardSlash"
  | Colon ->
      "Colon"
  | SemiColon ->
      "SemiColon"
  | LessThan ->
      "LessThan"
  | Equals ->
      "Equals"
  | GreaterThan ->
      "GreaterThan"
  | QuestionMark ->
      "QuestionMark"
  | At ->
      "At"
  | Letter l ->
      String.fromChar l
  | Number n ->
      String.fromChar n
  | LeftSquareBracket ->
      "LeftSquareBracket"
  | RightSquareBracket ->
      "RightSquareBracket"
  | Backslash ->
      "Backslash"
  | Caret ->
      "Caret"
  | Underscore ->
      "Underscore"
  | Backtick ->
      "Backtick"
  | LeftCurlyBrace ->
      "LeftCurlyBrace"
  | Pipe ->
      "Pipe"
  | RightCurlyBrace ->
      "RightCurlyBrace"
  | Tilde ->
      "Tilde"
  | Left ->
      "Left"
  | Right ->
      "Right"
  | Up ->
      "Up"
  | Down ->
      "Down"
  | Shift _ ->
      "Shift"
  | Ctrl _ ->
      "Ctrl"
  | Alt ->
      "Alt"
  | Tab ->
      "Tab"
  | ShiftTab ->
      "Tab"
  | CapsLock ->
      "CapsLock"
  | Escape ->
      "Escape"
  | Enter ->
      "Enter"
  | ShiftEnter ->
      "ShiftEnter"
  | Backspace ->
      "Backspace"
  | Delete ->
      "Delete"
  | PageUp ->
      "PageUp"
  | PageDown ->
      "PageDown"
  | End ->
      "End"
  | Home ->
      "Home"
  | Insert ->
      "Insert"
  | PrintScreen ->
      "PrintScreen"
  | PauseBreak ->
      "PauseBreak"
  | Windows ->
      "Windows"
  | Command ->
      "Command"
  | ChromeSearch ->
      "ChromeSearch"
  | NumLock ->
      "NumLock"
  | ScrollLock ->
      "ScrollLock"
  | F1 ->
      "F1"
  | F2 ->
      "F2"
  | F3 ->
      "F3"
  | F4 ->
      "F4"
  | F5 ->
      "F5"
  | F6 ->
      "F6"
  | F7 ->
      "F7"
  | F8 ->
      "F8"
  | F9 ->
      "F9"
  | F10 ->
      "F10"
  | F11 ->
      "F11"
  | F12 ->
      "F12"
  | Unknown hint ->
      "Unknown: " ^ hint
  | GoToStartOfLine ->
      "GoToStartOfLine"
  | GoToEndOfLine ->
      "GoToEndOfLine"
  | DeletePrevWord ->
      "DeletePrevWord"
  | DeleteNextWord ->
      "DeleteNextWord"
  | DeleteToStartOfLine ->
      "DeleteToStartOfLine"
  | DeleteToEndOfLine ->
      "DeleteToEndOfLine"
  | GoToStartOfWord ->
      "GoToStartOfWord"
  | GoToEndOfWord ->
      "GoToEndOfWord"
  | Undo ->
      "Undo"
  | Redo ->
      "Redo"
  | SelectAll ->
      "SelectAll"


let fromChar (char : char) : key =
  match char with
  | ' ' ->
      Space
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
  | '.' ->
      Period
  | '/' ->
      ForwardSlash
  | ':' ->
      Colon
  | ';' ->
      SemiColon
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
      Number char
  | 'A' .. 'Z' | 'a' .. 'z' ->
      Letter char
  | _ ->
      Unknown (String.fromChar char)


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
