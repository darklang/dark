open Tc

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
  | GoToFront

and side =
  | LeftHand
  | RightHand
[@@deriving show]

let fromKeyboardCode (shift : bool) (code : int) : key =
  match code with
  | 8 ->
      Backspace
  | 9 ->
      if shift then ShiftTab else Tab
  | 13 ->
      Enter
  | 16 ->
      Shift None
  | 17 ->
      Ctrl None
  | 18 ->
      Alt
  | 19 ->
      PauseBreak
  | 20 ->
      CapsLock
  | 27 ->
      Escape
  | 32 ->
      Space
  | 33 ->
      PageUp
  | 34 ->
      PageDown
  | 35 ->
      End
  | 36 ->
      Home
  | 37 ->
      Left
  | 38 ->
      Up
  | 39 ->
      Right
  | 40 ->
      Down
  | 45 ->
      Insert
  | 46 ->
      Delete
  | 48 ->
      if shift then RightParens else Number '0'
  | 49 ->
      if shift then ExclamationMark else Number '1'
  | 50 ->
      if shift then At else Number '2'
  | 51 ->
      if shift then Hash else Number '3'
  | 52 ->
      if shift then Dollar else Number '4'
  | 53 ->
      if shift then Percent else Number '5'
  | 54 ->
      if shift then Caret else Number '6'
  | 55 ->
      if shift then Ampersand else Number '7'
  | 56 ->
      if shift then Multiply else Number '8'
  | 57 ->
      if shift then LeftParens else Number '9'
  | 65 ->
      Letter (if shift then 'A' else 'a')
  | 66 ->
      Letter (if shift then 'B' else 'b')
  | 67 ->
      Letter (if shift then 'C' else 'c')
  | 68 ->
      Letter (if shift then 'D' else 'd')
  | 69 ->
      Letter (if shift then 'E' else 'e')
  | 70 ->
      Letter (if shift then 'F' else 'f')
  | 71 ->
      Letter (if shift then 'G' else 'g')
  | 72 ->
      Letter (if shift then 'H' else 'h')
  | 73 ->
      Letter (if shift then 'I' else 'i')
  | 74 ->
      Letter (if shift then 'J' else 'j')
  | 75 ->
      Letter (if shift then 'K' else 'k')
  | 76 ->
      Letter (if shift then 'L' else 'l')
  | 77 ->
      Letter (if shift then 'M' else 'm')
  | 78 ->
      Letter (if shift then 'N' else 'n')
  | 79 ->
      Letter (if shift then 'O' else 'o')
  | 80 ->
      Letter (if shift then 'P' else 'p')
  | 81 ->
      Letter (if shift then 'Q' else 'q')
  | 82 ->
      Letter (if shift then 'R' else 'r')
  | 83 ->
      Letter (if shift then 'S' else 's')
  | 84 ->
      Letter (if shift then 'T' else 't')
  | 85 ->
      Letter (if shift then 'U' else 'u')
  | 86 ->
      Letter (if shift then 'V' else 'v')
  | 87 ->
      Letter (if shift then 'W' else 'w')
  | 88 ->
      Letter (if shift then 'X' else 'x')
  | 89 ->
      Letter (if shift then 'Y' else 'y')
  | 90 ->
      Letter (if shift then 'Z' else 'z')
  | 91 ->
      Windows
  | 92 ->
      Windows
  (* Number pad - just pretend it's the same, though no shift versions *)
  | 96 ->
      Number '0'
  | 97 ->
      Number '1'
  | 98 ->
      Number '2'
  | 99 ->
      Number '3'
  | 100 ->
      Number '4'
  | 101 ->
      Number '5'
  | 102 ->
      Number '6'
  | 103 ->
      Number '7'
  | 104 ->
      Number '8'
  | 105 ->
      Number '9'
  | 106 ->
      Multiply
  | 107 ->
      Plus
  | 109 ->
      Minus
  | 110 ->
      Period
  | 111 ->
      ForwardSlash
  | 112 ->
      F1
  | 113 ->
      F2
  | 114 ->
      F3
  | 115 ->
      F4
  | 116 ->
      F5
  | 117 ->
      F6
  | 118 ->
      F7
  | 119 ->
      F8
  | 120 ->
      F9
  | 121 ->
      F10
  | 122 ->
      F11
  | 123 ->
      F12
  | 144 ->
      NumLock
  | 145 ->
      ScrollLock
  (* rhs of keyboard *)
  | 186 ->
      if shift then Colon else SemiColon
  | 187 ->
      if shift then Plus else Equals
  | 188 ->
      if shift then LessThan else Comma
  | 189 ->
      if shift then Underscore else Minus
  | 190 ->
      if shift then GreaterThan else Period
  | 191 ->
      if shift then QuestionMark else ForwardSlash
  | 219 ->
      if shift then LeftCurlyBrace else LeftSquareBracket
  | 220 ->
      if shift then Pipe else Backslash
  | 221 ->
      if shift then RightCurlyBrace else RightSquareBracket
  | 222 ->
      if shift then DoubleQuote else SingleQuote
  | _ ->
      Unknown (string_of_int code)


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
  | GoToFront ->
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
  | GoToFront ->
      "GoToFront"


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
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      Number char
  | 'A'
  | 'B'
  | 'C'
  | 'D'
  | 'E'
  | 'F'
  | 'G'
  | 'H'
  | 'I'
  | 'J'
  | 'K'
  | 'L'
  | 'M'
  | 'N'
  | 'O'
  | 'P'
  | 'Q'
  | 'R'
  | 'S'
  | 'T'
  | 'U'
  | 'V'
  | 'W'
  | 'X'
  | 'Y'
  | 'Z'
  | 'a'
  | 'b'
  | 'c'
  | 'd'
  | 'e'
  | 'f'
  | 'g'
  | 'h'
  | 'i'
  | 'j'
  | 'k'
  | 'l'
  | 'm'
  | 'n'
  | 'o'
  | 'p'
  | 'q'
  | 'r'
  | 's'
  | 't'
  | 'u'
  | 'v'
  | 'w'
  | 'x'
  | 'y'
  | 'z' ->
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
  { key = field "keyCode" int j |> fromKeyboardCode shift
  ; shiftKey = field "shiftKey" bool j
  ; ctrlKey = field "ctrlKey" bool j
  ; altKey = field "altKey" bool j
  ; metaKey = field "metaKey" bool j }


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
