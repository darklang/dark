open Tc

type rec key =
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Left
  | Right
  | Up
  | Down
  | Shift(option<side>)
  | Ctrl(option<side>)
  | Alt
  | Tab
  | CapsLock
  | Spacebar
  | Escape
  | Enter
  | Backspace
  | Delete
  | PageUp
  | PageDown
  | End
  | Home
  | Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
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
  | NumpadZero
  | NumpadOne
  | NumpadTwo
  | NumpadThree
  | NumpadFour
  | NumpadFive
  | NumpadSix
  | NumpadSeven
  | NumpadEight
  | NumpadNine
  | Multiply
  | Add
  | Subtract
  | Decimal
  | Divide
  | Ambiguous(list<key>)
  | Unknown(int)

@ppx.deriving(show)
and side =
  | LeftHand
  | RightHand

let fromCode = (keyCode: int): key =>
  switch keyCode {
  | 8 => Backspace
  | 9 => Tab
  | 13 => Enter
  | 16 => Shift(None)
  | 17 => Ctrl(None)
  | 18 => Alt
  | 19 => PauseBreak
  | 20 => CapsLock
  | 27 => Escape
  | 32 => Spacebar
  | 33 => PageUp
  | 34 => PageDown
  | 35 => End
  | 36 => Home
  | 37 => Left
  | 38 => Up
  | 39 => Right
  | 40 => Down
  | 44 => PrintScreen
  | 45 => Insert
  | 46 => Delete
  | 48 => Zero
  | 49 => One
  | 50 => Two
  | 51 => Three
  | 52 => Four
  | 53 => Five
  | 54 => Six
  | 55 => Seven
  | 56 => Eight
  | 57 => Nine
  | 65 => A
  | 66 => B
  | 67 => C
  | 68 => D
  | 69 => E
  | 70 => F
  | 71 => G
  | 72 => H
  | 73 => I
  | 74 => J
  | 75 => K
  | 76 => L
  | 77 => M
  | 78 => N
  | 79 => O
  | 80 => P
  | 81 => Q
  | 82 => R
  | 83 => S
  | 84 => T
  | 85 => U
  | 86 => V
  | 87 => W
  | 88 => X
  | 89 => Y
  | 90 => Z
  | 91 => Ambiguous(list{Windows, Command, ChromeSearch})
  | 96 => NumpadZero
  | 97 => NumpadOne
  | 98 => NumpadTwo
  | 99 => NumpadThree
  | 100 => NumpadFour
  | 101 => NumpadFive
  | 102 => NumpadSix
  | 103 => NumpadSeven
  | 104 => NumpadEight
  | 105 => NumpadNine
  | 106 => Multiply
  | 107 => Add
  | 109 => Subtract
  | 110 => Decimal
  | 111 => Divide
  | 112 => F1
  | 113 => F2
  | 114 => F3
  | 115 => F4
  | 116 => F5
  | 117 => F6
  | 118 => F7
  | 119 => F8
  | 120 => F9
  | 121 => F10
  | 122 => F11
  | 123 => F12
  | 144 => NumLock
  | 145 => ScrollLock
  | _ => Unknown(keyCode)
  }

let code = (key: key): option<int> =>
  switch key {
  | Backspace => Some(8)
  | Tab => Some(9)
  | Enter => Some(13)
  | Shift(_) => Some(16)
  | Ctrl(_) => Some(17)
  | Alt => Some(18)
  | PauseBreak => Some(19)
  | CapsLock => Some(20)
  | Escape => Some(27)
  | Spacebar => Some(32)
  | PageUp => Some(33)
  | PageDown => Some(34)
  | End => Some(35)
  | Home => Some(36)
  | Left => Some(37)
  | Up => Some(38)
  | Right => Some(39)
  | Down => Some(40)
  | PrintScreen => Some(44)
  | Insert => Some(45)
  | Delete => Some(46)
  | Zero => Some(48)
  | One => Some(49)
  | Two => Some(50)
  | Three => Some(51)
  | Four => Some(52)
  | Five => Some(53)
  | Six => Some(54)
  | Seven => Some(55)
  | Eight => Some(56)
  | Nine => Some(57)
  | A => Some(65)
  | B => Some(66)
  | C => Some(67)
  | D => Some(68)
  | E => Some(69)
  | F => Some(70)
  | G => Some(71)
  | H => Some(72)
  | I => Some(73)
  | J => Some(74)
  | K => Some(75)
  | L => Some(76)
  | M => Some(77)
  | N => Some(78)
  | O => Some(79)
  | P => Some(80)
  | Q => Some(81)
  | R => Some(82)
  | S => Some(83)
  | T => Some(84)
  | U => Some(85)
  | V => Some(86)
  | W => Some(87)
  | X => Some(88)
  | Y => Some(89)
  | Z => Some(90)
  | Ambiguous(choices) =>
    if List.all(~f=value => List.member(~value, list{Windows, Command, ChromeSearch}), choices) {
      Some(91)
    } else {
      None
    }
  | Windows => Some(91)
  | Command => Some(91)
  | ChromeSearch => Some(91)
  | NumpadZero => Some(96)
  | NumpadOne => Some(97)
  | NumpadTwo => Some(98)
  | NumpadThree => Some(99)
  | NumpadFour => Some(100)
  | NumpadFive => Some(101)
  | NumpadSix => Some(102)
  | NumpadSeven => Some(103)
  | NumpadEight => Some(104)
  | NumpadNine => Some(105)
  | Multiply => Some(106)
  | Add => Some(107)
  | Subtract => Some(109)
  | Decimal => Some(110)
  | Divide => Some(111)
  | F1 => Some(112)
  | F2 => Some(113)
  | F3 => Some(114)
  | F4 => Some(115)
  | F5 => Some(116)
  | F6 => Some(117)
  | F7 => Some(118)
  | F8 => Some(119)
  | F9 => Some(120)
  | F10 => Some(121)
  | F11 => Some(122)
  | F12 => Some(123)
  | NumLock => Some(144)
  | ScrollLock => Some(145)
  | Unknown(_) => None
  }

@ppx.deriving(show)
type rec keyEvent = {
  keyCode: key,
  key: option<string>,
  shiftKey: bool,
  ctrlKey: bool,
  altKey: bool,
  metaKey: bool,
  targetSelectionStart: option<int>,
  targetSelectionEnd: option<int>,
}

let keyEvent = j => {
  open Json.Decode
  {
    keyCode: field("keyCode", int, j) |> fromCode,
    key: field("key", string, j) |> (
      s =>
        if s == "" {
          None
        } else {
          Some(s)
        }
    ),
    shiftKey: field("shiftKey", bool, j),
    ctrlKey: field("ctrlKey", bool, j),
    altKey: field("altKey", bool, j),
    metaKey: field("metaKey", bool, j),
    targetSelectionStart: field("target", optional(field("selectionStart", int)), j),
    targetSelectionEnd: field("target", optional(field("selectionEnd", int)), j),
  }
}

let registerGlobal = (name, key, tagger) => {
  let enableCall = callbacks_base => {
    let callbacks = ref(callbacks_base)
    let fn = ev =>
      try Some(tagger(keyEvent(Obj.magic(ev)))) catch {
      | _ => None
      }
    let handler = Vdom.EventHandlerCallback(key, fn)
    // TODO: put on window, not document
    let elem = Web_node.document_node
    let cache = Vdom.eventHandler_Register(callbacks, elem, name, handler)
    () => ignore(Vdom.eventHandler_Unregister(elem, name, cache))
  }

  Tea_sub.registration(key, enableCall)
}

let downs = (~key="", tagger) => registerGlobal("keydown", key, tagger)
