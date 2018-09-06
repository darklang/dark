-- TODO: vendored for 0.19 since it isn't updated yet on package.elm-lang.org
-- Vendored from https://github.com/SwiftsNamesake/proper-keyboard

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Keyboard.Key exposing (..)
{-| Readable and typesafe key names.

# Definition
@docs Key, Side

#
@docs fromCode, code, toChar

-}

-- Types -----------------------------------------------------------------------------------------------------------------------------------

{-| Represents a Key. Currently incomplete. -}
-- TODO | - Complete the set (Function keys, Numpad, etc.)
--        - Categories and groups (eg. NumPad Int | Letter Char)
--        - Distinguishing duplicates (LeftCtrl | RightCtrl | LeftAlt | RightAlt)
--        - Should there be an AltLeft/AltRight?
--        - How do we deal with AltGr?
--        - How do we deal with Numeric?
--        - Sort out the logical grouping
type Key =
  -- Letters
    A | B | C | D | E | F | G | H | I | J
  | K | L | M | N | O | P | Q | R | S | T
  | U | V | W | X | Y | Z

  -- Arrow keys
  | Left | Right | Up | Down

  -- Modifiers
  | Shift (Maybe Side)
  | Ctrl  (Maybe Side)
  | Alt | Tab | CapsLock

  -- Control
  | Spacebar | Escape | Enter | Backspace | Delete | PageUp | PageDown | End | Home

  -- Digits row
  | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine

  | Insert | PrintScreen | PauseBreak
  | Windows | Command | ChromeSearch
  | NumLock | ScrollLock

  -- Function keys
  | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12

  -- Numpad
  | NumpadZero | NumpadOne | NumpadTwo | NumpadThree | NumpadFour | NumpadFive | NumpadSix | NumpadSeven | NumpadEight | NumpadNine
  | Multiply | Add | Subtract | Decimal | Divide

  -- Unspecified
  | Ambiguous (List Key) -- TODO: Pattern synonyms?
  | Unknown Int -- | Special _


-- {-| Represents a Key category -}
-- TODO | - I have to think carefully about how to define categories
-- type Category = Character Char | Modifier ModifierKey


{-| Type used to distinguish between multiple instances of the same key (such as Left Ctrl and Right Ctrl) -}
type Side = LeftHand | RightHand

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

{-| Transform a Int to a Key -}
-- TODO | - Use array instead?
fromCode : Int -> Key
fromCode code = case code of

  --
  8 -> Backspace
  9 -> Tab

  --
  13 -> Enter

  -- Modifiers
  16 -> Shift Nothing
  17 -> Ctrl  Nothing
  18 -> Alt
  19 -> PauseBreak
  20 -> CapsLock

  --
  27 -> Escape

  --
  32 -> Spacebar

  --
  33 -> PageUp
  34 -> PageDown
  35 -> End
  36 -> Home

  -- Arrows
  37 -> Left
  38 -> Up
  39 -> Right
  40 -> Down

  --
  44 -> PrintScreen
  45 -> Insert
  46 -> Delete

  -- Digits
  48 -> Zero
  49 -> One
  50 -> Two
  51 -> Three
  52 -> Four
  53 -> Five
  54 -> Six
  55 -> Seven
  56 -> Eight
  57 -> Nine

  -- Letters
  65 -> A
  66 -> B
  67 -> C
  68 -> D
  69 -> E
  70 -> F
  71 -> G
  72 -> H
  73 -> I
  74 -> J
  75 -> K
  76 -> L
  77 -> M
  78 -> N
  79 -> O
  80 -> P
  81 -> Q
  82 -> R
  83 -> S
  84 -> T
  85 -> U
  86 -> V
  87 -> W
  88 -> X
  89 -> Y
  90 -> Z

  -- System
  91 -> Ambiguous [Windows, Command, ChromeSearch]

  -- Numpad
  96  -> NumpadZero
  97  -> NumpadOne
  98  -> NumpadTwo
  99  -> NumpadThree
  100 -> NumpadFour
  101 -> NumpadFive
  102 -> NumpadSix
  103 -> NumpadSeven
  104 -> NumpadEight
  105 -> NumpadNine

  106 -> Multiply
  107 -> Add
  -- TODO: Numpad Enter
  109 -> Subtract
  110 -> Decimal
  111 -> Divide

  -- Function keys
  112 -> F1
  113 -> F2
  114 -> F3
  115 -> F4
  116 -> F5
  117 -> F6
  118 -> F7
  119 -> F8
  120 -> F9
  121 -> F10
  122 -> F11
  123 -> F12

  -- Locking keys
  144 -> NumLock
  145 -> ScrollLock

  -- Miscellaneous
  _ -> Unknown code


{-| Attempts to transform a key into a keycode -}
-- TODO | - Use array instead?
code : Key -> Maybe Int
code key = case key of

  --
  Backspace -> Just 8
  Tab      -> Just 9

  --
  Enter -> Just 13

  -- Modifiers
  Shift _ -> Just 16
  Ctrl  _ -> Just 17
  Alt     -> Just 18
  PauseBreak -> Just 19
  CapsLock   -> Just 20

  --
  Escape -> Just 27

  --
  Spacebar -> Just 32

  -- Navigation
  PageUp   -> Just 33
  PageDown -> Just 34
  End      -> Just 35
  Home     -> Just 36

  -- Arrows
  Left  -> Just 37
  Up    -> Just 38
  Right -> Just 39
  Down  -> Just 40

  --
  PrintScreen -> Just 44 -- The OS will probably intercept this one before we have a chance to respond
  Insert -> Just 45
  Delete -> Just 46

  -- Digits
  Zero  -> Just 48
  One   -> Just 49
  Two   -> Just 50
  Three -> Just 51
  Four  -> Just 52
  Five  -> Just 53
  Six   -> Just 54
  Seven -> Just 55
  Eight -> Just 56
  Nine  -> Just 57

  -- Letters
  A -> Just 65
  B -> Just 66
  C -> Just 67
  D -> Just 68
  E -> Just 69
  F -> Just 70
  G -> Just 71
  H -> Just 72
  I -> Just 73
  J -> Just 74
  K -> Just 75
  L -> Just 76
  M -> Just 77
  N -> Just 78
  O -> Just 79
  P -> Just 80
  Q -> Just 81
  R -> Just 82
  S -> Just 83
  T -> Just 84
  U -> Just 85
  V -> Just 86
  W -> Just 87
  X -> Just 88
  Y -> Just 89
  Z -> Just 90

  -- System
  Ambiguous choices -> if List.all (flip List.member [Windows, Command, ChromeSearch]) choices then Just 91 else Nothing
  Windows -> Just 91
  Command -> Just 91
  ChromeSearch -> Just 91

  -- Numpad digits
  NumpadZero  -> Just  96
  NumpadOne   -> Just  97
  NumpadTwo   -> Just  98
  NumpadThree -> Just  99
  NumpadFour  -> Just 100
  NumpadFive  -> Just 101
  NumpadSix   -> Just 102
  NumpadSeven -> Just 103
  NumpadEight -> Just 104
  NumpadNine  -> Just 105

  -- Numpad operators
  Multiply -> Just 106
  Add      -> Just 107
  -- TODO: NumpadEnter here?
  Subtract -> Just 109
  Decimal  -> Just 110
  Divide   -> Just 111

  -- Function keys
  F1  -> Just 112
  F2  -> Just 113
  F3  -> Just 114
  F4  -> Just 115
  F5  -> Just 116
  F6  -> Just 117
  F7  -> Just 118
  F8  -> Just 119
  F9  -> Just 120
  F10 -> Just 121
  F11 -> Just 122
  F12 -> Just 123

  -- Locking keys
  NumLock    -> Just 144
  ScrollLock -> Just 145

  -- Miscellaneous
  Unknown _ -> Nothing


{-| Attempt to transform a Key into a Char. This does not work for 'special' keys that are not used to type symbols. -}
toChar : Key -> Maybe Char
toChar key = case key of

  -- Space
  Spacebar -> Just ' '

  -- Digits
  Zero  -> Just '0'
  One   -> Just '1'
  Two   -> Just '2'
  Three -> Just '3'
  Four  -> Just '4'
  Five  -> Just '5'
  Six   -> Just '6'
  Seven -> Just '7'
  Eight -> Just '8'
  Nine  -> Just '9'

  -- Letters
  A -> Just 'A'
  B -> Just 'B'
  C -> Just 'C'
  D -> Just 'D'
  E -> Just 'E'
  F -> Just 'F'
  G -> Just 'G'
  H -> Just 'H'
  I -> Just 'I'
  J -> Just 'J'
  K -> Just 'K'
  L -> Just 'L'
  M -> Just 'M'
  N -> Just 'N'
  O -> Just 'O'
  P -> Just 'P'
  Q -> Just 'Q'
  R -> Just 'R'
  S -> Just 'S'
  T -> Just 'T'
  U -> Just 'U'
  V -> Just 'V'
  W -> Just 'W'
  X -> Just 'X'
  Y -> Just 'Y'
  Z -> Just 'Z'

  -- Numpad digits
  NumpadZero  -> Just '0'
  NumpadOne   -> Just '1'
  NumpadTwo   -> Just '2'
  NumpadThree -> Just '3'
  NumpadFour  -> Just '4'
  NumpadFive  -> Just '5'
  NumpadSix   -> Just '6'
  NumpadSeven -> Just '7'
  NumpadEight -> Just '8'
  NumpadNine  -> Just '9'

  -- Numpad operators
  Multiply -> Just '*'
  Add      -> Just '+'
  Subtract -> Just '-'
  Divide   -> Just '/'

  -- Dunno
  _ -> Nothing


-- Tests -----------------------------------------------------------------------------------------------------------------------------------
