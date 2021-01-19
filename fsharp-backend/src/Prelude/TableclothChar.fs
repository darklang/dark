module Tablecloth.Char

// Functions for working with single characters.

type t = char

let toCode (c : char) : int = System.Convert.ToInt32 c

let to_code c = toCode c

let fromCode (i : int) =
  try
    Some(System.Convert.ToChar i)
  with _ -> None

let from_code i = fromCode i

let toString (c : char) : string = c.ToString()

let to_string c = toString c

let fromString (str : string) =
  try
    Some(System.Convert.ToChar str)
  with _ -> None

let from_string s = fromString s

let toDigit char =
  match char with
  | '0' -> Some 0
  | '1' -> Some 1
  | '2' -> Some 2
  | '3' -> Some 3
  | '4' -> Some 4
  | '5' -> Some 5
  | '6' -> Some 6
  | '7' -> Some 7
  | '8' -> Some 8
  | '9' -> Some 9
  | _ -> None


let to_digit c = toDigit c

let toLowercase c = System.Char.ToLower c

let to_lowercase c = toLowercase c

let toUppercase c = System.Char.ToUpper c

let to_uppercase c = toUppercase c

let isLowercase c = System.Char.IsLower c

let is_lowercase c = isLowercase c

let isUppercase c = System.Char.IsUpper c

let is_uppercase c = isUppercase c

let isLetter c = System.Char.IsLetter c

let is_letter c = isLetter c

let isDigit c = System.Char.IsDigit c

let is_digit c = isDigit c

let isAlphanumeric c = System.Char.IsLetterOrDigit c

let is_alphanumeric c = isAlphanumeric c

let isPrintable c = not (System.Char.IsControl c)

let is_printable c = isPrintable c

let isWhitespace c = System.Char.IsWhiteSpace c

let is_whitespace c = isWhitespace c

let equal (c1 : char) (c2 : char) = c1 = c2

let compare (c1 : char) (c2 : char) = c1.CompareTo c2
