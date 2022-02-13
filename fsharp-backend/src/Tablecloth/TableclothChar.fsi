module Tablecloth.Char

(** Functions for working with single characters.

    Character literals are enclosed in ['a'] pair of single quotes.

    {[let digit = '7']}

    The functions in this module work on ASCII characters (range 0-255) only,
    {b not Unicode}.

    Since character 128 through 255 have varying values depending on what
    standard you are using (ISO 8859-1 or Windows 1252), you are advised to
    stick to the 0-127 range.
*)

type t = char

(** {1 Create}

    You can also create a {!Char} using single quotes:

    {[let char = 'c']}
*)

val fromCode: int -> char option
(** Convert an ASCII {{: https://en.wikipedia.org/wiki/Code_point } code point } to a character.

    Returns [None] if the codepoint is outside the range of 0 to 255 inclusive.

    {2 Examples}

    {[Char.fromCode 65 = Some 'A']}

    {[Char.fromCode 66 = Some 'B']}

    {[Char.fromCode 3000 = None]}

    {[Char.fromCode (-1) = None]}

    The full range of extended ASCII is from [0] to [255]. For numbers outside that range, you get [None].
*)

val from_code: int -> char option

val fromString: string -> char option
(** Converts a string to character. Returns None when the string isn't of length one.

    {2 Examples}

    {[Char.fromString "A" = Some 'A']}

    {[Char.fromString " " = Some ' ']}

    {[Char.fromString "" = None]}

    {[Char.fromString "abc" = None]}

    {[Char.fromString " a" = None]}
*)

val from_string: string -> char option

(** {1 Query}  *)

val isLowercase: char -> bool
(** Detect lower case ASCII characters.

    {2 Examples}

    {[Char.isLowercase 'a' = true]}

    {[Char.isLowercase 'b' = true]}

    {[Char.isLowercase 'z' = true]}

    {[Char.isLowercase '0' = false]}

    {[Char.isLowercase 'A' = false]}

    {[Char.isLowercase '-' = false]}
*)

val is_lowercase: char -> bool

val isUppercase: char -> bool
(** Detect upper case ASCII characters.

    {2 Examples}

    {[Char.isUppercase 'A' = true]}

    {[Char.isUppercase 'B' = true]}

    {[Char.isUppercase 'Z' = true]}

    {[Char.isUppercase 'h' = false]}

    {[Char.isUppercase '0' = false]}

    {[Char.isUppercase '-' = false]}
*)

val is_uppercase: char -> bool

val isLetter: char -> bool
(** Detect upper and lower case ASCII alphabetic characters.

    {2 Examples}

    {[Char.isLetter 'a' = true]}

    {[Char.isLetter 'b' = true]}

    {[Char.isLetter 'E' = true]}

    {[Char.isLetter 'Y' = true]}

    {[Char.isLetter '0' = false]}

    {[Char.isLetter '-' = false]}
*)

val is_letter: char -> bool

val isDigit: char -> bool
(** Detect when a character is a number

    {2 Examples}

    {[Char.isDigit '0' = true]}

    {[Char.isDigit '1' = true]}

    {[Char.isDigit '9' = true]}

    {[Char.isDigit 'a' = false]}

    {[Char.isDigit 'b' = false]}
*)

val is_digit: char -> bool

val isAlphanumeric: char -> bool
(** Detect upper case, lower case and digit ASCII characters.

    {2 Examples}

    {[Char.isAlphanumeric 'a' = true]}

    {[Char.isAlphanumeric 'b' = true]}

    {[Char.isAlphanumeric 'E' = true]}

    {[Char.isAlphanumeric 'Y' = true]}

    {[Char.isAlphanumeric '0' = true]}

    {[Char.isAlphanumeric '7' = true]}

    {[Char.isAlphanumeric '-' = false]}
*)

val is_alphanumeric: char -> bool

val isPrintable: char -> bool
(** Detect if a character is a {{: https://en.wikipedia.org/wiki/ASCII#Printable_characters } printable } character

    A Printable character has a {!Char.toCode} in the range 32 to 127, inclusive ([' '] to ['~']).

    {2 Examples}

    {[Char.isPrintable 'G' = true]}

    {[Char.isPrintable '%' = true]}

    {[Char.isPrintable ' ' = true]}

    {[Char.isPrintable '\t' = false]}

    {[Char.isPrintable '\007' = false]}
*)

val is_printable: char -> bool

val isWhitespace: char -> bool
(** Detect one of the following characters:
    - ['\t'] (tab)
    - ['\n'] (newline)
    - ['\011'] (vertical tab)
    - ['\012'] (form feed)
    - ['\r'] (carriage return)
    - [' '] (space)

    {2 Examples}

    {[Char.isWhitespace '\t' = true]}

    {[Char.isWhitespace ' ' = true]}

    {[Char.isWhitespace '?' = false]}

    {[Char.isWhitespace 'G' = false]}
*)

val is_whitespace: char -> bool

(** {1 Modify} *)

val toLowercase: char -> char
(** Converts an ASCII character to lower case, preserving non alphabetic ASCII characters.

    {2 Examples}

    {[Char.toLowercase 'A' = 'a']}

    {[Char.toLowercase 'B' = 'b']}

    {[Char.toLowercase '7' = '7']} *)

val to_lowercase: char -> char

val toUppercase: char -> char
(** Convert an ASCII character to upper case, preserving non alphabetic ASCII characters.

    {2 Examples}

    {[toUppercase 'a' = 'A']}

    {[toUppercase 'b' = 'B']}

    {[toUppercase '7' = '7']}
*)

val to_uppercase: char -> char

(** {1 Convert} *)

val toCode: char -> int
(** Convert to the corresponding ASCII [code point][cp].

    [cp]: https://en.wikipedia.org/wiki/Code_point

    {2 Examples}

    {[Char.toCode 'A' = 65]}

    {[Char.toCode 'B' = 66]}
*)

val to_code: char -> int

val toString: char -> string
(** Convert a character into a string.

    {2 Examples}

    {[Char.toString 'A' = "A"]}

    {[Char.toString '{' = "{"]}

    {[Char.toString '7' = "7"]}
*)

val to_string: char -> string

val toDigit: char -> int option
(** Converts a digit character to its corresponding {!Int}.

    Returns [None] when the character isn't a digit.

    {2 Examples}

    {[Char.toDigit "7" = Some 7]}

    {[Char.toDigit "0" = Some 0]}

    {[Char.toDigit "A" = None]}

    {[Char.toDigit "" = None]}
*)

val to_digit: char -> int option

(** {1 Compare} *)

val equal: t -> t -> bool
(** Test two {!Char}s for equality *)

val compare: t -> t -> int
(** Compare two {!Char}s *)