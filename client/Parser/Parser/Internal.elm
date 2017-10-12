module Parser.Internal exposing
  ( Parser(..)
  , Step(..)
  , State
  , chomp
  , chompDigits
  , chompDotAndExp
  , isBadIntEnd
  )


import Char
import ParserPrimitives as Prim



-- PARSERS


type Parser ctx x a =
  Parser (State ctx -> Step ctx x a)


type Step ctx x a
  = Good a (State ctx)
  | Bad x (State ctx)


type alias State ctx =
  { source : String
  , offset : Int
  , indent : Int
  , context : List ctx
  , row : Int
  , col : Int
  }



-- CHOMPERS


chomp : (Char -> Bool) -> Int -> String -> Int
chomp isGood offset source =
  let
    newOffset =
      Prim.isSubChar isGood offset source
  in
    if newOffset < 0 then
      offset

    else
      chomp isGood newOffset source



-- CHOMP DIGITS


chompDigits : (Char -> Bool) -> Int -> String -> Result Int Int
chompDigits isValidDigit offset source =
  let
    newOffset =
      chomp isValidDigit offset source
  in
    -- no digits
    if newOffset == offset then
      Err newOffset

    -- ends with non-digit characters
    else if Prim.isSubChar isBadIntEnd newOffset source /= -1 then
      Err newOffset

    -- all valid digits!
    else
      Ok newOffset


isBadIntEnd : Char -> Bool
isBadIntEnd char =
  Char.isDigit char
  || Char.isUpper char
  || Char.isLower char
  || char == '.'



-- CHOMP FLOAT STUFF


chompDotAndExp : Int -> String -> Result Int Int
chompDotAndExp offset source =
  let
    dotOffset =
      Prim.isSubChar isDot offset source
  in
    if dotOffset == -1 then
      chompExp offset source

    else
      chompExp (chomp Char.isDigit dotOffset source) source


isDot : Char -> Bool
isDot char =
  char == '.'


chompExp : Int -> String -> Result Int Int
chompExp offset source =
  let
    eOffset =
      Prim.isSubChar isE offset source
  in
    if eOffset == -1 then
      Ok offset

    else
      let
        opOffset =
          Prim.isSubChar isPlusOrMinus eOffset source

        expOffset =
          if opOffset == -1 then eOffset else opOffset
      in
        if Prim.isSubChar isZero expOffset source /= -1 then
          Err expOffset

        else if Prim.isSubChar Char.isDigit expOffset source == -1 then
          Err expOffset

        else
          chompDigits Char.isDigit expOffset source


isE : Char -> Bool
isE char =
  char == 'e' || char == 'E'


isZero : Char -> Bool
isZero char =
  char == '0'


isPlusOrMinus : Char -> Bool
isPlusOrMinus char =
  char == '+' || char == '-'

