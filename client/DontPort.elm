module DontPort exposing (..)

import Regex

fromInt : Int -> String
fromInt i =
  toString i

fromFloat : Float -> String
fromFloat f =
  toString f

(@) : List a -> List a -> List a
(@) a b = a ++ b

(^) : String -> String -> String
(^) a b = a ++ b

floatAdd : Float -> Float -> Float
floatAdd a b = a + b

floatDivide : Float -> Float -> Float
floatDivide a b = a / b

replace : String -> String -> String -> String
replace re repl str =
  Regex.replace Regex.All (Regex.regex re) (\_ -> repl) str

