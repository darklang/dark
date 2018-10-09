module DontPort exposing (..)

import Regex

fromInt : Int -> String
fromInt i =
  toString i

fromFloat : Float -> String
fromFloat f =
  toString f

deMaybe : String -> Maybe a -> a
deMaybe msg x =
  case x of
    Just y -> y
    Nothing -> Debug.crash ("something impossible occurred: got Nothing but expected something" ++ toString msg)

replace : String -> String -> String -> String
replace re repl str =
  Regex.replace Regex.All (Regex.regex re) (\_ -> repl) str
