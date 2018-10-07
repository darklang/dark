module DontPort exposing (..)

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


