module Nineteen.String exposing (..)

{-| Can be replaced by `String.fromInt` in 0.19
-}
fromInt : Int -> String
fromInt i =
  toString i


{-| Can be replaced by `String.fromFloat` in 0.19
-}
fromFloat : Float -> String
fromFloat f =
  toString f
