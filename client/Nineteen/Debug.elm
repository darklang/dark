module Nineteen.Debug exposing (..)

{-| This should be removed for `Debug.todo` when in 0.19
-}
todo : String -> a
todo =
  Debug.crash


{-| This can be removed in favor of `Debug.toString` in 0.19
-}
toString : a -> String
toString =
  Basics.toString
