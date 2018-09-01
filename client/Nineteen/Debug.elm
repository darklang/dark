module Nineteen.Debug exposing (..)

{-| This should be removed for `Debug.todo` when in 0.19
-}
todo : String -> a
todo =
  Debug.crash
