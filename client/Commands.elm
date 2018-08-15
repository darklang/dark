module Commands exposing (..)

-- builtins

-- libs

-- dark
-- import Prelude exposing (..)
import Types exposing (..)

executeCommand : Model -> TLID -> Maybe ID -> String -> Modification
executeCommand m tlid mId command =
  Select tlid mId
