module Commands exposing (..)

-- builtins

-- libs

-- dark
-- import Prelude exposing (..)
import Types exposing (..)

executeCommand : Model -> TLID -> Maybe ID -> Maybe AutocompleteItem -> Modification
executeCommand m tlid mId highlighted =
  let successMods =
    case highlighted of
      Just (ACCommand command) ->
        [ Error ("Executing command: " ++ command) ]
      _ -> []
  in
      Many (successMods ++ [endCommandExecution m tlid mId])

endCommandExecution : Model -> TLID -> Maybe ID -> Modification
endCommandExecution m tlid mId =
  Many
    [ AutocompleteMod ACReset
    , Select tlid mId
    ]
