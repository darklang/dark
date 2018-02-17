module SpecTypes exposing (..)

-- builtin

-- libs

-- dark
import Types exposing (..)

delete : ID -> HandlerSpec -> (Pointer, HandlerSpec)
delete id h =
  Debug.crash "todo delete"

deleteInType : ID -> DarkType -> Maybe (Pointer, DarkType)
deleteInType id dt =
  Debug.crash "todo delete in type"

