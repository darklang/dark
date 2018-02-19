module SpecTypes exposing (..)

-- builtin

-- libs

-- dark
import Types exposing (..)

delete : ID -> HandlerSpec -> ID -> HandlerSpec
delete id spec newID =
  Debug.crash "todo delete spectypes"

deleteInType : ID -> DarkType -> ID -> DarkType
deleteInType id dt newID =
  Debug.crash "todo delete in type"

replace : ID -> DarkType -> HandlerSpec -> HandlerSpec
replace id dt spec =
  { spec | types =
    { input = replaceInType id dt spec.types.input
    , output = replaceInType id dt spec.types.output
    }
  }

replaceInType : ID -> DarkType -> BlankOr DarkType -> BlankOr DarkType
replaceInType origID newDt dt =
  if blankOrID dt == origID
  then Filled origID newDt
  else
    case dt of
      Filled id (DTObj ts) ->
        Filled id (DTObj (List.map (\(n, t) ->
                           (n, replaceInType origID newDt t))
                           ts))
      _ -> dt
