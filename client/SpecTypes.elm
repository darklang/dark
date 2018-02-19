module SpecTypes exposing (..)

-- builtin

-- libs

-- dark
import Pointer as P
import Types exposing (..)

delete : ID -> HandlerSpec -> ID -> HandlerSpec
delete id spec newID =
  replace id (Blank id) spec

replace : ID -> BlankOr DarkType -> HandlerSpec -> HandlerSpec
replace id dt spec =
  { spec | types =
    { input = replaceInType id dt spec.types.input
    , output = replaceInType id dt spec.types.output
    }
  }

replaceInType : ID -> BlankOr DarkType -> BlankOr DarkType -> BlankOr DarkType
replaceInType origID newDt dt =
  if blankOrID dt == origID
  then newDt
  else
    case dt of
      Filled id (DTObj ts) ->
        Filled id (DTObj (List.map (\(n, t) ->
                           (n, replaceInType origID newDt t))
                           ts))
      _ -> dt


listInputPointers : SpecTypes -> List Pointer
listInputPointers st =
  listPointersInType st.input

listOutputPointers : SpecTypes -> List Pointer
listOutputPointers st =
  listPointersInType st.input

listPointers : SpecTypes -> List Pointer
listPointers st =
  listInputPointers st
  ++ listOutputPointers st

listPointersInType : BlankOr DarkType -> List Pointer
listPointersInType t =
  let nested =
        case t of
          Filled _ (DTObj ts) ->
            ts
            |> List.map (\(n, dt) -> [ P.blankTo DarkTypeField n
                                     , P.blankTo DarkType dt])
            |> List.concat
          _ -> []
  in
  [P.blankTo DarkType t]
  ++ nested

