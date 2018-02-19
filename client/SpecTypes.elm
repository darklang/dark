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


allPointers : BlankOr DarkType -> List Pointer
allPointers t =
  let nested =
        case t of
          Filled _ (DTObj ts) ->
            ts
            |> List.map (\(n, dt) -> [ P.blankTo DarkTypeField n]
                                     ++ allPointers dt)
            |> List.concat
          _ -> []
  in
  [P.blankTo DarkType t]
  ++ nested

-- recurse until we find ID, then return the children
childrenOf : ID -> BlankOr DarkType -> List Pointer
childrenOf id t =
  if blankOrID t == id
  then
    case t of
      Filled _ (DTObj ts) ->
        ts
        |> List.map (\(n, dt) ->
                       [ P.blankTo DarkTypeField n
                       , P.blankTo DarkType dt])
        |> List.concat
      _ -> []
  else
    case t of
      Filled _ (DTObj ts) ->
        ts
        |> List.map (\(n, dt) -> childrenOf id dt)
        |> List.concat
      _ -> []



-- recurse until we find ID, then return the children
siblings : Pointer -> BlankOr DarkType -> List Pointer
siblings p t =
  case t of
    Filled _ (DTObj ts) ->
      let result = ts
                   |> List.map (\(n, dt) ->
                                  [ P.blankTo DarkTypeField n
                                  , P.blankTo DarkType dt])
                   |> List.concat in
      if List.member p result
      then result
      else
        ts
        |> List.map (\(_, dt) -> siblings p dt)
        |> List.concat

    _ -> []

