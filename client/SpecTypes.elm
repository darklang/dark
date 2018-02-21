module SpecTypes exposing (..)

-- builtin

-- libs

-- dark
import Pointer as P
import Types exposing (..)

delete : Pointer -> HandlerSpec -> ID -> HandlerSpec
delete p spec newID =
  replace p (P.emptyD (P.typeOf p) newID) spec

replace : Pointer -> PointerData -> HandlerSpec -> HandlerSpec
replace p dt spec =
  { spec | types =
    { input = replaceInType p dt spec.types.input
    , output = replaceInType p dt spec.types.output
    }
  }

replaceInType : Pointer -> PointerData -> BlankOr DarkType -> BlankOr DarkType
replaceInType p replacement dt =
  if blankOrID dt == (P.idOf p)
  then
    case replacement of
      PDarkType _ t -> t
      _ -> dt
  else
    case dt of
      Filled id (DTObj ts) ->
        let newTs =
              ts
              |> List.map (\(n, t) ->
                   let newN = case replacement of
                                PDarkTypeField _ name ->
                                  if P.idOf p == blankOrID n
                                  then name
                                  else n
                                _ -> n
                       newT = case replacement of
                                PDarkType _ tipe ->
                                  replaceInType p replacement t
                                _ -> t
                   in (newN, newT))
        in Filled id (DTObj newTs)
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


