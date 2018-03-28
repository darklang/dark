module SpecTypes exposing (..)

-- builtin

-- libs

-- dark
import Pointer as P
import Types exposing (..)
import Blank as B

delete : PointerData -> HandlerSpec -> ID -> HandlerSpec
delete pd spec newID =
  replace pd (P.emptyD_ newID (P.typeOf pd)) spec

replace : PointerData -> PointerData -> HandlerSpec -> HandlerSpec
replace p dt spec =
  { spec | types =
    { input = replaceInType p dt spec.types.input
    , output = replaceInType p dt spec.types.output
    }
  }

replaceInType : PointerData -> PointerData -> DarkType -> DarkType
replaceInType pd replacement dt =
  if B.toID dt == P.toID pd
  then
    case replacement of
      PDarkType t -> t
      _ -> dt
  else
    case dt of
      F id (DTObj ts) ->
        let newTs =
              ts
              |> List.map (\(n, t) ->
                   let newN = case replacement of
                                PDarkTypeField name ->
                                  if P.toID pd == B.toID n
                                  then name
                                  else n
                                _ -> n
                       newT = case replacement of
                                PDarkType tipe ->
                                  replaceInType pd replacement t
                                _ -> t
                   in (newN, newT))
        in F id (DTObj newTs)
      _ -> dt

allData : DarkType -> List PointerData
allData t =
  let nested =
        case t of
          F _ (DTObj ts) ->
            ts
            |> List.map (\(n, dt) -> PDarkTypeField n :: allData dt)
            |> List.concat
          _ -> []
  in
  PDarkType t :: nested


-- recurse until we find ID, then return the children
childrenOf : ID -> DarkType -> List PointerData
childrenOf id t =
  if B.toID t == id
  then
    case t of
      F _ (DTObj ts) ->
        ts
        |> List.map (\(n, dt) ->
                       [ PDarkTypeField n
                       , PDarkType dt])
        |> List.concat
      _ -> []
  else
    case t of
      F _ (DTObj ts) ->
        ts
        |> List.map (\(n, dt) -> childrenOf id dt)
        |> List.concat
      _ -> []



-- recurse until we find ID, then return the children
siblings : PointerData -> DarkType -> List PointerData
siblings p t =
  case t of
    F _ (DTObj ts) ->
      let result = ts
                   |> List.map (\(n, dt) ->
                                  [ PDarkTypeField n
                                  , PDarkType dt])
                   |> List.concat in
      if List.member p result
      then result
      else
        ts
        |> List.map (\(_, dt) -> siblings p dt)
        |> List.concat

    _ -> []


