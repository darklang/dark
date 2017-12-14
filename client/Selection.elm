module Selection exposing (..)

-- builtins
import Maybe

-- dark
import Types exposing (..)
import Toplevel as TL
-- TODO: remove, code smell
import AST

upLevel : Model -> TLID -> (Maybe ID) -> Modification
upLevel m tlid cur =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.andThen (TL.getParentOf tl)
      |> Maybe.map (\p -> Select tlid (Just p))
      |> Maybe.withDefault (Select tlid Nothing)

downLevel : Model -> TLID -> (Maybe ID) -> Modification
downLevel m tlid cur =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.andThen (TL.firstChild tl)
      |> Maybe.map (\c -> Select tlid (Just c))
      |> Maybe.withDefault (Select tlid (TL.rootOf tl))

nextSibling : Model -> TLID -> (Maybe ID) -> Modification
nextSibling m tlid cur =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.map (TL.getNextSibling tl)
      |> Maybe.map (\s -> Select tlid (Just s))
      |> Maybe.withDefault (Select tlid cur)

previousSibling : Model -> TLID -> (Maybe ID) -> Modification
previousSibling m tlid cur =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.map (TL.getPrevSibling tl)
      |> Maybe.map (\s -> Select tlid (Just s))
      |> Maybe.withDefault (Select tlid cur)

nextHole : Model -> TLID -> (Maybe ID) -> Modification
nextHole m tlid cur =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.andThen
        (\c ->
          case TL.holeType tl c of
            NotAHole -> Nothing
            _ -> Just c)
      |> TL.getNextHole tl
      |> Maybe.map (\h -> Select tlid (Just h))
      |> Maybe.withDefault (Select tlid (TL.firstHole tl))

delete : Model -> TLID -> ID -> Modification
delete m tlid cur =
  let tl = TL.getTL m tlid in
  case tl.data of
    TLHandler h ->
      let (id, replacement) = AST.deleteExpr cur h.ast
      in
      RPC ([SetHandler tlid tl.pos { h | ast = replacement }], FocusExact tlid id)
    _ -> NoChange

enter : Model -> TLID -> ID -> Modification
enter m tlid cur =
  let tl = TL.getTL m tlid
  in
    case TL.holeType tl cur of
      NotAHole ->
        case tl.data of
          TLHandler h ->
            if AST.isLeaf cur h.ast
            then
              let se = AST.subtree cur h.ast in
              Many [ Enter False (Filling tlid cur)
                   , AutocompleteMod (ACSetQuery (AST.toContent se))
                   ]
            else
              downLevel m tlid (Just cur)
          _ -> NoChange
      _ -> Enter False (Filling tlid cur)

