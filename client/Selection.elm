module Selection exposing (..)

-- builtins
import Maybe

-- dark
import Types exposing (..)
import Toplevel as TL

upLevel : Model -> TLID -> (Maybe ID) -> CurrentThread -> Modification
upLevel m tlid cur thread =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.andThen (TL.getParentOf tl)
      |> Maybe.map (\p -> Select tlid (Just p) thread)
      |> Maybe.withDefault (Select tlid cur thread)

downLevel : Model -> TLID -> (Maybe ID) -> CurrentThread -> Modification
downLevel m tlid cur thread =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.andThen (TL.firstChild tl)
      |> Maybe.map (\c -> Select tlid (Just c) thread)
      |> Maybe.withDefault (Select tlid (TL.rootOf tl) thread)

nextSibling : Model -> TLID -> (Maybe ID) -> CurrentThread -> Modification
nextSibling m tlid cur thread =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.map (TL.getNextSibling tl)
      |> Maybe.map (\s -> Select tlid (Just s) thread)
      |> Maybe.withDefault (Select tlid cur thread)

previousSibling : Model -> TLID -> (Maybe ID) -> CurrentThread -> Modification
previousSibling m tlid cur thread =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.map (TL.getPrevSibling tl)
      |> Maybe.map (\s -> Select tlid (Just s) thread)
      |> Maybe.withDefault (Select tlid cur thread)

nextHole : Model -> TLID -> (Maybe ID) -> CurrentThread -> Modification
nextHole m tlid cur thread =
  let tl = TL.getTL m tlid
  in
      cur
      |> Maybe.andThen
        (\c ->
          case TL.holeType tl c of
            NotAHole -> Nothing
            _ -> Just c)
      |> TL.getNextHole tl
      |> Maybe.map (\h -> Select tlid (Just h) thread)
      |> Maybe.withDefault (Select tlid cur thread)

enter : Model -> TLID -> ID -> CurrentThread -> Modification
enter m tlid cur thread =
  let tl = TL.getTL m tlid
  in
    case TL.holeType tl cur of
      NotAHole -> NoChange
      _ -> Enter False (Filling tlid cur) thread

