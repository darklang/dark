module Selection exposing (..)

-- builtins
import Maybe

-- lib
import Maybe.Extra as ME
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Toplevel as TL
import Analysis
import Pointer as P
import Util exposing (deMaybe)

moveCursorBackInTime : Model -> TLID -> Modification
moveCursorBackInTime m selected =
  let maxCursor = List.length (Analysis.getAnalysisResults m selected) - 1
  in
    selected
    |> TL.getTL m
    |> (\tl -> SetCursor tl.id (min (tl.cursor + 1) maxCursor))

moveCursorForwardInTime : Model -> TLID -> Modification
moveCursorForwardInTime m selected =
  selected
  |> TL.getTL m
  |> (\tl -> SetCursor tl.id (max (tl.cursor - 1) 0))

selectNextToplevel : Model -> (Maybe TLID) -> Modification
selectNextToplevel m cur =
  let tls = List.map .id m.toplevels
      next =
        cur
        |> Maybe.andThen (\cur -> Util.listNextWrap cur tls)
  in
  case next of
    Just nextId -> Select nextId Nothing
    Nothing -> Deselect

selectPrevToplevel : Model -> (Maybe TLID) -> Modification
selectPrevToplevel m cur =
  let tls = List.map .id m.toplevels
      next =
        cur
        |> Maybe.andThen (\cur -> Util.listPreviousWrap cur tls)
  in
  case next of
    Just nextId -> Select nextId Nothing
    Nothing -> Deselect



selectUpLevel : Model -> TLID -> (Maybe ID) -> Modification
selectUpLevel m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> Maybe.andThen (TL.getParentOf tl)
  |> Maybe.map P.toID
  |> Select tlid

selectDownLevel : Model -> TLID -> (Maybe ID) -> Modification
selectDownLevel m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> ME.orElse (TL.rootOf tl)
  |> Maybe.andThen (TL.firstChild tl)
  |> ME.orElse pd
  |> Maybe.map P.toID
  |> Select tlid

selectNextSibling : Model -> TLID -> (Maybe ID) -> Modification
selectNextSibling m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> Maybe.map (TL.getNextSibling tl)
  |> ME.orElse pd
  |> Maybe.map P.toID
  |> Select tlid

selectPreviousSibling : Model -> TLID -> (Maybe ID) -> Modification
selectPreviousSibling m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> Maybe.map (TL.getPrevSibling tl)
  |> ME.orElse pd
  |> Maybe.map P.toID
  |> Select tlid

selectNextBlank : Model -> TLID -> (Maybe ID) -> Modification
selectNextBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getNextBlank tl
  |> Maybe.map P.toID
  |> Select tlid

enterNextBlank : Model -> TLID -> (Maybe ID) -> Modification
enterNextBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getNextBlank tl
  |> Maybe.map (\pd -> Enter (Filling tlid (P.toID pd)))
  |> Maybe.withDefault NoChange

selectPrevBlank : Model -> TLID -> (Maybe ID) -> Modification
selectPrevBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getPrevBlank tl
  |> Maybe.map P.toID
  |> Select tlid

enterPrevBlank : Model -> TLID -> (Maybe ID) -> Modification
enterPrevBlank m tlid cur =
  let tl = TL.getTL m tlid
      pd = Maybe.map (TL.findExn tl) cur
  in
  pd
  |> TL.getPrevBlank tl
  |> Maybe.map (\pd -> Enter (Filling tlid (P.toID pd)))
  |> Maybe.withDefault NoChange



delete : Model -> TLID -> Maybe ID -> Modification
delete m tlid mId =
  case mId of
    Nothing ->
      let tl = TL.getTL m tlid
      in
      case tl.data of
        TLHandler _ -> Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
        TLDB _ -> Many [ RPC ([DeleteTL tlid], FocusNothing), Deselect ]
        TLFunc _ -> Error ("Cannot delete functions!")
    Just id ->
      let newID = gid ()
          wrapDB op = RPC ([op], FocusExact tlid newID)
          tl = TL.getTL m tlid
          pd = TL.findExn tl id
      in
      case P.typeOf pd of
        DBColType ->
          wrapDB <| SetDBColType tlid id ""
        DBColName ->
          wrapDB <| SetDBColName tlid id ""
        _ ->
          let newTL = TL.delete tl pd newID in
              case newTL.data of
                TLHandler h -> RPC ([SetHandler tlid tl.pos h], FocusExact tlid newID)
                TLFunc f -> RPC ([SetFunction f], FocusExact tlid newID)
                TLDB _ -> Debug.crash "pointer type mismatch - Selection.delete"


enter : Model -> TLID -> ID -> Modification
enter m tlid id =
  let tl = TL.getTL m tlid
      pd = TL.findExn tl id
  in
  if TL.getChildrenOf tl pd /= []
  then selectDownLevel m tlid (Just id)
  else
    case pd of
      pd -> Many [ Enter (Filling tlid id)
                 , AutocompleteMod (ACSetQuery (P.toContent pd))
                 ]

