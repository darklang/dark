module Selection exposing (..)

-- builtins
import Maybe

-- lib
import Maybe.Extra as ME
import List.Extra as LE

-- dark
import Types exposing (..)
import Toplevel as TL
import SpecTypes
import SpecHeaders
import AST
import Analysis
import Pointer as P
import Util exposing (deMaybe)

moveCursorBackInTime : Model -> TLID -> Modification
moveCursorBackInTime m selected =
  let maxCursor = List.length (Analysis.getAnalysisResults m selected)
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
        |> Maybe.andThen (\c -> LE.elemIndex c tls)
        |> Maybe.map (\x -> x + 1)
        |> Maybe.map (\i -> i % List.length tls)
        |> Maybe.andThen (\i -> LE.getAt i tls)
        |> ME.orElse (m.toplevels |> (List.map .id) |> List.head)
  in
      case next of
        Just nextId -> Select nextId Nothing
        Nothing -> Deselect

selectPrevToplevel : Model -> (Maybe TLID) -> Modification
selectPrevToplevel m cur =
  let tls = List.map .id m.toplevels
      next =
        cur
        |> Maybe.andThen (\c -> LE.elemIndex c tls)
        |> Maybe.map (\x -> x - 1)
        |> Maybe.map (\i -> i % List.length tls)
        |> Maybe.andThen (\i -> LE.getAt i tls)
        |> ME.orElse (m.toplevels |> (List.map .id) |> LE.last)
  in
      case next of
        Just nextId -> Select nextId Nothing
        Nothing -> Deselect



selectUpLevel : Model -> TLID -> (Maybe Pointer) -> Modification
selectUpLevel m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.andThen (TL.getParentOf tl)
  |> Select tlid

selectDownLevel : Model -> TLID -> (Maybe Pointer) -> Modification
selectDownLevel m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> ME.orElse (TL.rootOf tl)
  |> Maybe.andThen (TL.firstChild tl)
  |> ME.orElse cur
  |> Select tlid

selectNextSibling : Model -> TLID -> (Maybe Pointer) -> Modification
selectNextSibling m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.map (TL.getNextSibling tl)
  |> ME.orElse cur
  |> Select tlid

selectPreviousSibling : Model -> TLID -> (Maybe Pointer) -> Modification
selectPreviousSibling m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.map (TL.getPrevSibling tl)
  |> ME.orElse cur
  |> Select tlid

getPrevBlank : Model -> TLID -> (Maybe Pointer) -> Maybe Pointer
getPrevBlank m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> ME.filter P.isBlank
  |> Maybe.andThen (TL.getPrevBlank tl)
  |> ME.orElse (TL.lastBlank tl)

getNextBlank : Model -> TLID -> (Maybe Pointer) -> Maybe Pointer
getNextBlank m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> ME.filter P.isBlank
  |> TL.getNextBlank tl
  |> ME.orElse (TL.firstBlank tl)

selectNextBlank : Model -> TLID -> (Maybe Pointer) -> Modification
selectNextBlank m tlid cur =
  cur
  |> getNextBlank m tlid
  |> Select tlid

enterNextBlank : Model -> TLID -> (Maybe Pointer) -> Modification
enterNextBlank m tlid cur =
  cur
  |> getNextBlank m tlid
  |> Maybe.map (\p -> Enter (Filling tlid p))
  |> Maybe.withDefault NoChange

selectPrevBlank : Model -> TLID -> (Maybe Pointer) -> Modification
selectPrevBlank m tlid cur =
  cur
  |> getPrevBlank m tlid
  |> Select tlid

enterPrevBlank : Model -> TLID -> (Maybe Pointer) -> Modification
enterPrevBlank m tlid cur =
  cur
  |> getPrevBlank m tlid
  |> Maybe.map (\p -> Enter (Filling tlid p))
  |> Maybe.withDefault NoChange



delete : Model -> TLID -> Pointer -> Modification
delete m tlid cur =
  let tl = TL.getTL m tlid
      newID = gid ()
      newP = PBlank (P.typeOf cur) newID
      wrapH newH = RPC ([SetHandler tlid tl.pos newH], FocusExact tlid newP)
      wrapDB op = RPC ([op], FocusExact tlid cur)
      maybeH = \_ -> TL.asHandler tl |> deMaybe "delete maybe"
      id = P.idOf cur
  in
  case P.typeOf cur of
    DBColType ->
      wrapDB <| SetDBColType tlid id ""
    DBColName ->
      wrapDB <| SetDBColName tlid id ""
    VarBind ->
      let h = maybeH ()
          replacement = AST.deleteExpr cur h.ast newID
      in wrapH { h | ast = replacement }
    HTTPVerb ->
      let h = maybeH ()
          replacement = SpecHeaders.deleteHTTPVerbBlank cur h.spec newID
      in wrapH { h | spec = replacement }
    HTTPRoute ->
      let h = maybeH ()
          replacement = SpecHeaders.deleteHTTPRouteBlank cur h.spec newID
      in wrapH { h | spec = replacement }
    Field ->
      let h = maybeH ()
          replacement = AST.deleteExpr cur h.ast newID
      in wrapH { h | ast = replacement }
    Expr ->
      let h = maybeH ()
          replacement = AST.deleteExpr cur h.ast newID
      in wrapH { h | ast = replacement }
    DarkType ->
      let h = maybeH ()
          replacement = SpecTypes.delete cur h.spec newID
      in wrapH { h | spec = replacement }
    DarkTypeField ->
      let h = maybeH ()
          replacement = SpecTypes.delete cur h.spec newID
      in wrapH { h | spec = replacement }

enter : Model -> TLID -> Pointer -> Modification
enter m tlid cur =
  let tl = TL.getTL m tlid in
  case cur of
    PBlank _ _ -> Enter (Filling tlid cur)
    PFilled tipe _ ->
      let id = P.idOf cur
          h () = (tl |> TL.asHandler |> deMaybe "selection enter")
      in
      if TL.getChildrenOf tl cur /= []
      then selectDownLevel m tlid (Just cur)
      else
        let text =
          case P.ownerOf cur of
            POAst ->
              (h ()).ast
              |> AST.subtree id
              |> AST.toContent
              |> Just
            POSpecHeader ->
              SpecHeaders.find (h ()) cur
              |> Maybe.andThen blankToMaybe
              |> Maybe.withDefault ""
              |> Just
            _ -> Nothing
        in
          case text of
            Just content ->
              Many [ Enter (Filling tlid cur)
                   , AutocompleteMod (ACSetQuery content)
                   ]
            Nothing -> NoChange

