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
import AST
import Pointer as P
import Util exposing (deMaybe)

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
      wrap p op = RPC ([op], FocusExact tlid p)
      maybeH = \_ -> TL.asHandler tl |> deMaybe "delete maybe"
      id = P.idOf cur
  in
  case P.typeOf cur of
    DBColType ->
      wrap cur <| SetDBColType tlid id ""
    DBColName ->
      wrap cur <| SetDBColName tlid id ""
    VarBind ->
      let h = maybeH ()
          (p, replacement) = AST.deleteExpr cur h.ast
      in wrap p <| SetHandler tlid tl.pos { h | ast = replacement }
    HTTPVerb ->
      let h = maybeH ()
          (p, replacement) = TL.deleteHTTPVerbBlank cur h.spec
      in wrap p <| SetHandler tlid tl.pos { h | spec = replacement }
    HTTPRoute ->
      let h = maybeH ()
          (p, replacement) = TL.deleteHTTPRouteBlank cur h.spec
      in wrap p <| SetHandler tlid tl.pos { h | spec = replacement }
    Field ->
      let h = maybeH ()
          (p, replacement) = AST.deleteExpr cur h.ast
      in wrap p <| SetHandler tlid tl.pos { h | ast = replacement }
    Expr ->
      let h = maybeH ()
          (p, replacement) = AST.deleteExpr cur h.ast
      in wrap p <| SetHandler tlid tl.pos { h | ast = replacement }
    DarkType ->
      let h = maybeH ()
          (p, replacement) = SpecTypes.delete id h.spec
      in wrap p <| SetHandler tlid tl.pos { h | spec = replacement }
    DarkTypeField ->
      let h = maybeH ()
          (p, replacement) = SpecTypes.delete id h.spec
      in wrap p <| SetHandler tlid tl.pos { h | spec = replacement }

enter : Model -> TLID -> Pointer -> Modification
enter m tlid cur =
  let tl = TL.getTL m tlid in
  case cur of
    PFilled tipe _ ->
      let id = P.idOf cur in
      case tl.data of
        TLHandler h ->
          if AST.isLeaf id h.ast
          then
            let se = AST.subtree id h.ast in
            Many [ Enter (Filling tlid cur)
                 , AutocompleteMod (ACSetQuery (AST.toContent se))
                 ]
          else if tipe == HTTPVerb || tipe == HTTPRoute
          then
            let content =
                TL.getSpec h cur
                |> Maybe.andThen blankToMaybe
                |> Maybe.withDefault ""
            in
            Many [ Enter (Filling tlid cur)
                 , AutocompleteMod (ACSetQuery (content))
                 ]
          else
            selectDownLevel m tlid (Just cur)
        _ -> NoChange
    _ -> Enter (Filling tlid cur)

