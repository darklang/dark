module Selection exposing (..)

-- builtins
import Maybe

-- lib
import Maybe.Extra as ME
import List.Extra as LE

-- dark
import Types exposing (..)
import Toplevel as TL
-- TODO: remove, code smell
import AST
import Pointer as P
import Util exposing (deMaybe)

nextToplevel : Model -> (Maybe TLID) -> Modification
nextToplevel m cur =
  let tls = List.map .id m.toplevels
      next =
        cur
        |> Maybe.andThen (\c -> LE.elemIndex c tls)
        |> Maybe.map ((+) 1)
        |> Maybe.map (\i -> i % List.length tls)
        |> Maybe.andThen (\i -> LE.getAt i tls)
        |> ME.orElse (m.toplevels |> (List.map .id) |> List.head)
  in
      case next of
        Just nextId -> Select nextId Nothing
        Nothing -> Deselect

upLevel : Model -> TLID -> (Maybe Pointer) -> Modification
upLevel m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.andThen (TL.getParentOf tl)
  |> Select tlid

downLevel : Model -> TLID -> (Maybe Pointer) -> Modification
downLevel m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> ME.orElse (TL.rootOf tl)
  |> Maybe.andThen (TL.firstChild tl)
  |> ME.orElse cur
  |> Select tlid

nextSibling : Model -> TLID -> (Maybe Pointer) -> Modification
nextSibling m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.map (TL.getNextSibling tl)
  |> ME.orElse cur
  |> Select tlid

previousSibling : Model -> TLID -> (Maybe Pointer) -> Modification
previousSibling m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.map (TL.getPrevSibling tl)
  |> ME.orElse cur
  |> Select tlid

nextBlank : Model -> TLID -> (Maybe Pointer) -> Modification
nextBlank m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> ME.filter P.isBlank
  |> TL.getNextBlank tl
  |> ME.orElse (TL.firstBlank tl)
  |> Select tlid

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
            downLevel m tlid (Just cur)
        _ -> NoChange
    _ -> Enter (Filling tlid cur)

