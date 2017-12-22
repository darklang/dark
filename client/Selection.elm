module Selection exposing (..)

-- builtins
import Maybe

-- lib
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Toplevel as TL
-- TODO: remove, code smell
import AST
import Pointer as P

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
  |> Maybe.andThen (TL.firstChild tl)
  |> ME.orElse (TL.rootOf tl)
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
  let tl = TL.getTL m tlid in
  case tl.data of
    TLHandler h ->
      let (p, replacement) = AST.deleteExpr cur h.ast in
      RPC ( [SetHandler tlid tl.pos { h | ast = replacement }]
          , FocusExact tlid p)
    _ -> NoChange

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
          else if tipe == Spec
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

