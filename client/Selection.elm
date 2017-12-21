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
  |> Maybe.map (\p -> Select tlid (Just p))
  |> Maybe.withDefault (Select tlid Nothing)

downLevel : Model -> TLID -> (Maybe Pointer) -> Modification
downLevel m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.andThen (TL.firstChild tl)
  |> Maybe.map (\c -> Select tlid (Just c))
  |> Maybe.withDefault (Select tlid (TL.rootOf tl))

nextSibling : Model -> TLID -> (Maybe Pointer) -> Modification
nextSibling m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.map (TL.getNextSibling tl)
  |> Maybe.map (\s -> Select tlid (Just s))
  |> Maybe.withDefault (Select tlid cur)

previousSibling : Model -> TLID -> (Maybe Pointer) -> Modification
previousSibling m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.map (TL.getPrevSibling tl)
  |> ME.or cur
  |> Select tlid

nextBlank : Model -> TLID -> (Maybe Pointer) -> Modification
nextBlank m tlid cur =
  let tl = TL.getTL m tlid in
  cur
  |> Maybe.andThen
    (\c ->
      case c of
        PFilled _ _ -> Nothing
        PBlank _ _ -> Just c)
  |> TL.getNextBlank tl
  |> ME.or (TL.firstBlank tl)
  |> Select tlid

delete : Model -> TLID -> Pointer -> Modification
delete m tlid cur =
  let tl = TL.getTL m tlid in
  case tl.data of
    TLHandler h ->
      let (p, replacement) = AST.deleteExpr (P.idOf cur) h.ast in
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
                |> Maybe.andThen (\s ->
                  case s of
                    Filled _ f -> Just f
                    _ -> Nothing)
                |> Maybe.withDefault ""
            in
            Many [ Enter (Filling tlid cur)
                 , AutocompleteMod (ACSetQuery (content))
                 ]
          else
            downLevel m tlid (Just cur)
        _ -> NoChange
    _ -> Enter (Filling tlid cur)

