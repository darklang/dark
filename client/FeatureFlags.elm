module FeatureFlags exposing (..)

-- built-in
import Dict

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Toplevel as TL
import Pointer as P
import Blank as B

toFlagged : ID -> Expr -> Expr
toFlagged msgId expr =
  case expr of
    F id (FeatureFlag _ _ _ _) ->
      impossible ("cant convert flagged to flagged", expr)
    _ ->
      F (gid ()) (FeatureFlag (Blank msgId) (B.new ()) expr (B.new ()))

fromFlagged : Pick -> Expr -> Expr
fromFlagged pick expr =
  case expr of
    F _ (FeatureFlag _ _ a b) ->
      case pick of
        PickA -> a
        PickB -> b
    _ -> impossible ("cant convert flagged to flagged", expr)

start : Model -> Modification
start m =
  case unwrapCursorState m.cursorState of
    Selecting tlid (Just id) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
          msgId = gid ()
          newPd = P.exprmap (toFlagged msgId) pd
          newTL = TL.replace pd newPd tl
      in
      RPC ([SetHandler tl.id tl.pos
                       (newTL |> TL.asHandler |> deMaybe "FF.start") ]
           , FocusExact tl.id msgId)
    _ -> NoChange

end : Model -> ID -> Pick -> Modification
end m id pick =
  case tlidOf (unwrapCursorState m.cursorState) of
    Nothing -> NoChange
    Just tlid ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
          newPd = P.exprmap (fromFlagged pick) pd
          newTL = TL.replace pd newPd tl
      in
      RPC ([SetHandler tl.id tl.pos
                       (newTL |> TL.asHandler |> deMaybe "FF.end") ]
           , FocusExact tl.id (P.toID newPd))

toggle : Model -> BlankOr String -> Bool -> Modification
toggle m name is =
  case name of
    Blank id ->
      ToggleUnnamedFF id is
    F id str -> ToggleNamedFF str id is

updateVisibility : Model -> String -> ID -> Bool -> Model
updateVisibility m name nid is =
  let id = deID nid
      fm = Dict.get name m.featureFlags
  in case fm of
    Just meta ->
      { m |
        featureFlags = Dict.insert name {
          meta | instances =  Dict.insert id is meta.instances
        } m.featureFlags
      }
    Nothing ->
      let instances = Dict.singleton id is
          meta = FlagMeta (B.new ()) instances
      in { m | featureFlags = Dict.insert name meta m.featureFlags }