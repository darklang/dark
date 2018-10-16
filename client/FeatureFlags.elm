module FeatureFlags exposing (..)

-- built-in
import IntDict

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
          focus = FocusExact tl.id msgId
      in case newTL.data of
        TLHandler h -> RPC ([SetHandler tl.id tl.pos h], focus)
        TLFunc f -> RPC ([SetFunction f], focus)
        _ -> NoChange
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
          focus = FocusExact tl.id (P.toID newPd)
      in case newTL.data of
        TLHandler h -> RPC ([SetHandler tl.id tl.pos h], focus)
        TLFunc f -> RPC ([SetFunction f], focus)
        _ -> NoChange

toggle : Model -> ID ->  Bool -> Modification
toggle m id isExpanded =
  TweakModel (\m_ -> {
    m_ | featureFlags = IntDict.insert (deID id) isExpanded m_.featureFlags
  })
