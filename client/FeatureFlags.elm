module FeatureFlags exposing (..)

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

fromFlagged : Expr -> Expr
fromFlagged expr =
  case expr of
    F id (FeatureFlag msg cond a b) ->
      a -- TODO: how to decide
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

end : Model -> ID -> Modification
end m id =
  case tlidOf (unwrapCursorState m.cursorState) of
    Nothing -> NoChange
    Just tlid->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
          newPd = P.exprmap fromFlagged pd
          newTL = TL.replace pd newPd tl
      in
      RPC ([SetHandler tl.id tl.pos
                       (newTL |> TL.asHandler |> deMaybe "FF.end") ]
           , FocusExact tl.id (P.toID newPd))


