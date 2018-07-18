module FeatureFlags exposing (..)

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Toplevel as TL
import Pointer as P
import Blank as B

toFlagged : ID -> BlankOr a -> BlankOr a
toFlagged msgId bo =
  case bo of
    Flagged _ _ _ _ _ ->
      impossible ("cant convert flagged to flagged", bo)
    _ -> Flagged (gid()) (Blank msgId) (B.new ()) bo (B.new ())

fromFlagged : BlankOr a -> BlankOr a
fromFlagged bo =
  case bo of
    Flagged _ _ setting a b ->
      a -- TODO: how to decide
    _ -> impossible ("cant convert flagged to flagged", bo)

start : Model -> Modification
start m =
  case unwrapCursorState m.cursorState of
    Selecting tlid (Just id) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
          msgId = gid ()
          newPd = pd
                  |> P.strmap (\_ a -> toFlagged msgId a)
                  |> P.dtmap (toFlagged msgId)
                  |> P.exprmap (toFlagged msgId)
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
          newPd = pd
                  |> P.strmap (\_ a -> fromFlagged a)
                  |> P.dtmap fromFlagged
                  |> P.exprmap fromFlagged
          newTL = TL.replace pd newPd tl
      in
      RPC ([SetHandler tl.id tl.pos
                       (newTL |> TL.asHandler |> deMaybe "FF.end") ]
           , FocusExact tl.id (P.toID newPd))


