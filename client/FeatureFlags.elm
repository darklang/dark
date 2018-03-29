module FeatureFlags exposing (..)

-- dark
import Types exposing (..)
import Toplevel as TL
import Pointer as P
import Blank as B
import Util exposing (deMaybe)

toFlagged : BlankOr a -> BlankOr a
toFlagged bo =
  case bo of
    Flagged _ _ _ _ _ -> Debug.crash "cant convert flagged to flagged"
    _ -> Flagged (gid()) (B.new ()) 0 bo (B.new ())


start : Model -> Modification
start m =
  case unwrapCursorState m.cursorState of
    Selecting tlid (Just id) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
          newPd = pd
                  |> P.strmap (\_ a -> toFlagged a)
                  |> P.dtmap toFlagged
                  |> P.exprmap toFlagged
          newTL = TL.replace pd newPd tl
      in
      RPC ([SetHandler tl.id tl.pos
                       (newTL |> TL.asHandler |> deMaybe "FF.start") ]
           , FocusExact tl.id (P.toID newPd))
    _ -> NoChange

