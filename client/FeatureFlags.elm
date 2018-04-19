module FeatureFlags exposing (..)

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Toplevel as TL
import Pointer as P
import Blank as B

toFlagged : BlankOr a -> BlankOr a
toFlagged bo =
  case bo of
    Flagged _ _ _ _ _ ->
      impossible ("cant convert flagged to flagged", bo)
    _ -> Flagged (gid()) (B.new ()) 0 bo (B.new ())

fromFlagged : BlankOr a -> BlankOr a
fromFlagged bo =
  case bo of
    Flagged _ _ setting a b ->
      case setting of
        0 -> a
        100 -> b
        _ -> bo
    _ -> impossible ("cant convert flagged to flagged", bo)

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

moveSlider : Int -> BlankOr a -> BlankOr a
moveSlider newSetting bo =
  case bo of
    Flagged id msg _ l r -> Flagged id msg newSetting l r
    _ -> impossible ("should only be called on slider", bo)

commitSlider : Model -> ID -> Modification
commitSlider m id =
  case tlidOf (unwrapCursorState m.cursorState) of
    Nothing -> NoChange
    Just tlid ->
      let tl = TL.getTL m tlid in
      RPC ([SetHandler tl.id tl.pos (tl
                                     |> TL.asHandler
                                     |> deMaybe "FF.updateSlider")]
           , FocusSame)

updateSlider : Model -> ID -> String -> Modification
updateSlider m id val =
  case String.toFloat val of
    Result.Err err -> Error err
    Result.Ok asFloat ->
      case tlidOf (unwrapCursorState m.cursorState) of
        Nothing -> NoChange
        Just tlid ->
          let tl = TL.getTL m tlid
              pd = TL.findExn tl id
              move = moveSlider (round asFloat)
              newPd = pd
                      |> P.strmap (\_ a -> move a)
                      |> P.dtmap move
                      |> P.exprmap move
              newTL = TL.replace pd newPd tl
          in
          TweakModel (\m -> TL.upsert m newTL)
