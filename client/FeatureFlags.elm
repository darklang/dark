module FeatureFlags exposing
    ( end
    , fromFlagged
    , start
    , toFlagged
    , toggle
    )

import Blank as B
import Dict
import Pointer as P
import Prelude exposing (..)
import Toplevel as TL
import Types exposing (..)


toFlagged : ID -> Expr -> Expr
toFlagged msgId expr =
    case expr of
        F id (FeatureFlag _ _ _ _) ->
            impossible ( "cant convert flagged to flagged", expr )

        _ ->
            F (gid ()) (FeatureFlag (Blank msgId) (B.new ()) expr (B.new ()))


fromFlagged : Pick -> Expr -> Expr
fromFlagged pick expr =
    case expr of
        F _ (FeatureFlag _ _ a b) ->
            case pick of
                PickA ->
                    a

                PickB ->
                    b

        _ ->
            impossible ( "cant convert flagged to flagged", expr )


start : Model -> Modification
start m =
    case unwrapCursorState m.cursorState of
        Selecting tlid (Just id) ->
            let
                tl =
                    TL.getTL m tlid

                pd =
                    TL.findExn tl id

                msgId =
                    gid ()

                newPd =
                    P.exprmap (toFlagged msgId) pd

                newTL =
                    TL.replace pd newPd tl
            in
            RPC
                ( [ SetHandler tl.id
                        tl.pos
                        (newTL |> TL.asHandler |> deMaybe "FF.start")
                  ]
                , FocusExact tl.id msgId
                )

        _ ->
            NoChange


end : Model -> ID -> Pick -> Modification
end m id pick =
    case tlidOf (unwrapCursorState m.cursorState) of
        Nothing ->
            NoChange

        Just tlid ->
            let
                tl =
                    TL.getTL m tlid

                pd =
                    TL.findExn tl id

                newPd =
                    P.exprmap (fromFlagged pick) pd

                newTL =
                    TL.replace pd newPd tl
            in
            RPC
                ( [ SetHandler tl.id
                        tl.pos
                        (newTL |> TL.asHandler |> deMaybe "FF.end")
                  ]
                , FocusExact tl.id (P.toID newPd)
                )


toggle : Model -> ID -> Bool -> Modification
toggle m id isExpanded =
    TweakModel
        (\m ->
            { m
                | featureFlags = Dict.insert (deID id) isExpanded m.featureFlags
            }
        )
