module Sync exposing
    ( fetch
    , markRequestInModel
    , markResponseInModel
    , markTickInModel
    , timedOut
    , toAnalyse
    )

import List.Extra as LE
import RPC
import Random
import Toplevel
import Types exposing (..)
import Util


markRequestInModel : Model -> Model
markRequestInModel m =
    let
        oldSyncState =
            m.syncState
    in
    { m
        | syncState =
            { oldSyncState | inFlight = True, ticks = 0 }
    }


markTickInModel : Model -> Model
markTickInModel m =
    let
        oldSyncState =
            m.syncState
    in
    { m
        | syncState =
            { oldSyncState | ticks = oldSyncState.ticks + 1 }
    }


markResponseInModel : Model -> Model
markResponseInModel m =
    let
        oldSyncState =
            m.syncState
    in
    { m
        | syncState =
            { oldSyncState | inFlight = False, ticks = 0 }
    }


timedOut : SyncState -> Bool
timedOut s =
    (s.ticks % 10) == 0 && s.ticks /= 0


fetch : Model -> ( Model, Cmd Msg )
fetch m =
    if
        not m.syncState.inFlight
            || timedOut m.syncState
    then
        let
            ( newModel, tlids ) =
                toAnalyse m
        in
        ( markRequestInModel newModel, RPC.getAnalysisRPC tlids )

    else
        markTickInModel m ! []


toAnalyse : Model -> ( Model, List TLID )
toAnalyse m =
    case m.cursorState of
        Selecting tlid _ ->
            ( m, [ tlid ] )

        Entering (Filling tlid _) ->
            ( m, [ tlid ] )

        Dragging tlid _ _ _ ->
            ( m, [ tlid ] )

        _ ->
            let
                ( randomNum, nextSeed ) =
                    Random.step Util.randomInt m.seed

                ids =
                    List.map .id (Toplevel.all m)

                index =
                    let
                        length =
                            List.length ids
                    in
                    if length > 0 then
                        Just (randomNum % length)

                    else
                        Nothing
            in
            index
                |> Maybe.andThen (\i -> LE.getAt i ids)
                |> Maybe.map (\e -> [ e ])
                |> Maybe.withDefault []
                |> (\a -> ( { m | seed = nextSeed }, a ))
