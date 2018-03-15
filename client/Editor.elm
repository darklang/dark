module Editor exposing (..)

-- Dark

import Defaults
import Json.Decode as JSD
import RPC
import Types exposing (..)


editor2model : Editor -> Model
editor2model e =
    let
        m =
            Defaults.defaultModel
    in
    case e.clipboard of
        Nothing ->
            m

        Just enc ->
            case JSD.decodeValue RPC.decodePointerData enc of
                Ok pd ->
                    { m | clipboard = Just pd }

                Err err ->
                    m


model2editor : Model -> Editor
model2editor m =
    case m.clipboard of
        Nothing ->
            { clipboard = Nothing }

        Just pd ->
            { clipboard = Just <| RPC.encodePointerData pd }
