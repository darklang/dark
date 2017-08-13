-- Taken from https://raw.githubusercontent.com/Gizra/elm-keyboard-event/master/examples/Window/Events.elm

effect module Window.Events where { subscription = MySub } exposing (onWindow)

{-| This module lets you subscribe to events received by the Javascript
`window` object.

@docs onWindow

-}

import Dict exposing (Dict)
import Dom.LowLevel
import Json.Decode exposing (Value, Decoder, value, decodeValue)
import Process
import Task exposing (Task)


{-| Listen for the given event name on the the `window` object.
Deocdes the event using the supplied decoder.
-}
onWindow : String -> Decoder msg -> Sub msg
onWindow eventName decoder =
    subscription (MySub eventName decoder)


type MySub msg
    = MySub String (Decoder msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub eventName decoder) =
    MySub eventName (Json.Decode.map func decoder)


type alias State msg =
    Dict String (Listener msg)


type alias Listener msg =
    { decoders : List (Decoder msg)
    , pid : Process.Id
    }


groupByEventName : List (MySub msg) -> Dict String (List (Decoder msg))
groupByEventName =
    let
        go (MySub eventName decoder) =
            Dict.update eventName (listify decoder)

        listify value =
            Maybe.map ((::) value)
                >> Maybe.withDefault [ value ]
                >> Just
    in
        List.foldl go Dict.empty


init : Task Never (State msg)
init =
    Task.succeed Dict.empty


type alias Msg =
    { eventName : String
    , event : Value
    }


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
    let
        leftStep _ { pid } task =
            -- We used to be listening for this event, but are not any longer.
            -- So, we should kill the process that was listening.
            Process.kill pid
                |> Task.andThen (always task)

        bothStep eventName { pid } decoders =
            -- We were listening for the event, and we're still listening to it.
            -- So, we just remember the decoders, so we can apply them when
            -- we get the event.
            Task.map (Dict.insert eventName (Listener decoders pid))

        rightStep eventName decoders =
            -- We weren't listening for this event, but now we are. So, we have
            -- to set up the process that listens for the event. We use the
            -- `value` decoder here to get the raw JSON value ... in `onSelfMsg`
            -- we can then apply each decoder and, depending on the result,
            -- send the message to the app.
            Task.andThen
                (\state ->
                    Process.spawn (Dom.LowLevel.onWindow eventName value (Platform.sendToSelf router << Msg eventName))
                        |> Task.andThen
                            (\pid ->
                                Task.succeed (Dict.insert eventName (Listener decoders pid) state)
                            )
                )
    in
        Dict.merge
            leftStep
            bothStep
            rightStep
            oldState
            (groupByEventName newSubs)
            (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router { eventName, event } state =
    case Dict.get eventName state of
        Just { decoders } ->
            let
                try decoder =
                    case decodeValue decoder event of
                        Ok msg ->
                            Just (Platform.sendToApp router msg)

                        Err err ->
                            Nothing
            in
                List.filterMap try decoders
                    |> Task.sequence
                    |> Task.andThen (always (Task.succeed state))

        Nothing ->
            Task.succeed state
