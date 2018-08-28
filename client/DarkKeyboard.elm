module DarkKeyboard exposing (decodeDarkKeyboardEvent)

import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
import Keyboard.Event
import Types exposing (..)


decodeDarkKeyboardEvent : JSD.Decoder DarkKeyboardEvent
decodeDarkKeyboardEvent =
    let
        toDEvent event selectionStart selectionEnd =
            { standard = event
            , selectionStart = selectionStart
            , selectionEnd = selectionEnd
            }
    in
    JSDP.decode toDEvent
        |> JSDP.requiredAt [] Keyboard.Event.decodeKeyboardEvent
        |> JSDP.optionalAt [ "target", "selectionStart" ] (JSD.map Just JSD.int) Nothing
        |> JSDP.optionalAt [ "target", "selectionEnd" ] (JSD.map Just JSD.int) Nothing
