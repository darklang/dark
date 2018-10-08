module JSD = Json.Decode
module JSDP = Json.Decode.Pipeline
open Types

let decodeDarkKeyboardEvent =
  let toDEvent event selectionStart selectionEnd =
    {standard= event; selectionStart; selectionEnd}
  in
  JSDP.decode toDEvent
  |> JSDP.requiredAt [] Keyboard.Event.decodeKeyboardEvent
  |> JSDP.optionalAt
       ["target"; "selectionStart"]
       (JSD.map Just JSD.int) Nothing
  |> JSDP.optionalAt ["target"; "selectionEnd"] (JSD.map Just JSD.int) Nothing
