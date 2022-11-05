open Prelude

module Msg = AppTypes.Msg
type msg = AppTypes.msg

type inputEvent = {
  data: option<string>,
  inputType: string,
}

let isInfixSymbol = (s: string): bool =>
  switch s {
  | "+" | "%" | "-" | "*" | "/" | "<" | ">" | "&" | "!" | "^" | "=" | "|" => true
  | _ => false
  }

let fromInputEvent = (evt: Dom.event): option<msg> => {
  open Tea.Json.Decoder
  let decoder = map2(
    (data, inputType) => {data: data, inputType: inputType},
    field("data", maybe(string)),
    field("inputType", string),
  )

  decodeEvent(decoder, Obj.magic(evt))
  |> Tea_result.result_to_option
  |> Option.andThen(~f=x =>
    switch x {
    | {inputType: "insertText", data: Some(" ")} =>
      /* space is bound up with autocomplete and a bunch of other stuff
       * that's hard to detangle, so it's still handled as a Keypress
       * for now (see FluidKeyboard) */
      None
    | {inputType: "insertText", data: Some(txt)} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(Msg.FluidMsg(FluidInputEvent(InsertText(txt))))
    | {inputType: "deleteContentBackward", _} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(FluidMsg(FluidInputEvent(DeleteContentBackward)))
    | {inputType: "deleteContentForward", _} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(FluidMsg(FluidInputEvent(DeleteContentForward)))
    | {inputType: "deleteWordBackward", _} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(FluidMsg(FluidInputEvent(DeleteWordBackward)))
    | {inputType: "deleteWordForward", _} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(FluidMsg(FluidInputEvent(DeleteWordForward)))
    // NB: Safari (incorrectly) fires deleteHardLine(Backward|Forward) for command-delete keystrokes
    | {inputType: "deleteSoftLineBackward", _} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(FluidMsg(FluidInputEvent(DeleteSoftLineBackward)))
    | {inputType: "deleteSoftLineForward", _} =>
      Webapi.Dom.Event.preventDefault(evt)
      Some(FluidMsg(FluidInputEvent(DeleteSoftLineForward)))
    | _ => None
    }
  )
}

let fromCompositionEndEvent = (evt: Web.Node.event): option<msg> => {
  open Tea.Json.Decoder
  decodeEvent(field("data", string), evt)
  |> Tea_result.result_to_option
  |> Option.map(~f=data => {
    Webapi.Dom.Event.preventDefault(Obj.magic(evt))
    Msg.FluidMsg(FluidInputEvent(InsertText(data)))
  })
}
