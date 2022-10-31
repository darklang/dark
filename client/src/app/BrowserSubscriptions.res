let registerGlobal = (eventName, key, tagger, decoder) => {
  open Vdom
  let enableCall = callbacks_base => {
    let callbacks = ref(callbacks_base)
    let fn = event => {
      open Tea_json.Decoder
      switch decodeEvent(decoder, event) {
      | Error(err) => Some(AppTypes.Msg.EventDecoderError(eventName, key, err))
      | Ok(data) => Some(tagger(data))
      }
    }

    let handler = EventHandlerCallback(key, fn)
    let eventTarget = Webapi.Dom.document |> Webapi.Dom.Document.asEventTarget
    let cache = eventHandler_Register(callbacks, eventTarget, eventName, handler)
    () => ignore(eventHandler_Unregister(eventTarget, eventName, cache))
  }

  Tea_sub.registration(key, enableCall)
}

// Same, but no JSON decoding
let registerGlobalDirect = (name, key, tagger) => {
  open Vdom
  let enableCall = callbacks_base => {
    let callbacks = ref(callbacks_base)
    let fn = ev => Some(tagger(Obj.magic(ev)))
    let handler = EventHandlerCallback(key, fn)
    let eventTarget = Webapi.Dom.document |> Webapi.Dom.Document.asEventTarget
    let cache = eventHandler_Register(callbacks, eventTarget, name, handler)
    () => ignore(eventHandler_Unregister(eventTarget, name, cache))
  }

  Tea_sub.registration(key, enableCall)
}

module Window = {
  @ocaml.doc(" [registerListener eventName key decoder]
   * registers an event listener for the given [eventName]
   * under the rescript key [key] with the [decoder].
   * The decoder must be a Tea_json.Decoder decoder,
   * and must therefore be wrapped if using a decoder of
   * the form (Js.Json.t -> 'a).
   *
   * Example usage:
   * registerListener \"mouseup\" key (Decoders.wrapDecoder (Decoders.clickEvent constructor))
   ")
  let registerListener = (eventName, key, decoder) => {
    open Vdom
    let enableCall = callbacks_base => {
      let callbacks = ref(callbacks_base)
      let fn = event => {
        open Tea_json.Decoder
        switch decodeEvent(decoder, event) {
        | Error(err) => Some(AppTypes.Msg.EventDecoderError(eventName, key, err))
        | Ok(data) => Some(data)
        }
      }

      let handler = EventHandlerCallback(key, fn)
      let eventTarget = Webapi.Dom.window->Webapi.Dom.Window.asEventTarget
      let cache = eventHandler_Register(callbacks, eventTarget, eventName, handler)
      () => ignore(eventHandler_Unregister(eventTarget, eventName, cache))
    }

    Tea_sub.registration(key, enableCall)
  }

  module OnFocusChange = {
    let decode = {
      open Tea.Json.Decoder
      map(visible => visible, field("detail", bool))
    }

    let listen = (~key, tagger) => registerGlobal("windowFocusChange", key, tagger, decode)
  }

  module Mouse = {
    let ups = (~key, constructor) =>
      registerListener(
        "mouseup",
        key,
        Decoders.wrapDecoder(Json.Decode.map(constructor, MouseEvent.decode)),
      )
  }
}

module DisplayClientError = {
  let decode = {
    open Tea.Json.Decoder
    map(msg => msg, field("detail", string))
  }

  let listen = (~key, tagger) => registerGlobal("displayError", key, tagger, decode)
}

module OnWheel = {
  let decode = {
    open Tea.Json.Decoder
    map2(/* Handle inputs that might be ints or floats, see decodeClickEvent for
     * details, and
     * https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
     * for the spec */
    (dX, dY) => (
      dX |> truncate,
      dY |> truncate,
    ), field("deltaX", Tea.Json.Decoder.float), field("deltaY", Tea.Json.Decoder.float))
  }

  let listen = (~key, tagger) => registerGlobal("wheel", key, tagger, decode)
}

module DarkMouse = {
  let moves = (~key, tagger) => registerGlobal("mousemove", key, tagger, Tea.Mouse.position)
}

module Clipboard = {
  let copyListener = (~key, tagger) => registerGlobalDirect("copy", key, tagger)

  let pasteListener = (~key, tagger) => registerGlobalDirect("paste", key, tagger)

  let cutListener = (~key, tagger) => registerGlobalDirect("cut", key, tagger)
}
