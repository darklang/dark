let registerGlobal eventName key tagger decoder =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn event =
      let open Tea_json.Decoder in
      match decodeEvent decoder event with
      | Tea_result.Error err ->
          Some (Types.EventDecoderError (eventName, key, err))
      | Tea_result.Ok data ->
          Some (tagger data)
    in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem eventName handler in
    fun () -> ignore (eventHandler_Unregister elem eventName cache)
  in
  Tea_sub.registration key enableCall


(* Same, but no JSON decoding *)
let registerGlobalDirect name key tagger =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn ev = Some (tagger (Obj.magic ev)) in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () -> ignore (eventHandler_Unregister elem name cache)
  in
  Tea_sub.registration key enableCall


module Window = struct
  external window_node : Web_node.t = "window" [@@bs.val]

  (** [registerListener eventName key decoder]
   * registers an event listener for the given [eventName]
   * under the bucklescript key [key] with the [decoder].
   * The decoder must be a Tea_json.Decoder decoder,
   * and must therefore be wrapped if using a decoder of
   * the form (Js.Json.t -> 'a).
   *
   * Example usage:
   * registerListener "mouseup" key (Decoders.wrapDecoder (Decoders.clickEvent constructor))
   *)
  let registerListener eventName key decoder =
    let open Vdom in
    let enableCall callbacks_base =
      let callbacks = ref callbacks_base in
      let fn event =
        let open Tea_json.Decoder in
        match decodeEvent decoder event with
        | Tea_result.Error err ->
            Some (Types.EventDecoderError (eventName, key, err))
        | Tea_result.Ok data ->
            Some data
      in
      let handler = EventHandlerCallback (key, fn) in
      let elem = window_node in
      let cache = eventHandler_Register callbacks elem eventName handler in
      fun () -> ignore (eventHandler_Unregister elem eventName cache)
    in
    Tea_sub.registration key enableCall


  module OnFocusChange = struct
    let decode =
      let open Tea.Json.Decoder in
      map (fun visible -> visible) (field "detail" bool)


    let listen ~key tagger =
      registerGlobal "windowFocusChange" key tagger decode
  end

  module Mouse = struct
    let ups ~key constructor =
      registerListener
        "mouseup"
        key
        (Decoders.wrapDecoder (Decoders.clickEvent constructor))
  end
end

module DisplayClientError = struct
  let decode =
    let open Tea.Json.Decoder in
    map (fun msg -> msg) (field "detail" string)


  let listen ~key tagger = registerGlobal "displayError" key tagger decode
end

module OnWheel = struct
  let decode =
    let open Tea.Json.Decoder in
    map2
      (* Handle inputs that might be ints or floats, see decodeClickEvent for
       * details, and
       * https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
       * for the spec *)
        (fun dX dY -> (dX |> truncate, dY |> truncate))
      (field "deltaX" Tea.Json.Decoder.float)
      (field "deltaY" Tea.Json.Decoder.float)


  let listen ~key tagger = registerGlobal "wheel" key tagger decode
end

module OnCaptureView = struct
  external _capture : unit -> unit = "capture"
    [@@bs.val] [@@bs.scope "window", "Dark", "view"]

  let capture (() : unit) : Types.msg Tea.Cmd.t =
    Tea_cmd.call (fun _ -> _capture ())


  let decode =
    let open Tea.Json.Decoder in
    map (fun msg -> msg) (field "detail" string)


  let listen ~key tagger = registerGlobal "captureView" key tagger decode
end

module DarkMouse = struct
  let moves ~key tagger =
    registerGlobal "mousemove" key tagger Tea.Mouse.position
end

module Clipboard = struct
  let copyListener ~key tagger = registerGlobalDirect "copy" key tagger

  let pasteListener ~key tagger = registerGlobalDirect "paste" key tagger

  let cutListener ~key tagger = registerGlobalDirect "cut" key tagger
end
