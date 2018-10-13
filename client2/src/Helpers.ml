external get_element : (string -> Dom.element) = "getElementById" [@@bs.val][@@bs.scope "document"]

let registerGlobal name key tagger decoder =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn = fun ev ->
      let open Tea_json.Decoder in
      let open Tea_result in
      match decodeEvent decoder ev with
      | Error _ -> None
      | Ok pos -> Some (tagger pos) in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () ->
      let _ = eventHandler_Unregister elem name cache in
      ()
  in Tea_sub.registration key enableCall
