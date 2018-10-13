type key_event =
  { key_code : int
  ; shift : bool
  ; ctrl : bool
  ; alt : bool
  ; meta : bool
  }

let key_event =
  let open Tea.Json.Decoder in
  map5
    (fun key_code shift ctrl alt meta ->
      {key_code; shift; ctrl; alt; meta})
    (field "keyCode" int)
    (field "shiftKey" bool)
    (field "ctrlKey" bool)
    (field "altKey" bool)
    (field "metaKey" bool)

let registerGlobal name key tagger =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn = fun ev ->
      let open Tea_json.Decoder in
      let open Tea_result in
      match decodeEvent key_event ev with
      | Error _ -> None
      | Ok pos -> Some (tagger pos) in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () ->
      let _ = eventHandler_Unregister elem name cache in
      ()
  in Tea_sub.registration key enableCall

let downs ?(key="") tagger =
  registerGlobal "keydown" key tagger

let nullEvent () =
  { key_code = 0
  ; shift = false
  ; ctrl = false
  ; alt = false
  ; meta = false
  }

let stringify key_event =
  Printf.sprintf "%i + %s + %s + %s + %s"
    key_event.key_code
    (if key_event.shift then "SHIFT" else "")
    (if key_event.ctrl then "CTRL" else "")
    (if key_event.alt then "ALT" else "")
    (if key_event.meta then "META" else "")

let isLoc key_event =
  key_event.key_code = 76 && key_event.shift = true && key_event.meta = true

let isError key_event =
  key_event.key_code = 69 && key_event.ctrl = true
