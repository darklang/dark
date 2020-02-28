open Tc

(* NOTE: this is an exact copy of Decoders.clickEvent,
 * but Decoders indirectly depends on Native via Utils,
 * so we can't easily use Decoders.clickEvent.
 *)
let clickEvent (fn : Types.mouseEvent -> 'a) j : 'a =
  fn
    { mePos =
        (* We decode floats b/c newer Chromes may use floats instead of ints; we
         * then truncate rather than moving to floats everywhere due to concerns
         * about sending data back to browsers whose DOMs don't support float
         * positions - see https://github.com/darklang/dark/pull/2016 for
         * discussion, and
         * https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
         * for the spec *)
        { vx = Json.Decode.field "pageX" Json.Decode.float j |> truncate
        ; vy = Json.Decode.field "pageY" Json.Decode.float j |> truncate }
    ; button = Json.Decode.field "button" Json.Decode.int j
    ; ctrlKey = Json.Decode.field "ctrlKey" Json.Decode.bool j
    ; shiftKey = Json.Decode.field "shiftKey" Json.Decode.bool j
    ; altKey = Json.Decode.field "altKey" Json.Decode.bool j
    ; detail = Json.Decode.field "detail" Json.Decode.int j }


(* NOTE: this is an exact copy of Decoders.wrapDecoder,
 * but Decoders indirectly depends on Native via Utils,
 * so we can't easily use Decoders.wrapDecoder.
 *)
let wrapDecoder (fn : Js.Json.t -> 'a) : (Js.Json.t, 'a) Tea.Json.Decoder.t =
  let open Json.Decode in
  Decoder
    (fun value ->
      try Tea_result.Ok (fn value)
      with e ->
        Unshared.reportError "undecodable json" value ;
        ( match e with
        | DecodeError e | Json.ParseError e ->
            Tea_result.Error e
        | e ->
            Tea_result.Error ("Json error: " ^ Printexc.to_string e) ))


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


type rect =
  { id : string
  ; top : int
  ; left : int
  ; right : int
  ; bottom : int }

exception NativeCodeError of string

module Ext = struct
  let window : Dom.window =
    [%bs.raw "(typeof window === undefined) ? window : {}"]


  external _querySelector : string -> Dom.element Js.Nullable.t
    = "querySelector"
    [@@bs.val] [@@bs.scope "document"]

  let querySelector (s : string) : Dom.element option =
    Js.Nullable.toOption (_querySelector s)


  external scrollHeight : Dom.element -> int = "scrollHeight" [@@bs.get]

  external clientWidth : Dom.element -> int = "clientWidth" [@@bs.get]

  external clientHeight : Dom.element -> int = "clientHeight" [@@bs.get]

  external getBoundingClientRect : Dom.element -> Dom.domRect
    = "getBoundingClientRect"
    [@@bs.send]

  external rectTop : Dom.domRect -> float = "top" [@@bs.get]

  external rectBottom : Dom.domRect -> float = "bottom" [@@bs.get]

  external rectRight : Dom.domRect -> float = "right" [@@bs.get]

  external rectLeft : Dom.domRect -> float = "left" [@@bs.get]

  external rectHeight : Dom.domRect -> float = "height" [@@bs.get]

  external rectWidth : Dom.domRect -> float = "width" [@@bs.get]

  let staticHost : unit -> string = [%bs.raw "function(){ return staticUrl; }"]

  external offsetTop : Dom.element -> int = "offsetTop" [@@bs.get]

  let getBoundingClient (e : Dom.element) (s : string) : rect =
    let client = getBoundingClientRect e in
    { id = s
    ; top = rectTop client |> int_of_float
    ; left = rectLeft client |> int_of_float
    ; right = rectRight client |> int_of_float
    ; bottom = rectBottom client |> int_of_float }


  external redirect : string -> unit = "replace"
    [@@bs.val] [@@bs.scope "window", "location"]
end

module OffsetEstimator = struct
  (* Takes a mouse event, ostensibly a `click` inside an BlankOr with id `elementID`
   * and produces an 0-based integer offset from the beginning of the BlankOrs content where
   * the click occurred on the DOM.
   *
   * ie. if the DOM element has "foobar" and the user clicks between the `o` and the `b`
   * the return value should be `4`.
   *
   * TODO: It's a super hacky estimate based on our common screen size at Dark and the default
   * font size and should be replaced with a proper implementation. But it's done us
   * okay so far. *)
  let estimateClickOffset (elementID : string) (event : Types.mouseEvent) :
      int option =
    match Js.Nullable.toOption (Web_document.getElementById elementID) with
    | Some elem ->
        let rect = elem##getBoundingClientRect () in
        if event.mePos.vy >= int_of_float rect##top
           && event.mePos.vy <= int_of_float rect##bottom
           && event.mePos.vx >= int_of_float rect##left
           && event.mePos.vx <= int_of_float rect##right
        then Some ((event.mePos.vx - int_of_float rect##left) / 8)
        else None
    | None ->
        None
end

module Random = struct
  let random () : int = Js_math.random_int 0 2147483647

  let range (min : int) (max : int) : int = Js_math.random_int min max
end

module Location = struct
  external queryString : string = "search"
    [@@bs.val] [@@bs.scope "window", "location"]

  external hashString : string = "hash"
    [@@bs.val] [@@bs.scope "window", "location"]

  external reload : bool -> unit = "reload"
    [@@bs.val] [@@bs.scope "window", "location"]

  (* TODO write string query parser *)
end

module Window = struct
  external viewportWidth : int = "innerWidth" [@@bs.val] [@@bs.scope "window"]

  external viewportHeight : int = "innerHeight" [@@bs.val] [@@bs.scope "window"]

  external pageWidth : int = "outerWidth" [@@bs.val] [@@bs.scope "window"]

  external pageHeight : int = "outerHeight" [@@bs.val] [@@bs.scope "window"]

  external openUrl : string -> string -> unit = "open"
    [@@bs.val] [@@bs.scope "window"]

  external window_node : Web_node.t = "window" [@@bs.val]

  (** [registerListener eventName key decoder]
   * registers an event listener for the given [eventName]
   * under the bucklescript key [key] with the [decoder].
   * The decoder must be a Tea_json.Decoder decoder,
   * and must therefore be wrapped if using a decoder of
   * the form (Js.Json.t -> 'a).
   *
   * Example usage:
   * registerListener "mouseup" key (wrapDecoder (clickEvent constructor))
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
      registerListener "mouseup" key (wrapDecoder (clickEvent constructor))
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

  external copyToClipboard : string -> unit = "clipboard-copy" [@@bs.module]
end

module BigInt = struct
  type t

  (* asUintNExn throws an exception when given stringified non-ints and truncates the most significant bits
     of numbers with magnitude too large to be represented in the given # of bits *)
  external asUintNExn : int -> string -> t = "asUintN"
    [@@bs.val] [@@bs.scope "BigInt"]

  let asUintN ~(nBits : int) (str : string) : t Option.t =
    try Some (asUintNExn nBits str) with _ -> None


  external toString : t -> string = "toString" [@@bs.send]
end

module Decoder = struct
  let tuple2 decodeA decodeB =
    let open Tea.Json.Decoder in
    Decoder
      (fun j ->
        match Web.Json.classify j with
        | JSONArray arr ->
            if Js_array.length arr == 2
            then
              match
                ( decodeValue decodeA (Caml.Array.unsafe_get arr 0)
                , decodeValue decodeB (Caml.Array.unsafe_get arr 1) )
              with
              | Ok a, Ok b ->
                  Ok (a, b)
              | Error e1, _ ->
                  Error ("tuple2[0] -> " ^ e1)
              | _, Error e2 ->
                  Error ("tuple2[1] -> " ^ e2)
            else Error "tuple2 expected array with 2 elements"
        | _ ->
            Error "tuple2 expected array")


  let wireIdentifier =
    let open Tea.Json.Decoder in
    Decoder
      (fun j ->
        match decodeValue string j with
        | Ok s ->
            Ok s
        | Error _ ->
            Ok (Js.Json.stringify j))
end

module Url = struct
  type t =
    < hash : string
    ; host : string
    ; hostname : string
    ; href : string
    ; origin : string
    ; password : string
    ; pathname : string
    ; port : string
    ; protocol : string
    ; search : string
    ; username : string >
    Js.t

  external make_internal : string -> t = "URL" [@@bs.new]

  let make s = try Some (make_internal s) with _ -> None
end
