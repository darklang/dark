open Tc

let registerGlobal name key tagger decoder =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn ev =
      let open Tea_json.Decoder in
      match decodeEvent decoder ev with
      | Tea_result.Error err ->
          Some (Types.EventDecoderError (name, key, err))
      | Tea_result.Ok pos ->
          Some (tagger pos)
    in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () -> ignore (eventHandler_Unregister elem name cache)
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


type size =
  { width : int
  ; height : int }

type rect =
  { id : string
  ; top : int
  ; left : int
  ; right : int
  ; bottom : int }

type list_pos =
  { atoms : rect list
  ; nested : rect list }

type jsRect = string Js.Dict.t

type jsRectArr = jsRect array Js.Dict.t

exception NativeCodeError of string

module Ext = struct
  let window : Dom.window =
    [%bs.raw "(typeof window === undefined) ? window : {}"]


  external astPositions : string -> jsRectArr = "positions"
    [@@bs.val] [@@bs.scope "window", "Dark", "ast"]

  external _querySelector :
    string -> Dom.element Js.Nullable.t
    = "querySelector"
    [@@bs.val] [@@bs.scope "document"]

  external scrollHeight : Dom.element -> int = "scrollHeight" [@@bs.get]

  external clientWidth : Dom.element -> int = "clientWidth" [@@bs.get]

  external clientHeight : Dom.element -> int = "clientHeight" [@@bs.get]

  external getBoundingClientRect :
    Dom.element -> Dom.domRect
    = "getBoundingClientRect"
    [@@bs.send]

  external rectTop : Dom.domRect -> float = "top" [@@bs.get]

  external rectBottom : Dom.domRect -> float = "bottom" [@@bs.get]

  let querySelector (s : string) : Dom.element option =
    Js.Nullable.toOption (_querySelector s)


  external windowWidth : Dom.window -> int = "innerWidth" [@@bs.get]

  external windowHeight : Dom.window -> int = "innerHeight" [@@bs.get]

  external offsetTop : Dom.element -> int = "offsetTop" [@@bs.get]

  let windowSize : int * int = (windowWidth window, windowHeight window)
end

module Random = struct
  let random () : int = Js_math.random_int 0 2147483647
end

module Size = struct
  let _convert (key : string) (pos : jsRectArr) : rect list =
    Js.Dict.unsafeGet pos key
    |> Belt.List.fromArray
    |> List.map ~f:(fun jsRect ->
           { id = Js.Dict.unsafeGet jsRect "id"
           ; top = int_of_string (Js.Dict.unsafeGet jsRect "top")
           ; left = int_of_string (Js.Dict.unsafeGet jsRect "left")
           ; right = int_of_string (Js.Dict.unsafeGet jsRect "right")
           ; bottom = int_of_string (Js.Dict.unsafeGet jsRect "bottom") } )


  let positions (tlid : string) : list_pos =
    let pos = Ext.astPositions tlid in
    {atoms = _convert "atoms" pos; nested = _convert "nested" pos}
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
  module OnResize = struct
    let decode =
      let open Tea.Json.Decoder in
      let decodeDetail =
        map2
          (fun width height -> (width, height))
          (field "width" int)
          (field "height" int)
      in
      map (fun msg -> msg) (field "detail" decodeDetail)


    let listen ~key tagger = registerGlobal "windowResize" key tagger decode
  end

  module OnLoad = struct
    let decode =
      let open Tea.Json.Decoder in
      let decodeDetail =
        map2
          (fun width height -> (width, height))
          (field "width" int)
          (field "height" int)
      in
      map (fun msg -> msg) (field "detail" decodeDetail)


    let listen ~key tagger = registerGlobal "windowOnload" key tagger decode
  end

  module OnFocusChange = struct
    let decode =
      let open Tea.Json.Decoder in
      map (fun visible -> visible) (field "detail" bool)


    let listen ~key tagger =
      registerGlobal "windowFocusChange" key tagger decode
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
    map2 (fun dX dY -> (dX, dY)) (field "deltaX" int) (field "deltaY" int)


  let listen ~key tagger = registerGlobal "wheel" key tagger decode
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
            Error "tuple2 expected array" )


  let pair = tuple2

  let tuple3 decodeA decodeB decodeC =
    let open Tea.Json.Decoder in
    Decoder
      (fun j ->
        match Web.Json.classify j with
        | JSONArray arr ->
            if Js_array.length arr == 3
            then
              match
                ( decodeValue decodeA (Caml.Array.unsafe_get arr 0)
                , decodeValue decodeB (Caml.Array.unsafe_get arr 1)
                , decodeValue decodeC (Caml.Array.unsafe_get arr 2) )
              with
              | Ok a, Ok b, Ok c ->
                  Ok (a, b, c)
              | Error e1, _, _ ->
                  Error ("tuple3[0] -> " ^ e1)
              | _, Error e2, _ ->
                  Error ("tuple3[1] -> " ^ e2)
              | _, _, Error e3 ->
                  Error ("tuple3[2] -> " ^ e3)
            else Error "tuple3 expected array with 3 elements"
        | _ ->
            Error "tuple3 expected array" )


  let triple = tuple3

  let tuple4 decodeA decodeB decodeC decodeD =
    let open Tea.Json.Decoder in
    Decoder
      (fun j ->
        match Web.Json.classify j with
        | JSONArray arr ->
            if Js_array.length arr == 4
            then
              match
                ( decodeValue decodeA (Caml.Array.unsafe_get arr 0)
                , decodeValue decodeB (Caml.Array.unsafe_get arr 1)
                , decodeValue decodeC (Caml.Array.unsafe_get arr 2)
                , decodeValue decodeD (Caml.Array.unsafe_get arr 3) )
              with
              | Ok a, Ok b, Ok c, Ok d ->
                  Ok (a, b, c, d)
              | Error e1, _, _, _ ->
                  Error ("tuple4[0] -> " ^ e1)
              | _, Error e2, _, _ ->
                  Error ("tuple4[1] -> " ^ e2)
              | _, _, Error e3, _ ->
                  Error ("tuple4[2] -> " ^ e3)
              | _, _, _, Error e4 ->
                  Error ("tuple4[3] -> " ^ e4)
            else Error "tuple4 expected array with 4 elements"
        | _ ->
            Error "tuple4 expected array" )


  let wireIdentifier =
    let open Tea.Json.Decoder in
    Decoder
      (fun j ->
        match decodeValue string j with
        | Ok s ->
            Ok s
        | Error _ ->
            Ok (Js.Json.stringify j) )
end
