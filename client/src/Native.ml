open Tc

let registerGlobal name key tagger decoder =
  let open Vdom in
  let enableCall callbacks_base =
    let callbacks = ref callbacks_base in
    let fn ev =
      let open Tea_json.Decoder in
      let open Tea_result in
      match decodeEvent decoder ev with
      | Error _ ->
          None
      | Ok pos ->
          Some (tagger pos)
    in
    let handler = EventHandlerCallback (key, fn) in
    let elem = Web_node.document_node in
    let cache = eventHandler_Register callbacks elem name handler in
    fun () ->
      let _ = eventHandler_Unregister elem name cache in
      ()
  in
  Tea_sub.registration key enableCall


module PageVisibility = struct
  type visibility =
    | Hidden
    | Visible
  [@@deriving show]
end

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


  external getWidth : Dom.window -> int = "innerWidth" [@@bs.get]

  external getHeight : Dom.window -> int = "innerHeight" [@@bs.get]

  external astPositions : string -> jsRectArr = "positions"
    [@@bs.val] [@@bs.scope "window", "Dark", "ast"]

  type blankOrId = string

  external findCaretXPos : unit -> int = "findCaretXPos"
    [@@bs.val] [@@bs.scope "window", "Dark", "caret"]

  external findLogicalOffset : blankOrId -> int -> int = "findLogicalOffset"
    [@@bs.val] [@@bs.scope "window", "Dark", "caret"]

  external moveCaretLeft : unit -> bool = "moveCaretLeft"
    [@@bs.val] [@@bs.scope "window", "Dark", "caret"]

  external moveCaretRight : unit -> bool = "moveCaretRight"
    [@@bs.val] [@@bs.scope "window", "Dark", "caret"]
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

  (* TODO write string query parser *)
end

module Window = struct
  let size () : size =
    {width = Ext.getWidth Ext.window; height = Ext.getHeight Ext.window}


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

  module OnFocusChange = struct
    let decode =
      let open Tea.Json.Decoder in
      map (fun visible -> visible) (field "detail" bool)


    let listen ~key tagger =
      registerGlobal "windowFocusChange" key tagger decode
  end
end

module Base64 = struct
  let encode (str : string) : string = Webapi.Base64.btoa str

  let decode (b64 : string) : (string, string) Result.t =
    try Ok (Webapi.Base64.atob b64) with e -> Error (Printexc.to_string e)
end

module Rollbar = struct
  external rollbarError :
    string -> string Js.nullable -> 'a -> 'a -> Js.Json.t -> unit
    = "error"
    [@@bs.val] [@@bs.scope "window", "Rollbar"]

  let send (msg : string) (url : string option) (custom : Js.Json.t) : unit =
    let url = Js.Nullable.fromOption url in
    rollbarError msg url Js.null Js.null custom
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
                ( decodeValue decodeA (Array.unsafe_get arr 0)
                , decodeValue decodeB (Array.unsafe_get arr 1) )
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
                ( decodeValue decodeA (Array.unsafe_get arr 0)
                , decodeValue decodeB (Array.unsafe_get arr 1)
                , decodeValue decodeC (Array.unsafe_get arr 2) )
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
