open Js_of_ocaml

type js_string = Js.js_string Js.t
type js_uint8Array = Typed_array.uint8Array Js.t
type js_arrayBuffer = Typed_array.arrayBuffer Js.t

external dark_targetjs_digest384 :
  js_string -> js_string
  = "dark_targetjs_digest384"

external dark_targetjs_digest384_uint8Array :
  js_uint8Array -> js_string
  = "dark_targetjs_digest384_bytes"

external dark_arrayBuffer_to_b64 :
  js_uint8Array -> js_string
  = "dark_arrayBuffer_to_b64"

external dark_arrayBuffer_from_b64 :
  js_string -> js_arrayBuffer
  = "dark_arrayBuffer_from_b64"

let _bytes_to_uint8Array (input : Bytes.t) : js_uint8Array =
  let len = Bytes.length input in
  let buf = new%js Typed_array.uint8Array len in
  for i = 0 to len-1 do
    (Typed_array.set buf i (int_of_char (Bytes.get input i)))
  done;
  buf

let _bytes_from_uint8Array (input : js_arrayBuffer) : Bytes.t =
  let len = input##.byteLength in
  let bytes = Bytes.create len in
  let reader = (new%js Typed_array.uint8Array_fromBuffer input) in
  for i = 0 to len-1 do
    let char = (Typed_array.unsafe_get reader i) in
    (Firebug.console##log input);
    (Firebug.console##log char);
    (Firebug.console##log i);
    Bytes.unsafe_set bytes i (char_of_int char)
  done;
  bytes


let digest384 (input : string) : string =
  input |> Js.string |> dark_targetjs_digest384 |> Js.to_string

let digest384_bytes (input : Bytes.t) : string =
  input |> _bytes_to_uint8Array |> dark_targetjs_digest384_uint8Array |> Js.to_string

let base64_bytes (input : Bytes.t) : string =
  input |> _bytes_to_uint8Array |> dark_arrayBuffer_to_b64 |> Js.to_string

let bytes_from_base64 (b64 : string) : Bytes.t = 
  b64
  |> Js.string
  |> dark_arrayBuffer_from_b64
  |> _bytes_from_uint8Array

external dark_targetjs_digest256 :
  js_string -> js_string
  = "dark_targetjs_digest256"

let digest256 (input : string) : string =
  input |> Js.string |> dark_targetjs_digest256 |> Js.to_string

let regexp_replace ~(pattern : string) ~(replacement : string) (str : string) :
    string =
  Regexp.global_replace (Regexp.regexp pattern) str replacement

let string_split ~(sep : string) (s : string) : string list =
  (* Although this uses a different regex engine than the server side, we only
   * match on an exact string, so this should work fine. *)
  Regexp.split (Regexp.regexp_string sep) s
