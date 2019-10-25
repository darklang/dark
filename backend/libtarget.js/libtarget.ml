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

external dark_arrayBuffer_to_padded_b64url :
  js_uint8Array -> js_string
  = "dark_arrayBuffer_to_padded_b64url"

(* dark_arrayBuffer_from_padded_b64url silently accepts non-base64 strings,
interpreting out-of-alphabet chars as 0 *)
external dark_arrayBuffer_from_padded_b64url :
  js_string -> js_arrayBuffer
  = "dark_arrayBuffer_from_padded_b64url"

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

let base64url_bytes (input : Bytes.t) : string =
  input |> _bytes_to_uint8Array |> dark_arrayBuffer_to_padded_b64url |> Js.to_string

(* The input here MUST be a b64 string as implemented;
   out-of-alphabet chars will map to 0 *)
let bytes_from_base64url (b64 : string) : Bytes.t = 
  b64
  |> Js.string
  |> dark_arrayBuffer_from_padded_b64url
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
