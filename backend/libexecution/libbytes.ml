open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : expr fn list =
  [ { prefix_names = ["Bytes::base64Encode"]
    ; infix_names = []
    ; parameters = [par "bytes" TBytes]
    ; return_type = TStr
    ; description =
        "Base64URL encodes `bytes` with `=` padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
    ; func =
        InProcess
          (function
          | _, [DBytes bytes] ->
              Dval.dstr_of_string_exn (Libtarget.base64url_bytes bytes)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bytes::base64Decode"]
    ; infix_names = []
    ; parameters = [par "str" TStr]
    ; return_type = TResult
    ; description =
        "Base64URL decodes `bytes` with `=` padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
    ; func =
        InProcess
          (function
          | _, [DStr str] ->
              let str = str |> Unicode_string.to_string in
              ( try
                  str
                  |> Libtarget.bytes_from_base64url
                  |> fun bytes -> DResult (ResOk (DBytes bytes))
                with
              (* This Not_found case should only happen if we get a string
               * with an invalid alphabet, and we check for that in Libtarget
               * and raise an Invalid_B64 exception. *)
              (* | (Not_found[@ocaml.warning "-3"]) -> *)
              | Libtarget.Invalid_B64 s ->
                  DResult (ResError (Dval.dstr_of_string_exn s))
              | e ->
                  Libcommon.Log.erroR
                    "Not valid Base64 input"
                    ~params:[("exn", Exn.to_string e)] ;
                  DResult
                    (ResError
                       (Dval.dstr_of_string_exn "Not valid base64 input.")) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bytes::hexEncode"]
    ; infix_names = []
    ; parameters = [par "bytes" TBytes]
    ; return_type = TStr
    ; description =
        "Hex (Base16) encodes `bytes` using an uppercase alphabet. Complies with RFC 4648 section 8."
    ; func =
        InProcess
          (function
          | _, [DBytes bytes] ->
              let hexUppercaseLookup = "0123456789ABCDEF" in
              let len = Bytes.length bytes in
              let buf = Buffer.create (len * 2) in
              for i = 0 to len - 1 do
                let byte = Bytes.unsafe_get bytes i |> int_of_char in
                Buffer.add_char
                  buf
                  (String.unsafe_get hexUppercaseLookup ((byte lsr 4) land 0xF)) ;
                Buffer.add_char
                  buf
                  (String.unsafe_get hexUppercaseLookup (byte land 0xF))
              done ;
              Buffer.contents buf |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bytes::length"]
    ; infix_names = []
    ; parameters = [par "bytes" TBytes]
    ; return_type = TInt
    ; description = "Length of encoded byte string"
    ; func =
        InProcess
          (function
          | _, [DBytes bytes] ->
              Dval.dint (Bytes.length bytes)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
