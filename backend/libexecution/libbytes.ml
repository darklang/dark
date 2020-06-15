open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Types.fluid_expr fn list =
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
