open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["Bytes::base64Encode"]
    ; ins = []
    ; p = [par "bytes" TBytes]
    ; r = TStr
    ; d = "Base64 encodes `bytes`. Uses URL-safe encoding."
    ; f =
        InProcess
          (function
          | _, [DBytes bytes] ->
              Dval.dstr_of_string_exn
                (B64.encode
                   ~alphabet:B64.uri_safe_alphabet
                   ~pad:false
                   (Bytes.to_string bytes))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ]
