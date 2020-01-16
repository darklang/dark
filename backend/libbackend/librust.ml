open Core_kernel
open Libexecution
open Libexecution.Lib
open Types.RuntimeT
module Unicode = Libexecution.Unicode_string

external rust_random_bytes : int -> char list = "random_bytes"

let fns =
  [ { pns = ["Bytes::random"]
    ; ins = []
    ; p = [par "num" TInt]
    ; r = TBytes
    ; d = "Return [num] random bytes."
    ; f =
        InProcess
          (function
          | _, [DInt num] ->
              let chars = rust_random_bytes (Dint.to_int_exn num) in
              DBytes (RawBytes.of_char_list chars)
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
