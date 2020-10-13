open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { name = fn "Uuid" "generate" 0

    ; parameters = []
    ; return_type = TUuid
    ; description = "Generate a new UUID v4 according to RFC 4122"
    ; func =
        InProcess (function _, [] -> DUuid (Uuidm.v `V4) | args -> fail args)
        (* similarly to Date::now, it's not particularly fun for this to change
     * when live programming *)
    ; preview_safety = Unsafe
    ; deprecated = NotDeprecated } ]
