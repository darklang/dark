open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { name = fn "Uuid" "generate" 0

    ; parameters = []
    ; returnType = TUuid
    ; description = "Generate a new UUID v4 according to RFC 4122"
    ; fn =
         (function _, [] -> DUuid (Uuidm.v `V4) | _ -> incorrectArgs ())
        (* similarly to Date::now, it's not particularly fun for this to change
     * when live programming *)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Impure
    ; deprecated = NotDeprecated } ]
