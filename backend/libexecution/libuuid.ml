open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

(* type coerces one list to another using a function *)
let list_coerce ~(f : dval -> 'a option) (l : dval list) :
    ('a list, dval list * dval) Result.t =
  l
  |> List.map ~f:(fun dv ->
         match f dv with Some v -> Result.Ok v | None -> Result.Error (l, dv))
  |> Result.all


let error_result msg = DResult (ResError (Dval.dstr_of_string_exn msg))

let ( >>| ) = Result.( >>| )

let fns : fn list =
  [ { prefix_names = ["Uuid::generate"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TUuid
    ; description = "Generate a new UUID v4 according to RFC 4122"
    ; func =
        InProcess (function _, [] -> DUuid (Uuidm.v `V4) | args -> fail args)
        (* similarly to Date::now, it's not particularly fun for this to change
     * when live programming *)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
