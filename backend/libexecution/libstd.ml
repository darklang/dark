open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

(* type coerces one list to another using a function *)
let list_coerce ~(f : dval -> 'a option) (l : dval list) :
    ('a list, dval list * dval) Result.t =
  l
  |> List.map ~f:(fun dv ->
         match f dv with Some v -> Result.Ok v | None -> Result.Error (l, dv)
     )
  |> Result.all


let error_result msg = DResult (ResError (Dval.dstr_of_string_exn msg))

let ( >>| ) = Result.( >>| )

let fns : Lib.shortfn list =
  [ { pns = ["toString"]
    ; ins = []
    ; p = [par "v" TAny]
    ; r = TStr
    ; d =
        "Returns a string representation of `v`, suitable for displaying to a user. Redacts passwords."
    ; f =
        InProcess
          (function
          | _, [a] ->
              Dval.dstr_of_string_exn (Dval.to_enduser_readable_text_v0 a)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["toRepr"]
    ; ins = []
    ; p = [par "v" TAny]
    ; r = TStr
    ; d =
        "Returns an adorned string representation of `v`, suitable for internal developer usage. Not designed for sending to end-users, use toString instead. Redacts passwords."
    ; f =
        InProcess
          (function
          | _, [a] ->
              Dval.dstr_of_string_exn (Dval.to_developer_repr_v0 a)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["equals"]
    ; ins = ["=="]
    ; p = [par "a" TAny; par "b" TAny]
    ; r = TBool
    ; d = "Returns true if the two value are equal"
    ; f =
        InProcess
          (function _, [a; b] -> DBool (equal_dval a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["notEquals"]
    ; ins = ["!="]
    ; p = [par "a" TAny; par "b" TAny]
    ; r = TBool
    ; d = "Returns true if the two value are not equal"
    ; f =
        InProcess
          (function
          | _, [a; b] -> DBool (not (equal_dval a b)) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["assoc"]
    ; ins = []
    ; p = [par "obj" TObj; par "key" TStr; par "val" TAny]
    ; r = TObj
    ; d = "Return a copy of `obj` with the `key` set to `val`."
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr k; v] ->
              DObj (Map.set o ~key:(Unicode_string.to_string k) ~data:v)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["dissoc"]
    ; ins = []
    ; p = [par "obj" TObj; par "key" TStr]
    ; r = TObj
    ; d = "Return a copy of `obj` with `key` unset."
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr k] ->
              DObj (Map.remove o (Unicode_string.to_string k))
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["toForm"]
    ; ins = []
    ; p = [par "obj" TObj; par "submit" TStr]
    ; r = TStr
    ; d =
        "For demonstration only. Returns a HTML form with the labels and types described in `obj`. `submit` is the form's action."
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr uri] ->
              let fmt =
                format_of_string
                  "<form action=\"%s\" method=\"post\">\n%s\n<input type=\"submit\" value=\"Save\">\n</form>"
              in
              let to_input (k, v) =
                let label =
                  Printf.sprintf "<label for=\"%s\">%s:</label>" k k
                in
                let input =
                  Printf.sprintf
                    "<input id=\"%s\" type=\"text\" name=\"%s\">"
                    k
                    k
                in
                label ^ "\n" ^ input
              in
              let inputs =
                o
                |> Map.to_alist
                |> List.map ~f:to_input
                |> String.concat ~sep:"\n"
              in
              Dval.dstr_of_string_exn
                (Printf.sprintf fmt (Unicode_string.to_string uri) inputs)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Error::toString"]
    ; ins = []
    ; p = [par "err" TError]
    ; r = TStr
    ; d = "Return a string representing the error"
    ; f =
        InProcess
          (function
          | _, [DError err] -> Dval.dstr_of_string_exn err | args -> fail args)
    ; ps = true
    ; dep = false } ]
