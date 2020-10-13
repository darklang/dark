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
  [ { prefix_names = ["toString"]
    ; infix_names = []
    ; parameters = [par "v" TAny]
    ; return_type = TStr
    ; description =
        "Returns a string representation of `v`, suitable for displaying to a user. Redacts passwords."
    ; func =
        InProcess
          (function
          | _, [a] ->
              Dval.dstr_of_string_exn (Dval.to_enduser_readable_text_v0 a)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["toRepr"]
    ; infix_names = []
    ; parameters = [par "v" TAny]
    ; return_type = TStr
    ; description =
        "Returns an adorned string representation of `v`, suitable for internal developer usage. Not designed for sending to end-users, use toString instead. Redacts passwords."
    ; func =
        InProcess
          (function
          | _, [a] ->
              Dval.dstr_of_string_exn (Dval.to_developer_repr_v0 a)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["equals"]
    ; infix_names = ["=="]
    ; parameters = [par "a" TAny; par "b" TAny]
    ; return_type = TBool
    ; description = "Returns true if the two value are equal"
    ; func =
        InProcess
          (function _, [a; b] -> DBool (equal_dval a b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["notEquals"]
    ; infix_names = ["!="]
    ; parameters = [par "a" TAny; par "b" TAny]
    ; return_type = TBool
    ; description = "Returns true if the two value are not equal"
    ; func =
        InProcess
          (function
          | _, [a; b] -> DBool (not (equal_dval a b)) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["assoc"]
    ; infix_names = []
    ; parameters = [par "obj" TObj; par "key" TStr; par "val" TAny]
    ; return_type = TObj
    ; description = "Return a copy of `obj` with the `key` set to `val`."
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr k; v] ->
              DObj (Map.set o ~key:(Unicode_string.to_string k) ~data:v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["dissoc"]
    ; infix_names = []
    ; parameters = [par "obj" TObj; par "key" TStr]
    ; return_type = TObj
    ; description = "Return a copy of `obj` with `key` unset."
    ; func =
        InProcess
          (function
          | _, [DObj o; DStr k] ->
              DObj (Map.remove o (Unicode_string.to_string k))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["toForm"]
    ; infix_names = []
    ; parameters = [par "obj" TObj; par "submit" TStr]
    ; return_type = TStr
    ; description =
        "For demonstration only. Returns a HTML form with the labels and types described in `obj`. `submit` is the form's action."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Error::toString"]
    ; infix_names = []
    ; parameters = [par "err" TError]
    ; return_type = TStr
    ; description = "Return a string representing the error"
    ; func =
        InProcess
          (function
          | _, [DError (_, err)] ->
              Dval.dstr_of_string_exn err
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["AWS::urlencode"]
    ; infix_names = []
    ; parameters = [par "str" TStr]
    ; return_type = TStr
    ; description = "Url encode a string per AWS' requirements"
    ; func =
        InProcess
          (function
          | _, [DStr str] ->
              str
              |> Unicode_string.to_string
              |> Stdlib_util.AWS.url_encode
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Twitter::urlencode"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description = "Url encode a string per Twitter's requirements"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              s
              |> Unicode_string.to_string
              |> Uri.pct_encode ~component:`Userinfo
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
