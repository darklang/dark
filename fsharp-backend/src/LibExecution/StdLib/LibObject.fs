open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { name = fn "Object" "empty" 0

    ; parameters = []
    ; return_type = TObj
    ; description = "Return an empty object"
    ; func =
        InProcess (function _, [] -> DObj DvalMap.empty | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Object" "merge" 0

    ; parameters = [Param.make "left" TObj; Param.make "right" TObj]
    ; return_type = TObj
    ; description =
        "Return a combined object with both objects' keys and values. If the same key exists in both `left` and `right`, then use the value from `right`"
    ; func =
        InProcess
          (function
          | _, [DObj l; DObj r] ->
              DObj (Stdlib_util.merge_right l r)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Object" "toJSON" 0

    ; parameters = [Param.make "obj" TObj]
    ; return_type = TStr
    ; description = "Dumps `obj` to a JSON string"
    ; func =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Object" "toJSON" 1

    ; parameters = [Param.make "obj" TObj]
    ; return_type = TStr
    ; description = "Dumps `obj` to a JSON string"
    ; func =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Dval.to_pretty_machine_json_v1
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = ReplacedBy(fn "" "" 0) } ]
