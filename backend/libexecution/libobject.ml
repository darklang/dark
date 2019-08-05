open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { pns = ["Object::empty"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Return an empty object"
    ; f =
        InProcess (function _, [] -> DObj DvalMap.empty | args -> fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Object::merge"]
    ; ins = []
    ; p = [par "left" TObj; par "right" TObj]
    ; r = TObj
    ; d =
        "Return a combined object with both objects' keys and values. If the same key exists in both `left` and `right`, then use the value from `right`"
    ; f =
        InProcess
          (function
          | _, [DObj l; DObj r] ->
              DObj (Util.merge_right l r)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Object::toJSON"]
    ; ins = []
    ; p = [par "obj" TObj]
    ; r = TStr
    ; d = "Dumps `obj` to a JSON string"
    ; f =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Object::toJSON_v1"]
    ; ins = []
    ; p = [par "obj" TObj]
    ; r = TStr
    ; d = "Dumps `obj` to a JSON string"
    ; f =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Dval.to_pretty_machine_json_v1
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = true } ]
