open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Types.fluid_expr fn list =
  [ { prefix_names = ["Bool::not"]
    ; infix_names = []
    ; parameters = [par "b" TBool]
    ; return_type = TBool
    ; description =
        "Returns the inverse of `b`: true if `b` is false and false if `b` is true"
    ; func =
        InProcess (function _, [DBool b] -> DBool (not b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bool::and"]
    ; infix_names = ["&&"]
    ; parameters = [par "a" TBool; par "b" TBool]
    ; return_type = TBool
    ; description = "Returns true if both a and b are true"
    ; func =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a && b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bool::or"]
    ; infix_names = ["||"]
    ; parameters = [par "a" TBool; par "b" TBool]
    ; return_type = TBool
    ; description = "Returns true if either a is true or b is true"
    ; func =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a || b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bool::xor"]
    ; infix_names = []
    ; parameters = [par "a" TBool; par "b" TBool]
    ; return_type = TBool
    ; description =
        "Returns `true` if exactly one of `a` and `b` is `true`. Returns `false` if both are `true` or neither is `true`."
    ; func =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a <> b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bool::isNull"]
    ; infix_names = []
    ; parameters = [par "check" TAny]
    ; return_type = TBool
    ; description = "Returns true if the `check` parameter is null"
    ; func =
        InProcess
          (function
          | _, [value] ->
            (match value with DNull -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Bool::isError"]
    ; infix_names = []
    ; parameters = [par "check" TAny]
    ; return_type = TBool
    ; description = "Returns `true` if the `check` parameter is an error"
    ; func =
        InProcess
          (function
          | _, [value] ->
            (match value with DError _ -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true } ]
