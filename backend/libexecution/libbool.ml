open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["Bool::not"]
    ; ins = []
    ; p = [par "b" TBool]
    ; r = TBool
    ; d =
        "Returns the inverse of `b`: true is `b` is false and false if `b` is true"
    ; f =
        InProcess
          (function _, [DBool b] -> DBool (not b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::and"]
    ; ins = ["&&"]
    ; p = [par "a" TBool; par "b" TBool]
    ; r = TBool
    ; d = "Returns true if both a and b are true"
    ; f =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a && b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::or"]
    ; ins = ["||"]
    ; p = [par "a" TBool; par "b" TBool]
    ; r = TBool
    ; d = "Returns true if either a is true or b is true"
    ; f =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a || b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::isNull"]
    ; ins = []
    ; p = [par "check" TAny]
    ; r = TBool
    ; d = "Returns true if the `check` parameter is null"
    ; f =
        InProcess
          (function
          | _, [value] ->
            (match value with DNull -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::isError"]
    ; ins = []
    ; p = [par "check" TAny]
    ; r = TBool
    ; d = "Returns true if the `check` parameter is an error"
    ; f =
        InProcess
          (function
          | _, [value] ->
            (match value with DError _ -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; ps = true
    ; dep = true } ]
