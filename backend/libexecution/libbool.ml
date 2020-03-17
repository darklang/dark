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
        "Returns the inverse of `b`: true if `b` is false and false if `b` is true"
    ; f =
        InProcess (function _, [DBool b] -> DBool (not b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::and"]
    ; ins = ["&&"]
    ; p = [par "a" TBool; par "b" TBool]
    ; r = TBool
    ; d =
        "Returns `true` if both `a` and `b` are true. Returns `false` otherwise."
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
    ; d =
        "Returns `true` if `a`, `b`, or both are `true`. Returns `false` otherwise."
    ; f =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a || b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::xor"]
    ; ins = []
    ; p = [par "a" TBool; par "b" TBool]
    ; r = TBool
    ; d =
        "Returns `true` if `a` is `true` or `b` is `true`, but not both. Returns `false` otherwise."
    ; f =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a <> b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::isNull"]
    ; ins = []
    ; p = [par "check" TAny]
    ; r = TBool
    ; d = "Returns `true` if the `check` parameter is `null`"
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
    ; d = "Returns `true` if the `check` parameter is some `Error msg`"
    ; f =
        InProcess
          (function
          | _, [value] ->
            (match value with DError _ -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; ps = true
    ; dep = true } ]
