open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["Result::map"]
    ; ins = []
    ; p = [par "result" TResult; func ["val"]]
    ; r = TResult
    ; d =
        "Transform a Result using `f`, only if the Result is an Ok. If Error, doesn't nothing."
    ; f =
        InProcess
          (function
          | _, [DResult r; DBlock fn] ->
            ( match r with
            | ResOk dv ->
                DResult (ResOk (fn [dv]))
            | ResError _ ->
                DResult r )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::mapError"]
    ; ins = []
    ; p = [par "result" TResult; func ["val"]]
    ; r = TAny
    ; d =
        "Transform a Result by calling `f` on the Error portion of the Result. If Ok , does nothing."
    ; f =
        InProcess
          (function
          | _, [DResult r; DBlock fn] ->
            ( match r with
            | ResOk _ ->
                DResult r
            | ResError err ->
                DResult (ResError (fn [err])) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::withDefault"]
    ; ins = []
    ; p = [par "result" TResult; par "default" TAny]
    ; r = TAny
    ; d =
        "Turn a result into a normal value, using `default` if the result is Error."
    ; f =
        InProcess
          (function
          | _, [DResult o; default] ->
            (match o with ResOk dv -> dv | ResError _ -> default)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::fromOption"]
    ; ins = []
    ; p = [par "option" TOption; par "error" TStr]
    ; r = TResult
    ; d =
        "Turn an option into a result, using `error` as the error message for Error."
    ; f =
        InProcess
          (function
          | _, [DOption o; DStr error] ->
            ( match o with
            | OptJust dv ->
                DResult (ResOk dv)
            | OptNothing ->
                DResult (ResError (DStr error)) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::toOption"]
    ; ins = []
    ; p = [par "result" TResult]
    ; r = TAny
    ; d = "Turn a result into an option."
    ; f =
        InProcess
          (function
          | _, [DResult o] ->
            ( match o with
            | ResOk dv ->
                DOption (OptJust dv)
            | ResError _ ->
                DOption OptNothing )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::andThen"]
    ; ins = []
    ; p = [par "result" TResult; func ["val"]]
    ; r = TResult
    ; d =
        "Transform a Result using `f`, only if the Result is an Ok. If Error, doesn't nothing. Combines the result into a single Result, where if both the caller and the result are Error, the result is a single Error"
    ; f =
        InProcess
          (function
          | _, [DResult o; DBlock fn] ->
            ( match o with
            | ResOk dv ->
              ( match fn [dv] with
              | DResult result ->
                  DResult result
              | other ->
                  RT.error
                    ~actual:other
                    ~expected:"a result"
                    "Expected `f` to return a result" )
            | ResError msg ->
                DResult (ResError msg) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
