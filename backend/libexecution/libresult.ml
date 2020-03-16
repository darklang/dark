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
        "If `result` is `Ok value`, returns `Ok (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Ok`). If `result` is `Error msg`, returns `result` unchanged."
    ; f =
        InProcess
          (function
          | state, [DResult r; DBlock b] ->
            ( match r with
            | ResOk dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                DResult (ResOk result)
            | ResError _ ->
                DResult r )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Result::map_v1"]
    ; ins = []
    ; p = [par "result" TResult; func ["val"]]
    ; r = TResult
    ; d =
        "If `result` is `Ok value`, returns `Ok (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Ok`). If `result` is `Error msg`, returns `result` unchanged."
    ; f =
        InProcess
          (function
          | state, [DResult r; DBlock d] ->
            ( match r with
            | ResOk dv ->
                let result = Ast.execute_dblock ~state d [dv] in
                Dval.to_res_ok result
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
        "If `result` is `Error msg`, returns `Error (f msg)` (the lambda `f` is applied to `msg` and the result is wrapped in `Error`). If `result` is `Ok value`, returns `result` unchanged."
    ; f =
        InProcess
          (function
          | state, [DResult r; DBlock b] ->
            ( match r with
            | ResOk _ ->
                DResult r
            | ResError err ->
                let result = Ast.execute_dblock ~state b [err] in
                DResult (ResError result) )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Result::mapError_v1"]
    ; ins = []
    ; p = [par "result" TResult; func ["val"]]
    ; r = TAny
    ; d =
        "If `result` is `Error msg`, returns `Error (f msg)` (the lambda `f` is applied to `msg` and the result is wrapped in `Error`). If `result` is `Ok value`, returns `result` unchanged."
    ; f =
        InProcess
          (function
          | state, [DResult r; DBlock b] ->
            ( match r with
            | ResOk _ ->
                DResult r
            | ResError err ->
                let result = Ast.execute_dblock ~state b [err] in
                Dval.to_res_err result )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::withDefault"]
    ; ins = []
    ; p = [par "result" TResult; par "default" TAny]
    ; r = TAny
    ; d =
        "If `result` is `Ok value`, returns `value`. Returns `default` otherwise."
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
        "Turn an option into a result, using `error` as the error message for Error. Specifically, if `option` is `Just value`, returns `Ok value`. Returns `Error error` otherwise."
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
    ; dep = true }
  ; { pns = ["Result::fromOption_v1"]
    ; ins = []
    ; p = [par "option" TOption; par "error" TStr]
    ; r = TResult
    ; d =
        "Turn an option into a result, using `error` as the error message for Error. Specifically, if `option` is `Just value`, returns `Ok value`. Returns `Error error` otherwise."
    ; f =
        InProcess
          (function
          | _, [DOption o; DStr error] ->
            ( match o with
            | OptJust dv ->
                Dval.to_res_ok dv
                (* match (Dval.to_opt_just dv) with
                  | DOption (OptJust v) -> Dval.to_res_ok v
                  | DError s -> DError s
                  | _ -> Dval.to_res_err (DStr error)
                *)
            | OptNothing ->
                Dval.to_res_err (DStr error) )
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
    ; dep = true }
  ; { pns = ["Result::toOption_v1"]
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
                Dval.to_opt_just dv
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
        "If `result` is `Ok value`, returns `f value` (the lambda `f` is applied to `value` and must return `Error msg` or `Ok newValue`). If `result` is `Error msg`, returns `result` unchanged."
    ; f =
        InProcess
          (function
          | state, [DResult o; DBlock b] ->
            ( match o with
            | ResOk dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                ( match result with
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
    ; dep = true }
  ; { pns = ["Result::andThen_v1"]
    ; ins = []
    ; p = [par "result" TResult; func ["val"]]
    ; r = TResult
    ; d =
        "If `result` is `Ok value`, returns `f value` (the lambda `f` is applied to `value` and must return `Error msg` or `Ok newValue`). If `result` is `Error msg`, returns `result` unchanged."
    ; f =
        InProcess
          (function
          | state, [DResult o; DBlock b] ->
            ( match o with
            | ResOk dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                ( match result with
                | DResult (ResOk res) ->
                    Dval.to_res_ok res
                | DResult (ResError res) ->
                    Dval.to_res_err res
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
