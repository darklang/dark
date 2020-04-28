open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { prefix_names = ["Result::map"]
    ; infix_names = []
    ; parameters = [par "result" TResult; func ["val"]]
    ; return_type = TResult
    ; description =
        "If `result` is `Ok value`, returns `Ok (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Ok`). If `result` is `Error msg`, returns `result` unchanged."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Result::map_v1"]
    ; infix_names = []
    ; parameters = [par "result" TResult; func ["val"]]
    ; return_type = TResult
    ; description =
        "If `result` is `Ok value`, returns `Ok (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Ok`). If `result` is `Error msg`, returns `result` unchanged."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Result::mapError"]
    ; infix_names = []
    ; parameters = [par "result" TResult; func ["val"]]
    ; return_type = TAny
    ; description =
        "If `result` is `Error msg`, returns `Error (f msg)` (the lambda `f` is applied to `msg` and the result is wrapped in `Error`). If `result` is `Ok value`, returns `result` unchanged."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Result::mapError_v1"]
    ; infix_names = []
    ; parameters = [par "result" TResult; func ["val"]]
    ; return_type = TAny
    ; description =
        "If `result` is `Error msg`, returns `Error (f msg)` (the lambda `f` is applied to `msg` and the result is wrapped in `Error`). If `result` is `Ok value`, returns `result` unchanged."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Result::withDefault"]
    ; infix_names = []
    ; parameters = [par "result" TResult; par "default" TAny]
    ; return_type = TAny
    ; description =
        "If `result` is `Ok value`, returns `value`. Returns `default` otherwise."
    ; func =
        InProcess
          (function
          | _, [DResult o; default] ->
            (match o with ResOk dv -> dv | ResError _ -> default)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Result::fromOption"]
    ; infix_names = []
    ; parameters = [par "option" TOption; par "error" TStr]
    ; return_type = TResult
    ; description =
        "Turn an option into a result, using `error` as the error message for Error. Specifically, if `option` is `Just value`, returns `Ok value`. Returns `Error error` otherwise."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Result::fromOption_v1"]
    ; infix_names = []
    ; parameters = [par "option" TOption; par "error" TStr]
    ; return_type = TResult
    ; description =
        "Turn an option into a result, using `error` as the error message for Error. Specifically, if `option` is `Just value`, returns `Ok value`. Returns `Error error` otherwise."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Result::toOption"]
    ; infix_names = []
    ; parameters = [par "result" TResult]
    ; return_type = TAny
    ; description = "Turn a result into an option."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Result::toOption_v1"]
    ; infix_names = []
    ; parameters = [par "result" TResult]
    ; return_type = TAny
    ; description = "Turn a result into an option."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Result::map2"]
    ; infix_names = []
    ; parameters =
        [par "result1" TResult; par "result2" TResult; func ["v1"; "v2"]]
    ; return_type = TResult
    ; description =
        "If both arguments are `Ok` (`result1` is `Ok v1` and `result2` is `Ok v2`), returns `Ok (f v1 v2)` --
        the lambda `f` is applied to `v1` and `v2`, and the result is wrapped in `Ok`.
        Otherwise, returns the first of `result1` and `result2` that is an error."
    ; func =
        InProcess
          (function
          | state, [DResult r1; DResult r2; DBlock b] ->
            ( match (r1, r2) with
            | ResError e1, _ ->
                DResult (ResError e1)
            | ResOk _, ResError e2 ->
                DResult (ResError e2)
            | ResOk dv1, ResOk dv2 ->
                let result = Ast.execute_dblock ~state b [dv1; dv2] in
                Dval.to_res_ok result )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Result::andThen"]
    ; infix_names = []
    ; parameters = [par "result" TResult; func ["val"]]
    ; return_type = TResult
    ; description =
        "If `result` is `Ok value`, returns `f value` (the lambda `f` is applied to `value` and must return `Error msg` or `Ok newValue`). If `result` is `Error msg`, returns `result` unchanged."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Result::andThen_v1"]
    ; infix_names = []
    ; parameters = [par "result" TResult; func ["val"]]
    ; return_type = TResult
    ; description =
        "If `result` is `Ok value`, returns `f value` (the lambda `f` is applied to `value` and must return `Error msg` or `Ok newValue`). If `result` is `Error msg`, returns `result` unchanged."
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false } ]
