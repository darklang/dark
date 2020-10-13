open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { name = fn "Result" "map" 0

    ; parameters = [Param.make "result" TResult; func ["val"]]
    ; returnType = TResult
    ; description =
        "If `result` is `Ok value`, returns `Ok (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Ok`). If `result` is `Error msg`, returns `result` unchanged."
    ; fn =

          (function
          | state, [DResult r; DBlock b] ->
            ( match r with
            | ResOk dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                DResult (ResOk result)
            | ResError _ ->
                DResult r )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Result" "map" 1

    ; parameters = [Param.make "result" TResult; func ["val"]]
    ; returnType = TResult
    ; description =
        "If <param result> is {{Ok <var value>}}, returns {{Ok (f <var value>)}}. The lambda <param f> is applied to <var value> and the result is wrapped in {{Ok}}. If <param result> is {{Error <var msg>}}, returns <param result> unchanged."
    ; fn =

          (function
          | state, [DResult r; DBlock d] ->
            ( match r with
            | ResOk dv ->
                let result = Ast.execute_dblock ~state d [dv] in
                Dval.to_res_ok result
            | ResError _ ->
                DResult r )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Result" "mapError" 0

    ; parameters = [Param.make "result" TResult; func ["val"]]
    ; returnType = TResult
    ; description =
        "If `result` is `Error msg`, returns `Error (f msg)` (the lambda `f` is applied to `msg` and the result is wrapped in `Error`). If `result` is `Ok value`, returns `result` unchanged."
    ; fn =

          (function
          | state, [DResult r; DBlock b] ->
            ( match r with
            | ResOk _ ->
                DResult r
            | ResError err ->
                let result = Ast.execute_dblock ~state b [err] in
                DResult (ResError result) )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Result" "mapError" 1

    ; parameters = [Param.make "result" TResult; func ["val"]]
    ; returnType = TResult
    ; description =
        "If <param result> is {{Error <var msg>}}, returns {{Error (f <var msg>)}}. The lambda <var f> is applied to <var msg> and the result is wrapped in {{Error}}. If <param result> is {{Ok <var value>}}, returns <param result> unchanged."
    ; fn =

          (function
          | state, [DResult r; DBlock b] ->
            ( match r with
            | ResOk _ ->
                DResult r
            | ResError err ->
                let result = Ast.execute_dblock ~state b [err] in
                Dval.to_res_err result )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Result" "withDefault" 0

    ; parameters = [Param.make "result" TResult; Param.make "default" TAny]
    ; returnType = TAny
    ; description =
        "If <param result> is {{Ok <var value>}}, returns <var value>. Returns <param default> otherwise."
    ; fn =

          (function
          | _, [DResult o; default] ->
            (match o with ResOk dv -> dv | ResError _ -> default)
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Result" "fromOption" 0

    ; parameters = [Param.make "option" TOption; Param.make "error" TStr]
    ; returnType = TResult
    ; description =
        "Turn an option into a result, using `error` as the error message for Error. Specifically, if `option` is `Just value`, returns `Ok value`. Returns `Error error` otherwise."
    ; fn =

          (function
          | _, [DOption o; DStr error] ->
            ( match o with
            | OptJust dv ->
                DResult (ResOk dv)
            | OptNothing ->
                DResult (ResError (DStr error)) )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Result" "fromOption" 1

    ; parameters = [Param.make "option" TOption; Param.make "error" TStr]
    ; returnType = TResult
    ; description =
        "Turn an option into a result, using <param error> as the error message for Error. Specifically, if <param option> is {{Just <var value>}}, returns {{Ok <var value>}}. Returns {{Error <var error>}} otherwise."
    ; fn =

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
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Result" "toOption" 0

    ; parameters = [Param.make "result" TResult]
    ; returnType = TOption
    ; description = "Turn a result into an option."
    ; fn =

          (function
          | _, [DResult o] ->
            ( match o with
            | ResOk dv ->
                DOption (OptJust dv)
            | ResError _ ->
                DOption OptNothing )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Result" "toOption" 1

    ; parameters = [Param.make "result" TResult]
    ; returnType = TOption
    ; description = "Turn a result into an option."
    ; fn =

          (function
          | _, [DResult o] ->
            ( match o with
            | ResOk dv ->
                Dval.to_opt_just dv
            | ResError _ ->
                DOption OptNothing )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Result" "map2" 0

    ; parameters =
        [Param.make "result1" TResult; Param.make "result2" TResult; func ["v1"; "v2"]]
    ; returnType = TResult
    ; description =
        "If both <param result1> is {{Ok <var v1>}} and <param result2> is {{Ok <var v2>}}, returns {{Ok (f <var v1> <var v2>)}} -- the lambda <var f> is applied to <var v1> and <var v2>, and the result is wrapped in {{Ok}}. Otherwise, returns the first of <param result1> and <param result2> that is an error."
    ; fn =

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
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Result" "andThen" 0

    ; parameters = [Param.make "result" TResult; func ["val"]]
    ; returnType = TResult
    ; description =
        "If `result` is `Ok value`, returns `f value` (the lambda `f` is applied to `value` and must return `Error msg` or `Ok newValue`). If `result` is `Error msg`, returns `result` unchanged."
    ; fn =

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
                      other
                      "a result"
                      "Expected `f` to return a result" )
            | ResError msg ->
                DResult (ResError msg) )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Result" "andThen" 1

    ; parameters = [Param.make "result" TResult; func ["val"]]
    ; returnType = TResult
    ; description =
        "If <param result> is {{Ok <var value>}}, returns {{f <var value>}}. The lambda <param f> is applied to <var value> and must return {{Error <var msg>}} or {{Ok <var newValue>}}. If <param result> is {{Error <var msg>}}, returns <param result> unchanged."
    ; fn =

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
                      other
                      "a result"
                      "Expected `f` to return a result" )
            | ResError msg ->
                DResult (ResError msg) )
          | args ->
              Error FnWrongType)
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated } ]
