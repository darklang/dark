open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { name = fn "Option" "map" 0

    ; parameters = [Param.make "option" TOption; func ["val"]]
    ; returnType = TOption
    ; description =
        "If `option` is `Just value`, returns `Just (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Just`).
        If `result` is `Nothing`, returns `Nothing`."
    ; fn =

          (function
          | state, [DOption o; DLambda b] ->
            ( match o with
            | OptJust dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                DOption (OptJust result)
            | OptNothing ->
                DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "Option" "map" 1

    ; parameters = [Param.make "option" TOption; func ["val"]]
    ; returnType = TOption
    ; description =
        "If <var option> is {{Just <var value>}}, then return {{Just (f <var value>)}}. The lambda <var f> applied to <var value> and the result is wrapped in {{Just}}. Otherwise if the result is {{Nothing}}, then return {{Nothing}}."
    ; fn =

          (function
          | state, [DOption o; DLambda b] ->
            ( match o with
            | OptJust dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                Dval.to_opt_just result
            | OptNothing ->
                DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Option" "map2" 0

    ; parameters =
        [Param.make "option1" TOption; Param.make "option2" TOption; func ["v1"; "v2"]]
    ; returnType = TOption
    ; description =
        "If both arguments are {{Just}} (<param option1> is {{Just <var v1>}} and <param option2> is {{Just <var v2>}}), then return {{Just (f <var v1> <var v2>)}} -- The lambda <param f> should have two parameters, representing <var v1> and <var v2>. But if either <param option1> or <param option2> are {{Nothing}}, returns {{Nothing}} without applying <param f>."
    ; fn =

          (function
          | state, [DOption o1; DOption o2; DLambda b] ->
            ( match (o1, o2) with
            | OptNothing, _ | _, OptNothing ->
                DOption OptNothing
            | OptJust dv1, OptJust dv2 ->
                let result = Ast.execute_dblock ~state b [dv1; dv2] in
                Dval.to_opt_just result )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Option" "andThen" 0

    ; parameters = [Param.make "option" TOption; func ["val"]]
    ; returnType = TOption
    ; description =
        "If <param option> is {{Just <var input>}}, returns {{f <var input>}}. Where the lambda <param f> is applied to <var input> and must return {{Just <var output>}} or {{Nothing}}. Otherwise if <param option> is {{Nothing}}, returns {{Nothing}}."
    ; fn =

          (function
          | state, [DOption o; DLambda b] ->
            ( match o with
            | OptJust dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                ( match result with
                | DOption result ->
                    DOption result
                | other ->
                    RT.error
                      other
                      "an option"
                      "Expected `f` to return an option" )
            | OptNothing ->
                DOption OptNothing )
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "Option" "withDefault" 0

    ; parameters = [Param.make "option" TOption; Param.make "default" TAny]
    ; returnType = TAny
    ; description =
        "If <param option> is {{Just <var value>}}, returns <var value>. Returns <param default> otherwise."
    ; fn =

          (function
          | _, [DOption o; default] ->
            (match o with OptJust dv -> dv | OptNothing -> default)
          | args ->
              incorrectArgs ())
    ; sqlSpec = NotYetImplementedTODO
      ; previewable = Pure
    ; deprecated = NotDeprecated } ]
