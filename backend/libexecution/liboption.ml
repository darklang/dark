open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { prefix_names = ["Option::map"]
    ; infix_names = []
    ; parameters = [par "option" TOption; func ["val"]]
    ; return_type = TOption
    ; description =
        "If `option` is `Just value`, returns `Just (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Just`).
        If `result` is `Nothing`, returns `Nothing`."
    ; func =
        InProcess
          (function
          | state, [DOption o; DBlock b] ->
            ( match o with
            | OptJust dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                DOption (OptJust result)
            | OptNothing ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["Option::map_v1"]
    ; infix_names = []
    ; parameters = [par "option" TOption; func ["val"]]
    ; return_type = TOption
    ; description =
        "If <var option> is {{Just <var value>}}, then return {{Just (f <var value>)}}. The lambda <var f> applied to <var value> and the result is wrapped in {{Just}}. Otherwise if the result is {{Nothing}}, then return {{Nothing}}."
    ; func =
        InProcess
          (function
          | state, [DOption o; DBlock b] ->
            ( match o with
            | OptJust dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                Dval.to_opt_just result
            | OptNothing ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Option::map2"]
    ; infix_names = []
    ; parameters =
        [par "option1" TOption; par "option2" TOption; func ["v1"; "v2"]]
    ; return_type = TOption
    ; description =
        "If both arguments are {{Just}} (<param option1> is {{Just <var v1>}} and <param option2> is {{Just <var v2>}}), then return {{Just (f <var v1> <var v2>)}} -- The lambda <param f> should have two parameters, representing <var v1> and <var v2>. But if either <param option1> or <param option2> are {{Nothing}}, returns {{Nothing}} without applying <param f>."
    ; func =
        InProcess
          (function
          | state, [DOption o1; DOption o2; DBlock b] ->
            ( match (o1, o2) with
            | OptNothing, _ | _, OptNothing ->
                DOption OptNothing
            | OptJust dv1, OptJust dv2 ->
                let result = Ast.execute_dblock ~state b [dv1; dv2] in
                Dval.to_opt_just result )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Option::andThen"]
    ; infix_names = []
    ; parameters = [par "option" TOption; func ["val"]]
    ; return_type = TOption
    ; description =
        "If <param option> is {{Just <var input>}}, returns {{f <var input>}}. Where the lambda <param f> is applied to <var input> and must return {{Just <var output>}} or {{Nothing}}. Otherwise if <param option> is {{Nothing}}, returns {{Nothing}}."
    ; func =
        InProcess
          (function
          | state, [DOption o; DBlock b] ->
            ( match o with
            | OptJust dv ->
                let result = Ast.execute_dblock ~state b [dv] in
                ( match result with
                | DOption result ->
                    DOption result
                | other ->
                    RT.error
                      ~actual:other
                      ~expected:"an option"
                      "Expected `f` to return an option" )
            | OptNothing ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Option::withDefault"]
    ; infix_names = []
    ; parameters = [par "option" TOption; par "default" TAny]
    ; return_type = TAny
    ; description =
        "If <param option> is {{Just <var value>}}, returns <var value>. Returns <param default> otherwise."
    ; func =
        InProcess
          (function
          | _, [DOption o; default] ->
            (match o with OptJust dv -> dv | OptNothing -> default)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
