open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

(* type coerces one list to another using a function *)
let list_coerce ~(f : dval -> 'a option) (l : dval list) :
    ('a list, dval list * dval) Result.t =
  l
  |> List.map ~f:(fun dv ->
         match f dv with Some v -> Result.Ok v | None -> Result.Error (l, dv))
  |> Result.all


let error_result msg = DResult (ResError (Dval.dstr_of_string_exn msg))

let ( >>| ) = Result.( >>| )

let fns : Lib.shortfn list =
  [ { pns = ["Option::map"]
    ; ins = []
    ; p = [par "option" TOption; func ["val"]]
    ; r = TOption
    ; d =
        "Transform an Option using `f`, only if the Option is a Just. If Nothing, does nothing."
    ; f =
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
    ; ps = true
    ; dep = true }
  ; { pns = ["Option::map_v1"]
    ; ins = []
    ; p = [par "option" TOption; func ["val"]]
    ; r = TOption
    ; d =
        "Transform an Option using `f`, only if the Option is a Just. If Nothing, does nothing."
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Option::andThen"]
    ; ins = []
    ; p = [par "option" TOption; func ["val"]]
    ; r = TOption
    ; d =
        "Transform an Option using `f`, only if the Option is a Just. If Nothing, does nothing. Combines the result into a single option, where if both the caller and the result are Just, the result is a single Just"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Option::withDefault"]
    ; ins = []
    ; p = [par "option" TOption; par "default" TAny]
    ; r = TAny
    ; d =
        "Turn an option into a normal value, using `default` if the option is Nothing."
    ; f =
        InProcess
          (function
          | _, [DOption o; default] ->
            (match o with OptJust dv -> dv | OptNothing -> default)
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
