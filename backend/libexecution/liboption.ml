open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

(* type coerces one list to another using a function *)
let list_coerce ~(f : dval -> 'a option) (l : dval list) :
    ('a list, dval list * dval) Result.t =
  l
  |> List.map ~f:(fun dv ->
         match f dv with Some v -> Result.Ok v | None -> Result.Error (l, dv)
     )
  |> Result.all


let error_result msg = DResult (ResError (Dval.dstr_of_string_exn msg))

let ( >>| ) = Result.( >>| )

let fns : Lib.shortfn list =
  [ { pns = ["Option::map"]
    ; ins = []
    ; p = [par "option" TOption; func ["val"]]
    ; r = TOption
    ; d =
        "Transform an Option using `f`, only if the Option is a Just. If Nothing, doesn't nothing."
    ; f =
        InProcess
          (function
          | _, [DOption o; DBlock fn] ->
            ( match o with
            | OptJust dv ->
                DOption (OptJust (fn [dv]))
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
        "Transform an Option using `f`, only if the Option is a Just. If Nothing, doesn't nothing. Combines the result into a single option, where if both the caller and the result are Just, the result is a single Just"
    ; f =
        InProcess
          (function
          | _, [DOption o; DBlock fn] ->
            ( match o with
            | OptJust dv ->
              ( match fn [dv] with
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
