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


let ( >>| ) = Result.( >>| )

let fns : fn list =
  [ { prefix_names = ["Float::ceiling"; "Float::roundUp"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TInt
    ; description = "Round above to an integer value"
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_up a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::floor"; "Float::roundDown"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TInt
    ; description = "Round down to an integer value"
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_down a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::round"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TInt
    ; description = "Round to nearest integer value"
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::sqrt"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TFloat
    ; description = "Get the square root of a float"
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (sqrt a) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::divide"]
    ; infix_names = ["/"]
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TFloat
    ; description = "Divide float `a` by float `b`"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a /. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::add"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TFloat
    ; description = "Add float `a` to float `b`"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a +. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::multiply"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TFloat
    ; description = "Multiply float `a` by float `b`"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a *. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::subtract"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TFloat
    ; description = "Subtract float `b` from float `a`"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a -. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::greaterThan"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TBool
    ; description = "Returns true if a is greater than b"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a >. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::greaterThanOrEqualTo"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TBool
    ; description = "Returns true if a is greater than b"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a >=. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::lessThan"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TBool
    ; description = "Returns true if a is less than b"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a <. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::lessThanOrEqualTo"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TBool
    ; description = "Returns true if a is less than b"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a <=. b) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["Float::sum"]
    ; infix_names = []
    ; parameters = [par "a" TList]
    ; return_type = TFloat
    ; description = "Returns the sum of all the floats in the list"
    ; func =
        InProcess
          (function
          | _, [DList l] ->
              l
              |> list_coerce ~f:Dval.to_float
              >>| List.fold_left ~f:( +. ) ~init:0.0
              >>| (fun x -> DFloat x)
              |> Result.map_error ~f:(fun (result, example_value) ->
                     RT.error
                       ~actual:(DList result)
                       ~result:(DList result)
                       ~long:
                         ( "Float::sum requires all values to be floats, but "
                         ^ Dval.to_developer_repr_v0 example_value
                         ^ " is a "
                         ^ Dval.pretty_tipename example_value )
                       ~expected:"every list item to be an float "
                       "Sum expects you to pass a list of floats")
              |> Result.ok_exn
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false } ]
