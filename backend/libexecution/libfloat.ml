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
    ; description = "Round up to an integer value"
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_up a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::floor"; "Float::roundDown"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TInt
    ; description =
        "Round down to an integer value. Consider Float::truncate if your goal is to discard the fractional part of a number: `Float::floor -1.9 == -2.0` but `Float::truncate -1.9 == -1.0`."
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_down a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::round"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TInt
    ; description = "Round to the nearest integer value"
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::truncate"; "Float::roundTowardsZero"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TInt
    ; description =
        "Discard the fractional portion of the float, rounding towards zero."
    ; func =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_towards_zero a |> Dint.of_float)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::absoluteValue"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the absolute value of `a` (turning negative inputs into positive outputs)."
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.abs a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::negate"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TFloat
    ; description = "Returns the negation of `a`, `-a`."
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.neg a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::sqrt"]
    ; infix_names = []
    ; parameters = [par "a" TFloat]
    ; return_type = TFloat
    ; description = "Get the square root of a float"
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (sqrt a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::power"]
    ; infix_names = []
    ; parameters = [par "base" TFloat; par "exponent" TFloat]
    ; return_type = TFloat
    ; description = "Returns `base` raised to the power of `exponent`"
    ; func =
        InProcess
          (function
          | _, [DFloat base; DFloat exp] ->
              DFloat (base ** exp)
          | args ->
              fail args)
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
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
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::min"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TFloat
    ; description = "Returns the lesser of float `a` and float `b`"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] ->
              DFloat (Float.min a b)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::max"]
    ; infix_names = []
    ; parameters = [par "a" TFloat; par "b" TFloat]
    ; return_type = TFloat
    ; description = "Returns the greater of float `a` and float `b`"
    ; func =
        InProcess
          (function
          | _, [DFloat a; DFloat b] ->
              DFloat (Float.max a b)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Float::clamp"]
    ; infix_names = []
    ; parameters = [par "value" TFloat; par "limitA" TFloat; par "limitB" TFloat]
    ; return_type = TFloat
    ; description =
        "If `value` is within the range given by `limitA` and `limitB`, returns `value`.
         If `value` is outside the range, returns `limitA` or `limitB`, whichever is closer to `value`.
         `limitA` and `limitB` can be provided in any order."
    ; func =
        InProcess
          (function
          | _, [DFloat v; DFloat a; DFloat b] ->
              let min, max = if a < b then (a, b) else (b, a) in
              ( match Float.clamp v ~min ~max with
              | Ok clamped ->
                  DFloat clamped
              | Error e ->
                  (* Since min and max are pre-sorted, this can only happen if min or max are NaN.
                   * TODO: eliminate NaNs so that this can't happen 
                   * (at time of writing (f86edaa1c58c94e27186060ae4fe8745112dd0e5), NaNs can't be parsed,
                   * so this can't happen in practice) *)
                  let info =
                    [("a", Float.to_string a); ("b", Float.to_string b)]
                  in
                  Exception.code
                    ~info
                    ("Internal Float.clamp exception: " ^ Error.to_string_hum e)
              )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
