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

let fns : Lib.shortfn list =
  [ { pns = ["Float::ceiling"; "Float::roundUp"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d = "Round up to an integer value"
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_up a |> Dint.of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::floor"; "Float::roundDown"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d =
        "Round down to an integer value. Consider Float::truncate if your goal is to discard the fractional part of a number: `Float::floor -1.9 == -2.0` but `Float::truncate -1.9 == -1.0`."
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_down a |> Dint.of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::round"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d = "Round to the nearest integer value"
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round a |> Dint.of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::truncate"; "Float::roundTowardsZero"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d = "Discard the fractional portion of the float, rounding towards zero."
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_towards_zero a |> Dint.of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::abs"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TFloat
    ; d =
        "Returns the absolute value of `a` (turning negative inputs into positive outputs)."
    ; f =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.abs a) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::negate"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TFloat
    ; d = "Returns the negation of `a`, `-a`."
    ; f =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.neg a) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::sqrt"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TFloat
    ; d = "Get the square root of a float"
    ; f =
        InProcess
          (function _, [DFloat a] -> DFloat (sqrt a) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::divide"]
    ; ins = ["/"]
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Divide float `a` by float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a /. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::add"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Add float `a` to float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a +. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::multiply"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Multiply float `a` by float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a *. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::subtract"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Subtract float `b` from float `a`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a -. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::greaterThan"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is greater than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a >. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::greaterThanOrEqualTo"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is greater than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a >=. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::lessThan"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is less than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a <. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::lessThanOrEqualTo"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is less than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a <=. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::sum"]
    ; ins = []
    ; p = [par "a" TList]
    ; r = TFloat
    ; d = "Returns the sum of all the floats in the list"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::min"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Returns the lesser of float `a` and float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] ->
              DFloat (Float.min a b)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::max"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Returns the greater of float `a` and float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] ->
              DFloat (Float.max a b)
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
