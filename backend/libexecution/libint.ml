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


let ( >>| ) = Result.( >>| )

let fns : Lib.shortfn list =
  [ { pns = ["Int::mod"]
    ; ins = ["%"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d =
        "Return `a` % `b`, the modulus of a and b. This is the integer remainder left when `a` is divided by `b`. For example, `15 % 6 = 3`."
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.( % ) a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::add"]
    ; ins = ["+"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Adds two integers together"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.( + ) a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::subtract"]
    ; ins = ["-"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Subtracts two integers"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.( - ) a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::multiply"]
    ; ins = ["*"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Multiplies two integers"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.( * ) a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::power"]
    ; ins = ["^"]
    ; p = [par "base" TInt; par "exponent" TInt]
    ; r = TInt
    ; d = "Raise `base` to the power of `exponent`"
    ; f =
        InProcess
          (function
          | _, [DInt base; DInt exp] ->
              DInt (Dint.pow base exp)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::divide"]
    ; ins = []
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Divides two integers"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.( / ) a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::greaterThan"]
    ; ins = [">"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is greater than b"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DBool (a > b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::greaterThanOrEqualTo"]
    ; ins = [">="]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is greater than or equal to b"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DBool (a >= b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::lessThan"]
    ; ins = ["<"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is less than b"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DBool (a < b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::lessThanOrEqualTo"]
    ; ins = ["<="]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is less than or equal to b"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DBool (a <= b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::random"]
    ; ins = []
    ; p = [par "start" TInt; par "end" TInt]
    ; r = TInt
    ; d = "Returns a random integer between a and b (inclusive)"
    ; f =
        InProcess
          (function
          (*( +1 as Random.int is exclusive *)
          | _, [DInt a; DInt b] ->
              let open Dint in
              DInt (a + one + Dint.random (b - a))
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["Int::random_v1"]
    ; ins = []
    ; p = [par "start" TInt; par "end" TInt]
    ; r = TInt
    ; d = "Returns a random integer between `start` and `end` (inclusive)."
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              let open Dint in
              (* upper+1 because as Random.int is exclusive *)
              let lower, upper =
                if a > b then (b, a + one) else (a, b + one)
              in
              DInt (lower + Dint.random (upper - lower))
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["Int::sqrt"]
    ; ins = []
    ; p = [par "a" TInt]
    ; r = TFloat
    ; d = "Get the square root of an Int"
    ; f =
        InProcess
          (function
          | _, [DInt a] -> DFloat (Dint.to_float a |> sqrt) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::toFloat"]
    ; ins = []
    ; p = [par "a" TInt]
    ; r = TFloat
    ; d = "Converts an Int to a Float"
    ; f =
        InProcess
          (function
          | _, [DInt a] -> DFloat (Dint.to_float a) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::sum"]
    ; ins = []
    ; p = [par "a" TList]
    ; r = TInt
    ; d = "Returns the sum of all the ints in the list"
    ; f =
        InProcess
          (function
          | _, [DList l] ->
              l
              |> list_coerce ~f:Dval.to_int
              >>| List.fold_left ~f:Dint.( + ) ~init:Dint.zero
              >>| (fun x -> DInt x)
              |> Result.map_error ~f:(fun (result, example_value) ->
                     RT.error
                       ~actual:(DList result)
                       ~result:(DList result)
                       ~long:
                         ( "Int::sum requires all values to be integers, but "
                         ^ Dval.to_developer_repr_v0 example_value
                         ^ " is a "
                         ^ Dval.tipename example_value )
                       ~expected:"every list item to be an int "
                       "Sum expects you to pass a list of ints" )
              |> Result.ok_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
