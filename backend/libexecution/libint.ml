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
  [ { prefix_names = ["Int::mod"]
    ; infix_names = ["%"]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TInt
    ; description =
        "Return `a` % `b`, the modulus of a and b. This is the integer remainder left when `a` is divided by `b`. For example, `15 % 6 = 3`."
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.( % ) a b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::add"]
    ; infix_names = ["+"]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TInt
    ; description = "Adds two integers together"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DInt (Dint.( + ) a b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::add to add Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::add to add Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [(DStr _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a String, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use ++ or String::append to join Strings or parse Strings to Ints with String::toInt."
                )
          | state, [_; (DStr _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a String, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use ++ or String::append to join Strings or parse Strings to Ints with String::toInt."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::subtract"]
    ; infix_names = ["-"]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TInt
    ; description = "Subtracts two integers"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DInt (Dint.( - ) a b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::subtract to subtract Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::subtract to subtract Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::multiply"]
    ; infix_names = ["*"]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TInt
    ; description = "Multiplies two integers"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DInt (Dint.( * ) a b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::multiply to multiply Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::multiply to multiply Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::power"]
    ; infix_names = ["^"]
    ; parameters = [par "base" TInt; par "exponent" TInt]
    ; return_type = TInt
    ; description = "Raise `base` to the power of `exponent`"
    ; func =
        InProcess
          (function
          | _, [DInt base; DInt exp] ->
              DInt (Dint.pow base exp)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::divide"]
    ; infix_names = []
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TInt
    ; description = "Divides two integers"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DInt (Dint.( / ) a b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::divide to divide Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::divide to divide Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::greaterThan"]
    ; infix_names = [">"]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TBool
    ; description = "Returns true if a is greater than b"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DBool (a > b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::greaterThan to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::greaterThan to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::greaterThanOrEqualTo"]
    ; infix_names = [">="]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TBool
    ; description = "Returns true if a is greater than or equal to b"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DBool (a >= b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::greaterThanOrEqualTo to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::greaterThanOrEqualTo to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::lessThan"]
    ; infix_names = ["<"]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TBool
    ; description = "Returns true if a is less than b"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DBool (a < b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::lessThan to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::lessThan to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::lessThanOrEqualTo"]
    ; infix_names = ["<="]
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TBool
    ; description = "Returns true if a is less than or equal to b"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              DBool (a <= b)
          | state, [(DFloat _ as a); _] ->
              DError
                ( SourceNone
                , "The first param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::lessThanOrEqualTo to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | state, [_; (DFloat _ as b)] ->
              DError
                ( SourceNone
                , "The second param ("
                  ^ Dval.to_developer_repr_v0 b
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::lessThanOrEqualTo to compare Floats or use Float::floor to truncate Floats to Ints."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::random"]
    ; infix_names = []
    ; parameters = [par "start" TInt; par "end" TInt]
    ; return_type = TInt
    ; description = "Returns a random integer between a and b (inclusive)"
    ; func =
        InProcess
          (function
          (*( +1 as Random.int is exclusive *)
          | _, [DInt a; DInt b] ->
              let open Dint in
              DInt (a + one + Dint.random (b - a))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["Int::random_v1"]
    ; infix_names = []
    ; parameters = [par "start" TInt; par "end" TInt]
    ; return_type = TInt
    ; description =
        "Returns a random integer between `start` and `end` (inclusive)."
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] ->
              let open Dint in
              (* upper+1 because as Random.int is exclusive *)
              let lower, upper = if a > b then (b, a + one) else (a, b + one) in
              DInt (lower + Dint.random (upper - lower))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["Int::sqrt"]
    ; infix_names = []
    ; parameters = [par "a" TInt]
    ; return_type = TFloat
    ; description = "Get the square root of an Int"
    ; func =
        InProcess
          (function
          | _, [DInt a] ->
              DFloat (Dint.to_float a |> sqrt)
          | state, [(DFloat _ as a)] ->
              DError
                ( SourceNone
                , "The param ("
                  ^ Dval.to_developer_repr_v0 a
                  ^ ") is a Float, but "
                  ^ state.executing_fnname
                  ^ " only works on Ints. Use Float::sqrt to take the square root of Floats or use Float::floor to truncate the Float to an Int."
                )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::toFloat"]
    ; infix_names = []
    ; parameters = [par "a" TInt]
    ; return_type = TFloat
    ; description = "Converts an Int to a Float"
    ; func =
        InProcess
          (function
          | _, [DInt a] -> DFloat (Dint.to_float a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::sum"]
    ; infix_names = []
    ; parameters = [par "a" TList]
    ; return_type = TInt
    ; description = "Returns the sum of all the ints in the list"
    ; func =
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
                         ^ Dval.pretty_tipename example_value )
                       ~expected:"every list item to be an int "
                       "Sum expects you to pass a list of ints")
              |> Result.ok_exn
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::max"]
    ; infix_names = []
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TBool
    ; description = "Returns the higher of a and b"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.max a b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Int::min"]
    ; infix_names = []
    ; parameters = [par "a" TInt; par "b" TInt]
    ; return_type = TBool
    ; description = "Returns the lower of `a` and `b`"
    ; func =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (Dint.min a b) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
