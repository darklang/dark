open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let pi =
  (* approximation of π, as a double-precision float, which is what DFloat stores *)
  3.141592653589793


let tau =
  (* approximation of τ, as a double-precision float, which is what DFloat stores *)
  6.283185307179586


let fns : fn list =
  [ { prefix_names = ["Math::pi"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TFloat
    ; description =
        "Returns an approximation for the mathematical constant π, the ratio of a circle's circumference to its diameter."
    ; func = InProcess (function _, [] -> DFloat pi | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::tau"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TFloat
    ; description =
        "Returns an approximation for the mathematical constant τ, the number of radians in one turn. Equivalent to `Float::multiply Math::pi 2`."
    ; func = InProcess (function _, [] -> DFloat tau | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::degrees"]
    ; infix_names = []
    ; parameters = [par "angleInDegrees" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the equivalent of `angleInDegrees` in radians, the unit used by all of Dark's trigonometry functions.
         There are 360 degrees in a circle."
    ; func =
        InProcess
          (function
          | _, [DFloat degrees] ->
              DFloat (degrees *. pi /. 180.0)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::turns"]
    ; infix_names = []
    ; parameters = [par "angleInTurns" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the equivalent of `angleInTurns` in radians, the unit used by all of Dark's trigonometry functions.
         There is 1 turn in a circle."
    ; func =
        InProcess
          (function
          | _, [DFloat turns] -> DFloat (tau *. turns) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::radians"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns `angleInRadians` in radians, the unit used by all of Dark's trigonometry functions.
        There are `Float::multiply 2 Math::pi` radians in a circle."
    ; func =
        InProcess
          (function _, [DFloat rads] -> DFloat rads | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::cos"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the cosine of the given `angleInRadians`.
         One interpretation of the result relates to a right triangle: the cosine is the ratio of the lengths of the side adjacent to the angle and the hypotenuse."
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.cos a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::sin"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the sine of the given `angleInRadians`.
         One interpretation of the result relates to a right triangle: the sine is the ratio of the lengths of the side opposite the angle and the hypotenuse."
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.sin a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::tan"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the tangent of the given `angleInRadians`.
         One interpretation of the result relates to a right triangle: the tangent is the ratio of the lengths of the side opposite the angle and the side adjacent to the angle."
    ; func =
        InProcess
          (function _, [DFloat a] -> DFloat (Float.tan a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::acos"]
    ; infix_names = []
    ; parameters = [par "ratio" TFloat]
    ; return_type = TOption
    ; description =
        "Returns the arc cosine of `ratio`, as an Option.
         If `ratio` is in the inclusive range `[-1.0, 1.0]`, returns
         `Just result` where `result` is in radians and is between `0.0` and `Math::pi`. Otherwise, returns `Nothing`.
         This function is the inverse of `Math::cos`."
    ; func =
        InProcess
          (function
          | _, [DFloat r] ->
              let res = Float.acos r in
              if Float.is_nan res
              then DOption OptNothing
              else DOption (OptJust (DFloat res))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::asin"]
    ; infix_names = []
    ; parameters = [par "ratio" TFloat]
    ; return_type = TOption
    ; description =
        "Returns the arc sine of `ratio`, as an Option.
         If `ratio` is in the inclusive range `[-1.0, 1.0]`, returns
         `Just result` where `result` is in radians and is between `-Math::pi/2` and `Math::pi/2`. Otherwise, returns `Nothing`.
         This function is the inverse of `Math::sin`."
    ; func =
        InProcess
          (function
          | _, [DFloat r] ->
              let res = Float.asin r in
              if Float.is_nan res
              then DOption OptNothing
              else DOption (OptJust (DFloat res))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::atan"]
    ; infix_names = []
    ; parameters = [par "ratio" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the arc tangent of `ratio`. The result is in radians and is between `-Math::pi/2` and `Math::pi/2`.
         This function is the inverse of `Math::tan`. Use `Math::atan2` to expand the output range, if you know the numerator and denominator of `ratio`."
    ; func =
        InProcess
          (function
          | _, [DFloat a] -> DFloat (Float.atan a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::atan2"]
    ; infix_names = []
    ; parameters = [par "y" TFloat; par "x" TFloat]
    ; return_type = TFloat
    ; description =
        "Returns the arc tangent of `y / x`, using the signs of `y` and `x` to determine the quadrant of the result.
         The result is in radians and is between `-Math::pi` and `Math::pi`. Consider `Math::atan` if you know the value of `y / x` but not the individual values `x` and `y`."
    ; func =
        InProcess
          (function
          | _, [DFloat y; DFloat x] ->
              DFloat (Float.atan2 y x)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::cosh"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description = "Returns the hyperbolic cosine of `angleInRadians`."
    ; func =
        InProcess
          (function
          | _, [DFloat a] -> DFloat (Float.cosh a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::sinh"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description = "Returns the hyperbolic sine of `angleInRadians`."
    ; func =
        InProcess
          (function
          | _, [DFloat a] -> DFloat (Float.sinh a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["Math::tanh"]
    ; infix_names = []
    ; parameters = [par "angleInRadians" TFloat]
    ; return_type = TFloat
    ; description = "Returns the hyperbolic tangent of `angleInRadians`."
    ; func =
        InProcess
          (function
          | _, [DFloat a] -> DFloat (Float.sinh a) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
