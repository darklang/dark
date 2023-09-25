module BuiltinExecution.Libs.Math

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let varA = TVariable "a"


let modules = [ "Math" ]
let fn = fn modules
let constant = constant modules

let constants : List<BuiltInConstant> = []

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "cos" 0
      typeParams = []
      parameters = [ Param.make "angleInRadians" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the cosine of the given <param angleInRadians>.

         One interpretation of the result relates to a right triangle: the cosine is
         the ratio of the lengths of the side adjacent to the angle and the
         hypotenuse."
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Cos a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sin" 0
      typeParams = []
      parameters = [ Param.make "angleInRadians" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the sine of the given <param angleInRadians>.

         One interpretation of the result relates to a right triangle: the sine is
         the ratio of the lengths of the side opposite the angle and the hypotenuse"
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Sin a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "tan" 0
      typeParams = []
      parameters = [ Param.make "angleInRadians" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the tangent of the given <param angleInRadians>.

         One interpretation of the result relates to a right triangle: the tangent is
         the ratio of the lengths of the side opposite the angle and the side
         adjacent to the angle."
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Tan a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "acos" 0
      typeParams = []
      parameters = [ Param.make "ratio" TFloat "" ]
      returnType = TypeReference.option TFloat
      description =
        "Returns the arc cosine of <param ratio>, as an <type Option>.

         If <param ratio> is in the inclusive range {{[-1.0, 1.0]}}, returns {{Some
         result}} where <var result> is in radians and is between {{0.0}} and <fn
         Math.pi>. Otherwise, returns {{None}}.

         This function is the inverse of <fn Math.cos>."
      fn =
        (function
        | _, _, [ DFloat r ] ->
          let res = System.Math.Acos r in

          if System.Double.IsNaN res then
            Ply(Dval.optionNone VT.float)
          else
            Ply(Dval.optionSome VT.float (DFloat res))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "asin" 0
      typeParams = []
      parameters = [ Param.make "ratio" TFloat "" ]
      returnType = TypeReference.option TFloat
      description =
        "Returns the arc sine of <param ratio>, as an <type Option>.

         If <param ratio> is in the inclusive range {{[-1.0, 1.0]}}, returns {{Some
         result}} where <var result> is in radians and is between {{-Math.pi/2}} and
         {{Math.pi/2}}. Otherwise, returns {{None}}.

         This function is the inverse of <fn Math.sin>."
      fn =
        (function
        | _, _, [ DFloat r ] ->
          let res = System.Math.Asin r in

          if System.Double.IsNaN res then
            Ply(Dval.optionNone VT.float)
          else
            Ply(Dval.optionSome VT.float (DFloat res))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "atan" 0
      typeParams = []
      parameters = [ Param.make "ratio" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the arc tangent of <param ratio>. The result is in radians and is between
         {{-Math.pi/2}} and {{Math.pi/2}}.

         This function is the inverse of <fn Math.tan>. Use <fn Math.atan2> to expand the
         output range, if you know the numerator and denominator of <param ratio>."
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Atan a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "atan2" 0
      typeParams = []
      parameters = [ Param.make "y" TFloat ""; Param.make "x" TFloat "" ]
      returnType = TFloat
      description =
        "Returns the arc tangent of {{y / x}}, using the signs of <param y> and
         <param x> to determine the quadrant of the result.

         The result is in radians and is between {{-Math.pi}} and {{Math.pi}}.

         Consider <fn Math.atan> if you know the value of {{y / x}} but not the
         individual values <param x> and <param y>."
      fn =
        (function
        | _, _, [ DFloat y; DFloat x ] -> Ply(DFloat(System.Math.Atan2(y, x)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "cosh" 0
      typeParams = []
      parameters = [ Param.make "angleInRadians" TFloat "" ]
      returnType = TFloat
      description = "Returns the hyperbolic cosine of <param angleInRadians>"
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Cosh a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sinh" 0
      typeParams = []
      parameters = [ Param.make "angleInRadians" TFloat "" ]
      returnType = TFloat
      description = "Returns the hyperbolic sine of <param angleInRadians>"
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Sinh a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "tanh" 0
      typeParams = []
      parameters = [ Param.make "angleInRadians" TFloat "" ]
      returnType = TFloat
      description = "Returns the hyperbolic tangent of <param angleInRadians>"
      fn =
        (function
        | _, _, [ DFloat a ] -> Ply(DFloat(System.Math.Sinh a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
