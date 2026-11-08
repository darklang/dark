module Builtins.Pure.Libs.Int8

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "int8Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "Int8.checkedRemainder> if you want the remainder after division, which has a "
        + "different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DInt8 v; DInt8 m |] ->
          if m = 0y then
            RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else if m < 0y then
            RTE.BigInts.NegativeModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0y then m + result else result
            Ply(DInt8 result)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Adds two 8-bit signed integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Subtracts two 8-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Multiplies two 8-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt8 ""; Param.make "exponent" TInt8 "" ]
      returnType = TInt8
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around. Raises an error if <param exponent> is negative."
      fn =
        (function
        | _, vm, _, [| DInt8 number; DInt8 exp |] ->
          if exp < 0y then
            RTE.BigInts.NegativeExponent |> RTE.BigInt |> raiseRTE vm.threadID
          else
            // wrap on overflow via modular exponentiation
            let m = System.Numerics.BigInteger.Pow(bigint 2, 8)
            let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
            let r = ((r % m) + m) % m
            let r = if r >= m / bigint 2 then r - m else r
            int8 r |> DInt8 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Divides two 8-bit signed integers"
      fn =
        (function
        | _, vm, _, [| DInt8 a; DInt8 b |] ->
          if b = 0y then
            RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID
          else
            // widen to int32 so -128y / -1y wraps on narrowing instead of throwing
            Ply(DInt8(int8 (int a / int b)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt8
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DInt8(-a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TString
      description = "Converts an <type Int8> to a <type String>"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TFloat
      description = "Converts an <type Int8> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Bitwise AND on two <type Int8> values"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Bitwise OR on two <type Int8> values"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Bitwise XOR on two <type Int8> values"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt8
      description = "Bitwise NOT on an <type Int8> value"
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Ply(DInt8(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Bitwise left shift of an <type Int8> value"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int8ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 ""; Param.make "b" TInt8 "" ]
      returnType = TInt8
      description = "Bitwise right shift of an <type Int8> value"
      fn =
        (function
        | _, _, _, [| DInt8 a; DInt8 b |] -> Ply(DInt8(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
