module Builtins.Pure.Libs.Int32

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "int32Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0.\n\nUse <fn "
        + "Int32.checkedRemainder> if you want the remainder after division, which has a "
        + "different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [| DInt32 v; DInt32 m |] ->
          if m = 0 then
            RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else if m < 0 then
            RTE.BigInts.NegativeModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0 then m + result else result
            Ply(DInt32 result)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Adds two 32-bit signed integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Subtracts two 32-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Multiplies two 32-bit signed integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Power" 0
      typeParams = []
      parameters = [ Param.make "base" TInt32 ""; Param.make "exponent" TInt32 "" ]
      returnType = TInt32
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around. Raises an error if <param exponent> is negative."
      fn =
        (function
        | _, vm, _, [| DInt32 number; DInt32 exp |] ->
          if exp < 0 then
            RTE.BigInts.NegativeExponent |> RTE.BigInt |> raiseRTE vm.threadID
          else
            // wrap on overflow via modular exponentiation
            let m = System.Numerics.BigInteger.Pow(bigint 2, 32)
            let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
            let r = ((r % m) + m) % m
            let r = if r >= m / bigint 2 then r - m else r
            int32 r |> DInt32 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Divides two 32-bit signed integers"
      fn =
        (function
        | _, vm, _, [| DInt32 a; DInt32 b |] ->
          if b = 0 then
            RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID
          else if a = System.Int32.MinValue && b = -1 then
            // wraps back to MinValue (the division itself would throw)
            Ply(DInt32 System.Int32.MinValue)
          else
            Ply(DInt32(a / b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32Negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt32
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DInt32(-a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TFloat
      description = "Converts an <type Int32> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt32 "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DInt32 int |] -> Ply(DString(string int))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise AND on two <type Int32> values"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise OR on two <type Int32> values"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise XOR on two <type Int32> values"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise NOT on an <type Int32> value"
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Ply(DInt32(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise left shift of an <type Int32> value"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "int32ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 ""; Param.make "b" TInt32 "" ]
      returnType = TInt32
      description = "Bitwise right shift of an <type Int32> value"
      fn =
        (function
        | _, _, _, [| DInt32 a; DInt32 b |] -> Ply(DInt32(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
