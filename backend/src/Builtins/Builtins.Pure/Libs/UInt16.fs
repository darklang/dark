module Builtins.Pure.Libs.UInt16

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint16Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0."
      fn =
        (function
        | _, vm, _, [| DUInt16 v; DUInt16 m |] ->
          if m = 0us then
            RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0us then m + result else result
            Ply(DUInt16(result))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Adds two 16-bit unsigned integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Subtracts two 16-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Multiplies two 16-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt16 ""; Param.make "exponent" TUInt16 "" ]
      returnType = TUInt16
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around."
      fn =
        (function
        | _, _, _, [| DUInt16 number; DUInt16 exp |] ->
          // wrap on overflow via modular exponentiation
          let m = System.Numerics.BigInteger.Pow(bigint 2, 16)
          let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
          let r = ((r % m) + m) % m
          uint16 r |> DUInt16 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Divides two 16-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [| DUInt16 a; DUInt16 b |] ->
          if b = 0us then
            RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID
          else
            Ply(DUInt16(a / b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TString
      description = "Stringify <param uint16>"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Converts an <type UInt16> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt16>"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise AND on two <type UInt16> values"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise OR on two <type UInt16> values"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise XOR on two <type UInt16> values"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise NOT on a <type UInt16> value"
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Ply(DUInt16(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise left shift of a <type UInt16> value"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint16ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 ""; Param.make "b" TUInt16 "" ]
      returnType = TUInt16
      description = "Bitwise right shift of a <type UInt16> value"
      fn =
        (function
        | _, _, _, [| DUInt16 a; DUInt16 b |] -> Ply(DUInt16(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
