module Builtins.Pure.Libs.UInt8

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError


let fns () : List<BuiltInFn> =
  [ { name = fn "uint8Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < "
        + "b}}.\n\nThe modulus <param b> must be greater than 0."
      fn =
        (function
        | _, vm, _, [| DUInt8 v; DUInt8 m |] ->
          if m = 0uy then
            RTE.BigInts.ZeroModulus |> RTE.BigInt |> raiseRTE vm.threadID
          else
            let result = v % m
            let result = if result < 0uy then m + result else result
            Ply(DUInt8(result))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Adds two 8-bit unsigned integers together, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a + b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Subtracts two 8-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a - b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Multiplies two 8-bit unsigned integers, wrapping on overflow"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a * b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8Power" 0
      typeParams = []
      parameters = [ Param.make "base" TUInt8 ""; Param.make "exponent" TUInt8 "" ]
      returnType = TUInt8
      description =
        "Raise <param base> to the power of <param exponent>. Overflow wraps "
        + "around."
      fn =
        (function
        | _, _, _, [| DUInt8 number; DUInt8 exp |] ->
          // wrap on overflow via modular exponentiation
          let m = System.Numerics.BigInteger.Pow(bigint 2, 8)
          let r = System.Numerics.BigInteger.ModPow(bigint number, bigint exp, m)
          let r = ((r % m) + m) % m
          uint8 r |> DUInt8 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Divides two 8-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [| DUInt8 a; DUInt8 b |] ->
          if b = 0uy then
            RTE.BigInts.DivideByZeroError |> RTE.BigInt |> raiseRTE vm.threadID
          else
            Ply(DUInt8(a / b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TString
      description = "Converts an <type UInt8> to a <type String>"
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TFloat
      description = "Converts an <type UInt8> to a <type Float>"
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }




    { name = fn "uint8Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt8>"
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8BitwiseAnd" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Bitwise AND on two <type UInt8> values"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a &&& b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8BitwiseOr" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Bitwise OR on two <type UInt8> values"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a ||| b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8BitwiseXor" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Bitwise XOR on two <type UInt8> values"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a ^^^ b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8BitwiseNot" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TUInt8
      description = "Bitwise NOT on a <type UInt8> value"
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> Ply(DUInt8(~~~a))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8ShiftLeft" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Bitwise left shift of a <type UInt8> value"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a <<< int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "uint8ShiftRight" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 ""; Param.make "b" TUInt8 "" ]
      returnType = TUInt8
      description = "Bitwise right shift of a <type UInt8> value"
      fn =
        (function
        | _, _, _, [| DUInt8 a; DUInt8 b |] -> Ply(DUInt8(a >>> int b))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
