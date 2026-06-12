/// Builtins for the arbitrary-precision integer type `Int` (the default,
/// suffix-less integer). The polymorphic operators in NoModule.fs back the
/// infix forms (`+`, `<`, ...); these typed builtins give the
/// `Darklang.Stdlib.Int` package its named functions, mirroring `Int64`.
module Builtins.Pure.Libs.Int

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module RTE = RuntimeError

let private bigZero = System.Numerics.BigInteger.Zero
let private bigOne = System.Numerics.BigInteger.One
let private bigMinusOne = System.Numerics.BigInteger.MinusOne

let private divideByZero (vm : VMState) : 'a =
  RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.threadID

let private zeroModulus (vm : VMState) : 'a =
  RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.threadID

let private negativeModulus (vm : VMState) : 'a =
  RTE.Ints.NegativeModulus |> RTE.Int |> raiseRTE vm.threadID

let private negativeExponent (vm : VMState) : 'a =
  RTE.Ints.NegativeExponent |> RTE.Int |> raiseRTE vm.threadID

let private outOfRange (vm : VMState) : 'a =
  RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.threadID


let fns () : List<BuiltInFn> =
  [ { name = fn "intMod" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.
         The modulus <param b> must be greater than 0.
         Use <fn Int.remainder> if you want the remainder after division, which
         has a different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [ DInt a; DInt b ] ->
          let m = DarkInt.toBigInt b
          if m = bigZero then
            zeroModulus vm
          elif m < bigZero then
            negativeModulus vm
          else
            let r = DarkInt.toBigInt a % m
            Ply(Dval.int (if r < bigZero then m + r else r))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intRemainder" 0
      typeParams = []
      parameters = [ Param.make "value" TInt ""; Param.make "divisor" TInt "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Returns the integer remainder left over after dividing <param value> by
         <param divisor>, as a <type Result>. The remainder will be negative only
         if {{<var value> < 0}}. The sign of <param divisor> doesn't influence the
         outcome. Returns an {{Error}} if <param divisor> is {{0}}."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DInt value; DInt divisor ] ->
          let d = DarkInt.toBigInt divisor
          if d = bigZero then
            DString "`divisor` must be non-zero" |> resultError |> Ply
          else
            DarkInt.toBigInt value % d |> Dval.int |> resultOk |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intAdd" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Adds two arbitrary-precision integers"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DInt(DarkInt.add a b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intSubtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Subtracts two arbitrary-precision integers"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DInt(DarkInt.subtract a b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intMultiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Multiplies two arbitrary-precision integers"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DInt(DarkInt.multiply a b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intPower" 0
      typeParams = []
      parameters = [ Param.make "base" TInt ""; Param.make "exponent" TInt "" ]
      returnType = TInt
      description =
        "Raise <param base> to the power of <param exponent>. <param exponent>
         must be non-negative. The arbitrary-precision result grows as needed."
      fn =
        (function
        | _, vm, _, [ DInt b; DInt exp ] ->
          let number = DarkInt.toBigInt b
          let exp = DarkInt.toBigInt exp
          if exp < bigZero then
            negativeExponent vm
          elif exp > bigint System.Int32.MaxValue then
            // `**` needs an Int32 exponent; only trivial bases are representable.
            if number = bigZero then
              Ply(Dval.int bigZero)
            elif number = bigOne then
              Ply(Dval.int bigOne)
            elif number = bigMinusOne then
              Ply(
                Dval.int (if exp % (bigint 2) = bigZero then bigOne else bigMinusOne)
              )
            else
              outOfRange vm
          else
            Ply(Dval.int (number ** (int exp)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intDivide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TInt
      description = "Divides two arbitrary-precision integers, rounding towards zero"
      fn =
        (function
        | _, vm, _, [ DInt a; DInt b ] ->
          if DarkInt.isZero b then divideByZero vm else Ply(DInt(DarkInt.divide a b))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intNegate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TInt
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | _, _, _, [ DInt a ] -> Ply(DInt(DarkInt.negate a))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intGreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b > 0))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intGreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b >= 0))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intLessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b < 0))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intLessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt ""; Param.make "b" TInt "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DInt a; DInt b ] -> Ply(DBool(DarkInt.compare a b <= 0))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intSqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description =
        "Get the square root of an <type Int> as a <type Float>. Note that very
         large values lose precision when converted to a float."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] -> Ply(DFloat(sqrt (float (Dval.asBigInt v))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TFloat
      description =
        "Converts an <type Int> to a <type Float>. Very large values lose
         precision."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] -> Ply(DFloat(float (Dval.asBigInt v)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intParse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Returns the <type Int> value of a <type String>. Arbitrary precision, so
         the only failure is a badly-formatted string."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DString s ] ->
          match System.Numerics.BigInteger.TryParse(s) with
          | true, i -> i |> Dval.int |> resultOk |> Ply
          | false, _ -> DString "Invalid Int" |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToString" 0
      typeParams = []
      parameters = [ Param.make "int" TInt "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] -> Ply(DString(string (Dval.asBigInt v)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TInt
      description = "Converts an Int64 to an arbitrary-precision Int."
      fn =
        (function
        // An Int64 always fits Int64, so it's the `Finite` representation.
        | _, _, _, [ DInt64 a ] -> DInt(DarkInt.Finite a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts an Int to an Int64, returning None if it doesn't fit the
         64-bit range."
      fn =
        let someType = KTInt64
        (function
        // By the invariant, a `Finite` always fits Int64 and an `Infinite` never
        // does.
        | _, _, _, [ DInt(DarkInt.Finite a) ] ->
          Dval.optionSome someType (DInt64 a) |> Ply
        | _, _, _, [ DInt(DarkInt.Infinite _) ] -> Dval.optionNone someType |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
