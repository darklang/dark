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
module PackageRefs = LibExecution.PackageRefs
module RTE = RuntimeError
module NR = LibExecution.RuntimeTypes.NameResolution

module ParseError =
  type ParseError = | BadFormat

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.intParseError ())
    DEnum(typeName, typeName, [], caseName, fields)


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

// BigInteger <-> 128-bit conversions (no `bigint`/cast operator covers these)
let private i128ToBig (a : System.Int128) : bigint =
  System.Numerics.BigInteger.op_Implicit a
let private u128ToBig (a : System.UInt128) : bigint =
  System.Numerics.BigInteger.op_Implicit a
let private bigToI128 (b : bigint) : System.Int128 =
  System.Numerics.BigInteger.op_Explicit b
let private bigToU128 (b : bigint) : System.UInt128 =
  System.Numerics.BigInteger.op_Explicit b

/// `Int` -> a fixed-width type: `Some` if the value fits `[lo, hi]`, else `None`.
let private toFixed
  (resultType : KnownType)
  (lo : bigint)
  (hi : bigint)
  (make : bigint -> Dval)
  (v : Dval)
  : Ply<Dval> =
  let b = Dval.asBigInt v
  if b >= lo && b <= hi then
    Dval.optionSome resultType (make b) |> Ply
  else
    Dval.optionNone resultType |> Ply


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
        (function
        | _, vm, _, [ DInt value; DInt divisor ] ->
          let d = DarkInt.toBigInt divisor
          if d = bigZero then
            divideByZero vm
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
      returnType =
        let errorType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.intParseError ())
        TypeReference.result TInt (TCustomType(NR.ok errorType, []))
      description =
        "Returns the <type Int> value of a <type String>. Arbitrary precision, so
         the only failure is a badly-formatted string."
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.intParseError ())
        let resultOk = Dval.resultOk KTInt (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTInt (KTCustomType(typeName, []))
        (function
        | _, _, _, [ DString s ] ->
          match System.Numerics.BigInteger.TryParse(s) with
          | true, i -> i |> Dval.int |> resultOk |> Ply
          | false, _ -> ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
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


    // Conversions from the fixed-width integer types into `Int`. Every
    // fixed-width value fits, so these are total.

    { name = fn "intFromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TInt
      description = "Converts an Int8 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DInt8 a ] -> Dval.int (bigint a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TInt
      description = "Converts a UInt8 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DUInt8 a ] -> Dval.int (bigint a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TInt
      description = "Converts an Int16 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DInt16 a ] -> Dval.int (bigint a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TInt
      description = "Converts a UInt16 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> Dval.int (bigint a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TInt
      description = "Converts an Int32 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DInt32 a ] -> Dval.int (bigint a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TInt
      description = "Converts a UInt32 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DUInt32 a ] -> Dval.int (bigint a) |> Ply
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
        // An Int64 is always in `DarkInt.Finite` range.
        | _, _, _, [ DInt64 a ] -> DInt(DarkInt.Finite a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TInt
      description = "Converts a UInt64 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DUInt64 a ] -> Dval.int (bigint a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TInt
      description = "Converts an Int128 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DInt128 a ] -> Dval.int (i128ToBig a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intFromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TInt
      description = "Converts a UInt128 to an arbitrary-precision Int."
      fn =
        (function
        | _, _, _, [ DUInt128 a ] -> Dval.int (u128ToBig a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Conversions from `Int` into the fixed-width integer types. The value may
    // not fit the target range, so each returns an `Option`.

    { name = fn "intToInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TInt8
      description = "Converts an Int to an Int8, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTInt8
            (bigint System.SByte.MinValue)
            (bigint System.SByte.MaxValue)
            (fun b -> DInt8(sbyte b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TUInt8
      description = "Converts an Int to a UInt8, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTUInt8
            (bigint System.Byte.MinValue)
            (bigint System.Byte.MaxValue)
            (fun b -> DUInt8(byte b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TInt16
      description = "Converts an Int to an Int16, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTInt16
            (bigint System.Int16.MinValue)
            (bigint System.Int16.MaxValue)
            (fun b -> DInt16(int16 b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TUInt16
      description = "Converts an Int to a UInt16, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTUInt16
            (bigint System.UInt16.MinValue)
            (bigint System.UInt16.MaxValue)
            (fun b -> DUInt16(uint16 b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TInt32
      description = "Converts an Int to an Int32, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTInt32
            (bigint System.Int32.MinValue)
            (bigint System.Int32.MaxValue)
            (fun b -> DInt32(int32 b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TUInt32
      description = "Converts an Int to a UInt32, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTUInt32
            (bigint System.UInt32.MinValue)
            (bigint System.UInt32.MaxValue)
            (fun b -> DUInt32(uint32 b))
            v
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
      deprecated = NotDeprecated }


    { name = fn "intToUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TUInt64
      description = "Converts an Int to a UInt64, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTUInt64
            (bigint System.UInt64.MinValue)
            (bigint System.UInt64.MaxValue)
            (fun b -> DUInt64(uint64 b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TInt128
      description = "Converts an Int to an Int128, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTInt128
            (i128ToBig System.Int128.MinValue)
            (i128ToBig System.Int128.MaxValue)
            (fun b -> DInt128(bigToI128 b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "intToUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TypeReference.option TUInt128
      description = "Converts an Int to a UInt128, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [ (DInt _) as v ] ->
          toFixed
            KTUInt128
            (u128ToBig System.UInt128.MinValue)
            (u128ToBig System.UInt128.MaxValue)
            (fun b -> DUInt128(bigToU128 b))
            v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
