/// Builtins for the arbitrary-precision integer type `BigInt`, written `1z`: parsing, printing and
/// conversions. Its operators (`+`, `<`, ...) are the polymorphic ones in NoModule.fs.
module Builtins.Pure.Libs.BigInt

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

    let typeName = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.bigIntParseError ())
    DEnum(typeName, typeName, [], caseName, fields)


// BigInteger <-> 128-bit conversions (no `bigint`/cast operator covers these)
let private i128ToBig (a : System.Int128) : bigint =
  System.Numerics.BigInteger.op_Implicit a
let private u128ToBig (a : System.UInt128) : bigint =
  System.Numerics.BigInteger.op_Implicit a
let private bigToI128 (b : bigint) : System.Int128 =
  System.Numerics.BigInteger.op_Explicit b
let private bigToU128 (b : bigint) : System.UInt128 =
  System.Numerics.BigInteger.op_Explicit b

/// `BigInt` -> a fixed-width type: `Some` if the value fits `[lo, hi]`, else `None`.
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
  [ { name = fn "bigIntToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TFloat
      description =
        "Converts a <type BigInt> to a <type Float>. Very large values lose "
        + "precision."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] -> Ply(DFloat(float (Dval.asBigInt v)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TFloat "" ]
      returnType = TBigInt
      description =
        "Converts a <type Float> to a <type BigInt>, truncating toward zero. "
        + "Unlike going via Int64, this preserves the full integer part of the "
        + "float (no 64-bit clamp)."
      fn =
        (function
        // `bigint f` truncates toward zero; `roundedToBigInt` adds the NaN/Infinity
        // guard (shared with the Float builtins) so those don't throw a host exn.
        | _, vm, _, [| DFloat f |] -> roundedToBigInt vm f
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntParse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        let errorType =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.bigIntParseError ())
        TypeReference.result TBigInt (TCustomType(NR.ok errorType, []))
      description =
        "Returns the <type BigInt> value of a <type String>. Arbitrary precision, "
        + "so the only failure is a badly-formatted string."
      fn =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.bigIntParseError ())
        let resultOk = Dval.resultOk KTBigInt (KTCustomType(typeName, []))
        let resultError = Dval.resultError KTBigInt (KTCustomType(typeName, []))
        (function
        | _, _, _, [| DString s |] ->
          match System.Numerics.BigInteger.TryParse(s) with
          | true, i -> i |> Dval.bigInt |> resultOk |> Ply
          | false, _ -> ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToString" 0
      typeParams = []
      parameters = [ Param.make "int" TBigInt "" ]
      returnType = TString
      description = "Stringify <param int>"
      fn =
        (function
        | _, _, _, [| DBigInt i |] -> Ply(DString(DarkBigInt.toString i))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Conversions from the fixed-width integer types into `BigInt`. Every
    // fixed-width value fits, so these are total.

    { name = fn "bigIntFromInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TInt8 "" ]
      returnType = TBigInt
      description = "Converts an Int8 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DInt8 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TBigInt
      description = "Converts a UInt8 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DUInt8 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TInt16 "" ]
      returnType = TBigInt
      description = "Converts an Int16 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DInt16 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TBigInt
      description = "Converts a UInt16 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DUInt16 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TInt32 "" ]
      returnType = TBigInt
      description = "Converts an Int32 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DInt32 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TBigInt
      description = "Converts a UInt32 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DUInt32 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt64 "" ]
      returnType = TBigInt
      description = "Converts an Int64 to an arbitrary-precision BigInt."
      fn =
        (function
        // An Int64 is always in `DarkBigInt.Finite` range.
        | _, _, _, [| DInt64 a |] -> Dval.dbigint (DarkBigInt.Finite a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TBigInt
      description = "Converts a UInt64 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DUInt64 a |] -> Dval.bigInt (bigint a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TBigInt
      description = "Converts an Int128 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DInt128 a |] -> Dval.bigInt (i128ToBig a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntFromUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TBigInt
      description = "Converts a UInt128 to an arbitrary-precision BigInt."
      fn =
        (function
        | _, _, _, [| DUInt128 a |] -> Dval.bigInt (u128ToBig a) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Conversions from `BigInt` into the fixed-width integer types. The value may
    // not fit the target range, so each returns an `Option`.

    { name = fn "bigIntToInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TInt8
      description = "Converts a BigInt to an Int8, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTInt8
            (bigint System.SByte.MinValue)
            (bigint System.SByte.MaxValue)
            (fun b -> Dval.int8 (sbyte b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TUInt8
      description = "Converts a BigInt to a UInt8, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTUInt8
            (bigint System.Byte.MinValue)
            (bigint System.Byte.MaxValue)
            (fun b -> Dval.uint8 (byte b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TInt16
      description =
        "Converts a BigInt to an Int16, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTInt16
            (bigint System.Int16.MinValue)
            (bigint System.Int16.MaxValue)
            (fun b -> DInt16(int16 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TUInt16
      description =
        "Converts a BigInt to a UInt16, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTUInt16
            (bigint System.UInt16.MinValue)
            (bigint System.UInt16.MaxValue)
            (fun b -> DUInt16(uint16 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TInt32
      description =
        "Converts a BigInt to an Int32, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTInt32
            (bigint System.Int32.MinValue)
            (bigint System.Int32.MaxValue)
            (fun b -> DInt32(int32 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TUInt32
      description =
        "Converts a BigInt to a UInt32, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTUInt32
            (bigint System.UInt32.MinValue)
            (bigint System.UInt32.MaxValue)
            (fun b -> DUInt32(uint32 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TInt64
      description =
        "Converts a BigInt to an Int64, returning None if it doesn't fit the "
        + "64-bit range."
      fn =
        let someType = KTInt64
        (function
        // By the invariant, a `Finite` always fits Int64 and an `Infinite` never
        // does.
        | _, _, _, [| DBigInt(DarkBigInt.Finite a) |] ->
          Dval.optionSome someType (DInt64 a) |> Ply
        | _, _, _, [| DBigInt(DarkBigInt.Infinite _) |] ->
          Dval.optionNone someType |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TUInt64
      description =
        "Converts a BigInt to a UInt64, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTUInt64
            (bigint System.UInt64.MinValue)
            (bigint System.UInt64.MaxValue)
            (fun b -> DUInt64(uint64 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TInt128
      description =
        "Converts a BigInt to an Int128, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTInt128
            (i128ToBig System.Int128.MinValue)
            (i128ToBig System.Int128.MaxValue)
            (fun b -> DInt128(bigToI128 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bigIntToUInt128" 0
      typeParams = []
      parameters = [ Param.make "a" TBigInt "" ]
      returnType = TypeReference.option TUInt128
      description =
        "Converts a BigInt to a UInt128, returning None if it doesn't fit."
      fn =
        (function
        | _, _, _, [| (DBigInt _) as v |] ->
          toFixed
            KTUInt128
            (u128ToBig System.UInt128.MinValue)
            (u128ToBig System.UInt128.MaxValue)
            (fun b -> DUInt128(bigToU128 b))
            v
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
