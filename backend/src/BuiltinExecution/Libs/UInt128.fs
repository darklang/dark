module BuiltinExecution.Libs.UInt128

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module RTE = RuntimeError


module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.uint128ParseError
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "uint128Mod" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description =
        "Returns the result of wrapping <param a> around so that {{0 <= res < b}}.

        The modulus <param b> must be greater than 0.

        Use <fn UInt128.remainder> if you want the remainder after division, which has
        a different behavior for negative numbers."
      fn =
        (function
        | _, vm, _, [ DUInt128 v; DUInt128 m ] ->
          if m = System.UInt128.Zero then
            RTE.Ints.ZeroModulus |> RTE.Int |> raiseRTE vm.callStack
          else
            let result = v % m
            let result = if result < System.UInt128.Zero then m + result else result
            Ply(DUInt128(result))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128Add" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Adds two 128-bit unsigned integers together"
      fn =
        (function
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_CheckedAddition (a, b)
            Ply(DUInt128(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128Subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Subtracts two 128-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_CheckedSubtraction (a, b)
            Ply(DUInt128(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "uint128Multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Multiplies two 128-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_CheckedMultiply (a, b)
            Ply(DUInt128(result))
          with :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: add power function


    { name = fn "uint128Divide" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TUInt128
      description = "Divides two 128-bit unsigned integers"
      fn =
        (function
        | _, vm, _, [ DUInt128 a; DUInt128 b ] ->
          try
            let result = System.UInt128.op_Division (a, b)
            Ply(DUInt128(result))
          with
          | :? System.DivideByZeroException ->
            RTE.Ints.DivideByZeroError |> RTE.Int |> raiseRTE vm.callStack
          | :? System.OverflowException ->
            RTE.Ints.OutOfRange |> RTE.Int |> raiseRTE vm.callStack
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128GreaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128GreaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128LessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128LessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 ""; Param.make "b" TUInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, _, [ DUInt128 a; DUInt128 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128ToString" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TString
      description = "Converts an <type UInt128> to a <type String>"
      fn =
        (function
        | _, _, _, [ DUInt128 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128ToFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TFloat
      description = "Converts an <type UInt128> to a <type Float>"
      fn =
        (function
        | _, _, _, [ DUInt128 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128Sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt128 "" ]
      returnType = TFloat
      description = "Get the square root of an <type UInt128>"
      fn =
        (function
        | _, _, _, [ DUInt128 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128Parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TUInt128
          (TCustomType(
            Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.uint128ParseError),
            []
          ))
      description = "Returns the <type UInt128> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTUInt128 KTString
        let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.uint128ParseError
        let resultError = Dval.resultError KTUInt128 (KTCustomType(typeName, []))
        (function
        | _, _, _, [ DString s ] ->
          try
            s |> System.UInt128.Parse |> DUInt128 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128FromUInt8" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt8 "" ]
      returnType = TUInt128
      description = "Converts a UInt8 to a 128-bit unsigned integer."
      fn =
        (function
        | _, _, _, [ DUInt8 a ] -> DUInt128(System.UInt128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128FromUInt16" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt16 "" ]
      returnType = TUInt128
      description = "Converts a UInt16 to a 128-bit unsigned integer."
      fn =
        (function
        | _, _, _, [ DUInt16 a ] -> DUInt128(System.UInt128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128FromUInt32" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt32 "" ]
      returnType = TUInt128
      description = "Converts a UInt32 to a 128-bit unsigned integer."
      fn =
        (function
        | _, _, _, [ DUInt32 a ] -> DUInt128(System.UInt128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uint128FromUInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TUInt64 "" ]
      returnType = TUInt128
      description = "Converts a UInt64 to a 128-bit unsigned integer."
      fn =
        (function
        | _, _, _, [ DUInt64 a ] -> DUInt128(System.UInt128.op_Implicit a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
