module BuiltinExecution.Libs.Int128

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let types : List<BuiltInType> = []

let modules = [ "Int128" ]
let fn = fn modules
let constant = constant modules

let constants : List<BuiltInConstant> =
  [ { name = constant "zero" 0
      typ = TInt128
      description = "The value 0"
      body = DInt128(System.Int128.Zero)
      deprecated = NotDeprecated } ]

module ParseError =
  type ParseError =
    | BadFormat
    | OutOfRange

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | BadFormat -> "BadFormat", []
      | OutOfRange -> "OutOfRange", []

    let typeName =
      TypeName.fqPackage "Darklang" [ "Stdlib"; "Int128" ] "ParseError" 0
    DEnum(typeName, typeName, [], caseName, fields)


let fns : List<BuiltInFn> =
  [ { name = fn "add" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Adds two 16 bits signed integers together"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_CheckedAddition (a, b)
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "subtract" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Subtracts two 128 bits signed integers"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_CheckedSubtraction (a, b)
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "multiply" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Multiplies two 128 bits signed integers"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_CheckedMultiply (a, b)
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "divide" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TInt128
      description = "Divides two 8 bits signed integers"
      fn =
        (function
        | state, _, [ DInt128 a; DInt128 b ] ->
          try
            let result = System.Int128.op_Division (a, b)
            Ply(DInt128(result))
          with
          | :? System.DivideByZeroException ->
            Int.IntRuntimeError.Error.DivideByZeroError
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          | :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "negate" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TInt128
      description = "Returns the negation of <param a>, {{-a}}"
      fn =
        (function
        | state, _, [ DInt128 a ] ->
          try
            let result = System.Int128.op_CheckedUnaryNegation a
            Ply(DInt128(result))
          with :? System.OverflowException ->
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }



    { name = fn "greaterThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is greater than <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a > b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "greaterThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is greater than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a >= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThan" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description = "Returns {{true}} if <param a> is less than <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a < b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "lessThanOrEqualTo" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 ""; Param.make "b" TInt128 "" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param a> is less than or equal to <param b>"
      fn =
        (function
        | _, _, [ DInt128 a; DInt128 b ] -> Ply(DBool(a <= b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toString" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TString
      description = "Converts an <type Int128> to a <type String>"
      fn =
        (function
        | _, _, [ DInt128 a ] -> Ply(DString(string a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toFloat" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat
      description = "Converts an <type Int128> to a <type Float>"
      fn =
        (function
        | _, _, [ DInt128 a ] -> Ply(DFloat(float a))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sqrt" 0
      typeParams = []
      parameters = [ Param.make "a" TInt128 "" ]
      returnType = TFloat
      description = "Get the square root of an <type Int128>"
      fn =
        (function
        | _, _, [ DInt128 a ] -> Ply(DFloat(sqrt (float a)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType =
        TypeReference.result
          TInt128
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "Int128" ]
                  name = TypeName.TypeName "ParseError"
                  version = 0 }
            ),
            []
          ))
      description = "Returns the <type Int128> value of a <type String>"
      fn =
        let resultOk = Dval.resultOk KTInt128 KTString
        let typeName = RuntimeError.name [ "Int128" ] "ParseError" 0
        let resultError = Dval.resultError KTInt128 (KTCustomType(typeName, []))
        (function
        | _, _, [ DString s ] ->
          try
            s |> System.Int128.Parse |> DInt128 |> resultOk |> Ply
          with
          | :? System.FormatException ->
            ParseError.BadFormat |> ParseError.toDT |> resultError |> Ply
          | :? System.OverflowException ->
            ParseError.OutOfRange |> ParseError.toDT |> resultError |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
