module BuiltinExecution.Libs.Int16

open FSharp.Control.Tasks
open System.Threading.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []


let fn = fn [ "Int16" ]

let fns : List<BuiltInFn> =
  [ { name = fn "fromInt64" 0
      typeParams = []
      parameters = [ Param.make "a" TInt "" ]
      returnType = TInt16
      description = "Converts an int64 to int16"
      fn =
        (function
        | state, _, [ DInt a ] ->
          if a < int64 System.Int16.MinValue || a > int64 System.Int16.MaxValue then
            Int.IntRuntimeError.Error.OutOfRange
            |> Int.IntRuntimeError.RTE.toRuntimeError
            |> raiseRTE state.caller
            |> Ply
          else
            DInt16(int16 a) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
