/// StdLib functions to create and utilize tuples with 2 elements
module LibExecutionStdLib.LibTuple2

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module Interpreter = LibExecution.Interpreter

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Tuple2" "create" 0
      parameters =
        [ Param.make "first" (TVariable "a") ""
          Param.make "second" (TVariable "b") "" ]
      returnType = TTuple(TVariable "a", TVariable "b", [])
      description = "Returns a pair with the given values"
      fn =
        (function
        | state, [ first; second ] -> Ply(DTuple(first, second, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "first" 0
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "a"
      description = "Returns the first value of a pair"
      fn =
        (function
        | state, [ DTuple (first, _second, []) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "second" 0
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "b"
      description = "Returns the second value of a pair"
      fn =
        (function
        | state, [ DTuple (_first, second, []) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "swap" 0
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "b", TVariable "a", [])
      description = "Returns a pair with the elements swapped"
      fn =
        (function
        | state, [ DTuple (first, second, []) ] -> Ply(DTuple(second, first, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "mapFirst" 0
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn([ TVariable "a" ], TVariable "c"))
            ""
            [ "val" ]
          Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "c", TVariable "b", [])
      description = "Transform the first value in a pair"
      fn =
        (function
        | state, [ DFnVal fn; DTuple (first, second, []) ] ->
          uply {
            let! newFirst =
              Interpreter.applyFnVal state (id 0) fn [ first ] NotInPipe NoRail
            return DTuple(newFirst, second, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "mapSecond" 0
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn([ TVariable "b" ], TVariable "c"))
            ""
            [ "val" ]
          Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "a", TVariable "c", [])
      description = "Transform the second value in a pair"
      fn =
        (function
        | state, [ DFnVal fn; DTuple (first, second, []) ] ->
          uply {
            let! newSecond =
              Interpreter.applyFnVal state (id 0) fn [ second ] NotInPipe NoRail
            return DTuple(first, newSecond, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "mapBoth" 0
      parameters =
        [ Param.makeWithArgs
            "fnFirst"
            (TFn([ TVariable "a" ], TVariable "c"))
            "used to map the first value in the tuple"
            [ "val" ]

          Param.makeWithArgs
            "fnSecond"
            (TFn([ TVariable "b" ], TVariable "d"))
            "used to map the second value in the tuple"
            [ "val" ]

          Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "c", TVariable "d", [])
      description = "Transform both values in a pair"
      fn =
        (function
        | state, [ DFnVal fnFst; DFnVal fnSnd; DTuple (first, second, []) ] ->
          uply {
            let! newFirst =
              Interpreter.applyFnVal state (id 0) fnFst [ first ] NotInPipe NoRail

            let! newSecond =
              Interpreter.applyFnVal state (id 0) fnSnd [ second ] NotInPipe NoRail

            return DTuple(newFirst, newSecond, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
