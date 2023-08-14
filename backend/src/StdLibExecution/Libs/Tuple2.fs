/// StdLib functions to create and utilize tuples with 2 elements
module StdLibExecution.Libs.Tuple2

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth
open LibExecution.StdLib.Shortcuts

module Interpreter = LibExecution.Interpreter
module StdLib = LibExecution.StdLib

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Tuple2" ]

let fns : List<BuiltInFn> =
  [ { name = fn "create" 0
      typeParams = []
      parameters =
        [ Param.make "first" (TVariable "a") ""
          Param.make "second" (TVariable "b") "" ]
      returnType = TTuple(TVariable "a", TVariable "b", [])
      description = "Returns a pair with the given values"
      fn =
        (function
        | _, _, [ first; second ] -> Ply(DTuple(first, second, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "first" 0
      typeParams = []
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "a"
      description = "Returns the first value of a pair"
      fn =
        (function
        | _, _, [ DTuple(first, _second, []) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "second" 0
      typeParams = []
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "b"
      description = "Returns the second value of a pair"
      fn =
        (function
        | _, _, [ DTuple(_first, second, []) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "swap" 0
      typeParams = []
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "b", TVariable "a", [])
      description = "Returns a pair with the elements swapped"
      fn =
        (function
        | _, _, [ DTuple(first, second, []) ] -> Ply(DTuple(second, first, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "mapFirst" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton (TVariable "a"), TVariable "c"))
            ""
            [ "val" ]
          Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "c", TVariable "b", [])
      description = "Transform the first value in a pair"
      fn =
        (function
        | state, _, [ DFnVal fn; DTuple(first, second, []) ] ->
          uply {
            let args = NEList.singleton first
            let! newFirst = Interpreter.applyFnVal state 0UL fn [] args
            return DTuple(newFirst, second, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "mapSecond" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton (TVariable "b"), TVariable "c"))
            ""
            [ "val" ]
          Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "a", TVariable "c", [])
      description = "Transform the second value in a pair"
      fn =
        (function
        | state, _, [ DFnVal fn; DTuple(first, second, []) ] ->
          uply {
            let args = NEList.singleton second
            let! newSecond = Interpreter.applyFnVal state 0UL fn [] args
            return DTuple(first, newSecond, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "mapBoth" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs
            "fnFirst"
            (TFn(NEList.singleton (TVariable "a"), TVariable "c"))
            "used to map the first value in the tuple"
            [ "val" ]

          Param.makeWithArgs
            "fnSecond"
            (TFn(NEList.singleton (TVariable "b"), TVariable "d"))
            "used to map the second value in the tuple"
            [ "val" ]

          Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "c", TVariable "d", [])
      description = "Transform both values in a pair"
      fn =
        (function
        | state, _, [ DFnVal fnFst; DFnVal fnSnd; DTuple(first, second, []) ] ->
          uply {
            let args = NEList.singleton first
            let! newFirst = Interpreter.applyFnVal state 0UL fnFst [] args
            let args = NEList.singleton second
            let! newSecond = Interpreter.applyFnVal state 0UL fnSnd [] args

            return DTuple(newFirst, newSecond, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
