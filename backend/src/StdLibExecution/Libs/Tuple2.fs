/// StdLib functions to create and utilize tuples with 2 elements
module StdLibExecution.Libs.Tuple2

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth
open LibExecution.StdLib.Shortcuts

module Interpreter = LibExecution.Interpreter
module StdLib = LibExecution.StdLib

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "Tuple2" "create" 0
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


    { name = fn "Tuple2" "first" 0
      typeParams = []
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "a"
      description = "Returns the first value of a pair"
      fn =
        (function
        | _, _, [ DTuple (first, _second, []) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "second" 0
      typeParams = []
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "b"
      description = "Returns the second value of a pair"
      fn =
        (function
        | _, _, [ DTuple (_first, second, []) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "swap" 0
      typeParams = []
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TTuple(TVariable "b", TVariable "a", [])
      description = "Returns a pair with the elements swapped"
      fn =
        (function
        | _, _, [ DTuple (first, second, []) ] -> Ply(DTuple(second, first, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "mapFirst" 0
      typeParams = []
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
        | state, _, [ DFnVal fn; DTuple (first, second, []) ] ->
          uply {
            let! newFirst = Interpreter.applyFnVal state fn [ first ]
            return DTuple(newFirst, second, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "mapSecond" 0
      typeParams = []
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
        | state, _, [ DFnVal fn; DTuple (first, second, []) ] ->
          uply {
            let! newSecond = Interpreter.applyFnVal state fn [ second ]
            return DTuple(first, newSecond, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "mapBoth" 0
      typeParams = []
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
        | state, _, [ DFnVal fnFst; DFnVal fnSnd; DTuple (first, second, []) ] ->
          uply {
            let! newFirst = Interpreter.applyFnVal state fnFst [ first ]

            let! newSecond = Interpreter.applyFnVal state fnSnd [ second ]

            return DTuple(newFirst, newSecond, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
