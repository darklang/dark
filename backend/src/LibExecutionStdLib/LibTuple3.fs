/// StdLib functions to create and utilize tuples with 3 elements
module LibExecutionStdLib.LibTuple3

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module Interpreter = LibExecution.Interpreter

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Tuple3" "create" 0
      parameters =
        [ Param.make "first" (TVariable "a") ""
          Param.make "second" (TVariable "b") ""
          Param.make "third" (TVariable "c") "" ]
      returnType = TTuple(TVariable "a", TVariable "b", [ TVariable "c" ])
      description = "Returns a triple with the given values"
      fn =
        (function
        | state, [ first; second; third ] -> Ply(DTuple(first, second, [ third ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "first" 0
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "a"
      description = "Returns the first value of a triple"
      fn =
        (function
        | state, [ DTuple (first, _second, [ _third ]) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "second" 0
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "b"
      description = "Returns the second value of a triple"
      fn =
        (function
        | state, [ DTuple (_first, second, [ _third ]) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "third" 0
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "c"
      description = "Returns the third value of a triple"
      fn =
        (function
        | state, [ DTuple (_first, _second, [ third ]) ] -> Ply(third)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "mapFirst" 0
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn([ TVariable "a" ], TVariable "d"))
            ""
            [ "val" ]
          Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TTuple(TVariable "d", TVariable "b", [ TVariable "c" ])
      description = "Transform the first value in a triple"
      fn =
        (function
        | state, [ DFnVal fn; DTuple (first, second, [ third ]) ] ->
          uply {
            let! newFirst =
              Interpreter.applyFnVal state (id 0) fn [ first ] NotInPipe NoRail
            return DTuple(newFirst, second, [ third ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "mapSecond" 0
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn([ TVariable "b" ], TVariable "d"))
            ""
            [ "val" ]
          Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TTuple(TVariable "a", TVariable "d", [ TVariable "c" ])
      description = "Transform the second value in a triple"
      fn =
        (function
        | state, [ DFnVal fn; DTuple (first, second, [ third ]) ] ->
          uply {
            let! newSecond =
              Interpreter.applyFnVal state (id 0) fn [ second ] NotInPipe NoRail
            return DTuple(first, newSecond, [ third ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "mapThird" 0
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn([ TVariable "c" ], TVariable "d"))
            ""
            [ "val" ]
          Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TTuple(TVariable "a", TVariable "b", [ TVariable "d" ])
      description = "Transform the third value in a triple"
      fn =
        (function
        | state, [ DFnVal fn; DTuple (first, second, [ third ]) ] ->
          uply {
            let! newThird =
              Interpreter.applyFnVal state (id 0) fn [ third ] NotInPipe NoRail
            return DTuple(first, second, [ newThird ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "mapAllThree" 0
      parameters =
        [ Param.makeWithArgs
            "fnFirst"
            (TFn([ TVariable "a" ], TVariable "d"))
            "used to map the first value in the tuple"
            [ "val" ]

          Param.makeWithArgs
            "fnSecond"
            (TFn([ TVariable "b" ], TVariable "e"))
            "used to map the second value in the tuple"
            [ "val" ]

          Param.makeWithArgs
            "fnThird"
            (TFn([ TVariable "c" ], TVariable "f"))
            "used to map the third value in the tuple"
            [ "val" ]

          Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TTuple(TVariable "d", TVariable "e", [ TVariable "f" ])
      description = "Transform all values in a triple"
      fn =
        (function
        | state,
          [ DFnVal fnFirst
            DFnVal fnSecond
            DFnVal fnThird
            DTuple (first, second, [ third ]) ] ->
          uply {
            let! newFirst =
              Interpreter.applyFnVal state (id 0) fnFirst [ first ] NotInPipe NoRail

            let! newSecond =
              Interpreter.applyFnVal
                state
                (id 0)
                fnSecond
                [ second ]
                NotInPipe
                NoRail

            let! newThird =
              Interpreter.applyFnVal state (id 0) fnThird [ third ] NotInPipe NoRail

            return DTuple(newFirst, newSecond, [ newThird ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
