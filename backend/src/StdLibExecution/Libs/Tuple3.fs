/// StdLib functions to create and utilize tuples with 3 elements
module StdLibExecution.Libs.Tuple3

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth
open LibExecution.StdLib.Shortcuts

module Interpreter = LibExecution.Interpreter



let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Tuple3" ]

let fns : List<BuiltInFn> =
  [ { name = fn "create" 0
      typeParams = []
      parameters =
        [ Param.make "first" (TVariable "a") ""
          Param.make "second" (TVariable "b") ""
          Param.make "third" (TVariable "c") "" ]
      returnType = TTuple(TVariable "a", TVariable "b", [ TVariable "c" ])
      description = "Returns a triple with the given values"
      fn =
        (function
        | _, _, [ first; second; third ] -> Ply(DTuple(first, second, [ third ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "first" 0
      typeParams = []
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "a"
      description = "Returns the first value of a triple"
      fn =
        (function
        | _, _, [ DTuple(first, _second, [ _third ]) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "second" 0
      typeParams = []
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "b"
      description = "Returns the second value of a triple"
      fn =
        (function
        | _, _, [ DTuple(_first, second, [ _third ]) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "third" 0
      typeParams = []
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "c"
      description = "Returns the third value of a triple"
      fn =
        (function
        | _, _, [ DTuple(_first, _second, [ third ]) ] -> Ply(third)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "mapFirst" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton (TVariable "a"), TVariable "d"))
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
        | state, _, [ DFnVal fn; DTuple(first, second, [ third ]) ] ->
          uply {
            let args = NEList.singleton first
            let! newFirst = Interpreter.applyFnVal state 0UL fn [] args
            return DTuple(newFirst, second, [ third ])
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
            (TFn(NEList.singleton (TVariable "b"), TVariable "d"))
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
        | state, _, [ DFnVal fn; DTuple(first, second, [ third ]) ] ->
          uply {
            let args = NEList.singleton second
            let! newSecond = Interpreter.applyFnVal state 0UL fn [] args
            return DTuple(first, newSecond, [ third ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "mapThird" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton (TVariable "c"), TVariable "d"))
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
        | state, _, [ DFnVal fn; DTuple(first, second, [ third ]) ] ->
          uply {
            let args = NEList.singleton third
            let! newThird = Interpreter.applyFnVal state 0UL fn [] args
            return DTuple(first, second, [ newThird ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "mapAllThree" 0
      typeParams = []
      parameters =
        [ Param.makeWithArgs
            "fnFirst"
            (TFn(NEList.singleton (TVariable "a"), TVariable "d"))
            "used to map the first value in the tuple"
            [ "val" ]

          Param.makeWithArgs
            "fnSecond"
            (TFn(NEList.singleton (TVariable "b"), TVariable "e"))
            "used to map the second value in the tuple"
            [ "val" ]

          Param.makeWithArgs
            "fnThird"
            (TFn(NEList.singleton (TVariable "c"), TVariable "f"))
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
          _,
          [ DFnVal fnFirst
            DFnVal fnSecond
            DFnVal fnThird
            DTuple(first, second, [ third ]) ] ->
          uply {
            let args = NEList.singleton first
            let! newFirst = Interpreter.applyFnVal state 0UL fnFirst [] args
            let args = NEList.singleton second
            let! newSecond = Interpreter.applyFnVal state 0UL fnSecond [] args
            let args = NEList.singleton third
            let! newThird = Interpreter.applyFnVal state 0UL fnThird [] args

            // TODO: handle fakevals
            return DTuple(newFirst, newSecond, [ newThird ])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
