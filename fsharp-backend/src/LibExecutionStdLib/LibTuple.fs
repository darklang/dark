module LibExecutionStdLib.LibTuple

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ // Tuple2
    { name = fn "Tuple2" "first" 0
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "a"
      description = "Returns the first part of a 2-tuple."
      fn =
        (function
        | state, [ DTuple (first, _second, []) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "second" 0
      parameters =
        [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "b"
      description = "Returns the second part of a 2-tuple."
      fn =
        (function
        | state, [ DTuple (_first, second, []) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    // Tuple3
    { name = fn "Tuple3" "first" 0
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "a"
      description = "Returns the first part of a 3-tuple."
      fn =
        (function
        | state, [ DTuple (first, _second, [ _third ]) ] -> Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "second" 0
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "b"
      description = "Returns the second part of a 3-tuple."
      fn =
        (function
        | state, [ DTuple (_first, second, [ _third ]) ] -> Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple3" "third" 0
      parameters =
        [ Param.make
            "tuple"
            (TTuple(TVariable "a", TVariable "b", [ TVariable "c" ]))
            "" ]
      returnType = TVariable "c"
      description = "Returns the third part of a 3-tuple."
      fn =
        (function
        | state, [ DTuple (_first, _second, [ third ]) ] -> Ply(third)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
