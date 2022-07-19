module LibExecutionStdLib.LibTuple

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Tuple2" "first" 0
      parameters = [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "a"
      description =
        // TODO: improve this text
        "Returns the first part of a 2-tuple."
      fn =
        (function
        | state, [ DTuple (first, _second, []) ] ->
          Ply(first)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Tuple2" "second" 0
      parameters = [ Param.make "tuple" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TVariable "b"
      description =
        // TODO: improve this text
        "Returns the second part of a 2-tuple."
      fn =
        (function
        | state, [ DTuple (_first, second, []) ] ->
          Ply(second)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
