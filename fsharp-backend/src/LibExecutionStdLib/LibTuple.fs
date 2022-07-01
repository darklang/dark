module LibExecutionStdLib.LibTuple

open LibExecution.RuntimeTypes
open Prelude

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs
let err (str : string) = Ply(Dval.errStr str)


let fns : List<BuiltInFn> =
  [ { name = fn "Tuple" "toList" 0
      parameters = [ Param.make "tpl" (TTuple(TVariable "a", TVariable "b", [])) "" ]
      returnType = TList(TVariable "a")
      description = "Returns a tuple containing the items in a tuple."
      fn =
        (function
        | _, [ DTuple (first, second, theRest) ] ->
          Ply(DList([ first; second ] @ theRest))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
