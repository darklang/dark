module LibExecutionStdLib.LibJson

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"


let fns : List<BuiltInFn> =
  [ { name = fn "Json" "serialize" 0
      typeArgs = [ TVariable "a" ]
      parameters = [ Param.make "arg" varA "" ]
      returnType = TResult(TStr, TStr)
      description = "TODO"
      fn =
        (function
        | _, [ typeArg ], [ arg ] ->
          match typeArg, arg with
          | TUnit, DUnit -> Ply(DResult(Ok(DStr "null")))
          | _ -> Ply(DResult(Error(DStr "TODO")))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Json" "parse" 0
      typeArgs = [ TVariable "a" ]
      parameters = [ Param.make "arg" TStr "" ]
      returnType = TResult(varA, TStr)
      description = "TODO"
      fn =
        (function
        | _, [ typeArg ], [ DStr arg ] ->
          match typeArg, arg with
          | TUnit, "null" -> Ply(DResult(Ok DUnit))
          | _ -> Ply(DResult(Error(DStr "TODO")))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]
