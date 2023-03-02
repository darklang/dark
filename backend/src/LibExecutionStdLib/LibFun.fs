module LibExecutionStdLib.LibFun

open System.Threading.Tasks
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Fun" "identity" 0
      parameters = [ Param.make "val" (TVariable "a") "" ]
      returnType = TVariable "a"
      description = "Returns the input <param a>, without doing anything."
      fn =
        (function
        | _, [ v ] -> Ply v
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Fun" "identityB" 0
      parameters = [ ]
      returnType = TFn ([TVariable "a"], TVariable "a")
      description = "Returns a function that returns its input, without doing anything"
      fn =
        (function
        | _, [ ] ->
          { parameters = [id 9999, "a"]
            symtable = Map.empty
            body = EVariable(id 9182, "a") }
          |> Lambda
          |> DFnVal
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
