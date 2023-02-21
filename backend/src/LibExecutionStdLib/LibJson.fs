module LibExecutionStdLib.LibJson

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs


let varErr = TVariable "err"
let varA = TVariable "a"

let fns : List<BuiltInFn> =
  [ { name = fn "JSON" "parse" 1
      parameters = [ Param.make "json" TStr "" ]
      returnType = TResult(varA, varErr)
      description =
        "Parses a json string and returns its value.

         HTTPClient functions, and our request handler, automatically parse JSON into
         the {{body}} and {{jsonBody}} fields, so you probably won't need this.
         However, if you need to consume bad JSON, you can use string functions to
         fix the JSON and then use this function to parse it."
      fn =
        (function
        | _, [ DStr json ] ->
          json
          |> DvalReprLegacyExternal.ofUnknownJsonV1
          |> Result.mapError DStr
          |> DResult
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]
