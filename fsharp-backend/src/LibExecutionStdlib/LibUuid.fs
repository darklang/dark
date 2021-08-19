module LibExecutionStdLib.LibUuid

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "Uuid" "generate" 0
      parameters = []
      returnType = TUuid
      description = "Generate a new UUID v4 according to RFC 4122"
      fn =
        (function
        | _, [] -> Value(DUuid(System.Guid.NewGuid()))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      // similarly to Date::now, it's not particularly fun for this to change
      // when live programming
      previewable = Impure
      deprecated = NotDeprecated } ]
