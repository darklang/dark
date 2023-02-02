module LibExecutionStdLib.LibUuid

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

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
        | _, [] -> Ply(DUuid(System.Guid.NewGuid()))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      // similarly to Date::now, it's not particularly fun for this to change
      // when live programming
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Uuid" "parse" 0
      parameters = [ Param.make "uuid" TStr "" ]
      returnType = TResult(TUuid, TStr)
      description =
        "Parse a <type UUID> of form {{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}}"
      fn =
        (function
        | _, [ DStr s ] ->
          match System.Guid.TryParse s with
          | true, x -> x |> DUuid |> Ok |> DResult |> Ply
          | _ ->
            "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
            |> DStr
            |> Error
            |> DResult
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]
