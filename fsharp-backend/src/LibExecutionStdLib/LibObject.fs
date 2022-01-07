module LibExecutionStdLib.LibObject

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors
module DvalReprExternal = LibExecution.DvalReprExternal
module Legacy = LibExecution.Legacy

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "Object" "empty" 0
      parameters = []
      returnType = TDict varA
      description = "Return an empty object"
      fn =
        (function
        | _, [] -> Ply(DObj(Map.empty))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "empty" 0) }
    { name = fn "Object" "merge" 0
      parameters =
        [ Param.make "left" (TDict varA) ""; Param.make "right" (TDict varA) "" ]
      returnType = TDict varA
      description =
        "Return a combined object with both objects' keys and values. If the same key exists in both `left` and `right`, then use the value from `right`"
      fn =
        (function
        | _, [ DObj l; DObj r ] -> Ply(DObj(Map.mergeFavoringRight l r))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "merge" 0) }
    { name = fn "Object" "toJSON" 0
      parameters = [ Param.make "obj" (TDict varA) "" ]
      returnType = TStr
      description = "Dumps `obj` to a JSON string"
      fn =
        (function
        | _, [ DObj o ] ->
          DObj o |> Legacy.PrettyResponseJsonV0.toPrettyResponseJsonV0 |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Object" "toJSON" 1) }
    { name = fn "Object" "toJSON" 1
      parameters = [ Param.make "obj" (TDict varA) "" ]
      returnType = TStr
      description = "Dumps `obj` to a JSON string"
      fn =
        (function
        | _, [ DObj o ] ->
          DObj o |> DvalReprExternal.toPrettyMachineJsonStringV1 |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Dict" "toJSON" 0) } ]
