module BuiltinPM.Libs.Merge

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "scmMerge" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to merge into parent" ]
      returnType =
        TypeReference.result TUnit (TCustomType(Ok PT2DT.MergeError.typeName, []))
      description = "Merge branch into its parent. Must be rebased with no WIP."
      fn =
        let resultOk = Dval.resultOk KTUnit PT2DT.MergeError.knownType
        let resultError = Dval.resultError KTUnit PT2DT.MergeError.knownType
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! result = LibPackageManager.Merge.merge branchId
            match result with
            | Ok() -> return resultOk DUnit
            | Error e -> return resultError (PT2DT.MergeError.toDT e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmCanMerge" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to check" ]
      returnType =
        TypeReference.result TUnit (TCustomType(Ok PT2DT.MergeError.typeName, []))
      description = "Check if a branch can be merged."
      fn =
        let resultOk = Dval.resultOk KTUnit PT2DT.MergeError.knownType
        let resultError = Dval.resultError KTUnit PT2DT.MergeError.knownType
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! result = LibPackageManager.Merge.canMerge branchId
            match result with
            | Ok() -> return resultOk DUnit
            | Error e -> return resultError (PT2DT.MergeError.toDT e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
