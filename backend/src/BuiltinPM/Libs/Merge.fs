module BuiltinPM.Libs.Merge

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType

open Builtin.Shortcuts


let private mergeErrorToString (e : LibPackageManager.Merge.MergeError) : string =
  match e with
  | LibPackageManager.Merge.NotRebased -> "Must rebase first"
  | LibPackageManager.Merge.HasWip -> "Commit or discard WIP first"
  | LibPackageManager.Merge.HasChildren -> "Merge or delete child branches first"
  | LibPackageManager.Merge.NothingToMerge -> "Nothing to merge"
  | LibPackageManager.Merge.NotFound -> "Branch not found"
  | LibPackageManager.Merge.IsMainBranch -> "Cannot merge main branch"


let fns : List<BuiltInFn> =
  [ { name = fn "scmMerge" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to merge into parent" ]
      returnType = TypeReference.result TUnit TString
      description = "Merge branch into its parent. Must be rebased with no WIP."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! result = LibPackageManager.Merge.merge branchId
            match result with
            | Ok() -> return resultOk DUnit
            | Error e -> return resultError (DString(mergeErrorToString e))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmCanMerge" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to check" ]
      returnType = TypeReference.result TUnit TString
      description = "Check if a branch can be merged."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! result = LibPackageManager.Merge.canMerge branchId
            match result with
            | Ok() -> return resultOk DUnit
            | Error e -> return resultError (DString(mergeErrorToString e))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
