module BuiltinPM.Libs.Rebase

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "scmRebase" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to rebase" ]
      returnType = TypeReference.result TString (TList TString)
      description =
        "Rebase branch onto parent's latest. Returns Ok message or Error with list of conflicting paths."
      fn =
        let resultOk = Dval.resultOk KTString (KTList VT.string)
        let resultError = Dval.resultError KTString (KTList VT.string)
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! result = LibPackageManager.Rebase.rebase branchId
            match result with
            | Ok msg -> return resultOk (DString msg)
            | Error conflicts ->
              let conflictStrs =
                conflicts
                |> List.map (fun c ->
                  $"{c.owner}.{c.modules}.{c.name} ({c.itemType})")
                |> List.map DString
              return resultError (DList(VT.string, conflictStrs))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRebaseConflicts" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch to check" ]
      returnType = TList TString
      description = "Get list of conflicting location paths for rebase."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! conflicts = LibPackageManager.Rebase.getConflicts branchId
            let conflictStrs =
              conflicts
              |> List.map (fun c ->
                DString $"{c.owner}.{c.modules}.{c.name} ({c.itemType})")
            return DList(VT.string, conflictStrs)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
