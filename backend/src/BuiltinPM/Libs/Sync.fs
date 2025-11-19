module BuiltinPM.Libs.Sync

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval

module Sync = LibPackageManager.PT.SQL.Sync

open Builtin.Shortcuts



// TODO: review/reconsider the accessibility of these fns
let fns : List<BuiltInFn> =
  [ { name = fn "scmGetLastSyncDate" 0
      typeParams = []
      parameters = [ Param.make "instanceID" TUuid "" ]
      returnType = TypeReference.option TDateTime
      description = "Get when we've most recently synced against the given instance."
      fn =
        function
        | _, _, _, [ DUuid instanceID ] ->
          uply {
            let! lastSync = Sync.getLastSyncDate instanceID

            return lastSync |> Option.map DDateTime |> Dval.option KTDateTime
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmRecordSync" 0
      typeParams = []
      parameters =
        [ Param.make "instanceID" TUuid ""
          Param.make "opsPushed" TInt64 ""
          Param.make "opsFetched" TInt64 "" ]
      returnType = TUnit
      description = "Record a sync event in the database."
      fn =
        function
        | _, _, _, [ DUuid instanceID; DInt64 opsPushed; DInt64 opsFetched ] ->
          uply {
            do! Sync.recordSync instanceID opsPushed opsFetched

            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
