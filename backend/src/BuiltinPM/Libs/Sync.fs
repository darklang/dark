module BuiltinPM.Libs.Sync

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval

open Builtin.Shortcuts


// TODO: Reconsider which of these functions should be public vs admin-only:
// - scmGetLastSyncDate: Read-only, probably OK as public
// - scmRecordSync: Writes sync metadata - should probably be admin-only or require auth
let fns : List<BuiltInFn> =
  [ { name = fn "scmGetLastSyncDate" 0
      typeParams = []
      parameters = [ Param.make "instanceId" TUuid "" ]
      returnType = TypeReference.option TDateTime
      description =
        "Get the most recent sync time with a specific instance. Returns None if no sync has occurred."
      fn =
        function
        | _, _, _, [ DUuid instanceId ] ->
          uply {
            let! lastSync = LibPackageManager.Sync.getLastSyncDate instanceId

            return
              match lastSync with
              | None -> Dval.optionNone KTDateTime
              | Some dt -> Dval.optionSome KTDateTime (DDateTime dt)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmRecordSync" 0
      typeParams = []
      parameters =
        [ Param.make "instanceId" TUuid ""
          Param.make "opsPushed" TInt64 ""
          Param.make "opsFetched" TInt64 "" ]
      returnType = TUnit
      description =
        "Record a sync event in the database with the specified instance and op counts."
      fn =
        function
        | _, _, _, [ DUuid instanceId; DInt64 opsPushed; DInt64 opsFetched ] ->
          uply {
            do! LibPackageManager.Sync.recordSync instanceId opsPushed opsFetched

            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
