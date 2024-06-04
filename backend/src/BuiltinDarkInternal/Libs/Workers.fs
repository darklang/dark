/// Builtin functions for building Dark functionality for Workers
/// Also has infra functions for managing workers - TODO: separate these
module BuiltinDarkInternal.Libs.Workers

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module DarkDateTime = LibExecution.DarkDateTime
module PackageIDs = LibExecution.PackageIDs
module SchedulingRules = LibCloud.QueueSchedulingRules
module Pusher = LibCloud.Pusher
module Queue = LibCloud.Queue

let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  (function
  | _, _, [ DUuid canvasID; DString handlerName ] ->
    uply {
      do! fn canvasID handlerName
      let! _s = SchedulingRules.getWorkerSchedules canvasID

      // TODO reenable
      // Pusher.push
      //   LibClientTypesToCloudTypes.Pusher.eventSerializer
      //   canvasID
      //   (Pusher.UpdateWorkerStates s)
      //   None

      return DUnit
    }
  | _ -> incorrectArgs ())


let schedulingRuleTypeName =
  FQTypeName.Package PackageIDs.Type.Internal.Worker.scheduleRule

let schedulingRuleTypeRef = TCustomType(Ok schedulingRuleTypeName, [])

let rulesToDval (rules : List<SchedulingRules.SchedulingRule.T>) : Dval =
  let typeName = schedulingRuleTypeName

  rules
  |> List.map (fun r ->
    let fields =
      [ ("id", DUuid r.id)
        ("rule_type", r.ruleType.ToString() |> DString)
        ("canvas_id", DUuid r.canvasID)
        ("handler_name", DString r.handlerName)
        ("event_space", DString r.eventSpace)
        ("created_at", DDateTime(DarkDateTime.fromInstant r.createdAt)) ]
    DRecord(typeName, typeName, [], Map fields))
  |> Dval.list (KTCustomType(typeName, []))


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalCanvasQueueCount" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TUInt64 "" ]
      returnType = TList TInt64
      description = "Get count of how many events are in the queue for this tlid"
      fn =
        (function
        | _, _, [ DUuid canvasID; DUInt64 tlid ] ->
          uply {
            let tlid = uint64 tlid
            let! count = LibCloud.Stats.workerStats canvasID tlid
            return DInt64 count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasQueueSchedulingRuleList" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList schedulingRuleTypeRef
      description =
        "Returns a list of all queue scheduling rules for the specified canvasID"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! rules = SchedulingRules.getSchedulingRules canvasID
            return rulesToDval rules
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraSchedulingRuleBlockInsert" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""; Param.make "handlerName" TString "" ]
      returnType = TUnit
      description =
        "Add a worker scheduling 'block' for the given canvas and handler. This prevents any events for that handler from being scheduled until the block is manually removed."
      fn = modifySchedule Queue.blockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraSchedulingRuleBlockDelete" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""; Param.make "handlerName" TString "" ]
      returnType = TUnit
      description =
        "Removes the worker scheduling block, if one exists, for the given canvas and handler"
      fn = modifySchedule Queue.unblockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraSchedulingRuleList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList schedulingRuleTypeRef
      description = "Returns a list of all queue scheduling rules"
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let! rules = SchedulingRules.getAllSchedulingRules ()
            return rulesToDval rules
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
