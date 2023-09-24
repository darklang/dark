/// Builtin functions for building Dark functionality for Workers
/// Also has infra functions for managing workers - TODO: separate these
module BuiltinDarkInternal.Libs.Workers

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module DarkDateTime = LibExecution.DarkDateTime
module SchedulingRules = LibCloud.QueueSchedulingRules
module Pusher = LibCloud.Pusher
module Queue = LibCloud.Queue


let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  (function
  | _, _, [ DUuid canvasID; DString handlerName ] ->
    uply {
      do! fn canvasID handlerName
      let! s = SchedulingRules.getWorkerSchedules canvasID
      // Pusher.push
      //   LibClientTypesToCloudTypes.Pusher.eventSerializer
      //   canvasID
      //   (Pusher.UpdateWorkerStates s)
      //   None
      return DUnit
    }
  | _ -> incorrectArgs ())

let schedulingRuleTypeName = typ [ "DarkInternal"; "SchedulingRule" ] "Rule" 0

let schedulingRuleTypeRef =
  TCustomType(Ok(FQName.BuiltIn(schedulingRuleTypeName)), [])

let rulesToDval (rules : List<SchedulingRules.SchedulingRule.T>) : Ply<Dval> =
  let typeName = FQName.BuiltIn schedulingRuleTypeName

  rules
  |> Ply.List.mapSequentially (fun r ->
    Dval.record
      typeName
      (Some [])
      [ ("id", Dval.int r.id)
        ("rule_type", r.ruleType.ToString() |> DString)
        ("canvas_id", DUuid r.canvasID)
        ("handler_name", DString r.handlerName)
        ("event_space", DString r.eventSpace)
        ("created_at", DDateTime(DarkDateTime.fromInstant r.createdAt)) ])
  |> Ply.map (Dval.list (ValueType.Known(KTCustomType(typeName, []))))

let types : List<BuiltInType> =
  [ { name = schedulingRuleTypeName
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "id"; typ = TInt }
                [ { name = "ruleType"; typ = TString }
                  { name = "canvasID"; typ = TUuid }
                  { name = "handlerName"; typ = TString }
                  { name = "createdAt"; typ = TDateTime } ]
            ) }
      deprecated = NotDeprecated
      description = "A scheduling rule for a worker" } ]


let fns : List<BuiltInFn> =
  [ { name = fn [ "DarkInternal"; "Canvas"; "Queue" ] "count" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TList TInt
      description = "Get count of how many events are in the queue for this tlid"
      fn =
        (function
        | _, _, [ DUuid canvasID; DInt tlid ] ->
          uply {
            let tlid = uint64 tlid
            let! count = LibCloud.Stats.workerStats canvasID tlid
            return DInt count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "DarkInternal"; "Canvas"; "Queue"; "SchedulingRule" ] "list" 0
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
            return! rulesToDval rules
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "DarkInternal"; "Infra"; "SchedulingRule"; "Block" ] "insert" 0
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


    { name = fn [ "DarkInternal"; "Infra"; "SchedulingRule"; "Block" ] "delete" 0
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


    { name = fn [ "DarkInternal"; "Infra"; "SchedulingRule" ] "list" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList schedulingRuleTypeRef
      description = "Returns a list of all queue scheduling rules"
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let! rules = SchedulingRules.getAllSchedulingRules ()
            return! rulesToDval rules
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents = (fns, types, constants)
