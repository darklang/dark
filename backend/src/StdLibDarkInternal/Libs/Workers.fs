/// StdLib functions for building Dark functionality for Workers
module StdLibDarkInternal.Libs.Workers

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module SchedulingRules = LibBackend.QueueSchedulingRules
module Pusher = LibBackend.Pusher
module Queue = LibBackend.Queue


let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  (function
  | _, _, [ DUuid canvasID; DString handlerName ] ->
    uply {
      do! fn canvasID handlerName
      let! s = SchedulingRules.getWorkerSchedules canvasID
      // Pusher.push
      //   ClientTypes2BackendTypes.Pusher.eventSerializer
      //   canvasID
      //   (Pusher.UpdateWorkerStates s)
      //   None
      return DUnit
    }
  | _ -> incorrectArgs ())

let ruleToDval (r : SchedulingRules.SchedulingRule.T) : Dval =
  Dval.record [ ("id", Dval.int r.id)
                ("rule_type", r.ruleType.ToString() |> Dval.DString)
                ("canvas_id", Dval.DUuid r.canvasID)
                ("handler_name", Dval.DString r.handlerName)
                ("event_space", Dval.DString r.eventSpace)
                ("created_at", Dval.DDateTime(DarkDateTime.fromInstant r.createdAt)) ]


let types : List<BuiltInType> =
  [ { name = typ "DarkInternal" "SchedulingRule" 0
      typeParams = []
      definition =
        CustomType.Record(
          { id = 1UL; name = "id"; typ = TInt },
          [ { id = 1UL; name = "ruleType"; typ = TString }
            { id = 1UL; name = "canvasID"; typ = TUuid }
            { id = 1UL; name = "handlerName"; typ = TString }
            // CLEANUP no event space needed, right?
            { id = 1UL; name = "eventSpace"; typ = TString }
            { id = 1UL; name = "createdAt"; typ = TDateTime } ]
        )
      deprecated = NotDeprecated
      description = "A scheduling rule for a worker" } ]

let schedulingRule =
  TCustomType(FQTypeName.Stdlib(typ "DarkInternal" "SchedulingRule" 0), [])

let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "getQueueCount" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TList TInt
      description = "Get count of how many events are in the queue for this tlid"
      fn =
        (function
        | _, _, [ DUuid canvasID; DInt tlid ] ->
          uply {
            let tlid = uint64 tlid
            let! count = LibBackend.Stats.workerStats canvasID tlid
            return DInt count
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getSchedulingRulesForCanvas" 0
      typeParams = []
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList schedulingRule
      description =
        "Returns a list of all queue scheduling rules for the specified canvasID"
      fn =
        (function
        | _, _, [ DUuid canvasID ] ->
          uply {
            let! rules = SchedulingRules.getSchedulingRules canvasID
            return rules |> List.map ruleToDval |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "addWorkerSchedulingBlock" 0
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


    { name = fn "DarkInternal" "removeWorkerSchedulingBlock" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid ""; Param.make "handlerName" TString "" ]
      returnType = TUnit
      description =
        "Removes the worker scheduling block, if one exists, for the given canvas and handler. Enqueued events from this job will immediately be scheduled."
      fn = modifySchedule Queue.unblockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getAllSchedulingRules" 0
      typeParams = []
      parameters = []
      returnType = TList schedulingRule
      description = "Returns a list of all queue scheduling rules"
      fn =
        (function
        | _, _, [] ->
          uply {
            let! rules = SchedulingRules.getAllSchedulingRules ()
            return rules |> List.map ruleToDval |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
