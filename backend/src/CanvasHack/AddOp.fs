/// API endpoint to receive and handle an Op
module CanvasHack.AddOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

module C = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module Op = LibBackend.Op
module PT = LibExecution.ProgramTypes

let causesAnyChanges (ops : PT.Oplist) : bool = List.any Op.hasEffect ops

let addOp
  (canvasInfo : C.Meta)
  (p : LibBackend.Op.AddOpParamsV1)
  : Task<LibBackend.Op.AddOpResultV1> =
  task {
    let! isLatest =
      Serialize.isLatestOpRequest (Some p.clientOpCtrID) p.opCtr canvasInfo.id

    let newOps = p.ops
    let newOps = if isLatest then newOps else Op.filterOpsReceivedOutOfOrder newOps
    let opTLIDs = List.map Op.tlidOf newOps

    let! dbTLIDs =
      match Op.requiredContextToValidateOplist newOps with
      | Op.NoContext -> Task.FromResult []
      // NOTE: Because we run canvas-wide validation logic, it's important that
      // we load _at least_ the context (ie. datastores, functions, types, etc)
      // and not just the tlids in the API payload.
      | Op.AllDatastores -> Serialize.fetchTLIDsForAllDBs canvasInfo.id

    let allTLIDs = opTLIDs @ dbTLIDs
    // We're going to save this, so we need all the ops
    let! oldOps =
      Serialize.loadOplists Serialize.IncludeDeletedToplevels canvasInfo.id allTLIDs
    let oldOps = oldOps |> List.map Tuple2.second |> List.concat

    let c = C.fromOplist canvasInfo oldOps newOps

    let result : Op.AddOpResultV1 =
      { handlers = Map.values c.handlers
        deletedHandlers = Map.values c.deletedHandlers
        dbs = Map.values c.dbs
        deletedDBs = Map.values c.deletedDBs
        userFunctions = Map.values c.userFunctions
        deletedUserFunctions = Map.values c.deletedUserFunctions
        userTypes = Map.values c.userTypes
        deletedUserTypes = Map.values c.deletedUserTypes }

    // work out the result before we save it, in case it has a
    // stackoverflow or other crashing bug
    let changed = causesAnyChanges newOps

    if changed then
      do!
        (oldOps @ newOps)
        |> Op.oplist2TLIDOplists
        |> List.filterMap (fun (tlid, oplists) ->
          let tlPair =
            match Map.get tlid (C.toplevels c) with
            | Some tl -> Some(tl, C.NotDeleted)
            | None -> None
          Option.map (fun (tl, deleted) -> (tlid, oplists, tl, deleted)) tlPair)
        |> C.saveTLIDs canvasInfo

    return result
  }
