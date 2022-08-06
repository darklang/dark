/// API endpoint to receive and handle an Op
module ApiServer.AddOps

open System.Threading.Tasks
open FSharp.Control.Tasks
open Microsoft.AspNetCore.Http

open Prelude
open Tablecloth
open Http

module Telemetry = LibService.Telemetry
module Span = Telemetry.Span

module C = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module Op = LibBackend.Op
module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

// Toplevel deletion:
// * The server announces that a toplevel is deleted by it appearing in
// * deleted_toplevels. The server announces it is no longer deleted by it
// * appearing in toplevels again.

module V0 =

  // A subset of responses to be merged in
  type T = Op.AddOpEventV0

  type Params = Op.AddOpParamsV0

  let empty : Op.AddOpResultV0 =
    { toplevels = []
      deleted_toplevels = []
      user_functions = []
      deleted_user_functions = []
      user_tipes = []
      deleted_user_tipes = [] }

  let causesAnyChanges (ops : PT.Oplist) : bool = List.any Op.hasEffect ops

  /// API endpoint to add a set of Op in a Canvas
  ///
  /// The Ops usually relate to a single Toplevel within the Canvas,
  /// but can technically include Ops against several TLIDs
  let addOp (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      let! p = ctx.ReadJsonAsync<Params>()
      let canvasID = canvasInfo.id

      let! isLatest =
        Serialize.isLatestOpRequest p.clientOpCtrId p.opCtr canvasInfo.id

      let newOps = Convert.ocamlOplist2PT p.ops
      let newOps = if isLatest then newOps else Op.filterOpsReceivedOutOfOrder newOps
      let opTLIDs = List.map Op.tlidOf newOps
      Telemetry.addTags [ "opCtr", p.opCtr
                          "clientOpCtrId", p.clientOpCtrId
                          "opTLIDs", opTLIDs ]

      t.next "load-saved-ops"
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
        Serialize.loadOplists
          Serialize.IncludeDeletedToplevels
          canvasInfo.id
          allTLIDs
      let oldOps = oldOps |> List.map Tuple2.second |> List.concat

      let c = C.fromOplist canvasInfo oldOps newOps

      t.next "to-frontend"
      let toplevels = C.toplevels c
      let deletedToplevels = C.deletedToplevels c

      let (tls, fns, types) = Convert.pt2ocamlToplevels toplevels
      let (dTLs, dFns, dTypes) = Convert.pt2ocamlToplevels deletedToplevels

      let result : Op.AddOpResultV0 =
        { toplevels = tls
          deleted_toplevels = dTLs
          user_functions = fns
          deleted_user_functions = dFns
          user_tipes = types
          deleted_user_tipes = dTypes }


      t.next "save-to-disk"
      // work out the result before we save it, in case it has a
      // stackoverflow or other crashing bug
      if causesAnyChanges newOps then
        do!
          (oldOps @ newOps)
          |> Op.oplist2TLIDOplists
          |> List.filterMap (fun (tlid, oplists) ->
            let tlPair =
              match Map.get tlid toplevels with
              | Some tl -> Some(tl, C.NotDeleted)
              | None ->
                match Map.get tlid deletedToplevels with
                | Some tl -> Some(tl, C.Deleted)
                | None ->
                  Telemetry.addEvent "Undone handler" [ "tlid", tlid ]
                  // If we don't find anything, this was Undo-ed completely. Let's not
                  // do anything.
                  // https://github.com/darklang/dark/issues/3675 for discussion.
                  None
            Option.map (fun (tl, deleted) -> (tlid, oplists, tl, deleted)) tlPair)
          |> C.saveTLIDs canvasInfo


      t.next "send-ops-to-pusher"
      let event =
        // To make this work with prodclone, we might want to have it specify
        // more ... else people's prodclones will stomp on each other ...
        if causesAnyChanges newOps then
          let event : Op.AddOpEventV0 = { result = result; ``params`` = p }
          LibBackend.Pusher.pushAddOpEventV0 canvasID event
          event
        else
          { result = empty; ``params`` = p }

      t.next "send-event-to-heapio"
      // NB: I believe we only send one op at a time, but the type is op list
      newOps
      // MoveTL and TLSavepoint make for noisy data, so exclude it from heapio
      |> List.filter (fun op ->
        match op with
        | PT.MoveTL _
        | PT.TLSavepoint _ -> false
        | _ -> true)
      |> List.iter (fun op ->
        LibService.HeapAnalytics.track
          canvasInfo.id
          canvasInfo.name
          canvasInfo.owner
          (Op.eventNameOfOp op)
          Map.empty)

      return event
    }

module V1 =

  // A subset of responses to be merged in
  type T = Op.AddOpResultV1

  type Params = Op.AddOpParamsV1

  let empty : Op.AddOpResultV1 =
    { handlers = []
      deletedHandlers = []
      dbs = []
      deletedDBs = []
      userFunctions = []
      deletedUserFunctions = []
      userTypes = []
      deletedUserTypes = [] }

  let causesAnyChanges (ops : PT.Oplist) : bool = List.any Op.hasEffect ops

  /// API endpoint to add a set of Op in a Canvas
  ///
  /// The Ops usually relate to a single Toplevel within the Canvas,
  /// but can technically include Ops against several TLIDs
  let addOp (ctx : HttpContext) : Task<T> =
    task {
      use t = startTimer "read-api" ctx
      let canvasInfo = loadCanvasInfo ctx

      let! p = ctx.ReadVanillaJsonAsync<Params>()
      let canvasID = canvasInfo.id

      let! isLatest =
        Serialize.isLatestOpRequest (Some p.clientOpCtrID) p.opCtr canvasInfo.id

      let newOps = p.ops
      let newOps = if isLatest then newOps else Op.filterOpsReceivedOutOfOrder newOps
      let opTLIDs = List.map Op.tlidOf newOps
      Telemetry.addTags [ "opCtr", p.opCtr
                          "clientOpCtrID", p.clientOpCtrID
                          "opTLIDs", opTLIDs ]

      t.next "load-saved-ops"
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
        Serialize.loadOplists
          Serialize.IncludeDeletedToplevels
          canvasInfo.id
          allTLIDs
      let oldOps = oldOps |> List.map Tuple2.second |> List.concat

      let c = C.fromOplist canvasInfo oldOps newOps

      t.next "to-frontend"

      let result : Op.AddOpResultV1 =
        { handlers = Map.values c.handlers
          deletedHandlers = Map.values c.deletedHandlers
          dbs = Map.values c.dbs
          deletedDBs = Map.values c.deletedDBs
          userFunctions = Map.values c.userFunctions
          deletedUserFunctions = Map.values c.deletedUserFunctions
          userTypes = Map.values c.userTypes
          deletedUserTypes = Map.values c.deletedUserTypes }

      t.next "save-to-disk"
      // work out the result before we save it, in case it has a
      // stackoverflow or other crashing bug
      if causesAnyChanges newOps then
        do!
          (oldOps @ newOps)
          |> Op.oplist2TLIDOplists
          |> List.filterMap (fun (tlid, oplists) ->
            let tlPair =
              match Map.get tlid (C.toplevels c) with
              | Some tl -> Some(tl, C.NotDeleted)
              | None ->
                match Map.get tlid (C.deletedToplevels c) with
                | Some tl -> Some(tl, C.Deleted)
                | None ->
                  Telemetry.addEvent "Undone handler" [ "tlid", tlid ]
                  // If we don't find anything, this was Undo-ed completely. Let's not
                  // do anything.
                  // https://github.com/darklang/dark/issues/3675 for discussion.
                  None
            Option.map (fun (tl, deleted) -> (tlid, oplists, tl, deleted)) tlPair)
          |> C.saveTLIDs canvasInfo


      t.next "send-ops-to-pusher"
      // To make this work with prodclone, we might want to have it specify
      // more ... else people's prodclones will stomp on each other ...
      if causesAnyChanges newOps then
        LibBackend.Pusher.pushAddOpEventV1
          canvasID
          { result = result; ``params`` = p }

      t.next "send-event-to-heapio"
      // NB: I believe we only send one op at a time, but the type is op list
      newOps
      // MoveTL and TLSavepoint make for noisy data, so exclude it from heapio
      |> List.filter (fun op ->
        match op with
        | PT.MoveTL _
        | PT.TLSavepoint _ -> false
        | _ -> true)
      |> List.iter (fun op ->
        LibService.HeapAnalytics.track
          canvasInfo.id
          canvasInfo.name
          canvasInfo.owner
          (Op.eventNameOfOp op)
          Map.empty)

      return result
    }
