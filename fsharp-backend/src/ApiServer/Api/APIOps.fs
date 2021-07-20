module ApiServer.AddOps

// Functions and API endpoints for the API

open Microsoft.AspNetCore.Http
open Giraffe
open Giraffe.EndpointRouting

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus
open Prelude
open Tablecloth

module C = LibBackend.Canvas
module Serialize = LibBackend.Serialize
module Op = LibBackend.Op
module PT = LibExecution.ProgramTypes
module OT = LibExecution.OCamlTypes
module ORT = LibExecution.OCamlTypes.RuntimeT
module AT = LibExecution.AnalysisTypes
module Convert = LibExecution.OCamlTypes.Convert


// Toplevel deletion:
// * The server announces that a toplevel is deleted by it appearing in
// * deleted_toplevels. The server announces it is no longer deleted by it
// * appearing in toplevels again.

// A subset of responses to be merged in
type T = Op.AddOpEvent

type Params = Op.AddOpParams

let empty : Op.AddOpResult =
  { toplevels = []
    deleted_toplevels = []
    user_functions = []
    deleted_user_functions = []
    user_tipes = []
    deleted_user_tipes = [] }

let causesAnyChanges (ops : PT.Oplist) : bool = List.any Op.hasEffect ops

let addOp (ctx : HttpContext) : Task<T> =
  task {
    let t = Middleware.startTimer ctx
    let canvasInfo = Middleware.loadCanvasInfo ctx
    let executionID = Middleware.loadExecutionID ctx

    let! p = ctx.BindModelAsync<Params>()
    let canvasID = canvasInfo.id

    let! isLatest = Serialize.isLatestOpRequest p.clientOpCtrId p.opCtr canvasInfo.id

    let ops = Convert.ocamlOplist2PT p.ops
    let ops = if isLatest then ops else Op.filterOpsReceivedOutOfOrder ops
    t "read-api"

    let opTLIDs = List.map Op.tlidOf ops

    // NOTE: Because we run canvas-wide validation logic, it's important
    // that we load _at least_ the context (ie. datastores, functions, types, etc)
    // and not just the tlids in the API payload.
    let! c =
      match Op.requiredContextToValidateOplist ops with
      | Op.NoContext -> C.loadTLIDs canvasInfo opTLIDs
      | Op.AllDatastores -> C.loadWithDBs canvasInfo opTLIDs

    let c = Result.unwrapUnsafe c
    t "2-load-saved-ops"

    let ocamlToplevels = c |> C.toplevels |> Convert.pt2ocamlToplevels

    let ocamlDeletedToplevels = c |> C.deletedToplevels |> Convert.pt2ocamlToplevels


    let result : Op.AddOpResult =
      { toplevels = Tuple3.first ocamlToplevels
        deleted_toplevels = Tuple3.first ocamlDeletedToplevels
        user_functions = Tuple3.second ocamlToplevels
        deleted_user_functions = Tuple3.second ocamlDeletedToplevels
        user_tipes = Tuple3.third ocamlToplevels
        deleted_user_tipes = Tuple3.third ocamlDeletedToplevels }

    t "3-to-frontend"

    // work out the result before we save it, in case it has a
    // stackoverflow or other crashing bug
    // Canvas.saveTLIDs meta [ (h.tlid, oplists, PT.TLHandler h, Canvas.NotDeleted) ]
    if causesAnyChanges ops then do! C.saveTLIDs canvasInfo []
    t "4-save-to-disk"

    let event =
      // To make this work with prodclone, we might want to have it specify
      // more ... else people's prodclones will stomp on each other ...
      if causesAnyChanges ops then
        let event : Op.AddOpEvent = { result = result; ``params`` = p }
        LibBackend.Pusher.pushAddOpEvent executionID canvasID event
        event
      else
        { result = empty; ``params`` = p }

    t "5-send-ops-to-pusher"

    // NB: I believe we only send one op at a time, but the type is op list
    ops
    // MoveTL and TLSavepoint make for noisy data, so exclude it from heapio
    |> List.filter
         (function
         | PT.MoveTL _
         | PT.TLSavepoint _ -> false
         | _ -> true)
    |> List.iter
         (fun op ->
           LibBackend.HeapAnalytics.track
             executionID
             canvasInfo.id
             canvasInfo.name
             canvasInfo.owner
             (Op.eventNameOfOp op)
             Map.empty)

    t "6-send-event-to-heapio"

    // FSTODO
    // Span.set_attr parent "op_ctr" (Int p.opCtr)

    return event
  }
