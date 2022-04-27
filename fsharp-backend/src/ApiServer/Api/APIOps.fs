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

let loadTLDetails
  (canvasID : CanvasID)
  : Task<List<tlid * array<byte> * Option<array<byte>> * Option<array<byte>> * Option<array<byte>>>> =
  let query =
    "SELECT tlid, data, rendered_oplist_cache, oplist, oplist_cache FROM toplevel_oplists
        WHERE canvas_id = @canvasID
          AND (oplist is null OR oplist_cache is null)
          AND deleted IS NOT NULL"
  Sql.query query
  |> Sql.parameters [ "canvasID", Sql.uuid canvasID ]
  |> Sql.executeAsync (fun read ->
    (read.tlid "tlid",
     read.bytea "data",
     read.byteaOrNone "rendered_oplist_cache",
     read.byteaOrNone "oplist",
     read.byteaOrNone "oplist_cache"))

/// Fill in the oplist and oplist_cache fields in the toplevel_oplists table in the DB
let clean (ctx : HttpContext) : Task<unit> =
  // CLEANUP remove this once it's been run on all canvases
  task {
    use t = startTimer "read-api" ctx
    let meta = loadCanvasInfo ctx
    let! details = loadTLDetails meta.id
    do!
      details
      |> Task.iterWithConcurrency
        20
        (fun (tlid, ocOplist, ocCache, fsOplist, fsCache) ->
          task {
            // if the oplist is missing, read it from ocaml, and save it
            let! oplist =
              match fsOplist with
              | None ->
                task {
                  let! oplist = LibBackend.OCamlInterop.oplistOfBinary ocOplist
                  do! Serialize.saveOplistToFSharpCache meta.id tlid ocOplist oplist
                  return oplist
                }
              | Some bytes ->
                Task.FromResult(
                  LibBinarySerialization.BinarySerialization.deserializeOplist
                    tlid
                    bytes
                )

            // if the cached toplevel is missing (but ocaml has it), read the
            // ocaml one, and save it
            match ocCache, fsCache with
            | Some ocamlBytes, None ->
              // OCaml interop doesn't have a converter for toplevels, so just
              // recreating using the oplists we have already.
              let c = LibBackend.Canvas.fromOplist meta [] oplist
              match LibBackend.Canvas.getToplevel tlid c with
              | Some (_, tl) ->
                do! Serialize.saveToplevelToFSharpCache meta.id ocamlBytes tl
              | None ->
                // The sample-gettingstarted canvas has some toplevels that were
                // deleted. As part of the canvas_clone process, those toplevels were
                // trimmed to their latest op. But since the toplevel was deleted,
                // the latest op was just a DeletedTL or similar. That means that we
                // don't actually have enough ops to make a cached toplevel. The
                // obvious solution is to delete these, probably manually.
                ()
            | _ -> ()
          })
  }


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

/// API endpoint to add a set of Op in a Canvas
///
/// The Ops usually relate to a single Toplevel within the Canvas,
/// but can technically include Ops against several TLIDs
let addOp (ctx : HttpContext) : Task<T> =
  task {
    use t = startTimer "read-api" ctx
    let canvasInfo = loadCanvasInfo ctx
    let executionID = loadExecutionID ctx

    let! p = ctx.ReadJsonAsync<Params>()
    let canvasID = canvasInfo.id

    let! isLatest = Serialize.isLatestOpRequest p.clientOpCtrId p.opCtr canvasInfo.id

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
      Serialize.loadOplists Serialize.IncludeDeletedToplevels canvasInfo.id allTLIDs
    let oldOps = oldOps |> List.map Tuple2.second |> List.concat

    let c = C.fromOplist canvasInfo oldOps newOps


    t.next "to-frontend"
    let toplevels = C.toplevels c
    let deletedToplevels = C.deletedToplevels c

    let (tls, fns, types) = Convert.pt2ocamlToplevels toplevels
    let (dTLs, dFns, dTypes) = Convert.pt2ocamlToplevels deletedToplevels

    let result : Op.AddOpResult =
      { toplevels = tls
        deleted_toplevels = dTLs
        user_functions = fns
        deleted_user_functions = dFns
        user_tipes = types
        deleted_user_tipes = dTypes }

    let emptyHandler (tlid : tlid) : PT.Toplevel.T =
      let ids : PT.Handler.ids =
        { moduleID = gid (); nameID = gid (); modifierID = gid () }
      PT.Toplevel.TLHandler
        { pos = { x = 0; y = 0 }
          tlid = tlid
          ast = PT.EBlank(gid ())
          spec = PT.Handler.HTTP("", "", ids) }


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
        let event : Op.AddOpEvent = { result = result; ``params`` = p }
        LibBackend.Pusher.pushAddOpEvent executionID canvasID event
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
        executionID
        canvasInfo.id
        canvasInfo.name
        canvasInfo.owner
        (Op.eventNameOfOp op)
        Map.empty)

    return event
  }
