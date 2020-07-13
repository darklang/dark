open Prelude

(* Tea *)
module Cmd = Tea.Cmd

(* Dark *)
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression

(* ---------------------- *)
(* Analyses *)
(* ---------------------- *)

(** [defaultTraceIDForTL ~tlid] returns the id of the "default" trace
 * for the top level with the given [tlid].
 *
 * This default trace exists in order to eg populate the autocomplete
 * with parameters of functions that haven't been called yet. *)
let defaultTraceIDForTL ~(tlid : TLID.t) =
  BsUuid.Uuid.V5.create
    ~name:(TLID.toString tlid)
    ~namespace:(`Uuid "00000000-0000-0000-0000-000000000000")
  |> BsUuid.Uuid.V5.toString


let getTraces (m : model) (tlid : TLID.t) : trace list =
  StrDict.get ~key:(TLID.toString tlid) m.traces
  |> Option.withDefaultLazy ~default:(fun () ->
         [(defaultTraceIDForTL ~tlid, Result.fail NoneYet)])


let getTrace (m : model) (tlid : TLID.t) (traceID : traceID) : trace option =
  getTraces m tlid |> List.find ~f:(fun (id, _) -> id = traceID)


let getStoredAnalysis (m : model) (traceID : traceID) : analysisStore =
  (* only handlers have analysis results, but lots of stuff expect this *)
  (* data to exist. It may be better to not do that, but this is fine *)
  (* for now. *)
  StrDict.get ~key:traceID m.analyses
  |> Option.withDefault ~default:LoadableNotInitialized


let record (old : analyses) (id : traceID) (result : analysisStore) : analyses =
  StrDict.insert ~key:id ~value:result old


let replaceFunctionResult
    (m : model)
    (tlid : TLID.t)
    (traceID : traceID)
    (callerID : ID.t)
    (fnName : string)
    (hash : dvalArgsHash)
    (hashVersion : int)
    (dval : dval) : model =
  let newResult =
    { fnName
    ; callerID
    ; argHash = hash
    ; argHashVersion = hashVersion
    ; value = dval }
  in
  let traces =
    m.traces
    |> StrDict.update ~key:(TLID.toString tlid) ~f:(fun ml ->
           ml
           |> Option.withDefault
                ~default:
                  [ ( traceID
                    , Result.succeed
                        { input = StrDict.empty
                        ; timestamp = ""
                        ; functionResults = [newResult] } ) ]
           |> List.map ~f:(fun ((tid, tdata) as t) ->
                  if tid = traceID
                  then
                    ( tid
                    , Result.map
                        ~f:(fun tdata ->
                          { tdata with
                            functionResults = newResult :: tdata.functionResults
                          })
                        tdata )
                  else t)
           |> fun x -> Some x)
  in
  {m with traces}


let getLiveValueLoadable (analysisStore : analysisStore) (ID id : ID.t) :
    executionResult loadable =
  match analysisStore with
  | LoadableSuccess dvals ->
      StrDict.get dvals ~key:id
      |> Option.map ~f:(fun dv -> LoadableSuccess dv)
      |> Option.withDefault ~default:LoadableNotInitialized
  | LoadableNotInitialized ->
      LoadableNotInitialized
  | LoadableLoading oldDvals ->
      oldDvals
      |> Option.andThen ~f:(StrDict.get ~key:id)
      |> Option.map ~f:(fun dv -> LoadableSuccess dv)
      |> Option.withDefault ~default:(LoadableLoading None)
  | LoadableError error ->
      LoadableError error


let getLiveValue' (analysisStore : analysisStore) (ID id : ID.t) : dval option =
  match analysisStore with
  | LoadableSuccess dvals ->
    ( match StrDict.get dvals ~key:id with
    | Some (ExecutedResult dval) | Some (NonExecutedResult dval) ->
        Some dval
    | _ ->
        None )
  | _ ->
      None


let getLiveValue (m : model) (id : ID.t) (traceID : traceID) : dval option =
  getLiveValue' (getStoredAnalysis m traceID) id


let getTipeOf' (analysisStore : analysisStore) (id : ID.t) : tipe option =
  getLiveValue' analysisStore id |> Option.map ~f:RT.typeOf


let getTipeOf (m : model) (id : ID.t) (traceID : traceID) : tipe option =
  getLiveValue m id traceID |> Option.map ~f:RT.typeOf


let getArguments
    (m : model) (tl : toplevel) (callerID : ID.t) (traceID : traceID) :
    dval list option =
  match TL.getAST tl with
  | Some ast ->
      let argIDs = ast |> AST.getArguments callerID |> List.map ~f:E.toID in
      let dvals =
        List.filterMap argIDs ~f:(fun id -> getLiveValue m id traceID)
      in
      if List.length dvals = List.length argIDs then Some dvals else None
  | None ->
      None


(** [getAvailableVarnames m tl id traceID] gets a list of (varname, dval option)s that are in scope
 * at an expression with the given [id] within the ast of the [tl]. The dval for a given varname
 * comes from the trace with [traceID]. *)
let getAvailableVarnames
    (m : model) (tl : toplevel) (id : ID.t) (traceID : traceID) :
    (string * dval option) list =
  (* TODO: Calling out is so slow that calculating on the fly is faster.
   * But we can also cache this so that's it's not in the display hot-path *)
  let tlid = TL.id tl in
  let traceDict =
    getTrace m tlid traceID
    |> Option.andThen ~f:(fun (_tid, td) -> td |> Result.toOption)
    |> Option.andThen ~f:(fun t -> Some t.input)
    |> Option.withDefault ~default:StrDict.empty
  in
  let varsFor (ast : FluidAST.t) =
    ast
    |> FluidAST.toExpr
    |> AST.variablesIn
    |> StrDict.get ~key:(ID.toString id)
    |> Option.withDefault ~default:StrDict.empty
    |> StrDict.toList
    |> List.map ~f:(fun (varname, id) -> (varname, getLiveValue m id traceID))
  in
  let glob =
    TL.allGloballyScopedVarnames m.dbs
    |> List.map ~f:(fun v -> (v, Some (DDB v)))
  in
  let inputVariables =
    RT.inputVariables tl
    |> List.map ~f:(fun varname ->
           (varname, traceDict |> StrDict.get ~key:varname))
  in
  match tl with
  | TLHandler h ->
      varsFor h.ast @ glob @ inputVariables
  | TLFunc fn ->
      varsFor fn.ufAST @ glob @ inputVariables
  | TLPmFunc _ | TLDB _ | TLTipe _ | TLGroup _ ->
      []


(* ---------------------- *)
(* Which trace is selected *)
(* ---------------------- *)

let selectedTraceID
    (tlTraceIDs : tlTraceIDs) (traces : trace list) (tlid : TLID.t) :
    traceID option =
  (* We briefly do analysis on a toplevel which does not have an *)
  (* analysis available, so be careful here. *)
  match TLIDDict.get ~tlid tlTraceIDs with
  | Some c ->
      Some c
  | None ->
      (* if we don't have it, pick the first trace *)
      List.head traces |> Option.map ~f:Tuple2.first


let selectedTrace
    (tlTraceIDs : tlTraceIDs) (traces : trace list) (tlid : TLID.t) :
    trace option =
  selectedTraceID tlTraceIDs traces tlid
  |> Option.andThen ~f:(fun traceID ->
         List.find ~f:(fun (id, _) -> id = traceID) traces)


let setSelectedTraceID (m : model) (tlid : TLID.t) (traceID : traceID) : model =
  let newCursors = TLIDDict.insert ~tlid ~value:traceID m.tlTraceIDs in
  {m with tlTraceIDs = newCursors}


let getSelectedTraceID (m : model) (tlid : TLID.t) : traceID option =
  let traces = getTraces m tlid in
  selectedTraceID m.tlTraceIDs traces tlid


(* ---------------------- *)
(* Communication with server *)
(* ---------------------- *)
module ReceiveAnalysis = struct
  let decode : (Js.Json.t, performAnalysisResult) Tea.Json.Decoder.t =
    let open Tea.Json.Decoder in
    map
      (fun msg -> msg)
      (field "detail" (Decoder (fun json -> Tea_result.Ok (Obj.magic json))))


  let listen ~key tagger =
    BrowserListeners.registerGlobal "receiveAnalysis" key tagger decode
end

module ReceiveFetch = struct
  let decode : (Js.Json.t, fetchResult) Tea.Json.Decoder.t =
    let open Tea.Json.Decoder in
    map
      (fun msg -> msg)
      (field "detail" (Decoder (fun json -> Tea_result.Ok (Obj.magic json))))


  let listen ~key tagger =
    BrowserListeners.registerGlobal "receiveFetch" key tagger decode
end

module NewTracePush = struct
  let decode =
    let open Tea.Json.Decoder in
    let traceID = map (fun id -> (id : traceID)) string in
    let tlids = list (map TLID.fromString Native.Decoder.wireIdentifier) in
    field "detail" (Native.Decoder.tuple2 traceID tlids)


  let listen ~key tagger =
    BrowserListeners.registerGlobal "newTracePush" key tagger decode
end

module New404Push = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.fof)


  let listen ~key tagger =
    BrowserListeners.registerGlobal "new404Push" key tagger decode
end

module NewPresencePush = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (list (Decoders.wrapDecoder Decoders.presenceMsg))


  let listen ~key tagger =
    BrowserListeners.registerGlobal "newPresencePush" key tagger decode
end

module AddOps = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.addOpAPIStrollerMsg)


  let listen ~key tagger =
    BrowserListeners.registerGlobal "addOp" key tagger decode
end

module WorkerStatePush = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.updateWorkerScheduleAPIResult)


  let listen ~key tagger =
    BrowserListeners.registerGlobal "workerStatePush" key tagger decode
end

(* Request analysis *)
module RequestAnalysis = struct
  external send : performAnalysisParams -> unit = "requestAnalysis"
    [@@bs.val] [@@bs.scope "window", "Dark", "analysis"]
end

module Fetcher = struct
  external request : fetchContext * fetchRequest -> unit = "fetch"
    [@@bs.val] [@@bs.scope "window", "Dark", "fetcher"]
end

external origin : string = "origin" [@@bs.val] [@@bs.scope "window", "location"]

external prefix : string = "testcafeInjectedPrefix"
  [@@bs.val] [@@bs.scope "window"]

let contextFromModel (m : model) : fetchContext =
  {canvasName = m.canvasName; csrfToken = m.csrfToken; origin; prefix}


let updateDBStats m tlid =
  Sync.attempt
    ~key:("update-db-stats-" ^ TLID.toString tlid)
    m
    (Tea_cmd.call (fun _ ->
         Fetcher.request
           (contextFromModel m, DbStatsFetch {dbStatsTlids = [tlid]})))


let getWorkerStats m tlid =
  Sync.attempt
    ~key:("get-worker-stats-" ^ TLID.toString tlid)
    m
    (Tea_cmd.call (fun _ ->
         Fetcher.request
           (contextFromModel m, WorkerStatsFetch {workerStatsTlid = tlid})))


(* [mergeTraces ~selectedTraceIDs ~onConflict ~oldTraces ~newTraces]
 * returns the results of "merging" [oldTraces] and [newTraces] by merging
 * the list of traces for each handler in the following manner:
 *
 * - Start with the handler's old traces, replacing any that share the id of a new trace using the [onConflict] function
 * - For any remaining new traces for that handler, prepend them.
 * - Drop any traces in excess of a hardcoded limit (currently 10 to match stored_event.load_events,
 *   plus 1 for any selected trace),
 *   keeping the newest traces and any [selectedTraceIDs]
 *
 * Note that this preserves the order of the newTraces relative to the oldTraces
 * (except for newTraces that share an old id -- we preserve the order of those in oldTraces)
 * and we also preserve the relative order of the selected traces.
 *)
let mergeTraces
    ~(selectedTraceIDs : tlTraceIDs)
    ~(onConflict : trace -> trace -> trace)
    ~(oldTraces : traces)
    ~(newTraces : traces) : traces =
  let maxTracesPerHandler = 10 (* shared with stored_event.load_events *) + 1 in
  StrDict.merge oldTraces newTraces ~f:(fun tlid oldList newList ->
      match (oldList, newList) with
      | None, None ->
          None
      | Some o, None ->
          Some o
      | None, Some n ->
          Some n
      | Some o, Some n ->
          (* Algorithm overview:
           * 1. merge the lists, updating the trace in the same position
           * if present, and adding it to the front otherwise.
           *
           * 2. drop any traces in excess of [maxTracesPerHandler]
           * from the back, making sure to preserve any selected traces.
           *
           ***
           * Example:
           *
           * o = [o1,o2,o3,o4,o5,o6,o7,o8,o9]
           * n = [n1,n2,n_3,n4,n5,n6]
           * n_3 shares an id with o3
           * selectedTrace in this handler = o8
           *
           * Pass 1 produces:
           * [n1,n2,n4,n5, o1,o2,n_3,o4,o5,o6,o7,o8,o9]
           * Pass 2 produces:
           * [n1,n2,n4,n5, o1,o2,n_3,o4,o5,o6, o8]
           *
           ***
           *)
          (* Pass 1: merge the lists, using foldr to preserve the order of n *)
          let merged =
            List.foldr n ~init:o ~f:(fun ((newID, newData) as new_) acc ->
                let found = ref false in
                let updated =
                  List.map acc ~f:(fun ((oldID, oldData) as old) ->
                      if oldID = newID
                      then (
                        found := true ;
                        onConflict old new_ )
                      else (oldID, oldData))
                in
                if !found (* deref, not "not" *)
                then updated
                else (newID, newData) :: acc)
          in
          (* Pass 2: preserve up to [maxTracesPerHandler] traces
           * guaranteeing that we won't duplicate the selected trace and
           * that we preserve the relative order
           *)
          let selectedTraceID =
            let tlid = TLID.fromString tlid in
            TLIDDict.get ~tlid selectedTraceIDs
          in
          let maxNonSelectedTraces = maxTracesPerHandler - 1 in
          let preserved, _ =
            List.foldl
              merged
              ~init:([], 0)
              ~f:(fun ((id, _) as currTrace) (acc, nonSelectedCount) ->
                if selectedTraceID = Some id
                then (currTrace :: acc, nonSelectedCount)
                else if nonSelectedCount < maxNonSelectedTraces
                then (currTrace :: acc, nonSelectedCount + 1)
                else (acc, nonSelectedCount))
          in
          (* We have to reverse because the fold prepended in reverse order *)
          Some (List.reverse preserved))


let requestTrace ?(force = false) m tlid traceID : model * msg Cmd.t =
  let should =
    (* DBs + Types dont have traces *)
    TL.get m tlid
    |> Option.map ~f:(fun tl ->
           not (TL.isDB tl || TL.isUserTipe tl || TL.isGroup tl))
    |> Option.withDefault ~default:false
  in
  if should
  then
    Sync.attempt
      ~force
      ~key:("tracefetch-" ^ traceID)
      m
      (Tea_cmd.call (fun _ ->
           Fetcher.request
             ( contextFromModel m
             , TraceFetch {gtdrpTlid = tlid; gtdrpTraceID = traceID} )))
  else (m, Cmd.none)


let requestAnalysis m tlid traceID : msg Cmd.t =
  let dbs = TD.values m.dbs in
  let userFns = TD.values m.userFunctions in
  let userTipes = TD.values m.userTipes in
  let trace = getTrace m tlid traceID in
  let tl = TL.get m tlid in
  let secrets = m.secrets in
  match (tl, trace) with
  | Some (TLHandler h), Some (_, Ok traceData) ->
      Tea_cmd.call (fun _ ->
          RequestAnalysis.send
            (AnalyzeHandler
               { handler = h
               ; traceID
               ; traceData
               ; dbs
               ; userFns
               ; userTipes
               ; secrets }))
  | Some (TLFunc f), Some (_, Ok traceData) ->
      Tea_cmd.call (fun _ ->
          RequestAnalysis.send
            (AnalyzeFunction
               {func = f; traceID; traceData; dbs; userFns; userTipes; secrets}))
  | _ ->
      Cmd.none


let updateTraces (m : model) (traces : traces) : model =
  let newTraces =
    mergeTraces
      ~selectedTraceIDs:m.tlTraceIDs
      ~onConflict:(fun (oldID, oldData) (newID, newData) ->
        (* Update if:
         * - new data is ok (successful fetch)
         * - old data is an error, so is new data, but different errors
         * *)
        if Result.isOk newData
           || ((not (Result.isOk oldData)) && oldData <> newData)
        then (newID, newData)
        else (oldID, oldData))
      ~oldTraces:m.traces
      ~newTraces:traces
  in
  {m with traces = newTraces}


let analyzeFocused (m : model) : model * msg Cmd.t =
  match CursorState.tlidOf m.cursorState with
  | Some tlid ->
      let trace =
        getSelectedTraceID m tlid
        |> Option.andThen ~f:(fun traceID ->
               getTrace m tlid traceID
               |> Option.orElse (Some (traceID, Error NoneYet)))
      in
      ( match trace with
      | Some (_, Error MaximumCallStackError) ->
          (* Don't attempt to refetch if we blew the stack trying
           * to decode it *)
          (m, Cmd.none)
      | Some (traceID, Error NoneYet) ->
          (* Fetch the trace data if it's missing. *)
          requestTrace m tlid traceID
      | Some (traceID, Ok _) ->
          (* Run the analysis, if missing *)
          (m, requestAnalysis m tlid traceID)
      | None ->
          (m, Cmd.none) )
  | None ->
      (m, Cmd.none)
