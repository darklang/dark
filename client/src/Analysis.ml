open Tc
open Prelude
open Types

(* Tea *)
module Cmd = Tea.Cmd

(* Dark *)
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel
module TD = TLIDDict

(* ---------------------- *)
(* Analyses *)
(* ---------------------- *)

let getTraces (m : model) (tlid : tlid) : trace list =
  StrDict.get ~key:(deTLID tlid) m.traces
  |> Option.withDefault
       ~default:
         [ ( BsUuid.Uuid.V5.create
               ~name:(deTLID tlid)
               ~namespace:(`Uuid "00000000-0000-0000-0000-000000000000")
             |> BsUuid.Uuid.V5.toString
           , None ) ]


let getTrace (m : model) (tlid : tlid) (traceID : traceID) : trace option =
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
    (tlid : tlid)
    (traceID : traceID)
    (callerID : id)
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
    |> StrDict.update ~key:(deTLID tlid) ~f:(fun ml ->
           ml
           |> Option.withDefault
                ~default:
                  [ ( traceID
                    , Some
                        { input = StrDict.empty
                        ; timestamp = ""
                        ; functionResults = [newResult] } ) ]
           |> List.map ~f:(fun ((tid, tdata) as t) ->
                  if tid = traceID
                  then
                    ( tid
                    , Option.map tdata ~f:(fun tdata ->
                          { tdata with
                            functionResults = newResult :: tdata.functionResults
                          }) )
                  else t)
           |> fun x -> Some x)
  in
  {m with traces}


let getLiveValueLoadable (analysisStore : analysisStore) (ID id : id) :
    dval loadable =
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


let getLiveValue' (analysisStore : analysisStore) (ID id : id) : dval option =
  match analysisStore with
  | LoadableSuccess dvals ->
      StrDict.get dvals ~key:id
  | _ ->
      None


let getLiveValue (m : model) (id : id) (traceID : traceID) : dval option =
  getLiveValue' (getStoredAnalysis m traceID) id


let getTipeOf' (analysisStore : analysisStore) (id : id) : tipe option =
  getLiveValue' analysisStore id |> Option.map ~f:RT.typeOf


let getTipeOf (m : model) (id : id) (traceID : traceID) : tipe option =
  getLiveValue m id traceID |> Option.map ~f:RT.typeOf


let getArguments (m : model) (tl : toplevel) (callerID : id) (traceID : traceID)
    : dval list option =
  let threadPrevious =
    tl
    |> TL.getAST
    |> Option.andThen ~f:(AST.threadPrevious callerID)
    |> Option.toList
  in
  let caller =
    tl |> TL.getAST |> Option.andThen ~f:(FluidExpression.find callerID)
  in
  let args =
    match caller with
    | Some (EFnCall (_, _, args, _)) ->
        threadPrevious @ args
    | _ ->
        []
  in
  let argIDs = List.map ~f:FluidExpression.id args in
  let dvals = List.filterMap argIDs ~f:(fun id -> getLiveValue m id traceID) in
  if List.length dvals = List.length argIDs then Some dvals else None


let getAvailableVarnames
    (m : model) (tl : toplevel) (ID id : id) (traceID : traceID) :
    (varName * dval option) list =
  (* TODO: Calling out is so slow that calculating on the fly is faster. But
   * we can also cache this so that's it's not in the display hot-path. *)
  let tlid = TL.id tl in
  let traceDict =
    getTrace m tlid traceID
    |> Option.andThen ~f:(fun (_tid, td) -> td)
    |> Option.andThen ~f:(fun t -> Some t.input)
    |> Option.withDefault ~default:StrDict.empty
  in
  let varsFor ast =
    ast
    |> AST.variablesIn
    |> StrDict.get ~key:id
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
  let astVars =
    TL.getAST tl |> Option.map ~f:varsFor |> Option.withDefault ~default:[]
  in
  astVars @ glob @ inputVariables


(* ---------------------- *)
(* Which trace is selected *)
(* ---------------------- *)

let selectedTrace (tlTraceIDs : tlTraceIDs) (traces : trace list) (tlid : tlid)
    : traceID option =
  (* We briefly do analysis on a toplevel which does not have an *)
  (* analysis available, so be careful here. *)
  match TLIDDict.get ~tlid tlTraceIDs with
  | Some c ->
      Some c
  | None ->
      (* if we don't have it, pick the first trace *)
      List.head traces |> Option.map ~f:Tuple2.first


let setSelectedTraceID (m : model) (tlid : tlid) (traceID : traceID) : model =
  let newCursors = TLIDDict.insert ~tlid ~value:traceID m.tlTraceIDs in
  {m with tlTraceIDs = newCursors}


let getSelectedTraceID (m : model) (tlid : tlid) : traceID option =
  let traces = getTraces m tlid in
  selectedTrace m.tlTraceIDs traces tlid


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
    Native.registerGlobal "receiveAnalysis" key tagger decode
end

module ReceiveFetch = struct
  let decode : (Js.Json.t, fetchResult) Tea.Json.Decoder.t =
    let open Tea.Json.Decoder in
    map
      (fun msg -> msg)
      (field "detail" (Decoder (fun json -> Tea_result.Ok (Obj.magic json))))


  let listen ~key tagger =
    Native.registerGlobal "receiveFetch" key tagger decode
end

module NewTracePush = struct
  let decode =
    let open Tea.Json.Decoder in
    let open Native.Decoder in
    let traceID = map (fun id -> (id : traceID)) string in
    let tlids = list (map (fun id -> TLID id) wireIdentifier) in
    field "detail" (pair traceID tlids)


  let listen ~key tagger =
    Native.registerGlobal "newTracePush" key tagger decode
end

module New404Push = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.fof)


  let listen ~key tagger = Native.registerGlobal "new404Push" key tagger decode
end

module NewPresencePush = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (list (Decoders.wrapDecoder Decoders.presenceMsg))


  let listen ~key tagger =
    Native.registerGlobal "newPresencePush" key tagger decode
end

module AddOp = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.addOpRPCStrollerMsg)


  let listen ~key tagger = Native.registerGlobal "addOp" key tagger decode
end

module WorkerStatePush = struct
  let decode =
    let open Tea.Json.Decoder in
    field "detail" (Decoders.wrapDecoder Decoders.updateWorkerScheduleRPCResult)


  let listen ~key tagger =
    Native.registerGlobal "workerStatePush" key tagger decode
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


let updateDBStats m (TLID tlid) =
  Sync.attempt
    ~key:("update-db-stats-" ^ tlid)
    m
    (Tea_cmd.call (fun _ ->
         Fetcher.request
           (contextFromModel m, DbStatsFetch {dbStatsTlids = [TLID tlid]})))


let getWorkerStats m (TLID tlid) =
  Sync.attempt
    ~key:("get-worker-stats-" ^ tlid)
    m
    (Tea_cmd.call (fun _ ->
         Fetcher.request
           (contextFromModel m, WorkerStatsFetch {workerStatsTlid = TLID tlid})))


let mergeTraces ~(onConflict : trace -> trace -> trace) oldTraces newTraces :
    traces =
  StrDict.merge oldTraces newTraces ~f:(fun _tlid oldList newList ->
      match (oldList, newList) with
      | None, None ->
          None
      | Some o, None ->
          Some o
      | None, Some n ->
          Some n
      | Some o, Some n ->
          (* merge the lists, updating the trace in the same position
              * if present, and adding it to the front otherwise. *)
          Some
            (List.foldl n ~init:o ~f:(fun ((newID, newData) as new_) acc ->
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
                 else (newID, newData) :: acc)))


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
  match (tl, trace) with
  | Some (TLHandler h), Some (_, Some traceData) ->
      Tea_cmd.call (fun _ ->
          RequestAnalysis.send
            (AnalyzeHandler
               {handler = h; traceID; traceData; dbs; userFns; userTipes}))
  | Some (TLFunc f), Some (_, Some traceData) ->
      Tea_cmd.call (fun _ ->
          RequestAnalysis.send
            (AnalyzeFunction
               {func = f; traceID; traceData; dbs; userFns; userTipes}))
  | _ ->
      Cmd.none


let analyzeFocused (m : model) : model * msg Cmd.t =
  match tlidOf m.cursorState with
  | Some tlid ->
      let trace =
        getSelectedTraceID m tlid |> Option.andThen ~f:(getTrace m tlid)
      in
      ( match trace with
      | Some (traceID, None) ->
          (* Fetch the trace data, if missing *)
          requestTrace m tlid traceID
      | Some (traceID, Some _) ->
          (* Run the analysis, if missing *)
          (m, requestAnalysis m tlid traceID)
      | None ->
          (m, Cmd.none) )
  | None ->
      (m, Cmd.none)
