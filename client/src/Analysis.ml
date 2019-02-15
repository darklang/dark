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

(* "current" in this indicates that it uses the cursor to pick the right inputValue *)

let defaultResults : analysisResults = {liveValues = StrDict.empty}

let cursor' (tlCursors : tlCursors) (tlid : tlid) : traceID option =
  (* We briefly do analysis on a toplevel which does not have an *)
  (* analysis available, so be careful here. *)
  StrDict.get ~key:(deTLID tlid) tlCursors


let cursor (m : model) (tlid : tlid) : traceID option =
  cursor' m.tlCursors tlid


let setCursor (m : model) (tlid : tlid) (traceID : traceID) : model =
  let newCursors =
    StrDict.insert ~key:(deTLID tlid) ~value:traceID m.tlCursors
  in
  {m with tlCursors = newCursors}


let getAnalysisResults (m : model) (traceID : traceID) : analysisResults option
    =
  (* only handlers have analysis results, but lots of stuff expect this *)
  (* data to exist. It may be better to not do that, but this is fine *)
  (* for now. *)
  StrDict.get ~key:traceID m.analyses


let getCurrentAnalysisResults (m : model) (tlid : tlid) : analysisResults =
  cursor m tlid
  |> Option.andThen ~f:(getAnalysisResults m)
  |> Option.withDefault ~default:defaultResults


let record (old : analyses) (id : traceID) (result : analysisResults) :
    analyses =
  StrDict.insert ~key:id ~value:result old


let getCurrentLiveValuesDict (m : model) (tlid : tlid) : lvDict =
  getCurrentAnalysisResults m tlid |> fun x -> x.liveValues


let getCurrentLiveValue (m : model) (tlid : tlid) (ID id : id) : dval option =
  tlid |> getCurrentLiveValuesDict m |> StrDict.get ~key:id


let getCurrentTipeOf (m : model) (tlid : tlid) (id : id) : tipe option =
  match getCurrentLiveValue m tlid id with
  | None ->
      None
  | Some dv ->
      Some (RT.typeOf dv)


(* TODO: copied from Libexecution/http.ml *)
let route_variables (route : string) : string list =
  let split_uri_path (path : string) : string list =
    let subs = String.split ~on:"/" path in
    List.filter ~f:(fun x -> String.length x > 0) subs
  in
  route
  |> split_uri_path
  |> List.filter ~f:(String.startsWith ~prefix:":")
  |> List.map ~f:(String.dropLeft ~count:1 (* ":" *))


let getCurrentAvailableVarnames (m : model) (tlid : tlid) (ID id : id) :
    varName list =
  (* TODO: Calling out is so slow that calculating on the fly is faster. But we
   * can also cache this so that's it's not in the display hot-path. *)
  let varsFor ast =
    ast
    |> AST.variablesIn
    |> StrDict.get ~key:id
    |> Option.withDefault ~default:[]
  in
  let tl = TL.getTL m tlid in
  let dbs = TL.allDBNames m.toplevels in
  match tl.data with
  | TLHandler h ->
      let extras =
        match h.spec.module_ with
        | F (_, m) when String.toLower m = "http" ->
            let fromRoute =
              h.spec.name
              |> Blank.toMaybe
              |> Option.map ~f:route_variables
              |> Option.withDefault ~default:[]
            in
            ["request"] @ fromRoute
        | F (_, m) when String.toLower m = "cron" ->
            []
        | F (_, _) ->
            ["event"]
        | _ ->
            ["request"; "event"]
      in
      varsFor h.ast @ dbs @ extras
  | TLDB _ ->
      []
  | TLFunc fn ->
      let params =
        fn.ufMetadata.ufmParameters
        |> List.filterMap ~f:(fun p -> Blank.toMaybe p.ufpName)
      in
      varsFor fn.ufAST @ params


let currentVarnamesFor (m : model) (target : (tlid * pointerData) option) :
    varName list =
  match target with
  | Some (tlid, (PExpr _ as pd)) ->
      getCurrentAvailableVarnames m tlid (P.toID pd)
  | _ ->
      []


let getTraces (m : model) (tlid : tlid) : trace list =
  StrDict.get ~key:(deTLID tlid) m.traces |> Option.withDefault ~default:[]


let getCurrentTrace (m : model) (tlid : tlid) : trace option =
  getTraces m tlid
  |> List.find ~f:(fun (traceID, _) -> cursor m tlid = Some traceID)


let replaceFunctionResult
    (m : model)
    (tlid : tlid)
    (traceID : traceID)
    (callerID : id)
    (fnName : string)
    (hash : dvalArgsHash)
    (dval : dval) : model =
  let newResult = {fnName; callerID; argHash = hash; value = dval} in
  let traces =
    m.traces
    |> StrDict.update ~key:(deTLID tlid) ~f:(fun ml ->
           ml
           |> Option.withDefault
                ~default:
                  [ ( traceID
                    , Some
                        {input = StrDict.empty; functionResults = [newResult]}
                    ) ]
           |> List.map ~f:(fun ((tid, tdata) as t) ->
                  if tid = traceID
                  then
                    ( tid
                    , Option.map tdata ~f:(fun tdata ->
                          { tdata with
                            functionResults =
                              newResult :: tdata.functionResults } ) )
                  else t )
           |> fun x -> Some x )
  in
  {m with traces}


let getArguments (m : model) (tlid : tlid) (traceID : traceID) (callerID : id)
    : dval list option =
  let tl = TL.get m tlid in
  match tl with
  | None ->
      None
  | Some tl ->
      let caller = TL.find tl callerID in
      let threadPrevious =
        match TL.rootOf tl with
        | Some (PExpr expr) ->
            Option.toList (AST.threadPrevious callerID expr)
        | _ ->
            []
      in
      let args =
        match caller with
        | Some (PExpr (F (_, FnCall (_, args, _)))) ->
            threadPrevious @ args
        | _ ->
            []
      in
      let argIDs = List.map ~f:B.toID args in
      let analyses = StrDict.get ~key:traceID m.analyses in
      let dvals =
        match analyses with
        | Some analyses_ ->
            List.filterMap
              ~f:(fun id -> StrDict.get ~key:(deID id) analyses_.liveValues)
              argIDs
        | None ->
            []
      in
      if List.length dvals = List.length argIDs then Some dvals else None


module ReceiveAnalysis = struct
  let decode : (Js.Json.t, performAnalysisResult) Tea.Json.Decoder.t =
    let open Tea.Json.Decoder in
    map
      (fun msg -> msg)
      (field "detail" (Decoder (fun json -> Tea_result.Ok (Obj.magic json))))


  let listen ~key tagger =
    Native.registerGlobal "receiveAnalysis" key tagger decode
end

module ReceiveTraces = struct
  let decode : (Js.Json.t, traceFetchResult) Tea.Json.Decoder.t =
    let open Tea.Json.Decoder in
    map
      (fun msg -> msg)
      (field "detail" (Decoder (fun json -> Tea_result.Ok (Obj.magic json))))


  let listen ~key tagger =
    Native.registerGlobal "receiveTraces" key tagger decode
end

module NewTracePush = struct
  let decode =
    let open Tea.Json.Decoder in
    let open Native.Decoder in
    let tlid = map (fun id -> TLID id) wireIdentifier in
    let traceID = map (fun id -> (id : traceID)) string in
    field "detail" (pair tlid traceID)


  let listen ~key tagger =
    Native.registerGlobal "newTracePush" key tagger decode
end

module New404Push = struct
  let decode =
    let open Tea.Json.Decoder in
    let open Native.Decoder in
    let mk404WithTs (space, path, modifier) = {space; path; modifier} in
    map mk404WithTs (field "detail" (tuple3 string string string))


  let listen ~key tagger = Native.registerGlobal "new404Push" key tagger decode
end

(* Request analysis *)

module RequestAnalysis = struct
  external send : performAnalysisParams -> unit = "requestAnalysis"
    [@@bs.val] [@@bs.scope "window", "Dark", "analysis"]
end

module RequestTraces = struct
  external send : traceFetchContext * getTraceDataRPCParams -> unit = "fetch"
    [@@bs.val] [@@bs.scope "window", "Dark", "traceFetcher"]
end

external origin : string = "origin"
  [@@bs.val] [@@bs.scope "window", "location"]

external prefix : string = "testcafeInjectedPrefix"
  [@@bs.val] [@@bs.scope "window"]

let contextFromModel (m : model) : traceFetchContext =
  {canvasName = m.canvasName; csrfToken = m.csrfToken; origin; prefix}


let analyzeFocused (m : model) : 'msg Cmd.t =
  match tlidOf m.cursorState with
  | Some tlid ->
    ( match getCurrentTrace m tlid with
    | None ->
        Cmd.none
    | Some (traceID, None) ->
        (* Fetch the trace data, if missing *)
        Tea_cmd.call (fun _ ->
            RequestTraces.send
              (contextFromModel m, {gtdrpTlid = tlid; gtdrpTraceID = traceID})
        )
    | Some (traceID, Some traceData) ->
        if getAnalysisResults m traceID = None
        then
          (* Run the analysis, if missing *)
          let h = TL.getTL m tlid |> TL.asHandler in
          match h with
          | None ->
              Cmd.none
          | Some h ->
              let dbs = TL.dbs m.toplevels in
              let userFns = m.userFunctions in
              Tea_cmd.call (fun _ ->
                  RequestAnalysis.send
                    {handler = h; traceID; traceData; dbs; userFns} )
        else Cmd.none )
  | None ->
      Cmd.none
