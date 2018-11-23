open! Porting
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module RT = Runtime
module TL = Toplevel

(* "current" in this indicates that it uses the cursor to pick the right inputValue *)

let defaultResults : analysisResults =
  {liveValues= StrDict.empty; availableVarnames= StrDict.empty}

let cursor_ (cursors : tLCursors) (tlid : tlid) : int =
  (* We briefly do analysis on a toplevel which does not have an *)
  (* analysis available, so be careful here. *)
  StrDict.get (deTLID tlid) cursors |> Option.withDefault 0

let cursor (m : model) (tlid : tlid) : int = cursor_ m.tlCursors tlid

let setCursor (m : model) (tlid : tlid) (cursorNum : int) : model =
  let newCursors = StrDict.insert (deTLID tlid) cursorNum m.tlCursors in
  {m with tlCursors= newCursors}

let getCurrentAnalysisResults (m : model) (tlid : tlid) : analysisResults =
  let traceIndex = cursor m tlid in
  let traceID =
    StrDict.get (deTLID tlid) m.traces
    |> Option.andThen (List.getAt traceIndex)
    |> Option.map (fun x -> x.traceID)
    |> Option.withDefault "invalid trace key"
  in
  (* only handlers have analysis results, but lots of stuff expect this *)
  (* data to exist. It may be better to not do that, but this is fine *)
  (* for now. *)
  StrDict.get traceID m.analyses |> Option.withDefault defaultResults

let record (old : analyses) (id : traceID) (result : analysisResults) :
    analyses =
  StrDict.insert id result old

let getCurrentLiveValuesDict (m : model) (tlid : tlid) : lvDict =
  getCurrentAnalysisResults m tlid |> fun x -> x.liveValues

let getCurrentLiveValue (m : model) (tlid : tlid) (ID id : id) : dval option =
  tlid |> getCurrentLiveValuesDict m |> StrDict.get id

let getCurrentTipeOf (m : model) (tlid : tlid) (id : id) : tipe option =
  match getCurrentLiveValue m tlid id with
  | None -> None
  | Some dv -> Some (RT.typeOf dv)

(* TODO: copied from Libexecution/http.ml *)
let route_variable_pairs (route: string) : (int * string) list =
  let split_uri_path (path: string) : string list =
    let subs = String.split "/" path in
    List.filter (fun x -> String.length x > 0) subs
  in
  route
  |> split_uri_path
  |> List.mapi (fun i x -> (i, x))
  |> List.filter (fun (_, x) -> String.startsWith ":" x)
  |> List.map (fun (i, x) -> (i, String.dropLeft 1 (* ":" *) x))

let route_variables (route: string) : string list =
  route
  |> route_variable_pairs
  |> List.map Tuple.second


let getCurrentAvailableVarnames (m : model) (tlid : tlid) (ID id : id) :
    varName list =
  (* TODO: Calling out is so slow that calculating on the fly is faster. But we
   * can also cache this so that's it's not in the display hot-path. *)
  let varsFor ast =
    ast
    |> AST.variablesIn
    |> StrDict.get id
    |> Option.withDefault []
  in
  let tl = TL.getTL m tlid in
  let dbs = m.toplevels
            |> List.filterMap TL.asDB
            |> List.map (fun db -> db.dbName)
  in
  match tl.data with
  | TLHandler h ->
    let extras =
      match h.spec.module_ with
      | F (_, m) when String.toLower m = "http" ->
        let fromRoute =
          h.spec.name
          |> Blank.toMaybe
          |> Option.map route_variables
          |> Option.withDefault []
        in
        ["request"] @ fromRoute
      | F (_, m) when String.toLower "cron" = m -> []
      | F (_, _) -> ["event"]
      | _ -> ["request"; "event"]
    in
    (varsFor h.ast) @ dbs @ extras
  | TLDB _ -> []
  | TLFunc fn ->
    let params =
      fn.ufMetadata.ufmParameters
      |> List.filterMap (fun p -> Blank.toMaybe p.ufpName)
    in
    varsFor fn.ufAST @ params




let currentVarnamesFor (m : model) (target : (tlid * pointerData) option) :
    varName list =
  match target with
  | None -> []
  | Some (tlid, pd) -> getCurrentAvailableVarnames m tlid (P.toID pd)

let getTraces (m : model) (tlid : tlid) : trace list =
  StrDict.get (deTLID tlid) m.traces |> Option.withDefault []

let getCurrentTrace (m : model) (tlid : tlid) : trace option =
  StrDict.get (deTLID tlid) m.traces
  |> Option.andThen (List.getAt (cursor m tlid))

let replaceFunctionResult (m : model) (tlid : tlid) (traceID : traceID)
    (callerID : id) (fnName : string) (hash : dvalArgsHash) (dval : dval) :
    model =
  let newResult = {fnName; callerID; argHash= hash; value= dval} in
  let traces =
    m.traces
    |> StrDict.update (deTLID tlid) (fun ml ->
           ml
           |> Option.withDefault
                [{traceID; input= StrDict.empty; functionResults= [newResult]}]
           |> List.map (fun t ->
                  if t.traceID = traceID then
                    {t with functionResults= newResult :: t.functionResults}
                  else t )
           |> fun x -> Some x )
  in
  {m with traces}

let getArguments (m : model) (tlid : tlid) (traceID : traceID) (callerID : id)
    : dval list option =
  let tl = TL.get m tlid in
  match tl with
  | None -> None
  | Some tl ->
      let caller = TL.find tl callerID in
      let threadPrevious =
        match TL.rootOf tl with
        | Some (PExpr expr) -> Option.toList (AST.threadPrevious callerID expr)
        | _ -> []
      in
      let args =
        match caller with
        | Some (PExpr (F (_, FnCall (_, args, _)))) -> threadPrevious @ args
        | _ -> []
      in
      let argIDs = List.map B.toID args in
      let analyses = StrDict.get traceID m.analyses in
      let dvals =
        match analyses with
        | Some analyses_ ->
            List.filterMap
              (fun id -> StrDict.get (deID id) analyses_.liveValues)
              argIDs
        | None -> []
      in
      if List.length dvals = List.length argIDs then Some dvals else None

module ReceiveAnalysis = struct
  let decode =
    let open Tea.Json.Decoder in
    map (fun msg -> msg)
      (field "detail" string)
  let listen ~key tagger =
    Porting.registerGlobal "receiveAnalysis" key tagger decode
end

(* Request analysis *)

module RequestAnalysis = struct

  external send : (string -> unit) = "requestAnalysis" [@@bs.val][@@bs.scope "window", "Dark", "analysis"]

end
