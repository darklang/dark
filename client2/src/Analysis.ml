open Tea
open! Porting
module B = Blank
module P = Pointer
open Prelude
module RT = Runtime
module TL = Toplevel
open Types

let defaultResults : analysisResults =
  {liveValues= IntDict.empty; availableVarnames= IntDict.empty}

let cursor_ (cursors : tLCursors) (tlid : tlid) : int =
  IntDict.get (deTLID tlid) cursors |> Option.withDefault 0

let cursor (m : model) (tlid : tlid) : int = cursor_ m.tlCursors tlid

let setCursor (m : model) (tlid : tlid) (cursorNum : int) : model =
  let newCursors = IntDict.insert (deTLID tlid) cursorNum m.tlCursors in
  {m with tlCursors= newCursors}

let getCurrentAnalysisResults (m : model) (tlid : tlid) : analysisResults =
  let traceIndex = cursor m tlid in
  let traceID =
    IntDict.get (deTLID tlid) m.traces
    |> Option.andThen (List.getAt traceIndex)
    |> Option.map (fun x -> x.traceID)
    |> Option.withDefault "invalid trace key"
  in
  StrDict.get traceID m.analyses |> Option.withDefault defaultResults

let record (old : analyses) (id : traceID) (result : analysisResults) :
    analyses =
  StrDict.insert id result old

let getCurrentLiveValuesDict (m : model) (tlid : tlid) : lvDict =
  getCurrentAnalysisResults m tlid |> fun x -> x.liveValues

let getCurrentLiveValue (m : model) (tlid : tlid) (ID id : id) : dval option =
  tlid |> getCurrentLiveValuesDict m |> IntDict.get id

let getCurrentTipeOf (m : model) (tlid : tlid) (id : id) : tipe option =
  match getCurrentLiveValue m tlid id with
  | None -> None
  | Some dv -> Some (RT.typeOf dv)

let getCurrentAvailableVarnamesDict (m : model) (tlid : tlid) : avDict =
  getCurrentAnalysisResults m tlid |> fun x -> x.availableVarnames

let getCurrentAvailableVarnames (m : model) (tlid : tlid) (ID id : id) :
    varName list =
  tlid
  |> getCurrentAvailableVarnamesDict m
  |> IntDict.get id |> Option.withDefault []

let currentVarnamesFor (m : model) (target : (tlid * pointerData) option) :
    varName list =
  match target with
  | None -> []
  | Some (tlid, pd) -> getCurrentAvailableVarnames m tlid (P.toID pd)

let getTraces (m : model) (tlid : tlid) : trace list =
  IntDict.get (deTLID tlid) m.traces |> Option.withDefault []

let getCurrentTrace (m : model) (tlid : tlid) : trace option =
  IntDict.get (deTLID tlid) m.traces
  |> Option.andThen (List.getAt (cursor m tlid))

let replaceFunctionResult (m : model) (tlid : tlid) (traceID : traceID)
    (callerID : id) (fnName : string) (hash : dvalArgsHash) (dval : dval) :
    model =
  let newResult = {fnName; callerID; argHash= hash; value= dval} in
  let traces =
    m.traces
    |> IntDict.update (deTLID tlid) (fun ml ->
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
              (fun id -> IntDict.get (deID id) analyses_.liveValues)
              argIDs
        | None -> []
      in
      if List.length dvals = List.length argIDs then Some dvals else None

module ReceiveAnalysis = struct
  let decode =
    let open Tea.Json.Decoder in
    map (fun msg -> msg)
      (field "detail" string)
  let listen ?(key="") tagger =
    Porting.registerGlobal "receiveAnalysis" key tagger decode
end

(* Request analysis *)

module RequestAnalysis = struct

  external send : (int list -> unit) = "requestAnalysis" [@@bs.val][@@bs.scope "window", "Dark", "analysis"]

end