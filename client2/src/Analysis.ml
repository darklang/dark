open Tea
open! Porting
module B = Blank
module P = Pointer
open Prelude
module RT = Runtime
module TL = Toplevel
open Types

let currentVarnamesFor (m : model) (target : (tlid * pointerData) option) :
    varName list =
  match target with
  | None -> []
  | Some (tlid, pd) -> getCurrentAvailableVarnames m tlid (P.toID pd)

let defaultResults : analysisResults =
  {liveValues= Belt.Map.Int.empty; availableVarnames= Belt.Map.Int.empty}

let getCurrentAnalysisResults (m : model) (tlid : tlid) : analysisResults =
  let traceIndex = cursor m tlid in
  let traceID =
    Dict.get (deTLID tlid) m.traces
    |> Option.andThen (List.getAt traceIndex)
    |> Option.map (fun x -> x.id)
    |> Option.withDefault "invalid trace key"
  in
  Dict.get traceID m.analyses |> Option.withDefault defaultResults

let record (old : analyses) (id : traceID) (result : analysisResults) :
    analyses =
  Dict.insert id result old

let cursor (m : model) (tlid : tlid) : int = cursor_ m.tlCursors tlid

let cursor_ (cursors : tLCursors) (tlid : tlid) : int =
  Dict.get (deTLID tlid) cursors |> Option.withDefault 0

let setCursor (m : model) (tlid : tlid) (cursorNum : int) : model =
  let newCursors = Dict.insert (deTLID tlid) cursorNum m.tlCursors in
  {m with tlCursors= newCursors}

let getCurrentLiveValuesDict (m : model) (tlid : tlid) : lvDict =
  getCurrentAnalysisResults m tlid |> fun x -> x.liveValues

let getCurrentLiveValue (m : model) (tlid : tlid) (ID id : id) : dval option =
  tlid |> getCurrentLiveValuesDict m |> Dict.get id

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
  |> Dict.get id |> Option.withDefault []

let getTraces (m : model) (tlid : tlid) : trace list =
  Dict.get (deTLID tlid) m.traces |> Option.withDefault []

let getCurrentTrace (m : model) (tlid : tlid) : trace option =
  Dict.get (deTLID tlid) m.traces
  |> Option.andThen (List.getAt (cursor m tlid))

let replaceFunctionResult (m : model) (tlid : tlid) (traceID : traceID)
    (callerID : id) (fnName : string) (hash : dvalArgsHash) (dval : dval) :
    model =
  let newResult = {fnName; callerID; argHash= hash; value= dval} in
  let traces =
    m.traces
    |> Dict.update (deTLID tlid) (fun ml ->
           ml
           |> Option.withDefault
                [ { id= traceID
                  ; input= Belt.Map.String.empty
                  ; functionResults= [newResult] } ]
           |> List.map (fun t ->
                  if t.id = traceID then
                    {t with functionResults= newResult :: t.functionResults}
                  else t )
           |> Some )
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
        | Some (PExpr (F (_, FnCall (_, args, _)))) -> threadPrevious ^ args
        | _ -> []
      in
      let argIDs = List.map B.toID args in
      let analyses = Dict.get traceID m.analyses in
      let dvals =
        match analyses with
        | Some analyses_ ->
            List.filterMap
              (fun id -> Dict.get (deID id) analyses_.liveValues)
              argIDs
        | None -> []
      in
      if List.length dvals = List.length argIDs then Some dvals else None
