open Belt
open Tea
open Porting
module B = Blank
module P = Pointer
open Prelude
module RT = Runtime
module TL = Toplevel
open Types

let currentVarnamesFor m target =
  match target with
  | None -> []
  | Some (tlid, pd) -> getCurrentAvailableVarnames m tlid (P.toID pd)

let defaultResults = {liveValues= Dict.empty; availableVarnames= Dict.empty}

let getCurrentAnalysisResults m tlid =
  let traceIndex = cursor m tlid in
  let traceID =
    Dict.get (deTLID tlid) m.traces
    |> Option.andThen (List.get traceIndex)
    |> Option.map (fun x -> x.id)
    |> Maybe.withDefault "invalid trace key"
  in
  Dict.get traceID m.analyses |> Maybe.withDefault defaultResults

let record old id result = Dict.insert id result old

let cursor m tlid = cursor_ m.tlCursors tlid

let cursor_ cursors tlid =
  Dict.get (deTLID tlid) cursors |> Maybe.withDefault 0

let setCursor m tlid cursorNum =
  let newCursors = Dict.insert (deTLID tlid) cursorNum m.tlCursors in
  {m with tlCursors= newCursors}

let getCurrentLiveValuesDict m tlid =
  getCurrentAnalysisResults m tlid |> fun x -> x.liveValues

let getCurrentLiveValue m tlid (ID id) =
  tlid |> getCurrentLiveValuesDict m |> Dict.get id

let getCurrentTipeOf m tlid id =
  match getCurrentLiveValue m tlid id with
  | None -> None
  | Some dv -> Some (RT.typeOf dv)

let getCurrentAvailableVarnamesDict m tlid =
  getCurrentAnalysisResults m tlid |> fun x -> x.availableVarnames

let getCurrentAvailableVarnames m tlid (ID id) =
  tlid
  |> getCurrentAvailableVarnamesDict m
  |> Dict.get id |> Maybe.withDefault []

let getTraces m tlid = Dict.get (deTLID tlid) m.traces |> Maybe.withDefault []

let getCurrentTrace m tlid =
  Dict.get (deTLID tlid) m.traces |> Option.andThen (List.get (cursor m tlid))

let replaceFunctionResult m tlid traceID callerID fnName hash dval =
  let newResult = {fnName; callerID; argHash= hash; value= dval} in
  let traces =
    m.traces
    |> Dict.update (deTLID tlid) (fun ml ->
           ml
           |> Maybe.withDefault
                [{id= traceID; input= Dict.empty; functionResults= [newResult]}]
           |> List.map (fun t ->
                  if t.id = traceID then
                    {t with functionResults= newResult :: t.functionResults}
                  else t )
           |> Some )
  in
  {m with traces}

let getArguments m tlid traceID callerID =
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
