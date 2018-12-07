open! Porting
open Types

let markRequestInModel (m : model) : model =
  {m with syncState = {inFlight = true; ticks = 0}}


let markTickInModel (m : model) : model =
  let oldSyncState = m.syncState in
  {m with syncState = {oldSyncState with ticks = oldSyncState.ticks + 1}}


let markResponseInModel (m : model) : model =
  {m with syncState = {inFlight = false; ticks = 0}}


let timedOut (s : syncState) : bool = s.ticks mod 10 = 0 && s.ticks <> 0

let toAnalyse (m : model) : tlid list =
  match m.cursorState with
  | Selecting (tlid, _) ->
      [tlid]
  | Entering (Filling (tlid, _)) ->
      [tlid]
  | Dragging (tlid, _, _, _) ->
      [tlid]
  | _ ->
      let ids = List.map (fun x -> x.id) (Toplevel.all m) in
      let index =
        let length = List.length ids in
        if length > 0 then Some (Util.random () mod length) else None
      in
      index
      |> Option.andThen (fun i -> List.getAt i ids)
      |> Option.map (fun e -> [e])
      |> Option.withDefault []


let fetch (m : model) : model * msg Tea.Cmd.t =
  if (not m.syncState.inFlight) || timedOut m.syncState
  then
    ( markRequestInModel m
    , RPC.getAnalysisRPC
        (contextFromModel m)
        {tlids = toAnalyse m; latest404 = m.latest404} )
  else (markTickInModel m, Tea.Cmd.none)
