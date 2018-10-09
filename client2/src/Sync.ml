open Belt
open Tea
open! Porting
open Types

let markRequestInModel m =
  let oldSyncState = m.syncState in
  {m with syncState= {oldSyncState with inFlight= true; ticks= 0}}

let markTickInModel m =
  let oldSyncState = m.syncState in
  {m with syncState= {oldSyncState with ticks= oldSyncState.ticks + 1}}

let markResponseInModel m =
  let oldSyncState = m.syncState in
  {m with syncState= {oldSyncState with inFlight= false; ticks= 0}}

let timedOut s = (s.ticks % 10 = 0 && s.ticks) <> 0

let fetch m =
  if (not m.syncState.inFlight) || timedOut m.syncState then
    (markRequestInModel m, RPC.getAnalysisRPC m.canvasName (toAnalyse m))
  else (markTickInModel m, Cmd.none)

let toAnalyse m =
  match m.cursorState with
  | Selecting (tlid, _) -> [tlid]
  | Entering (Filling (tlid, _)) -> [tlid]
  | Dragging (tlid, _, _, _) -> [tlid]
  | _ ->
      let ids = List.map (fun x -> x.id) (Toplevel.all m) in
      let index =
        let length = List.length ids in
        if length > 0 then Some (Util.random () % length) else None
      in
      index
      |> Option.andThen (fun i -> List.get i ids)
      |> Option.map (fun e -> [e])
      |> Maybe.withDefault []
