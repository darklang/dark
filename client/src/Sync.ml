open Tc
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
      let ids = List.map ~f:(fun x -> x.id) (Toplevel.all m) in
      let index =
        let length = List.length ids in
        if length > 0 then Some (Util.random () mod length) else None
      in
      index
      |> Option.andThen ~f:(fun i -> List.getAt ~index:i ids)
      |> Option.map ~f:(fun e -> [e])
      |> Option.withDefault ~default:[]


let fetch ~(ignoreTraces : bool) ~(ignore404s : bool) (m : model) :
    model * msg Tea.Cmd.t =
  if (not m.syncState.inFlight) || timedOut m.syncState
  then
    ( markRequestInModel m
    , Tea_cmd.call (fun _ ->
          Analysis.RequestTraces.send
            ( contextFromModel m
            , { tlids = toAnalyse m
              ; latest404 = m.latest404
              ; ignoreTraces
              ; ignore404s } ) ) )
  else (markTickInModel m, Tea.Cmd.none)


let fetchAll m : msg Tea.Cmd.t =
  Tea_cmd.call (fun _ ->
      Analysis.RequestTraces.send
        ( contextFromModel m
        , { tlids = []
          ; latest404 = m.latest404
          ; ignoreTraces = false
          ; ignore404s = false } ) )
