open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let fromList (handlers : handler list) : handler TLIDDict.t =
  handlers |> List.map ~f:(fun h -> (h.hTLID, h)) |> TLIDDict.fromList


let upsert (m : model) (h : handler) : model =
  {m with handlers = TD.insert ~tlid:h.hTLID ~value:h m.handlers}


let update (m : model) ~(tlid : TLID.t) ~(f : handler -> handler) : model =
  {m with handlers = TD.updateIfPresent ~tlid ~f m.handlers}


let remove (m : model) (h : handler) : model =
  {m with handlers = TD.remove ~tlid:h.hTLID m.handlers}


let getWorkerSchedule (m : model) (h : handler) : string option =
  match h.spec.name with
  | F (_, name) ->
      StrDict.get ~key:name m.workerSchedules
  | Blank _ ->
      None


let setHandlerLock (tlid : TLID.t) (lock : bool) (m : model) : model =
  let updateProps prop =
    match prop with
    | Some p ->
        Some {p with handlerLock = lock}
    | None ->
        Some {Defaults.defaultHandlerProp with handlerLock = lock}
  in
  let props = m.handlerProps |> TLIDDict.update ~tlid ~f:updateProps in
  {m with handlerProps = props}


let setHandlerState (tlid : TLID.t) (state : handlerState) (m : model) : model =
  let updateProps prop =
    match prop with
    | Some p ->
        Some {p with handlerState = state}
    | None ->
        Some {Defaults.defaultHandlerProp with handlerState = state}
  in
  let props = m.handlerProps |> TLIDDict.update ~tlid ~f:updateProps in
  {m with handlerProps = props}
