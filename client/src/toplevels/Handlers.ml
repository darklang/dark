open Prelude

(* Dark *)
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let fromList (handlers : handler list) : handler TLIDDict.t =
  handlers |> List.map ~f:(fun h -> (h.hTLID, h)) |> TLIDDict.fromList


let upsert (m : model) (h : handler) : model =
  {m with handlers = Map.add ~key:h.hTLID ~value:h m.handlers}


let update (m : model) ~(tlid : TLID.t) ~(f : handler -> handler) : model =
  {m with handlers = Map.updateIfPresent ~key:tlid ~f m.handlers}


let remove (m : model) (h : handler) : model =
  {m with handlers = Map.remove ~key:h.hTLID m.handlers}


let getWorkerSchedule (m : model) (h : handler) : string option =
  match h.spec.name with
  | F (_, name) ->
      Map.get ~key:name m.workerSchedules
  | Blank _ ->
      None
