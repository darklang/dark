open Tc

(* open Prelude *)
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TD = TLIDDict

let fromList (handlers : handler list) : handler TLIDDict.t =
  handlers |> List.map ~f:(fun h -> (h.hTLID, h)) |> TLIDDict.fromList


let upsert (m : model) (h : handler) : model =
  {m with handlers = TD.insert ~tlid:h.hTLID ~value:h m.handlers}


let update (m : model) ~(tlid : tlid) ~(f : handler -> handler) : model =
  {m with handlers = TD.update ~tlid ~f m.handlers}


let remove (m : model) (h : handler) : model =
  {m with handlers = TD.remove ~tlid:h.hTLID m.handlers}
