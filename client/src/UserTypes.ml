open Tc
open Types

let allData (_t : userTipe) : pointerData list = []

let siblings (_p : pointerData) (_t : userTipe) : pointerData list = []

let find (m : model) (id : tlid) : userTipe option =
  List.find ~f:(fun t -> id = t.utTLID) m.userTipes


let upsert (m : model) (t : userTipe) : model =
  match find m t.utTLID with
  | Some old ->
      { m with
        userTipes =
          m.userTipes
          |> List.filter ~f:(fun ut -> ut.utTLID <> old.utTLID)
          |> List.cons t }
  | None ->
      {m with userTipes = t :: m.userTipes}


let containsByTLID (tipes : userTipe list) (elem : userTipe) : bool =
  List.find ~f:(fun t -> t.utTLID = elem.utTLID) tipes <> None


let removeByTLID ~(toBeRemoved : userTipe list) (origTipes : userTipe list) :
    userTipe list =
  List.filter ~f:(fun orig -> not (containsByTLID toBeRemoved orig)) origTipes


let upsertByTLID (tipes : userTipe list) (t : userTipe) : userTipe list =
  removeByTLID tipes ~toBeRemoved:[t] @ [t]


let upsertAllByTLID (tipes : userTipe list) ~(newTipes : userTipe list) :
    userTipe list =
  List.foldl ~f:(fun t acc -> upsertByTLID acc t) ~init:tipes newTipes
