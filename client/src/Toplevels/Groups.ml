open Types

(* open Prelude *)

(* Dark *)
module B = Blank
module P = Pointer
module TD = TLIDDict

let remove (m : model) (g : group) : model =
  {m with groups = TD.remove ~tlid:g.gTLID m.groups}


let fromList (groups : group list) : group TLIDDict.t =
  groups |> List.map (fun g -> (g.gTLID, g)) |> TLIDDict.fromList


let upsert (m : model) (g : group) : model =
  {m with groups = TD.insert ~tlid:g.gTLID ~value:g m.groups}


let landedInGroup (mePos : pos) (groups : group TLIDDict.t) : tlid list =
  groups
  |> TLIDDict.mapValues ~f:(fun group -> group)
  |> List.filter (fun (g : Types.group) ->
         (* X *)
         let horStart = g.pos.x in
         (* 360X250 = the static size of a group *)
         (* SYD TODO - add group box size to model then get it here *)
         let horEnd = horStart + 250 in
         (* Y *)
         let vertStart = g.pos.y in
         let vertEnd = vertStart + 360 in
         (* Check if mePos (x,y) is within the group box *)
         if mePos.x > horStart
            && mePos.x < horEnd
            && mePos.y > vertStart
            && mePos.y < vertEnd
         then true
         else false )
  |> fromList
  |> TD.tlids


let replaceGroupName (old : pointerData) (new_ : pointerData) (group : group) :
    group =
  let sId = P.toID old in
  if B.toID group.name = sId
  then
    match new_ with
    | PGroupName new_ ->
        {group with name = B.replace sId new_ group.name}
    | _ ->
        group
  else group


let replace (old : pointerData) (new_ : pointerData) (group : group) : group =
  group |> replaceGroupName old new_


let allData (g : group) : pointerData list =
  let namePointer = PGroupName g.name in
  [namePointer]
