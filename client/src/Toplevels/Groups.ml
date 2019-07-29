open Types

(* open Prelude *)

(* Dark *)
module B = Blank
module P = Pointer
module TD = TLIDDict

let remove m _group = m


let replaceGroupName (old : pointerData) (new_ : pointerData) (group : group)
    : group =
  let sId = P.toID old in
  if B.toID group.name = sId
  then
    match new_ with
    | PGroupName new_ ->
        {group with name = B.replace sId new_ group.name}
    | _ ->
        group
  else group

let replace (old : pointerData) (new_ : pointerData) (group : group) :
group =
  group |> replaceGroupName old new_ 

let allData  (g : group) : pointerData list =
  let namePointer = PGroupName g.name in
  [namePointer]
