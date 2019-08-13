open Tc
open Types
open Prelude

(* Dark *)
module B = Blank
module P = Pointer
module TD = TLIDDict

let remove (m : model) (g : group) : model =
  { m with
    groups = TD.remove ~tlid:g.gTLID m.groups
  ; deletedGroups = TD.insert ~tlid:g.gTLID ~value:g m.deletedGroups }


(* Temporary to check if tlid is a deletedGroup *)
let isFromDeletedGroup (m : model) (tlid : tlid) : group option =
  let deletedGroupTLIDs = TD.get ~tlid m.deletedGroups in
  deletedGroupTLIDs


let fromList (groups : group list) : group TLIDDict.t =
  groups |> List.map ~f:(fun g -> (g.gTLID, g)) |> TLIDDict.fromList


let upsert (m : model) (g : group) : model =
  {m with groups = TD.insert ~tlid:g.gTLID ~value:g m.groups}


let generateGroupName (_ : unit) : string =
  "Group_" ^ (() |> Util.random |> string_of_int)


let createEmptyGroup (name : string) (pos : pos) : modification =
  let tlid = Prelude.gtlid () in
  let nameid = Prelude.gid () in
  let group = {gName = F (nameid, name); members = []; gTLID = tlid; pos} in
  Many [NewGroup group; Deselect]


let isNotInGroup (tlid : tlid) (groups : group TLIDDict.t) : bool =
  groups
  |> TLIDDict.mapValues ~f:(fun group -> group)
  |> List.filter ~f:(fun g -> List.member ~value:tlid g.members)
  |> List.length
  == 0


let landedInGroup (tlid : tlid) (groups : group TLIDDict.t) : tlid list =
  match
    Native.Ext.querySelector (".tl-" ^ deTLID tlid)
    |> Option.andThen ~f:(fun e -> Some (Native.Ext.getBoundingClientRect e))
  with
  | Some tlPos ->
      let tlLeft, tlRight, tlTop, tlBottom =
        let tlTop = Native.Ext.rectTop tlPos in
        let tlBottom = Native.Ext.rectBottom tlPos in
        let tlLeft = Native.Ext.rectLeft tlPos in
        let tlRight = Native.Ext.rectRight tlPos in
        (tlLeft, tlRight, tlTop, tlBottom)
      in
      groups
      |> TLIDDict.mapValues ~f:(fun group -> group)
      |> List.filter ~f:(fun (g : Types.group) ->
             match Native.Ext.querySelector (".tl-" ^ deTLID g.gTLID) with
             | Some elem ->
                 let groupPos = Native.Ext.getBoundingClientRect elem in
                 let gLeft, gRight, gTop, gBottom =
                   let tlTop = Native.Ext.rectTop groupPos in
                   let tlBottom = Native.Ext.rectBottom groupPos in
                   let tlLeft = Native.Ext.rectLeft groupPos in
                   let tlRight = Native.Ext.rectRight groupPos in
                   (tlLeft, tlRight, tlTop, tlBottom)
                 in
                 (* Check if the toplevel is inside the group *)
                 if tlRight < gLeft
                    || tlLeft > gRight
                    || tlBottom < gTop
                    || tlTop > gBottom
                 then false
                 else true
             | None ->
                 false )
      |> fromList
      |> TD.tlids
  | None ->
      []


let replaceGroupName (old : pointerData) (new_ : pointerData) (group : group) :
    group =
  let sId = P.toID old in
  if B.toID group.gName = sId
  then
    match new_ with
    | PGroupName new_ ->
        {group with gName = B.replace sId new_ group.gName}
    | _ ->
        group
  else group


let replace (old : pointerData) (new_ : pointerData) (group : group) : group =
  group |> replaceGroupName old new_


let allData (g : group) : pointerData list =
  let namePointer = PGroupName g.gName in
  [namePointer]
