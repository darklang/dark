open Tc
open Types
open Prelude

(* Tea *)
module Cmd = Tea.Cmd

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
  TD.get ~tlid m.deletedGroups


let fromList (groups : group list) : group TLIDDict.t =
  groups |> List.map ~f:(fun g -> (g.gTLID, g)) |> TLIDDict.fromList


let upsert (m : model) (g : group) : model =
  {m with groups = TD.insert ~tlid:g.gTLID ~value:g m.groups}


let addToGroup (m : model) (gTLID : tlid) (tlid : tlid) : model * msg Cmd.t =
  let group = TD.get ~tlid:gTLID m.groups in
  match group with
  | Some g ->
      let newGroup = {g with members = [tlid] @ g.members} in
      let newMod = upsert m newGroup in
      (newMod, Cmd.none)
  | None ->
      (m, Cmd.none)


let isGroupNameUnique (group : group) (groups : group TLIDDict.t) : bool =
  let allNames =
    groups
    |> TD.filterMapValues ~f:(fun g ->
           match g.gName with Blank _ -> None | F (_, name) -> Some name )
  in
  List.member
    ~value:(group.gName |> Blank.toMaybe |> Option.withDefault ~default:"")
    allNames


let generateGroupName (_ : unit) : string =
  "Group_" ^ (() |> Util.random |> string_of_int)


let createEmptyGroup (name : string option) (pos : pos) : modification =
  let name = match name with Some n -> n | None -> generateGroupName () in
  let tlid = Prelude.gtlid () in
  let nameid = Prelude.gid () in
  let group = {gName = F (nameid, name); members = []; gTLID = tlid; pos} in
  Many [NewGroup group; Deselect]


let isInGroup (tlid : tlid) (groups : group TLIDDict.t) : bool =
  groups
  |> TLIDDict.values
  |> List.any ~f:(fun g -> List.member ~value:tlid g.members)


let posInGroup (mePos : pos) (groups : group TLIDDict.t) : tlid list =
  groups
  |> TLIDDict.mapValues ~f:(fun group -> group)
  |> List.filter ~f:(fun (g : Types.group) ->
         match Native.Ext.querySelector (".tl-" ^ deTLID g.gTLID) with
         | Some elem ->
             let groupPos = Native.Ext.getBoundingClientRect elem in
             let clientWidth, clientHeight =
               let clientWidth =
                 Native.Ext.rectWidth groupPos |> int_of_float
               in
               let clientHeight =
                 Native.Ext.rectHeight groupPos |> int_of_float
               in
               (clientWidth, clientHeight)
             in
             (* Check if the toplevel pos is inside the group *)
             (* X *)
             let horStart = g.pos.x in
             let horEnd = horStart + clientWidth in
             (* Y *)
             let vertStart = g.pos.y in
             let vertEnd = vertStart + clientHeight in
             (* Check if mePos (x,y) is within the group box *)
             if mePos.x > horStart
                && mePos.x < horEnd
                && mePos.y > vertStart
                && mePos.y < vertEnd
             then true
             else false
         | None ->
             false )
  |> fromList
  |> TD.tlids


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
