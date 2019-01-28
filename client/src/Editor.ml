open Tc
open Types

let fromString (json : string option) : serializableEditor =
  match json with
  | None ->
      Debug.loG "no serialized editor" None ;
      Defaults.defaultEditor
  | Some json ->
    ( try json |> Json.parseOrRaise |> Decoders.serializableEditor with e ->
        Debug.loG "error parsing serialized editor" e ;
        Defaults.defaultEditor )


let rec stripDragging (cs : cursorState) : cursorState =
  match cs with Dragging (_, _, _, state) -> stripDragging state | _ -> cs


let toString (se : serializableEditor) : string =
  se |> Encoders.serializableEditor |> Json.stringify


let editor2model (e : serializableEditor) : model =
  let m = Defaults.defaultModel in
  { m with
    timersEnabled = e.timersEnabled
  ; clipboard = e.clipboard
  ; cursorState = e.cursorState |> stripDragging
  ; lockedHandlers = e.lockedHandlers
  ; routingTableOpenDetails = e.routingTableOpenDetails }


let model2editor (m : model) : serializableEditor =
  { clipboard = m.clipboard
  ; timersEnabled = m.timersEnabled
  ; cursorState = m.cursorState
  ; lockedHandlers = m.lockedHandlers
  ; routingTableOpenDetails = m.routingTableOpenDetails }


let updateLockedHandlers (tlid : tlid) (lockHandler : bool) (m : model) :
    modification =
  let tl = Toplevel.getTL m tlid in
  match tl.data with
  | TLHandler _ ->
      let lockedList =
        if lockHandler
        then tlid :: m.lockedHandlers
        else List.filter ~f:(fun t -> t <> tlid) m.lockedHandlers
      in
      SetLockedHandlers lockedList
  | _ ->
      NoChange
