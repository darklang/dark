open Tea
open! Porting
module JSD = Json.Decode
module JSE = Json.Encode
module TL = Toplevel
open Types

let fromString (json : string option) : serializableEditor =
  json
  |> Option.map (JSD.decodeString JSON.decodeSerializableEditor)
  |> Option.withDefault (Ok Defaults.defaultEditor)
  |> Result.withDefault Defaults.defaultEditor

let toString (se : serializableEditor) : string =
  JSON.encodeSerializableEditor se |> JSE.encode 0

let editor2model (e : serializableEditor) : model =
  let m = Defaults.defaultModel in
  { m with
    timersEnabled= e.timersEnabled
  ; clipboard= e.clipboard
  ; cursorState= e.cursorState
  ; lockedHandlers= e.lockedHandlers }

let model2editor (m : model) : serializableEditor =
  { clipboard= m.clipboard
  ; timersEnabled= m.timersEnabled
  ; cursorState= m.cursorState
  ; lockedHandlers= m.lockedHandlers }

let updateLockedHandlers (tlid : tlid) (lockHandler : bool) (m : model) :
    modification =
  let tl = TL.getTL m tlid in
  match tl.data with
  | TLHandler _ ->
      let lockedList =
        if lockHandler then tlid :: m.lockedHandlers
        else List.filter (fun t -> t <> tlid) m.lockedHandlers
      in
      SetLockedHandlers lockedList
  | _ -> NoChange
