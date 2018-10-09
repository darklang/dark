open Belt
open Tea
open Porting
module JSD = Json.Decode
module JSE = Json.Encode
module TL = Toplevel
open Types

let fromString json =
  json
  |> Option.map (JSD.decodeString RPC.decodeSerializableEditor)
  |> Maybe.withDefault (Ok Defaults.defaultEditor)
  |> Result.getWithDefault Defaults.defaultEditor

let toString se = RPC.encodeSerializableEditor se |> JSE.encode 0

let editor2model e =
  let m = Defaults.defaultModel in
  { m with
    timersEnabled= e.timersEnabled
  ; clipboard= e.clipboard
  ; cursorState= e.cursorState
  ; lockedHandlers= e.lockedHandlers }

let model2editor m =
  { clipboard= m.clipboard
  ; timersEnabled= m.timersEnabled
  ; cursorState= m.cursorState
  ; lockedHandlers= m.lockedHandlers }

let updateLockedHandlers tlid lockHandler m =
  let tl = TL.getTL m tlid in
  match tl.data with
  | TLHandler _ ->
      let lockedList =
        if lockHandler then tlid :: m.lockedHandlers
        else List.filter (fun t -> t <> tlid) m.lockedHandlers
      in
      SetLockedHandlers lockedList
  | _ -> NoChange
