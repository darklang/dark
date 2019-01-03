open Types
open Selection
open Prelude
open Porting

(* Dark *)
module Key = Keyboard

(* only needed for moving up/down, not left/right *)
(* NB: assumes no id collisions in a given canvas *)
let offsetFromCurrent (newId : id) : int =
  let oldCaretX = Native.Ext.findCaretXPos () in
  Native.Ext.findLogicalOffset (showID newId) oldCaretX


(* TODO this whole arrowMove* section could be DRY'd up, post-experiment *)
let arrowMoveUp (m : model) (tlid : tlid) (mId : id option) : modification =
  let default = body m tlid in
  let newMId =
    findTargetId tlid mId (moveUpDown Up) default |> deOption "mId"
  in
  enterWithOffset m tlid newMId (Some (offsetFromCurrent newMId))


let arrowMoveDown (m : model) (tlid : tlid) (mId : id option) : modification =
  let default =
    TL.getTL m tlid |> TL.allData |> List.head |> Option.map P.toID
  in
  let newMId =
    findTargetId tlid mId (moveUpDown Down) default |> deOption "mId"
  in
  enterWithOffset m tlid newMId (Some (offsetFromCurrent newMId))


let arrowMoveLeft (m : model) (tlid : tlid) (mId : id option) : modification =
  (* true means we handled it in js; false means we're moving between nodes *)
  match Native.Ext.moveCaretLeft () with
  | true ->
      NoChange
  | false ->
      let default = body m tlid in
      let newMId = findTargetId tlid mId (moveLeftRight Right) default in
      if newMId = mId
      then NoChange
      else enterWithOffset m tlid (deOption "mId" newMId) (Some (-1))


let arrowMoveRight (m : model) (tlid : tlid) (mId : id option) : modification =
  (* true means we handled it in js; false means we're moving between nodes *)
  match Native.Ext.moveCaretRight () with
  | true ->
      NoChange
  | false ->
      let default = body m tlid in
      let newMId = findTargetId tlid mId (moveLeftRight Left) default in
      if newMId = mId
      then NoChange
      else enterWithOffset m tlid (deOption "mId" newMId) (Some 0)


let arrowMoveHandler (event : Keyboard.keyEvent) (m : model) :
    modification option =
  match VariantTesting.variantIsActive m FluidInputModel with
  | false ->
      None
  | true ->
    ( match m.cursorState with
    | Entering (Filling (tlid, id)) ->
      ( match event.keyCode with
      | Key.Up ->
          if m.complete.visible
          then Some (AutocompleteMod ACSelectUp)
          else Some (arrowMoveUp m tlid (Some id))
      | Key.Down ->
          if m.complete.visible
          then Some (AutocompleteMod ACSelectDown)
          else Some (arrowMoveDown m tlid (Some id))
      | Key.Right ->
          if event.metaKey && event.shiftKey
          then Some (Select (tlid, Some id))
          else Some (arrowMoveRight m tlid (Some id))
      | Key.Left ->
          if event.metaKey && event.shiftKey
          then Some (Select (tlid, Some id))
          else Some (arrowMoveLeft m tlid (Some id))
      | Key.Escape ->
          Some NoChange
      | _ ->
          None )
    | Selecting (tlid, Some id) ->
      ( match event.keyCode with
      | Key.Escape ->
          Some (Enter (Filling (tlid, id)))
      | _ ->
          None )
    | _ ->
        None )
