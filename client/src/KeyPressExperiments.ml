open Types
open Selection
open Prelude
open Porting

(* Dark *)
module Key = Keyboard

(* only needed for moving up/down, not left/right *)
(* NB: assumes no id collisions in a given canvas *)
let new_offset (newId : id) : int =
let oldCaretPointDict = Native.Ext.findCaretPointWithinTextElement "fluidWidthSpan" in
  let oldCaretX, oldCaretY = Js.Dict.unsafeGet oldCaretPointDict "x", Js.Dict.unsafeGet oldCaretPointDict "y" in
    Native.Ext.findLogicalOffsetWithinTextElement (showID newId) oldCaretX oldCaretY


(* TODO this whole arrowMove* section could be DRY'd up, post-experiment *)
let arrowMoveUp (m : model) (tlid : tlid) (mId : id option) :
  modification =
  let default = body m tlid in
  let newMId = findTargetId tlid mId (moveUpDown Up) default |> deOption "mId" in
  enterWithOffset m tlid newMId (Some (new_offset newMId))

let arrowMoveDown (m : model) (tlid : tlid) (mId : id option) :
  modification =
  let default = TL.getTL m tlid |> TL.allData |> List.head |> Option.map P.toID
  in
  let newMId = findTargetId tlid mId (moveUpDown Down) default |> deOption "mId" in
  enterWithOffset m tlid newMId (Some (new_offset newMId))

let arrowMoveLeft (m : model) (tlid : tlid) (mId : id option) :
  modification =
  (* true means we handled it in js; false means we're moving between nodes *)
  match Native.Ext.moveCaretLeft "fluidWidthSpan" with
    true -> NoChange
  | false -> let default = body m tlid in
    let newMId = findTargetId tlid mId (moveLeftRight Right) default in
    enterWithOffset m tlid (deOption "mId" newMId) (Some (-1))

let arrowMoveRight (m : model) (tlid : tlid) (mId : id option) :
  modification =
  (* true means we handled it in js; false means we're moving between nodes *)
  match Native.Ext.moveCaretRight "fluidWidthSpan" with
    true -> NoChange
  | false -> let default = body m tlid in
    let newMId = findTargetId tlid mId (moveLeftRight Left) default in
    enterWithOffset m tlid (deOption "mId" newMId) (Some 0)

let arrowMoveHandler (event : Keyboard.keyEvent) (m : model) : modification option =
  match VariantTesting.variantIsActive m FluidInputModel with
    false -> None
  | true -> (match m.cursorState with
      | Selecting (tlid, Some mId) | Entering (Filling (tlid, mId)) -> (
          match event.keyCode with
          | Key.Up -> Some (arrowMoveUp m tlid (Some mId))
          | Key.Down -> Some (arrowMoveDown m tlid (Some mId))
          | Key.Right -> Some (arrowMoveRight m tlid (Some mId))
          | Key.Left -> Some (arrowMoveLeft m tlid (Some mId))
          | _ -> None)
      | _ -> None)
