open Types

(* Dark *)
module Key = Keyboard

let arrowMoveHandler (event : Keyboard.keyEvent) (m : model) : modification option =
  match VariantTesting.variantIsActive m ArrowMove with
    false -> None
  | true -> (match m.cursorState with
      | Selecting (tlid, Some mId) | Entering (Filling (tlid, mId)) -> (
          match event.keyCode with
          | Key.Up -> Some (Selection.moveUp ~andEnter:true m tlid (Some mId))
          | Key.Down -> Some (Selection.moveDown ~andEnter:true m tlid (Some mId))
          | Key.Right -> Some (Selection.moveRight ~andEnter:true m tlid (Some mId))
          | Key.Left -> Some (Selection.moveLeft ~andEnter:true m tlid (Some mId))
          | _ -> None)
      | _ -> None)
