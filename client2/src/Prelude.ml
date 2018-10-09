open Tea
open! Porting
open Types

let deID (ID i) = i

let deTLID (TLID i) = i

let gid unit = ID (Util.random unit)

let gtlid unit = TLID (Util.random unit)

let tlCursorID tlid idx =
  let stringID = string_of_int (deTLID tlid) ^ string_of_int idx in
  let intID = Result.withDefault 0 (String.toInt stringID) in
  ID intID

let unwrapCursorState s =
  match s with Dragging (_, _, _, unwrap) -> unwrap | _ -> s

let tlidOf s =
  match unwrapCursorState s with
  | Selecting (tlid, _) -> Some tlid
  | Entering entryCursor -> (
    match entryCursor with
    | Creating _ -> None
    | Filling (tlid, _) -> Some tlid )
  | Deselected -> None
  | Dragging (_, _, _, _) -> None
  | SelectingCommand (tlid, _) -> Some tlid

let idOf s =
  match unwrapCursorState s with
  | Selecting (_, id) -> id
  | Entering entryCursor -> (
    match entryCursor with Creating _ -> None | Filling (_, id) -> Some id )
  | Deselected -> None
  | Dragging (_, _, _, _) -> None
  | SelectingCommand (_, id) -> Some id

let assert_ fn a = if fn a then a else impossible ("assertion failure", a)

let impossible a = Debug.crash ("something impossible occurred: " ^ toString a)

let recoverable msg val_ =
  let error =
    ( ( ( ( "An unexpected but recoverable error happened. "
          ^ "For now we crash. " )
        ^ "Message: " )
      ^ toString msg )
    ^ "Value: " )
    ^ toString val_
  in
  let _ = "comment" in
  let _ = "comment" in
  let _ = Debug.crash error in
  val_

let todo a = Debug.crash ("TODO: " ^ toString a)
