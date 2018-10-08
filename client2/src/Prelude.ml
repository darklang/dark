open Types

let tlCursorID tlid idx =
  let stringID = string_of_int (deTLID tlid) ++ string_of_int idx in
  let intID = Result.withDefault 0 (String.toInt stringID) in
  ID intID

let unwrapCursorState s =
  match s with Dragging (_, _, _, unwrap) -> unwrap | _ -> s

let tlidOf s =
  match unwrapCursorState s with
  | Selecting (tlid, _) -> Just tlid
  | Entering entryCursor -> (
    match entryCursor with
    | Creating _ -> Nothing
    | Filling (tlid, _) -> Just tlid )
  | Deselected -> Nothing
  | Dragging (_, _, _, _) -> Nothing
  | SelectingCommand (tlid, _) -> Just tlid

let idOf s =
  match unwrapCursorState s with
  | Selecting (_, id) -> id
  | Entering entryCursor -> (
    match entryCursor with Creating _ -> Nothing | Filling (_, id) -> Just id )
  | Deselected -> Nothing
  | Dragging (_, _, _, _) -> Nothing
  | SelectingCommand (_, id) -> Just id

let deID (ID i) = i

let deTLID (TLID i) = i

let gid unit = ID (Util.random unit)

let gtlid unit = TLID (Util.random unit)

let assert_ fn a = if fn a then a else impossible ("assertion failure", a)

let impossible a = Debug.crash ("something impossible occurred: " ++ toString a)

let recoverable msg val_ =
  let error =
    "An unexpected but recoverable error happened. " ++ "For now we crash. "
    ++ "Message: " ++ toString msg ++ "Value: " ++ toString val_
  in
  let _ = "comment" in
  let _ = "comment" in
  let _ = Debug.crash error in
  val_

let todo a = Debug.crash ("TODO: " ++ toString a)
