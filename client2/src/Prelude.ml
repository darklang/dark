open! Porting
open Types

let deID (ID i : id) : int = i
let showID (ID i) = string_of_int i

let deTLID (TLID i : tlid) : int = i
let showTLID (TLID i) = string_of_int i

let gid (unit : unit) : id = ID (Util.random unit)

let gtlid (unit : unit) : tlid = TLID (Util.random unit)

let tlCursorID (tlid : tlid) (idx : int) : id =
  let stringID = string_of_int (deTLID tlid) ^ string_of_int idx in
  let intID = Result.withDefault 0 (String.toInt stringID) in
  ID intID

let unwrapCursorState (s : cursorState) : cursorState =
  match s with
  | Dragging (_, _, _, unwrap) -> unwrap
  | _ -> s

let tlidOf (s : cursorState) : tlid option =
  match unwrapCursorState s with
  | Selecting (tlid, _) -> Some tlid
  | Entering entryCursor -> (
    match entryCursor with
    | Creating _ -> None
    | Filling (tlid, _) -> Some tlid )
  | Deselected -> None
  | Dragging (_, _, _, _) -> None
  | SelectingCommand (tlid, _) -> Some tlid

let idOf (s : cursorState) : id option =
  match unwrapCursorState s with
  | Selecting (_, id) -> id
  | Entering entryCursor -> (
    match entryCursor with Creating _ -> None | Filling (_, id) -> Some id )
  | Deselected -> None
  | Dragging (_, _, _, _) -> None
  | SelectingCommand (_, id) -> Some id

let deOption (msg : string) (x : 'a option) : 'a =
  match x with
  | Some y -> y
  | None ->
      Debug.crash
        ( "something impossible occurred: got None but expected something"
        ^ msg )

let impossible (a : 'a) : 'b =
        (* Js.String.make is unreliable, but Debug.crash is sort of a last ditch
           handler anyway *)
  Debug.crash ("something impossible occurred: " ^ Js.String.make a)

let assert_ (fn : 'a -> bool) (a : 'a) : 'a =
  if fn a then a else impossible ("assertion failure", a)

let recoverable (msg : 'a) (val_ : 'b) : 'b =
  let error =
    "An unexpected but recoverable error happened. " ^ "For now we crash. "
    ^ "Message: " ^ Js.String.make msg ^ "Value: " ^ Js.String.make val_
  in
  let _ = "comment" in
  let _ = "comment" in
  let _ = Debug.crash error in
  val_

let todo (a : 'a) : 'b = Debug.crash ("TODO: " ^ Js.String.make a)
