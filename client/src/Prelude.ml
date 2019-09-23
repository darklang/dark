open Types

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

(* -------------------------------------- *)
(* IDs *)
(* -------------------------------------- *)
let deID (ID i : id) : string = i

let showID (ID i) = i

let deTLID (TLID i : tlid) : string = i

let showTLID (TLID i) = i

let gid (unit : unit) : id = ID (Util.random unit |> string_of_int)

let gtlid (unit : unit) : tlid = TLID (Util.random unit |> string_of_int)

let gtlidDT (unit : unit) : tlid =
  let id =
    Js.Date.now unit
    |> Js.Float.toString
    |> Tc.String.split ~on:"."
    |> Tc.List.head
    |> Tc.Option.withDefault ~default:(Util.random unit |> string_of_int)
  in
  TLID id


(* -------------------------------------- *)
(* CursorState *)
(* -------------------------------------- *)

let unwrapCursorState (s : cursorState) : cursorState =
  match s with Dragging (_, _, _, unwrap) -> unwrap | _ -> s


let tlidOf (s : cursorState) : tlid option =
  match unwrapCursorState s with
  | Selecting (tlid, _) ->
      Some tlid
  | Entering entryCursor ->
    ( match entryCursor with
    | Creating _ ->
        None
    | Filling (tlid, _) ->
        Some tlid )
  | Deselected ->
      None
  | Dragging (_, _, _, _) ->
      None
  | SelectingCommand (tlid, _) ->
      Some tlid
  | FluidEntering tlid ->
      Some tlid


let idOf (s : cursorState) : id option =
  match unwrapCursorState s with
  | Selecting (_, id) ->
      id
  | Entering entryCursor ->
    (match entryCursor with Creating _ -> None | Filling (_, id) -> Some id)
  | Deselected ->
      None
  | Dragging (_, _, _, _) ->
      None
  | SelectingCommand (_, id) ->
      Some id
  | FluidEntering _ ->
      None


(* -------------------------------------- *)
(* Crashing *)
(* -------------------------------------- *)

let deOption (msg : string) (x : 'a option) : 'a =
  match x with
  | Some y ->
      y
  | None ->
      Debug.crash ("deOption, got None but expected something: " ^ msg)


(* We never want to crash the app. Instead, send a rollbar notification of the invalid state and try to continue. *)
let recover (msg : 'a) (val_ : 'b) : 'b =
  let error =
    "An unexpected but recoverable error happened. "
    ^ "For now we crash. "
    ^ "Message: "
    ^ Js.String.make msg
    ^ "Value: "
    ^ Js.String.make val_
  in
  (* TODO: surface the error to the user and in rollbar and continue *)
  ignore (Debug.crash error) ;
  val_


let assert_ (fn : 'a -> bool) (a : 'a) : 'a =
  if fn a then a else recover "assertion failure" a


let asserT (fn : 'a -> bool) (a : 'a) : unit = ignore (assert_ fn a)

(* Like impossible but with the message TODO *)
let todo (a : 'a) : 'b = Debug.crash ("TODO: " ^ Js.String.make a)
