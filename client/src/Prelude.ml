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
  TLID (Js.Date.now unit *. 1000.0 |> int_of_float |> string_of_int)


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

(* `Impossible` crashes with the value provided. *)
(* Is it very obvious why? *)
(*   impossible () *)
(* Want a string message? *)
(*   impossible "The widgets can't arrive in this order" *)
(* Show a value: *)
(*   impossible myVar *)
(* Combine them: *)
(*   impossible ("The widges can't arrive in this order:", myVar) *)
let impossible (a : 'a) : 'b =
  Debug.crash ("something impossible occurred: " ^ Js.String.make a)


let deOption (msg : string) (x : 'a option) : 'a =
  match x with
  | Some y ->
      y
  | None ->
      impossible ("got None but expected something: " ^ msg)


let assert_ (fn : 'a -> bool) (a : 'a) : 'a =
  if fn a then a else impossible ("assertion failure", a)


(* Like impossible but has a different semantic meaning. If you have a *)
(* value you _could_ continue with, consider this. *)
let recoverable (msg : 'a) (val_ : 'b) : 'b =
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


(* Like impossible but with the message TODO *)
let todo (a : 'a) : 'b = Debug.crash ("TODO: " ^ Js.String.make a)
