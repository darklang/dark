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
  | FluidEntering tlid | FluidMouseSelecting tlid ->
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
  | FluidEntering _ | FluidMouseSelecting _ ->
      None


(* -------------------------------------- *)
(* Crashing *)
(* -------------------------------------- *)
let reportError (msg : 'msg) (msgVal : 'b) : unit =
  let error =
    "An unexpected but recoverable error happened. "
    ^ "Message: "
    ^ Js.String.make msg
    ^ ", Value: "
    ^ Js.String.make msgVal
  in
  Js.log3 "An unexpected but recoverable error happened: " msg msgVal ;
  Js.Console.trace () ;
  Native.Rollbar.send error None Js.Json.null ;
  ()


(* We never want to crash the app. Instead, send a rollbar notification of the invalid state and try to continue. *)
let recover (msg : 'msg) (msgVal : 'b) (recoveryVal : 'c) : 'c =
  reportError msg msgVal ;
  recoveryVal


let recoverOpt (msg : 'msg) ~(default : 'a) (x : 'a option) : 'a =
  match x with
  | Some y ->
      y
  | None ->
      recover ("Got None but expected something: " ^ msg) x default


(* Assert that `f a` returns true, passing the value back as a result. All
 * assertion functions report to rollbar if they fail. *)
let assertFn (msg : 'msg) ~(f : 'a -> bool) (a : 'a) : 'a =
  if f a then a else recover ("assertion failure", msg) a a


(* Assert that `f a` returns true, as a statement. All assertion functions
 * report to rollbar if they fail. *)
let asserTFn (msg : 'msg) ~(f : 'a -> bool) (val_ : 'a) : unit =
  ignore (assertFn ~f msg val_)


(* Assert `cond`, returning val either way.  All assertion functions report
 * to rollbar if they fail.  *)
let assert_ (msg : 'msg) (cond : bool) (val_ : 'a) : 'a =
  if cond then val_ else recover ("assertion failure", msg) val_ val_


(* Assert `cond` as a statement.  All assertion functions report to rollbar
 * if they fail.  *)
let asserT (msg : 'msg) (cond : bool) (val_ : 'a) : unit =
  ignore (assert_ msg cond val_)


(* Like recover but with the message TODO *)
let todo (msg : 'a) (recoveryVal : 'b) : 'b = recover "TODO" msg recoveryVal
