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
let reportError (msg : string) (msgVal : 'm) : unit =
  Js.log3 "An unexpected but recoverable error happened: " msg msgVal ;
  Js.Console.trace () ;
  Native.Rollbar.send
    msg
    None
    (* It seems ridiculous to convert to JSON strings, and then parse, to
     * get the right type to send through, but I can't figure out a
     * different way to do it. *)
    ( msgVal
    |> Js.Json.stringifyAny
    |> Tc.Option.map ~f:Js.Json.parseExn
    |> Tc.Option.withDefault ~default:Js.Json.null ) ;
  ()


(* We never want to crash the app. Instead, send a rollbar notification of the invalid state and try to continue. *)
let recover ?(debug : 'd option) (msg : string) (recoveryVal : 'r) : 'r =
  reportError ("Recover: " ^ msg) debug ;
  recoveryVal


let recoverOpt ?(debug : 'd option) (msg : 'msg) ~(default : 'r) (x : 'r option)
    : 'r =
  match x with
  | Some y ->
      y
  | None ->
      recover ~debug ("Got None but expected something: " ^ msg) default


(* Assert `cond`, returning val either way.  All assertion functions report
 * to rollbar if they fail. *)
let assert_ ?(debug : 'd option) (msg : string) (cond : bool) (returnVal : 'r) :
    'r =
  if cond
  then returnVal
  else recover ("Assertion failure: " ^ msg) ~debug returnVal


(* Assert `cond` as a statement.  All assertion functions report to rollbar
 * if they fail. *)
let asserT ?(debug : 'd option) (msg : 'msg) (cond : bool) : unit =
  assert_ ~debug msg cond ()


(* Assert that `f a` returns true, passing the value back as a result. All
 * assertion functions report to rollbar if they fail. *)
let assertFn
    ?(debug : 'd option) (msg : string) ~(f : 'r -> bool) (returnVal : 'r) : 'r
    =
  assert_ ~debug msg (f returnVal) returnVal


(* Assert that `f a` returns true, as a statement. All assertion functions
 * report to rollbar if they fail. *)
let asserTFn ?(debug : 'd option) (msg : string) ~(f : 'a -> bool) : unit =
  assertFn ~f ~debug msg ()


(* Like recover but with the message TODO *)
let todo (msg : string) (recoveryVal : 'b) : 'b =
  recover ~debug:recoveryVal ("TODO: " ^ msg) recoveryVal
