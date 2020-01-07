include Tc
include Types

(* Every other module should have `open Prelude` as its first statement.
 * You don't need to open/include Tc or Types, Prelude includes them. *)

module Tea = struct
  (* Extend Tea functions *)
  module Result = Tea_result
  module Cmd = Tea_cmd
  module Sub = Tea_sub
  module App = Tea_app
  module Debug = Tea_debug
  module Html = Tea_html_extended
  module Html2 = Tea_html2
  module Svg = Tea_svg
  module Task = Tea_task
  module Program = Tea_program
  module Time = Tea_time_extended
  module Json = Tea_json
  module Navigation = Tea_navigation
  module Random = Tea_random
  module AnimationFrame = Tea_animationframe
  module Mouse = Tea_mouse
  module Http = Tea_http
  module Ex = Tea_ex
end

module Html = Tea.Html

module Json = struct
  exception ParseError = Json.ParseError

  let parseOrRaise = Json.parseOrRaise

  let parse = Json.parse

  let stringify = Json.stringify

  module Decode = Json_decode_extended
  module Encode = Json_encode_extended
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

(* -------------------------------------- *)
(* CursorState *)
(* -------------------------------------- *)

let rec unwrapCursorState (s : cursorState) : cursorState =
  match s with Dragging (_, _, _, nested) -> unwrapCursorState nested | _ -> s


let tlidOf (s : cursorState) : tlid option =
  match unwrapCursorState s with
  | Selecting (tlid, _) | Entering (tlid, _) | FluidEntering tlid ->
      Some tlid
  | Omnibox _ | Deselected | Dragging _ ->
      None


let idOf (s : cursorState) : id option =
  match unwrapCursorState s with
  | Selecting (_, id) ->
      id
  | Entering (_, id) ->
      Some id
  | Deselected | Dragging _ | Omnibox _ | FluidEntering _ ->
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


module Debug = struct
  let log ?(f : 'a -> 'b = fun x -> Obj.magic x) (msg : string) (data : 'a) : 'a
      =
    Js.log2 msg (f data) ;
    data


  let loG ?(f : 'a -> 'b = fun x -> Obj.magic x) (msg : string) (data : 'a) :
      unit =
    Js.log2 msg (f data)
end

(* Needs a better home *)

let rec tipe2str (t : tipe) : string =
  match t with
  | TAny ->
      "Any"
  | TInt ->
      "Int"
  | TFloat ->
      "Float"
  | TBool ->
      "Bool"
  | TNull ->
      "Null"
  | TCharacter ->
      "Character"
  | TStr ->
      "String"
  | TList ->
      "List"
  | TObj ->
      "Dict"
  | TBlock ->
      "Block"
  | TIncomplete ->
      "Incomplete"
  | TError ->
      "Error"
  | TResp ->
      "Response"
  | TDB ->
      "Datastore"
  | TDate ->
      "Date"
  | TOption ->
      "Option"
  | TPassword ->
      "Password"
  | TUuid ->
      "UUID"
  | TErrorRail ->
      "ErrorRail"
  | TResult ->
      "Result"
  | TDbList a ->
      "[" ^ tipe2str a ^ "]"
  | TUserType (name, _) ->
      name
  | TBytes ->
      "Bytes"
