open Core_kernel
open Lwt

(* ----------------- *)
(* levels *)
(* ----------------- *)

type level =
  [ `Off
  | `Inspect
  | `Fatal
  | `Error
  | `Warn
  | `Info
  | `Success
  | `Debug
  | `All ]

let logkey : (string * Yojson.Safe.t) list Lwt.key ref =
  (* This key doesn't need to be unique across threads - it's thread-local. The
   * key there so you could have, say, a logkey and an other-variable-key *)
  ref (Lwt.new_key ())


let loglevel : level ref = ref `All

let set_level (newlevel : level) : unit = loglevel := newlevel

let level_to_string (level : level) : string =
  match level with
  | `Off ->
      "OFF"
  | `Inspect ->
      "INSPECT"
  | `Fatal ->
      "FATAL"
  | `Error ->
      "ERROR"
  | `Warn ->
      "WARN"
  | `Info ->
      "INFO"
  | `Debug ->
      "DEBUG"
  | `Success ->
      "SUCCESS"
  | `All ->
      "ALL"


let string_to_level_opt (str : string) : level option =
  match str |> String.strip |> String.uppercase with
  | "OFF" ->
      Some `Off
  | "INSPECT" ->
      Some `Inspect
  | "FATAL" ->
      Some `Fatal
  | "ERROR" ->
      Some `Error
  | "WARN" ->
      Some `Warn
  | "INFO" ->
      Some `Info
  | "DEBUG" ->
      Some `Debug
  | "SUCCESS" ->
      Some `Success
  | "ALL" ->
      Some `All
  | _ ->
      None


let level_to_color (level : level) : string =
  (* https://misc.flogisoft.com/bash/tip_colors_and_formatting *)
  match level with
  | `Off ->
      ""
  | `Inspect ->
      "\027[38;5;196m"
  | `Fatal ->
      "\027[38;5;196m"
  | `Error ->
      "\027[38;5;165m"
  | `Warn ->
      "\027[38;5;108m"
  | `Info ->
      "\027[38;5;38m"
  | `Success ->
      "\027[38;5;221m"
  | `Debug ->
      "\027[38;5;221m"
  | `All ->
      ""


let should_log (user_level : level) : bool =
  match user_level with
  | `Off ->
      false
  | `Inspect ->
    (match !loglevel with `Off -> false | _ -> true)
  | `Fatal ->
    (match !loglevel with `Off -> false | `Inspect -> false | _ -> true)
  | `Error ->
    ( match !loglevel with
    | `Off ->
        false
    | `Inspect ->
        false
    | `Fatal ->
        false
    | _ ->
        true )
  | `Warn ->
    ( match !loglevel with
    | `Off ->
        false
    | `Inspect ->
        false
    | `Fatal ->
        false
    | `Error ->
        false
    | _ ->
        true )
  | `Info ->
    ( match !loglevel with
    | `Off ->
        false
    | `Inspect ->
        false
    | `Fatal ->
        false
    | `Error ->
        false
    | `Warn ->
        false
    | _ ->
        true )
  | `Debug ->
    ( match !loglevel with
    | `Off ->
        false
    | `Inspect ->
        false
    | `Fatal ->
        false
    | `Error ->
        false
    | `Warn ->
        false
    | `Info ->
        false
    | _ ->
        true )
  | `Success ->
    ( match !loglevel with
    | `Off ->
        false
    | `Inspect ->
        false
    | `Fatal ->
        false
    | `Error ->
        false
    | `Warn ->
        false
    | `Info ->
        false
    | `Debug ->
        false
    | _ ->
        true )
  | `All ->
      true


(* ----------------- *)
(* format *)
(* ----------------- *)
type format =
  [ `Json
  | `DecoratedJson ]

let format : format ref = ref `Json

let set_format (newformat : format) = format := newformat

let dump v : string =
  if Obj.tag (Obj.repr v) = Obj.string_tag
  then "'" ^ Obj.magic v ^ "'"
  else Vendor.dump v


(* This exists because ... our decorating, in print_json_log, is to use terminal
 * coloring on the keys. Yojson.Safe.tojson then escapes those strings to make
 * the whole thing proper json. This function does a global search and replace
 * for the escaped strings, making terminal output colorful.
 *
 * e.g., \\u001b[0m -> \x1b[0m resets coloring
 * *)
let fix_json_colors ~(decorate : bool) ~(level : level) (input : string) :
    string =
  if not decorate
  then input
  else
    String.Search_pattern.replace_all
      (String.Search_pattern.create "\\u001b")
      ~in_:input
      ~with_:"\x1b"


let rfc3339_of_float (time : float) : string =
  let span = Time.Span.of_sec time in
  (* rfc3339 is ~iso8601, and we're using the variant with nanoseconds *)
  (* to_string_iso8601_basic gets us ms, so this fakes ns *)
  String.Search_pattern.replace_first
    (String.Search_pattern.create "Z")
    ~in_:
      (Time.to_string_iso8601_basic
         (Time.of_span_since_epoch span)
         ~zone:Time.Zone.utc)
    ~with_:"000Z"


let print_json_log
    ~(level : level)
    ~(decorate : bool)
    ?(bt : Caml.Printexc.raw_backtrace option = None)
    (params : (string * Yojson.Safe.t) list) : unit =
  let timestamp = rfc3339_of_float (Unix.gettimeofday ()) in
  let bt_params =
    match bt with
    | None ->
        []
    | Some some_bt ->
        [("backtrace", `String (Caml.Printexc.raw_backtrace_to_string some_bt))]
  in
  let color = if decorate then level_to_color level else "" in
  let reset = if decorate then "\x1b[0m" else "" in
  let params =
    [ ("timestamp", `String timestamp)
    ; ("level", `String (level_to_string level)) ]
    @ params
    @ bt_params
    |> List.map ~f:(fun (k, v) -> (color ^ k ^ reset, v))
  in
  Yojson.Safe.to_string (`Assoc params)
  |> fix_json_colors ~decorate ~level
  |> Caml.print_endline


let pP
    ?(data : string option)
    ?(jsonparams : (string * Yojson.Safe.t) list = [])
    ?(params : (string * string) list = [])
    ?(bt : Caml.Printexc.raw_backtrace option)
    ~(level : level)
    (name : string) : unit =
  try
    if should_log level
    then
      let data_param =
        match data with None -> [] | Some data -> [("data", `String data)]
      in
      let thread_params =
        match Lwt.get !logkey with
        | Some annotations ->
            annotations
        | None ->
            []
      in
      let params =
        thread_params
        @ [ ("name", `String name)
          (* operation time *)
          (* timestamp *)
          (* slow request *)
          (* ip address *)
           ]
        @ data_param
        @ jsonparams
        @ List.map params (fun (k, v) -> (k, `String v))
      in
      match !format with
      | `DecoratedJson ->
          print_json_log ~bt ~decorate:true ~level params
      | `Json ->
          print_json_log ~bt ~decorate:false ~level params
  with e -> Caml.print_endline ("UNHANDLED ERROR: log.pP: " ^ Exn.to_string e)


let inspecT ?(f = dump) (name : string) (x : 'a) : unit =
  pP ~level:`Inspect name ~params:[("data", f x)]


let inspect ?(f = dump) (name : string) (x : 'a) : 'a =
  inspecT ~f name x ;
  x


let debuG = pP ~level:`Debug

let infO = pP ~level:`Info

let warN = pP ~level:`Warn

let erroR = pP ~level:`Error

let fataL = pP ~level:`Fatal

let succesS = pP ~level:`Success

(* Add to the current set of thread-local log annotations. *)
(* We make no attempt whatsoever to deal with dupe keys, except to put new ones
 * later in the ordering *)
let add_log_annotations (annotations : (string * Yojson.Safe.t) list) :
    (unit -> 'a) -> 'a =
  let existing =
    match Lwt.get !logkey with None -> [] | Some annotations -> annotations
  in
  Lwt.with_value !logkey (Some (existing @ annotations))


(* ----------------- *)
(* init *)
(* ----------------- *)

let init ~(level : level) ~(format : format) () =
  set_level level ;
  set_format format ;
  ()
