open Core_kernel

(* ----------------- *)
(* levels *)
(* ----------------- *)

type level = [`Off | `Fatal | `Error | `Warn | `Info | `Debug | `All ]

let loglevel : level ref =
  ref `All

let set_level (newlevel: level) : unit =
  loglevel := newlevel

let level_to_string (level: level) : string =
  match level with
  | `Off -> "OFF"
  | `Fatal -> "FATAL"
  | `Error -> "ERROR"
  | `Warn -> "WARN"
  | `Info -> "INFO"
  | `Debug -> "DEBUG"
  | `All -> "ALL"

let level_to_color (level: level) : string =
  (* https://misc.flogisoft.com/bash/tip_colors_and_formatting *)
  match level with
  | `Off -> ""
  | `Fatal -> "\027[38;5;196m"
  | `Error -> "\027[38;5;165m"
  | `Warn -> "\027[38;5;108m"
  | `Info -> "\027[38;5;38m"
  | `Debug -> "\027[38;5;221m"
  | `All -> ""



let should_log (user_level : level) : bool =
  match user_level with
  | `Off -> false
  | `Fatal ->
    (match !loglevel with
     | `Off -> false
     | _ -> true)
  | `Error ->
    (match !loglevel with
     | `Off -> false
     | `Fatal -> false
     | _ -> true)
  | `Warn ->
    (match !loglevel with
     | `Off -> false
     | `Fatal -> false
     | `Error -> false
     | _ -> true)
  | `Info ->
    (match !loglevel with
     | `Off -> false
     | `Fatal -> false
     | `Error -> false
     | `Warn -> false
     | _ -> true)
  | `Debug ->
    (match !loglevel with
     | `Off -> false
     | `Fatal -> false
     | `Error -> false
     | `Warn -> false
     | `Info -> false
     | _ -> true)
  | `All -> true

(* ----------------- *)
(* format *)
(* ----------------- *)
type format = [`Stackdriver | `Regular | `Decorated ]

let format : format ref =
  ref `Stackdriver

let set_format (newformat:format) =
  format := newformat

let timestr time =
  if Float.is_nan time
  then ""
  else
    let result = "(" ^ (time |> Float.to_int |> Int.to_string) ^ "ms)" in
    if Float.to_int time > 100
    then result ^ "(SLOW REQUEST)"
    else result

let format_string (s: string) =
  let last = String.length s in
  let str = String.slice s 0 (min 50 last) in
  let str = Util.string_replace "\n" "\\n" str in
  let str =
    if String.contains str ' '
    then "\"" ^ str ^ "\""
    else str
  in
  str

let print_console_log ~decorate ~level params : unit =
  let color = if decorate then level_to_color level else "" in
  let reset = if decorate then "\x1b[0m" else "" in
  let paramstr = params
                 |> List.map ~f:(fun (k,v) ->
                     color ^ k ^ reset ^ "=" ^ format_string v)
                 |> String.concat ~sep:" "
  in
  let msg =
    color
    ^ "log "
    ^ "level=" ^ level_to_string level
    ^ reset
    ^ " "
    ^ paramstr
  in
  Caml.print_endline msg

let pP ?(f=Batteries.dump)
       ?(name:string="")
       ~(level:level)
       (msg : string)
       (x : 'a)
       : unit =
  if should_log level
  then
    let params = [ "name", name
                 ; "msg", msg
                 ; "data", f x
                (* operation time *)
                (* timestamp *)
                (* slow request *)
                (* ip address *)
                 ]
    in

    match !format with
    | `Stackdriver ->
      print_console_log ~decorate:false ~level params
    | `Regular ->
      print_console_log ~decorate:false ~level params
    | `Decorated ->
      print_console_log ~decorate:true ~level params

let pp ?(f=Batteries.dump)
       ?(name:string="")
       ~(level:level)
       (msg : string)
       (x : 'a)
       : 'a =
  pP ~level ~f ~name msg x;
  x

let debuG = pP ~level:`Debug
let debug = pp ~level:`Debug
let infO = pP ~level:`Info
let info = pp ~level:`Info
let warN = pP ~level:`Warn
let warn = pp ~level:`Warn
let erroR = pP ~level:`Error
let error = pp ~level:`Error
let fataL = pP ~level:`Fatal
let fatal = pp ~level:`Fatal

(* ----------------- *)
(* init *)
(* ----------------- *)

let init ~(level: level) ~(format: format) () =
  set_level level;
  set_format format;
  ()
