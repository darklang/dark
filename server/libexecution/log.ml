open Core_kernel

(* ----------------- *)
(* levels *)
(* ----------------- *)

type level = [`Off
             | `Inspect | `Fatal | `Error | `Warn | `Info |
              `Success | `Debug | `All ]

let loglevel : level ref =
  ref `All

let set_level (newlevel: level) : unit =
  loglevel := newlevel

let level_to_string (level: level) : string =
  match level with
  | `Off -> "OFF"
  | `Inspect -> "INSPECT"
  | `Fatal -> "FATAL"
  | `Error -> "ERROR"
  | `Warn -> "WARN"
  | `Info -> "INFO"
  | `Debug -> "DEBUG"
  | `Success -> "SUCCESS"
  | `All -> "ALL"

let level_to_color (level: level) : string =
  (* https://misc.flogisoft.com/bash/tip_colors_and_formatting *)
  match level with
  | `Off -> ""
  | `Inspect -> "\027[38;5;196m"
  | `Fatal -> "\027[38;5;196m"
  | `Error -> "\027[38;5;165m"
  | `Warn -> "\027[38;5;108m"
  | `Info -> "\027[38;5;38m"
  | `Success -> "\027[38;5;221m"
  | `Debug -> "\027[38;5;221m"
  | `All -> ""



let should_log (user_level : level) : bool =
  match user_level with
  | `Off -> false
  | `Inspect ->
    (match !loglevel with
     | `Off -> false
     | _ -> true)
  | `Fatal ->
    (match !loglevel with
     | `Off -> false
     | `Inspect -> false
     | _ -> true)
  | `Error ->
    (match !loglevel with
     | `Off -> false
     | `Inspect -> false
     | `Fatal -> false
     | _ -> true)
  | `Warn ->
    (match !loglevel with
     | `Off -> false
     | `Inspect -> false
     | `Fatal -> false
     | `Error -> false
     | _ -> true)
  | `Info ->
    (match !loglevel with
     | `Off -> false
     | `Inspect -> false
     | `Fatal -> false
     | `Error -> false
     | `Warn -> false
     | _ -> true)
  | `Success ->
    (match !loglevel with
     | `Off -> false
     | `Inspect -> false
     | `Fatal -> false
     | `Error -> false
     | `Warn -> false
     | `Info -> false
     | _ -> true)
  | `Debug ->
    (match !loglevel with
     | `Off -> false
     | `Inspect -> false
     | `Fatal -> false
     | `Error -> false
     | `Warn -> false
     | `Info -> false
     | `Success -> false
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

let dump (value:'a) : string =
  Batteries.dump value

let pP ?(data)
       ?(params:((string * string) list)=[])
       ~(level:level)
       (name: string)
       : unit =
  if should_log level
  then
    let data_param =
      match data with
      | None -> []
      | Some data -> ["data", data]
    in
    let params = [ "name", (Util.string_replace " " "_" name)
                (* operation time *)
                (* timestamp *)
                (* slow request *)
                (* ip address *)
                 ] @ data_param @ params
    in

    match !format with
    | `Stackdriver ->
      print_console_log ~decorate:false ~level params
    | `Regular ->
      print_console_log ~decorate:false ~level params
    | `Decorated ->
      print_console_log ~decorate:true ~level params

let inspecT
    ?(f=Batteries.dump)
    (name: string)
    (x : 'a)
  : unit =
  pP ~level:`Inspect name ~params:["data", f x]

let inspect
    ?(f=Batteries.dump)
    (name: string)
    (x : 'a)
  : 'a =
  inspecT ~f name x;
  x

let debuG = pP ~level:`Debug
let infO = pP ~level:`Info
let warN = pP ~level:`Warn
let erroR = pP ~level:`Error
let fataL = pP ~level:`Fatal
let succesS = pP ~level:`Success

(* ----------------- *)
(* init *)
(* ----------------- *)

let init ~(level: level) ~(format: format) () =
  set_level level;
  set_format format;
  ()
