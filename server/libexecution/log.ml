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
  | `Off -> "off"
  | `Fatal -> "fatal"
  | `Error -> "error"
  | `Warn -> "warn"
  | `Info -> "info"
  | `Debug -> "debug"
  | `All -> "all"

let level_to_color (level: level) : string =
  match level with
  | `Off -> "off"
  | `Fatal -> "fatal"
  | `Error -> "error"
  | `Warn -> "warn"
  | `Info -> "info"
  | `Debug -> "debug"
  | `All -> "all"



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

let print_endline =
  Caml.print_endline

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


let print_console_log ~decorate ~level ~msg ~f x : unit =
  let red = "\x1b[6;31m" in
  let black = "\x1b[6;30m" in
  let reset = "\x1b[0m" in
  let prefix =
    if decorate
    then black ^ level_to_string level
         ^ reset
         ^ " " ^ red ^ msg ^ " "
         ^ reset
    else "log " ^ msg ^ ": "
  in
  x
  |> f
  |> (fun s ->
      let last = String.length s in
      String.slice s 0 (min 1000 last))
  |> (^) prefix
  |> print_endline

let print_stackdriver_log ~level ~msg ~f x : unit =
  let prefix = msg ^ " " ^ ": " in
  x
  |> f
  |> (fun s ->
      let last = String.length s in
      String.slice s 0 (min 1000 last))
  |> (^) prefix
  |> print_endline


let pP ?(f=Batteries.dump)
       ?(name:string="")
       ~(level:level)
       (msg : string)
       (x : 'a)
       : unit =
  if should_log level
  then
    match !format with
    | `Stackdriver ->
      print_console_log ~decorate:false ~level ~msg ~f x
      (* print_stackdriver_log ~level ~msg ~f x *)
    | `Regular ->
      print_console_log ~decorate:false ~level ~msg ~f x
    | `Decorated ->
      print_console_log ~decorate:true ~level ~msg ~f x

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
