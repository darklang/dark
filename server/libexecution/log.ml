open Core_kernel

(* ----------------- *)
(* levels *)
(* ----------------- *)

type level = [`Off | `Fatal | `Error | `Warn | `Info | `Debug | `All ]

let level : level ref =
  ref `All

let set_level (newlevel: level) : unit =
  level := newlevel

let level_to_string (level: level) : string =
  match level with
  | `Off -> "off"
  | `Fatal -> "fatal"
  | `Error -> "error"
  | `Warn -> "warn"
  | `Info -> "info"
  | `Debug -> "debug"
  | `All -> "all"

let level : level ref =
  ref `All

let should_log (user_level : level) : bool =
  match user_level with
  | `Off -> false
  | `Fatal ->
    (match !level with
     | `Off -> false
     | _ -> true)
  | `Error ->
    (match !level with
     | `Off -> false
     | `Fatal -> false
     | _ -> true)
  | `Warn ->
    (match !level with
     | `Off -> false
     | `Fatal -> false
     | `Error -> false
     | _ -> true)
  | `Info ->
    (match !level with
     | `Off -> false
     | `Fatal -> false
     | `Error -> false
     | `Warn -> false
     | _ -> true)
  | `Debug ->
    (match !level with
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
type format = [`Stackdriver | `Regular ]

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


let print_console_log ~ind ~msg ~start ~stop ~time ~f x : unit =
  let red = "\x1b[6;31m" in
  let black = "\x1b[6;30m" in
  let reset = "\x1b[0m" in
  let indentStr = String.make ind '>' in
  let prefix =
    if false
      (* TODO: SPLIT *)
      (* Config.log_decorate *)
    then black ^ "log "
         ^ reset ^ indentStr
         ^ " " ^ red ^ msg ^ " "
         ^ reset
         ^ timestr time
         ^ ": "
    else "log " ^ msg ^ " " ^ timestr time ^ ": "
  in
  x
  |> f
  |> (fun s ->
      let last = String.length s in
      String.slice s (min start last) (min stop last))
  |> (^) prefix
  |> print_endline

let print_stackdriver_log ~msg ~start ~stop ~f ~time x : unit =
  let prefix = msg ^ " " ^ timestr time ^ ": " in
  x
  |> f
  |> (fun s ->
      let last = String.length s in
      String.slice s (min start last) (min stop last))
  |> (^) prefix
  |> print_endline


(* ----------------- *)
(* logging *)
(* ----------------- *)
let pP ?(f=Batteries.dump)
       ?(start=0)
       ?(stop=0)
       ?(show:bool=true)
       ?(time:float=Float.nan)
       ?(ind=0)
       ?(name:string="")
       (msg : string)
       (x : 'a)
       : unit =
  if show && (not (!level = `Off))
  then
    if !format = `Stackdriver
    then print_stackdriver_log ~msg ~start ~stop ~time ~f x
    else print_console_log ~ind ~msg ~start ~stop ~time ~f x

let pp ?(f=Batteries.dump)
       ?(start=0)
       ?(stop=0)
       ?(show:bool=true)
       ?(time:float=Float.nan)
       ?(ind=0)
       ?(name:string="")
       (msg : string)
       (x : 'a)
       : 'a =
  pP ~f ~name ~start ~stop ~show ~time ~ind msg x;
  x

let debuG ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Debug
  then pP ~f ~name ~start ~stop ~show ~time ~ind msg x
  else ()

let debug ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Debug
  then pp ~f ~name ~start ~stop ~show ~time ~ind msg x
  else x

let infO ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Info
  then pP ~f ~name ~start ~stop ~time ~show ~ind msg x
  else ()

let info ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Info
  then pp ~f ~name ~start ~stop ~show ~time ~ind msg x
  else x

let warN ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Warn
  then pP ~f ~name ~start ~stop ~show ~time ~ind msg x
  else ()

let warn ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Warn
  then pp ~f ~name ~start ~stop ~show ~time ~ind msg x
  else x

let erroR ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Error
  then pP ~f ~name ~start ~stop ~show ~time ~ind msg x
  else ()

let error ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Error
  then pp ~f ~name ~start ~stop ~show ~time ~ind msg x
  else x

let fataL ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Fatal
  then pP ~f ~name ~start ~stop ~show ~time ~ind msg x
  else ()

let fatal ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(time:float=Float.nan)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Fatal
  then pp ~f ~name ~start ~stop ~show ~time ~ind msg x
  else x

(* ----------------- *)
(* init *)
(* ----------------- *)

let init ~(level: level) ~(format: format) () =
  set_level level;
  set_format format;
  ()
