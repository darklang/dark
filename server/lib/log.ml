open Core

type level = [`Off | `Fatal | `Error | `Warn | `Info | `Debug | `All ]

let level : level ref =
  ref `All

let quiet name =
  name = "execution"

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


let pP ?(f=Batteries.dump)
       ?(start=0)
       ?(stop=0)
       ?(show:bool=true)
       ?(ind=0)
       ?(name:string="")
       (msg : string)
       (x : 'a)
       : unit =
  if show && (not (quiet name))
  then
    let red = "\x1b[6;31m" in
    let black = "\x1b[6;30m" in
    let reset = "\x1b[0m" in
    let indentStr = String.make ind '>' in
    let prefix = black ^ "log " ^ reset ^ indentStr ^ " " ^ red ^ msg ^ ": " ^ reset in
    x
    |> f
    |> (fun s ->
    let last = String.length s in
      String.slice s (min start last) (min stop last))
    |> (^) prefix
    |> print_endline

let pp ?(f=Batteries.dump)
       ?(start=0)
       ?(stop=0)
       ?(show:bool=true)
       ?(ind=0)
       ?(name:string="")
       (msg : string)
       (x : 'a)
       : 'a =
  pP ~f ~name ~start ~stop ~show ~ind msg x;
  x

let debuG ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Debug
  then pP ~f ~name ~start ~stop ~show ~ind msg x
  else ()

let debug ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Debug
  then pp ~f ~name ~start ~stop ~show ~ind msg x
  else x

let infO ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Info
  then pP ~f ~name ~start ~stop ~show ~ind msg x
  else ()

let info ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Info
  then pp ~f ~name ~start ~stop ~show ~ind msg x
  else x

let warN ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Warn
  then pP ~f ~name ~start ~stop ~show ~ind msg x
  else ()

let warn ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Warn
  then pp ~f ~name ~start ~stop ~show ~ind msg x
  else x

let erroR ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Error
  then pP ~f ~name ~start ~stop ~show ~ind msg x
  else ()

let error ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Error
  then pp ~f ~name ~start ~stop ~show ~ind msg x
  else x

let fataL ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : unit =
  if should_log `Fatal
  then pP ~f ~name ~start ~stop ~show ~ind msg x
  else ()

let fatal ?(f=Batteries.dump)
          ?(start=0)
          ?(stop=0)
          ?(show:bool=true)
          ?(ind=0)
          ?(name:string="")
          (msg : string)
          (x : 'a)
          : 'a =
  if should_log `Fatal
  then pp ~f ~name ~start ~stop ~show ~ind msg x
  else x


(* TODO: make this properly respect log levels *)

let tS (msg : string) : unit =
  let time = Float.mod_float (1000.0 *. Unix.gettimeofday ()) 10000.0 in
  let ts = string_of_float time in
  let red = "\x1b[6;31m" in
  let reset = "\x1b[0m" in
  red ^ "ts: " ^ msg ^ " (" ^ ts ^ "): " ^ reset
  |> print_endline

let ts (msg : string) (x : 'a) : 'a =
  tS msg;
  x


