open Core

let quiet name =
  name = "execution"

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


