open Core
open Lwt.Infix

let readfile ?(default="") f : string =
  (* Has a 65k limit *)
  let flags = [Unix.O_RDONLY; Unix.O_CREAT] in
  let file = Unix.openfile f ~mode:flags ~perm:0o640 in
  let len = 1000000 in
  let raw = Bytes.create len in
  let count = Unix.read file ~buf:raw ~pos:0 ~len in
  let str = Caml.Bytes.sub_string raw 0 count in
  if str = "" then
    default
  else
    str

(* TODO: Some files only work with readfile above, some only with readfile below *)
let readfile2 ?(default="") f : string =
  let ic = Caml.open_in f in
  try
    let n = Caml.in_channel_length ic in
    let s = Bytes.create n in
    Caml.really_input ic s 0 n;
    Caml.close_in ic;
    s
  with e ->
    Caml.close_in_noerr ic;
    raise e

let readfile_lwt ?(default="") f : string Lwt.t =
  Lwt_io.with_file ~mode:Lwt_io.input f (Lwt_io.read ?count:None) >|= function
  | "" -> default
  | s -> s

let writefile f str : unit =
  let flags = [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] in
  let file = Unix.openfile f ~mode:flags ~perm:0o640 in
  let _ = Unix.write file ~buf:str in
  Unix.close file



let create_id (_ : unit) : int =
  Random.int (Int.pow 2 29)

let inspecT ?(formatter=Batteries.dump)
            ?(start=0)
            ?(stop=0)
            ?(show:bool=true)
            ?(indent=0)
            (msg : string)
            (x : 'a)
            : unit =
  if show
  then
    let red = "\x1b[6;31m" in
    let reset = "\x1b[0m" in
    let indentStr = String.make indent '>' in
    let prefix = red ^ "inspect: " ^ indentStr ^ msg ^ ": " ^ reset in
    x
    |> formatter
    |> (fun s ->
    let last = String.length s in
      String.slice s (min start last) (min stop last))
    |> (^) prefix
    |> print_endline

let inspect ?(formatter=Batteries.dump)
            ?(start=0)
            ?(stop=0)
            ?(show:bool=true)
            ?(indent=0)
            (msg : string)
            (x : 'a)
            : 'a =
  inspecT ~formatter ~start ~stop ~show ~indent msg x;
  x

let tS (msg : string) (x : 'a) : unit =
  let time = Float.mod_float (1000.0 *. Unix.gettimeofday ()) 10000.0 in
  let ts = string_of_float time in
  let red = "\x1b[6;31m" in
  let reset = "\x1b[0m" in
  red ^ "ts: " ^ msg ^ " (" ^ ts ^ "): " ^ reset ^ (Batteries.dump x)
  |> print_endline

let ts (msg : string) (x : 'a) : 'a =
  tS msg x;
  x

let string_replace (search: string) (replace: string) (str: string) : string =
  String.Search_pattern.replace_first (String.Search_pattern.create search) ~in_:str ~with_:replace

let random_string length =
    let gen() = match Random.int(26+26+10) with
        n when n < 26 -> int_of_char 'a' + n
      | n when n < 26 + 26 -> int_of_char 'A' + n - 26
      | n -> int_of_char '0' + n - 26 - 26 in
    let gen _ = String.make 1 (char_of_int(gen())) in
    String.concat ~sep:"" (Array.to_list (Array.init length gen))


let list_repeat times item =
  List.init times ~f:(fun _ -> item)

