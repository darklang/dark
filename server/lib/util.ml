open Core

let slurp f =
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

let create_id (_ : unit) : int =
  Random.int (Int.pow 2 29)

let inspecT (msg : string) (x : 'a) : unit =
  print_endline ("inspect: " ^ msg ^ ": " ^ x)

let inspect (msg : string) (x : 'a) : 'a =
  inspecT msg x;
  x

(* How do you chain using (^) *)
let string_append (s1: string) (s2: string) : string =
  s1 ^ s2

let to_list e = [e]
