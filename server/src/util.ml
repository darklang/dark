let slurp f =
  let ic = open_in f in
  try
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    s
  with e ->
    close_in_noerr ic;
    raise e

let create_id (_ : unit) : int =
  Random.int (Core.Int.pow 2 29)

let inspecT (msg : string) (x : 'a) : unit =
  print_endline ("inspect: " ^ msg ^ ": " ^ x)

let inspect (msg : string) (x : 'a) : 'a =
  inspecT msg x;
  x

(* How do you chain using (^) *)
let string_append (s1: string) (s2: string) : string =
  s1 ^ s2
