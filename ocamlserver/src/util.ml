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
  Random.int (Core.Std.Int.pow 2 29)

let inspect (msg : string) (x : 'a) : 'a =
  let () = print_endline ("inspect: " ^ msg ^ ": " ^ x) in
  x
