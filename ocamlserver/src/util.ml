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
