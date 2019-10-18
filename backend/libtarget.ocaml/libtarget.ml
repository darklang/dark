let sha (input : string) ~f : string =
  input
  |> Cstruct.of_string
  |> f
  |> Cstruct.to_string
  |> B64.encode ~pad:true ~alphabet:B64.uri_safe_alphabet

let sha_bytes (input : Bytes.t) ~f : string =
  input
  |> Cstruct.of_bytes
  |> f
  |> Cstruct.to_string
  |> B64.encode ~pad:true ~alphabet:B64.uri_safe_alphabet


let digest256 (input : string) : string =
  sha ~f:Nocrypto.Hash.SHA256.digest input


let digest384 (input : string) : string =
  let _ = (Caml.print_string "LIBTARGET digest384") in
  sha ~f:Nocrypto.Hash.SHA384.digest input

let digest384_bytes (input : Bytes.t) : string =
  let _ = (Caml.print_string "LIBTARGET digest384") in
  sha_bytes ~f:Nocrypto.Hash.SHA384.digest input

let base64_bytes (bytes : Bytes.t) : string =
  let _ = (Caml.print_string "base64_bytes") in
  let _ = (Caml.print_int (Bytes.length bytes)) in
  bytes |> Bytes.to_string |> B64.encode

let bytes_from_base64 (b64 : string) : Bytes.t = 
  let _ = (Caml.print_string "bytes_from_base64") in
  let _ = (Caml.print_string b64) in
  b64 |> B64.decode |> Bytes.of_string


let regexp_replace ~(pattern : string) ~(replacement : string) (str : string) :
    string =
  Re2.replace_exn (Re2.create_exn pattern) str ~f:(fun _ -> replacement)


let string_split ~sep s : string list = Str.split (Str.regexp_string sep) s
