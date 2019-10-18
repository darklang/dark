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
  sha ~f:Nocrypto.Hash.SHA384.digest input

let digest384_bytes (input : Bytes.t) : string =
  sha_bytes ~f:Nocrypto.Hash.SHA384.digest input

let regexp_replace ~(pattern : string) ~(replacement : string) (str : string) :
    string =
  Re2.replace_exn (Re2.create_exn pattern) str ~f:(fun _ -> replacement)


let string_split ~sep s : string list = Str.split (Str.regexp_string sep) s
