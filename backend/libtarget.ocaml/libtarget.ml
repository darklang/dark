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


let base64url_bytes (bytes : Bytes.t) : string =
  bytes
  |> Bytes.to_string
  |> B64.encode ~pad:true ~alphabet:B64.uri_safe_alphabet


let regexp_replace ~(pattern : string) ~(replacement : string) (str : string) :
    string =
  Re2.replace_exn (Re2.create_exn pattern) str ~f:(fun _ -> replacement)


exception Invalid_B64 of string

let valid_rfc4648_b64_or_exn (str : string) =
  let rfc4648_section5_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\\-_"
  in
  let replaced_string =
    regexp_replace (* '=' isn't in the alphabet, but we allow it as padding *)
      ~pattern:("[" ^ rfc4648_section5_alphabet ^ "=" ^ "]*")
      ~replacement:""
      str
  in
  if replaced_string = ""
  then str
  else
    raise
      (Invalid_B64
         ( "Expected B64 input matching RFC4648 alphabet, but contained invalid characters: "
         ^ replaced_string ))


(* Raises Not_found when passed a string with an out-of-alphabet character *)
let bytes_from_base64url (b64 : string) : Bytes.t =
  b64
  |> valid_rfc4648_b64_or_exn
  |> B64.decode ~alphabet:B64.uri_safe_alphabet
  |> Bytes.of_string


let string_split ~sep s : string list = Str.split (Str.regexp_string sep) s
