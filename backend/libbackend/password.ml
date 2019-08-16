type t = string

let to_bytes pw = pw

let from_plaintext password =
  password
  |> Bytes.of_string
  |> Libcrypto.Hash.wipe_to_password
  |> Libcrypto.Hash.hash_password Sodium.Password_hash.interactive
  |> Bytes.to_string
  |> B64.encode


let invalid = ""

let from_hash pw = pw
