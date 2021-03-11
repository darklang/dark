type t = string

let to_bytes pw = pw

let from_plaintext password =
  password
  |> Bytes.of_string
  |> Sodium.Password_hash.Bytes.wipe_to_password
  |> Sodium.Password_hash.Bytes.hash_password Sodium.Password_hash.interactive
  |> Bytes.to_string
  |> B64.encode


let invalid = ""

let from_hash pw = pw
