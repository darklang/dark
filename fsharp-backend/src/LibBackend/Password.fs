module LibBackend.Password

// FSTODO: this does not yet match the OCaml version. It's not strictly
// necessary that this version does match, as it's not really used. However,
// Crypto::password function use the same algorithm so we do need to get this
// right.

type T = private Pw of string

let toString (Pw pw : T) : string = pw

let fromPlaintext (password : string) : T =
  password |> Sodium.PasswordHash.ArgonHashString |> Prelude.base64Encode |> Pw

let invalid : T = Pw ""

let fromHash (hash : string) : T = Pw hash
