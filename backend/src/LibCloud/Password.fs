module LibBackend.Password

open Prelude

type T =
  private
  | Pw of string

  override this.ToString() : string =
    let (Pw pw) = this
    pw

let fromPlaintext (password : string) : T =
  password
  |> Sodium.PasswordHash.ArgonHashString
  |> UTF8.toBytes
  |> Base64.defaultEncodeToString
  |> Pw

let invalid : T = Pw ""

let fromHash (hash : string) : T = Pw hash
