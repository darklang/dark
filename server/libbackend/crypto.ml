open Core_kernel

let hash (input:string) : string =
  input
  |> Cstruct.of_string
  |> Nocrypto.Hash.SHA384.digest
  |> Cstruct.to_string
  |> B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false


