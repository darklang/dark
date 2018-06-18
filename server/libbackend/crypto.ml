open Core_kernel

let sha (input: string) ~f : string =
  input
  |> Cstruct.of_string
  |> f
  |> Cstruct.to_string
  |> B64.encode ~pad:true

let digest256 (input: string) : string =
  sha ~f:Nocrypto.Hash.SHA256.digest input

let digest384 (input: string) : string =
  sha ~f:Nocrypto.Hash.SHA384.digest input

let hash (input:string) : string =
  input
  |> Cstruct.of_string
  |> Nocrypto.Hash.SHA384.digest
  |> Cstruct.to_string
  |> B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false


