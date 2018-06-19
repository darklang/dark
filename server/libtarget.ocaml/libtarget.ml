open Core

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

let date_to_isostring (d: Time.t) : string =
  (* for conduit tests. May do something different later *)
  Time.format d "%FT%TZ" ~zone:Time.Zone.utc

let date_of_isostring (str: string) : Time.t =
  Time.parse str ~fmt:"%FT%TZ" ~zone:Time.Zone.utc

let date_to_sqlstring (d: Time.t) : string =
  Time.format d "%Y-%m-%d %H:%M:%S" ~zone:Time.Zone.utc

let date_of_sqlstring (str: string) : Time.t =
  Time.parse str ~fmt:"%Y-%m-%d %H:%M:%S" ~zone:Time.Zone.utc


