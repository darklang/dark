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

let date_to_isostring (d: Core_kernel.Time.t) : string =
  (* for conduit tests. May do something different later *)
  Core.Time.format d "%FT%TZ" ~zone:Core.Time.Zone.utc

let date_of_isostring (str: string) : Core_kernel.Time.t =
  Core.Time.parse str ~fmt:"%FT%TZ" ~zone:Core.Time.Zone.utc

let date_to_sqlstring (d: Core_kernel.Time.t) : string =
  Core.Time.format d "%Y-%m-%d %H:%M:%S" ~zone:Core.Time.Zone.utc

let date_of_sqlstring (str: string) : Core_kernel.Time.t =
  Core.Time.parse str ~fmt:"%Y-%m-%d %H:%M:%S" ~zone:Core.Time.Zone.utc

let dump = Batteries.dump

