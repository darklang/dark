open Prelude

let obj (l : (string * dval) list) : dval =
  l |> List.toArray |> Belt.Map.String.fromArray |> fun m -> DObj m
