open Prelude

let obj = (l: list<(string, dval)>): dval =>
  l |> List.toArray |> Belt.Map.String.fromArray |> (m => DObj(m))
