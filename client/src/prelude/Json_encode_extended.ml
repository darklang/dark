include Json.Encode

let variant constructor vals =
  jsonArray (Array.of_list (string constructor :: vals))


let tcStrSet set =
  set
  |> Tc.Set.toList
  |> Tc.List.map ~f:Js.Json.string
  |> Belt.List.toArray
  |> jsonArray


let tcStrDict f dict =
  dict |> Tc.Map.toList |> Tc.List.map ~f:(fun (k, v) -> (k, f v)) |> object_
