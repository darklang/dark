include Json.Encode

let variant constructor vals =
  jsonArray (Array.of_list (string constructor :: vals))


let tcStrSet set =
  set
  |> Tc.StrSet.toList
  |> Tc.List.map ~f:Js.Json.string
  |> Belt.List.toArray
  |> jsonArray
