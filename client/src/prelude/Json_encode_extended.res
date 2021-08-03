include Json.Encode

let variant = (constructor, vals) => jsonArray(Array.of_list(list{string(constructor), ...vals}))

let tcStrSet = set =>
  set |> Tc.Set.toList |> Tc.List.map(~f=Js.Json.string) |> Belt.List.toArray |> jsonArray

let tcStrDict = (f, dict) =>
  dict |> Tc.Map.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let beltStrDict = (f, dict) =>
  dict |> Belt.Map.String.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_
