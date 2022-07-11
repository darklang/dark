include Json.Encode

let variant = (constructor, vals) => jsonArray(Array.of_list(list{string(constructor), ...vals}))

let tcStrSet = set =>
  set |> Tc.Set.toList |> Tc.List.map(~f=Js.Json.string) |> Belt.List.toArray |> jsonArray

let tcStrDict = (f, dict) =>
  dict |> Tc.Map.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let beltStrDict = (f, dict) =>
  dict |> Belt.Map.String.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let int64 = i =>
  if i > 9007199254740992L {
    i->Int64.to_string->string
  } else if i < -9007199254740992L {
    i->Int64.to_string->string
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53.
    i->Int64.to_float->Json.Encode.float
  }

let uint64 = (i : U.UInt64.t) =>
  if i > U.UInt64.ofInt64(9007199254740992L) {
    i->U.UInt64.toString->string
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53.
    i->U.UInt64.toInt64->Int64.to_float->Json.Encode.float
  }

