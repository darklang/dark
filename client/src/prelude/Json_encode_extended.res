include Json.Encode

let variant = (constructor, vals) => jsonArray(Array.of_list(list{string(constructor), ...vals}))

let tcStrSet = set =>
  set |> Tc.Set.toList |> Tc.List.map(~f=Js.Json.string) |> Belt.List.toArray |> jsonArray

let tcStrDict = (f, dict) =>
  dict |> Tc.Map.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let beltStrDict = (f, dict) =>
  dict |> Belt.Map.String.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let date = (d: Js.Date.t) => string(Js.Date.toString(d))

let int64 = (i: int64) =>
  if i > 9007199254740992L {
    i->Int64.to_string->string
  } else if i < -9007199254740992L {
    i->Int64.to_string->string
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53.
    i->Int64.to_float->Json.Encode.float
  }

let uint64 = (i: UInt64.t) =>
  // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
  // 2^53.
  switch UInt64.toFloat(i) {
  | Some(f) => f->Json.Encode.float
  | None => i->UInt64.toString->string
  }
