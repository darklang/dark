include Json.Decode

let succeed any json = any

let index i decode json =
  if Js.Array.isArray json then begin
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    decode (Array.unsafe_get source i)
  end
  else
    raise @@ DecodeError ("Expected array, got " ^ _stringify json)

let decodeVariant0 constructor = succeed constructor

let decodeVariant1 constructor d1 =
  map constructor (index 1 d1)

