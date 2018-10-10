include Json.Decode

let succeed any json = any

let index i decode json =
  if Js.Array.isArray json then begin
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    decode (Array.unsafe_get source i)
  end
  else
    raise @@ DecodeError ("Expected array, got " ^ _stringify json)

let map2 f d1 d2 json =
  List.map ~f [d1 json; d2 json]

let map3 f d1 d2 d3 json =
  List.map ~f [d1 json; d2 json; d3 json]

let map4 f d1 d2 d3 d4 json =
  List.map ~f [d1 json; d2 json; d3 json; d4 json]

let decodeVariant0 constructor = succeed constructor

let decodeVariant1 constructor d1 =
  map constructor
    (index 1 d1)

let decodeVariant2 constructor d1 d2 =
  map2 constructor
    (index 1 d1)
    (index 2 d2)

let decodeVariant3 constructor d1 d2 d3 =
  map3 constructor
    (index 1 d1)
    (index 2 d2)
    (index 3 d3)

let decodeVariant4 constructor d1 d2 d3 d4 =
  map4 constructor
    (index 1 d1)
    (index 2 d2)
    (index 3 d3)
    (index 4 d4)



