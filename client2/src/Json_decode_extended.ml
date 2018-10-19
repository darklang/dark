include Json.Decode

external _stringify : Js.Json.t -> string = "JSON.stringify" [@@bs.val]

let succeed any json = any

let index i decode json =
  if Js.Array.isArray json then begin
    let source = (Obj.magic (json : Js.Json.t) : Js.Json.t array) in
    decode (Array.unsafe_get source i)
  end
  else
    raise @@ DecodeError ("Expected array, got " ^ _stringify json)

let map2 f d1 d2 json =
  f (d1 json) (d2 json)

let map3 f d1 d2 d3 json =
  f (d1 json) (d2 json) (d3 json)

let map4 f d1 d2 d3 d4 json =
  f (d1 json) (d2 json) (d3 json) (d4 json)

let variant0 constructor = succeed constructor

let variant1 constructor d1 =
  map constructor
    (index 1 d1)

let variant2 constructor d1 d2 =
  map2 constructor
    (index 1 d1)
    (index 2 d2)

let variant3 constructor d1 d2 d3 =
  map3 constructor
    (index 1 d1)
    (index 2 d2)
    (index 3 d3)

let variant4 constructor d1 d2 d3 d4 =
  map4 constructor
    (index 1 d1)
    (index 2 d2)
    (index 3 d3)
    (index 4 d4)

let variants decoders =
  let constructors =
    decoders
    |. Belt.List.map (fun (a, _) -> a)
    |> String.concat ", "
  in
  index 0 string
  |> andThen
    (fun str_constructor ->
      (match Belt.List.getAssoc decoders str_constructor (=) with
      | Some decode ->
        decode
      | None ->
        raise @@ DecodeError ("Got " ^ (str_constructor) ^ ", expected one of " ^ constructors)))

let orNull decoder default json =
  if (Obj.magic json : 'a Js.null) == Js.null then
    default
  else
    decoder json

let dict decoder json =
  dict decoder json
  |> Js.Dict.entries
  |> Belt.Map.String.fromArray


let decodeString decoder str =
  try
    Belt.Result.Ok (decoder (Json.parseOrRaise str))
  with e ->
    Belt.Result.Error (Printexc.to_string e)
