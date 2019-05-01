open Tc
include Json.Decode

external _stringify : Js.Json.t -> string = "JSON.stringify" [@@bs.val]

let succeed any _ = any

let index i decode json =
  if Js.Array.isArray json
  then
    let source : Js.Json.t array = Obj.magic (json : Js.Json.t) in
    decode (Caml.Array.unsafe_get source i)
  else raise (DecodeError ("Expected array, got " ^ _stringify json))


let map2 f d1 d2 json = f (d1 json) (d2 json)

let map3 f d1 d2 d3 json = f (d1 json) (d2 json) (d3 json)

let map4 f d1 d2 d3 d4 json = f (d1 json) (d2 json) (d3 json) (d4 json)

let variant0 constructor = succeed constructor

let variant1 constructor d1 = map constructor (index 1 d1)

let variant2 constructor d1 d2 = map2 constructor (index 1 d1) (index 2 d2)

let variant3 constructor d1 d2 d3 =
  map3 constructor (index 1 d1) (index 2 d2) (index 3 d3)


let variant4 constructor d1 d2 d3 d4 =
  map4 constructor (index 1 d1) (index 2 d2) (index 3 d3) (index 4 d4)


let variants decoders =
  let constructors =
    decoders |> List.map ~f:Tuple2.first |> String.join ~sep:", "
  in
  index 0 string
  |> andThen (fun str_constructor ->
         match Belt.List.getAssoc decoders str_constructor ( = ) with
         | Some decode ->
             decode
         | None ->
             raise
             @@ DecodeError
                  ( "Got "
                  ^ str_constructor
                  ^ ", expected one of "
                  ^ constructors ) )


let orNull decoder default json =
  if (Obj.magic json : 'a Js.null) == Js.null then default else decoder json


let dict decoder json =
  dict decoder json |> Js.Dict.entries |> Belt.Map.String.fromArray


let tcStrSet json = json |> array string |> Belt.Set.String.fromArray

let decodeString decoder str =
  try Belt.Result.Ok (decoder (Json.parseOrRaise str)) with
  | DecodeError e ->
      (* Debug.loG ("json decoding error: '" ^ e ^ "'") str; *)
      Belt.Result.Error e
  | Json.ParseError e ->
      (* Debug.loG ("json parse error: '" ^ e ^ "'") str; *)
      Error e
  | _ ->
      (* let errStr = Printexc.to_string e in *)
      (* Debug.loG ("unknown json parsing error: '" ^ errStr ^ "'") str; *)
      Error str
