module Tablecloth.Map

// A collection of key-value pairs

type t<'key, 'value when 'key : comparison> = Map<'key, 'value>

let keepLatestOnly _ latest = latest

let empty : t<'key, 'value> = Map.empty

let singleton key value : t<'key, 'value> = Map [ (key, value) ]

let fromArray (elements : ('key * 'value) array) : t<'key, 'value> =
  Map.ofArray elements

let from_array a = fromArray a

let fromList (elements : ('key * 'value) list) : t<'key, 'value> =
  Map.ofList elements

let from_list es = fromList es

let isEmpty m = Map.isEmpty m

let is_empty m = isEmpty m

let includes k m = Map.containsKey k m

let length m = Map.count m

let minimum m =
  if length m = 0 then
    None
  else
    m |> Map.toList |> List.map fst |> Tablecloth.List.minimum

let maximum m =
  if length m = 0 then
    None
  else
    m |> Map.toList |> List.map fst |> Tablecloth.List.maximum

let extent t = Tablecloth.Option.both (minimum t) (maximum t)

let add key value m = Map.add key value m
//
// let ( .?{}<- ) (map : t<'key, 'value>) (key : 'key) (value : 'value) :
//     t<'key, 'value> =
//   add map ~key ~value
//
//
let remove k m = Map.remove k m

let get k m = Map.tryFind k m

// let ( .?{} ) (map : ('key, 'value, _) t) (key : 'key) : 'value option =
//   get map key


let update key f m = Map.change key f m

// let merge f m1 m2 =
//   Map.merge m1 m2 (fun key desc ->
//       match desc with
//       | Left v1 ->
//           f key (Some v1) None
//       | Right v2 ->
//           f key None (Some v2)
//       | Both (v1, v2) ->
//           f key (Some v1) (Some v2))

let fold (initial : 'a) (f : 'key -> 'value -> 'a -> 'a) (m : t<'key, 'value>) : 'a =
  Map.fold (fun map k v -> f k v map) initial m

let map f m = Map.map (fun _ v -> f v) m

let mapWithIndex (f : 'key -> 'value -> 'b) (m : t<'key, 'value>) : t<'key, 'b> =
  Map.map f m

let map_with_index f m = mapWithIndex f m

let filter f m = Map.filter (fun _ v -> f v) m

let partition f m = Map.partition f m

let find f m = Map.tryFindKey f m |> Option.map (fun k -> (k, m.[k]))

let any f m = Map.exists (fun _ v -> f v) m

let all f m = Map.forall (fun _ v -> f v) m

let forEach f m = Map.iter (fun _ v -> f v) m

let for_each f m = forEach f m

let forEachWithIndex f m : unit = Map.iter f m

let for_each_with_index f m = forEachWithIndex f m

let keys (map : Map<'k, 'v>) =
  seq {
    for KeyValue (key, value) in map do
      yield key
  }
  |> List.ofSeq

let values (map : Map<'k, 'v>) =
  seq {
    for KeyValue (key, value) in map do
      yield value
  }
  |> List.ofSeq


let toArray m = Map.toArray m

let to_array m = toArray m

let toList m = Map.toList m

let to_list m = toList m
