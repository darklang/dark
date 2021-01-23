module Tablecloth.Map

// A collection of key-value pairs

type t<'key, 'value when 'key : comparison> = Map<'key, 'value>

let keepLatestOnly _ latest = latest

let empty : t<'key, 'value> = Map.empty

let singleton key value : t<'key, 'value> = Map [ (key, value) ]

let fromArray (elements : ('key * 'value) array) : t<'key, 'value> =
  Map.ofArray elements

let from_array a = fromArray a

// let fromList
//
//     (elements : ('key * 'value) list) : t<'key, 'value> =
//   Base.Map.of_alist_reduce
//     elements
//     ~f:keepLatestOnly
//
//
// let from_list = fromList
//
// let isEmpty = Base.Map.is_empty
//
// let is_empty = isEmpty
//
// let includes = Base.Map.mem
//
// let length = Base.Map.length
//
// let minimum t = Base.Map.min_elt t |> Option.map fst
//
// let maximum t = Base.Map.max_elt t |> Option.map fst
//
// let extent t = TableclothOption.both (minimum t) (maximum t)
//
// let add m ~key ~value = Base.Map.set m ~key ~data:value
//
// let ( .?{}<- ) (map : t<'key, 'value>) (key : 'key) (value : 'value) :
//     t<'key, 'value> =
//   add map ~key ~value
//
//
// let remove = Base.Map.remove
//
// let get = Base.Map.find
//
// let ( .?{} ) (map : ('key, 'value, _) t) (key : 'key) : 'value option =
//   get map key
//
//
// let update m ~key ~f = Base.Map.change m key ~f
//
// let merge m1 m2 ~f =
//   Base.Map.merge m1 m2 ~f:(fun ~key desc ->
//       match desc with
//       | `Left v1 ->
//           f key (Some v1) None
//       | `Right v2 ->
//           f key None (Some v2)
//       | `Both (v1, v2) ->
//           f key (Some v1) (Some v2))
//
//
// let map = Base.Map.map
//
// let mapWithIndex t ~f = Base.Map.mapi t ~f:(fun ~key ~data -> f key data)
//
// let map_with_index = mapWithIndex
//
// let filter = Base.Map.filter
//
// let partition m ~f =
//   Base.Map.partition_mapi m ~f:(fun ~key ~data ->
//       if f ~key ~value:data then `Fst data else `Snd data)
//
//
// let find m ~f =
//   Base.Map.fold m ~init:None ~f:(fun ~key ~data matching ->
//       match matching with
//       | Some _ ->
//           matching
//       | None ->
//           if f ~key ~value:data then Some (key, data) else None)
//
//
// let any = Base.Map.exists
//
// let all = Base.Map.for_all
//
// let forEach = Base.Map.iter
//
// let for_each = forEach
//
// let forEachWithIndex
//     (map : ('key, 'value, _) t) ~(f : key:'key -> value:'value -> unit) : unit =
//   Base.Map.iteri map ~f:(fun ~key ~data -> f ~key ~value:data)
//
//
// let for_each_with_index = forEachWithIndex
//
// let fold m ~initial ~f =
//   Base.Map.fold m ~init:initial ~f:(fun ~key ~data acc ->
//       f acc ~key ~value:data)
//
//
// let keys = Base.Map.keys
//
// let values = Base.Map.data
//
// let toArray m = Base.Map.to_alist m |> Base.List.to_array
//
// let to_array = toArray
//
// let toList m = Base.Map.to_alist m
//
// let to_list = toList
//
let placeholder = 0
