open Tc

let random (a : unit) : int = Native.Random.random a

let reContains ~(re : string) (s : string) : bool =
  Regex.contains ~re:(Regex.regex re) s


let reExactly (re : string) (s : string) : bool =
  reContains ~re:("^" ^ re ^ "$") s


let findIndex ~(f : 'a -> bool) (l : 'a list) : (int * 'a) option =
  List.find ~f:(fun (_, a) -> f a) (List.indexedMap ~f:Tuple2.create l)


let listPrevious ~(value : 'a) (l : 'a list) : 'a option =
  l
  |> List.elemIndex ~value
  |> Option.map ~f:(fun x -> x - 1)
  |> Option.andThen ~f:(fun (index : int) -> List.getAt ~index l)


let listNext ~(value : 'a) (l : 'a list) : 'a option =
  l
  |> List.elemIndex ~value
  |> Option.map ~f:(fun x -> x + 1)
  |> Option.andThen ~f:(fun (index : int) -> List.getAt ~index l)


let listPreviousWrap ~(value : 'a) (l : 'a list) : 'a option =
  l |> listPrevious ~value |> Option.orElse (List.last l)


let listNextWrap ~(value : 'a) (l : 'a list) : 'a option =
  l |> listNext ~value |> Option.orElse (List.head l)


let removeQuotes (s : string) : string =
  if String.endsWith ~suffix:"\"" s && String.startsWith ~prefix:"\"" s
  then s |> String.dropLeft ~count:1 |> String.dropRight ~count:1
  else s
