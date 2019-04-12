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


(* The view we see is different from the value representation in a few *)
(* ways: *)
(* - the start and end quotes are skipped *)
(* - all other quotes are escaped *)
let transformToStringEntry (s_ : string) : string =
  (* the first time we won't have a closing quote so add it *)
  let s = if String.endsWith ~suffix:"\"" s_ then s_ else s_ ^ "\"" in
  s
  |> String.dropLeft ~count:1
  |> String.dropRight ~count:1
  |> Regex.replace ~re:(Regex.regex "\\\\\"") ~repl:"\""


let transformFromStringEntry (s : string) : string =
  let s2 = s |> Regex.replace ~re:(Regex.regex "\"") ~repl:"\\\"" in
  "\"" ^ s2 ^ "\""


let removeQuotes (s : string) : string =
  if String.endsWith ~suffix:"\"" s && String.startsWith ~prefix:"\"" s
  then s |> String.dropLeft ~count:1 |> String.dropRight ~count:1
  else s
