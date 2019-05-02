open Tc

let random (a : unit) : int = Native.Random.random a

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


module Regex = struct
  type t = Js.Re.t

  type result = Js.Re.result

  let regex s : Js.Re.t = Js.Re.fromStringWithFlags ~flags:"g" s

  let contains ~(re : Js.Re.t) (s : string) : bool = Js.Re.test_ re s

  let replace ~(re : Js.Re.t) ~(repl : string) (str : string) =
    Js.String.replaceByRe re repl str


  let matches ~(re : Js.Re.t) (s : string) : Js.Re.result option =
    Js.Re.exec_ re s


  let exactly ~(re : string) (s : string) : bool =
    contains ~re:(regex ("^" ^ re ^ "$")) s
end
