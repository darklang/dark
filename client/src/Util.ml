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


let listNextWrap ~(value : 'a) (l : 'a list) : 'a option =
  l |> listNext ~value |> Option.orElse (List.head l)


let removeQuotes (s : string) : string =
  if String.endsWith ~suffix:"\"" s && String.startsWith ~prefix:"\"" s
  then s |> String.dropLeft ~count:1 |> String.dropRight ~count:1
  else s


let humanReadableTimeElapsed (time : float) : string =
  let msPerMinute = 60.0 *. 1000.0 in
  let msPerHour = msPerMinute *. 60.0 in
  let msPerDay = msPerHour *. 24.0 in
  let msPerMonth = msPerDay *. 30.0 in
  let msPerYear = msPerDay *. 365.0 in
  let rec f time =
    if time /. msPerYear > 1.0
    then
      let suffix = if time /. msPerYear > 2.0 then "years" else "year" in
      ( (time /. msPerYear |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerYear)
    else if time /. msPerMonth > 1.0
    then
      let suffix = if time /. msPerMonth > 2.0 then "months" else "month" in
      ( (time /. msPerMonth |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerMonth)
    else if time /. msPerDay > 1.0
    then
      let suffix = if time /. msPerDay > 2.0 then "days" else "day" in
      ( (time /. msPerDay |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerDay)
    else if time /. msPerHour > 1.0
    then
      let suffix = if time /. msPerHour > 2.0 then "hours" else "hour" in
      ( (time /. msPerHour |> int_of_float |> string_of_int)
      ^ " "
      ^ suffix
      ^ ", " )
      ^ f (mod_float time msPerHour)
    else if time /. msPerMinute > 1.0
    then
      let suffix = if time /. msPerMinute > 2.0 then "minutes" else "minute" in
      ((time /. msPerMinute |> int_of_float |> string_of_int) ^ " " ^ suffix)
      ^ f (mod_float time msPerMinute)
    else ""
  in
  let diff = f time in
  if diff = "" then "less than a minute" else diff


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
