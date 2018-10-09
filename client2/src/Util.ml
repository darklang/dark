open Belt
open Tea
open! Porting

let windowSize a =
  let size = Native.Window.size a in
  (size.width, size.height - 45)

let random a = Native.Random.random a

let toIntWithDefault d s = s |> int_of_string |> Result.getWithDefault d

let reContains re s = Regex.contains (Regex.regex re) s

let reExactly re s = reContains (("^" ^ re) ^ "$") s

let findIndex fn l =
  List.getBy (fun (_, a) -> fn a) (List.indexedMap Tuple2.create l)

let listPrevious a l =
  l |> List.elemIndex a
  |> Option.map (fun x -> x - 1)
  |> Option.andThen (fun i -> List.get i l)

let listNext a l =
  l |> List.elemIndex a
  |> Option.map (fun x -> x + 1)
  |> Option.andThen (fun i -> List.get i l)

let listPreviousWrap a l = l |> listPrevious a |> Option.orElse (List.last l)

let listNextWrap a l = l |> listNext a |> Option.orElse (List.head l)

let cacheSet k v = Native.Cache.set k v

let cacheGet k = Native.Cache.get k

let cacheClear k = Native.Cache.clear k

let transformToStringEntry s_ =
  let s = if String.endsWith "\"" s_ then s_ else s_ ^ "\"" in
  s |> String.dropLeft 1 |> String.dropRight 1 |> Regex.replace "\\\\\"" "\""

let transformFromStringEntry s =
  let s2 = s |> Regex.replace "\"" "\\\"" in
  ("\"" ^ s2) ^ "\""
