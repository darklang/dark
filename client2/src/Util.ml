let windowSize a =
  let size = Port.windowSize a in
  (size.width, size.height - 45)

let random a = Port.random a

let htmlSize str =
  let size = Port.size_size str in
  (size.width, size.height)

let toIntWithDefault d s = s |> String.toInt |> Result.withDefault d

let reContains re s = Regex.contains (Regex.regex re) s

let reExactly re s = reContains ("^" ++ re ++ "$") s

let replace re repl str =
  Regex.replace Regex.All (Regex.regex re) (fun _ -> repl) str

let findIndex fn l =
  Port.getBy (fun (_, a) -> fn a) (List.indexedMap to_tuple2 l)

let zip a b = List.map2 to_tuple2 a b

let resultIsOk r = match r with Ok _ -> true | Err _ -> false

let int2letter i =
  'a' |> Char.toCode |> ( + ) i |> Char.fromCode |> String.fromChar

let letter2int s =
  s |> String.uncons
  |> Maybe.withDefault ('!', "")
  |> Tuple.first |> Char.toCode
  |> ( - ) (Char.toCode 'a')
  |> ( * ) (-1)

let combineResult results =
  List.foldl
    (fun r rs ->
      match (r, rs) with
      | Ok a, Ok bs -> Ok (a :: bs)
      | Err a, Ok _ -> Err [a]
      | Ok _, Err bs -> Err bs
      | Err a, Err bs -> Err (a :: bs) )
    (Ok []) results

let listContainsOrdered needle haystack =
  match (needle, haystack) with
  | [], _ -> true
  | _, [] -> false
  | [ns; n], [hs; h] ->
      if n == h then listContainsOrdered ns hs
      else listContainsOrdered (n :: ns) hs

let stringContainsOrdered needle haystack =
  match String.uncons needle with
  | Just (c, newneedle) ->
      let char = String.fromChar c in
      String.contains char haystack
      && stringContainsOrdered newneedle
           (haystack |> String.split char |> List.drop 1 |> String.join char)
  | Nothing -> true

let uniqueCombinations xs =
  xs |> LE.tails |> zip xs
  |> List.concatMap (fun (x, ys) -> List.map (fun y -> (x, y)) ys)
  |> List.filter (fun (x, y) -> x /= y)

let listPrevious a l =
  l |> LE.elemIndex a
  |> Maybe.map (fun x -> x - 1)
  |> Maybe.andThen (fun i -> LE.getAt i l)

let listNext a l =
  l |> LE.elemIndex a
  |> Maybe.map (fun x -> x + 1)
  |> Maybe.andThen (fun i -> LE.getAt i l)

let listPreviousWrap a l = l |> listPrevious a |> Port.optionOrElse (LE.last l)

let listNextWrap a l = l |> listNext a |> Port.optionOrElse (List.head l)

let cacheSet k v = Port.cache_set k v

let cacheGet k = Port.cache_get k

let cacheClear k = Port.cache_clear k

let transformToStringEntry s_ =
  let s = if String.endsWith "\"" s_ then s_ else s_ ++ "\"" in
  s |> String.dropLeft 1 |> String.dropRight 1 |> replace "\\\\\"" "\""

let transformFromStringEntry s =
  let s2 = s |> replace "\"" "\\\"" in
  "\"" ++ s2 ++ "\""
