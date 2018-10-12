open Tea
open! Porting

let windowSize (a : unit) : int * int =
  let size = Native.Window.size a in
  (size.width, size.height - 45)

let random (a : unit) : int = Native.Random.random a

let toIntWithDefault (d : int) (s : string) : int =
  s |> String.toInt |> Result.withDefault d

let reContains (re : string) (s : string) : bool =
  Regex.contains (Regex.regex re) s

let reExactly (re : string) (s : string) : bool = reContains ("^" ^ re ^ "$") s

let findIndex (fn : 'a -> bool) (l : 'a list) : (int * 'a) option =
  List.find (fun (_, a) -> fn a) (List.indexedMap Tuple2.create l)

let listPrevious (a : 'a) (l : 'a list) : 'a option =
  l |> List.elemIndex a
  |> Option.map (fun x -> x - 1)
  |> Option.andThen (fun i -> List.getAt i l)

let listNext (a : 'a) (l : 'a list) : 'a option =
  l |> List.elemIndex a
  |> Option.map (fun x -> x + 1)
  |> Option.andThen (fun i -> List.getAt i l)

let listPreviousWrap (a : 'a) (l : 'a list) : 'a option =
  l |> listPrevious a |> Option.orElse (List.last l)

let listNextWrap (a : 'a) (l : 'a list) : 'a option =
  l |> listNext a |> Option.orElse (List.head l)

let cacheSet (k : 'k) (v : 'v) : 'v option = Native.Cache.set k v

let cacheGet (k : 'k) : 'v = Native.Cache.get k

let cacheClear (k : 'k) : unit = Native.Cache.clear k

let transformToStringEntry (s_ : string) : string =
  let s = if String.endsWith "\"" s_ then s_ else s_ ^ "\"" in
  s |> String.dropLeft 1 |> String.dropRight 1 |> Regex.replace "\\\\\"" "\""

let transformFromStringEntry (s : string) : string =
  let s2 = s |> Regex.replace "\"" "\\\"" in
  "\"" ^ s2 ^ "\""
