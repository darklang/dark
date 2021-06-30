module Tablecloth.List

type 'a t = 'a list

module List = FSharp.Collections.List

let empty = List.empty

let singleton v = [ v ]

let repeat times element = List.replicate times element

let rec range from to_ = if from >= to_ then [] else from :: range (from + 1) to_

let initialize count f = List.init count f

let inline sum (list : 'a list) = List.sum list

let fromArray l = List.ofArray l

let from_array l = fromArray l

let isEmpty (l : 'a list) = List.isEmpty l

let is_empty l = isEmpty l

let head l = List.tryHead l

let tail l =
  match l with
  | [] -> None
  | _ :: tail -> Some tail

let cons element list = element :: list

let take count t = List.take count t

let takeWhile (f : 'a -> bool) (l : 'a list) = List.takeWhile f l

let take_while f l = takeWhile f l

let drop count t = List.skip count t

let rec dropWhile (f : 'a -> bool) (l : 'a list) = List.skipWhile f l

let drop_while f l = dropWhile f l

let initial (l : 'a list) =
  (match List.rev l with
   | [] -> None
   | _ :: rest -> Some(List.rev rest) : 'a list option)


let rec last (l : 'a list) = List.tryLast l


let append (l1 : 'a list) (l2 : 'a list) = List.append l1 l2

let flatten (l : 'a list list) : 'a list = List.concat l

let map2 f l1 l2 = List.map2 f l1 l2

let map3 f l1 l2 l3 = List.map3 f l1 l2 l3

let reverse (l : 'a list) : 'a list = List.rev l

let map f l = List.map f l

let mapWithIndex f l = List.mapi f l

let map_with_index f l = mapWithIndex f l

let flatMap f l = List.collect f l

let flat_map f l = flatMap f l

let includes elem l = List.contains elem l

let find f l = List.tryFind f l

// Implementation adapted from https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/list.fs
let findIndex f l =
  let rec loop n list =
    match list with
    | [] -> None
    | h :: t -> if f n h then Some(n, h) else loop (n + 1) t

  loop 0 l

let find_index f l = findIndex f l

let any f l = List.exists f l

let all f l = List.forall f l

let getAt (index : int) (l : 'a list) = (List.tryItem index l : 'a option)

let get_at index l = getAt index l

let filterMap f l = List.choose f l

let filter_map f l = filterMap f l

let filter f l = List.filter f l

let filterWithIndex f l =
  l |> List.indexed |> List.choose (fun (k, v) -> if f k v then Some v else None)

let filter_with_index f l = filterWithIndex f l

let partition f l = List.partition f l

let fold initial f l = List.fold f initial l

let count f l = List.filter f l |> List.length

let foldRight initial f l = List.foldBack (fun item accum -> f accum item) l initial

let fold_right initial f l = foldRight initial f l

let splitAt (i : int) (l : 'a list) = List.splitAt i l

let split_at i l = splitAt i l

let splitWhen (f : 'a -> bool) (l : 'a list) =
  (match findIndex (fun _ element -> f element) l with
   | Some (index, _) -> splitAt index l
   | None -> (l, []) : 'a list * 'a list)

let split_when f l = splitWhen f l

let updateAt (index : int) (f : 'a -> 'a) (l : 'a list) =
  (if index < 0 then
     l
   else
     let front, back = splitAt index l in

     match back with
     | [] -> l
     | x :: rest -> append front (f x :: rest) : 'a list)

let update_at i f l = updateAt i f l

let length (l : 'a list) = List.length l

let removeAt (index : int) (l : 'a list) =
  (if index < 0 then
     l
   else
     let front, back = splitAt index l in

     match tail back with
     | None -> l
     | Some t -> append front t : 'a list)

let remove_at i l = removeAt i l

let minimum l = if length l = 0 then None else Some(List.min l)

let maximum l = if length l = 0 then None else Some(List.max l)

let extent l =
  fold
    None
    (fun current element ->
      match current with
      | None -> Some(element, element)
      | Some (min, max) ->
          Some(
            (match compare element min < 0 with
             | true -> element
             | false -> min),
            match compare element max > 0 with
            | true -> element
            | false -> max
          ))
    l

let insertAt (index : int) (value : 'a) (l : 'a list) =
  (let front, back = splitAt index l in
   append front (value :: back) : 'a list)

let insert_at i v l = insertAt i v l

let zip listA listB =
  let rec loop result xs ys =
    match (xs, ys) with
    | [], _ -> result
    | _, [] -> result
    | x :: xs, y :: ys -> loop ((x, y) :: result) xs ys

  loop [] listA listB


let unzip l = List.unzip l

let sliding (step : int) (size : int) (l : 'a t) =
  (let rec takeAllOrEmpty t n (current, count) =
    if count = n then
      reverse current
    else
      match t with
      | [] -> []
      | x :: xs -> takeAllOrEmpty xs n (x :: current, count + 1)

   let rec loop t =
     if isEmpty t then
       []
     else
       let sample = takeAllOrEmpty t size ([], 0) in
       if isEmpty sample then [] else sample :: loop (List.skip step l)

   loop l : 'a t t)

let chunksOf size l = sliding size size l

let chunks_of size l = chunksOf size l

let intersperse sep l =
  (match l with
   | [] -> []
   | [ x ] -> [ x ]
   | x :: rest -> x :: foldRight [] (fun acc x -> sep :: x :: acc) rest : 'a list)

let forEach f l = List.iter f l

let for_each f l = forEach f l

let forEachWithIndex f l = List.iteri f l

let for_each_with_index f l = forEachWithIndex f l

let toArray l = List.toArray l

let to_array l = toArray l

let rec groupWhile f l =
  let span f l =
    match l with
    | [] -> ([], [])
    | _ -> (takeWhile f l, dropWhile f l)

  match l with
  | [] -> []
  | x :: rest ->
      let ys, zs = span (f x) rest in
      (x :: ys) :: groupWhile f zs


let group_while f l = groupWhile f l

let sort l = List.sort l

let join (sep : string) (l : string list) = String.concat sep l

let groupBy f l = List.groupBy f l |> Map

let group_by f l = groupBy f l

let rec equal equalElement (a : 'a list) (b : 'a list) : bool =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys -> equalElement x y && equal equalElement xs ys
  | _ -> false

let rec compare compareElement (a : 'a list) (b : 'a list) : int =
  match (a, b) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
      (match compareElement x y with
       | 0 -> compare compareElement xs ys
       | result -> result)
