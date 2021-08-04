module Tablecloth.Array

// A fixed length collection of values

type 'a t = 'a array

let singleton (a : 'a) = ([| a |] : 'a array)

let clone a = Array.copy a

let initialize (f : int -> 'a) (length : int) =
  if length <= 0 then [||] else Array.init length f


let repeat element length = Array.create length element


let range (from : int) (``to`` : int) =
  initialize (fun i -> i + from) (max 0 (``to`` - from))


let fromList l = Array.ofList l

let from_list l = fromList l

let length (a : 'a array) = Array.length a

let isEmpty a = length a = 0

let is_empty a = isEmpty a

let first t = if length t < 1 then None else Some t.[0]

let last t = if length t < 1 then None else Some t.[length t - 1]

let get i a = Array.get a i

let getAt i a = if i >= 0 && i < length a then Some(Array.get a i) else None

let get_at i a = getAt i a

// let ( .?() ) (array : 'element t) (index : int) : 'element option =
//   getAt array ~index

let set i v a = Array.set a i v

let setAt i v a = set i v a

let set_at i v a = setAt i v a

let filter f a = Array.filter f a

let inline sum (a : 'a array) : 'a = Array.sum a

let filterMap f a = Array.choose f a

let filter_map f a = filterMap f a

let flatMap f a = Array.collect f a

let flat_map f a = flatMap f a

let fold initial f a = Array.fold f initial a

let foldRight i f a = Array.foldBack (fun v accum -> f accum v) a i

let fold_right i f a = foldRight i f a

let count f a =
  fold
    0
    (fun total element ->
      total
      + match f element with
        | true -> 1
        | false -> 0)
    a

let swap (i : int) (j : int) (a : 'a array) =
  let temp = a.[i]
  a.[i] <- a.[j]
  a.[j] <- temp
  ()

let find f a =
  try
    Some(Array.find f a)
  with
  | _ -> None

let findIndex (f : int -> 'a -> bool) (a : 'a array) : (int * 'a) option =
  let mutable i = -1

  find
    (fun (v : 'a) ->
      i <- i + 1
      f i v)
    a
  |> Option.map (fun v -> (i, v))


let find_index f a = findIndex f a

let map f a = Array.map f a

let mapWithIndex f a = Array.mapi f a

let map_with_index f a = mapWithIndex f a

let map2 (f : 'a -> 'b -> 'c) (a : 'a array) (b : 'b array) =
  let minLength = min (length a) (length b) in

  Array.init minLength (fun i -> f a.[i] b.[i])


let zip a b = map2 (fun left right -> (left, right)) a b

let map3
  (f : 'a -> 'b -> 'c -> 'd)
  (arrayA : 'a array)
  (arrayB : 'b array)
  (arrayC : 'c array)
  =
  let minLength = min (length arrayA) (min (length arrayC) (length arrayB))
  Array.init minLength (fun i -> f arrayA.[i] arrayB.[i] arrayC.[i])


let partition f a = Array.partition f a

let splitAt i a = Array.splitAt i a

let split_at i a = splitAt i a

let splitWhen f a =
  match findIndex (fun _index element -> f element) a with
  | None -> (a, [||])
  | Some (index, _) -> splitAt index a


let split_when f a = splitWhen f a

let unzip a = Array.unzip a

let append (a1 : 'a array) (a2 : 'a array) = Array.append a1 a2

let flatten (``as`` : 'a array array) = Array.concat ``as``

let intersperse sep array =
  initialize
    (fun i -> if i % 2 <> 0 then sep else get (i / 2) array)
    (max 0 ((Array.length array * 2) - 1))


let any f a = Array.exists f a

let all f a = Array.forall f a

let includes v a = Array.contains v a

let values a =
  (fold
    []
    (fun results element ->
      match element with
      | None -> results
      | Some value -> value :: results)
    a)
  |> fromList


let join sep (a : string t) = String.concat sep a

let groupBy (f : 'v -> 'k) (a : 'v array) : Map<'k, 'v list> =
  fold
    Map.empty
    (fun map element ->
      let key = f element in

      Tablecloth.Map.update
        key
        (function
        | None -> Some [ element ]
        | Some elements -> Some(element :: elements))
        map)
    a


let group_by f a = groupBy f a

let slice ``to`` from array =
  let sliceFrom =
    if from >= 0 then
      min (length array) from
    else
      max 0 (min (length array) (length array + from))

  let sliceTo =
    if ``to`` >= 0 then
      min (length array) ``to``
    else
      max 0 (min (length array) (length array + ``to``))

  if sliceFrom >= sliceTo then
    [||]
  else
    Array.init (sliceTo - sliceFrom) (fun i -> array.[i + sliceFrom])


let sliding step size a =
  let n = Array.length a in

  if size > n then
    [||]
  else
    initialize
      (fun i -> initialize (fun j -> a.[(i * step) + j]) size)
      (1 + ((n - size) / step))


let chunksOf size a = sliding size size a

let chunks_of s a = chunksOf s a

let maximum a = if length a = 0 then None else Some(Array.max a)

let minimum a = if length a = 0 then None else Some(Array.min a)

let extent a =
  fold
    None
    (fun range element ->
      match range with
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
    a


let sort a = Array.sortInPlace a

let reverse (a : 'a array) : unit = System.Array.Reverse a

let forEach f a = Array.iter f a

let for_each f a = forEach f a

let forEachWithIndex f a = Array.iteri f a

let for_each_with_index f a = forEachWithIndex f a

let toList (a : 'a array) = Array.toList a

let to_list a = toList a

let toIndexedList a = a |> Array.indexed |> Array.toList

let to_indexed_list a = toIndexedList a

let equal (f : 'a -> 'a -> bool) a b =
  if length a <> length b then
    false
  else if length a = 0 then
    true
  else
    let rec loop index =
      if index = length a then true else f a.[index] b.[index] && loop (index + 1)

    loop 0

let compare (f : 'a -> 'a -> int) a b =
  match Int.compare (length a) (length b) with
  | 0 ->
    if length a = 0 then
      0
    else
      let rec loop index =
        if index = length a then
          0
        else
          match f a.[index] b.[index] with
          | 0 -> loop (index + 1)
          | result -> result

      loop 0
  | result -> result
