module Tablecloth.Option

// Functions for working with optional values.

type 'a t = 'a option

let some a = Some a

let isSome o = Option.isSome o

let is_some o = isSome o

let isNone o = Option.isNone o

let is_none o = isNone o

let and_ (ta : 'a option) (tb : 'a option) : 'a option =
  match isSome ta with
  | true -> tb
  | false -> None

let or_ ta tb =
  match isSome ta with
  | true -> ta
  | false -> tb

let orElse ta tb =
  match isSome tb with
  | true -> tb
  | false -> ta

let or_else o1 o2 = orElse o1 o2

let andThen f t =
  match t with
  | Some x -> f x
  | None -> None

let and_then f t = andThen f t

let flatten o = Option.flatten o

let both a b =
  match (a, b) with
  | Some a, Some b -> Some(a, b)
  | _ -> None

let map f t = Option.map f t

let map2 (f : 'a -> 'b -> 'c) (ta : 'a t) (tb : 'b t) =
  (match (ta, tb) with
   | Some a, Some b -> Some(f a b)
   | _ -> None : 'c t)


let unwrap ``default`` t =
  match t with
  | None -> ``default``
  | Some value -> value

let unwrapUnsafe x =
  match x with
  | None -> invalidArg "option" "Option.unwrapUnsafe called with None"
  | Some x -> x


let unwrap_unsafe o = unwrapUnsafe o

let tap f t = Option.iter f t

let toArray t =
  match t with
  | None -> [||]
  | Some value -> [| value |]

let to_array o = toArray o

let toList t =
  match t with
  | None -> []
  | Some value -> [ value ]

let to_list o = toList o

let equal (equal : 'a -> 'a -> bool) (a : 'a option) (b : 'a option) : bool =
  match (a, b) with
  | None, None -> true
  | Some a', Some b' -> equal a' b'
  | _ -> false


let compare (compare : 'a -> 'a -> int) a b =
  match (a, b) with
  | None, None -> 0
  | Some a', Some b' -> compare a' b'
  | None, Some _ -> -1
  | Some _, None -> 1


let (|?) (t : 'a option) (def : 'a) : 'a = unwrap def t

let (>>|) t f = map f t

let (>>=) t f = andThen f t
