module Tablecloth.Tuple2

// Functions for manipulating tuples of length two
type ('a, 'b) t = 'a * 'b

let make a b = (a, b)

let fromArray array =
  match array with
  | [||]
  | [| _ |] -> None
  | [| a; b |] -> Some(a, b)
  | _ -> None


let from_array a = fromArray a

let fromList list =
  match list with
  | []
  | [ _ ] -> None
  | a :: b :: _rest -> Some(a, b)


let from_list l = fromList l

let first (a, _) = a

let second (_, b) = b

let mapFirst f (a, b) = (f a, b)

let map_first f t = mapFirst f t

let mapSecond f (a, b) = (a, f b)

let map_second f t = mapSecond f t

let mapEach f g (a, b) = (f a, g b)

let map_each f g t = mapEach f g t

let mapAll f (a1, a2) = (f a1, f a2)

let map_all f t = mapAll f t

let swap (a, b) = (b, a)

let toArray (a, b) = [| a; b |]

let to_array t = toArray t

let toList (a, b) = [ a; b ]

let to_list t = toList t

let equal
  (equalFirst : 'a -> 'a -> bool)
  (equalSecond : 'b -> 'b -> bool)
  (a, b)
  (a', b')
  =
  equalFirst a a' && equalSecond b b'


let compare
  (compareFirst : 'a -> 'a -> int)
  (compareSecond : 'b -> 'b -> int)
  (a, b)
  (a', b')
  =
  match compareFirst a a' with
  | 0 -> compareSecond b b'
  | result -> result
