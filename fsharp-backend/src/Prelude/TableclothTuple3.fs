module Tablecloth.Tuple3

// Functions for manipulating tuples of length three

type ('a, 'b, 'c) t = 'a * 'b * 'c

let make a b c = (a, b, c)

let fromArray array =
  match array with
  | [||]
  | [| _ |]
  | [| _; _ |] -> None
  | [| a; b; c |] -> Some(a, b, c)
  | _ -> None


let fromList list =
  match list with
  | []
  | [ _ ]
  | [ _; _ ] -> None
  | a :: b :: c :: _rest -> Some(a, b, c)


let first (a, _, _) = a

let second (_, b, _) = b

let third (_, _, c) = c

let initial (a, b, _) = (a, b)

let tail (_, b, c) = (b, c)

let mapFirst f (a, b, c) = (f a, b, c)

let mapSecond f (a, b, c) = (a, f b, c)

let mapThird f (a, b, c) = (a, b, f c)

let mapEach f g h (a, b, c) = (f a, g b, h c)

let mapAll f (a1, a2, a3) = (f a1, f a2, f a3)

let rotateLeft (a, b, c) = (b, c, a)

let rotateRight (a, b, c) = (c, a, b)

let toArray (a, b, c) = [| a; b; c |]

let toList (a, b, c) = [ a; b; c ]

let equal
  (equalFirst : 'a -> 'a -> bool)
  (equalSecond : 'b -> 'b -> bool)
  (equalThird : 'c -> 'c -> bool)
  (a, b, c)
  (a', b', c')
  =
  equalFirst a a' && equalSecond b b' && equalThird c c'

let compare
  (compareFirst : 'a -> 'a -> int)
  (compareSecond : 'b -> 'b -> int)
  (compareThird : 'c -> 'c -> int)
  (a, b, c)
  (a', b', c')
  =
  match compareFirst a a' with
  | 0 ->
    (match compareSecond b b' with
     | 0 -> compareThird c c'
     | result -> result)
  | result -> result
