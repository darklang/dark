/// Functions for working with functions.
module Tablecloth.Fun

let identity a = a

let ignore _ = ()

let constant a _ = a

let sequence _ b = b

let flip f x y = f y x

let negate f t = not (f t)

let apply f a = f a

let (<|) a b = a b

let pipe (v : 'a) (f : 'a -> 'b) : 'b = f v

let (|>) (v : 'a) (f : 'a -> 'b) : 'b = f v

let compose g f a = g (f a)

let (<<) a b = compose a b

let composeRight g f a = f (g a)

let (>>) a b = composeRight a b

let tap f a =
  f a
  a


let rec times n f =
  if n <= 0 then
    ()
  else
    (f ()
     times (n - 1) f)


let forever f =
  try
    while true do
      f ()

    failwith "[while true] managed to return, you are in trouble now."
  with
  | exn -> exn


let curry (f : 'a * 'b -> 'c) a b = (f (a, b) : 'c)

let uncurry (f : 'a -> 'b -> 'c) ((a, b) : 'a * 'b) = (f a b : 'c)

let curry3 f a b c = f (a, b, c)

let uncurry3 f (a, b, c) = f a b c
