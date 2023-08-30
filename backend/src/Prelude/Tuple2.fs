module Tuple2

let fromKeyValuePair
  (kvp : System.Collections.Generic.KeyValuePair<'a, 'b>)
  : ('a * 'b) =
  kvp.Key, kvp.Value

let toKeyValuePair
  ((k, v) : 'a * 'b)
  : (System.Collections.Generic.KeyValuePair<'a, 'b>) =
  System.Collections.Generic.KeyValuePair<'a, 'b>(k, v)

let first (v1 : 'a, _ : 'b) : 'a = v1
let second (_ : 'a, v2 : 'b) : 'b = v2
let mapFirst (f : 'x -> 'r) (x : 'x, y : 'y) : 'r * 'y = (f x, y)
let mapSecond (f : 'y -> 'r) (x : 'x, y : 'y) : 'x * 'r = (x, f y)
