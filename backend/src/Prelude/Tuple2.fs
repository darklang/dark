module Tuple2

let fromKeyValuePair
  (kvp : System.Collections.Generic.KeyValuePair<'a, 'b>)
  : ('a * 'b) =
  kvp.Key, kvp.Value

let toKeyValuePair
  ((k, v) : 'a * 'b)
  : (System.Collections.Generic.KeyValuePair<'a, 'b>) =
  System.Collections.Generic.KeyValuePair<'a, 'b>(k, v)

let first (a, b) = a
let second (a, b) = b

