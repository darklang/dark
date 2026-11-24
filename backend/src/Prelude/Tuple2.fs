module Tuple2

let first (v1 : 'a, _ : 'b) : 'a = v1
let second (_ : 'a, v2 : 'b) : 'b = v2
