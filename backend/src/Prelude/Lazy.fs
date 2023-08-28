module Lazy

let inline force (l : Lazy<_>) = l.Force()
let map f l = lazy ((f << force) l)
let bind f l = lazy ((force << f << force) l)
