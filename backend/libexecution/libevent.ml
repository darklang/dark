open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["emit"]
    ; ins = []
    ; p = [par "Data" TAny; par "Space" TStr; par "Name" TStr]
    ; r = TAny
    ; d = "Emit event `name` in `space`, passing along `data` as a parameter"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false } ]
