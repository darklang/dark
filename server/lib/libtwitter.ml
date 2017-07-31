open Core
open Runtime

open Lib

let fns : shortfn list = [
  { n = "Twitter::get"
  ; o = []
  ; p = ["api"; "arguments"]
  ; f = function
      | [DStr url; arg] -> Twitter.get url arg
      | args -> expected "2 ints" args
  }
  ;
  { n = "Twitter::post"
  ; o = []
  ; p = ["api"; "arguments"]
  ; f = function
      | [DStr url; arg] -> Twitter.post url arg
      | args -> expected "2 ints" args
  }
]
