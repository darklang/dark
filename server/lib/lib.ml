open Core
open Runtime

let req name tipe : param =
  { name = name
  ; tipe = tipe
  ; optional = false
  ; anon_args = []
  ; description = ""
  }
let opt name tipe : param =
  { name = name
  ; tipe = tipe
  ; anon_args = []
  ; optional = true
  ; description = ""
  }

let func args : param =
  { name = "f"
  ; tipe = TFun
  ; anon_args = args
  ; optional = false
  ; description = "" }


(* Shorthand *)
type shortfn = { n : string
               ; o : string list
               ; p : param list
               ; r : tipe
               ; f : ccfunc
               ; d : string
               ; pr : (dval list -> int -> dval list list) option
               ; pu : bool
               }

let fail (args: dval list) : 'a =
  raise (Runtime.TypeError args)
