open Core
open Runtime

let req name tipe = { name = name
                    ; tipe = tipe
                    ; optional = false
                    ; arity = 0
                    ; description = ""
                    }
let opt name tipe = { name = name
                    ; tipe = tipe
                    ; arity = 0
                    ; optional = true
                    ; description = ""
                    }

let func arity = { name = "f"
                 ; tipe = TFun
                 ; arity = arity
                 ; optional = false
                 ; description = "" }



(* Shorthand *)
type shortfn = { n : string
               ; o : string list
               ; p : param list
               ; r : tipe
               ; f : ccfunc
               ; d : string
               ; pr : (dval list -> dval list) option
               ; pu : bool
               }

let fail (args: dval list) : 'a =
  raise (Runtime.TypeError args)
