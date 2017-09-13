open Core
open Runtime

let req name tipe = { name = name
                    ; tipe = tipe
                    ; optional = false
                    ; description = ""
                    }
let opt name tipe = { name = name
                    ; tipe = tipe
                    ; optional = true
                    ; description = ""
                    }



(* Shorthand *)
type shortfn = { n : string
               ; o : string list
               ; p : param list
               ; r : tipe
               ; f : ccfunc
               ; d : string
               ; pr : (dval list -> dval) option
               }

let fail (args: dval list) : 'a =
  raise (Runtime.TypeError args)
