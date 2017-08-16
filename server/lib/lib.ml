open Core
open Runtime

let req name tipe = { name = name
                    ; tipe = tipe
                    ; optional = false
                    }
let opt name tipe = { name = name
                    ; tipe = tipe
                    ; optional = true
                    }



(* Shorthand *)
type shortfn = { n : string
               ; o : string list
               ; p : param list
               ; r : tipe
               ; f : (dval list) -> dval
               }
(* TODO: use deriving here instead *)
let expected (msg : string) (args : dval list) : dval =
  args
  |> List.map ~f:to_error_repr
  |> String.concat ~sep:", "
  |> (^) ("Expected: " ^ msg ^ ", got: ")
  |> Exception.raise

let fail (args: dval list) : dval =
  raise (Runtime.TypeError args)
