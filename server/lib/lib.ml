open Core
open Runtime


(* Shorthand *)
type shortfn = { n : string
               ; o : string list
               ; p : string list
               ; f : (dval list) -> dval
               }
(* TODO: use deriving here instead *)
let expected (msg : string) (args : dval list) : dval =
  args
  |> List.map ~f:to_error_repr
  |> String.concat ~sep:", "
  |> (^) ("Expected: " ^ msg ^ ", got: ")
  |> Exception.raise

