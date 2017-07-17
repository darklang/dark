open Runtime

module Map = Core.Map.Poly

(* Functions defs *)
type fn = { name : string
          ; parameters : string list
          ; fn : (dval list) -> dval
          }
type fnmap = (string, fn) Map.t


(* Short list *)
type shortfn = { n : string
               ; p : string list
               ; f : (dval list) -> dval
               }

(* Add a short function to the map *)
let add_fn (m : fnmap) (s : shortfn) : fnmap =
  Map.add m s.n {name = s.n; parameters = s.p; fn = s.f}

let fns_list = [
  { n = "Page_page"
  ; p = ["url"; "outputs"]
  ; f = fun [DInt a; DInt b] -> a + b |> DInt
  }
  ;
  { n = "Int_add"
  ; p = ["a"; "b"]
  ; f = fun [DInt a; DInt b] -> a + b |> DInt
  }
]


(* Give access to other modules *)
let fns : fnmap =
  List.fold_left add_fn Map.empty fns_list

let get_fn (name : string) : fn =
  match Map.find fns name with
  | Some f -> f
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.UserException |> raise
