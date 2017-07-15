module Map = Core.Map.Poly

(* Functions defs *)
type fn = {name : string;
           parameters : string list}
type fnmap = (string, fn) Map.t


(* Short list *)
type shortfn = {n : string;
                p : string list}

(* Add a short function to the map *)
let add_fn (m : fnmap) (fn : shortfn) : fnmap =
  Map.add m fn.n {name = fn.n; parameters = fn.p}

let fns_list = [
  { n = "Page_page"
  ; p = ["url"; "outputs"]}
]


(* Give access to other modules *)
let fns : fnmap =
  List.fold_left add_fn Map.empty fns_list

let get_fn (name : string) : fn =
  match Map.find fns name with
  | Some f -> f
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.UserException |> raise
