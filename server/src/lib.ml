open Runtime


(* Shorthand *)
type shortfn = { n : string
               ; p : string list
               ; f : (dval list) -> dval
               }

let fns_list = [
  { n = "Page_page"
  ; p = ["url"; "outputs"]
  ; f = function
      | _ -> DStr "todo: implement Page_page"
  }
  ;
  { n = "Int_add"
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a + b |> DInt
      | _ -> Exception.raise "Expected 2 ints"
  }
  ;
  { n = "String_map"
  ; p = ["s"; "f"]
  ; f = function
      | [DStr s; DFn fn] ->
        let charf (c : char) : char =
          to_char @@ fn [DChar c] in
        String.map charf s |> DStr
      | _ -> Exception.raise "Expected a string and a function"
  }
  ;
  { n = "Char_inc"
  ; p = ["c"]
  ; f = function
      | [DChar c] -> c |> Char.code |> (+) 1 |> Char.chr |> DChar
      | _ -> Exception.raise "Expected a char"
  }
]

let fns : fnmap =
  let add_fn (m : fnmap) (s : shortfn) : fnmap =
    Map.add m s.n {name = s.n; parameters = s.p; fn = s.f} in
  List.fold_left add_fn Map.empty fns_list

(* Give access to other modules *)
let get_fn (name : string) : fn =
  match Map.find fns name with
  | Some f -> f
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.raise
