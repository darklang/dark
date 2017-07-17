open Runtime


(* Shorthand *)
type shortfn = { n : string
               ; p : string list
               ; f : (dval list) -> dval
               }
(* TODO: use deriving here instead *)
let expected (msg : string) (args : dval list) : dval =
  args
  |> List.map to_error_repr
  |> String.concat ", "
  |> Util.string_append ("Expected: " ^ msg ^ ", got: ")
  |> Exception.raise



let fns_list = [
  { n = "Page_page"
  ; p = ["url"; "outputs"]
  ; f = function
      | args -> expected "this to be implmented" args
  }
  ;
  { n = "Int_add"
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a + b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  { n = "String_map"
  ; p = ["s"; "f"]
  ; f = function
      | [DStr s; DFn fn] ->
        let charf (c : char) : char =
          to_char @@ fn.func [DChar c] in
        String.map charf s |> DStr
      | args -> expected "a strint and a function" args
  }
  ;
  { n = "Char_inc"
  ; p = ["c"]
  ; f = function
      | [DChar c] -> c |> Char.code |> (+) 1 |> Char.chr |> DChar
      | args -> expected "a char" args
  }
]

type fnmap = (string, fn) Map.t
let fns : fnmap =
  let add_fn (m : fnmap) (s : shortfn) : fnmap =
    Map.add m s.n
      {name = s.n; parameters = s.p; func = s.f; partials = []} in
  List.fold_left add_fn Map.empty fns_list

(* Give access to other modules *)
let get_fn (name : string) : fn =
  match Map.find fns name with
  | Some f -> f
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.raise
