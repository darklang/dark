open Core
open Runtime


(* Shorthand *)
type shortfn = { n : string
               ; p : string list
               ; f : (dval list) -> dval
               }
(* TODO: use deriving here instead *)
let expected (msg : string) (args : dval list) : dval =
  args
  |> List.map ~f:to_error_repr
  |> String.concat ~sep:", "
  |> Util.string_append ("Expected: " ^ msg ^ ", got: ")
  |> Exception.raise



let fns_list = [
  { n = "Page_page"
  ; p = ["url"; "outputs"]
  ; f = function
      | args -> expected "this to be implmented" args
  }
  ;
  { n = "Int_mod"
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a mod b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  { n = "Int_add"
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a + b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  { n = "Int_sub"
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a - b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  (* { n = "String_map" *)
  (* ; p = ["s"; "f"] *)
  (* ; f = function *)
  (*     | [DStr s; DFn fn] -> *)
  (*       let charf (c : char) : char = *)
  (*         fn.func [DChar c] |> to_char in *)
  (*       s |> String.map charf |> DStr *)
  (*     | args -> expected "a strint and a function" args *)
  (* } *)
  (* ; *)
  { n = "Char_code"
  ; p = ["c"]
  ; f = function
      | [DChar c] -> c |> Char.to_int |> DInt
      | args -> expected "a char" args
  }
  ;
  { n = "Char_to_uppercase"
  ; p = ["c"]
  ; f = function
      | [DChar c] -> c |> Char.to_int |> (-) 32 |> Char.of_int_exn |> DChar
      | args -> expected "a char" args
  }
  ;
  { n = "Char_chr"
  ; p = ["i"]
  ; f = function
      | [DInt i] -> i |> Char.of_int_exn |> DChar
      | args -> expected "an char's integer ascii (todo: unicode) value" args
  }
]

module SMap = String.Map

type fnmap = fn SMap.t
let fns : fnmap =
  let add_fn (m : fnmap) (s : shortfn) : fnmap =
    SMap.add m ~key:s.n
      ~data:{name = s.n; parameters = s.p; func = s.f} in
  List.fold_left ~f:add_fn ~init:SMap.empty fns_list

(* Give access to other modules *)
let get_fn (name : string) : fn option =
  SMap.find fns name

let get_fn_exn (name : string) : fn =
  match SMap.find fns name with
  | Some fn -> fn
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.raise
