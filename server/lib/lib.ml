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
  |> Util.string_append ("Expected: " ^ msg ^ ", got: ")
  |> Exception.raise



let fns_list = [
  { n = "Page::page"
  ; o = []
  ; p = ["url"; "outputs"]
  ; f = function
      | args -> expected "this to be implmented" args
  }
  ;
  { n = "%"
  ; o = ["Int::mod"]
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a mod b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  { n = "+"
  ; o = ["Int::add"]
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a + b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  { n = "-"
  ; o = ["Int::sub"]
  ; p = ["a"; "b"]
  ; f = function
      | [DInt a; DInt b] -> a - b |> DInt
      | args -> expected "2 ints" args
  }
  ;
  { n = "String::foreach"
  ; o = []
  ; p = ["s"; "f"]
  ; f = function
      | [DStr s; DAnon (id, fn)] ->
        let charf (c: char) : char =
          let result = fn (DChar c) in
          match result with
          | DChar c -> c
          | r -> failwith "expected a char"
        in
        s |> String.map ~f:charf |> DStr
      | args -> expected "a string and a function" args
  }
  ;
  { n = "Char::code"
  ; o = []
  ; p = ["c"]
  ; f = function
      | [DChar c] -> c |> Char.to_int |> DInt
      | args -> expected "a char" args
  }
  ;
  { n = "Char::to_uppercase"
  ; o = []
  ; p = ["c"]
  ; f = function
      | [DChar c] -> c |> Char.to_int |> (-) 32 |> Char.of_int_exn |> DChar
      | args -> expected "a char" args
  }
  ;
  { n = "Char::chr"
  ; o = []
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
    let def = { name = s.n
              ; other_names = s.o
              ; parameters = s.p
              ; func = s.f} in
    List.fold_left ~f:(fun m1 n -> SMap.add m1 ~key:n ~data:def) ~init:m (s.n::s.o)
  in
  List.fold_left ~f:add_fn ~init:SMap.empty fns_list

(* Give access to other modules *)
let get_fn (name : string) : fn option =
  SMap.find fns name

let get_fn_exn (name : string) : fn =
  match SMap.find fns name with
  | Some fn -> fn
  | None -> "No function named '" ^ name ^ "' exists" |> Exception.raise
