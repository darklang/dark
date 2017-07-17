open Core

module Map = Core.Map.Poly

(* ------------------------- *)
(* Values *)
(* ------------------------- *)
type dval = DInt of int
          | DStr of string
          | DChar of char
          | DFloat of float
          | DFn of (dval list -> dval)

let parse (str : string) : dval =
  if String.equal str "" then
    "Values cannot be blank" |> Exception.raise
  else if String.length str >= 2
       && Char.equal '"' (String.nget str 0)
       && Char.equal '"' (String.nget str (-1))
  then DStr (String.slice str 1 (-1))
  else if String.length str = 3
       && Char.equal '\'' (String.nget str 0)
       && Char.equal '\'' (String.nget str (-1))
  then DChar (String.get str 1)
  else
    try str |> int_of_string |> DInt
    with
    | Failure _ ->
      try str |> float_of_string |> DFloat
      with
      | Failure _ ->
        Exception.raise ("Cannot parse value: " ^ str)

let to_repr (dv : dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DStr s -> "\"" ^ s ^ "\""
  | DFloat f -> string_of_float f
  | DChar c -> "'" ^ (Core.Char.to_string c) ^ "'"
  | DFn _ -> "Function: todo"

let get_type (dv : dval) : string =
  match dv with
  | DInt _ -> "Integer"
  | DStr _ -> "String"
  | DFloat _ -> "Float"
  | DChar _ -> "Char"
  | DFn _ -> "Function"



(* ------------------------- *)
(* Functions *)
(* ------------------------- *)
type fn = { name : string
          ; parameters : string list
          ; fn : (dval list) -> dval
          }
type fnmap = (string, fn) Map.t

let exe (fn : fn) (args : dval list) : dval =
  fn.fn args
